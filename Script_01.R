# ==============================================================================
# PROGETTO: INDICE DI VIVIBILITÀ / SVILUPPO 2026 per Comuni Italiani (Focus su Piemonte e Lombardia)
# Descrizione: Pipeline geo-relazionale a 5 variabili
# ==============================================================================

# ------------------------------------------------------------------------------
# CAPITOLO 0: CONFIGURAZIONE AMBIENTE E CARICAMENTO MODULI
# ------------------------------------------------------------------------------
rm(list = ls())

library(chromote)
library(rvest)
library(stringr)
library(dplyr)
library(sf)
library(leaflet)
library(viridis)
library(readxl)

print("🚀 Inizializzazione")

# ------------------------------------------------------------------------------
# CAPITOLO 1: CONFIGURAZIONE DEL COORTICE GEOGRAFICO TARGET
# ------------------------------------------------------------------------------
df_target_province <- data.frame(
  regione = c(rep("piemonte", 8), rep("lombardia", 12)),
  provincia_cod = c("AL", "AT", "BI", "CN", "NO", "TO", "VB", "VC", 
                    "BG", "BS", "CO", "CR", "LC", "LO", "MN", "MI", "MB", "PV", "SO", "VA"),
  slug = c("alessandria", "asti", "biella", "cuneo", "novara", "torino", "verbania", "vercelli",
           "bergamo", "brescia", "como", "cremona", "lecco", "lodi", "mantova", "milano", "monza-brianza", "pavia", "sondrio", "varese")
)

# ------------------------------------------------------------------------------
# CAPITOLO 2: COOPTAZIONE WEB-DRIVER (REMOTE DEBUGGING ENGINE)
# ------------------------------------------------------------------------------
print("🔌 Connessione al web-driver Chrome sulla porta9222...")
# avvia via Terminale : "C:\Program Files\Google\Chrome\Application\chrome.exe" --remote-debugging-port=9222 --user-data-dir="C:\tmp\chrome_dev_profile"

chrome_remoto <- chromote::ChromeRemote$new(host = "127.0.0.1", port = 9222)
chromote_obj  <- Chromote$new(browser = chrome_remoto)
b             <- ChromoteSession$new(chromote_obj)

df_immobiliare_consolidato <- data.frame()

# ------------------------------------------------------------------------------
# CAPITOLO 3: WEB SCRAPING DATASET COMMERCIALE (1° KPI - COSTO IMMOBILI)
# ------------------------------------------------------------------------------
for(i in 1:nrow(df_target_province)) {
  
  reg   <- df_target_province$regione[i]
  prov  <- df_target_province$provincia_cod[i]
  slug  <- df_target_province$slug[i]
  
  url <- paste0("https://www.immobiliare.it/mercato-immobiliare/", reg, "/", slug, "-provincia/")
  print(paste("🌐 Scraping analitico provincia:", toupper(slug), "(", i, "/", nrow(df_target_province), ")"))
  
  b$Page$navigate(url)
  Sys.sleep(5) 
  
  runtime_result <- b$Runtime$evaluate("document.documentElement.outerHTML")
  html_contenuto <- runtime_result$result$value
  
  pagina <- read_html(html_contenuto)
  tutti_i_testi <- pagina %>% html_elements("div") %>% html_text(trim = TRUE)
  stringa_tabella <- tutti_i_testi[str_detect(tutti_i_testi, "^ComuniVendita")]
  
  if(length(stringa_tabella) == 0) {
    print(paste("⚠️ Record non intercettato per la struttura:", slug))
    next
  }
  
  stringa_dati <- str_remove(stringa_tabella[1], "ComuniVendita €/m²Affitto €/m²")
  
  pattern_estrazione <- "([A-Za-zÀ-ÿ\\s'-]+?)([0-9\\.]+)(\\([^)]+\\))?([0-9]+(?:,[0-9]{1,2})?)(\\([^)]+\\))?"
  matrice_estratta <- str_match_all(stringa_dati, pattern_estrazione)[[1]]
  
  if(nrow(matrice_estratta) > 0) {
    df_provincia_parziale <- as.data.frame(matrice_estratta) %>%
      select(Comune = V2, Prezzo_Vendita_Raw = V3, Prezzo_Affitto_Raw = V5) %>%
      mutate(
        Comune = str_trim(Comune),
        Prezzo_Vendita_mq = as.numeric(str_remove_all(Prezzo_Vendita_Raw, "\\.")),
        Prezzo_Affitto_mq = as.numeric(str_replace(Prezzo_Affitto_Raw, ",", ".")),
        Provincia = prov,
        Regione = str_to_title(reg),
        Data_Rilevazione = Sys.Date()
      ) %>%
      select(Comune, Prezzo_Vendita_mq, Prezzo_Affitto_mq, Provincia, Regione, Data_Rilevazione)
    
    df_immobiliare_consolidato <- bind_rows(df_immobiliare_consolidato, df_provincia_parziale)
  }
}
b$close() 
print("✅ Acquisizione e parsing dei dati di mercato immobiliare completati.")

# ------------------------------------------------------------------------------
# CAPITOLO 4: ACQUISIZIONE E PRE-ELABORAZIONE FONTI STRUTTURALI LOCALI
# ------------------------------------------------------------------------------
print("📖 Caricamento dei registri informativi territoriali (ISPRA, AGCOM, ISTAT)...")

# --- 4.1: COMPONENTE AMBIENTALE (2° KPI - SUOLO ED ECO-SISTEMI ISPRA) ---
df_ispra_raw <- read_excel("ISPRA_Consumo_Suolo.xlsx", sheet = "Comuni")
df_verde_processed <- df_ispra_raw %>%
  select(Nome_Comune, Nome_Provincia, matches("Consumo di suolo \\(%\\)|%|Consumo")) %>%
  rename(Consumo_Suolo_Percentuale = 3) %>%
  mutate(
    Consumo_Suolo_Percentuale = as.numeric(Consumo_Suolo_Percentuale),
    Comune_Join = str_to_upper(Nome_Comune),
    Provincia_Join = str_to_upper(Nome_Provincia),
    Indice_Verde_ISPRA = 100 - Consumo_Suolo_Percentuale 
  ) %>%
  select(Comune_Join, Provincia_Join, Indice_Verde_ISPRA)

# --- 4.2: COMPONENTE TELECOMUNICAZIONI (3° KPI - BANDA ULTRA LARGA AGCOM) ---
df_agcom_raw <- read_excel("AGCOM_Fibra.xls", sheet = "Comuni")
df_fibra_processed <- df_agcom_raw %>%
  mutate(
    Comune_Join = str_to_upper(Comune),
    Provincia_Join = str_to_upper(Provincia),
    Percentuale_Fibra = as.numeric(`Copertura FTTH DESI`) * 100 # dati in percentuale
  ) %>%
  select(Comune_Join, Provincia_Join, Percentuale_Fibra)

# --- 4.3: COMPONENTE SICUREZZA PUBBLICA (4° KPI - TASSI REATI MINISTERO INTERNO) ---
df_criminalita_raw <- read_excel("ISTAT_Tasso_Delitti.xlsx", sheet = "Province")
df_criminalita_processed <- df_criminalita_raw %>%
  select(Provincia_Estesa = Luogo, Valore_Indice = `2024`) %>%
  mutate(
    Provincia_Join = str_to_upper(str_trim(Provincia_Estesa)),
    Reati_100k = as.numeric(Valore_Indice)
  ) %>%
  select(Provincia_Join, Reati_100k)

# --- 4.4: COMPONENTE INFRASTRUTTURA LOGISTICA (5° KPI - PROSSIMITÀ SERVIZI MINISTERO/ISTAT) ---
df_aree_interne_raw <- read_excel("Ministero_Aree_Comuni.xlsx", sheet = "Comuni")
df_servizi_processed <- df_aree_interne_raw %>%
  select(Nome_Comune = `Comune`, Provincia = `Provincia`, Categoria_Area = `Mappa`) %>%
  mutate(
    Comune_Join = str_to_upper(str_trim(Nome_Comune)),
    Provincia_Join = str_to_upper(str_trim(Provincia)),
    Valore_Mappa = str_to_upper(str_trim(Categoria_Area)),
    
    Indice_Servizi_ISTAT = case_when(
      str_detect(Valore_Mappa, "A|POLO") ~ 100,
      str_detect(Valore_Mappa, "B|CINTURA") ~ 85,
      str_detect(Valore_Mappa, "C|INTERMEDIO") ~ 60,
      str_detect(Valore_Mappa, "D|PERIFERICO") ~ 40,
      str_detect(Valore_Mappa, "E|F|ULTRAPERIFERICO") ~ 15,
      TRUE ~ 50
    )
  ) %>%
  select(Comune_Join, Provincia_Join, Indice_Servizi_ISTAT)

# ------------------------------------------------------------------------------
# CAPITOLO 5: INGEGNERIA GEOGRAFICA E COMPILAZIONE DEL GEODATABASE
# ------------------------------------------------------------------------------
print("🗺️ Configurazione spaziale ed esecuzione dei Join multilivello...")

url_piemonte  <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_R_1_municipalities.geojson"
url_lombardia <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_R_3_municipalities.geojson"
sf_macro_regione <- rbind(read_sf(url_piemonte), read_sf(url_lombardia))

sf_base_geografica <- sf_macro_regione %>%
  mutate(Comune_Join = str_to_upper(name)) %>%
  mutate(Comune_Join = case_when(
    Comune_Join %in% c("MOSSO", "SOPRANA", "TRIVERO", "VALLE MOSSO") ~ "VALDILANA",
    Comune_Join %in% c("QUAREGNA", "CERRETO CASTELLO") ~ "QUAREGNA CERRETO",
    Comune_Join %in% c("SAN PAOLO CERVO", "QUITTENGO") ~ "CAMPIGLIA CERVO",
    Comune_Join %in% c("CROSA") ~ "LESSONA",
    Comune_Join %in% c("RONAGO", "UGGIATE-TREVANO") ~ "UGGIATE CON RONAGO",
    Comune_Join %in% c("ALBAREDO ARNABOLDI", "CAMPOSPINOSO") ~ "CAMPOSPINOSO ALBAREDO",
    Comune_Join == "LIRIO" ~ "PIETRA DE' GIORGI",
    Comune_Join == "CASORZO" ~ "CASORZO MONFERRATO",
    Comune_Join == "GRANA" ~ "GRANA MONFERRATO",
    Comune_Join == "LEINÌ" ~ "LEINI",
    TRUE ~ Comune_Join
  )) %>%
  mutate(Provincia_Join = str_to_upper(prov_name)) %>%
  group_by(Comune_Join, Provincia_Join, prov_acr) %>%
  summarise(.groups = "drop")

df_prezzi_mappati <- df_immobiliare_consolidato %>% mutate(Comune_Join = str_to_upper(Comune))
sf_join_prezzi <- sf_base_geografica %>%
  left_join(df_prezzi_mappati, by = c("Comune_Join" = "Comune_Join", "prov_acr" = "Provincia"))

# Isolamento temporaneo del formato data.frame per prevenire eccezioni sui vettori geometrici
sf_dati_consolidati <- sf_join_prezzi %>%
  as.data.frame() %>% 
  left_join(df_verde_processed, by = c("Comune_Join" = "Comune_Join", "Provincia_Join" = "Provincia_Join")) %>%
  left_join(df_fibra_processed, by = c("Comune_Join" = "Comune_Join", "Provincia_Join" = "Provincia_Join")) %>%
  left_join(df_criminalita_processed, by = "Provincia_Join") %>% 
  left_join(df_servizi_processed, by = c("Comune_Join" = "Comune_Join", "Provincia_Join" = "Provincia_Join")) %>%
  st_as_sf()

# Tracciamento del confine perimetrale della provincia di Biella
sf_bordo_biella <- sf_dati_consolidati %>% filter(prov_acr == "BI") %>% st_union()

# ------------------------------------------------------------------------------
# CAPITOLO 6: ALGORITMO DI NORMALIZZAZIONE STATISTICA E COMPILAZIONE NOTA DINAMICA
# ------------------------------------------------------------------------------
print("🧮 Esecuzione del modello di calcolo dell'Indice di Vivibilità / Sviluppo 2026...")

sf_dati_consolidati <- sf_dati_consolidati %>%
  mutate(
    # Normalizzazione Min-Max (Fattori inversi e diretti riscalati a 0-100)
    min_p = min(Prezzo_Vendita_mq, na.rm = TRUE),
    max_p = max(Prezzo_Vendita_mq, na.rm = TRUE),
    Score_Prezzo = ifelse(!is.na(Prezzo_Vendita_mq), 100 * (max_p - Prezzo_Vendita_mq) / (max_p - min_p), 50),
    
    Score_Verde = ifelse(!is.na(Indice_Verde_ISPRA), Indice_Verde_ISPRA, 50),
    Score_Fibra = ifelse(!is.na(Percentuale_Fibra), Percentuale_Fibra, 0),
    Score_Servizi = ifelse(!is.na(Indice_Servizi_ISTAT), Indice_Servizi_ISTAT, 50),
    
    min_c = min(Reati_100k, na.rm = TRUE),
    max_c = max(Reati_100k, na.rm = TRUE),
    Score_Sicurezza = ifelse(!is.na(Reati_100k), 100 * (max_c - Reati_100k) / (max_c - min_c), 50),
    
    # INDICE SINTETICO SULLA VIVIBILITÀ (Ponderazione simmetrica al 20% per ciascun indicatore)
    Indice_Vivibilita_2026 = round((Score_Prezzo + Score_Verde + Score_Fibra + Score_Sicurezza + Score_Servizi) / 5, 1),
    
    # STRUTTURAZIONE DELLA NOTA DI SINTESI IN FORMATO HTML PER COMPONENTE WIDGET
    Nota_Sintesi = paste0(
      "<div style='font-family: Arial, sans-serif; min-width: 250px;'>",
      "<h3 style='margin:0 0 5px 0; color:#2c3e50;'>📍 ", Comune_Join, " (", prov_acr, ")</h3>",
      "<div style='background:#f8f9fa; padding:8px; border-left:4px solid #2980b9; margin-bottom:8px;'>",
      "<strong style='font-size:13px;'>🏆 VIVIBILITÀ: ", Indice_Vivibilita_2026, " / 100</strong><br/>",
      "<span style='font-size:11px; color:#7f8c8d;'>Valutazione: ", 
      case_when(
        Indice_Vivibilita_2026 >= 72 ~ "🌟 Eccellente (Area ad alto sviluppo)",
        Indice_Vivibilita_2026 >= 58 ~ "✅ Consigliato (Equilibrio ottimale)",
        Indice_Vivibilita_2026 >= 45 ~ "⚖️ Discreto (Compromesso territoriale)",
        TRUE ~ "⚠️ Critico (Forti penalizzazioni strutturali)"
      ),
      "</span>",
      "</div>",
      "<table style='width:100%; font-size:11px; border-collapse: collapse;'>",
      "<tr><td>💰 <strong>Prezzo Casa:</strong></td><td style='text-align:right;'>", ifelse(is.na(Prezzo_Vendita_mq), "N.D.", paste0(Prezzo_Vendita_mq, " €/m²")), "</td></tr>",
      "<tr><td>🌳 <strong>Verde Naturale:</strong></td><td style='text-align:right;'>", round(Indice_Verde_ISPRA, 1), "%</td></tr>",
      "<tr><td>⚡ <strong>Fibra FTTH:</strong></td><td style='text-align:right;'>", round(Percentuale_Fibra, 1), "%</td></tr>",
      "<tr><td>🚨 <strong>Sicurezza (Reati):</strong></td><td style='text-align:right;'>", round(Reati_100k, 0), " /100k</td></tr>",
      "<tr><td>🏙️ <strong>Accesso Servizi:</strong></td><td style='text-align:right; color:#8e44ad; font-weight:bold;'>", Indice_Servizi_ISTAT, " / 100</td></tr>",
      "</table>",
      "</div>"
    )
  )

# ------------------------------------------------------------------------------
# CAPITOLO 7: RENDERING CARTOGRAFICO GEODINAMICO
# ------------------------------------------------------------------------------
print("🎨 Compilazione delle funzioni cartografiche e generazione della mappa interattiva...")

# Generazione dei gradienti discreti e continui per i layer analitici
pal_indice_vivibilita <- colorNumeric(palette = "RdYlGn", domain = sf_dati_consolidati$Indice_Vivibilita_2026, na.color = "#d5dbdb")
pal_prezzi            <- colorBin(palette = "RdYlBu", domain = sf_dati_consolidati$Prezzo_Vendita_mq, bins = 8, reverse = TRUE, na.color = "#d5dbdb")
pal_verde             <- colorNumeric(palette = c("#7E5109", "#D35400", "#FFFF99", "#113F23", "#0A2F1D"), domain = sf_dati_consolidati$Indice_Verde_ISPRA, na.color = "#d5dbdb")
pal_fibra             <- colorNumeric(palette = "YlGnBu", domain = sf_dati_consolidati$Percentuale_Fibra, na.color = "#d5dbdb")
pal_crimine           <- colorNumeric(palette = "OrRd", domain = sf_dati_consolidati$Reati_100k, na.color = "#d5dbdb")
pal_servizi           <- colorNumeric(palette = "Purples", domain = sf_dati_consolidati$Indice_Servizi_ISTAT, na.color = "#d5dbdb")

# Assemblaggio finale del widget Leaflet a 6 gruppi cartografici condivisibili
widget_mappa_vivibilita <- leaflet(sf_dati_consolidati) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Layer 1: Indice di Sintesi Globale (Inizializzazione di default sullo schermo)
  addPolygons(fillColor = ~pal_indice_vivibilita(Indice_Vivibilita_2026), weight = 0.5, color = "white", fillOpacity = 0.8, group = "🏆 Indice di Vivibilità / Sviluppo 2026", popup = ~Nota_Sintesi) %>%
  
  # Layer Informativi Verticali
  addPolygons(fillColor = ~pal_prezzi(Prezzo_Vendita_mq), weight = 0.5, color = "white", fillOpacity = 0.8, group = "💰 Prezzi Vendita (€/m²)", popup = ~Nota_Sintesi) %>%
  addPolygons(fillColor = ~pal_verde(Indice_Verde_ISPRA), weight = 0.5, color = "white", fillOpacity = 0.8, group = "🌳 Indice del Verde (%)", popup = ~Nota_Sintesi) %>%
  addPolygons(fillColor = ~pal_fibra(Percentuale_Fibra), weight = 0.5, color = "white", fillOpacity = 0.8, group = "⚡ Copertura Fibra FTTH (%)", popup = ~Nota_Sintesi) %>%
  addPolygons(fillColor = ~pal_crimine(Reati_100k), weight = 0.5, color = "white", fillOpacity = 0.8, group = "🚨 Indice Criminalità", popup = ~Nota_Sintesi) %>%
  addPolygons(fillColor = ~pal_servizi(Indice_Servizi_ISTAT), weight = 0.5, color = "white", fillOpacity = 0.8, group = "🏙️ Accesso Servizi (ISTAT)", popup = ~Nota_Sintesi) %>%
  
  # Overlay grafico fisso: Perimetro della provincia di Biella
  addPolylines(data = sf_bordo_biella, color = "#2c3e50", weight = 4.5, opacity = 1) %>%
  
  # Pannello di controllo interattivo per lo switch dei database visualizzati
  addLayersControl(
    baseGroups = c(
      "🏆 Indice di Vivibilità / Sviluppo 2026", 
      "💰 Prezzi Vendita (€/m²)", 
      "🌳 Indice del Verde (%)", 
      "⚡ Copertura Fibra FTTH (%)", 
      "🚨 Indice Criminalità", 
      "🏙️ Accesso Servizi (ISTAT)"
    ),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  )

# Generazione a schermo del prodotto finale
widget_mappa_vivibilita


# ------------------------------------------------------------------------------
# CAPITOLO 8: ESPORTAZIONE PER LA PUBBLICAZIONE SU GITHUB PAGES
# ------------------------------------------------------------------------------
print("💾 Esportazione della mappa in formato HTML standalone...")

# Salviamo il widget come index.html nella cartella locale della repository
htmlwidgets::saveWidget(
  widget_mappa_vivibilita, 
  file = "index.html", 
  selfcontained = TRUE
)

print("🎉 File 'index.html' pronto per il commit Git!")
