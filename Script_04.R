# ==============================================================================
# PROGETTO: BIELLA REMOTE INDEX (BRI) - QUADRIVARIATE ENTERPRISE PIPELINE
# Obiettivo: Ingestione e visualizzazione a 4 Layer (Prezzi, Verde, Fibra, Sicurezza)
# Allineamento: File locali 2026 + Bypass geometrico dei vincoli Tibble
# ==============================================================================

# 0. RESET E PREPARAZIONE AMBIENTE
rm(list = ls())

library(chromote)
library(rvest)
library(stringr)
library(dplyr)
library(sf)
library(leaflet)
library(viridis)
library(readxl)

print("🚀 Avvio della super-pipeline a 4 dimensioni...")

# ==============================================================================
# FASE 1: DEFINIZIONE TARGET CON SLUGS ALLINEATI
# ==============================================================================
target_province <- data.frame(
  regione = c(rep("piemonte", 8), rep("lombardia", 12)),
  provincia_cod = c("AL", "AT", "BI", "CN", "NO", "TO", "VB", "VC", 
                    "BG", "BS", "CO", "CR", "LC", "LO", "MN", "MI", "MB", "PV", "SO", "VA"),
  slug = c("alessandria", "asti", "biella", "cuneo", "novara", "torino", "verbania", "vercelli",
           "bergamo", "brescia", "como", "cremona", "lecco", "lodi", "mantova", "milano", "monza-brianza", "pavia", "sondrio", "varese")
)

# ==============================================================================
# FASE 2: CONNESSIONE AL BROWSER FISICO (PORTA REMOTE DEBUGGING 9222)
# ==============================================================================
print("🔌 Connessione all'istanza aperta di Google Chrome...")
chrome_remoto <- chromote::ChromeRemote$new(host = "127.0.0.1", port = 9222)
chromote_obj  <- Chromote$new(browser = chrome_remoto)
b             <- ChromoteSession$new(chromote_obj)

dataset_immobiliare_master <- data.frame()

# ==============================================================================
# FASE 3: ESTRAZIONE MASSIVA MERCATO IMMOBILIARE (SCRAPING LIVE)
# ==============================================================================
for(i in 1:nrow(target_province)) {
  
  reg   <- target_province$regione[i]
  prov  <- target_province$provincia_cod[i]
  slug  <- target_province$slug[i]
  
  url <- paste0("https://www.immobiliare.it/mercato-immobiliare/", reg, "/", slug, "-provincia/")
  print(paste("🌐 Ingestione prezzi provincia:", toupper(slug), "(", i, "/", nrow(target_province), ")"))
  
  b$Page$navigate(url)
  Sys.sleep(15) 
  
  runtime_result <- b$Runtime$evaluate("document.documentElement.outerHTML")
  html_grezzo <- runtime_result$result$value
  
  pagina <- read_html(html_grezzo)
  tutti_i_testi <- pagina %>% html_elements("div") %>% html_text(trim = TRUE)
  stringa_tabella <- tutti_i_testi[str_detect(tutti_i_testi, "^ComuniVendita")]
  
  if(length(stringa_tabella) == 0) {
    print(paste("⚠️ Skip o tabella protetta su:", slug))
    next
  }
  
  stringa_dati <- str_remove(stringa_tabella[1], "ComuniVendita €/m²Affitto €/m²")
  
  pattern_universale <- "([A-Za-zÀ-ÿ\\s'-]+?)([0-9\\.]+)(\\([^)]+\\))?([0-9]+(?:,[0-9]{1,2})?)(\\([^)]+\\))?"
  matrice_estratta <- str_match_all(stringa_dati, pattern_universale)[[1]]
  
  if(nrow(matrice_estratta) > 0) {
    df_provincia <- as.data.frame(matrice_estratta) %>%
      select(Comune = V2, Prezzo_Vendita_Grezzo = V3, Prezzo_Affitto_Grezzo = V5) %>%
      mutate(
        Comune = str_trim(Comune),
        Prezzo_Vendita_mq = as.numeric(str_remove_all(Prezzo_Vendita_Grezzo, "\\.")),
        Prezzo_Affitto_mq = as.numeric(str_replace(Prezzo_Affitto_Grezzo, ",", ".")),
        Provincia = prov,
        Regione = str_to_title(reg),
        Data_Rilevazione = Sys.Date()
      ) %>%
      select(Comune, Prezzo_Vendita_mq, Prezzo_Affitto_mq, Provincia, Regione, Data_Rilevazione)
    
    dataset_immobiliare_master <- bind_rows(dataset_immobiliare_master, df_provincia)
  }
}
b$close()
print("✅ Ingestione immobiliare completata con successo.")

# ==============================================================================
# FASE 4A: INGESTIONE FONTI EXTRA LOCALI (ISPRA, AGCOM, CRIMINALITÀ EXCEL)
# ==============================================================================
print("📖 Caricamento e pulizia dei file Excel locali...")

# --- 1. LIVELLO VERDE: REGISTRO ISPRA ---
dati_ispra_grezzi <- read_excel("ISPRA_Consumo_Suolo.xlsx", sheet = "Comuni")
df_verde_ispra <- dati_ispra_grezzi %>%
  select(Nome_Comune, Nome_Provincia, matches("Consumo di suolo \\(%\\)|%|Consumo")) %>%
  rename(Consumo_Suolo_Percentuale = 3) %>%
  mutate(
    Consumo_Suolo_Percentuale = as.numeric(Consumo_Suolo_Percentuale),
    Comune_Join = str_to_upper(Nome_Comune),
    Provincia_Join = str_to_upper(Nome_Provincia),
    Indice_Verde_ISPRA = 100 - Consumo_Suolo_Percentuale
  ) %>%
  select(Comune_Join, Provincia_Join, Indice_Verde_ISPRA)

# --- 2. LIVELLO SERVIZI DIGITALI: AGCOM REPORT ---
df_fibra_grezza <- read_excel("AGCOM_Fibra.xls", sheet = "Comuni")
df_fibra_pulito <- df_fibra_grezza %>%
  mutate(
    Comune_Join = str_to_upper(Comune),
    Provincia_Join = str_to_upper(Provincia),
    Percentuale_Fibra = as.numeric(`Copertura FTTH DESI`)*100 # Apici inversi corretti
  ) %>%
  select(Comune_Join, Provincia_Join, Percentuale_Fibra)

# --- 3. LIVELLO SICUREZZA: IL TUO NUOVO EXCEL DELLA CRIMINALITÀ ---
# Modifica il nome del file o dello sheet se differiscono sul tuo PC
dati_crimine_grezzi <- read_excel("ISTAT_Tasso_Delitti.xlsx", sheet = "Province")

# Ispezioniamo al volo i nomi per debug visivo in console
print("Colonne rilevate nel tuo Excel Criminalità:")
print(names(dati_crimine_grezzi))

df_criminalita_pulito <- dati_crimine_grezzi %>%
  # Selezioniamo la colonna della Provincia e quella dell'Indice Numerico (es: Delitti ogni 100k)
  # Usiamo l'indice della colonna (1 e 2 o i nomi) per essere totalmente sicuri
  select(
    Luogo, 
    `2024`
  ) %>%
  mutate(
    # Puliamo il testo della provincia (es: "Biella" -> "BIELLA")
    Provincia_Join = str_to_upper(str_trim(Luogo)),
    Reati_100k = as.numeric(`2024`)
  ) %>%
  select(Provincia_Join, Reati_100k)


# ==============================================================================
# FASE 4B: ELABORAZIONE GEOGRAFICA E COMPILAZIONE SUPER-DATABASE (TIBBLE BYPASS)
# ==============================================================================
print("🗺️ Sincronizzazione cartografica dei vettori regionali...")

url_piemonte  <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_R_1_municipalities.geojson"
url_lombardia <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_R_3_municipalities.geojson"
mappa_macro_regione <- rbind(read_sf(url_piemonte), read_sf(url_lombardia))

mappa_geografica_pulita <- mappa_macro_regione %>%
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

# Unione sequenziale applicando il bypass .data.frame per evitare l'errore geometry
df_prezzi_macro <- dataset_immobiliare_master %>% mutate(Comune_Join = str_to_upper(Comune))
mappa_con_prezzi <- mappa_geografica_pulita %>%
  left_join(df_prezzi_macro, by = c("Comune_Join" = "Comune_Join", "prov_acr" = "Provincia"))

mappa_quadrivariata_finale <- mappa_con_prezzi %>%
  as.data.frame() %>% # <-- Bypass salvavita contro il blocco del tibble
  left_join(df_verde_ispra, by = c("Comune_Join" = "Comune_Join", "Provincia_Join" = "Provincia_Join")) %>%
  left_join(df_fibra_pulito, by = c("Comune_Join" = "Comune_Join", "Provincia_Join" = "Provincia_Join")) %>%
  left_join(df_criminalita_pulito, by = "Provincia_Join") %>% # Join provinciale della sicurezza
  st_as_sf() # <-- Ripristino dell'architettura geografica

# Isolamento del perimetro di Biella per la gerarchia visiva superiore
confine_esterno_biella <- mappa_quadrivariata_finale %>% filter(prov_acr == "BI") %>% st_union()

# ==============================================================================
# FASE 5: RENDERING CARTOGRAFICO REALE A 4 LAYER (AD ALTO CONTRASTO)
# ==============================================================================
print("🎨 Compilazione del pannello Leaflet multi-layer tracciabile...")

# 1. Palette Prezzi (Percentili ad alta dinamicità dal Blu al Rosso)
valori_prezzi_validi <- mappa_quadrivariata_finale$Prezzo_Vendita_mq[!is.na(mappa_quadrivariata_finale$Prezzo_Vendita_mq)]
intervalli_dinamici <- round(unique(quantile(valori_prezzi_validi, probs = seq(0, 1, length.out = 9))))
pal_prezzi <- colorBin(palette = "RdYlBu", domain = mappa_quadrivariata_finale$Prezzo_Vendita_mq, bins = intervalli_dinamici, reverse = TRUE, na.color = "#d5dbdb")

# 2. Palette Verde CORRETTA (Dal Cemento Marrone Diarrea al Verdone Notte #0A2F1D)
pal_verde <- colorNumeric(palette = c("#7E5109", "#D35400", "#FFFF99", "#113F23", "#0A2F1D"), domain = mappa_quadrivariata_finale$Indice_Verde_ISPRA, na.color = "#d5dbdb")

# 3. Palette Fibra AGCOM (Scala Ciano/Blu digitale ad alta tecnologia)
pal_fibra <- colorNumeric(palette = "YlGnBu", domain = mappa_quadrivariata_finale$Percentuale_Fibra, na.color = "#d5dbdb")

# 4. Palette Criminalità (Dal Giallo tenue al Rosso Shock per evidenziare il pericolo)
pal_crimine <- colorNumeric(palette = "OrRd", domain = mappa_quadrivariata_finale$Reati_100k, na.color = "#d5dbdb")

# Compilazione del Widget finale
mappa_BRI <- leaflet(mappa_quadrivariata_finale) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # --- LAYER 1: COSTO IMMOBILI ---
  addPolygons(
    fillColor = ~pal_prezzi(Prezzo_Vendita_mq), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "💰 Prezzi Vendita (€/m²)",
    popup = ~paste0("<strong>Comune:</strong> ", Comune_Join, " (", prov_acr, ")<br/>",
                    "<strong>Prezzo Medio:</strong> ", Prezzo_Vendita_mq, " €/m²")
  ) %>%
  
  # --- LAYER 2: IMPATTO AMBIENTALE ---
  addPolygons(
    fillColor = ~pal_verde(Indice_Verde_ISPRA), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "🌳 Indice del Verde (%)",
    popup = ~paste0("<strong>Comune:</strong> ", Comune_Join, " (", prov_acr, ")<br/>",
                    "<strong>Verde Naturale:</strong> ", round(Indice_Verde_ISPRA, 1), "%<br/>",
                    "Cemento/Asfalto: ", round(100 - Indice_Verde_ISPRA, 1), "%")
  ) %>%
  
  # --- LAYER 3: INFRASTRUTTURA INTERNET ---
  addPolygons(
    fillColor = ~pal_fibra(Percentuale_Fibra), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "⚡ Copertura Fibra FTTH (%)",
    popup = ~paste0("<strong>Comune:</strong> ", Comune_Join, " (", prov_acr, ")<br/>",
                    "<strong>Copertura Fibra:</strong> ", round(Percentuale_Fibra, 1), "%")
  ) %>%
  
  # --- LAYER 4: SICUREZZA REALE EXCEL ---
  addPolygons(
    fillColor = ~pal_crimine(Reati_100k), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "  Indice Criminalità (Fonte Excel)",
    popup = ~paste0("<strong>Comune:</strong> ", Comune_Join, "<br/>",
                    "<strong>Provincia di:</strong> ", Provincia_Join, "<br/>",
                    "Delitti denunciati: ", Reati_100k, " ogni 100k ab.")
  ) %>%
  
  # Cornice protettiva esterna spessa su Biella
  addPolylines(data = confine_esterno_biella, color = "#2c3e50", weight = 4.5, opacity = 1) %>%
  
  # Il super-selettore di controllo in alto a destra
  addLayersControl(
    baseGroups = c("💰 Prezzi Vendita (€/m²)", "🌳 Indice del Verde (%)", "⚡ Copertura Fibra FTTH (%)", "  🚨Indice Criminalità (Fonte Excel)"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  )

# Eseguiamo il rendering a schermo
mappa_BRI
