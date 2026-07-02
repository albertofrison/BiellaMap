# ==============================================================================
# PROGETTO: BIELLA REMOTE INDEX (BRI) - TRIVARIATE MACRO PIPELINE
# Obiettivo: Ingestione e visualizzazione 3D (Prezzi, Verde ISPRA, Fibra AGCOM)
# Allineamento: Dataset reali 2026 + Bypass geometrico dei vincoli Tibble
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

print("🚀 Avvio della pipeline integrata a tre livelli...")

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
# FASE 2: CONNESSIONE AL BROWSER FISICO (ANTI-BOT BYPASS)
# ==============================================================================
# lancia i codici su terminale
# "C:\Program Files\Google\Chrome\Application\chrome.exe" --remote-debugging-port=9222 --user-data-dir="C:\chrome_dev_profile"

print("🔌 Connessione all'istanza aperta di Google Chrome (Porta 9222)...")
chrome_remoto <- chromote::ChromeRemote$new(host = "127.0.0.1", port = 9222)
chromote_obj  <- Chromote$new(browser = chrome_remoto)
b             <- ChromoteSession$new(chromote_obj)

dataset_immobiliare_master <- data.frame()

# ==============================================================================
# FASE 3: ESTRAZIONE MASSIVA MERCATO IMMOBILIARE
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
  
  # Regex flessibile universale per catturare decimali assenti o singoli
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
print("🎉 Ingestione immobiliare completata con successo.")

# ==============================================================================
# FASE 4A: CARICAMENTO E PREPARAZIONE FONTI ESTERNE (ISPRA & AGCOM)
# ==============================================================================
print("📖 Importazione e pulizia dei registri ISPRA e AGCOM...")

# --- 1. LIVELLO VERDE: REGISTRO ISPRA ---
dati_ispra_grezzi <- read_excel("consumo_di_suolo_estratto_dati_2025_anni_2006_2024.xlsx", sheet = "Comuni_2006_2024")

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

# --- 2. LIVELLO SERVIZI: AGCOM REPORT ---
df_fibra_grezza <- read_excel("Reportistica_260331_Comuni.xls", sheet = "Agcom Report Comuni 31-03-2026")

df_fibra_pulito <- df_fibra_grezza %>%
  mutate(
    Comune_Join = str_to_upper(Comune),
    Provincia_Join = str_to_upper(Provincia),
    # Utilizzo tassativo degli apici inversi per gestire la colonna con gli spazi
    Percentuale_Fibra = as.numeric(`Copertura FTTH DESI`)*100
  ) %>%
  select(Comune_Join, Provincia_Join, Percentuale_Fibra)

# ==============================================================================
# FASE 4B: ELABORAZIONE GEOGRAFICA AVANZATA E MERGING (TIBBLE BYPASS)
# ==============================================================================
print("🗺️ Integrazione geografica dei confini regionali e fusione tabelle...")

url_piemonte  <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_R_1_municipalities.geojson"
url_lombardia <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_R_3_municipalities.geojson"
mappa_macro_regione <- rbind(read_sf(url_piemonte), read_sf(url_lombardia))

# Strutturazione mappa con dizionario fusioni aggiornato
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

# 1. Primo accoppiamento: Prezzi Immobiliari
df_prezzi_macro <- dataset_immobiliare_master %>% mutate(Comune_Join = str_to_upper(Comune))
mappa_con_prezzi <- mappa_geografica_pulita %>%
  left_join(df_prezzi_macro, by = c("Comune_Join" = "Comune_Join", "prov_acr" = "Provincia"))

# 2. Secondo e terzo accoppiamento con BYPASS DATA.FRAME per evitare l'errore geometry del tibble
mappa_trivariata_finale <- mappa_con_prezzi %>%
  as.data.frame() %>% # <-- Rompe le catene rigide del tibble
  left_join(df_verde_ispra, by = c("Comune_Join" = "Comune_Join", "Provincia_Join" = "Provincia_Join")) %>%
  left_join(df_fibra_pulito, by = c("Comune_Join" = "Comune_Join", "Provincia_Join" = "Provincia_Join")) %>%
  st_as_sf() # <-- Reinietta l'intelligenza e i vincoli geografici all'oggetto spaziale

# Generazione del macro-confine di Biella spaccato sopra i comuni
confine_esterno_biella <- mappa_trivariata_finale %>% filter(prov_acr == "BI") %>% st_union()

# ==============================================================================
# FASE 5: RENDERING CARTOGRAFICO TRIVARIATO AD ALTO IMPATTO
# ==============================================================================
print("🎨 Compilazione e rendering del pannello Leaflet multi-layer...")

# Palette Layer 1: Prezzi (Percentili ad alto contrasto dal Blu al Rosso)
valori_prezzi_validi <- mappa_trivariata_finale$Prezzo_Vendita_mq[!is.na(mappa_trivariata_finale$Prezzo_Vendita_mq)]
intervalli_dinamici <- round(unique(quantile(valori_prezzi_validi, probs = seq(0, 1, length.out = 9))))
pal_prezzi <- colorBin(palette = "RdYlBu", domain = mappa_trivariata_finale$Prezzo_Vendita_mq, bins = intervalli_dinamici, reverse = TRUE, na.color = "#d5dbdb")

# Palette Layer 2: Verde Violento (Dal Cemento Marrone Diarrea al Verdone Scuro Cupo #0A2F1D)
pal_verde <- colorNumeric(palette = c("#7E5109", "#D35400", "#FFFF99", "#113F23", "#0A2F1D"), domain = mappa_trivariata_finale$Indice_Verde_ISPRA, na.color = "#d5dbdb")

# Palette Layer 3: Servizi Digitali (Scala tecnologica YlGnBu per la Fibra)
pal_fibra <- colorNumeric(palette = "YlGnBu", domain = mappa_trivariata_finale$Percentuale_Fibra, na.color = "#d5dbdb")

# Costruzione del Widget HTML definitivo
mappa_super_BRI <- leaflet(mappa_trivariata_finale) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # --- LIVELLO 1: COMMERCIALE ---
  addPolygons(
    fillColor = ~pal_prezzi(Prezzo_Vendita_mq), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "💰 Prezzi Vendita (€/m²)",
    popup = ~paste0("<strong>Comune:</strong> ", Comune_Join, " (", prov_acr, ")<br/>",
                    "<strong>Prezzo Medio:</strong> ", Prezzo_Vendita_mq, " €/m²")
  ) %>%
  
  # --- LIVELLO 2: NATURA VS URBANIZZAZIONE ---
  addPolygons(
    fillColor = ~pal_verde(Indice_Verde_ISPRA), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "🌳 Indice del Verde (%)",
    popup = ~paste0("<strong>Comune:</strong> ", Comune_Join, " (", prov_acr, ")<br/>",
                    "<strong>Verde Naturale:</strong> ", round(Indice_Verde_ISPRA, 1), "%<br/>",
                    "Cemento/Asfalto: ", round(100 - Indice_Verde_ISPRA, 1), "%")
  ) %>%
  
  # --- LIVELLO 3: INFRASTRUTTURA DIGITALE ---
  addPolygons(
    fillColor = ~pal_fibra(Percentuale_Fibra), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "⚡ Copertura Fibra FTTH (%)",
    popup = ~paste0("<strong>Comune:</strong> ", Comune_Join, " (", prov_acr, ")<br/>",
                    "<strong>Copertura Fibra DESI:</strong> ", round(Percentuale_Fibra, 1), "%")
  ) %>%
  
  # La cornice protettiva ardesia spessa che blinda la provincia di Biella
  addPolylines(data = confine_esterno_biella, color = "#2c3e50", weight = 4.5, opacity = 1) %>%
  
  # Pannello switch in alto a destra per l'utente finale
  addLayersControl(
    baseGroups = c("💰 Prezzi Vendita (€/m²)", "🌳 Indice del Verde (%)", "⚡ Copertura Fibra FTTH (%)"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  )

# Output
mappa_super_BRI
