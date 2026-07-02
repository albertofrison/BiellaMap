library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(leaflet)
library(viridis)

# ==============================================================================
# FASE 4A: SCARICARE DATI GREEN ISPRA (ADATTATO SULLA TUA STRUTTURA)
# ==============================================================================


dati_ispra_grezzi <- read_excel("consumo_suolo_ispra.xlsx", sheet = "Comuni")

# ==============================================================================
# FASE 4B: ELABORAZIONE DATI GREEN ISPRA (ADATTATO SULLA TUA STRUTTURA)
# ==============================================================================
print("📖 Elaborazione dei dati ISPRA basata sul tuo Tibble...")

# Usiamo un selettore flessibile (Regex) per trovare la colonna del consumo (%)
# che si nasconde tra le 32 variabili abbreviate in fondo al tuo tibble.
df_verde_ispra <- dati_ispra_grezzi %>%
  select(
    Nome_Comune,
    Nome_Provincia,
    # Cerca automaticamente la colonna che contiene la percentuale o la parola Consumo
    matches("Consumo di suolo \\(%\\)|%|Consumo") 
  ) %>%
  # Rinominiamo la terza colonna (quella intercettata dalla regex) per lavorarla in sicurezza
  rename(Consumo_Suolo_Percentuale = 3) %>%
  
  # Standardizziamo i testi in MAIUSCOLO per non fallire il Join
  mutate(
    Consumo_Suolo_Percentuale = as.numeric(Consumo_Suolo_Percentuale),
    Comune_Join = str_to_upper(Nome_Comune),
    Provincia_Join = str_to_upper(Nome_Provincia),
    
    # KPI Qualità della Vita: Calcoliamo l'inverso per ottenere il Verde Naturale
    Indice_Verde_ISPRA = 100 - Consumo_Suolo_Percentuale
  ) %>%
  select(Comune_Join, Provincia_Join, Indice_Verde_ISPRA)

# ==============================================================================
# FASE 4C: UNIONE GEOGRAFICA MASCHERA-DATI (MAPPING A DOPPIA CHIAVE)
# ==============================================================================
print("🔗 Connessione dei dati ISPRA alla mappa interregionale...")

url_piemonte  <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_R_1_municipalities.geojson"
url_lombardia <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_R_3_municipalities.geojson"

mappa_macro_regione <- rbind(read_sf(url_piemonte), read_sf(url_lombardia))

mappa_geografica_pulita <- mappa_macro_regione %>%
  mutate(Comune_Join = str_to_upper(name)) %>%
  
  # DIZIONARIO DI ARMONIZZAZIONE ESTESO (Allineamento fusioni comuni)
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
  
  # Creiamo la colonna Provincia_Join usando il nome esteso (prov_name) presente nel GeoJSON
  mutate(Provincia_Join = str_to_upper(prov_name)) %>%
  
  # DISSOLVE GEOMETRICO: Raggruppiamo per Comune e Provincia per evitare omonimie
  group_by(Comune_Join, Provincia_Join, prov_acr) %>%
  summarise(.groups = "drop")

# 1. Uniamo prima i Prezzi Immobiliari del tuo dataset_immobiliare_master
df_prezzi_macro <- dataset_immobiliare_master %>% 
  mutate(Comune_Join = str_to_upper(Comune))

mappa_con_prezzi <- mappa_geografica_pulita %>%
  left_join(df_prezzi_macro, by = c("Comune_Join" = "Comune_Join", "prov_acr" = "Provincia"))

# 2. Uniamo ora il livello del Verde ISPRA usando la doppia chiave (Comune + Provincia Estesa)
mappa_bivariata_finale <- mappa_con_prezzi %>%
  left_join(df_verde_ispra, by = c("Comune_Join" = "Comune_Join", "Provincia_Join" = "Provincia_Join"))

# Estrazione del confine di Biella per la gerarchia visiva
confine_esterno_biella <- mappa_bivariata_finale %>% filter(prov_acr == "BI") %>% st_union()

print("=== STATISTICHE MATCHING VERDE COMMERCIALE ===")
print(summary(mappa_bivariata_finale$Indice_Verde_ISPRA))

# ==============================================================================
# FASE 5: RENDERING CARTOGRAFICO AD ALTO CONTRASTO BI-LAYER
# ==============================================================================
print("🎨 Generazione della mappa interattiva a due livelli...")

# Palette Prezzi (Divergente RdYlBu basata sui percentili dei tuoi dati)
valori_prezzi_validi <- mappa_bivariata_finale$Prezzo_Vendita_mq[!is.na(mappa_bivariata_finale$Prezzo_Vendita_mq)]
intervalli_dinamici <- round(unique(quantile(valori_prezzi_validi, probs = seq(0, 1, length.out = 9))))

pal_prezzi <- colorBin(palette = "RdYlBu", domain = mappa_bivariata_finale$Prezzo_Vendita_mq, bins = intervalli_dinamici, reverse = TRUE, na.color = "#d5dbdb")

# Palette Verde ISPRA (Giallo-Verde saturo ad alto contrasto)
pal_verde <- colorNumeric(palette = "YlGn", domain = mappa_bivariata_finale$Indice_Verde_ISPRA, na.color = "#d5dbdb")
pal_verde <- colorNumeric(
  palette = c("#7E5109", "#D35400", "#FFFF99", "#113F23", "#0A2F1D"), 
  domain = mappa_bivariata_finale$Indice_Verde_ISPRA, 
  na.color = "#d5dbdb"
)

mappa_finale_BRI <- leaflet(mappa_bivariata_finale) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # --- LAYER 1: MERCATO IMMOBILIARE ---
  addPolygons(
    fillColor = ~pal_prezzi(Prezzo_Vendita_mq),
    weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "💰 Prezzi Vendita (€/m²)",
    popup = ~paste0("<strong>Comune:</strong> ", Comune_Join, " (", prov_acr, ")<br/>",
                    "<strong>Prezzo Medio:</strong> ", Prezzo_Vendita_mq, " €/m²")
  ) %>%
  
  # --- LAYER 2: INDICE DEL VERDE UFFICIALE (ISPRA) ---
  addPolygons(
    fillColor = ~pal_verde(Indice_Verde_ISPRA),
    weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "🌳 Indice del Verde Naturale (%)",
    popup = ~paste0("<strong>Comune:</strong> ", Comune_Join, " (", prov_acr, ")<br/>",
                    "<strong>Territorio Naturale/Verde:</strong> ", round(Indice_Verde_ISPRA, 1), "%<br/>",
                    "Superficie Cemento: ", round(100 - Indice_Verde_ISPRA, 1), "%")
  ) %>%
  
  # Cornice nera/blu notte spessa per evidenziare Biella
  addPolylines(data = confine_esterno_biella, color = "#2c3e50", weight = 4.5, opacity = 1) %>%
  
  # Pannello di controllo interattivo
  addLayersControl(
    baseGroups = c("💰 Prezzi Vendita (€/m²)", "🌳 Indice del Verde Naturale (%)"),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  )

# Lancia lo strumento cartografico completo
mappa_finale_BRI
