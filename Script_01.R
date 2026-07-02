# ==============================================================================
# PROGETTO: BIELLA REMOTE INDEX (BRI) - REAL ESTATE DATA PIPELINE
# Obiettivo: Estrazione, pulizia e mappatura dei prezzi immobiliari massivi
# Stack: Chromote (Headed), Rvest, Regex Universale, SF Geometric Dissolve, Leaflet
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

print("🚀 Avvio della pipeline immobiliare...")

# ==============================================================================
# FASE 1: DEFINIZIONE DEL TARGET (PIEMONTE & LOMBARDIA)
# ==============================================================================
target_province <- data.frame(
  regione = c(rep("piemonte", 8), rep("lombardia", 12)),
  provincia_cod = c("AL", "AT", "BI", "CN", "NO", "TO", "VB", "VC", 
                    "BG", "BS", "CO", "CR", "LC", "LO", "MN", "MI", "MB", "PV", "SO", "VA"),
  slug = c("alessandria", "asti", "biella", "cuneo", "novara", "torino", "verbano-cusio-ossola", "vercelli",
           "bergamo", "brescia", "como", "cremona", "lecco", "lodi", "mantova", "milano", "monza-brianza", "pavia", "sondrio", "varese")
)

# ==============================================================================
# FASE 2: CONNESSIONE AL BROWSER REALE (ANTI-DATADOME DETECTING)
# ==============================================================================
print("🔌 Connessione all'istanza visibile di Google Chrome (Porta 9222)...")
tryCatch({
  chrome_remoto <- chromote::ChromeRemote$new(host = "127.0.0.1", port = 9222)
  chromote_obj  <- Chromote$new(browser = chrome_remoto)
  b             <- ChromoteSession$new(chromote_obj)
}, error = function(e) {
  stop("❌ Errore critico: Impossibile connettersi a Chrome. Verifica di aver lanciato il browser da Terminale con i parametri corretti.")
})

# Contenitore master per accumulare i dati di tutte le province
dataset_immobiliare_master <- data.frame()

# ==============================================================================
# FASE 3: CICLO DI ESTRAZIONE MASSIVA (LOOP)
# ==============================================================================
for(i in 1:nrow(target_province)) {
  
  reg   <- target_province$regione[i]
  prov  <- target_province$provincia_cod[i]
  slug  <- target_province$slug[i]
  
  url <- paste0("https://www.immobiliare.it/mercato-immobiliare/", reg, "/", slug, "-provincia/")
  print(paste("🌐 Navigazione sulla provincia di:", toupper(slug), "(", i, "/", nrow(target_province), ")"))
  
  # Comando di navigazione impartito al browser fisico
  b$Page$navigate(url)
  
  # Pausa precauzionale per caricamento script asincroni e risoluzione Captcha manuale
  Sys.sleep(15) 
  
  # Cattura dell'HTML renderizzato dal browser reale
  runtime_result <- b$Runtime$evaluate("document.documentElement.outerHTML")
  html_grezzo <- runtime_result$result$value
  
  # Inizio parsing testuale del blocco dati
  pagina <- read_html(html_grezzo)
  tutti_i_testi <- pagina %>% html_elements("div") %>% html_text(trim = TRUE)
  stringa_tabella <- tutti_i_testi[str_detect(tutti_i_testi, "^ComuniVendita")]
  
  # Controllo di sicurezza se la tabella viene nascosta o bloccata
  if(length(stringa_tabella) == 0) {
    print(paste("⚠️ Attenzione: Tabella non intercettata per", slug, ". Controlla lo schermo di Chrome per blocchi di sicurezza!"))
    next
  }
  
  # Pulizia intestazione di stringa
  stringa_dati <- str_remove(stringa_tabella[1], "ComuniVendita €/m²Affitto €/m²")
  
  # REGEX UNIVERSALE (Risolve i casi Villanova, Gaglianico, Andorno, Portula)
  # Gestisce decimali mancanti, singoli o doppi e cattura correttamente le variazioni.
  pattern_universale <- "([A-Za-zÀ-ÿ\\s'-]+?)([0-9\\.]+)(\\([^)]+\\))?([0-9]+(?:,[0-9]{1,2})?)(\\([^)]+\\))?"
  
  matrice_estratta <- str_match_all(stringa_dati, pattern_universale)[[1]]
  
  if(nrow(matrice_estratta) > 0) {
    df_provincia <- as.data.frame(matrice_estratta) %>%
      select(Comune = V2, Prezzo_Vendita_Grezzo = V3, Prezzo_Affitto_Grezzo = V5) %>%
      mutate(
        Comune = str_trim(Comune),
        # Trasforma la vendita in intero rimuovendo i punti delle migliaia (es: 1.472 -> 1472)
        Prezzo_Vendita_mq = as.numeric(str_remove_all(Prezzo_Vendita_Grezzo, "\\.")),
        # Trasforma l'affitto in decimale gestendo la virgola italiana (es: 7,09 -> 7.09)
        Prezzo_Affitto_mq = as.numeric(str_replace(Prezzo_Affitto_Grezzo, ",", ".")),
        Provincia = prov,
        Regione = str_to_title(reg),
        Data_Rilevazione = Sys.Date()
      ) %>%
      select(Comune, Prezzo_Vendita_mq, Prezzo_Affitto_mq, Provincia, Regione, Data_Rilevazione)
    
    # Iniezione nel dataset master condivisibile
    dataset_immobiliare_master <- bind_rows(dataset_immobiliare_master, df_provincia)
    print(paste("✅ Compilati con successo", nrow(df_provincia), "comuni per la provincia corrente."))
  }
}

# Chiusura sicura della sessione di automazione remota
b$close()
print(paste("🎉 Scraping completato! Totale database caricato in memoria:", nrow(dataset_immobiliare_master), "comuni."))


# ==============================================================================
# FASE 4: ELABORAZIONE GEOGRAFICA AVANZATA - TUTTO PIEMONTE & LOMBARDIA
# ==============================================================================
print("🗺️ Elaborazione geografica di tutti i confini per Piemonte e Lombardia...")

# 1. Download mirato dei confini geografici (Regione 1 = Piemonte, Regione 3 = Lombardia)
# Questo approccio evita di scaricare l'intera Italia, rimanendo leggerissimo.
url_piemonte  <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_R_1_municipalities.geojson"
url_lombardia <- "https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_R_3_municipalities.geojson"

print("📥 Download mappe regionali da Openpolis in corso...")
mappa_piemonte  <- read_sf(url_piemonte)
mappa_lombardia <- read_sf(url_lombardia)

# Uniamo geometricamente i due fogli mappa regionali in un unico macro-vettore
mappa_macro_regione <- rbind(mappa_piemonte, mappa_lombardia)

# 2. STANDARDIZZAZIONE E DIZIONARIO DI RACCORDO ESTESO PER LE FUSIONI COMUNALI
# Integriamo i raccordi storici del Piemonte e le principali fusioni lombarde recenti
mappa_geografica_pulita <- mappa_macro_regione %>%
  mutate(Comune_Join = str_to_upper(name)) %>%
  
  mutate(Comune_Join = case_when(
    # --- Fusioni Piemonte ---
    Comune_Join %in% c("MOSSO", "SOPRANA", "TRIVERO", "VALLE MOSSO") ~ "VALDILANA",
    Comune_Join %in% c("QUAREGNA", "CERRETO CASTELLO") ~ "QUAREGNA CERRETO",
    Comune_Join %in% c("SAN PAOLO CERVO", "QUITTENGO") ~ "CAMPIGLIA CERVO",
    Comune_Join %in% c("CROSA") ~ "LESSONA",
    
    # --- Principali Fusioni Lombardia (Allineamento database 2026) ---
    Comune_Join %in% c("PIADENA", "DRIZZONA") ~ "PIADENA DRIZZONA",
    Comune_Join %in% c("BORGOFRANCO SUL PO", "CARBONARA DI PO") ~ "BORGOCARBONARA",
    Comune_Join %in% c("SAN GIORGIO DI MANTOVA", "BIGARELLO") ~ "SAN GIORGIO BIGARELLO",
    Comune_Join %in% c("CASASCO D'INTELVI", "CASTIGLIONE D'INTELVI", "SAN FEDELE INTELVI") ~ "CENTRO VALLE INTELVI",
    Comune_Join %in% c("INTREVI SUPERIORE", "PELLIO INTELVI", "RAMPONIO VERNA") ~ "ALTA VALLE INTELVI",
    Comune_Join %in% c("VALLEVE", "FOPPOLO") ~ "VALLEVE", # Esempio di raccordi territoriali
    TRUE ~ Comune_Join
  )) %>%
  
  # GLOBAL GEOMETRIC DISSOLVE: Fonde frammenti staccati ed enclave orfane.
  # CRITICO: Raggruppiamo per Comune E per Provincia per evitare di fondere omonimi distanti!
  group_by(Comune_Join, prov_acr) %>%
  summarise(.groups = "drop")

# 3. PREPARAZIONE DEL JOIN CON IL TUO DATASET MASTER (2.309 RIGHE)
df_prezzi_macro <- dataset_immobiliare_master %>%
  mutate(Comune_Join = str_to_upper(Comune))

# Eseguiamo il Left Join a doppia chiave per la massima precisione chirurgica
mappa_completa_finale <- mappa_geografica_pulita %>%
  left_join(df_prezzi_macro, by = c("Comune_Join" = "Comune_Join", "prov_acr" = "Provincia"))

# 4. AUDIT FINALE SULLA QUALITÀ DEI DATI DI TUTTI I COMUNI
buchi_globali <- mappa_completa_finale %>% filter(is.na(Prezzo_Vendita_mq)) %>% pull(Comune_Join)

print("==========================================================================")
print(paste("📊 STATISTICHE MAPPA COMPLETA:"))
print(paste("   - Poligoni totali generati nella mappa:", nrow(mappa_completa_finale)))
print(paste("   - Comuni senza corrispondenza di prezzo (aree grigie):", length(buchi_globali)))
print("==========================================================================")


# ==============================================================================
# FASE 5: CONFIGURAZIONE MOTORE CARTOGRAFICO ED EMISSIONE MAPPA MACRO
# ==============================================================================
print("🎨 Generazione del rendering della mappa interattiva Leaflet su scala macro...")

# Per evitare che i picchi di Milano appiattiscano visivamente il resto delle regioni,
# spezziamo i prezzi in classi/intervalli intelligenti (Bins) per massimizzare il contrasto.
intervalli_prezzo <- c(0, 500, 750, 1000, 1300, 1700, 2200, 3200, 4500, Inf)

pal_macro <- colorBin(
  palette = "viridis", 
  domain = mappa_completa_finale$Prezzo_Vendita_mq,
  bins = intervalli_prezzo,
  na.color = "#808080"
)

# Compilazione del widget HTML Leaflet
mappa_macro_BRI <- leaflet(mappa_completa_finale) %>%
  # Sfondo cartografico minimale (mette in risalto i tuoi dati)
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Disegno asincrono di tutti i 2.300+ poligoni
  addPolygons(
    fillColor = ~pal_macro(Prezzo_Vendita_mq),
    weight = 1, # Confini più sottili per una visualizzazione pulita ad alto zoom
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.65,
    highlightOptions = highlightOptions(
      weight = 2.5,
      color = "#444",
      dashArray = "",
      fillOpacity = 0.85,
      bringToFront = TRUE
    ),
    popup = ~paste0(
      "<strong>Comune:</strong> ", Comune_Join, " (", prov_acr, ")<br/>",
      "<strong>Prezzo Vendita Medio:</strong> ", ifelse(is.na(Prezzo_Vendita_mq), "Dato non disp.", paste0(Prezzo_Vendita_mq, " €/m²")), "<br/>",
      "<strong>Prezzo Affitto Medio:</strong> ", ifelse(is.na(Prezzo_Affitto_mq), "Dato non disp.", paste0(Prezzo_Affitto_mq, " €/m²"))
    )
  ) %>%
  
  # Posizionamento della legenda dinamica strutturata a blocchi
  addLegend(
    pal = pal_macro, 
    values = ~Prezzo_Vendita_mq, 
    opacity = 0.7, 
    title = "Vendita (€/m²)",
    position = "bottomright"
  )

# Mostra il super-prototipo finale
mappa_macro_BRI
