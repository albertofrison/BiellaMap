# ==============================================================================
# FASE 4F: INGESTIONE 5° LAYER - ACCESSIBILITÀ SERVIZI REALE
# Fonte: Ministero_Aree_Comuni.xlsx | Tab: Comuni
# Mappatura colonne: Comuni -> Nome, Provincia -> Sigla, Mappa -> Classe Area
# ==============================================================================
print("📖 Caricamento del file Excel locale Ministero_Aree_Comuni.xlsx...")

# Leggiamo il file e il foglio esatto che hai indicato
dati_aree_interne_grezzi <- read_excel("Ministero_Aree_Comuni.xlsx", sheet = "Comuni")

df_servizi_istat <- dati_aree_interne_grezzi %>%
  select(
    Nome_Comune = `Comune`, 
    Codice_Provincia = `Provincia`,
    Categoria_Area = `Mappa`
  ) %>%
  mutate(
    Comune_Join = str_to_upper(str_trim(Nome_Comune)),
    Provincia_Acr = str_to_upper(str_trim(Codice_Provincia)),
    
    # Standardizziamo il testo della colonna Mappa per evitare problemi di maiuscole/minuscole o spazi
    Valore_Mappa = str_to_upper(str_trim(Categoria_Area)),
    
    # TRASFORMAZIONE IN INDICE REALE DI SVILUPPO LOGISTICO (0-100)
    # Gestiamo sia il codice a lettere ISTAT sia la dicitura testuale estesa
    Indice_Servizi_ISTAT = case_when(
      str_detect(Valore_Mappa, "A|POLO") ~ 100,         # Massima accessibilità ai servizi
      str_detect(Valore_Mappa, "B|CINTURA") ~ 85,      # Hinterland immediato
      str_detect(Valore_Mappa, "C|INTERMEDIO") ~ 60,   # Inizio isolamento (20-40 min dai servizi)
      str_detect(Valore_Mappa, "D|PERIFERICO") ~ 40,   # Distante (40-75 min dai servizi)
      str_detect(Valore_Mappa, "E|F|ULTRAPERIFERICO") ~ 15, # Isolamento forte (>75 min dai servizi)
      TRUE ~ 50                                        # Fallback di sicurezza in caso di celle vuote
    )
  ) %>%
  # Teniamo solo le colonne pulite che servono per il Join geografico a doppia chiave
  select(Comune_Join, Provincia_Acr, Indice_Servizi_ISTAT)




# ==============================================================================
# FASE 4F: INGEGNERIA ALGORITMICA - COMPOSITE INDEX & NOTA DI SINTESI DINAMICA
# ==============================================================================
print("🧮 Calcolo dell'Indice Sintetico Globale (Biella Remote Index)...")

mappa_quadrivariata_finale <- mappa_quadrivariata_finale %>%
  mutate(
    # 1. ECONOMIČITÀ (Inversa: più il prezzo è basso, più il punteggio è vicino a 100)
    min_p = min(Prezzo_Vendita_mq, na.rm = TRUE),
    max_p = max(Prezzo_Vendita_mq, na.rm = TRUE),
    Score_Prezzo = ifelse(!is.na(Prezzo_Vendita_mq), 100 * (max_p - Prezzo_Vendita_mq) / (max_p - min_p), 50),
    
    # 2. NATURA (L'indice ISPRA è già una percentuale 0-100, la usiamo nativa)
    Score_Verde = ifelse(!is.na(Indice_Verde_ISPRA), Indice_Verde_ISPRA, 50),
    
    # 3. CONNETTIVITÀ (Anche la fibra AGCOM è già in percentuale 0-100)
    Score_Fibra = ifelse(!is.na(Percentuale_Fibra), Percentuale_Fibra, 0),
    
    # 4. SICUREZZA 🚨 (Inversa: meno reati ci sono, più il punteggio è vicino a 100)
    min_c = min(Reati_100k, na.rm = TRUE),
    max_c = max(Reati_100k, na.rm = TRUE),
    Score_Sicurezza = ifelse(!is.na(Reati_100k), 100 * (max_c - Reati_100k) / (max_c - min_c), 50),
    
    # --- ALGORITMO DI SINTESI FINALE (BRI SCORE) ---
    # Somma pesata (25% a testa per perfetto bilanciamento democratico dei fattori)
    BRI_Score = round((Score_Prezzo + Score_Verde + Score_Fibra + Score_Sicurezza) / 4, 1),
    
    # --- GENERAZIONE DELLA NOTA DI SINTESI DI LIVELLO 4 ---
    # Questo testo HTML verrà iniettato direttamente nel popup di Leaflet
    Nota_Sintesi = paste0(
      "<div style='font-family: Arial, sans-serif; min-width: 220px;'>",
      "<h3 style='margin:0 0 5px 0; color:#2c3e50;'>📍 ", Comune_Join, " (", prov_acr, ")</h3>",
      "<div style='background:#f8f9fa; padding:8px; border-left:4px solid #27ae60; margin-bottom:8px;'>",
      "<strong style='font-size:14px;'>🏆 BRI SCORE: ", BRI_Score, " / 100</strong><br/>",
      "<span style='font-size:11px; color:#7f8c8d;'>Rank: ", 
      case_when(
        BRI_Score >= 75 ~ "🌟 Eccellente (Top Destination)",
        BRI_Score >= 60 ~ "✅ Consigliato (Ottimo bilanciamento)",
        BRI_Score >= 45 ~ "⚖️ Discreto (Compromesso locale)",
        TRUE ~ "⚠️ Sconsigliato (Forti criticità)"
      ),
      "</span>",
      "</div>",
      "<table style='width:100%; font-size:11px; border-collapse: collapse;'>",
      "<tr><td>💰 <strong>Prezzo Casa:</strong></td><td style='text-align:right;'>", ifelse(is.na(Prezzo_Vendita_mq), "N.D.", paste0(Prezzo_Vendita_mq, " €/m²")), "</td></tr>",
      "<tr><td>🌳 <strong>Verde Naturale:</strong></td><td style='text-align:right;'>", round(Indice_Verde_ISPRA, 1), "%</td></tr>",
      "<tr><td>⚡ <strong>Fibra FTTH:</strong></td><td style='text-align:right;'>", round(Percentuale_Fibra, 1), "%</td></tr>",
      "<tr><td>🚨 <strong>Sicurezza (Reati):</strong></td><td style='text-align:right;'>", round(Reati_100k, 0), " /100k ab.</td></tr>",
      "</table>",
      "</div>"
    )
  )

# ==============================================================================
# FASE 5: RENDERING CARTOGRAFICO INTEGRATO A 5 LAYER DEFINTIVO
# ==============================================================================
print("🎨 Compilazione del super-pannello cartografico con Indice di Sintesi...")

# Palette dei singoli strati precedenti
pal_prezzi  <- colorBin(palette = "RdYlBu", domain = mappa_quadrivariata_finale$Prezzo_Vendita_mq, bins = 8, reverse = TRUE, na.color = "#d5dbdb")
pal_verde   <- colorNumeric(palette = c("#7E5109", "#D35400", "#FFFF99", "#113F23", "#0A2F1D"), domain = mappa_quadrivariata_finale$Indice_Verde_ISPRA, na.color = "#d5dbdb")
pal_fibra   <- colorNumeric(palette = "YlGnBu", domain = mappa_quadrivariata_finale$Percentuale_Fibra, na.color = "#d5dbdb")
pal_crimine <- colorNumeric(palette = "OrRd", domain = mappa_quadrivariata_finale$Reati_100k, na.color = "#d5dbdb")

# Nuova Palette per l'Indice Sintetico: Scala divergente classica dal Rosso al Verde (RdYlGn)
pal_bri <- colorNumeric(palette = "RdYlGn", domain = mappa_quadrivariata_finale$BRI_Score, na.color = "#d5dbdb")

# Generazione del Widget Leaflet Totale
mappa_BRI_definitiva <- leaflet(mappa_quadrivariata_finale) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # --- LAYER 1: IL CAPOLAVORO (BIELLA REMOTE INDEX) ---
  # Lo mettiamo per primo così la mappa si apre mostrando subito la sintesi globale
  addPolygons(
    fillColor = ~pal_bri(BRI_Score), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "🏆 BIELLA REMOTE INDEX (Sintesi)",
    popup = ~Nota_Sintesi # <--- Stampiamo la nota di sintesi dinamica nei popup di ogni layer!
  ) %>%
  
  # --- LAYER 2: COSTO IMMOBILI ---
  addPolygons(
    fillColor = ~pal_prezzi(Prezzo_Vendita_mq), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "💰 Prezzi Vendita (€/m²)",
    popup = ~Nota_Sintesi
  ) %>%
  
  # --- LAYER 3: IMPATTO AMBIENTALE ---
  addPolygons(
    fillColor = ~pal_verde(Indice_Verde_ISPRA), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "🌳 Indice del Verde (%)",
    popup = ~Nota_Sintesi
  ) %>%
  
  # --- LAYER 4: INFRASTRUTTURA INTERNET ---
  addPolygons(
    fillColor = ~pal_fibra(Percentuale_Fibra), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "⚡ Copertura Fibra FTTH (%)",
    popup = ~Nota_Sintesi
  ) %>%
  
  # --- LAYER 5: SICUREZZA REALE 🚨 ---
  addPolygons(
    fillColor = ~pal_crimine(Reati_100k), weight = 0.5, color = "white", fillOpacity = 0.8,
    group = "🚨 Indice Criminalità Provinciale",
    popup = ~Nota_Sintesi
  ) %>%
  
  # Cornice protettiva esterna spessa su Biella
  addPolylines(data = confine_esterno_biella, color = "#2c3e50", weight = 4.5, opacity = 1) %>%
  
  # Pannello switch aggiornato a 5 opzioni
  addLayersControl(
    baseGroups = c(
      "🏆 BIELLA REMOTE INDEX (Sintesi)", 
      "💰 Prezzi Vendita (€/m²)", 
      "🌳 Indice del Verde (%)", 
      "⚡ Copertura Fibra FTTH (%)", 
      "🚨 Indice Criminalità Provinciale"
    ),
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  )

# Mostra il motore cartografico finale
mappa_BRI_definitiva

# ==============================================================================
# FASE 4F: INGESTIONE 5° LAYER - ACCESSIBILITÀ SERVIZI REALE
# Fonte: Ministero_Aree_Comuni.xlsx | Tab: Comuni
# Mappatura colonne: Comuni -> Nome, Provincia -> Sigla, Mappa -> Classe Area
# ==============================================================================
print("📖 Caricamento del file Excel locale Ministero_Aree_Comuni.xlsx...")

# Leggiamo il file e il foglio esatto che hai indicato
dati_aree_interne_grezzi <- read_excel("Ministero_Aree_Comuni.xlsx", sheet = "Comuni")

df_servizi_istat <- dati_aree_interne_grezzi %>%
  select(
    Nome_Comune = `Comuni`, 
    Codice_Provincia = `Provincia`,
    Categoria_Area = `Mappa`
  ) %>%
  mutate(
    Comune_Join = str_to_upper(str_trim(Nome_Comune)),
    Provincia_Acr = str_to_upper(str_trim(Codice_Provincia)),
    
    # Standardizziamo il testo della colonna Mappa per evitare problemi di maiuscole/minuscole o spazi
    Valore_Mappa = str_to_upper(str_trim(Categoria_Area)),
    
    # TRASFORMAZIONE IN INDICE REALE DI SVILUPPO LOGISTICO (0-100)
    # Gestiamo sia il codice a lettere ISTAT sia la dicitura testuale estesa
    Indice_Servizi_ISTAT = case_when(
      str_detect(Valore_Mappa, "A|POLO") ~ 100,         # Massima accessibilità ai servizi
      str_detect(Valore_Mappa, "B|CINTURA") ~ 85,      # Hinterland immediato
      str_detect(Valore_Mappa, "C|INTERMEDIO") ~ 60,   # Inizio isolamento (20-40 min dai servizi)
      str_detect(Valore_Mappa, "D|PERIFERICO") ~ 40,   # Distante (40-75 min dai servizi)
      str_detect(Valore_Mappa, "E|F|ULTRAPERIFERICO") ~ 15, # Isolamento forte (>75 min dai servizi)
      TRUE ~ 50                                        # Fallback di sicurezza in caso di celle vuote
    )
  ) %>%
  # Teniamo solo le colonne pulite che servono per il Join geografico a doppia chiave
  select(Comune_Join, Provincia_Acr, Indice_Servizi_ISTAT)

# ==============================================================================
# FUSIONE GEOGRAFICA NEL SUPER-DATABASE PENTAVARIATO CERTIFICATO
# ==============================================================================
print("🔗 Integrazione dell'indice servizi ISTAT nel motore geografico...")

mappa_pentavariata_finale <- mappa_quadrivariata_finale %>%
  as.data.frame() %>%
  # Eseguiamo il left_join usando la doppia chiave Comune + Sigla Provincia (es: BI, TO, MI)
  left_join(df_servizi_istat, by = c("Comune_Join" = "Comune_Join", "prov_acr" = "Provincia_Acr")) %>%
  
  # Ricalcoliamo l'algoritmo del BRI_Score con il peso reale al 20% per ognuno dei 5 KPI
  mutate(
    BRI_Score = round((Score_Prezzo + Score_Verde + Score_Fibra + Score_Sicurezza + Indice_Servizi_ISTAT) / 5, 1),
    
    # AGGIORNAMENTO COMPLETO DELLA NOTA DI SINTESI HTML PER I POPUP
    Nota_Sintesi = paste0(
      "<div style='font-family: Arial, sans-serif; min-width: 240px;'>",
      "<h3 style='margin:0 0 5px 0; color:#2c3e50;'>📍 ", Comune_Join, " (", prov_acr, ")</h3>",
      "<div style='background:#f8f9fa; padding:8px; border-left:4px solid #8e44ad; margin-bottom:8px;'>",
      "<strong style='font-size:14px;'>🏆 BRI SCORE: ", BRI_Score, " / 100</strong><br/>",
      "<span style='font-size:11px; color:#7f8c8d;'>Rank: ", 
      case_when(
        BRI_Score >= 75 ~ "🌟 Eccellente (Top Destination)",
        BRI_Score >= 60 ~ "✅ Consigliato (Ottimo bilanciamento)",
        BRI_Score >= 45 ~ "⚖️ Discreto (Compromesso locale)",
        TRUE ~ "⚠️ Sconsigliato (Forti criticità)"
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
  ) %>%
  st_as_sf()

print("🎯 Dataset Ministero/ISTAT agganciato con successo alla mappa!")
  