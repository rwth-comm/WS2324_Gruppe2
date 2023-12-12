# Pakete laden ----

library(tidyverse)
library(psych)
source("qualtricshelpers.R")

# Daten einlesen ----

filename <- "data/Testdaten_numeric.csv"
raw <- load_qualtrics_csv(filename)

# Zeilen entfernen ----

raw %>% 
  #filter(Status == 0) %>% 
  filter(Progress == 100) -> raw

# Spalten entfernen ----

raw.short <- raw[,c(-38:-45,-64:-82,-89:-104,-111:-133,-152,-153,-1:-18)]

# Spalten umbenennen ----

generate_codebook(raw.short, filename, "data/codebook.csv")
codebook <- read_codebook("data/codebook_final.csv")
names(raw.short) <- codebook$variable

# Richtige Datentypen zuordnen ----

#raw.short[188,]$age = "55" 
#raw.short[266,]$age = "39"
#raw.short[285,]$age = "59"

raw.short$age <- as.numeric(raw.short$age)

raw.short$gender %>% 
  recode(`1`= "männlich", `2` = "weiblich", `3`="divers") %>% 
  as.factor() -> raw.short$gender

#raw.short$branch %>% 
  recode(`1`="Forschung und Entwicklung",
         `2`="Finanzen, Versicherungen und Immobilien",
         `3`="Lehre", 
         `4`="Medizin (Pharma und Gesundheit",
         `5`="Dienstleistungen und Handwerk",
         `6`="Freizeit", 
         `7`="Gesellschaft", 
         `8`="Agrarwissenschaft", 
         `9`="Technik", 
         `10`="Sonstiges") %>% 
  as.factor() -> raw.short$branch

raw.short$edu %>% 
  ordered(levels = c(1:8),
          labels = c("noch Schüler*in",
                     "von der Schule abgegangen ohne Schulabschluss",
                     "Hauptschulabschluss oder gleichwertiger Abschluss",
                     "Realschulabschluss (mittlere Reife) oder gleichwertiger Abschluss",
                     "(Fach-)Hochschulreife (Abitur)",
                     "(Fach-)Hochschulabschluss (z.B. Bachelor, Master, Diplom)",
                     "Promotion",
                     "einen anderen Bildungsabschluss, und zwar")) -> raw.short$edu

raw.short$jobtype %>% 
  ordered(levels = c(1:6),
          labels = c("vollzeiterwerbstätig",
                     "teilzeiterwerbstätig",
                     "in beruflicher Ausbildung/Lehre",
                     "Student*in",
                     "Schüler*in",
                     "nicht erwerbstätig")) -> raw.short$jobtype


raw.short$income %>% 
  ordered(levels = c(1:12),
          labels = c("bis 450 €",
                     "451 bis 1000 €",
                     "1001 bis 1500 €", 
                     "1501 bis 2000 €",
                     "2001 bis 2500 €",
                     "2501 bis 3000 €",
                     "3001 bis 3500 €", 
                     "3501 bis 4000 €",
                     "4001 bis 4500 €",
                     "4501 bis 5000 €",
                     "über 5000 €",
                     "keine Angabe")) -> raw.short$income

raw.short$urban %>% 
  ordered(levels = c(1:3),
          labels = c("Stadt",
                     "Vorstadt / Kleinstadt",
                     "Ländlich")) -> raw.short$urban



# Qualitätskontrolle ----

speederlimit <- median(raw.short$`Duration (in seconds)`) / 3
raw.short <- filter(raw.short, `Duration (in seconds)` > speederlimit)

# Skalen berechnen ----

#schluesselliste <- list(
  #BF_Extraversion = c("-bf_1n","bf_6"),
  #BF_Agreeableness = c("bf_2","-bf_7n", "bf_11"),
  #BF_Openness = c("-bf_5n", "bf_10"),
  #BF_Neuroticism = c("-bf_4n", "bf_9"),
  #BF_Concientiousness= c("-bf_3n", "bf_8"),
  #ATI = vars4psych(raw.short, "ati"),
  #M_Identification = c("motivation_1","motivation_7","motivation_14"),
  #M_ExternalRegulaition = c("motivation_2","motivation_9","motivation_16"),
  #M_Amotivation = c("motivation_3","motivation_12","motivation_17"),
  #M_Intrinsic = c("motivation_4","motivation_8","motivation_15"),
  #M_Integration = c("motivation_5","motivation_10","motivation_18"),
  #M_Introjection = c("motivation_6","motivation_11","motivation_13"),
  #RF_Promotion = c("wrfq_1","wrfq_2","wrfq_3","wrfq_4","wrfq_9"),
  #RF_Prevention = c("wrfq_5","wrfq_6","wrfq_7","wrfq_8"),
  #Change_GeneralWillingness = vars4psych(raw.short, "willi"),
  #Change_Ability = vars4psych(raw.short, "abi"),
  #Change_SpecificWillingness = vars4psych(raw.short, "willi"),
  #DT_Machiavellianism = c("dark tetrad_1","dark tetrad_6","dark tetrad_9","dark tetrad_10","dark tetrad_11"),
  #DT_Narcissism = c("dark tetrad_3","-dark tetrad_7n","-dark tetrad_15n"),
  #DT_Psychopathy = c("dark tetrad_2","-dark tetrad_4n","-dark tetrad_8n","dark tetrad_13"),
  #DT_Sadism = c("dark tetrad_5","dark tetrad_10","dark tetrad_14","dark tetrad_16"))
  
schluesselliste_final <- list(
  SMK = c("sm1_1", "sm1_2", "sm1_3", "sm1_4", "sm1_5", "sm1_6", "sm1_7"),
  EF = c("ef"),
  PO = c("orientation1_1", "orientation1_2", "orientation1_3", "orientation1_4", "orientation1_5", "orientation1_6", "orientation1_7", "orientation1_8", "orientation2_1", "orientation2_2", "orientation2_3", "orientation2_4", "activism1", "activism2", "sciencescepticism_1", "sciencescepticism_2", "sciencescepticism_3"),
  SPP = c("orientation1_1", "orientation1_2", "orientation1_3", "orientation1_4", "orientation1_5", "orientation1_6", "orientation1_7", "orientation1_8"),
  GN = c("gender"),
  WO = c("urban", "SD8"),
  MV = c("auto1", "auto2", "auto3", "freq_transport_1", "freq_transport_2", "freq_transport_3", "freq_transport_4", "freq_transport_5", "freq_transport_6", "freq_transport_7", "like_transport_1", "like_transport_2", "like_transport_3", "like_transport_4","like_transport_5", "like_transport_6","like_transport_7"),
  PBE = c("bedrohung1", "bedrohung2", "bedrohung3", "bedrohung4","bedrohung5", "bedrohung6"),
  BEK = c("bereitschaft1", "bereitschaft2", "bereitschaft3", "bereitschaft4", "bereitschaft5", "bereitschaft6"),
  AGE = c("age")
  
)


scores <- scoreItems(schluesselliste, items = raw.short, missing = TRUE, min = 1, max = 6)

data <- bind_cols(raw.short, as_tibble(scores$scores))

# Lösung abspeichern ----

data %>% 
  select(-starts_with("bf", ignore.case = F)) %>% 
  select(-starts_with("ati", ignore.case = F)) %>%
  select(-starts_with("wrf", ignore.case = F)) %>%
  select(-starts_with("will", ignore.case = F)) %>%
  select(-starts_with("abi", ignore.case = F)) %>%
  select(-starts_with("commi", ignore.case = F)) %>%
  select(-starts_with("motiv", ignore.case = F)) %>%
  select(-starts_with("dark", ignore.case = F)) -> data

saveRDS(data, "data/dataFromNumeric.rds")

