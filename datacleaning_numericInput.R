# Pakete laden ----
library(tidyverse)
library(psych)
library(jmv)
source("qualtricshelpers.R")

# Daten einlesen ----

filename <- "data/Umfragedaten090124.csv"
raw <- load_qualtrics_csv(filename)

# Zeilen entfernen ----

raw %>% 
  filter(Status == 0) %>% 
  filter(Progress == 100) -> raw

# Spalten entfernen ----

raw.short <- raw[,c(-38:-45,-64:-82,-89:-104,-111:-133,-152,-153,-1:-18,-25:-27,-29, -47:-63, -147:-151)]


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
  recode(`2`= "männlich", `1` = "weiblich", `3`="divers") %>% 
  as.factor() -> raw.short$gender

# #raw.short$branch %>% 
#  # recode(`1`="Forschung und Entwicklung",
#   #       `2`="Finanzen, Versicherungen und Immobilien",
#    #      `3`="Lehre", 
#     #     `4`="Medizin (Pharma und Gesundheit)",
#      #    `5`="Dienstleistungen und Handwerk",
#       #   `6`="Freizeit", 
#     #     `7`="Gesellschaft", 
#     #     `8`="Agrarwissenschaft", 
#          `9`="Technik", 
#          `10`="Sonstiges") %>% 
#   as.factor() -> raw.short$branch

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



raw.short$urban %>% 
  ordered(levels = c(1:3),
          labels = c("Stadt",
                     "Vorstadt / Kleinstadt",
                     "Ländlich")) -> raw.short$urban

raw.short$ef %>% 
  ordered(levels = c(1:6),
          labels = c("vegan",
                     "vegetarisch",
                     "omni",
                     "omni",
                     "omni",
                     "omni")) -> raw.short$ef


# Qualitätskontrolle ----

#speederlimit <- median(raw.short$`Duration (in seconds)`) / 3
#raw.short <- filter(raw.short, `Duration (in seconds)` > speederlimit)

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
  

## Feedback JRH: Mit der Schlüselliste bin ich leider noch nicht so zufrieden:
## - SMK ist keine Skala im engeren Sinne. Hier passiert aber kein inhaltlicher Fehler. Sie können das so lassen, solange Sie im Hinterkopf behalten, dass die Skala formativ ist. 
## - PO und SPP machen als Skala keinen Sinn. Die Sympathiewerte für einzelne Parteien haben ja keinen gemeinsamen dahinterliegenden Faktor. Ich würde Ihnen empfehlen, im Codebook die Items in symp_spd usw. umzubenennnen. 
## - In MV fassen Sie sehr viele unterschiedliche Items in einem Topf. Autobesitz in 3 Stufen, wie oft man ein Lastenfahrad benutzt in 7 Stufen, wie beliebt das Auto im Vergleich zum Taxi ist usw.
## - Ich würde ihnen empfehlen, PO, SPP und MV einfach zu löschen. Sie haben zu diesen Faktoren auch gar keine Hypothesen. 
## - PBE und BEK passt.
## - Es fehlt ein Faktor für linke und rechte politische Orientierung. 

schluesselliste <- list(
  SMK = c("sm1_1", "sm1_2", "sm1_3", "sm1_4", "sm1_5", "sm1_6", "sm1_7"),
  PO = c("orientation2_1", "orientation2_2", "orientation2_3", "orientation2_4"),
  PBE = c("bedrohung1", "-bedrohung2", "bedrohung3", "bedrohung4","bedrohung5", "bedrohung6"),
  BEK = c("bereitschaft1", "bereitschaft2", "bereitschaft3", "bereitschaft4", "bereitschaft5", "bereitschaft6")
)


scores <- scoreItems(schluesselliste, items = raw.short, missing = TRUE, min = 1, max = 6)

scores$alpha

data <- bind_cols(raw.short, as_tibble(scores$scores))

# Lösung abspeichern ----

# data %>% 
#   select(-starts_with("bf", ignore.case = F)) %>% 
#   select(-starts_with("ati", ignore.case = F)) %>%
#   select(-starts_with("wrf", ignore.case = F)) %>%
#   select(-starts_with("will", ignore.case = F)) %>%
#   select(-starts_with("abi", ignore.case = F)) %>%
#   select(-starts_with("commi", ignore.case = F)) %>%
#   select(-starts_with("motiv", ignore.case = F)) %>%
#   select(-starts_with("dark", ignore.case = F)) -> data

saveRDS(data, "data/dataFromNumeric.rds")


# Age Histogramm


library(ggplot2)

ggplot(raw.short) +
 aes(x = age) +
 geom_histogram(bins = 30L, fill = "#112446") +
 labs(x = "Alter in Jahren", y = "Anzahl", 
 title = "Stichprobe", subtitle = paste0("Altersverteilung im Histogramm (n=",nrow(raw.short), ")") )+
 theme_minimal()


