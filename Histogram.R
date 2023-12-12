# Pakete installieren
remotes::install_github("statisticsforsocialscience/hcictools")
install.packages("esquisse")

# Pakete aktivieren
library(hcictools)
library(tidyverse)
rwthcolor <- hcictools::rwth.colorpalette()
df <- dataforsocialscience::robo_care

library(ggplot2)

# Musterlösung Histogramm ----


df %>% 
  ggplot() +
  aes(x = age) +
  geom_histogram(bins = 30L, fill = rwthcolor$petrol) +
  geom_vline(xintercept = mean(df$age)) +
  annotate("text", x=mean(df$age)+2, y=40, label=paste0("M = ", round(mean(df$age), digits = 2)), angle=90) +
  labs(x = "Alter in Jahren", 
       y = "Anzahl", 
       title = "Studentische Stichprobe", 
       subtitle = paste0("Altersverteilung im Histogramm (n=",nrow(df), ")"),
       caption = "Anzahl der Bins: 30") +
  theme_minimal()
ggsave("histogram.png",units = "mm", width = 297, height = 210)

#Histogram gender
ggplot(df) +
  aes(x = gender) +
  geom_bar(fill = "#112446") +
  labs(
    x = " ",
    y = " ",
    title = " ",
    subtitle = " ",
    caption = " "
  ) +
  theme_minimal()

#Histogram age
ggplot(df) +
  aes(x = age) +
  geom_histogram(bins = 30L, fill = "#112446") +
  labs(
    x = " ",
    y = " ",
    title = " ",
    subtitle = " ",
    caption = " "
  ) +
  theme_minimal()

#Histogram job_type
ggplot(df) +
  aes(x = gender) +
  geom_bar(fill = "#112446") +
  labs(
    x = " ",
    y = " ",
    title = " ",
    subtitle = " ",
    caption = " "
  ) +
  theme_minimal()

# Musterlösung Boxplot ----


df %>% 
  filter(gender != "rather not say") %>% 
  ggplot() +
  aes(x = gender, y = cse, fill = gender) +
  geom_boxplot(show.legend = FALSE, width = 0.4) +
  scale_y_continuous(breaks = c(1:6)) +
  scale_x_discrete(labels = c("männlich", "weiblich")) +
  scale_fill_manual(values = c(rwthcolor$lightblue, rwthcolor$red)) +
  labs(x = "Geschlecht", 
       y = "KUT [1-6]", 
       title = "Frauen haben einen niedrigeren KUT als Männer.", 
       subtitle = paste0("Deskriptiver Datenvergleich im Boxplot (n=",nrow(df),")") , 
       caption = "Punkte zeigen Ausreißer (1,5*IQR)") +
  theme_minimal() +
  NULL
ggsave("boxplot.png",units = "mm", width = 297, height = 210)

