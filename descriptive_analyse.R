# Dataframe
df_data_processed_1 <- read.csv2("data/BA SVB Zeitreihe.csv")
df_data_processed_1
# entferne zeile 89
df_data_processed_1 <- df_data_processed_1[-(89:86), ]


# Daten von Wide Fromat > long Format


library(tidyr)
library(dplyr)
library(plyr)
library(stringr)

df_data_processed_long <- df_data_processed_1 %>%
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "Zeit",
    values_to = "Beschäftigung"
  )
# Quartale und Jahr splitten damit Dummy möglich ist 

df_data_processed_long <- df_data_processed_long %>%
  mutate(
    Quartal = str_extract(Zeit, "\\d"),      
    Jahr = str_extract(Zeit, "\\d{4}")         
  )

df_data_processed_long <- df_data_processed_long %>%
  mutate(
    Quartal = as.numeric(Quartal),
    Jahr = as.numeric(Jahr)
  )

# Aggregieren (von Wz2 > Wz1)

df_data_processed_2 <- aggregate(
  Beschäftigung ~ Quartal + Jahr + Code, data = df_data_processed_long , FUN = sum
)

# Beschäftigung Logarithmieren 

df_data_processed_2$log_Beschäftigung <- log(
  df_data_processed_2$Beschäftigung
)


# Exposure Hinzufügen 

df_exposure <- read.csv2("data/Exposition WZ1.csv")

View(df_exposure)

df_data_processed_3 <- join(
  df_data_processed_2, df_exposure, by = "Code", type = "left", match = "all"
)

# deskriptive Analyse
library(stargazer)

summary(df_data_processed_3$Beschäftigung)

df_description <- df_data_processed_3[, c("Beschäftigung","log_Beschäftigung","Exposure")]

stargazer(
  
  df_description,
  
  type = "latex",
  
  summary = TRUE,
  
  out = "outputs/description.tex"
  
)

pdf("outputs/description.pdf",
    width = 7,
    height = 5)
description <- stargazer(
  df_description,
  type = "text",
  summary = TRUE
)
print(description)
dev.off()

# Histogramm von log_Beschäftigung

hist(df_data_processed_3$log_Beschäftigung,
     xlab = "logarithmierte Beschäftigung", ylab = "Häufigkeit",
     main = "Histogramm logarithmierte Beschäftigung", col="steelblue",
     breaks = seq(10, 16, by = 0.5))
     
     
     
