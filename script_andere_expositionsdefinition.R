# Dataframe
df_data_processed_1 <- read.csv2("data/BA SVB Zeitreihe.csv")
df_data_processed_1
# entferne zeile 89
df_data_processed_1 <- df_data_processed_1[-(89:86), ]
View(df_data_processed_1)


# Daten von Wide Fromat > long Format
install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")


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

View(df_data_processed_long)

# Aggregieren (von Wz2 > Wz1)

df_data_processed_2 <- aggregate(
  Beschäftigung ~ Quartal + Jahr + Code, data = df_data_processed_long , FUN = sum
)

# Beschäftigung Logarithmieren 

df_data_processed_2$log_Beschäftigung <- log(
  df_data_processed_2$Beschäftigung
)

View(df_data_processed_2)

# Exposure Hinzufügen 

df_exposure <- read.csv2("data/Exposition WZ1.csv")

View(df_exposure)

df_data_processed_3 <- join(
  df_data_processed_2, df_exposure, by = "Code", type = "left", match = "all"
)


# Post hinzufügen 

df_data_processed_3$Post <- as.integer(
  df_data_processed_3$Jahr > 2022 
  | (df_data_processed_3$Jahr == 2022 & df_data_processed_3$Quartal >= 4)
)


class(df_data_processed_3)

df_data_processed_3$high_exp <- ifelse(
  df_data_processed_3$Exposure > median(
    df_data_processed_3$Exposure ),1, 0)

View(df_data_processed_3)

Reg_1<- feols(
  fml = log_Beschäftigung ~ Post*high_exp
  | Code + Jahr:Quartal, 
  data = df_data_processed_3,
  cluster = ~Code)

summary(Reg_1)
