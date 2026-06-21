# Dataframe
df_data_processed_1 <- read.csv2("data/BA SVB Zeitreihe 2017-2024.csv")
df_data_processed_1
# entferne zeile 89
df_data_processed_1 <- df_data_processed_1[-(89:86), ]
View(df_data_processed_1)


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

View(df_data_processed_long)

class(df_data_processed_long$Beschäftigung)

as.numeric(df_data_processed_long$Beschäftigung)

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
  df_data_processed_3$Jahr > 2021 
  | (df_data_processed_3$Jahr == 2021 & df_data_processed_3$Quartal >= 2)
)

View(df_data_processed_3)


## Regression

library(fixest)
library(modelsummary)

reg_antizipation <- feols(
  fml = log_Beschäftigung ~ Exposure*Post 
  | Code + Jahr:Quartal, 
  data = df_data_processed_3,
  cluster = ~Code)

summary(reg_antizipation)


modelsummary(
  
  reg_antizipation,
  
  stars = c(
    
    '*' = .10,
    
    '**' = .05,
    
    '***' = .01
    
  ),
  
  statistic = "std.error",
  
  coef_map = c(
    
    "Exposure" = "Treatment",
    
    "Post" = "Post",
    
    "Exposure:Post" = "Treatment × Post"
    
  ),
  
  gof_omit = "IC|Log|Adj",
  
  title = "Antizipationstest",
  
  output = "outputs/reg_antizipation.tex",
  
  latex = TRUE
  
)