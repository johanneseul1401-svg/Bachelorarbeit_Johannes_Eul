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


df_data_processed_3 <- join(
  df_data_processed_2, df_exposure, by = "Code", type = "left", match = "all"
)
install.packages("xtable")
library(xtable)

print(
  
  xtable(df_exposure),
  
  file = "outputs/exposure.tex",
  
  include.rownames = FALSE
  
)

# Post hinzufügen 

df_data_processed_3$Post <- as.integer(
  df_data_processed_3$Jahr > 2022 
  | (df_data_processed_3$Jahr == 2022 & df_data_processed_3$Quartal >= 4)
)


summary(df_data_processed_3$Beschäftigung)

View(df_data_processed_3)

## Regression

library(fixest)
library(modelsummary)

Reg_1 <- feols(
  fml = log_Beschäftigung ~ Exposure*Post 
  | Code + Jahr:Quartal, 
  data = df_data_processed_3,
  cluster = ~Code)

summary(Reg_1)

confint(Reg_1)

# Regressionstabelle für Latex 

modelsummary(
  
  Reg_1,
  
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
  
  title = "Difference-in-Differences Regression",
  
  output = "outputs/reg1.tex",
  
  latex = TRUE
  
)


# neue Spalte "Event time" erstellen 

df_data_processed_3 <- df_data_processed_3 %>%
  mutate(Event_Time = (Jahr - 2022)*4 + (Quartal - 4))

View(df_data_processed_3)
# Event study 


event_study <- feols(
  log_Beschäftigung ~ i(
    Event_Time, Exposure, ref = -1
  ) 
  | Code + Jahr:Quartal, data = df_data_processed_3,
  cluster = ~Code )

summary(event_study)


pdf("outputs/eventstudy.pdf",
    
    width = 7,
    
    height = 5)

iplot(event_study)

dev.off()
