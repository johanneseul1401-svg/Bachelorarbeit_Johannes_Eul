
df_data_processed_1 <- read.csv2("data/BA SVB Zeitreihe.csv")
df_data_processed_1
# entferne zeile 89
df_data_processed_1 <- df_data_processed_1[-(89:86), ]

View(df_data_processed_1)

library(tidyr)
library(dplyr)
library(plyr)
library(stringr)

df_data_processed_long <- df_data_processed_1 %>%
  pivot_longer(
    cols = starts_with("Q"),
    names_to = "Zeit",
    values_to = "Beschaeftigung"
  )
?factor(Variable)

# Beschäftigung Logarithmieren 

df_data_processed_long$log_Beschaeftigung <- log(
  df_data_processed_long$Beschaeftigung
)


# Exposure Hinzufügen 

df_exposure <- read.csv2("data/Exposition WZ1.csv")


df_data_processed_2 <- join(
  df_data_processed_long, df_exposure, by = "Code", type = "left", match = "all"
)

View(df_data_processed_2)

# Grenzen für die Terzile berechnen 

terzil_grenzen <- quantile(df_data_processed_2$Exposure, probs = c(0, 1/3, 2/3, 1))

# Werte in Gruppen einteilen und benennen

df_data_processed_3 <- df_data_processed_2 %>%
  mutate(terzil_kat = cut(Exposure, 
                          breaks = terzil_grenzen, 
                          labels = c("Niedrig", "Mittel", "Hoch"), 
                          include.lowest = TRUE))

View(df_data_processed_3)


# Mittelwerte Beschäftigung nach Gruppen 

plot_df <- df_data_processed_3 %>%
  
  group_by(Zeit, terzil_kat) %>%
  
  dplyr:: summarise(
    mean_log_emp = mean(log_Beschaeftigung),
    .groups = "drop"
  )


View(plot_df)

# Plotten 

plot_df$Zeit <- factor(
  plot_df$Zeit,
  
  levels = c(
    "Q1.2019", "Q2.2019", "Q3.2019", "Q4.2019",
    "Q1.2020", "Q2.2020", "Q3.2020", "Q4.2020",
    "Q1.2021", "Q2.2021", "Q3.2021", "Q4.2021",
    "Q1.2022", "Q2.2022", "Q3.2022", "Q4.2022",
    "Q1.2023", "Q2.2023", "Q3.2023", "Q4.2023",
    "Q1.2024", "Q2.2024", "Q3.2024", "Q4.2024"
  )
)

library(ggplot2)

pdf("outputs/beschäftigung_plot.pdf",
    
    width = 7,
    
    height = 5)

employment_plot <- ggplot(plot_df, aes(
  x = Zeit, y = mean_log_emp, group = terzil_kat, color = terzil_kat
  )) +
  geom_line() +
  theme_minimal() + 
  scale_color_manual(values = c("red", "blue", "green")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_vline(
    xintercept = 16,
    color = "grey",
    linetype = "dashed"
  )
print(employment_plot)
dev.off()