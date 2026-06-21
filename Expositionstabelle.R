df_exposure <- read.csv2("data/Exposition WZ1.csv")

View(df_exposure)

df_Wz1klassifizierung <- c("Land- und Forstwirtschaft, Fischerei",
                           "Bergbau und Gewinnung von Steinen und Erden",
                           "Verarbeitendes Gewerbe",
                           "Energieversorgung",
                           "Abwasser- und Abfallentsorgung und Beseitigung von Umweltverschmutzungen",
                           "Baugewerbe",
                           "Handel",
                           "Verkehr und Lagerei",
                           "Gastgewerbe",
                           "Telekommunikation und IT",
                           "Erbringung von Finanz- und Versicherungsdienstleistungen",
                           "Grundstücks- und Wohnungswesen",
                           "Erbringung von wissenschaftlichen und technischen Dienstleistungen",
                           "Erbringung von sonstigen wirtschaftlichen Dienstleistungen",
                           "Öffentliche Verwaltung, Verteidigung; Sozialversicherung",
                           "Erziehung und Unterricht",
                           "Gesundheits- und Sozialwesen",
                           "Kunst, Sport und Erholung",
                           "Erbringung von sonstigen Dienstleistungen"
                          )

df_exposure <- cbind(
  
  WZ1_klassifizierung = df_Wz1klassifizierung,
  
  df_exposure
  
)

View(df_exposure)
install.packages("xtable")
library(xtable)

print(
  
  xtable(df_exposure),
  
  file = "outputs/exposure.tex",
  
  include.rownames = FALSE
  
)

