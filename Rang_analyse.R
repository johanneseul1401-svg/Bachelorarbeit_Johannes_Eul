df_rang_vergleich  <- read.csv2("data/datensatzvse2014:18.csv")

df_rang_vergleich$Niedriglohnanteil2018 <- df_rang_vergleich$Niedriglohn2018 / df_rang_vergleich$Beschäftung.2018

View(df_rang_vergleich)

corr_exp2014_Nl2018 <- cor(df_rang_vergleich$Exposure.2014, df_rang_vergleich$Niedriglohnanteil2018, method = "spearman")

corr_exp2014_Nl2014 <- cor(df_rang_vergleich$Exposure.2014, df_rang_vergleich$Niedriglohnanteil.2014, method = "spearman")

print(corr_exp2014_Nl2018)

print(corr_exp2014_Nl2014)