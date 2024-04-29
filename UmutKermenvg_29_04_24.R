data <- read.csv("https://raw.githubusercontent.com/mcavs/data/main/results.csv")

install.packages("dplyr")
library(dplyr)

# veri manipulasyonu icin dplyr paketine ihtiyac??m var


# 5 metrigin skorlarini cekecegim
# Daha sonra veri setlerini birlestirecegim

filtered_validity <- data %>%
  filter (Quality_Metric == "Validity")
metric_1 <- rename(filtered_validity,
                   M1 = "Values")

filtered_proximity <- data %>%
  filter (Quality_Metric == "Proximity")

metric_2 <- rename(filtered_proximity,
                   M2 = "Values")
m2 <- subset(metric_2, select = -c(CE_Method, Used_Model, Quality_Metric))


filtered_sparsity <- data %>%
  filter (Quality_Metric == "Sparsity")

metric_3 <- rename(filtered_sparsity,
                   M3 = "Values")
m3 <- subset(metric_3, select = -c(CE_Method, Used_Model, Quality_Metric))

filtered_plausibility <- data %>%
  filter (Quality_Metric == "Plausibility")

metric_4 <- rename(filtered_plausibility,
                   M4 = "Values")
m4 <- subset(metric_4, select = -c(CE_Method, Used_Model, Quality_Metric))


filtered_minimality <- data %>%
  filter (Quality_Metric == "minimality")

metric_5 <- rename(filtered_minimality,
                   M5 = "Values")
m5 <- subset(metric_5, select = -c(CE_Method, Used_Model, Quality_Metric))

# 5 metric degeri cekildi. subset alinarak birlestirilmeye hazir hale geldi

birlestirilmis_data <- cbind(metric_1, m2, m3, m4, m5) 

#metric degerleri sutunlara eklendi. datasetler birlestirildi.

birlestirilmis_data$Ortalama_Score <- (birlestirilmis_data$M1 + birlestirilmis_data$M2 + birlestirilmis_data$M3 + birlestirilmis_data$M4 + birlestirilmis_data$M5) / 5

# 5 farkl?? metrigin ortalamasi alindi ve data setin sagina eklendi

birlestirilmis_data %>%
  group_by(CE_Method, Used_Model) %>% 
  summarise_at(vars("Ortalama_Score"), mean)

# Grup iclerindeki ortalama skorlar hesaplandi
# CE_Method Used_Model    Ortalama_Score
<chr>     <chr>                  <dbl>
  1 MOC       Decision tree          0.304
2 MOC       Extratrees             0.350
3 MOC       Randomforest           0.355
4 NICE      Decision tree          0.206
5 NICE      Extratrees             0.286
6 NICE      Randomforest           0.212
7 WhatIf    Decision tree          1.81 
8 WhatIf    Extratrees             2.22 
9 WhatIf    Randomforest           2.32 

# grup iclerinde ortalama skorlar


gorsellestirme <- data.frame(Yontem = rep(c("MOC", "NICE", "WhatIf"), each = 3),
                             Model = rep(c("Extratrees", "Random Forest", "Decision Tree"),3),
                             Skor = c(0.304, 0.350, 0.355, 0.206, 0.286, 0.212, 1.81, 2.22, 2.32))
# Yontem, Model ve Skorlar gorsellestirme icin data frame haline getirildi.                            

install.packages("ggplot2")
library(ggplot2)
# gorsellestirme icin ggplot2 paketine ihtiyacim var

ggplot(gorsellestirme, aes(x = Model, y = Skor, group = Yontem)) +
  labs(x = "Makine ????renmesi Modeli",
       y = "Skor",
       title = "Kullan??lan Y??nteme G??re Makine ????renmesi Modellerine Ait Ortalama Skor De??i??imleri",
       fill = "Y??ntem") +
  geom_line(aes(color = Yontem)) +
  geom_point(aes(color = Yontem)) +
  scale_color_brewer(palette="Dark2") +
  theme_bw()
