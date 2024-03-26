install.packages("DALEX")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("carData")
install.packages("palmerpenguins")
install.packages("tidyverse")
library(palmerpenguins)
library(tidyverse)
library(DALEX)
data("apartments")
str(apartments)
library(dplyr)
library(ggplot2)
library(carData)

install.packages("gridExtra")

library(gridExtra)
data("Salaries")
ggplot(Salaries, aes(x = salary, fill = rank)) + 
  geom_histogram(color = "black")+
  labs(x = "Maaş", 
       y = "Frekans",
       title = "Ünvana Göre Maaş Dağılımı",
       subtitle = "Yığılmış Histogram",
       fill = "Ünvan") + 
  scale_fill_discrete(labels = c("Dr. Öğretim Üyesi", "Doçent", "Profesor")) 
 
ggplot(Salaries, aes(x = salary, fill = rank)) + 
  geom_density(alpha = 0.5)+
  labs(x = "Maaş", 
       y = "Frekans",
       title = "Ünvana Göre Maaş Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Ünvan") + 
  scale_fill_discrete(labels = c("Dr. Öğretim Üyesi", "Doçent", "Profesor")) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

  
  
  install.packages("rpart")
library(rpart)  
data("cu.summary")
str(cu.summary)  
ggplot(cu.summary, aes(x = Price, fill = Type)) + 
  geom_histogram(color = "black")+
  labs(x = "Ortalama Araç Fiyatı", 
       y = "Frekans",
       title = "Araç Tipine Göre Ortalama Araç Fiyatı Dağılımı",
       subtitle = "Yığılmış Histogram",
       fill = "Araç Tipi") + 
  scale_fill_discrete(labels = c("Kompakt", "Geniş", "Midyum", "Küçük", "Spor", "Van"))


ggplot(cu.summary, aes(x = Price, fill = Type)) + 
  geom_density(alpha = 0.5)+
  labs(x = "Ortalama Araç Fiyatı", 
       y = "Frekans",
       title = "Araç Tipine Göre Ortalama Araç Fiyatı Dağılımı",
       subtitle = "Kernel Yoğunluk Tahmini",
       fill = "Araç Tipi") + 
  scale_fill_discrete(labels = c("Kompakt", "Geniş", "Midyum", "Küçük", "Spor", "Van")) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

filmler <- data.frame(isim = c("Star Wars",
                               "Jumanji",
                               "Pitch Perfect 3",
                               "Greatest Showman",
                               "Ferdinand"),
                      hasılat = c(71565498,
                                  36169328,
                                  19928525,
                                  8805843,
                                  7316746))
summary(filmler)
str(filmler)
glimpse(filmler)
library(ggplot2)

ggplot(filmler, aes(x = reorder (isim, +hasılat), y = hasılat)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma)

ggplot(filmler, aes(x = reorder (isim, +hasılat), y = hasılat)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

ggplot(filmler, aes(x = reorder(isim, +hasılat), y = hasılat)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  labs(x = "Film",
       y = "Hasılat ($)",
       title = "22-24 Aralık 2017 Tarihlerinde En Çok Hasılat Yapan Filmler",
       caption = "Veri Kaynağı: Box Office Mojo") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggplot(filmler, aes(x = reorder(isim, +hasılat), y = hasılat)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  labs(x = "Film",
       y = "Hasılat ($)",
       title = "22-24 Aralık 2017 Tarihlerinde En Çok Hasılat Yapan Filmler",
       caption = "Veri Kaynağı: Box Office Mojo") + 
  theme_bw()+
  coord_flip()

ggplot(filmler, aes(x = reorder(isim, +hasılat), y = hasılat)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  labs(x = "",
       y = "Hasılat ($)",
       title = "22-24 Aralık 2017 Tarihlerinde En Çok Hasılat Yapan Filmler",
       caption = "Veri Kaynağı: Box Office Mojo") + 
  theme_bw()+
  coord_flip()

Salaries
Salaries$sex

library(dplyr)

sal_2 <- Salaries %>%
  drop_na() %>%
  group_by(sex) %>%
  summarise(ort_gelir = mean(salary))

ggplot(sal_2, aes(x = sex, y = ort_gelir)) +
  geom_bar(stat = "identity") +
  labs(x = "Cinsiyet",
       y = "Ortalama Gelir ($)",
       title = "Akademisyenlerin Cinsiyetine Göre Ortalama Gelirleri",
       caption = "Veri: carData") +
  theme_bw()

ggplot(sal_2, aes(x = reorder (sex, + ort_gelir), y = ort_gelir)) +
  geom_bar(stat = "identity") +
  labs(x = "Cinsiyet",
       y = "Ortalama Gelir ($)",
       title = "Akademisyenlerin Cinsiyetine Göre Ortalama Gelirleri",
       caption = "Veri: carData") +
  theme_bw()

gruplanmis_cubuk <- Salaries %>%
  drop_na() %>%
  filter(rank %in% c("Prof", "AsstProf", "AssocProf")) %>%
  group_by(sex, rank) %>%
  summarise(ortalama_gelir = mean(salary))

ggplot(gruplanmis_cubuk, aes(x = sex, y = ortalama_gelir, fill = rank)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x= "Cinsiyet",
       y= "Ortalama Gelir ($)",
       fill = "Unvan",
       title = "Akademisyenlerin Ünvan ve Cinsiyetlerine Göre Ortalama Gelirleri",
       caption = "veri: carData")+
  theme_bw()+
  scale_y_continuous(labels = scales::comma)

library(rpart)  
data(cu.summary)

grup_cu_summary <- cu.summary %>%
  drop_na() %>%
  group_by(Reliability) %>%
  summarise(ortalama_arac_fiyati = mean(Price))

ggplot(grup_cu_summary, aes(x = Reliability, y = ortalama_arac_fiyati))+
  geom_bar(stat = "identity")+
  labs(x = "Araç Güvenirlik Düzeyi",
       y = "Ortalama Araç Fiyatı ($)",
       title = "Araç Güvenirlik Düzeyine Göre Ortalama Araç Fiyatları",
       caption = "Veri:rpart")+
  theme_bw()
  
grup_cu_2 <- cu.summary %>%
  drop_na() %>%
  filter(Type %in% c("Small", "Sporty", "Compact", "Medium", "Large", "Van")) %>%
  group_by(Reliability, Type) %>%
  summarise(ort_fiyat = mean(Price))

ggplot(grup_cu_2, aes(x = Reliability, y= ort_fiyat, fill = Type))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "Araç Güvenirlik Düzeyi",
       y = "Ortalama Araç Fiyatı ($)",
       fill = "Araç Türü",
       title = "Araç Güvenirlik Düzeyi ve Araç Türüne Ortalama Fiyatlar",
       caption = "Veri:rpart")+
  theme_bw()
