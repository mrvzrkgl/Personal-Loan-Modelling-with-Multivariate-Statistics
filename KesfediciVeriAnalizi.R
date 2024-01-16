
# Kesfedici Veri Analizi

bank <- read.csv("Bank_Personal_Loan_Modelling.csv")
View(bank)



colnames(bank)[colnames(bank) == "ID"] <- "Kimlik"
colnames(bank)[colnames(bank) == "Age"] <- "Yas"
colnames(bank)[colnames(bank) == "Experience"] <- "Deneyim"
colnames(bank)[colnames(bank) == "Income"] <- "Gelir(Yillik)"
colnames(bank)[colnames(bank) == "ZIP.Code"] <- "PostaKodu"
colnames(bank)[colnames(bank) == "Family"] <- "AileFerdi"
colnames(bank)[colnames(bank) == "CCAvg"] <- "KKHarcama"
colnames(bank)[colnames(bank) == "Education"] <- "Egitim"
colnames(bank)[colnames(bank) == "Mortgage"] <- "IpotekMiktari"
colnames(bank)[colnames(bank) == "Personal.Loan"] <- "BireyselKredi"
colnames(bank)[colnames(bank) == "Securities.Account"] <- "MenkulKiymetHesabi"
colnames(bank)[colnames(bank) == "CD.Account"] <- "MevduatHesapSertifikasi"
colnames(bank)[colnames(bank) == "Online"] <- "CevrimiciBankacilik"
colnames(bank)[colnames(bank) == "CreditCard"] <- "KrediKarti"


bank$Kimlik <- NULL
bank$PostaKodu <- NULL

bank$BireyselKredi <- factor(bank$BireyselKredi, levels = c(0,1), 
                             labels = c("Hayir", "Evet"))


bank$MenkulKiymetHesabi <- factor(bank$MenkulKiymetHesabi, levels = c(0,1), 
                                  labels = c("Hayir", "Evet"))


bank$MevduatHesapSertifikasi <- factor(bank$MevduatHesapSertifikasi, levels = c(0,1), 
                                       labels = c("Hayir", "Evet"))

bank$CevrimiciBankacilik <- factor(bank$CevrimiciBankacilik, levels = c(0,1), 
                                   labels = c("Hayir", "Evet"))


bank$KrediKarti <- factor(bank$KrediKarti, levels = c(0,1), 
                          labels = c("Hayir", "Evet"))


bank$Egitim <- factor(bank$Egitim, levels = c(1,2,3), 
                      labels = c("Lisans", "Yuksek Lisans", "Doktora"))


table(bank$AileFerdi)

bank$AileFerdi <- factor(bank$AileFerdi, levels = c(1,2,3,4), 
                         labels = c("Kucuk", "Kucuk", "Orta", "Buyuk"))


library(dplyr)

set.seed(1234436264)

bank_orneklem <- bank %>%
  group_by(AileFerdi, Egitim, BireyselKredi) %>%
  slice_sample(n = 100) %>%
  ungroup() %>%
  sample_n(300)

View(bank_orneklem)


bank_orneklem <- bank_orneklem[(bank_orneklem$Deneyim != -2) & (bank_orneklem$Deneyim != -1), ]


boxplot(bank_orneklem$Yas, main = "Yas", col = "cadetblue")
boxplot(bank_orneklem$Deneyim, main = "Deneyim", col = "cadetblue")
boxplot(bank_orneklem$`Gelir(Yillik)`, main = "Gelir(Yillik)", col = "cadetblue")
boxplot(bank_orneklem$KKHarcama, main = "KKHarcama", col = "cadetblue")
boxplot(bank_orneklem$IpotekMiktari, main = "IpotekMiktari", col = "cadetblue")


qqnorm(bank_orneklem$KKHarcama, col = "darkblue", main = "Aylık Kredi Kartı Harcaması QQ Plot")
qqline(bank_orneklem$KKHarcama, col = "red")


qqnorm(bank_orneklem$IpotekMiktari, col = "darkblue", main = "İpotek Miktarı QQ Plot")
qqline(bank_orneklem$IpotekMiktari, col = "red")


hist(bank_orneklem$KKHarcama, main = "KKHarcama", col = "orange")
hist(bank_orneklem$IpotekMiktari, main = "IpotekMiktari", col = "orange")


bank_orneklem$KKHarcama <- bank_orneklem$KKHarcama + 1
bank_orneklem$IpotekMiktari <- bank_orneklem$IpotekMiktari + 1


bank_orneklem$KKHarcama <- log10(bank_orneklem$KKHarcama)
bank_orneklem$IpotekMiktari <- log10(bank_orneklem$IpotekMiktari)


boxplot(bank_orneklem$KKHarcama, main = "KKHarcama", col = "cadetblue")
boxplot(bank_orneklem$IpotekMiktari, main = "IpotekMiktari", col = "cadetblue")

summary(bank_orneklem)

library(moments)

cat("Yaş değişkeni carpıklık katsayısı:", skewness(bank_orneklem$Yas), "\n")
cat("Yaş değişkeni basıklık katsayısı:", kurtosis(bank_orneklem$Yas), "\n")
cat("Deneyim değişkeni carpıklık katsayısı:", skewness(bank_orneklem$Deneyim), "\n")
cat("Deneyim değişkeni basıklık katsayısı:", kurtosis(bank_orneklem$Deneyim), "\n")
cat("Gelir(Yillik) değişkeni carpıklık katsayısı:", skewness(bank_orneklem$`Gelir(Yillik)`), "\n")
cat("Gelir(Yillik) değişkeni basıklık katsayısı:", kurtosis(bank_orneklem$`Gelir(Yillik)`), "\n")
cat("KKHarcama değişkeni carpıklık katsayısı:", skewness(bank_orneklem$KKHarcama), "\n")
cat("KKHarcama değişkeni basıklık katsayısı:", kurtosis(bank_orneklem$KKHarcama), "\n")
cat("IpotekMiktari değişkeni carpıklık katsayısı:", skewness(bank_orneklem$IpotekMiktari), "\n")
cat("IpotekMiktari değişkeni basıklık katsayısı:", kurtosis(bank_orneklem$IpotekMiktari), "\n")

hist(bank_orneklem$Yas, main = "Yas", col = "orange")
hist(bank_orneklem$Deneyim, main = "Deneyim", col = "orange")
hist(bank_orneklem$`Gelir(Yillik)`, main = "Gelir(Yillik)", col = "orange")
hist(bank_orneklem$KKHarcama, main = "KKHarcama", col = "orange")
hist(bank_orneklem$IpotekMiktari, main = "IpotekMiktari", col = "orange")


boxplot(bank_orneklem$Yas, main = "Yas", col = "cadetblue")
boxplot(bank_orneklem$Deneyim, main = "Deneyim", col = "cadetblue")
boxplot(bank_orneklem$`Gelir(Yillik)`, main = "Gelir(Yillik)", col = "cadetblue")
boxplot(bank_orneklem$KKHarcama, main = "KKHarcama", col = "cadetblue")
boxplot(bank_orneklem$IpotekMiktari, main = "IpotekMiktari", col = "cadetblue")



qqnorm(bank_orneklem$Yas, col = "darkblue", main = "Yaş Değişkeni QQ Plot")
qqline(bank_orneklem$Yas, col = "red")


qqnorm(bank_orneklem$Deneyim, col = "darkblue", main = "Deneyim Değişkeni QQ Plot")
qqline(bank_orneklem$Deneyim, col = "red")


qqnorm(bank_orneklem$`Gelir(Yillik)`, col = "darkblue", main = "Gelir Miktarı (Yıllık) QQ Plot")
qqline(bank_orneklem$`Gelir(Yillik)`, col = "red")


qqnorm(bank_orneklem$KKHarcama, col = "darkblue", main = "Kredi Kartı Harcama Miktarı QQ Plot")
qqline(bank_orneklem$KKHarcama, col = "red")


qqnorm(bank_orneklem$IpotekMiktari, col = "darkblue", main = "İpotek Miktarı QQ Plot")
qqline(bank_orneklem$IpotekMiktari, col = "red")


library(ggplot2)

ggplot(bank_orneklem, aes(x=Egitim ,y=Deneyim, fill=Egitim))+
  geom_boxplot()+
  labs(title="Deneyim Icin Eğitim Seviyesi Kutu Cizimi",
       x="Egitim Kategorileri", y = "Deneyim Sureleri")+
  scale_fill_discrete(name = "Egitim Kategorileri")+
  stat_summary(fun = median, geom="line", group= 1, color= "black", size = 1) 


ggplot(bank_orneklem, aes(x=AileFerdi ,y=KKHarcama, fill=AileFerdi))+
  geom_boxplot()+
  labs(title="Kredi Kartı Harcaması Icin Aile Fert Sayısı Kutu Cizimi",
       x="Aile Ferdi Kategorileri", y = "Kredi kartı Harcamaları")+
  scale_fill_discrete(name = "Aile Ferdi Kategorileri")+
  stat_summary(fun = median, geom="line", group= 1, color= "black", size = 1)

