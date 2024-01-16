
# Manova



library(readxl)
bank <- read_xlsx("bank_donusum.xlsx")
View(bank)


library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(dplyr)
library(tidyr)
library(haven)
library(effectsize)
library(heplots)
library(biotools)

any(duplicated(bank))

box_m(bank[, c("Yas", "Deneyim", "Gelir", "KKHarcama", "IpotekMiktari")], 
      bank$AileFerdi)

library(dplyr)

bank %>%
  dplyr::select(Yas, Deneyim, Gelir, KKHarcama, IpotekMiktari) %>%
  mshapiro_test()

bank_manova <- manova(cbind(Yas, Deneyim, Gelir,KKHarcama, Egitim, IpotekMiktari) ~ AileFerdi, data = bank)
summary(bank_manova, test = "Pillai")

etasq(bank_manova, test="Pillai")

bank %>% 
  pivot_longer( c(Yas, Deneyim, Gelir,KKHarcama, Egitim, IpotekMiktari),
                names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  levene_test(value ~ AileFerdi, center=mean)



summary.aov(bank_manova)

gh <- games_howell_test(bank, IpotekMiktari ~ AileFerdi)
gh


yas_aov <- aov(Yas ~ AileFerdi, data = bank)
etasq(yas_aov)
TukeyHSD(yas_aov, "AileFerdi")

deneyim_aov <- aov(Deneyim ~ AileFerdi, data = bank)
etasq(deneyim_aov)
TukeyHSD(deneyim_aov, "AileFerdi")


gelir_aov <- aov(Gelir ~ AileFerdi, data = bank)
etasq(gelir_aov)
TukeyHSD(gelir_aov, "AileFerdi")


kkharcama_aov <- aov(KKHarcama ~ AileFerdi, data = bank)
etasq(kkharcama_aov)
TukeyHSD(kkharcama_aov, "AileFerdi")

egitim_aov <- aov(Egitim ~ AileFerdi, data = bank)
etasq(egitim_aov)
TukeyHSD(egitim_aov, "AileFerdi")



bank$Egitim <- factor(bank$Egitim, levels = c(1, 2, 3), labels = c("Lisans", "Yuksek Lisans", "Doktora"))


manova_cift <- manova(cbind(Yas, Deneyim, Gelir, KKHarcama, IpotekMiktari) ~
                        AileFerdi * Egitim, data = bank)
summary(manova_cift, test = "Pillai")

etasq(manova_cift, test="Pillai")

bank %>% 
  pivot_longer( c(Yas, Deneyim, Gelir, KKHarcama, IpotekMiktari),names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  levene_test(value ~AileFerdi * Egitim, center = mean)


summary.aov(manova_cift)
etasq(aov(Yas ~ AileFerdi * Egitim, data = bank))
etasq(aov(Deneyim ~ AileFerdi * Egitim, data = bank))
etasq(aov(Gelir ~ AileFerdi * Egitim, data = bank))
etasq(aov(KKHarcama ~ AileFerdi * Egitim, data = bank))
etasq(aov(IpotekMiktari ~ AileFerdi * Egitim, data = bank))


tukey <- bank %>%
  pivot_longer( c(Yas, Deneyim, Gelir, KKHarcama, IpotekMiktari),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  tukey_hsd(value ~ AileFerdi * Egitim)
tukey <- tukey[,]
departman_etk <- filter(tukey, term=="Egitim")
departman_etk

attach(bank)
interaction.plot(AileFerdi ,Egitim, Yas, fun=mean, type="l", legend=TRUE, col=1:3,lwd=2)
interaction.plot(AileFerdi ,Egitim, Deneyim, fun=mean, type="l", legend=TRUE, col=1:3,lwd=2)
interaction.plot(AileFerdi ,Egitim, Gelir, fun=mean, type="l", legend=TRUE, col=1:3,lwd=2)
interaction.plot(AileFerdi ,Egitim, KKHarcama, fun=mean, type="l", legend=TRUE,col=1:3, lwd=2)
interaction.plot(AileFerdi ,Egitim, IpotekMiktari, fun=mean, type="l", legend=TRUE,col=1:3, lwd=2)
detach(bank)


