
# Diskriminant Analizi

library(readxl)
bank <- read_xlsx("bank_donusum.xlsx")
View(bank)

library(funModeling)
plot_num(bank[, c(1, 2, 3, 5, 7)])

shapiro.test(bank$Yas)
shapiro.test(bank$Deneyim)
shapiro.test(bank$Gelir)
shapiro.test(bank$KKHarcama)
shapiro.test(bank$IpotekMiktari)

library(dplyr)
library(rstatix)

bank %>% 
  dplyr::select(c(1, 2, 3, 5, 7)) %>%
  mshapiro_test()


library(corrplot)
corrplot(cor(bank[, c(1, 2, 3, 5, 7)], method = "spearman"), method="number")


library(biotools)
boxM(bank[, c(1, 2, 3, 5, 7)], bank$Egitim)

library(MASS) 
lda_bank <- lda(Egitim ~ Yas + Deneyim + Gelir + KKHarcama + IpotekMiktari, 
                data = bank) 
lda_bank


cons <- apply(bank[,c(1, 2, 3, 5, 7)], 2, mean)
(-cons)%*%(lda_bank$scaling)


lda_pred <- predict(lda_bank)
lda_pred$class 

lda_pred$x


ldahist(lda_pred$x, g = bank$Egitim) 

tablo<-table(bank$Egitim,lda_pred$class)
tablo


classrate<-sum(diag(tablo))/sum(tablo)
classrate

lda_bank$prior[1]^2 + lda_bank$prior[2]^2


comp<-cbind(bank$Egitim,lda_bank$class)
comp


