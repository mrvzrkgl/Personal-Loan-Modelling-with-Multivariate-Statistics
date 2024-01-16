
# Faktor Analizi

library(readxl)
bank <- read_xlsx("bank_donusum.xlsx")
View(bank)



bank_factor <- bank[, c(1, 2, 3, 5, 7)]
bank_factor <- na.omit(bank_factor)
summary(bank_factor)

library(corrplot)
corrplot(cor(bank_factor))
korelasyon <- cor(bank_factor)
korelasyon

library(matlib)
invkor <- inv(korelasyon)
colnames(invkor) <- rownames(invkor) <- colnames(korelasyon)
invkor


library(psych)
KMO(bank_factor)


bank_factor <- bank[, c(1, 2, 3, 7)]
KMO(bank_factor)


cortest.bartlett(cor(bank_factor),nrow(bank_factor)) 


fa_kokl <- principal(bank_factor, nfactors =2, rotate = "none")

print(fa_kokl$loadings, digits=3, cutoff=.3, sort=TRUE) 

fa_kokl$communality 

fa.diagram(fa_kokl)

factor.plot(fa_kokl, labels=rownames(fa_kokl$loadings),cex=0.8)


library(psych)
fsolution <- fa(bank_factor, nfactors = 2, rotate = "none", fm = "pa") 
print(fsolution$loadings, digits=3, cutoff=.3, sort=TRUE)

fa.diagram(fsolution)


