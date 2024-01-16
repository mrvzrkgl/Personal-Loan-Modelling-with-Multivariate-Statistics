
# Temel Bilesenler Analizi

library(readxl)
bank <- read_xlsx("bank_donusum.xlsx")
View(bank)

library(PerformanceAnalytics)
chart.Correlation(bank[, c(1, 2, 3, 5, 7)], histogram = TRUE, pch = 19)

library(corrplot)
corrplot(cor(bank[,c(1, 2, 3, 5, 7)]))


library(psych)
KMO(bank[,c(1, 2, 3, 5, 7)])
KMO(bank[,c(1, 2, 5, 7)])
KMO(bank[,c(1, 2, 3, 7)])

cortest.bartlett(cor(bank[,c(1, 2, 3, 7)]),nrow(bank)) 


fit.pca <- prcomp(bank[, c(1, 2, 3, 7)], scale = TRUE)
fit.pca$rotation


(fit.pca$sdev)^2
((fit.pca$sdev)^2)/5


plot(fit.pca, col = "brown")
plot(fit.pca,type="line", col = "red")fit.pca$rotation[,1:2]

fit.pca$rotation[,1:2]

bank$comp1 <- fit.pca$x[,1] 
bank$comp2 <- fit.pca$x[,2]

bank$index <- bank$comp1 + bank$comp2
head(bank[order(bank$index, decreasing = T),1])

library(factoextra)
fviz_pca_var(fit.pca,col.var="steelblue",
             repel = TRUE 
)



