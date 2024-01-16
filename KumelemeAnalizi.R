

# Kumeleme Analizi

library(haven)
library(dendextend)


library(readxl)
bank <- read_xlsx("bank_donusum.xlsx")
View(bank)


library(Hmisc)
rcorr(as.matrix(bank[, c(1, 2, 3, 5, 7)]),type="pearson") 


d <- dist(bank[, c(1, 2, 3, 5, 7)], method = "euclidean")
fit <- hclust(d, method="ward.D")

dend <- as.dendrogram(fit) 

plot(dend)
plot(color_branches(dend, k = 4))


geneldend<-dend %>%
  set("branches_lwd", 2) %>%
  set("branches_k_color", k = 4)%>%
  set("labels_cex", 1.2)%>%
  set("labels_colors", k = 4)
plot(geneldend,font.axis = 2) 



library(factoextra)
res.hc <- hclust(d,  method = "ward.D2")
fviz_dend(res.hc, cex = 0.5, k = 4, palette = "jco") 


fviz_nbclust(bank[, c(1, 2, 3, 5, 7)], kmeans, method = "wss")
fviz_nbclust(bank[, c(1, 2, 3, 5, 7)], kmeans, method = "silhouette")


set.seed(95739487) 
km.res <- kmeans(bank[, c(1, 2, 3, 5, 7)],4, iter.max=10, algorithm="Lloyd")
t(km.res$centers)

library(cluster)
clusplot(bank[, c(1, 2, 3, 5, 7)], km.res$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)



fviz_cluster(km.res, data = bank[, c(1, 2, 3, 5, 7)], palette = "jco",
             ggtheme = theme_minimal())


bank$cluster <- km.res$cluster
bank$cluster <- as.factor(bank$cluster)



library(rstatix)

yas_aov <- aov(Yas ~ cluster, data = bank)
summary(yas_aov)

deneyim_aov <- aov(Deneyim ~ cluster, data = bank)
summary(deneyim_aov)

gelir_aov <- aov(Gelir ~ cluster, data = bank)
summary(gelir_aov)

kkharcama_aov <- aov(KKHarcama ~ cluster, data = bank)
summary(kkharcama_aov)

ipotek_aov <- aov(IpotekMiktari ~ cluster, data = bank)
summary(ipotek_aov)


