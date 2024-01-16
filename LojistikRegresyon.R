
# Lojistik Regresyon


library(tidyverse) 
library(modelr)    
library(broom)


library(readxl)
bank <- read_xlsx("bank_donusum.xlsx")
View(bank)

summary(bank)


library(corrplot)
library(matlib)
correlation <- corrplot(cor(bank[, c(1, 2, 3, 5, 7)]), method="number")
invkor <- inv(correlation$corr)# korelasyon matrisinin tersi (VIF)
invkor

colnames(bank)[colnames(bank) == "Gelir(Yillik)"] <- "Gelir"


model <- glm(BireyselKredi ~ Yas + Deneyim + Gelir + AileFerdi + KKHarcama + Egitim + IpotekMiktari, 
             family = "binomial", data = bank)

summary(model)

model$deviance
model$null.deviance
kikare <- model$null.deviance - model$deviance
kikare


model$df.null
model$df.residual
df <- model$df.null-model$df.residual
df



kikare.p <- 1 - pchisq(kikare,df)
kikare.p

library(ResourceSelection)
hoslem.test(model$y, fitted(model))



library("DescTools")
PseudoR2(model, which = c("CoxSnell","Nagelkerke"))

n <- nrow(bank)
R.cs <- 1 - exp ((model$deviance - model$null.deviance) / n)
R.cs


R.n <- R.cs /(1-(exp(-(model$null.deviance / n))))
R.n


exp(coef(model))
exp(confint.default(model,level = 0.95))



type_pred <- fitted(model)
typebk <- ifelse(type_pred > 0.5,"Hayir","Evet")
t_tab <- table(bank$BireyselKredi, typebk)
t_tab

sum(diag(t_tab)) / sum(t_tab)



library(cvms)
library(ggplot2)
library(broom) 
library(tibble)   

datatib <- tibble("target" = bank$BireyselKredi, "prediction" = typebk)
datatib

basic_table <- table(datatib)
basic_table
cfm <- tidy(basic_table)
cfm



plot_confusion_matrix(cfm, 
                      target_col = "target", 
                      prediction_col = "prediction",
                      counts_col = "n",
                      add_normalized = FALSE,
                      add_row_percentages = FALSE,
                      font_col_percentages= font(size = 6),
                      font_counts = font(size = 6),
                      tile_border_color = "black")+
  xlab("Gercek")+
  ylab("Tahmin")+
  theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"))


library(MASS)

step_model <- step(model, direction="both") 
step_model

summary(step_model)
exp(step_model$coefficients)
exp(confint.default(step_model,level = 0.95)) 

type_pred<-fitted(step_model)
typebk <- ifelse(type_pred > 0.5,"Hayir","Evet")
t_tab <- table(bank$BireyselKredi, typebk)
t_tab


sum(diag(t_tab)) / sum(t_tab)


library(cvms)
library(ggplot2)
library(broom)    
library(tibble)   
datatib <- tibble("target" = bank$BireyselKredi,"prediction" = typebk)
datatib

basic_table <- table(datatib)
basic_table

cfm <- tidy(basic_table)
cfm



plot_confusion_matrix(cfm, 
                      target_col = "target", 
                      prediction_col = "prediction",
                      counts_col = "n",
                      add_normalized = FALSE,
                      add_row_percentages = FALSE,
                      font_col_percentages= font(size = 6),
                      font_counts = font(size = 6),
                      tile_border_color = "black")+
  xlab("Gercek")+
  ylab("Tahmin")+
  theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"))

