#install.packages(c("psc  l","popbio"))
library(pscl)
library(popbio)
library(huxtable)
library(jtools)
library(ggplot2)
library(scales)
library(magrittr)
library(dplyr)
library(reshape2)
library(gridExtra)
library(apaTables)

setwd("C:/Users/mirko/Desktop/Cristian")
data2012 <- read.delim("2012f.csv", header=T, sep=";")
data2016 <- read.delim("2016f.csv", header=T, sep=";")

##ajusta modelo para elecciones 2012
log2012 <- glm(electo ~ gasto_por, data=data2012, family="binomial")
summary(log2012)


#calcula odds ratio para el predictor
exp(coef(log2012))

#ajuste respecto al modelo nulo
anova(log2012, test="Chisq")

#seudo R cuadrado
pR2(log2012)

#predicciones del modelo para cada sujeto
#crea una nueva variable con la probabilidad de elección de cada candidato
data2012$electoP <- predict(log2012, newdata = data2012, type = "response")

#gráfico con histograma
logi.hist.plot(data2012$gasto_por,data2012$electo,
               boxp=FALSE,type="hist",col="gray", 
               xlab="Gasto porcentaje", ylabel = "Probabilidad de éxito", 
               ylabel2 = "Número de candidatos", 
               mainlabel="Estimación del éxito electoral - 2012")
#Las barras del 

#Correlación %votos vs %gasto
cor.test(data2012$votos_por, data2012$gasto_por, method="spearman", exact=F)



#####   2016
####


##ajusta modelo par elecciones 2016
log2016 <- glm(electo ~ gasto_por, data=data2016, family="binomial")
summary(log2016)


#calcula odds ratio para el predictor
exp(coef(log2016))

#ajuste respecto al modelo nulo
anova(log2016, test="Chisq")

##seudoRcuadrado
pR2(log2016)

#predicciones del modelo para cada sujeto
#crea una nueva variable con la probabilidad de elección de cada candidato
data2016$electoP <- predict(log2016, newdata = data2016, type = "response")

#gráfico con histograma
logi.hist.plot(data2016$gasto_por,data2016$electo,
               boxp=FALSE,type="hist",col="gray", 
               xlab="Gasto porcentaje", ylabel = "Probabilidad de éxito", 
               ylabel2 = "Número de candidatos", 
               mainlabel="Estimación del éxito electoral - 2016")

#Correlación %votos vs %gasto
cor2016 <- cor.test(data2016$votos_por, data2016$gasto_por, method="spearman", exact=F)
cor2016

#histogramas para media de gasto porcentual
hist2012 <- ggplot(data2012, aes(x=gasto_por)) +
  geom_histogram(bins=10, aes(y=stat(width*density)), 
                 colour = "black",
                 fill="grey") +
  ylab("Porcentaje") +
  xlab("Gasto Electoral") +
  ggtitle("Porcentaje de Gasto Electoral - 2012") +
  scale_x_continuous(breaks=seq(0, 100, by=10)) +
  scale_y_continuous(labels=percent_format(), 
                     breaks=seq(0, 0.3, by=0.05), 
                     limits = (c(0,0.3))) +
  geom_vline(xintercept = mean(data2012$gasto_por), color="red")+
  annotate("text", x = 80, y = 0.25, 
           label = paste("Media =", round(mean(data2012$gasto_por), 1), "%"),
           size=4) +
  theme_apa(x.font.size = 14,  y.font.size = 14)

hist2016 <- ggplot(data2016, aes(x=gasto_por)) +
  geom_histogram(bins=10, aes(y=stat(width*density)), 
                 colour = "black",
                 fill="grey") +
  ylab("Porcentaje") +
  xlab("Gasto Electoral") +
  ggtitle("Porcentaje de Gasto Electoral - 2016") +
  scale_x_continuous(breaks=seq(0, 100, by=10)) +
  scale_y_continuous(labels=percent_format(), 
                     breaks=seq(0, 0.3, by=0.05), 
                     limits = (c(0,0.3))) +
  geom_vline(xintercept = mean(data2016$gasto_por), color="red")+
  annotate("text", x = 80, y = 0.25, 
           label = paste("Media =", round(mean(data2016$gasto_por), 1), "%"),
           size=4) +
  theme_apa(x.font.size = 14,  y.font.size = 14)+
  theme(axis.title.y=element_blank())

grid.arrange(hist2012, hist2016, ncol=2)

##tabla única comparando ambos modelos (2012 y 2016)
export_summs(log2012, log2016)


##formular modelo nuevo con más predictores
#al predictor que ya está le pones "+" y colocas la otra variable
#por ejemplo: gasto_por + partido genera un modelo donde hay dos variables independientes: gasto y partido

#relevel permite cambiar la categoría de referencia
#sirve para que el OR sea más fácil de interpretar

#sexo en 2012
data2012$sexo <- relevel(data2012$sexo, ref="MUJER")
log2012sex <- glm(electo ~ sexo, data=data2012, family="binomial")
summary(log2012sex)
exp(coef(log2012sex))
pR2(log2012sex)

#sexo+gasto 2012
data2012$sexo <- relevel(data2012$sexo, ref="MUJER")
log2012sex2 <- glm(electo ~ sexo + gasto_por, data=data2012, family="binomial")
summary(log2012sex2)
exp(coef(log2012sex2))
pR2(log2012sex2)

export_summs(log2012sex, log2012sex2, model.names = c("Sexo", "Sexo+Gasto"))

#sexo en 2016
data2016$sexo <- relevel(data2016$sexo, ref="MUJER")
log2016sex <- glm(electo ~ sexo, data=data2016, family="binomial")
summary(log2016sex)
exp(coef(log2016sex))
pR2(log2016sex)

#sexo+gasto 2016
data2016$sexo <- relevel(data2016$sexo, ref="MUJER")
log2016sex2 <- glm(electo ~ gasto_por + sexo, data=data2016, family="binomial")
summary(log2016sex2)
exp(coef(log2016sex2))
pR2(log2016sex2)

export_summs(log2016sex, log2016sex2, model.names = c("Sexo", "Sexo+Gasto"))

#si bien el sexo tiene un efecto significativo, no tiene un gran aporte
#al ajuste del modelo, no debería incluirse
#comparación de modelos de uno y dos predictores
anova(log2016, log2016sex, test="Chisq")

###############
#Modelos contrastando el efecto de partido y luego partido+gasto
###############


##Primero se contrasta que haya un efecto del partido sobre el gasto electoral
#comparación del gasto según grupo de partidos - 2012
anova2012 <- aov(gasto_por ~ partido_grupo, data=data2012)
summary(anova2012)

#análisis posthoc para gasto por partido
TukeyHSD(anova2012)

#comparación del gasto según grupo de partidos - 2016
anova2016 <- aov(gasto_por ~ partido_grupo, data=data2016)
summary(anova2016)

#análisis posthoc para gasto por partido
TukeyHSD(anova2016)


#scatterplot para relación entre gasto y votos
plot(data2012$gasto_por, data2012$votos_por,
     xlab = "Porcentaje Gasto Electoral",
     ylab = "Porcentaje de Votos",
     main = "Relación entre gasto electoral y votos obtenidos - 2012",
     pch  = 20,
     cex  = 0.8)
abline(lm(data2012$votos_por ~ data2012$gasto_por))
legend("topright", bty="n", 
       legend=paste("R2 =",format(summary(lm(data2012$votos_por ~ data2012$gasto_por))$adj.r.squared, digits=4)))

plot(data2016$gasto_por, data2016$votos_por,
     xlab = "Porcentaje Gasto Electoral",
     ylab = "Porcentaje de Votos",
     main = "Relación entre gasto electoral y votos obtenidos - 2016",
     pch  = 20,
     cex  = 0.8)
abline(lm(data2016$votos_por ~ data2016$gasto_por))
legend("topright", bty="n", 
       legend=paste("R2 =",format(summary(lm(data2016$votos_por ~ data2016$gasto_por))$adj.r.squared, digits=4)))


#Modelo logístico para la estimación del éxito a partir del partido - 2012

log2012par <- glm(electo ~ partido_grupo, data=data2012, family="binomial")
summary(log2012par)
exp(coef(log2012par))
pR2(log2012par)

log2012par2 <- glm(electo ~ partido_grupo + gasto_por, data=data2012, family="binomial")
summary(log2012par2)
exp(coef(log2012par2))
pR2(log2012par2)

export_summs(log2012par, log2012par2, model.names = c("Partido","Partido+Gasto"))


#Modelo logístico para la estimación del éxito a partir del partido - 2016

data2016$partido_grupo <- relevel(data2016$partido_grupo, ref="Tradicionales")
log2016par <- glm(electo ~ partido_grupo, data=data2016, family="binomial")
summary(log2016par)
exp(coef(log2016par))
pR2(log2016par)

log2016par2 <- glm(electo ~ partido_grupo + gasto_por, data=data2016, family="binomial")
summary(log2016par2)
exp(coef(log2016par2))
pR2(log2016par2)

export_summs(log2016par, log2016par2, model.names = c("Partido", "Partido+Gasto"))