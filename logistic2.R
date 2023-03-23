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
library(agricolae)
library(graphics)
library(PMCMR)
setwd("C:/Users/mirko/Desktop/Cristian")
data2012 <- read.delim("2012f.csv", header=T, sep=";")
data2016 <- read.delim("2016f.csv", header=T, sep=";")
gasto <- read.delim("gastopor.csv", header=T, sep=";")
gasto$year <- as.factor(gasto$year)

data2012$partido_grupo <- relevel(data2012$partido_grupo, ref="Tradicionales")
data2016$partido_grupo <- relevel(data2016$partido_grupo, ref="Tradicionales")
data2012$sexo <- relevel(data2012$sexo, ref="MUJER")
data2016$sexo <- relevel(data2016$sexo, ref="MUJER")

##ajusta modelo para elecciones 2012
log2012 <- glm(electo ~ gasto_por+partido_grupo+sexo, data=data2012, family="binomial")
summary(log2012)
exp(coef(log2012))
#ajuste respecto al modelo nulo
anova(log2012, test="Chisq")
pR2(log2012)


##2012 sin partidos
log2012a <- glm(electo ~ gasto_por+sexo, data=data2012, family="binomial")
summary(log2012a)
exp(coef(log2012a))
pR2(log2012a)
anova(log2012, log2012a, test="Chisq")

##2012 sólo gasto
log2012b <- glm(electo ~ gasto_por, data=data2012, family="binomial")
summary(log2012b)
exp(coef(log2012b))
pR2(log2012b)
anova(log2012a, log2012b, test="Chisq")

export_summs(log2012, log2012a, log2012b, model.names = c("Gasto+Partido+Sexo", "Gasto+Sexo", "Gasto"))

#####   2016
####
##ajusta modelo para elecciones 2016
log2016 <- glm(electo ~ gasto_por+partido_grupo+sexo, data=data2016, family="binomial")
summary(log2016)
exp(coef(log2016))
#ajuste respecto al modelo nulo
anova(log2016, test="Chisq")
pR2(log2016)


##2016 sin partidos
log2016a <- glm(electo ~ gasto_por+sexo, data=data2016, family="binomial")
summary(log2016a)
exp(coef(log2016a))
pR2(log2016a)
anova(log2016, log2016a, test="Chisq")

##2016 sólo gasto
log2016b <- glm(electo ~ gasto_por, data=data2016, family="binomial")
summary(log2016b)
exp(coef(log2016b))
pR2(log2016b)
anova(log2016a, log2016b, test="Chisq")

export_summs(log2016, log2016a, log2016b, model.names = c("Gasto+Partido+Sexo", "Gasto+Sexo", "Gasto"))


##Comparación modelos 2012-2016

export_summs(log2012b, log2016b, model.names = c("Gasto - 2012", "Gasto - 2016"))



##anova
anova2012 <- aov(gasto_por ~ partido_grupo, data=data2012)
summary(anova2012)
TukeyHSD(anova2012)

anova2016 <- aov(gasto_por ~ partido_grupo, data=data2016)
summary(anova2016)
TukeyHSD(anova2016)

mean2012 <- HSD.test(anova2012, 'partido_grupo')
mean2016 <- HSD.test(anova2016, 'partido_grupo')
t(mean2012$means)
t(mean2016$means)

l2012 <- lm(gasto_por ~ partido_grupo, data=data2012)
l2016 <- lm(gasto_por ~ partido_grupo, data=data2016)
summary(l2012)
summary(l2016)


##tabla electos
el2012<-prop.table(table(data2012$electo_r))
el2016<-prop.table(table(data2016$electo_r))

#tabla partidos_grupo
prop.table(table(data2012$partido_grupo))
prop.table(table(data2016$partido_grupo))


####chicuadrado

#partido_grupo
pel2012 <- table(data2012$electo_r, data2012$partido_grupo)
prop.table(pel2012, 2)
chi2012<-chisq.test(pel2012)
fisher.test(pel2012, hybrid=T)
chi2012$residuals

pel2016 <- table(data2016$electo_r, data2016$partido_grupo)
prop.table(pel2016, 2)
chi2016<-chisq.test(pel2016)
chi2016$residuals


#sexo
sel2012 <- table(data2012$electo_r, data2012$sexo)
sel2016 <- table(data2016$electo_r, data2016$sexo)
prop.table(sel2012, 2)
prop.table(sel2016, 2)
chisq.test(sel2012)
chisq.test(sel2016)

par(mfrow=c(1,1))
mosaicplot(pel2012, shade=T, main="Éxito electoral por partido - 2012", las=1)
mosaicplot(pel2016, shade=T, main="Éxito electoral por partido - 2016", las=1)


###gasto_por
ggplot(gasto, aes(x=gasto_por)) +
  geom_freqpoly(bins=10, aes(y=stat(width*density), colour=year))+
  ylab("Porcentaje") +
  xlab("Gasto Electoral") +
  scale_x_continuous(breaks=seq(0, 100, by=20)) +
  scale_y_continuous(labels=percent_format(), 
                     breaks=seq(0, 0.3, by=0.05), 
                     limits = (c(0,0.3))) +
  annotate("text", x = 80, y = 0.25, 
           label = paste("Media 2012 =", round(mean(data2012$gasto_por), 1), "%"),
           size=3.5) +
  annotate("text", x = 80, y = 0.225, 
           label = paste("Media 2016 =", round(mean(data2016$gasto_por), 1), "%"),
           size=3.5) +
  theme_apa(x.font.size = 11,  y.font.size = 11, legend.font.size=9)

ggplot(gasto, aes(x=gasto_por)) +
  geom_histogram(bins=10, aes(y=stat(width*density), colour=year), 
                 fill="grey") +
  ylab("Porcentaje") +
  xlab("Gasto Electoral") +
  ggtitle("Porcentaje de Gasto Electoral - 2012") +
  scale_x_continuous(breaks=seq(0, 100, by=10)) +
  scale_y_continuous(labels=percent_format(), 
                     breaks=seq(0, 0.3, by=0.05), 
                     limits = (c(0,0.3))) +
  annotate("text", x = 80, y = 0.25, 
           label = paste("Media =", round(mean(data2012$gasto_por), 1), "%"),
           size=4) +
  theme_apa(x.font.size = 14,  y.font.size = 14)


#contraste gasto 2012-2016
t.test(data2012$gasto_por, data2016$gasto_por)
#wilcox.test entrega U de Mann-Whitney cuando paired=F
#usado ante no normalidad de los datos
wilcox.test(data2012$gasto_por, data2016$gasto_por, paired=F)



ggplot(gasto, aes(x=gasto_por)) +
  geom_freqpoly(bins=10, aes(y=stat(width*density), colour=year))+
  ylab("Porcentaje") +
  xlab("Gasto Electoral") +
  scale_x_continuous(breaks=seq(0, 100, by=20)) +
  scale_y_continuous(labels=percent_format(), 
                     breaks=seq(0, 0.3, by=0.05), 
                     limits = (c(0,0.3))) +
  annotate("text", x = 80, y = 0.25, 
           label = paste("Media 2012 =", round(mean(data2012$gasto_por), 1), "%"),
           size=3.5) +
  annotate("text", x = 80, y = 0.225, 
           label = paste("Media 2016 =", round(mean(data2016$gasto_por), 1), "%"),
           size=3.5) +
  theme_apa(x.font.size = 11,  y.font.size = 11, legend.font.size=9)