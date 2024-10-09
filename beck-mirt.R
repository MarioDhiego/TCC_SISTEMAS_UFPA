
library(mirt)
library(ltm)
library(lavaan)
library(psych)
library(irtoys)
library(dplyr)
library(ggplot2)
library(reshape2)
library(sjPlot)
library(kohonen) 
library(stringr)
library(sm)
library(FactoMineR)
library(lordif)
library(psychotree)
library(colorspace)
library(difR)
library(mosaic)
library(xlsx)
library(d3heatmap)
library(WrightMap)
library(RColorBrewer)
library(TeachingDemos)
library(aplpack)


#############################Descritiva
str(Banco_BDI_Rose_2021)
names(Banco_BDI_Rose_2021)
describe(BASE_TRI_ENANDE_2017)
table(Banco_Depressao_Tati)
summary(BASE_COMPONENTE)


notas1=rowSums(BASE_COMPONENTE)
notas2=rowSums(BASE_FORMACAO_GERAL)
notas3=rowSums(BASE_TRI_ENANDE_2017)

summary(notas1)
summary(notas2)
summary(notas3)

table(notas3)
table(notas2)
table(notas1)

boxplot(notas2)


boxplot(notas1, notas2, notas3, names = c("Bruta", "Formação Geral", "Componente Específico"), main="Notas Brutas Gerais", xlab="Formato da Prova", ylab="Nº de Acertos", col= "blue", horizontal=FALSE, add = FALSE)


AcerIt=colSums(notas3) 

############################Alpha de Cronback
lsat.desc <- descript(BASE_TRI_ENANDE_2017)
names(lsat.desc)
lsat.desc
plot(lsat.desc,type='b',includeFirstLast=TRUE)
plot(lsat.desc,items=c(1:5),type="b",includeFirstLast=TRUE,pch=c('1','2','3','4','5'))
cronbach.alpha(Banco_Depressao_Tati)



############################criar o modelo
mirt_1pl <- mirt(BASE_TRI_ENANDE_2017, 1, itemtype = "Rasch", se= TRUE, se.type = "BL") # modelo 1pl
mirt_2pl <- mirt(BASE_TRI_ENANDE_2017, 1, itemtype = "2PL", se= TRUE, se.type = "BL") # modelo 2pl
mirt_3pl <- mirt(Banco_Regulamentacao, 1, itemtype = "3PL", se= TRUE, se.type = "BL") # modelo 3pl



mirt_1pl <- mirt(Banco_Depressao_Tati, 1, itemtype = "Rasch", se= TRUE, se.type = "BL") # modelo 1pl

mirt_2pl <- mirt(Banco_Depressao_Tati, 1, itemtype = "2PL", se= TRUE, se.type = "BL") # modelo 2pl

mirt_3pl <- mirt(Banco_Depressao_Tati, 1, itemtype = "3PL", se= TRUE, se.type = "BL") # modelo 3pl


############################calcular os par?metros dos itens
coef(mirt_2pl, simplify = TRUE) 

coef(mirt_1pl, IRTpars = TRUE, simplify = TRUE) # parametros dos itens
coef(mirt_2pl, IRTpars = TRUE, simplify = TRUE) # parametros dos itens
coef(mirt_3pl, IRTpars = TRUE, simplify = TRUE) # parametros dos itens





###########habilidades

fscores(mirt_1pl)
fscores(mirt_2pl)
fscores(mirt_3pl)
hist(fscores(mirt_1pl))
hist(fscores(mirt_2pl))
hist(fscores(mirt_3pl))



###########################Plotar curvas caracater?sticas dos itens
plot(mirt_1pl)
plot(mirt_2pl)
plot(mirt_3pl)

plot(mirt_1pl, type ="info" )
plot(mirt_2pl, type ="info" )
plot(mirt_3pl, type ="info" )


plot(mirt_1pl, type = "trace")
plot(mirt_2pl, type = "trace")
plot(mirt_3pl, type = "trace")


#par(mfrow=c(2,2)) 
plot(mirt_2pl, type = "trace", which.items = 1:10)
plot(mirt_2pl, type = "trace", which.items = 11:20)
plot(mirt_2pl, type = "trace", facet_items=FALSE, which.items = 1:2)
plot(mirt_2pl, type = "trace", facet_items=FALSE, which.items = 1:21)
itemplot(mirt_2pl, type = "infotrace", item = 1)
itemplot(mirt_2pl, type = "infotrace", item = 2)
itemplot(mirt_2pl, type = "infotrace", item = 3)
itemplot(mirt_2pl, type = "infotrace", item = 4)
itemplot(mirt_2pl, type = "infotrace", item = 5)

plot(mirt_1pl, type = "infotrace")
plot(mirt_2pl, type = "infotrace")
plot(mirt_3pl, type = "infotrace")


#renomear os intens na ordem crescente
#colnames(DEPBECK)<-c("TRISTESA", "PESSIMISMO", "SENTIMENTODEFRACASSO", "INSATISFA??ES", "CULPA", "PUNI??O", "AUTOVERSAO", "IDEIASSUICIDAS", "CHORO", "IRRITABILIDADE", "RETRAIMENTOSOCIAL", "INDECIS?O", "", "", "")
#vars<- c("PESSIMISMO", "INSATISFA??O", "CULPA")
#dados8<- dados[vars]


#Comparar os Modelos

anova(mirt_1pl, mirt_2pl)
anova(mirt_2pl, mirt_3pl)


#An?lise de Fatores
summary(mirt_2pl)
summary(mirt_3pl)

#Estat?stica M2
M2(mirt_1pl)
M2(mirt_2pl)
M2(mirt_3pl)


#Correla??es
d3heatmap(Banco_Depressao_Tati, scale = "column", colors = "Blues")
d3heatmap(cor(Banco_Depressao_Tati[ , 1:20], use="pair"), 
          symn= TRUE,  symm = TRUE, 
          k_row = 3, k_col = 3)



#data("Anxiety") 
#Age <- Anxiety$age 
#Resp <- Anxiety[paste("R", 1:29, sep = "")] 
#ageDIF <- lordif(Resp, Age, criterion = "Chisqr", alpha = 0.01, + minCell = 5) 
#print(ageDIF) 
#summary(ageDIF) 
#plot(ageDIF, labels = c("Younger (<65)", "Older (65+)"))


#An?lise de Cluster
#method = single, complete, average, ward

#d <- dist(DEPBECK[ , 1:20], method="euclidean")
#cluster <- hclust(d, method="ward.D2")
#plot(cluster, hang = -1)
#abline(h=4.3,lty=3,col="red")
#rect.hclust(cluster, k=3, border="red")


#Faces de Chernoff
#stars(DEPBECK)
#faces(DEPBECK[,1:10], labels=DEPBECK$trat)

########################################iTENS EXCLU?DOS########

#Criar Modelo sem o ITEM 18
mirt_2pl18 <- mirt(DEPBECK2, 1, itemtype = "2PL", se= TRUE, se.type = "BL") # modelo 2pl

#calcular os par?metros dos itens
coef(mirt_2pl18, IRTpars = TRUE, simplify = TRUE) # parametros dos itens

#plotar curvas caracater?sticas dos itens
plot(mirt_2pl18)
plot(mirt_2pl18, type ="info" )
plot(mirt_2pl18, type = "trace")
plot(mirt_2pl18, type = "trace", which.items = 1:10)
plot(mirt_2pl18, type = "trace", which.items = 11:19)
plot(mirt_2pl18, type = "trace", facet_items=FALSE, which.items = 1:10)
plot(mirt_2pl18, type = "trace", facet_items=FALSE, which.items = 11:19)

