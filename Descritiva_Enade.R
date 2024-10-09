

############################ Estat?stica Descritiva - MicroDados ENADE-2018 ########
############################ Pacotes ###########################################
library(readr)
library(ggplot2)
library(plotly)
library(e1071)
library(dplyr)
library(Hmisc)

library(gridExtra)
library(devtools)
library(scales)
library(tidyverse)
library(knitr)
library(esquisse)
library(shiny)
#############################################################################

####### Diret?rio de Trabalho #############################
setwd("C:/Users/mario Dhiego/Documents/ENADE")
getwd()
##########################################################

############ Importando a Base da Dados  ########
Base_Completa=read.table("microdados_enade_2018.txt", 
                         header=TRUE, 
                         sep=";", 
                         dec=",", 
                         colClasses=c(NT_OBJ_FG="numeric"))

#Visualizando o Banco de Dados
View(Base_Completa)

#Verificando as dimens?es e nomes
dim(Base_Completa)
names(Base_Completa)
#######################################################################

############ Filtro de Variaveis #####################################
Base = Base_Completa %>% dplyr::select (NT_OBJ_FG,
                                       CO_GRUPO,
                                       CO_REGIAO_CURSO,
                                       QE_I02,
                                       CO_TURNO_GRADUACAO, TP_SEXO, NU_IDADE)
View(Base)
dim(Base)
names(Base)
#################################################


############ Classificar as Variaveis #####################
# NT_OBJ_FG=nota: Vari?vel Quantitativa Cont?nua 
# CO_GRUPO=curso: Vari?vel Qualitativa Nominal
# CO_REGIAO_CURSO: Vari?vel Qualitativa Nominal
# QE_I02: Vari?vel Qualitativa Nominal
# CO_TURNO_GRADUACAO: Vari?vel Qualitativa Ordinal
#########################################

############ Resumo dos dados com as vari?veis selecionadas #####
s=summary(Base)
s
###########
d=describe(Base)
d
##################################################




######################### Filtrar um  Curso ####################
DIREITO = Base %>% filter (CO_GRUPO == 2)
View(DIREITO)
dim(DIREITO)
names(DIREITO)
###############################################################

######################## Filtrar outros Cursos ###############
PSICOLOGIA = Base %>% filter (CO_GRUPO == 18)
CONTABEIS = Base %>% filter (CO_GRUPO == 22)
##############################################################

############# Certificando que o filtro funcionou(c?digo do curso e n?mero de linhas)
table(DIREITO$CO_GRUPO)
#################################



######################## Transformar os R?tulos das Var?veis #####################
#Criando categorias no pacote dplyr para que facilite a nossa An?lise Descritiva

####################### Transformar o Curso/DIREITO #####################
DIREITO = DIREITO %>% mutate(CURSO = case_when(CO_GRUPO== 2 ~ "Direito"))
#########################################################################

####################### Transformar a Vari?vel CO_REGIAO/REGI?O  #######################
DIREITO = DIREITO %>% mutate(REGIAO = case_when(CO_REGIAO_CURSO == 1 ~ "Norte",
                                                CO_REGIAO_CURSO == 2 ~ "Nordeste",
                                                CO_REGIAO_CURSO == 3 ~ "Sudeste",
                                                CO_REGIAO_CURSO == 4 ~ "Sul",
                                                CO_REGIAO_CURSO == 5 ~ "Centro-Oeste"))
###########################################################################

####################### Transformar a Vari?vel QE_I02/RA?A #########################
DIREITO = DIREITO %>% mutate(RACA = case_when(QE_I02 == "A" ~ "Branca",
                                              QE_I02 == "B" ~ "Preta",
                                              QE_I02 == "C" ~ "Amarela",
                                              QE_I02 == "D" ~ "Indigena",
                                              QE_I02 == "E" ~ "ND"))
############################################################################

######################## Transformar a Vari?vel CO_TURNO_GRADUACAO/TURNO
DIREITO = DIREITO %>% mutate(TURNO = case_when(CO_TURNO_GRADUACAO == "1" ~ "Matutino",
                                               CO_TURNO_GRADUACAO == "2" ~ "Vespertino",
                                               CO_TURNO_GRADUACAO == "3" ~ "Integral",
                                               CO_TURNO_GRADUACAO == "4" ~ "Noturno"))
###########################################################################


######################## Transformar a Vari?vel TP_SEXO/SEXO ################
DIREITO = DIREITO %>% mutate(SEXO = case_when(TP_SEXO == "F" ~ "Feminino",
                                              TP_SEXO == "M" ~ "Masculino"))
##########################################################################



##################### Verificando os r?tulos das vari?veis transformadas ######
names(DIREITO)
View(DIREITO)
#####################################################

#################### Verificando a classe dos r?tulos das vari?veis transformadas
class(DIREITO$NT_OBJ_FG)
class(DIREITO$CURSO)
class(DIREITO$REGIAO)
class(DIREITO$RACA)
class(DIREITO$TURNO)
class(DIREITO$SEXO)
###########################


################## Removendo as Variaveis antigas #############
DIREITO = DIREITO[,-c(2,3,4,5,6)]
names(DIREITO)[1] = "NOTAS"
view(DIREITO)
######################################################


################ Estatistica Descritiva Vari?veis Transformadas
ED = summary(DIREITO)
ED
#
d=describe(DIREITO)
d
##########################



################ Retirando os missing de todas vari?veis
DIREITO = DIREITO %>% na.omit()
ED_SEM_MISSING = summary(DIREITO)
ED_SEM_MISSING
#############

#################Resumo de todas as vari?veis sem NA
resumo_nas = ED_SEM_MISSING %>%
  select(everything()) %>%  
  summarise_all(list(~sum(is.na(.))))
resumo_nas
##############################

#Quatidade de linhas do banco original com as vari?veis selecionadas
dim(DIREITO)[1]
#Quatidade de linhas do banco com as vari?veis selecionadas e sem os NAs
dim(ED_SEM_MISSING)[1]

#Total de linhas removidas que continham NAs(Missing)
total_linhas_excluidas=dim(DIREITO)[1]-dim(ED_SEM_MISSING)[1]
total_linhas_excluidas
######################################


########### Medidas EstatIsticas ############

#CAlculo do tamanho do vetor de Notas
quantidade_de_notas=length(DIREITO$NOTAS)

#Calculo da Media
MEDIA = mean(DIREITO$NOTAS)

#C?lculo da Mediana
MEDIANA = median(DIREITO$NOTAS)

####### Calculo da Moda
#Primeira etapa: Calcular as frequ?ncias simples
FREQUENCIA = table(DIREITO$NOTAS)

#Segunda etapa: Calcular o m?ximo das frequ?ncias simples
MAXIMO = max(FREQUENCIA)

#Terceira etapa: Trazer os nomes que correspondem ?s observa??es das FREQUENCIA
NOMES = names(FREQUENCIA)

#Quarta etapa: Trazer os nomes que satisfazem ? compara??o l?gica
MODA_TEXTO = NOMES[FREQUENCIA == MAXIMO]
class(MODA_TEXTO)

#Quinta etapa: Transformar de caractere em n?mero
MODA_NUMERO = as.numeric(MODA_TEXTO)
class(MODA_NUMERO)
#########################

#C?lculo da Vari?ncia
VARIANCIA = var(DIREITO$NOTAS)

#C?lculo da Vari?ncia
DESVIO = sd(DIREITO$NOTAS)

#C?lculo dO Coeficiente de Varia??o
CV=sd(DIREITO$NOTAS)/mean(DIREITO$NOTAS)*100

#C?lculo da Assimetria:
AS=skewness(DIREITO$NOTAS)

#C?lculo do Coeficiente de Curtose da forma como o R calcula, comparada a da normal
CURTOSE=kurtosis(DIREITO$NOTAS)


#Resumo das Estat?sticas
RESULTADO = c(MAXIMO,MEDIA,MEDIANA,MODA_NUMERO,VARIANCIA,DESVIO,CV,AS,CURTOSE)
RESULTADO
##########################################


############### Total Agrupado para identificar poss?veis dados faltantes (NAs)
#Total de Nota agrupado em classes 
DIREITO %>% 
  select(NOTAS) %>% 
  group_by(NOTAS) %>% 
  summarise(total = n())
#############################
#Total agrupado por Regi?o 
DIREITO %>% 
  select(REGIAO) %>% 
  group_by(REGIAO) %>% 
  summarise(total = n())
#############################

#Total agrupado por Raca
DIREITO %>% 
  select(RACA) %>% 
  group_by(RACA) %>% 
  summarise(total = n())
#############################

#Total agrupado por Turno 
DIREITO %>% 
  select(TURNO) %>% 
  group_by(TURNO) %>% 
  summarise(total = n())
#############################

#Total agrupado por Sexo
DIREITO %>% 
  select(SEXO) %>% 
  group_by(SEXO) %>% 
  summarise(total = n())
#############################





#################### Tabelas de Cruzamento
table(DIREITO$NOTAS)
prop.table(table(DIREITO$TURNO))

table(DIREITO$CURSO)
prop.table(table(DIREITO$REGIAO))

table(DIREITO$TURNO, DIREITO$REGIAO)
prop.table(table(DIREITO$TURNO, DIREITO$REGIAO))


NOTA_TURNO=DIREITO %>% select (TURNO, NOTAS) %>%
                         group_by (TURNO) %>%
                         summarise (MEDIA = mean(NOTAS))
NOTA_TURNO

NOTA_TURNO_REG = DIREITO %>% select (TURNO, REGIAO, NOTAS, RACA) %>%
                             group_by (TURNO, REGIAO, RACA) %>%
                             summarise (MEDIA = mean(NOTAS))
NOTA_TURNO_REG
####################################


################## VISUALIZA??O GR?FICA DE DADOS ################
#An?lise das Notas dos Alunos por Ra?as
#Gr?fico de Densidade de Alunos por Ra?as e Regi?es

dados=DIREITO
grafico_geom_density1=ggplot(dados,aes(NOTAS,fill=RACA))+
  geom_density(alpha=0.6)+
  xlab("Nota dos Alunos")+
  ylab("Densidade")+
  ggtitle("Gr?fico de Densidade das Notas dos Alunos por Ra?as e Regi?es")+
  facet_grid(~REGIAO)

ggplotly(grafico_geom_density1)
#####################################################


########Gr?fico Histograma das Notas dos Alunos por Ra?as-Frequ?ncia Simples
dados=DIREITO
grafico_histograma1 = ggplot(dados, aes(x=NOTAS,fill=RACA))+ 
  geom_histogram() +
  ggtitle("Gr?fico Histograma das Notas dos Alunos por Ra?as e Regi?es-Frequ?ncia Simples")+
  xlab("Nota") +
  ylab("Frequ?ncia simples") +
  facet_grid(~REGIAO)

ggplotly(grafico_histograma1)
###################################################################

#Gr?fico Box-plot das Notas dos Alunos por Ra?as e Regi?es
dados=DIREITO
grafico_boxplot1 = ggplot(dados, aes(x=RACA,y=NOTAS,fill=RACA)) + 
  geom_boxplot() +
  ggtitle("Gr?fico Box-plot das Notas dos Alunos por Ra?as e Regi?es") +
  xlab("Ra?a") +
  ylab("Nota") +
  facet_grid(~REGIAO)

ggplotly(grafico_boxplot1)
###############################################


#An?lise das Notas dos Alunos por Regi?es
#Gr?fico de Densidade de Alunos por Regi?es e Ra?as
dados=DIREITO
grafico_geom_density2=ggplot(dados,aes(NOTAS,fill=REGIAO))+
  geom_density(alpha=0.6)+
  xlab("Nota")+
  ylab("Densidade")+
  ggtitle("Gr?fico de Densidade de Alunos por Regi?es e Ra?as")+
  facet_grid(~RACA)

ggplotly(grafico_geom_density2)
####################################

#Gr?fico Histograma das Notas dos Alunos por Regi?es-Frequ?ncia Simples
dados=DIREITO
grafico_histograma2 = ggplot(dados, aes(x=NOTAS,fill=REGIAO)) + 
  geom_histogram() +
  ggtitle("Gr?fico Histograma das Notas dos Alunos por Regi?es e Ra?as-Frequ?ncia Simples") +
  xlab("Nota") +
  ylab("Frequ?ncia simples") +
  facet_grid(~RACA)

ggplotly(grafico_histograma2)
#############################################

#Gr?fico Box-plot das Notas dos Alunos por Regi?es e Ra?as
dados=DIREITO
grafico_boxplot2 = ggplot(dados, aes(x=Regiao,y=NOTAS,fill=REGIAO)) + 
  geom_boxplot() +
  ggtitle("Gr?fico Box-plot das Notas dos Alunos por Regi?es e Ra?as") +
  xlab("Regi?o") +
  ylab("Nota") +
  facet_grid(~RACA)

ggplotly(grafico_boxplot2)
#############################################


#################################### An?lise das Notas dos Alunos por Turnos
#Gr?fico de Densidade de Alunos por Turnos e Ra?as
dados=DIREITO
grafico_geom_density3=ggplot(dados,aes(NOTAS,fill=TURNO))+
  geom_density(alpha=0.6)+
  xlab("Nota")+
  ylab("Densidade")+
  ggtitle("Gr?fico de Densidade de Alunos por Turnos e Ra?as")+
  facet_grid(~RACA)

ggplotly(grafico_geom_density3)
#############################################

#Gr?fico Histograma das Notas dos Alunos por Turnos-Frequ?ncia Simples
dados=DIREITO
grafico_histograma3 = ggplot(dados, aes(x=NOTAS,fill=TURNO)) + 
  geom_histogram() +
  ggtitle("Gr?fico Histograma das Notas dos Alunos por Turnos e Ra?as-Frequ?ncia Simples") +
  xlab("Nota") +
  ylab("Frequ?ncia simples") +
  facet_grid(~RACA)

ggplotly(grafico_histograma3)
#############################################


#Gr?fico Box-plot das Notas dos Alunos por Turnos e Ra?as
dados=DIREITO
grafico_boxplot3 = ggplot(dados, aes(x=Turno,y=NOTAS,fill=TURNO)) + 
  geom_boxplot() +
  ggtitle("Gr?fico Box-plot das Notas dos Alunos por Turnos e Ra?as") +
  xlab("Turno") +
  ylab("Nota") +
  facet_grid(~RACA)

ggplotly(grafico_boxplot3)
#############################################

#An?lise das Notas dos Alunos por Turnos e Regi?es
#Gr?fico de Densidade de Alunos por Turnos e Regi?es
dados=DIREITO
grafico_geom_density4=ggplot(dados,aes(NOTAS,fill=TURNO))+
  geom_density(alpha=0.6)+
  xlab("Nota")+
  ylab("Densidade")+
  ggtitle("Gr?fico de Densidade de Alunos por Turnos e Regi?es")+
  facet_grid(~REGIAO)

ggplotly(grafico_geom_density4)
######################################################


#Gr?fico Histograma das Notas dos Alunos por Turnos-Frequ?ncia Simples
dados=DIREITO
grafico_histograma4 = ggplot(dados, aes(x=NOTAS,fill=TURNO)) + 
  geom_histogram() +
  ggtitle("Gr?fico Histograma das Notas dos Alunos por Turnos-Frequ?ncia Simples") +
  xlab("Notas") +
  ylab("Frequ?ncia simples") +
  facet_grid(~REGIAO)

ggplotly(grafico_histograma3)
###################################################



#Gr?fico Box-plot das Notas dos Alunos por Turnos e Regi?es
dados=DIREITO
grafico_boxplot4 = ggplot(dados, aes(x=Turno,y=NOTAS,fill=TURNO)) + 
  geom_boxplot() +
  ggtitle("Gr?fico Box-plot das Notas dos Alunos por Turnos e Regi?es") +
  xlab("Turno") +
  ylab("Nota") +
  facet_grid(~REGIAO)

ggplotly(grafico_boxplot4)
###################################################


############################ TESTE DE HIP?TESES ###########
#[Hip?tese 1]: H? raz?o para desconfiar que pessoas brancas 
#tem melhores notas que amarelas?

#An?lise da Hip?tese:
#Tabula??o cruzada entre as vari?veis ra?a e notas dos alunos 
table(DIREITO$RACA,DIREITO$NOTAS)


#[Hip?tese 2]: H? raz?o para desconfiar que pessoas que estudam no turno da manh? 
#tem maiores notas que os que estudam ? noite?

#An?lise da Hip?tese:
#Tabula??o cruzada entre as vari?veis turnos e notas dos alunos 
table(DIREITO$TURNO, DIREITO$NOTAS)







