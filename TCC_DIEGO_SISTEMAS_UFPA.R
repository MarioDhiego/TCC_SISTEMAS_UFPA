#################################################################################################
## Universidade : UFPA                       ####################################################
## Curso        : Sistemas de Informação     ####################################################
## Matrícula    : 202240011142               ####################################################
## Discente     : Mário Diego Rocha Valente  ####################################################
## Orientador   : Prof Dr. Dione Cavalcante Monteiro  ###########################################
## Co-Orientador: Prof Dr. Heliton Ribeiro Tavares    ###########################################
#################################################################################################


#################################################################################################
## TíTULO: Avaliação do Desenpenho dos alunos matriculado                        ################
## nos Cursos da Area de Computação, UFPA, que Prestaram ao ENADE DE 2005 A 2017 ################
#################################################################################################
## OBJETIVO GERAL: Mensuarar o Desenpenhos(proficiência) dos alunos matriculado  ################
## nos Cursos da Area de Computação, UFPA, que Prestaram ao ENADE DE 2005 A 2017 ################
#################################################################################################


### PASSO 1: Baixar os Microdados do ENADE, no Sítio do INEP ###################################

## https://download.inep.gov.br/microdados/microdados_enade_2017_LGPD.zip
################################################################################################


### Passo 2: Criação de Diretório de Trabalho: Computador + Github #############################
## Definir Diretorio de TrabalhO ###############################################################
## Definir Local
setwd("C:/Users/usuario/Documents/TCC_SISTEMAS_UFPA")

## Testar Local
getwd()
################################################################################################


### PASSO 3: INSTALAÇÃO DOS PACOTES NECESSÁRIOS ################################################
## Instalar Pacotes para leitura de Bases de Dados #############################################
install.packages(c("readr", "readxl", "readxlsx", "have"))
################################################################################################ 

## Instalar Pacotes para Manipulação nos Dados #################################################
install.packages(c("tidyverse","tibble","purrr","forcats","stringr","magrittr",
                   "dplyr","tidyr","lubridate"))
################################################################################################

## Instalar Pacotes para Resumo nos Dados ######################################################
install.packages(c("DT","data.table","DescTools","janitor"))
################################################################################################

## Instalar Pacotes para Visualização nos Dados ################################################
install.packages(c("ggplot2","graphics","ggThemes","plotly", "ggpubr"))
################################################################################################

## Instalar Pacotes para Relatórios Dinâmico ###################################################
install.packages(c("tinytex","knitr","kableExtra","formattable", "htmltools", "rmarkdown"))
################################################################################################


################################################################################################
### PASSO4: Ativação dos Pacotes ###############################################################

# Pacotes p/ Leitura de Base de Dados
library(readr)
library(readxl)
library(openxlsx)

# Pacotes p/ Maninulacao de Dados
library(tidyverse)
library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(purrr)
library(forcats)
library(stringr)

# Pacotes p/ Representação Tabular
library(DT)
library(data.table)
library(reactable)
library(kableExtra)
library(DescTools)
library(devtools)
library(rmarkdown)
library(knitr)
library(e1071)

# Pacotes p/ Representação Gráfica 
library(ggplot2)
library(plotly)
library(graphics)
library(grid)
library(ggpubr)
library(gridExtra)
library(ggthemes)
library(ggThemeAssis)
library(dygraphs)
library(esquisse)
library(RColorBrewer)
library(wesanderson)
library(falrec)

# Pacotes p/ Relatórios Dinâmicos
library(tinytex)
library(knitr)
library(kableExtra)
library(formattable)
library(htmltools)
#library(rmarkdown)
################################################################################################

# Desativar Pacotes
detach("package:tidyverse", unload = TRUE)
################################################################################################


################################################################################################
## PASSO5: Fazer Leitura dos Microdados do ENADE ###############################################
# OPÇÃO1: Leitura da Base de Dados em .txt

Base_Completa = read.table("MICRODADOS_ENADE_2017.txt", 
                         header=TRUE, 
                         sep=";", 
                         dec=",", 
                         colClasses=c(NT_OBJ_FG="numeric"))

# Banco Geral
MICRODADOS_ENADE_2017 <- read_csv("MICRODADOS_ENADE_2017.csv")
MICRODADOS_ENADE_2017 <- read_csv2("MICRODADOS_ENADE_2017.txt")

#  FOrmação Geral
BANCO_ITENS_FG <- read_excel("BANCO_ITENS_FG.xlsx")
BANCO_ITENS_CE <- read_excel("BANCO_ITENS_CE.xlsx")


################################################################################################

## Leitura de Base de Dados via URL do github ##################################################
# OPÇÃO2: Fazer o Download da Base Compactada .rar

download.file(url = "https://github.com/MarioDhiego/ENADE_2018_RMarkdown/blob/main/microdados_enade_2018.rar", destfile = "C:/Users/mario.valente/Documents/GitHub_scripts/TCC_DIEGO/microdados_enade_2018.rar")

datazip <- unzip("TCC_DIEGO/microdados_enade_2018.rar",
                 exdir="TCC_DIEGO")
################################################################################################

### Leia os Dados a partir do disco, sem carrega-los na RAM ####################################
# OPÇÃO3: Instalar o Pacote ff
#install.packages("ff")

# Ativar o Pacote ff
library(ff)

# Leitura dos Microdados
ENADE_2018 <- read.csv.ffdf(file="microdados_enade_2018.txt", header=TRUE)
#####################################################################################################



### Instalando Gerenciador de Base de Dados: RSQlite ou MonetDBLite #################################
#
install.packages('RSQlite', dependencies = TRUE)
install.packages('MonetDBLite', dependencies = TRUE)
#####################################################################################################


#####################################################################################################
### PASSO6: MANIPULAÇÃO DOS MICRODADOS/FAXINA #######################################################
### Filtrar as Variáveis SocioEconomicas ############################################################
Base_Filtrada = MICRODADOS_ENADE_2017 %>% 
  dplyr::select (CO_IES,
                 CO_CATEGAD,
                 CO_GRUPO,
                 CO_CURSO,
                 CO_MODALIDADE,
                 CO_MUNIC_CURSO,
                 CO_UF_CURSO,
                 CO_REGIAO_CURSO,
                 CO_TURNO_GRADUACAO,
                 TP_SEXO,
                 NU_IDADE,
                 QE_I01,
                 QE_I02,
                 QE_I06,
                 QE_I07,
                 QE_I08,
                 QE_I10,
                 QE_I23,
                 NT_GER,
                 NT_CE,
                 NT_OBJ_FG,
                 NT_OBJ_CE,
                 DS_VT_ESC_OFG,
                 DS_VT_ESC_OCE,
                 DS_VT_ACE_OFG,
                 DS_VT_ACE_OCE,
                 TP_PR_GER)
#####################################################################################################


#####################################################################################################
########################### Descrição das Variáveis #################################################

### Institucional ###################################################################################
# CO_IES             : Codigo da Instituição Ensino Superior(IES);
# CO_CATEGAD         : Código da categoria administrativa da IES;
# CO_CURSO           : Codigo do Curso da IES;
# CO_GRUPO           : Codigo Area de enquadramento do Curso
# CO_MODALIDADE      : Codigo da Modalidade do Curso(Presencial/Distancia)
# CO_MUNIC_CURSO     : Codigo do Município Funcionamento do Curso;
# CO_UF_CURSO        : Codigo da UF de Funcionamento do Curso;
# TP_PR_GER          : Tipo de Presença na Prova;
#####################################################################################################

### SocioDemografica ################################################################################
# CO_REGIA_CURSO     : Codigo da Regiao de Funcionamento do Curso;
# QE_I01             : Codigo do Estado Civil dos Alunos;
# QE_I02             : Codigo da Raça dos Alunos;
# QE_I06             : Codigo da Moradia dos Alunos;
# QE_I07             : Codigo do Número de Pessoas Moram na Casa;
# QE_I10             : Codigo de Trabalho dos Alunos;
# QE_I23             : Codigo Horas por semana,você dedicou aos estudos, excetuando as horas de aula?;
# CO_TURNO_GRADUACAO : Codigo do Turno de Matricula dos Alunos;
# TP_SEXO            : Sexo declarado dos Alunos;
# NU_IDADE           : Idade dos Alunos;
#####################################################################################################

### Notas ###########################################################################################
# NT_GER             : Nota Bruta da Prova;
# NT_FG              : Nota bruta na Formação Geral;
# NT_CE              : Nota Bruta no Componente Específico;
# NT_OBJ_FG          : Nota Bruta/Prova Objetiva/Componente: Formação Geral;
# NT_OBJ_CE          : Nota Bruta/Prova Objetiva/Componente: Componente Especifico;
#####################################################################################################


### Classificação das Variaveis #####################################################################
# NT_OBJ_FG          : Variavel Quantitativa Contínua 
# CO_GRUPO           : Variavel Qualitativa Nominal
# CO_REGIAO_CURSO    : Variavel Qualitativa Nominal
# QE_I02             : Variavel Qualitativa Nominal
# CO_TURNO_GRADUACAO : Variavel Qualitativa Ordinal
#####################################################################################################


### Visualização das Bases de Dados #################################################################
# Base Completa
View(Base_Completa)
glimpse(Base_Completa)

# Base Filtrada
View(Base_Filtrada)
glimpse(Base_Filtrada)
#####################################################################################################


### Checagem e Verificação dos Filtros ##############################################################
# Conferindo o Nome das Variaveis 
names(Base_Completa)
names(Base_Filtrada)

# Verificando as dimensÃµes (numero de linhas e colunas)
dim(Base_Completa)
dim(Base_Filtrada)

# Resumo dos Dados com as Variaveis selecionadas
Resumo_Geral = summary(Base_Filtrada)
Resumo

# Resumo Curso de Sistemas
Resumo_Sistemas = summary(SISTEMAS_20117)
Resumo_Sistemas

# Resumo Curso de Ciencia da Computação
Resumo_Ciencias = summary(BANCO_ITENS_FG)
Resumo_Ciencias


# Resumo Curso de Engenharia da Computação
Resumo_Engenharia = summary(Engenharia_Computacao)
Resumo_Ciencias

Descritiva = describe(BANCO_ITENS_FG)
Descritiva
######################################################################################################


### Selecionar uma Instituição: UFPA, UNAMA, CESUPA ##################################################
# Filtrar somente as linhas(alunos) da Instituição (UFPA).
Base_Filtrada_UFPA = subset(Base_Filtrada, Base_Filtrada$CO_IES == 792)
######################################################################################################

### Selecionar um Municipio: Belém ###################################################################
# Filtrar somente as linhas(alunos) do Municipio de Belém 
Base_Filtrada_BELEM = subset(Base_Completa, Base_Completa$CO_MUNIC_CURSO == 1501402);
######################################################################################################


### Selecionar Uma UF: PA ############################################################################
# Filtrar somente as linhas(alunos) do PARÁ (UF).
Base_Filtrada_PARA = subset(Base_Completa, Base_Completa$CO_UF_CURSO == 15);
######################################################################################################


### Selecionar um Curso ##############################################################################


#Filtrando os Dados p/Sistema de Informação
SISTEMAS_2017= Base_Filtrada %>% filter(CO_GRUPO== 4006) 

#Filtrando os Dados p/Ciencia da Computação Bacharelado
Ciencia_Computacao_Bach= Base_Filtrada %>% filter(CO_GRUPO== 4004)

#Filtrando os Dados p/Ciencia da Computação Licenciatura
Ciencia_Computacao_Lice= Base_Filtrada %>% filter(CO_GRUPO== 4005)

#Filtrando os Dados p/Ciencia da Computação Licenciatura
Engenharia_Computacao= Base_Filtrada %>% filter(CO_GRUPO== 4003)

######################################################################################################


#### Salvar as Novas Bases de Dados Limpa ############################################################

write.csv(SISTEMAS_SEM_NA, file= "microdados_enade_2017_Sistemas_SEM_NAS.csv")


write.csv(Ciencia_Computacao_Bach, file= "microdados_enade_2017_Ciencias_Computacao.csv")
######################################################################################################


####################### Transformar o Curso/SISTEMAS #################################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(CURSO = case_when(CO_GRUPO == 4006 ~ "Sistemas de Informação"))

COMPUTACAO_BACHARELADO = COMPUTACAO_BACHARELADO %>% mutate(CURSO=case_when(CO_GRUPO== 4004 ~ "Ciência da Computação Bacharelado"))


COMPUTACAO_LICENCIATURA = COMPUTACAO_LICENCIATURA %>% mutate(CURSO=case_when(CO_GRUPO== 4005 ~ "Ciência da Computação Licenciatura"))
######################################################################################################


######################################################################################################
### Transformar a Variavel: CO_REGIAO in REGIAO ######################################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(REGIAO = case_when(CO_REGIAO_CURSO == 1 ~ "Norte",
                            CO_REGIAO_CURSO == 2 ~ "Nordeste",
                            CO_REGIAO_CURSO == 3 ~ "Sudeste",
                            CO_REGIAO_CURSO == 4 ~ "Sul",
                            CO_REGIAO_CURSO == 5 ~ "Centro-Oeste"))
######################################################################################################


######################################################################################################
### Transformar a Variavel: CO_TURNO_GRADUACAO in TURNO ##############################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(TURNO = case_when(CO_TURNO_GRADUACAO == "1" ~ "Matutino",
                           CO_TURNO_GRADUACAO == "2" ~ "Vespertino",
                           CO_TURNO_GRADUACAO == "3" ~ "Integral",
                           CO_TURNO_GRADUACAO == "4" ~ "Noturno"))
######################################################################################################


### Transformar a Variável: CO_CATEGAD in Categoria da IES ###########################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(CATEGORIA_IES = case_when(CO_CATEGAD == "1" ~ "Pública Federal",
                                   CO_CATEGAD == "2" ~ "Pública Estadual",
                                   CO_CATEGAD == "3" ~ "Pública Municipal",
                                   CO_CATEGAD == "4" ~ "Privada c/ Fins Lucrativos",
                                   CO_CATEGAD == "5" ~ "Privada s/ Fins Lucrativos"))
#######################################################################################################


### Transformar a Variavel: CO_MODALIDADE in MODALIDADE ################################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(MODALIDADE = case_when(CO_MODALIDADE == "1" ~ "Presencial",
                                CO_MODALIDADE == "0" ~ "EAD"))
########################################################################################################


########################################################################################################
### Transformar a Variavel: TP_SEXO in SEXO ############################################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(SEXO = case_when(TP_SEXO == "F" ~ "Feminino",
                          TP_SEXO == "M" ~ "Masculino"))
########################################################################################################


########################################################################################################
### Transformar a Variavel: QE_I01 in ESTADO CIVIL #####################################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(ESTADO_CIVIL = case_when(QE_I01 == "A" ~ "Solteiro",
                                  QE_I01 == "B" ~ "Casado",
                                  QE_I01 == "C" ~ "Separado/Divorciado",
                                  QE_I01 == "D" ~ "Viuvo",
                                  QE_I01 == "E" ~ "Outro"))
########################################################################################################


########################################################################################################
### Transformar a Variavel: QE_I02 in RACA #############################################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(RACA = case_when(QE_I02 == "A" ~ "Branca",
                          QE_I02 == "B" ~ "Preta",
                          QE_I02 == "C" ~ "Amarela",
                          QE_I02 == "D" ~ "Indigena",
                          QE_I02 == "E" ~ "ND"))
########################################################################################################


########################################################################################################
### Transformar a Variavel: QE_I06 in MORADIA ##########################################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(MORADIA = case_when(QE_I06 == "A" ~ "Sozinho",
                             QE_I06 == "B" ~ "Pais e/ou parentes",
                             QE_I06 == "C" ~ "Cônjuge e/ou filhos",
                             QE_I06 == "D" ~ "Outras pessoas",
                             QE_I06 == "E" ~ "Alojamento universitário",
                             QE_I06 == "F" ~ "Hospedaria ou outro"))
########################################################################################################


#########################################################################################################
### Transformar a Variavel: QE_I07 in Nº de Pessoas/Familiares  #########################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(FAMILIARES = case_when(QE_I07 == "A" ~ "Nenhum",
                                QE_I07 == "B" ~ "Um",
                                QE_I07 == "C" ~ "Dois",
                                QE_I07 == "D" ~ "Três",
                                QE_I07 == "E" ~ "Quatro",
                                QE_I07 == "F" ~ "Cinco",
                                QE_I07 == "G" ~ "Seis",
                                QE_I07 == "H" ~ "Sete ou +"))
#########################################################################################################


########################################################################################################
### Transformar a Variável: QE_I08 in Renda Familiar ###################################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(RENDA = case_when(QE_I08 == "A" ~ "Até 1,5 S.M",
                           QE_I08 == "B" ~ "1,5 a 3 S.M",
                           QE_I08 == "C" ~ "3 a 4,5 S.M",
                           QE_I08 == "D" ~ "4,5 a 6 S.M",
                           QE_I08 == "E" ~ "6 a 10 S.M",
                           QE_I08 == "F" ~ "10 a 30 S.M",
                           QE_I08 == "G" ~ "Acima de 30 S.M"))
########################################################################################################


########################################################################################################
### Transformar a Variável: QE_I10 in Trabalho #########################################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(TRABALHO = case_when(QE_I10 == "A" ~ "Desempregado",
                              QE_I10 == "B" ~ "Eventualmente",
                              QE_I10 == "C" ~ "Até 20 hs",
                              QE_I10 == "D" ~ "21 a 39 hs",
                              QE_I10 == "E" ~ "40h ou +"))
########################################################################################################


########################################################################################################
### Transformar a Variável: QE_I23 Horas de Estudo #####################################################
SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(HORAS_ESTUDO = case_when(QE_I23 == "A" ~ "Nenhuma",
                                  QE_I23 == "B" ~ "De  1 a 3 horas",
                                  QE_I23 == "C" ~ "De 4 a 7",
                                  QE_I23 == "D" ~ "De 8 a 12",
                                  QE_I23 == "E" ~ "Mais de 12"))
########################################################################################################


########################################################################################################
### Codificacao da NU_IDADE para Faixa-Etária 
# Faixa 1 = até 25 anos
# Faixa 2 = entre 26 e 30 anos
# Faixa 3 = Acima de 30 anos
 
SISTEMAS_2017[,length(SISTEMAS_2017)] = ifelse(SISTEMAS_2017$NU_IDADE<=24,"Faixa1",
                                     ifelse(SISTEMAS_2017$NU_IDADE>=30,"Faixa3","Faixa2"))
names(SISTEMAS_2017)[length(SISTEMAS_2017)] = "Faixa_Etaria" 
 
########################################################################################################
### Transformar a Variável: NU_IDADE Faixa-Etária  #####################################################

SISTEMAS_2017 = SISTEMAS_2017 %>% 
  mutate(FAIXA_ETARIA = case_when(Faixa_Etaria == "Faixa1" ~ "Até 24 Anos",
                                  Faixa_Etaria == "Faixa2" ~ "Entre 25 e 30 Anos",
                                  Faixa_Etaria == "Faixa3" ~ "Acima de 30 Anos"))
########################################################################################################

################## Removendo as Variaveis antigas ######################################################
SISTEMAS_2017 = SISTEMAS_2017[,-c(2,3,5,8,9,10,12,13,14,15,16,17,18,40)]

ED = summary(SISTEMAS_SEM_NA)
ED
#
d=describe(SISTEMAS_SEM_NA)
d


########### Medidas Estatisticas ############

#CAlculo do tamanho do vetor de Notas
quantidade_de_notas_NORMAL_CE =length(SISTEMAS_2017$NT_OBJ_CE)
quantidade_de_notas_NORMAL_FG =length(SISTEMAS_2017$NT_OBJ_FG)

quantidade_de_notas_SEM_NA_CE =length(SISTEMAS_SEM_NA$NT_OBJ_CE)
quantidade_de_notas_SEM_NA_FG =length(SISTEMAS_SEM_NA$NT_OBJ_FG)




#Calculo da Media
MEDIA = mean(SISTEMAS_2017$NT_OBJ_CE)
MEDIA = mean(SISTEMAS_SEM_NA$NT_OBJ_CE)

#Calculo da Mediana
MEDIANA = median(SISTEMAS_2017$NT_OBJ_CE)
MEDIANA = median(SISTEMAS_SEM_NA$NT_OBJ_CE)

####### Calculo da Moda
#Primeira etapa: Calcular as frequencias simples
FREQUENCIA = table(SISTEMAS_20117$NT_OBJ_CE)
FREQUENCIA = table(SISTEMAS_SEM_NA$NT_OBJ_CE)

#Segunda etapa: Calcular o maximo das frequencias simples
MAXIMO = max(FREQUENCIA)

#Terceira etapa: Trazer os nomes que correspondem as observacoes das FREQUENCIA
NOMES = names(FREQUENCIA)

#Quarta etapa: Trazer os nomes que satisfazem a comparação logica
MODA_TEXTO = NOMES[FREQUENCIA == MAXIMO]
class(MODA_TEXTO)

#Quinta etapa: Transformar de caractere em numero
MODA_NUMERO = as.numeric(MODA_TEXTO)
class(MODA_NUMERO)

#Calculo da Variancia
VARIANCIA = var(SISTEMAS_20117$NT_OBJ_CE)
VARIANCIA = var(SISTEMAS_SEM_NA$NT_OBJ_CE)

#Calculo da VariAncia
DESVIO = sd(SISTEMAS_20117$NT_OBJ_CE)
DESVIO = sd(SISTEMAS_SEM_NA$NT_OBJ_CE)

#Calculo dO Coeficiente de Variação
CV=sd(SISTEMAS_20117$NT_OBJ_CE)/mean(SISTEMAS_20117$NT_OBJ_CE)*100
CV=sd(SISTEMAS_SEM_NA$NT_OBJ_CE)/mean(SISTEMAS_SEM_NA$NT_OBJ_CE)*100

#Calculo da Assimetria:
AS=skewness(SISTEMAS_20117$NT_OBJ_CE)
AS=skewness(SISTEMAS_SEM_NA$NT_OBJ_CE)

#Calculo do Coeficiente de Curtose da forma como o R calcula, comparada a da normal
CURTOSE=kurtosis(SISTEMAS_20117$NT_OBJ_CE)
CURTOSE=kurtosis(SISTEMAS_SEM_NA$NT_OBJ_CE)

#Resumo das Estatisticas
RESULTADO = c(MAXIMO,
              MEDIA,
              MEDIANA,
              MODA_NUMERO,
              VARIANCIA,
              DESVIO,
              CV,
              AS,
              CURTOSE)
RESULTADO
########################################################################################################


### ANALISE DE OUTLIER'S/MISSING VALUES #######################################################
### Nota Agrupado/Classes: identificar possiveis dados faltantes (NAs) ########################
SISTEMAS_SEM_NA %>% 
  select(NT_OBJ_CE) %>% 
  group_by(NT_OBJ_CE) %>% 
  summarise(total = n())

#Contabilizando os Na´s
resumo_nas= SISTEMAS_20117 %>%
  select(everything()) %>%  
  summarise_all(list(~sum(is.na(.))))

#Removendo  Na´S De todas As variáveis que possuem NA
SISTEMAS_SEM_NA = SISTEMAS_2017 %>% na.omit()

#Verificando de foram retirados os NA´S
resumo_nas = SISTEMAS_SEM_NA %>%
  select(everything()) %>%  
  summarise_all(list(~sum(is.na(.))))
resumo_nas %>% kbl %>% kable_material_dark(full_width = F)

# Nota Agrupado/Classes Sem os NA´S
SISTEMAS_SEM_NA %>% 
  select(NT_OBJ_FG) %>% 
  group_by(NT_OBJ_FG) %>% 
  summarise(total = n())

SISTEMA_sem_NA %>% 
  select(NT_OBJ_FG) %>% 
  summarise(  quantidade=n(),
              media = mean(NT_OBJ_FG),
              mediana = median(NT_OBJ_FG),
              moda=Mode(NT_OBJ_FG),
              cv=sd(NT_OBJ_FG)/media*100,
              assimetria=skewness(NT_OBJ_FG),
              curtose=kurtosis(NT_OBJ_FG)
  ) %>% 
  arrange(desc(mediana))  %>% 
  kbl %>% 
  kable_material_dark(full_width = F)

####
SISTEMA_sem_NA %>% 
  select(NT_OBJ_CE) %>% 
  group_by(NT_OBJ_CE) %>% 
  summarise(total = n())

SISTEMA_sem_NA %>% 
  select(NT_OBJ_CE) %>% 
  summarise(  quantidade=n(),
              media = mean(NT_OBJ_CE),
              mediana = median(NT_OBJ_CE),
              moda=Mode(NT_OBJ_CE),
              cv=sd(NT_OBJ_CE)/media*100,
              assimetria=skewness(NT_OBJ_CE),
              curtose=kurtosis(NT_OBJ_CE)
  ) %>% 
  arrange(desc(mediana))  %>% 
  kbl %>% 
  kable_material_dark(full_width = F)


# Estatística Resumo
summary(SISTEMAS_SEM_NA$NT_OBJ_CE)
summary(SISTEMAS_SEM_NA$NT_OBJ_FG)




# Histograma Notas Objetivas FG(formação Geral)
g_hist_densidade1 = ggplot(SISTEMAS_20117,
                          aes(x= NT_OBJ_FG)) + 
  geom_histogram(color = "black",fill="blue",bins =50,aes(y=(..count..)/sum(..count..)))+
  geom_density(col=2, aes(y = 27 * (..count..)/sum(..count..))) +
  ggtitle("Formação Geral")+
  xlab("Nota dos alunos de Sistemas") +
  ylab("Frequência relativa")
ggplotly(g_hist_densidade1)


# Histograma Notas Objetivas CE(componente específico)
g_hist_densidade2 = ggplot(MICRODADOS_SISTEMA_sem_NA,
                          aes(x= NT_OBJ_CE)) + 
  geom_histogram(color = "black",fill="blue",bins =50,aes(y=(..count..)/sum(..count..)))+
  geom_density(col=2, aes(y = 27 * (..count..)/sum(..count..))) +
  ggtitle("Componente Específico")+
  xlab("Nota dos alunos de Sistemas") +
  ylab("Frequência relativa")
ggplotly(g_hist_densidade2)



################################################################################################

### Total Agrupado/Sexo: identificar possiveis dados faltantes (NAs) ###########################
SISTEMAS %>% 
  select(SEXO) %>% 
  group_by(SEXO) %>% 
  summarise(total = n()) %>% 
  arrange(-total)
################################################################################################

### Total Agrupado/Região: identificar possiveis dados faltantes (NAs) #########################
SISTEMAS %>% 
  select(REGIAO) %>% 
  group_by(REGIAO) %>% 
  summarise(total = n())%>% 
  arrange(-total)
################################################################################################

### Total Agrupado/Raça: identificar possiveis dados faltantes (NAs)
SISTEMAS %>% 
  select(RACA) %>% 
  group_by(RACA) %>% 
  summarise(total = n()) %>% 
  arrange(-total)
#################################################################################################


### Total agrupado/Turno: identificar possiveis dados faltantes (NAs) ############################
SISTEMAS %>% 
  select(TURNO) %>% 
  group_by(TURNO) %>% 
  summarise(total = n()) %>% 
  arrange(-total)


###################################################################################################


### Retirando os missing de todas variaveis
SISTEMAS_SEM_NA = SISTEMAS_2017 %>% na.omit()
ED_SEM_MISSING = summary(SISTEMAS_SEM_NA)
ED_SEM_MISSING

# Resumo das Variáveis SEM/NA'S
Resumo_NAS = SISTEMAS_SEM_NA %>%
  select(everything()) %>%  
  summarise_all(list(~sum(is.na(.))))
Resumo_NAS
################################################################################################



### PASSO 7: Visualização dos Dados ############################################################
### Notas dos Alunos: (RAÇA)
# Gráfico1 de Densidade de Alunos por Raças e Regiões
dados = SISTEMAS_SEM_NA
grafico_geom_density1=ggplot(dados,aes(NT_OBJ_FG,fill= ESTADO_CIVIL))+
  geom_density(alpha=0.6)+
  xlab("Notas")+
  ylab("Densidade")+
  ggtitle("Grafico de Densidade das Notas dos Alunos por Raças e Regioes")+
  facet_grid(~REGIAO)
ggplotly(grafico_geom_density1)
################################################################################################


dados = SISTEMAS_SEM_NA
grafico_histograma1 = ggplot(dados, aes(x=NT_GER,fill=CATEGORIA_IES))+ 
  geom_histogram() +
  ggtitle("Histograma das Notas dos Alunos por Raças e Regiões-Frequência Simples")+
  xlab("Notas") +
  ylab("Frequência simples") +
  facet_grid(~REGIAO)
ggplotly(grafico_histograma1)


dados= SISTEMAS_SEM_NA
grafico_histograma2 = ggplot(dados, aes(x=NT_GER,fill=REGIAO)) + 
  geom_histogram() +
  ggtitle("Histograma das Notas dos Alunos por Regiões e Raças-Frequência Simples") +
  xlab("Notas") +
  ylab("Frequência Simples") +
  facet_grid(~RACA)
ggplotly(grafico_histograma2)


dados= SISTEMAS_SEM_NA
grafico_histograma3 = ggplot(dados, aes(x=NT_GER,fill=TURNO)) + 
  geom_histogram() +
  ggtitle("Histograma das Notas dos Alunos por Turnos e Raças-Frequência Simples") +
  xlab("Notas") +
  ylab("Frequência simples") +
  facet_grid(~RACA)
ggplotly(grafico_histograma3)


dados= SISTEMAS_SEM_NA
grafico_histograma4 = ggplot(dados, aes(x=NT_GER,fill=TURNO)) + 
  geom_histogram() +
  ggtitle("Histograma das Notas dos Alunos por Turnos-Frequência Simples") +
  xlab("Notas") +
  ylab("Frequência simples") +
  facet_grid(~REGIAO)
ggplotly(grafico_histograma4)



dados = SISTEMAS_SEM_NA
grafico_histograma5 = ggplot(dados, aes(x=NT_GER,fill=TURNO)) + 
  geom_histogram() +
  ggtitle("Histograma das Notas dos Alunos por Turnos-Frequência Simples") +
  xlab("Notas") +
  ylab("Frequência simples") +
  facet_grid(~SEXO)
ggplotly(grafico_histograma5)

dados = SISTEMAS_SEM_NA
grafico_histograma6 = ggplot(dados, aes(x=NT_GER,fill=REGIAO)) + 
  geom_histogram() +
  ggtitle("Histograma das Notas dos Alunos por Turnos-Frequência Simples") +
  xlab("Notas") +
  ylab("Frequência simples") +
  facet_grid(~SEXO)
ggplotly(grafico_histograma6)

dados = SISTEMAS_SEM_NA
grafico_histograma7 = ggplot(dados, aes(x=NT_GER,fill= ESTADO_CIVIL)) + 
  geom_histogram() +
  ggtitle("Histograma das Notas dos Alunos por Turnos-Frequência Simples") +
  xlab("Notas") +
  ylab("Frequência simples") +
  facet_grid(~SEXO)
ggplotly(grafico_histograma7)


################################################################################################








#################################################################################################
### Passo 8: Analise via Teoria Classica dos Testes #############################################



### Instalação dos Pacotes ######################################################################
install.packages(c("tinytex","knitr","kableExtra","formattable", "htmltools", "rmarkdown"))
################################################################################################

### Ativação dos Pacotes ########################################################################
library(ltm)
library(psych)
library(irtoys)
#library(lordif)
library(FactoMineR)
library(CTT)
library(mirt)
library(lavaan)
library(KernSmoothIRT)
library(d3heatmap)
##################################################################################################



### Teoria Clássica dos Testes/Pacote:ltm ########################################################

# Nota Bruta Objetiva: Formação Geral
table(SISTEMAS_SEM_NA$NT_OBJ_FG)

table(BANCO_ITENS_FG$ITEM1)

# Nota Bruta Objetiva: Componente Especifico
table(SISTEMAS_SEM_NA$NT_OBJ_CE)


# Frequência de Escores
descritiva <- descript(BANCO_ITENS_FG)
descritiva <- descript(BANCO_ITENS_CE)

descritiva
names(descritiva)
lsat.desc
plot(lsat.desc,type='b',includeFirstLast=TRUE)
plot(lsat.desc,items=c(1:3),type="b",includeFirstLast=TRUE,pch=c('1','2','3'))

# Coeficiente Alpha de Cronbach
cronbach.alpha(BANCO_ITENS_FG)


### Análise-de-Fatores/Pacote: psych ############################################################
cortest.bartlett(SISTEMAS_SEM_NA, n = NULL, diag=TRUE) 
KMO(SISTEMAS_SEM_NA)
irt.fa(SISTEMAS_SEM_NA)
fa.parallel(SISTEMAS_SEM_NA)
#################################################################################################


### Mapa-de-Correlações/Pacote:d3heatmap ########################################################
d3heatmap(EMOCOES, scale = "column", colors = "Blues")
d3heatmap(cor(EMOCOES[ , 1:20], use="pair"), 
          symn= TRUE,  symm = TRUE, 
          k_row = 3, k_col = 3)
#################################################################################################

#################################################################################################
### Passo 9: Analise via Teoria da Resposta Ao Item #############################################


### Estimação dos Modelos da TRI ################################################################
mirt_1pl <- mirt(BANCO_ITENS_FG, 1, itemtype = "Rasch", se= TRUE, se.type = "BL")           # modelo Rasch
mirt_2pl <- mirt(BANCO_ITENS_FG, 1, itemtype = "2PL", se= TRUE, se.type = "BL", TOL = .001) # modelo 2pl
mirt_3pl <- mirt(BANCO_ITENS_FG, 1, itemtype = "3PL", se= TRUE, se.type = "BL", TOL = .001) # modelo 3pl
#################################################################################################


### Calcular os Parâmetros dos itens ############################################################
# Parametrização: intercept e slope)
coef(mirt_1pl, simplify = TRUE) 
coef(mirt_2pl, simplify = TRUE)  
coef(mirt_3pl, simplify = TRUE) 
#################################################################################################

#(Parametrização: a e b  da TRI)
coef(mirt_1pl, IRTpars = TRUE, simplify = TRUE) 
coef(mirt_2pl, IRTpars = TRUE, simplify = TRUE) 
coef(mirt_3pl, IRTpars = TRUE, simplify = TRUE)
#################################################################################################


### Comparar Modelos: Teste de Razão de Verossimilhança #########################################
anova(mirt_1pl, mirt_2pl)
anova(mirt_1pl, mirt_3pl)
anova(mirt_2pl, mirt_3pl)
#################################################################################################


### Estatística M² ##############################################################################
M2(mirt_1pl)
M2(mirt_2pl)
M2(mirt_3pl)
#################################################################################################


### Informações do Modelo Escolhido #############################################################
summary(mirt_1pl)
summary(mirt_2pl)
summary(mirt_3pl)
#################################################################################################


### Testes para a qualidade do ajuste ###########################################################
(ifit_mirt2pl <- itemfit(mirt_2pl)) 
(ifit_mirt3pl <- itemfit(mirt_3pl)) 
#################################################################################################


### Curvas caracaterísticas dos itens ###########################################################
#Curva Característica do Item
plot(mirt_1pl, type = "trace")
plot(mirt_2pl, type = "trace")
plot(mirt_3pl, type = "trace")
##################################################################################################

#Curva de informação do Item
plot(mirt_1pl, type = "infotrace")
plot(mirt_2pl, type = "infotrace")
plot(mirt_3pl, type = "infotrace")
###################################################################################################


#Plot de Curvas Individuais
#par(mfrow=c(2,2))
itemplot(mirt_3pl, type = "infotrace", item = 1)
itemplot(mirt_3pl, type = "infotrace", item = 2)
####################################################################################################



####################################################################################################




















### Referencias Bibliograficas ####################################################################
# Software's

- R DEVELOPMENT CORE TEAM. R4.0: A language and Enviornment for Statistical Computing, 2020. https://cran.r-project.org/bin/windows/base 
- RSTUDIO. Rstudio: Integrated Development Environment for R (versÃ£o 1.2.5).
https://rstudio.com/products/rstudio/download/#download
  
  
  # Packages ########################################################################################

- Allaire, JJ, Rich Iannone, Alison Presmanes Hill, and Yihui Xie. 2020. **Distill: R Markdown Format for Scientific and Technical Writing**. https://CRAN.R-project.org/package=distill.

- Zhu, Hao. 2020. **kableExtra: Construct Complex Table with Kable and Pipe Syntax**. https://CRAN.R-project.org/package=kableExtra.

- Hadley Wickham; Romain FranÃ§ois;Lionel Henry; Kirill MÃ¼ller. **dplyr: A Grammar of Data Manipulation**
  https://cloud.r-project.org/web/packages/dplyr/index.html.

- Alboukadel Kassambara. **rstatix: Pipe-Friendly Framework for Basic Statistical Tests**
  https://cran.r-project.org/web/packages/rstatix/index.html.

- Joe Cheng; Carson Sievert; Winston Chang; Yihui Xie; Jeff Allen. **htmltools: Tools for HTML**
  https://cran.r-project.org/web/packages/htmltools/index.html.

- Phil Chalmers; Joshua Pritikin; Alexander Robitzsch; Mateusz Zoltak; KwonHyun Kim; Carl F. Falk; Adam Meade; Lennart Schneider; David King; Chen-Wei Liu; Ogreden Oguzhan. **mirt: Multidimensional Item Response Theory** https://cran.r-project.org/web/packages/mirt/index.html.

- Dimitris Rizopoulos.**ltm: Latent Trait Models under IRT**
  https://cran.r-project.org/web/packages/ltm/index.html.

- William Revelle. **psych: Procedures for Psychological, Psychometric, and Personality Research**
  https://cran.r-project.org/web/packages/psych/index.html.

- Ivailo Partchev; Gunter Maris; Tamaki Hattori. **irtoys: A Collection of Functions Related to Item Response Theory (IRT)** https://cran.r-project.org/web/packages/irtoys/index.html.

# Books ###############################################################################################

- ANDRADE, D. F., TAVARES, H. R., VALLE, R. C. **Teoria da resposta ao item: conceitos
e aplicaÃ§Ãµes**. SÃ£o Paulo, 2000. https://docs.ufpr.br/~aanjos/CE095/LivroTRI_DALTON.pdf

- BAKER, F. B. **The Basics of Item Response Theory**. 2 ed. USA, 2001. https://eric.ed.gov/?id=ED458219

- Yihui Xie, J. J. Allaire, Garrett Grolemund. **R Markdown: The Definitive Guide**. https://bookdown.org/yihui/rmarkdown/.

- Yihui Xie. **Dynamic Documents with R and knitr**,(Chapman & Hall/CRC The R Series) 2nd Edition.
https://yihui.org/knitr/.

- Field, A. P., Miles, J., & Field, Z. (2012). Discovering statistics using R. https://aedmoodle.ufpa.br/pluginfile.php/401852/mod_resource/content/5/Material_PDF/1.Discovering%20Statistics%20Using%20R.pdf
#######################################################################################################


### Considerações Finais ##############################################################################

- Este estudo teve como objetivo mensurar o desempenho(proficiência) no ENDE/2017, dos estudantes de Computação matriculados na UFPA, por meio TRI. Apartir da estimaÃ§Ã£o realizada com o modelo logistico de 2 e 3 parÃ¢metros nos 35 itens da prova objetiva, verificou-se que, a prova não apresentou nem o dominio cognitivo compreendido pela escala. Este Resultado corrobora o baixo desempenho dos estudantes, apontado aspectos de fragilidades de aprendizagem. 
#########################################################################################################