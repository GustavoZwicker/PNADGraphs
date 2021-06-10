rm(list=ls(all=T)) # limpa os objetos do ambiente

#### Seleciona as bibliotecas utilizadas no código ####
if(!require(PNADcIBGE))
  install.packages("PNADcIBGE")
library(PNADcIBGE)

if(!require(survey))
  install.packages("survey")
library(survey)

if(!require(srvyr))
  install.packages("srvyr")
library(srvyr)

if(!require(ggplot2))
  install.packages("ggplot2")
library(ggplot2)

if(!require(gridExtra))
  install.packages("gridExtra")
library(gridExtra)


###### Início do script para o tratamento dos dados da pnad ######


# Exporta os dados da pnad
pnad2020<- get_pnadc(
  year = 2020,
  quarter = 4,
  vars=c('V2009','VD3004','VD4001','VD4002','VD4020'), # Variáveis utilizadas
)


# Gera uma tabela com a média do salário por nível de instrução
tabNivelSalario <- svyby(~VD4020, ~VD3004, subset(pnad2020,VD4001=='Pessoas na força de trabalho'), svyquantile, quantiles=0.5 ,ci=TRUE, keep.var=FALSE, na.rm = T)


# Usa a tabela gerada na linha anterior para formar um gráfico, com as legendas informadas dentro do labs()
g1<-ggplot(data = tabNivelSalario, aes(statistic.quantiles, VD3004)) + 
  labs(title='Média salarial por Nível de instrução',x='Salário',y='Nível de Instrução')+
  geom_bar(stat = "identity")



## Gera uma tabela com a proporção de desocupados por nível de instrução
tabNívelDesoc <- svyby(~VD4002, ~VD3004, subset(pnad2020,VD4001=='Pessoas na força de trabalho'), svymean, na.rm = T)


## Troca os espaços por "." no nome das colunas
names(tabNívelDesoc) <- sub("VD4002Pessoas desocupadas", "desocupadas", names(tabNívelDesoc))


## Usa a tabNívelDesoc para formar um gráfico, com as legendas informadas dentro do labs()
g2<-ggplot(data = tabNívelDesoc, aes(x=desocupadas, y=VD3004))+ 
  labs(title='Taxa de Desocupação por Nível de instrução',x='Taxa de Desocupação',y='Nível de Instrução')+
  geom_bar(stat = "identity")


### Gera uma tabela com a média de idade por nível de instrução
tabNivelIdade <- svyby(~V2009, ~VD3004, subset(pnad2020,VD4001=='Pessoas na força de trabalho'), svymean, na.rm=T)


### Utiliza a tabNivelIdade para gerar um gráfico, com as legendas informadas dentro do labs()
g3<-ggplot(data = tabNivelIdade, aes(x=V2009, y=VD3004))+ 
  labs(title='Idade média por Nível de instrução',x='Idade média',y='Nível de Instrução')+
  geom_bar(stat = "identity")


#### Arranja todos os gráficos em apenas um Plot
grid.arrange(g1,g2,g3,ncol=2,nrow=2)

