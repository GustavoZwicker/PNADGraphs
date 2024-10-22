rm(list=ls(all=T)) # limpa os objetos do ambiente

#### Seleciona as bibliotecas utilizadas no c�digo ####
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


###### In�cio do script para o tratamento dos dados da pnad ######


# Exporta os dados da pnad
pnad2020<- get_pnadc(
  year = 2020,
  quarter = 4,
  vars=c('V2009','VD3004','VD4001','VD4002','VD4020'), # Vari�veis utilizadas
)


# Gera uma tabela com a m�dia do sal�rio por n�vel de instru��o
tabNivelSalario <- svyby(~VD4020, ~VD3004, subset(pnad2020,VD4001=='Pessoas na for�a de trabalho'), svyquantile, quantiles=0.5 ,ci=TRUE, keep.var=FALSE, na.rm = T)


# Usa a tabela gerada na linha anterior para formar um gr�fico, com as legendas informadas dentro do labs()
g1<-ggplot(data = tabNivelSalario, aes(statistic.quantiles, VD3004)) + 
  labs(title='M�dia salarial por N�vel de instru��o',x='Sal�rio',y='N�vel de Instru��o')+
  geom_bar(stat = "identity")



## Gera uma tabela com a propor��o de desocupados por n�vel de instru��o
tabN�velDesoc <- svyby(~VD4002, ~VD3004, subset(pnad2020,VD4001=='Pessoas na for�a de trabalho'), svymean, na.rm = T)


## Troca os espa�os por "." no nome das colunas
names(tabN�velDesoc) <- sub("VD4002Pessoas desocupadas", "desocupadas", names(tabN�velDesoc))


## Usa a tabN�velDesoc para formar um gr�fico, com as legendas informadas dentro do labs()
g2<-ggplot(data = tabN�velDesoc, aes(x=desocupadas, y=VD3004))+ 
  labs(title='Taxa de Desocupa��o por N�vel de instru��o',x='Taxa de Desocupa��o',y='N�vel de Instru��o')+
  geom_bar(stat = "identity")


### Gera uma tabela com a m�dia de idade por n�vel de instru��o
tabNivelIdade <- svyby(~V2009, ~VD3004, subset(pnad2020,VD4001=='Pessoas na for�a de trabalho'), svymean, na.rm=T)


### Utiliza a tabNivelIdade para gerar um gr�fico, com as legendas informadas dentro do labs()
g3<-ggplot(data = tabNivelIdade, aes(x=V2009, y=VD3004))+ 
  labs(title='Idade m�dia por N�vel de instru��o',x='Idade m�dia',y='N�vel de Instru��o')+
  geom_bar(stat = "identity")


#### Arranja todos os gr�ficos em apenas um Plot
grid.arrange(g1,g2,g3,ncol=2,nrow=2)

