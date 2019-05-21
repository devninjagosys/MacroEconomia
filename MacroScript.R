# Installation Setup
install.packages("systemfit")
install.packages("gdata")
install.packages("dplyr")
install.packages("ggplot2", dependencies = TRUE)

#library Setup
library(systemfit)
library(gdata)
library(caTools)
library(dplyr)
library(ggplot2)

#Directory Setup
setwd("C:/Users/vinir/Desktop/Economia/Trabalho/Dados/Done")

#Data IPCA
IPCA <-read.csv("IPCA.csv",header=TRUE,sep=";")
str(IPCA) # Strings do arquivo (os valores precisam ser "num" - numéricos)
ts(IPCA) # Transformação em Séries de Tempo
dim(IPCA) # Dimensão do Arquivo - Conferir com o excel
list(IPCA) # Variáveis
typeof(IPCA[1,2])

#Data Desemprego
Desemprego <-read.csv("TaxaDesempregoFinal.csv",header=TRUE,sep=";")
str(Desemprego) # Strings do arquivo (os valores precisam ser "num" - numéricos)
ts(Desemprego) # Transformação em Séries de Tempo
dim(Desemprego) # Dimensão do Arquivo - Conferir com o excel
list(Desemprego) # Variáveis
typeof(Desemprego[1,2])

#Lag Diff on IPCA
Diff_IPCA <- diff(IPCA[,2], lag=1)
print(Diff_IPCA)

# Transforming Series into List
ValuesRateUnemploy = list(Desemprego[,2])
List_Diff_IPCA = list(Diff_IPCA)
typeof(ValuesRateUnemploy)
typeof(List_Diff_IPCA)

#Lists into Data Frame
Data <- do.call(rbind, Map(data.frame, A=ValuesRateUnemploy, B=List_Diff_IPCA))

#Plotting Data to Graph
Graph <- ggplot(Data,aes(x =A , y = B)) + geom_point(color="darkred",shape = 17,size=3)
Graph + geom_smooth(method = "lm", fill = NA,colour = "black")
Graph + geom_smooth(se=TRUE,method = "loess",span=0.2,formula=y ~ x,color="grey") 
Graph <- Graph + geom_line(colour = "blue")
Graph <- Graph + labs(x = "Taxa de Desemprego") +labs(y = "Variação do IPCA")
#Smooth Curve : geom_smooth(se = FALSE, method = "loess",span=0.25)
Graph<-Graph+theme(text = element_text(size=20),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20))
#Correlation Data
Correlation = cor(Data$A, Data$B)
cor(Data$B, Data$A)
slope <- cor(Data$A, Data$B) * (sd(Data$A) / sd(Data$B))
print(slope)

#Model Least Squares
lmodel <- lm(B ~ A, data = Data)
DataCoefficients=lmodel$coefficients
LMIntercept=lmodel$coefficients[1]
print(LMIntercept)
LMSlope=lmodel$coefficients[2]
print(LMSlope)
NaturalRate = -lmodel$coefficients[1]/lmodel$coefficients[2]
print(NaturalRate*100)
summary(lmodel)
#Adding Nairu
Graph + geom_vline(xintercept=NaturalRate,color="red")+geom_smooth(se=TRUE,method = "loess",span=0.2,formula=y ~ x,color="grey")

#Plots into Data Frame - Desemprego
DataUnemploy <- do.call(rbind, Map(data.frame, ANO=Desemprego[,1],DES=Desemprego[,2]*100))
Graph2<-ggplot(DataUnemploy,aes(x =ANO , y = DES))
Graph2<-Graph2+geom_point(color="darkred",shape = 17,size=3)+geom_line(colour = "blue")+labs(y = "Taxa de Desemprego(%)")+theme(text = element_text(size=20),axis.text.x = element_text(size=20))
#Plots into Data Frame - IPCA
IPCADATA <- do.call(rbind, Map(data.frame, ANOIPCA=IPCA[,1],VALIPCA=IPCA[,2]*100))
Graph3<-ggplot(IPCADATA,aes(x =ANOIPCA , y = VALIPCA))
Graph3<-Graph3+geom_point(color="darkred",shape = 17,size=3)+geom_line(colour = "blue")+labs(y = "IPCA(%)")+theme(text = element_text(size=20),axis.text.x = element_text(size=20))


#Final Graph
DataGeneral <- do.call(rbind, Map(data.frame, Anos=Desemprego[,1],Rate=Desemprego[,2]*100,VALIPCA=IPCA[-1,2]*100))
Graph4<-ggplot(DataGeneral)+geom_point(aes(x=Anos,y = Rate),color="blue",shape=19,size=3)+geom_point(aes(x=Anos,y = VALIPCA),color="Red",shape=19,size=3)
Graph4<-Graph4+geom_line(aes(x=Anos,y = Rate),color="blue")+geom_line(aes(x=Anos,y = VALIPCA),color="red")
Graph4<-Graph4+geom_hline(yintercept=NaturalRate*100,color="black")
Graph4<-Graph4+theme(text = element_text(size=20),axis.text.x = element_text(size=20),axis.text.y = element_text(size=20))
Graph4
