install.packages("quantmod")  #Instalem un paquet. Aquest paquest és per importar dades directament, per exemple de yahoo

library(quantmod)   #Carreguem el paquet a la sessió

getSymbols("AAPL",from="2012-01-01", to="2018-09-18") 

############ descarrega del SP500
SPX <- getSymbols("^GSPC",auto.assign = FALSE, from = "2017-11-01")

library(tidyverse)
SPX<-as.tibble(SPX)

SP500<-select(SPX, GSPC.Adjusted)

write.table(zoo(SP500),"SP500.csv") 
x<-c(1:262)
ClosingPrices<-c(SP500$GSPC.Adjusted)

XY<-data.frame(y)
XY[0,1]<-"Closing Prices"
write.table(zoo(XY),"SP500.csv")
############ final de la descarrega

write.table(zoo(AAPL),"AAPL_20120101_20180918.txt")  #Guardar les dades al directori de treball en format txt


#####################################################################
####  Si ho preferiu podeu treballar les dades amb Excel
#####################################################################


#####################################################################
####  Exloreu les sèries
#####################################################################

#Podeu representar algunes sèries:
plot(AAPL$AAPL.Adjusted)
plot(AAPL$AAPL.Close)
plot(dailyReturn(AAPL))
plot(volatility(AAPL))

#repetim els gràfics en format millorat
chartSeries(AAPL$AAPL.Adjusted,type = c("auto"),subset = NULL,theme="white",TA=NULL)
chartSeries(volatility(AAPL),type = c("auto"),subset = NULL,theme="white",TA=NULL)
chartSeries(dailyReturn(AAPL),type = c("auto"),subset = NULL,theme="white",TA=NULL)


#####################################################################
####  Si treballem amb excel, els logreturns i les volatilitats poden variar lleugerament
####  ja que usem fórmules diferents. 
####  A l'ajut d'R de la funció específica trobareu els detalls de la formulació usada.
####  Si seguim la notació i formulació de classe, podem simplificar les dades de la manera següent:
#####################################################################

t<-as.Date(rownames(zoo(AAPL)))
Pt<-as.numeric(AAPL$AAPL.Close)#Adjusted)
rt<-diff(log(Pt))
datos<-data.frame(t=t[-1],Pt=Pt[-1],rt=rt)


