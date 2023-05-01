# situación de workspace en directorio
library(rstudioapi) 
# lectura de datos
library(data.table)
library(dplyr)
library(ggfortify)
library(tidyverse)
library(ggplot2)
library(TSA)
library(forecast)
library(tseries)
library(zoo)

setwd(dirname(getActiveDocumentContext()$path))
diary_df <- fread("data/SN_d_tot_V2.0.csv",header=TRUE)
yearly_df <- fread("data/SN_y_tot_V2.0.csv",header=TRUE)

#############################################################################
#a) Representar ambas series.
#Representacion de la info diaria
ggplot(data = diary_df,
       mapping = aes(x = decimal_year, y = SNvalue))+
  ggtitle("Ciclo de actividad del Sol: Promedio Diario de manchas solares")+
  geom_point(aes(color=SNvalue))+
  geom_line(aes(color=SNvalue))

ggsave("diary_fig.png")

#Representacion de la info anual
ggplot(data = yearly_df,
       mapping = aes(x = decimal_year, y = SNvalue))+
  ggtitle("Ciclo de actividad del Sol: Promedio Anual de manchas solares")+
  geom_point(aes(color=SNvalue))+
  geom_line(aes(color=SNvalue))

ggsave("yearly_fig.png")


#############################################################################
#b) Para la serie diaria, convertirla en una serie mensual.
# A ver si me sale
monthly_df <- diary_df %>% group_by(year, month) %>% 
  summarize(SNvalue=mean(SNvalue),
            SNerror=mean(SNerror),
            Observations=sum(observations), 
            .groups = 'drop')

ggplot(data = monthly_df,
       mapping = aes(x = year, y = SNvalue))+
  ggtitle("Ciclo de actividad del Sol: Promedio Mensual de manchas solares")+
  geom_point(aes(color=SNvalue))+
  geom_line(aes(color=SNvalue))

ggsave("monthly_fig.png")

#############################################################################
####
#c) Calcular el periodo de actividad solar.
monthly_ts<-ts(monthly_df$SNvalue,start=1818, frequency=12)

peri <- periodogram(monthly_ts)
freqs<-peri$freq
potencia<-peri$spec

DENSIDAD_ESPECTRAL<-data.frame(FREQS=freqs,
                               PERIODO=(1/(freqs)),
                               PESO=potencia)

FILTRO<-DENSIDAD_ESPECTRAL %>% 
  arrange(desc(PESO)) 
FILTRO %>% head(5) 

1/freqs[which.max(potencia)]

#EL PERIODO ES DE 131.57-> 132 meses -> 11 años

#############################################################################
# d) Tómese como conjunto de entrenamiento la serie mensual en el periodo previo a 2020, 
#y como conjunto de test el periodo posterior hasta el final de dicha serie.
train <- monthly_df %>% filter(year<2020)
test <- monthly_df %>% filter(year>=2020)


#############################################################################
#e) Realícese el estudio completo de la serie mensual de entrenamiento, comprobando, 
#en caso de existir, su tendencia, estacionalidad, ruido.
#Haciendo un plot rapido
plot(train$year, train$SNvalue)
#Tendencia no hay, eso es algo
#Buscamos estacionalidad y ruido
train_ts<-ts(train$SNvalue, start=1818, frequency=12)
train_decom <- stl(train_ts,s.window = "periodic")
autoplot(stl(train_ts,s.window = "periodic"))
#Una tendencua que sigue exactamente a la figura, por lo que no la tocamos
#una estacionalidad altisima.
#Tambien hay ruido que parece seguir un patron

#############################################################################

#f) Determinar si la serie temporal mensual de entrenamiento es estacionaria.
#A primera vista parece ser estacionaria, dado que la evolución de la media y de la varianza no cambian tanto
#a lo largo del tiempo. Tampoco tiene tendencia.
#Pero bueno, me imagino que lo que quiere Jaime es que usemos los test
#Lo primero es quitar la estacionalidad de la serie, para ello diferenciamos sobre la estacionalidad
train_seasonal <- decompose(train_ts)$seasonal
peri <- periodogram(train_seasonal)

freqs<-peri$freq

potencia<-peri$spec
DENSIDAD_ESPECTRAL<-data.frame(FREQS=freqs,
                               PERIODO=(1/(freqs)),
                               PESO=potencia)

FILTRO<-DENSIDAD_ESPECTRAL %>% 
  arrange(desc(PESO)) 
FILTRO %>% head(5) 
#Aqui vemos que el periodo de la estacionalidad es 12

#Quitamos la estacionalidad
D_dif_train_ts<-diff(train_ts,12)
autoplot(D_dif_train_ts)+
  geom_line(color="red")+
  ylab("DIFF(diff(log($ por acción)),4)")+
  ggtitle("Serie transformada: DIFF(diff(log($ por acción)),4)")+
  geom_smooth()

ggtsdisplay(D_dif_train_ts)
#Corremos los tests
#1º KPSS Test
kpss.test(D_dif_train_ts)
#Da más de 0.05, por lo que se acepta la H0: La serie es estacionaria

#2º ADF Test
adf.test(D_dif_train_ts)
#Da menos de 0.05, por lo que rechazamos la H0: La serie NO es estacionaia

#Conclusion tras ejecutar los tests:  es estacionaria!

#############################################################################
#TODO (no estoy seguro de si he pillado en cada caso los valores correctos)
#g) Mediante los diagramas ACF y PACF calcular los parámetros (p, d, q) y (P, D, Q) 
#para la serie mensual de entrenamiento. Comparar con los resultados obtenidos por un método 
#autoarima (paquete forecast en R o pmdarima en Python).

Acf(D_dif_train_ts)
#q<=11 Q<=1
#Se puede ver que hay una correlacion fuerte entre los terminos,
#aunque va descendiendo a medida que pasan los meses

Pacf(D_dif_train_ts)
#p<=6 P<=1
#Las correlaciones fuertes estan entre los pimeros y ultimos meses de cada año

#Viendo las graficas podemos afirmar que estamos ante un Proceso Autoregresivo (AR).
#En estos casos no podemos determinar la p y la P, únicamente q y Q, las cuales
#valen 1 y 12 respectivamente
mod_arima<-auto.arima(train_ts,
                       max.q = 0,
                       max.Q = 0,
                       max.p = 6,
                       max.P = 1,
                       d=0,
                       D=1,
                       trace = T,
                       ic=c("aicc"),
                       stepwise = T)

Box.test(mod_arima$residuals,type = "Ljung-Box")
checkresiduals(mod_arima)
#############################################################################
#h) Predecir con el modelo arima obtenido los resultados del conjunto de test y 
#medir el error mediante MAE y MAPE.
pred_arima <- forecast(mod_arima,h=(3*12)+3)

plot(pred_arima)

REAL=test$SNvalue
ARIMA=as.numeric(pred_arima$mean)
EARIMA=ARIMA-REAL

MAE_ARIMA=sum(abs(EARIMA))/length(test$SNvalue)
MAE_ARIMA
MAPE_ARIMA=sum(abs(EARIMA)/test$SNvalue)/length(test$SNvalue)
MAPE_ARIMA

#############################################################################
#i) Comparar convenientemente los resultados obtenidos por esta predicción con los datos anuales.
#Entiendo, entonces, que tengo que comparar con los anuales.... hay que transformar a anual
yearly_df$decimal_year <- trunc(yearly_df$decimal_year)
yearly_ts<-ts(yearly_df$SNvalue,start=2015,end=2022,frequency=1)

pred_arima_df <- data.frame(SNvalue=as.matrix(pred_arima$mean),
                            date=as.integer(as.numeric(time(pred_arima$mean))))

pred_arima_df <- pred_arima_df %>% group_by(date) %>%
  summarize(SNvalue=mean(SNvalue), .groups = 'drop')


arima_ts <- ts(pred_arima_df$SNvalue,start=2020,end=2023,frequency=1)

ts.plot(yearly_ts, arima_ts,lty = c(1,3),col="red",
        main="Prediccion a 3 años y 3 meses de la actividad de Manchas Solares")

#############################################################################
#j) Estimar el valor esperado para el número promedio de manchas en el próximo máximo de actividad solar.
test_ts<-ts(test$SNvalue,start=c(min(test$year),1), frequency=12)

#Vamos a probar el TBATS
loa_tbats<-tbats(train_ts)
pred_tbats<-forecast(loa_tbats,h=(3*12)+3)
plot(pred_tbats$mean,main="Prediccion TBATS(0,(0,0),1,{<12,4>})")
lines(test_ts,col="red")
plot(forecast(loa_tbats,h=(3*12)+3)$mean)
lines(test_ts,col="red")
      
TBATS=as.numeric(pred_tbats$mean)
ETBATS=TBATS-test$SNvalue
MAE_BATS=sum(abs(ETBATS))/length(test$SNvalue)
MAE_BATS
MAPE_TBATS=sum(abs(ETBATS)/test$SNvalue)/length(test$SNvalue)
MAPE_TBATS

