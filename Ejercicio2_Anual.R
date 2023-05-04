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
monthly_ts<-ts(monthly_df$SNvalue,start=c(1818,1), end=c(2023,3), frequency=12)

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
# d) Tómese como conjunto de entrenamiento la serie anual en el periodo previo a 2020, 
#y como conjunto de test el periodo posterior hasta el final de dicha serie.
yearly_df$decimal_year <- trunc(yearly_df$decimal_year)
train <- yearly_df %>% filter(decimal_year<2020)
test <- yearly_df %>% filter(decimal_year>=2020)


#############################################################################
#e) Realícese el estudio completo de la serie anual de entrenamiento, comprobando, 
#en caso de existir, su tendencia, estacionalidad, ruido.
#Haciendo un plot rapido con los datos del dataframe
plot(train$decimal_year, train$SNvalue)
#A primera vista no hay tendencia no hay, eso es algo
#Buscamos estacionalidad y ruido
#Debido a que se trabaja con un periodo de 132 meses, se define la serie temporal de la siguiente forma:
train_ts<-ts(train$SNvalue,start = c(1,1),frequency=11)
#Esto nos hace perder los años y meses, pero ante la particularidad de la frecuencia poco se puede hacer excepto
#definir unos tiempos propios.
plot(train_ts)
autoplot(autoplot(stl(train_ts,s.window = "periodic")))
#Una tendencua que sigue aproximadamente el contorno de la serie
#una estacionalidad muy clara
#Tambien hay ruido que parece seguir un patron


#############################################################################

#f) Determinar si la serie temporal mensual de entrenamiento es estacionaria.
#A primera vista parece ser que sólo la estacionalidad es lo que no la hace estacionaria, 
#dado que la evolución de la media y de la varianza no cambian tanto a lo largo del tiempo. 
#No parece tener tendencia.

#Para asegurarnos usamos las funciones ndiffs y nsdiffs para averiguar el número necesario de diferencias
#con las que eliminar la tendencia y estacionalidad, de modo que se alcance la estacionaridad.
ndiffs(train_ts)
nsdiffs(train_ts)
#En ambos casos se tiene 1, por lo que es preciso diferenciar primero para la tendencia y luego la estacionalidad,
#(Esto significa que al final sí que tenía tendencia)
#Lo primero es quitar la estacionalidad de la serie, para ello diferenciamos sobre la estacionalidad
#Podemos determinar el numero de diferencias necesarias para que sea estacionarioa:

#A continuación calculamos el periodo de la estacionalidad, el cual será el lag de la próxima diferencia.
train_seasonal <- decompose(train_ts)$seasonal
plot(train_seasonal)
peri <- periodogram(train_seasonal)

freqs<-peri$freq

potencia<-peri$spec
DENSIDAD_ESPECTRAL<-data.frame(FREQS=freqs,
                               PERIODO=(1/(freqs)),
                               PESO=potencia)

FILTRO<-DENSIDAD_ESPECTRAL %>% 
  arrange(desc(PESO)) 
FILTRO %>% head(5) 

1/freqs[which.max(potencia)]
#El valor es 135

#Quitamos la estacionalidad
D_dif_train_ts<-diff(train_ts,lag=11)
#ndifss y nsdiffs ya indican que no es preciso hacer más diferencias
nsdiffs(D_dif_train_ts)
ndiffs(D_dif_train_ts)

autoplot(D_dif_train_ts)+
  geom_line(color="red")+
  ylab("DIFF(train_ts,132)")+
  ggtitle("Serie transformada: DIFF(train_ts,132)")+
  geom_smooth()

ggtsdisplay(D_dif_train_ts)
#Corremos los tests
#1º KPSS Test
kpss.test(D_dif_train_ts)
#0.1>0.05, por lo que se acepta la H0: La serie es estacionaria

#2º ADF Test
adf.test(D_dif_train_ts)
#0.01<0.05, por lo que rechazamos la H0: La serie NO es estacionaia

#Con ambos test queda demostrado que la serie es estacionaria


#############################################################################
#g) Mediante los diagramas ACF y PACF calcular los parámetros (p, d, q) y (P, D, Q) 
#para la serie mensual de entrenamiento. Comparar con los resultados obtenidos por un método 
#autoarima (paquete forecast en R o pmdarima en Python).

Acf(D_dif_train_ts)
#Se trata de un AR, por lo que no hay q ni Q

Pacf(D_dif_train_ts)
#La p es 2
#Por parte de la P, ésta se corresponde al periodo, 11, por lo que P=1


#Con los valores sacados de ACF y PACF, otrosí de teniendo en cuenta las diferencias de tendencia y estacionaridad,
#se llama a auto.arima de la siguiente forma:
mod_arima<-auto.arima(train_ts,
                      max.q = 0,
                      max.Q = 0,
                      max.p = 2,
                      max.P = 1,
                      d=0,
                      D=1,
                      trace = T,
                      ic=c("aicc"),
                      stepwise = T)

checkresiduals(mod_arima)
Box.test(mod_arima$residuals,type = "Ljung-Box")

#############################################################################
#h) Predecir con el modelo arima obtenido los resultados del conjunto de test y 
#medir el error mediante MAE y MAPE.
pred_arima <- forecast(mod_arima,h=3)

plot(pred_arima)
plot(pred_arima$mean)

REAL=test$SNvalue
ARIMA=as.numeric(pred_arima$mean)
EARIMA=ARIMA-REAL

MAE_ARIMA=sum(abs(EARIMA))/length(test$SNvalue)
MAE_ARIMA
MAPE_ARIMA=sum(abs(EARIMA)/test$SNvalue)/length(test$SNvalue)*100
MAPE_ARIMA

#############################################################################
#i) Comparar convenientemente los resultados obtenidos por esta predicción con los datos anuales.
#Entiendo, entonces, que tengo que comparar con los anuales.... hay que transformar a anual
yearly_df$decimal_year <- trunc(yearly_df$decimal_year)
yearly_ts<-ts(yearly_df$SNvalue,start=2015,end=2022,frequency=1)

pred_arima_df <- data.frame(SNvalue=as.matrix(pred_arima$mean),
                            date=as.integer(as.numeric(time(pred_arima$mean))))

int_year=2020

for(i in 1:nrow(pred_arima_df)){
  pred_arima_df$date[i]=int_year
  int_year = int_year+1

}

pred_arima_df

arima_ts <- ts(pred_arima_df$SNvalue,start=2020,end=2022,frequency=1)

ts.plot(yearly_ts, arima_ts,lty = c(1,3),col="red",
        main="Prediccion a 3 años y 3 meses de la actividad de Manchas Solares")

#############################################################################
#j) Estimar el valor esperado para el número promedio de manchas en el próximo máximo de actividad solar.
pred_arima <- forecast(mod_arima,h=11)

plot(pred_arima)

pred_arima

pred_arima_df <- data.frame(SNvalue=as.matrix(pred_arima$mean),
                            date=as.integer(as.numeric(time(pred_arima$mean))))

int_year=2020

for(i in 1:nrow(pred_arima_df)){
  pred_arima_df$date[i]=int_year
  int_year = int_year+1
}

pred_arima_df

arima_ts <- ts(pred_arima_df$SNvalue,start=2020,end=2030,frequency=1)

ts.plot(yearly_ts, arima_ts,lty = c(1,3),col="red",
        main="Prediccion a 11 años de la actividad de Manchas Solares")

#Sentencia que retorna la fila con mayor SN de la prediccion
pred_arima_df[which.max(pred_arima_df$SNvalue),]

