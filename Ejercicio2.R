# situación de workspace en directorio
library(rstudioapi) 
# lectura de datos
library(data.table)
library(ggfortify)
library(tidyverse)
library(TSA)
library(forecast)
library(tseries)
library(zoo)
library(xts)

setwd(dirname(getActiveDocumentContext()$path))
#Se cargan los CSVs en data frames
daily_df <- fread("data/SN_d_tot_V2.0.csv",header=TRUE)
yearly_df <- fread("data/SN_y_tot_V2.0.csv",header=TRUE)

#############################################################################
#a) Representar ambas series.
#Representacion de las mediciones diarias
ggplot(data = daily_df,
       mapping = aes(x = decimal_year, y = SNvalue))+
  ggtitle("Ciclo de actividad del Sol: Promedio Diario de manchas solares")+
  geom_point(aes(color=SNvalue))

ggsave("daily_df.png")

#Representacion de las mediciones anuales

ggplot(data = yearly_df,
       mapping = aes(x = decimal_year, y = SNvalue))+
  ggtitle("Ciclo de actividad del Sol: Promedio Anual de manchas solares")+
  geom_point(aes(color=SNvalue))+
  geom_line(aes(color=SNvalue))

ggsave("yearly_fig.png")


#############################################################################
#b) Para la serie diaria, convertirla en una serie mensual.

#Se agrupan los datos diarios por mes y se les calculan los valores medios y 
#el sumatorio
monthly_df <- daily_df %>% group_by(year, month) %>% 
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
#Primero se crea una serie temporal con el dataframe mensual
monthly_ts<-ts(monthly_df$SNvalue,start=c(1818,1), end=c(2023,3), frequency=12)

#Se crea un periodograma con el que se extraen los vectores de frecuencias y
#de potencias
peri <- periodogram(monthly_ts)
freqs<-peri$freq
potencia<-peri$spec

#Se crea un nuevo data frame con los vectores freqs, potencia y
# se genera uno que es el periodo
DENSIDAD_ESPECTRAL<-data.frame(FREQS=freqs,
                               PERIODO=(1/(freqs)),
                               PESO=potencia)

#Ordenamos los valores del data frame en base al peso
FILTRO<-DENSIDAD_ESPECTRAL %>% 
  arrange(desc(PESO)) 

#Recuperamos los primeros 5 valores
FILTRO %>% head(5) 

#EL PERIODO ES DE 131.57-> 132 meses -> 11 años
#############################################################################
# d) Tómese como conjunto de entrenamiento la serie mensual en el periodo previo a 2020, 
#y como conjunto de test el periodo posterior hasta el final de dicha serie.

#Creamos los dos data frames filtrando por año
train <- monthly_df %>% filter(year<2020)
test <- monthly_df %>% filter(year>=2020)

#############################################################################
#e) Realícese el estudio completo de la serie mensual de entrenamiento, comprobando, 
#en caso de existir, su tendencia, estacionalidad, ruido.

#Haciendo un plot rapido con los datos del dataframe
plot(train$year, train$SNvalue)
#A primera vista no hay tendencia, aunque si estacionalidad

#Se crea una serire temporal para los datos de entrenamiento.
#Debido a que se trabaja con un periodo de 132 meses, se define la serie temporal de la siguiente forma:
train_ts<-ts(train$SNvalue,start = c(1,1),frequency=132)
#Esto nos hace perder los años y meses, pero ante la particularidad de la frecuencia se deben
# definir unos tiempos propios.

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

#Primero eliminamos la tendencia con una diferenciación
diff_train_ts<- diff(train_ts)

#A continuación calculamos el periodo de la estacionalidad, el cual será el lag de la próxima diferencia.
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

1/freqs[which.max(potencia)]
#El valor del periodo es 135

#Quitamos la estacionalidad
D_dif_train_ts<-diff(diff_train_ts,lag=135)

#Vamos a comprobar con tests que sea estacionaria
nsdiffs(D_dif_train_ts)
ndiffs(D_dif_train_ts)
#ndifss y nsdiffs ya indican que no es preciso hacer más diferencias

#KPSS Test
kpss.test(D_dif_train_ts)
#0.1>0.05, por lo que se acepta la H0: La serie es estacionaria

#ADF Test
adf.test(D_dif_train_ts)
#0.01<0.05, por lo que rechazamos la H0: La serie NO es estacionaia

#Con ambos test queda demostrado que la serie es estacionaria


#############################################################################
#g) Mediante los diagramas ACF y PACF calcular los parámetros (p, d, q) y (P, D, Q) 
#para la serie mensual de entrenamiento. Comparar con los resultados obtenidos por un método 
#autoarima (paquete forecast en R o pmdarima en Python).

Acf(D_dif_train_ts)
#La q se corresponde al lag 2, mientras que la Q al 132 del periodo, por lo que Q=1.

Pacf(D_dif_train_ts)
#La p es dificil de ver, pero parece que 8 es su valor máximo.
#Por parte de la P, ésta se corresponde al periodo, 132, por lo que P=1


#Con los valores sacados de ACF y PACF, otrosí de teniendo en cuenta las diferencias de tendencia
# y estacionaliidad, se llama a auto.arima de la siguiente forma:

mod_arima<-auto.arima(train_ts,
                       max.q = 2,
                       max.Q = 1,
                       max.p = 8,
                       max.P = 1,
                       d=1,
                       D=1,
                       trace = T,
                       ic=c("aicc"),
                       stepwise = T)

Box.test(mod_arima$residuals,type = "Ljung-Box")
#El test acepta la Ho: los residuos son independientes
checkresiduals(mod_arima)
#Se ve que hay alta correlacion con los residuos del periodo 132, pero no se repite
#Por otro lado, los residuos tienen de media cero una distribucución que se ajusta a
#la normal.

#############################################################################
#h) Predecir con el modelo arima obtenido los resultados del conjunto de test y 
#medir el error mediante MAE y MAPE.

#Se llama al forecast con el mod_arima, e indicando el periodo de meses 
# a predecir
pred_arima <- forecast(mod_arima,h=(3*12)+3)

plot(pred_arima)

#Se calcula el vector de las diferencias entre predicciones y
# mediciones relaes
REAL=test$SNvalue
ARIMA=as.numeric(pred_arima$mean)
EARIMA=ARIMA-REAL

#Se obtienen el MAE y el MAPE
MAE_ARIMA=sum(abs(EARIMA))/length(test$SNvalue)
MAE_ARIMA
MAPE_ARIMA=sum(abs(EARIMA)/test$SNvalue)/length(test$SNvalue)*100
MAPE_ARIMA

#Se tiene un error medio no muy alto, pero el MAPE es casi del 100%,
# por lo que casi la totalidad de las predicciones presentan error.

#############################################################################
#i) Comparar convenientemente los resultados obtenidos por esta predicción con los datos anuales.

#Antes que nada hay que obtener una serie temporal a partir del data frame de test
yearly_df$decimal_year <- trunc(yearly_df$decimal_year)
yearly_ts<-ts(yearly_df$SNvalue,start=1700,end=2022,frequency=1)
#Para una comparacion mas clara nos vamos a quedar con los valores desde 2015
yearly_ts<- window(yearly_ts,start=2015,end=2022)


#Es preciso que los datos de la prediccion tengan una dimension temporal adecuada, así que 
#generamos un df a partir de la ts
pred_arima_df <- data.frame(SNvalue=as.matrix(pred_arima$mean),
                            date=as.integer(as.numeric(time(pred_arima$mean))))

#A continuación, con un bucle, asignamos los valores temporales correspondientes.
int_year=2020

for(i in 1:nrow(pred_arima_df)){
  pred_arima_df$date[i]=int_year
  if (i%%12==0){
    int_year = int_year+1
  }
}

#Ahora agrupamos y calculamos la media para cada anho
pred_arima_df <- pred_arima_df %>% group_by(date) %>%
  summarize(SNvalue=mean(SNvalue), .groups = 'drop')

#Generamos una serie temporal con los datos adaptados
arima_ts <- ts(pred_arima_df$SNvalue,start=2020,end=2023,frequency=1)

#Representación gráfica
ts.plot(yearly_ts,lty = c(1,3),col="red",
        main="Prediccion a 3 años y 3 meses de la actividad de Manchas Solares")
lines(arima_ts,col='blue')
#Como se podía intuir gracias al MAE y el MAPE, las preciddiones presentan error, 
#pero no es tan alto, por lo que se encuentra cercana a los valores relales.


#############################################################################
#j) Estimar el valor esperado para el número promedio de manchas en el próximo máximo de actividad solar.

#Se siguen los mismos pasos que en el caso anterior, aunque ahora se predice para los proximos
# 11 años
pred_arima <- forecast(mod_arima,h=11*12)

plot(pred_arima)

#Data frame de la prediccion
pred_arima_df <- data.frame(SNvalue=as.matrix(pred_arima$mean),
                            date=as.integer(as.numeric(time(pred_arima$mean))))

#Se establecen los anhos
int_year=2020

for(i in 1:nrow(pred_arima_df)){
  pred_arima_df$date[i]=int_year
  if (i%%12==0){
    int_year = int_year+1
  }
}

#Se calculan las medias para cada anho
pred_arima_df <- pred_arima_df %>% group_by(date) %>%
  summarize(SNvalue=mean(SNvalue), .groups = 'drop')

#Se genera la serie temporal adaptada
arima_ts <- ts(pred_arima_df$SNvalue,start=2020,end=2030,frequency=1)

#Representacion comparativa
ts.plot(yearly_ts, arima_ts,lty = c(1,3),col="red",
        main="Prediccion a 11 años de la actividad de Manchas Solares")
lines(arima_ts,col='blue')

#Sentencia que retorna la fila con mayor SN de la prediccion
pred_arima_df[which.max(pred_arima_df$SNvalue),]

