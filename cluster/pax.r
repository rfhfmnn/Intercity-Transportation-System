wd<-"C:/Users/rafah/Desktop/rafah/ERSeP/ERSeP/2 - PAX"
setwd(wd)

#LIBRERIAS
library(cluster)
library(readr)
library(shiny)
library(plotly)
library(DT)
library(readxl)
library(dplyr)
library(ggrepel)
library(caret)
library(forecast)

#datos
data_pax <- read_delim("pax.csv",delim=";")
sum(is.na(data_pax))
data_pax<-data_pax %>% 
  na.omit(data_pax)
table(data_pax$empresa)

#RENTABILIDAD
##linea por empresa
rentabilidad<-data_pax %>%
  group_by(empresa,mes) %>%
  summarise(
    total_pax=sum(cantidad,na.rm=TRUE),
    total_y=sum(importe,na.rm=TRUE),
    ypax=mean(importe/cantidad,na.rm=TRUE)
  )

ggplot(rentabilidad, aes(x = mes, y = total_y, color = empresa, group = empresa)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Rentabilidad por Empresa y Mes",
    x = "Mes",
    y = "Ingresos Totales",
    color = "Empresa"
  )

##scatter
rentabilidad<-data_pax %>%
  group_by(empresa) %>%
  summarise(
    total_pax=sum(cantidad,na.rm=TRUE),
    total_y=sum(importe,na.rm=TRUE),
    ypax=mean(importe/cantidad,na.rm=TRUE)
  )

ggplot(rentabilidad, aes(x = total_pax, y = total_y, label = empresa)) +
  geom_point() +
  labs(
    title = "Rentabilidad por Empresa",
    x = "Cantidad Total de Pasajeros",
    y = "Ingresos Totales"
  )


#CLUSTERIZACIÓN PAXvsINGRESOS

rutas <- data_pax %>%
  #filter(data_pax$empresa=="ERSA URBANO SA") %>% 
  group_by(origentransporte, destinotransporte) %>%
  summarise(
    total_pax = sum(cantidad, na.rm = TRUE),
    total_y = sum(importe, na.rm = TRUE)
  )

matriz <- scale(cbind(rutas$total_pax, rutas$total_y))

set.seed(123)
kmeans_result <- kmeans(matriz, centers = 3)

rutas$Cluster <- kmeans_result$cluster

ggplot(rutas, aes(x = total_pax, y = total_y, color = as.factor(Cluster))) +
  geom_point() +
  geom_text_repel(
    data = rutas %>% filter(Cluster == 1),
    aes(label = paste(origentransporte, "→", destinotransporte)),
    size = 3,
    color = "black"
  ) +
  labs(
    title = "Clusterización de Rutas",
    x = "Cantidad de Pasajeros",
    y = "Ingresos",
    color = "Cluster"
  ) +
  theme_minimal()


########## INCLUYENDO MES
rutas <- data_pax %>%
  group_by(origentransporte, destinotransporte,mes) %>%
  summarise(
    total_pax = sum(cantidad, na.rm = TRUE),
    total_y = sum(importe, na.rm = TRUE)
  )
rutas<-rutas %>% 
  na.omit(rutas$origentransporte) %>% 
  na.omit(rutas$destinotransporte) %>%
  na.omit(rutas$total_pax) %>%
  na.omit(rutas$total_y) %>%
  na.omit(rutas$mes)

matriz_multivariable <- rutas %>%
  mutate(mes = as.numeric(as.factor(mes))) %>% 
  select(total_pax, total_y, mes)
matriz_multivariable_escalada <- scale(cbind(matriz_multivariable$total_pax,
                                             matriz_multivariable$total_y,
                                             matriz_multivariable$mes))

kmeans_result_multivariable <- kmeans(matriz_multivariable_escalada, centers = 4)# Añadir clusters
  rutas$Cluster <- kmeans_result_multivariable$cluster

ggplot(rutas, aes(x = mes, y = total_pax, color = factor(Cluster))) +
    geom_line() +
    facet_wrap(~ Cluster)

ggplot(rutas, aes(x = total_y, y = total_pax, color = factor(Cluster))) +
  geom_point()

ggplot(rutas, aes(x = factor(mes), fill = factor(Cluster))) +
  geom_bar(position = "fill")

plot_ly(rutas, x = ~total_pax, y = ~total_y, z = ~mes, color = ~factor(Cluster), type = 'scatter3d')

#CLUSTERIZACION MESvsINGRESOS (MEJOR HACERLO EN TERMINOS REALES)
rutas <- data_pax %>%
  group_by(origentransporte, destinotransporte,mes) %>%
  summarise(
    total_pax = sum(cantidad, na.rm = TRUE),
    total_y = sum(importe, na.rm = TRUE)
  )
rutas<-rutas %>% 
  na.omit(rutas$origentransporte) %>% 
  na.omit(rutas$destinotransporte) %>%
  na.omit(rutas$total_pax) %>%
  na.omit(rutas$total_y) %>%
  na.omit(rutas$mes)

matriz_mes_ingresos <- rutas %>%
  group_by(mes) %>%
  summarise(total_y = sum(total_y, na.rm = TRUE)) %>%
  mutate(mes = as.numeric(as.factor(mes))) %>%
  select(total_y,mes)
matriz_mes_ingresos_escalada<-scale(cbind(matriz_mes_ingresos$total_y,
                                          matriz_mes_ingresos$mes))


kmeans_mes_ingresos <- kmeans(matriz_mes_ingresos_escalada, centers = 3)
matriz_mes_ingresos$Cluster <- kmeans_mes_ingresos$cluster

ggplot(matriz_mes_ingresos, aes(x = mes, y = total_y, color = factor(Cluster))) +
  geom_point(size = 4) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Clusterización de Ingresos Totales por Mes",
    x = "Mes",
    y = "Ingresos Totales",
    color = "Cluster"
  ) +
  theme_minimal()


#DEMANDA
demanda_mensual <- data_pax %>%
  group_by(mes) %>%
  summarise(
    total_pax = sum(cantidad, na.rm = TRUE),
    total_y = sum(importe, na.rm = TRUE)
  )


# Crear serie temporal
ts_demanda <- ts(demanda_mensual$total_pax, start = c(2024, 1), frequency = 12)

# Modelo ARIMA
modelo_arima <- auto.arima(ts_demanda)

# Pronóstico
forecast_demanda <- forecast(modelo_arima, h = 2)

# Visualización
plot(forecast_demanda, main = "Pronóstico de Demanda")
forecast_demanda

