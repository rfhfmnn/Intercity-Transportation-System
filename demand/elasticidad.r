library(tseries)
library(forecast)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(openxlsx)
library(car)
library(zoo)
library(vars)
library(orcutt)
library(dynlm)
library(tidyr)


data_raw <- read.xlsx("datos_transporte.xlsx",sheet = 'datos ln')
data <- data_raw[,c(2:5)]
data_ts <- ts(data,start = c(2021,11),end = c(2025,1),frequency = 12)
pax<-data$pax
pax_ts<-ts(pax,start = c(2021,11),end = c(2025,1),frequency = 12)
plot(data_ts,main="Series",xlab="Tiempo",col="blue",lwd=2)
par(mfrow = c(1,1))
plot(pax_ts, main = "Evolución de la Cantidad de Pasajeros", 
     xlab = "Tiempo", ylab = "Cantidad de Pasajeros", col = "blue",lwd=2)


adf.test(diff(pax_ts,differences=1))
tsdisplay(diff(pax_ts,differences = 1))
arima(pax_ts,c(4,1,12)) 
arima(pax_ts,c(8,1,12)) #mejor
pax_arima<-arima(pax_ts,c(8,1,12))
pax_forecast<-forecast(pax_arima,h=1)
pax_forecast
autoplot(pax_forecast)
checkresiduals(pax_forecast)

MCO <- lm(pax ~ .,data=data)
summary(MCO)

prueba <- dynlm(pax ~ kms + tbk.real + L(pax,1:1),data = data_ts)
dwt(prueba)

prueba2 <- dynlm(pax ~ kms + tbk.real + L(pax,1:2),data = data_ts)
dwt(prueba2) #lag de 2do orden para quitar autocorrelación

resumen_reg <- summary(prueba)
prueba$fitted.values
write.xlsx(prueba$fitted.values,"predicho.xlsx")

#GRAFICAR SERIE ORIGINAL CON FITTED

##deben coincidir la cantidad de datos
n_fitted <- length(prueba$fitted.values)
n_original <- length(pax_ts)
pax_ts_alineado <- tail(pax_ts, n_fitted)

##crear df
df_plot <- data.frame(
  Fecha = seq(from = as.Date("2021-11-01"), by = "month", length.out = n_fitted),
  pax_ts = as.numeric(pax_ts_alineado),
  fitted_ts = as.numeric(prueba$fitted.values)
) %>%
  pivot_longer(cols = c("pax_ts", "fitted_ts"), names_to = "Serie", values_to = "Valor")

##graph
ggplot(df_plot, aes(x = Fecha, y = Valor, color = Serie)) +
  geom_line(linewidth = 1) +
  labs(title = "Comparación Serie Original vs. Ajustada",
       x = "Fecha", y = "Cantidad de Pasajeros") +
  scale_color_manual(values = c("black", "red")) +
  theme_minimal()
