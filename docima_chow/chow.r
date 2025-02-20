library(strucchange)
library(openxlsx)


#leer los datos desde el archivo excel
data_raw <- read.xlsx("datos_transporte.xlsx", sheet = 'datos ln')
data_raw$fecha<-as.Date(data_raw$mes,origin="1899-12-30")
mes<-data_raw$fecha
pax<-data_raw$pax
tiempo<-1:39
data <- data_raw[, c(2:5)]

#convertir los datos a serie de tiempo
data_ts <- ts(data, start = c(2021, 11), end = c(2025, 1), frequency = 12)
plot(data_ts, main = "Series de Tiempo de Datos de Transporte")


#crear un data frame para el análisis
datos <- data.frame(tiempo,mes,pax)

#prueba de Chow
punto_quiebre<-26
chow_test <- sctest(tiempo ~ pax, type = "Chow", point = punto_quiebre, data = datos)
print(chow_test)

plot(tiempo ~ pax, data = datos, col = ifelse(tiempo <= punto_quiebre, "blue", "red"),
     main = "Prueba de Chow: Quiebre en t = 26 (12/2023)", xlab = "pax", ylab = "mes")
abline(lm(tiempo ~ pax, data = datos[tiempo <= punto_quiebre, ]), col = "blue", lwd = 2)
abline(lm(tiempo ~ pax, data = datos[tiempo > punto_quiebre, ]), col = "red", lwd = 2)
legend("topleft", legend = c("Antes del quiebre", "Después del quiebre"),
       col = c("blue", "red"), lwd = 2)


#prueba de fluctuación de parámetros (residuos recursivos)
fluctuation_test <- efp(tiempo ~ pax, type = "Rec-CUSUM", data = datos)
plot(fluctuation_test, main = "Prueba de Fluctuación de Parámetros",
     ylab = "Residuos recursivos")

#contraste de residuos recursivos
recursive_test <- efp(tiempo ~ pax, type = "Rec-MOSUM", data = datos)
plot(recursive_test, main = "Contraste de Residuos Recursivos (MOSUM)",
     ylab = "Residuos recursivos")

#detección automática del punto de quiebre
bp_test <- breakpoints(tiempo ~ pax, data = datos)
summary(bp_test)
plot(bp_test, main = "Detección Automática del Quiebre Estructural")
lines(bp_test)


