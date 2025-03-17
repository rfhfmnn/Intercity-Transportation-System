#librerias 

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.api as sm
from statsmodels.tsa.stattools import adfuller
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.arima.model import ARIMA
from statsmodels.stats.diagnostic import acorr_ljungbox
from statsmodels.stats.stattools import durbin_watson
import seaborn as sns
from datetime import datetime, timedelta

#-----------------------------------------------

#datos
data_raw = pd.read_excel("datos_transporte.xlsx", sheet_name='datos ln')
data = data_raw.iloc[:, 1:5]  # Seleccionar columnas 2 a 5 (índice base 0 en Python)

#serie
fecha_inicio = pd.Timestamp(year=2021, month=11, day=1)
fecha_fin = pd.Timestamp(year=2025, month=1, day=1)
fechas = pd.date_range(start=fecha_inicio, end=fecha_fin, freq='MS')
data.index = fechas[:len(data)]

#serie pax
pax = data['pax']

#graph
plt.figure(figsize=(15, 8))
for i, col in enumerate(data.columns):
    plt.subplot(2, 2, i+1)
    plt.plot(data.index, data[col], color='blue', linewidth=2)
    plt.title(f'Serie {col}')
    plt.xlabel('Tiempo')

plt.tight_layout()
plt.show()

#graph pax
plt.figure(figsize=(10, 6))
plt.plot(pax.index, pax, color='blue', linewidth=2)
plt.title('Evolución de la Cantidad de Pasajeros')
plt.xlabel('Tiempo')
plt.ylabel('Cantidad de Pasajeros')
plt.show()

#adf test
pax_diff1 = pax.diff().dropna() 
adf_result1 = adfuller(pax_diff)
print('Resultado de la prueba ADF en la serie diferenciada:')
print(f'Estadístico ADF: {adf_result[0]}')
print(f'p-value: {adf_result[1]}')
print(f'Valores críticos: {adf_result[4]}')

#graph adf
plt.figure(figsize=(12, 8))
plt.subplot(311)
plt.plot(pax_diff1)
plt.title('Serie con primera diferencia')
plt.subplot(312)
plot_acf(pax_diff1, ax=plt.gca(), lags=30)
plt.subplot(313)
plot_pacf(pax_diff1, ax=plt.gca(), lags=18)
plt.tight_layout()
plt.show()

#modelo
model1 = ARIMA(pax, order=(8, 1, 12))
resultados = model1.fit()
print(resultados.summary())

pax_forecast = pax_arima.forecast(steps=2)
print("Pronóstico para el siguiente período:")
print(pax_forecast)

#forecast. 2 meses
pax_forecast = pax_arima.forecast(steps=2)
print("Pronóstico para los próximos 2 meses:")
print(pax_forecast)
plt.figure(figsize=(10, 6))
plt.plot(pax, label='Observado', color='blue')

ultima_fecha = pax.index[-1]
fecha_pronostico1 = ultima_fecha + pd.DateOffset(months=1)
fecha_pronostico2 = ultima_fecha + pd.DateOffset(months=2)
fechas_pronostico = [fecha_pronostico1, fecha_pronostico2]


plt.scatter(fechas_pronostico, pax_forecast, color='red', marker='o', s=50, label='Pronósticos')
puntos_x = [ultima_fecha] + fechas_pronostico
puntos_y = [pax.iloc[-1]] + list(pax_forecast)
plt.plot(puntos_x, puntos_y, color='red', linestyle='--', linewidth=1.5)


pred_ci = pax_arima.get_prediction(start=len(pax), end=len(pax)+1).conf_int()
for i, fecha in enumerate(fechas_pronostico):
    limite_inferior = pred_ci.iloc[i, 0]
    limite_superior = pred_ci.iloc[i, 1]
    plt.fill_between(
        [fecha - timedelta(days=5), fecha + timedelta(days=5)],
        [limite_inferior, limite_inferior],
        [limite_superior, limite_superior],
        color='lightblue',
        alpha=0.5
    )
    plt.hlines(limite_inferior, fecha - timedelta(days=5), fecha + timedelta(days=5), 
               colors='steelblue', linestyles='dotted', linewidth=1)
    plt.hlines(limite_superior, fecha - timedelta(days=5), fecha + timedelta(days=5), 
               colors='steelblue', linestyles='dotted', linewidth=1)


plt.grid(True, linestyle='--', alpha=0.7)
plt.legend()
plt.title('Pronóstico ARIMA para 2 meses con intervalos de confianza')
plt.xlabel('Fecha')
plt.ylabel('Cantidad de Pasajeros')
plt.tight_layout()
plt.show()

#elasticidad

#mco
X = data.drop('pax', axis=1) #data.drop elimina la columna pax y mantiene las demás 
y = data['pax']
X = sm.add_constant(X)
modelo_mco = sm.OLS(y, X).fit()
print(modelo_mco.summary())

#dinámico
data_dyn = data.copy()
data_dyn['pax_lag1'] = data_dyn['pax'].shift(1) 
data_dyn = data_dyn.dropna() 

X_dyn = data_dyn[['kms', 'tbk real', 'pax_lag1']]
X_dyn = sm.add_constant(X_dyn)
y_dyn = data_dyn['pax']
modelo_dyn = sm.OLS(y_dyn, X_dyn).fit()
print("Modelo dinámico con 1 rezago:")
print(modelo_dyn.summary())

#d-w
dw_stat = durbin_watson(modelo_dyn.resid)
print(f"Estadístico Durbin-Watson: {dw_stat}")

#2 rezagos 
data_dyn2 = data.copy()
data_dyn2['pax_lag1'] = data_dyn2['pax'].shift(1)
data_dyn2['pax_lag2'] = data_dyn2['pax'].shift(2)
data_dyn2 = data_dyn2.dropna()

X_dyn2 = data_dyn2[['kms', 'tbk real', 'pax_lag1', 'pax_lag2']]
X_dyn2 = sm.add_constant(X_dyn2)
y_dyn2 = data_dyn2['pax']
modelo_dyn2 = sm.OLS(y_dyn2, X_dyn2).fit()
print("Modelo dinámico con 2 rezagos:")
print(modelo_dyn2.summary())

dw_stat2 = durbin_watson(modelo_dyn2.resid)
print(f"Estadístico Durbin-Watson (modelo con 2 rezagos): {dw_stat2}")

#fitted
valores_ajustados = modelo_dyn2.fittedvalues
pd.DataFrame(valores_ajustados, columns=['predicted']).to_excel("predicho.xlsx")

#original vs fitted
n_fitted = len(valores_ajustados) 
pax_alineado = pax.iloc[-n_fitted:]

#df
df_plot = pd.DataFrame({
    'Fecha': pax_alineado.index,
    'pax_ts': pax_alineado.values,
    'fitted_ts': valores_ajustados.values
})

#pivot longer
df_plot_long = pd.melt(df_plot, id_vars=['Fecha'], value_vars=['pax_ts', 'fitted_ts'], 
                       var_name='Serie', value_name='Valor')

#graph
plt.figure(figsize=(10, 6))
sns.lineplot(data=df_plot_long, x='Fecha', y='Valor', hue='Serie', linewidth=1)
plt.title('Comparación Serie Original vs. Ajustada')
plt.xlabel('Fecha')
plt.ylabel('Cantidad de Pasajeros')
plt.legend(title='')
plt.xticks(rotation=45)
colores = {'pax_ts': 'black', 'fitted_ts': 'red'}
plt.show()

