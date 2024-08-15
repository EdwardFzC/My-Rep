import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from statsmodels.tsa.stattools import grangercausalitytests
from datetime import datetime, timedelta
import matplotlib.dates as mdates
import seaborn as sns
import io
from contextlib import redirect_stdout
from matplotlib.colors import LinearSegmentedColormap


# Función para calcular el test de Granger a lo largo del tiempo
def granger_window_test(data, var_causa, var_efecto, ventana=30, lag = 1):
    n = len(data)
    resultados = pd.DataFrame({
        'date': pd.to_datetime(data.index),
        'p_value': [np.nan] * n
    })

    for i in range(ventana, n):
        subset_data = data.iloc[i - ventana + 1:i + 1]
        try:
            test = grangercausalitytests(subset_data[[var_efecto, var_causa]], maxlag=[lag])
            resultados.loc[i, 'p_value'] = test[1][0]['ssr_ftest'][1]
        except Exception as e:
            print(f"Error en el cálculo de Granger para el punto {i}: {str(e)}")
    return resultados

def granger_index(data, max_val=10):
    def impacto(p, max_val):
        return np.where(p <= 0.05, 1 + ((0.05 - p) / 0.05) * (max_val - 1), 1 - (p - 0.05))
    data['index'] = impacto(data['p_value'], max_val)
    return data

def granger_window_comparation_index(data, var1, var2, ventana=30, lag=1, max_val=10):
    n = len(data)
    resultados = pd.DataFrame({
        'date': pd.to_datetime(data.index),
        'p_value_1_2': [np.nan] * n,
        'p_value_2_1': [np.nan] * n,
        'indicador_1_2': [np.nan] * n,
        'indicador_2_1': [np.nan] * n
    })
    
    def impacto(p, max_val):
        return np.where(p <= 0.05, 1 + ((0.05 - p) / 0.05) * (max_val - 1), 1 - (p - 0.05))
    
    for i in range(ventana, n):
        subset_data = data.iloc[i - ventana + 1:i + 1]
        try:
            # Test de var1 causando var2
            test_1_2 = grangercausalitytests(subset_data[[var2, var1]], maxlag=[lag])
            p_value_1_2 = test_1_2[lag][0]['ssr_ftest'][1]
            
            # Test de var2 causando var1
            test_2_1 = grangercausalitytests(subset_data[[var1, var2]], maxlag=[lag])
            p_value_2_1 = test_2_1[lag][0]['ssr_ftest'][1]
            
            resultados.loc[i, 'p_value_1_2'] = p_value_1_2
            resultados.loc[i, 'p_value_2_1'] = p_value_2_1
            
            # Cálculo de los indicadores
            resultados.loc[i, 'indicador_1_2'] = impacto(p_value_1_2, max_val)
            resultados.loc[i, 'indicador_2_1'] = impacto(p_value_2_1, max_val)
        
        except Exception as e:
            print(f"Error en el cálculo de Granger para el punto {i}: {str(e)}")
    
    return resultados


"""def graficar_granger_test(data, var_causa, var_efecto, ventana=30):
    resultados = data.dropna()

    plt.figure(figsize=(12, 6))
    plt.axhline(y=0.05, color='black', linestyle='--')
    plt.scatter(resultados['date'], resultados['p_value'],
                c=['#CDAD00' if p < 0.05 else '#528B8B' for p in resultados['p_value']], s=1)

    plt.title(f"Causalidad de Granger: {var_causa} -> {var_efecto}")
    plt.suptitle(f"Ventana Movil = {ventana}", y=0.95)
    plt.xlabel("")
    plt.ylabel("Valor p")

    plt.legend(['', 'Influencia Potencial (p < 0.05)'], loc='lower center', bbox_to_anchor=(0.5, -0.15))
    plt.tight_layout()
    return plt"""

def graficar_granger_test(data, var_causa, var_efecto, ventana=30):
    resultados = data.dropna()
    
    # Configurar el estilo
    sns.set_style("darkgrid")
    sns.set_palette("deep")
    
    # Crear la figura y los ejes
    fig, ax = plt.subplots(figsize=(16, 10))  # Aumentado ligeramente la altura
    
    # Graficar los puntos
    scatter = ax.scatter(resultados['date'], resultados['p_value'],
                         c=['#FFD700' if p < 0.05 else '#4682B4' for p in resultados['p_value']], 
                         s=5, alpha=0.7)
    
    # Añadir línea de umbral
    ax.axhline(y=0.05, color='#FF4500', linestyle='--', linewidth=2, alpha=0.7)
    
    # Configurar títulos y etiquetas
    fig.suptitle(f"Ventana Móvil = {ventana} días", 
                 fontsize=10, fontweight='bold', y=0.90)
    ax.set_title(f"Causalidad de Granger: {var_causa} → {var_efecto}", 
                 fontsize=20, style='italic', pad=30)
    
    ax.set_xlabel(None)
    ax.set_ylabel("Valor p", fontsize=14, labelpad=10)
    
    # Configurar el formato de las fechas en el eje x
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m-%d'))
    ax.xaxis.set_major_locator(mdates.MonthLocator(interval=2))
    ax.xaxis.set_minor_locator(mdates.MonthLocator())
    plt.setp(ax.xaxis.get_majorticklabels(), rotation=45, ha='right')
    
    # Ajustar los límites del eje y
    ax.set_ylim(-0.05, 1.05)
    
    # Añadir leyenda
    legend_elements = [
        plt.Line2D([0], [0], marker='o', color='w', label='Influencia Potencial (p < 0.05)',
                   markerfacecolor='#FFD700', markersize=10),
        plt.Line2D([0], [0], marker='o', color='w', label='Sin Influencia Significativa',
                   markerfacecolor='#4682B4', markersize=10),
        plt.Line2D([0], [0], color='#FF4500', lw=2, linestyle='--', label='Umbral (p = 0.05)')
    ]
    ax.legend(handles=legend_elements, loc='upper center', bbox_to_anchor=(0.5, -0.15),
              ncol=3, fontsize=12, frameon=True, facecolor='white', edgecolor='gray')
    
    # Ajustar los límites y el diseño
    plt.tight_layout()
    plt.subplots_adjust(top=0.88, bottom=0.2)
    
    # Añadir una cuadrícula más sutil
    ax.grid(True, which='major', linestyle='-', linewidth=0.5, color='#E0E0E0')
    ax.grid(True, which='minor', linestyle=':', linewidth=0.5, color='#F0F0F0')
    
    return fig






def granger_test_temporal_graficas(data, var_causa, var_efecto, ventana=30):
    resultados = calcular_granger_temporal(data, var_causa, var_efecto, ventana)
    plot = graficar_granger_test(resultados, var_causa, var_efecto, ventana)
    return plot

def calcular_rendimientos_logaritmicos(data, date_column = "date", date_format='%d/%m/%Y'):
    # Verificar si la primera columna es 'date' y si no es el índice
    if data.columns[0] == date_column and data.index.name != date_column:
        # Asegurarse de que la primera columna sea de tipo datetime
        data['date'] = pd.to_datetime(data['date'], format=date_format)
        data.set_index(date_column, inplace=True)
    
    data_rlog = np.log(data / data.shift(1))
    data_rlog = data_rlog.dropna()
    
    return data_rlog



data = pd.read_csv("Cry/40 criptos analisis/crypto.csv/crypto.csv")
# Seleccionar columnas que contengan prefijo "_close"
data_cl = data.filter(items=['date']).join(data.filter(like='close'))
names = {col: col.replace('USDT_close', '') for col in data_cl.columns}
data_cl = data_cl.rename(columns=names)
data_cl['date'] = pd.to_datetime(data['date'], format='%d/%m/%Y %H:%M')
data_cl.head()

# obtener rendimientos log
data_rlog = calcular_rendimientos_logaritmicos(data_cl)
data_rlog.head()

resultados_lrends= calcular_granger_temporal(data_rlog, 'BTC', 'ETH', ventana=24)
resultados_lrends.head()

plot = graficar_granger_test(resultados_lrends, 'BTC', 'ETH', ventana=24)
plot.show()
