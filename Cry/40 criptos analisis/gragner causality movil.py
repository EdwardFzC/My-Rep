import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from statsmodels.tsa.stattools import grangercausalitytests
from datetime import datetime, timedelta
import matplotlib.dates as mdates
import seaborn as sns


# Función para calcular el test de Granger a lo largo del tiempo
def calcular_granger_temporal(data, var_causa, var_efecto, ventana=30, lag = 1):
    n = len(data)
    resultados = pd.DataFrame({
        'fecha': range(n),
        'p_value': [np.nan] * n
    })

    for i in range(ventana, n):
        subset_data = data.iloc[i - ventana + 1:i + 1]
        try:
            test = grangercausalitytests(subset_data[[var_efecto, var_causa]], maxlag=[lag])
            resultados.loc[i, 'p_value'] = test[1][0]['ssr_ftest'][1]
        except Exception as e:
            print(f"Error en el cálculo de Granger para el punto {i}: {str(e)}")
    resultados['date'] = pd.to_datetime(data.index)
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

def calcular_rendimientos_logaritmicos(data):
    # Asegúrate de que la primera columna sea de tipo datetime
    data.iloc[:, 0] = pd.to_datetime(data.iloc[:, 0])

    # Calculamos los rendimientos logarítmicos
    # Tomamos el logaritmo de los valores y luego restamos el logaritmo de los valores anteriores
    data_rlog = data.iloc[:, 1:].apply(lambda x: np.log(x / x.shift(1)))

    # Añadir la columna de fechas para que no se pierda
    data_rlog.insert(0, 'date', data.iloc[:, 0])
    data_rlog = data_rlog.dropna()
    data_rlog= data_rlog.set_index('date')
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
