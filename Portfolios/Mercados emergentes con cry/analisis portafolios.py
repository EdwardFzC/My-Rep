import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import yfinance as yf
from pypfopt import EfficientFrontier, expected_returns
from pypfopt import risk_models
from pypfopt import plotting
import quantstats as qs


# Fecha de inicio
start = '2015-01-01'

# Obtener datos de criptomonedas 
tk_cry = ['BTC-USD', 'ETH-USD']
cripto = yf.download(tk_cry, start=start)['Adj Close']
cripto.columns = ['BTC', 'ETH']
cripto.head()

# Obtener datos de mercados emergentes
tk_emerg_etf = ['EEM', 'VWO']
emerg_etf = yf.download(tk_emerg_etf, start=start)['Adj Close']
emerg_etf.columns = ['EEM', 'VWO']
emerg_etf.head()

# Obtener datos de principales indices del mundo
tk_major_ind = ['^GSPC', '^IXIC', '^GDAXI', '^FTSE', '^N225']
major_ind = yf.download(tk_major_ind, start=start)['Adj Close']
major_ind.columns = ['SP500', 'NASDAQ', 'DAX', 'FTSE', 'NIKKEI']
major_ind.head()

# Datos de indices principales de Mexico, brasil, china, india, rusia, turquia, colombia, argentina, taiwan, corea del sur, sudafrica, indonesia, tailandia, malasia, filipinas, polonia, hungria, chile, peru, egipto
tk_emerging_markets = ['^MXX', '^BVSP', '000001.SS', '^BSESN', '^N225', '^ATG', '^MERV', '^TWII', '^KS11', '^JN0U.FGI', '^FTSEJSE', '^JKSE', '^SET.BK', '^KLSE', '^PSEI', '^WIG', '^BUX', '^IPSA', '^SPBLPGPT', '^EGX30']
emerging_markets = yf.download(tk_emerging_markets, start=start)['Adj Close']
emerging_markets.columns = ['MEX', 'BRA', 'CHN', 'IND', 'RUS', 'TUR', 'COL', 'ARG', 'TWN', 'KOR', 'ZAF', 'IDN', 'THA', 'MYS', 'PHL', 'POL', 'HUN', 'CHL', 'PER', 'EGY']
emerging_markets.head()



# funcion rendimientos log
def rlog(data):
    return np.log(data/data.shift(1)).dropna()

# Funcion rendimientos pct
def pct(data):
    return data.pct_change().dropna()

# Portafolio y metricas de preformance
def portafolio(data, nombre):
    # Calcular los rendimientos esperados y la matriz de covarianza
    mu = expected_returns.mean_historical_return(data)
    S = risk_models.sample_cov(data)
    
    # Crear el objeto EfficientFrontier
    ef = EfficientFrontier(mu, S)
    
    # Optimizar el portafolio para maximizar el ratio de Sharpe
    weights_sharpe = ef.max_sharpe()
    cleaned_weights_sharpe = ef.clean_weights()
    performance_sharpe = ef.portfolio_performance(verbose=True)
    
    # Optimizar el portafolio para minimizar la volatilidad
    ef_min_vol = EfficientFrontier(mu, S)
    weights_min_vol = ef_min_vol.min_volatility()
    cleaned_weights_min_vol = ef_min_vol.clean_weights()
    performance_min_vol = ef_min_vol.portfolio_performance(verbose=True)
    
    # Optimizar el portafolio para maximizar el rendimiento
    ef_max_ret = EfficientFrontier(mu, S)
    weights_max_ret = ef_max_ret.max_quadratic_utility()
    cleaned_weights_max_ret = ef_max_ret.clean_weights()
    performance_max_ret = ef_max_ret.portfolio_performance(verbose=True)
    
    # Imprimir el nombre del portafolio
    print(f"Portafolio: {nombre}")
    
    # Retornar los pesos y rendimientos de los tres portafolios
    return {
        "max_sharpe": {
            "weights": cleaned_weights_sharpe,
            "performance": performance_sharpe
        },
        "min_volatility": {
            "weights": cleaned_weights_min_vol,
            "performance": performance_min_vol
        },
        "max_return": {
            "weights": cleaned_weights_max_ret,
            "performance": performance_max_ret
        },
        "nombre": nombre
    }

# Frontera eficiente
def graficar_frontera(data, resultados):
    mu = expected_returns.mean_historical_return(data)
    S = risk_models.sample_cov(data)
    
    ef = EfficientFrontier(mu, S)
    fig, ax = plt.subplots()
    
    # Graficar la frontera eficiente
    plotting.plot_efficient_frontier(ef, ax=ax, show_assets=False)
    
    # Añadir los puntos de los portafolios óptimos
    ax.scatter(resultados['max_sharpe']['performance'][1], resultados['max_sharpe']['performance'][0], marker='*', color='r', s=100, label='Máximo Sharpe')
    ax.scatter(resultados['min_volatility']['performance'][1], resultados['min_volatility']['performance'][0], marker='*', color='g', s=100, label='Mínima Volatilidad')
    ax.scatter(resultados['max_return']['performance'][1], resultados['max_return']['performance'][0], marker='*', color='b', s=100, label='Máximo Rendimiento')
    
    # Añadir leyenda y etiquetas
    ax.legend()
    ax.set_title('Frontera Eficiente con Portafolios Óptimos')
    ax.set_xlabel('Volatilidad')
    ax.set_ylabel('Rendimiento')
    
    plt.show()




# Función para calcular el drawdown
def calcular_drawdown(data):
    cum_returns = (1 + data).cumprod()
    peak = cum_returns.cummax()
    drawdown = (cum_returns - peak) / peak
    return drawdown

# Función para graficar el rendimiento acumulado, drawdown y underwater
def graficar_rendimiento_drawdown(data, nombre):
    rendimientos = rlog(data).dropna()
    drawdown = calcular_drawdown(rendimientos)
    
    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(10, 12), sharex=True)
    
    # Graficar el rendimiento acumulado
    (1 + rendimientos).cumprod().plot(ax=ax1)
    ax1.set_title(f'Rendimiento Acumulado del Portafolio: {nombre}')
    ax1.set_ylabel('Rendimiento Acumulado')
    
    # Graficar el drawdown
    drawdown.plot(ax=ax2, color='red')
    ax2.set_title('Drawdown del Portafolio')
    ax2.set_ylabel('Drawdown')
    
    # Graficar el underwater
    drawdown.plot(ax=ax3, color='blue')
    ax3.fill_between(drawdown.index, drawdown, color='blue', alpha=0.3)
    ax3.set_title('Underwater del Portafolio')
    ax3.set_ylabel('Drawdown')
    ax3.set_xlabel('Fecha')
    
    plt.tight_layout()
    plt.show()
    

# Función para calcular el portafolio optimizado con un riesgo máximo específico
def portafolio_con_riesgo_fijo(data, nombre, riesgo_maximo):
    # Calcular los rendimientos esperados y la matriz de covarianza
    mu = expected_returns.mean_historical_return(data)
    S = risk_models.sample_cov(data)
    
    # Crear el objeto EfficientFrontier
    ef = EfficientFrontier(mu, S)
    
    # Optimizar el portafolio para limitar el riesgo a un número específico máximo
    weights_risk_limited = ef.efficient_risk(riesgo_maximo)
    cleaned_weights_risk_limited = ef.clean_weights()
    performance_risk_limited = ef.portfolio_performance(verbose=True)
    
    # Imprimir el nombre del portafolio
    print(f"Portafolio: {nombre}")
    
    # Retornar los pesos y rendimientos del portafolio optimizado con riesgo limitado
    return {
        "risk_limited": {
            "weights": cleaned_weights_risk_limited,
            "performance": performance_risk_limited
        },
        "nombre": nombre
    }
    
    
    
# Obtener key metrics de los portafolios con quantstats
def key_metrics(data):
    rendimientos = rlog(data).dropna()
    qs.reports.html(rendimientos, output='report.html')
    return qs.reports.metrics(rendimientos)





cry_port = portafolio(cripto, 'Criptomonedas')