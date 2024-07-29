

# FILTRAR LAS DE MAYOR VOLUMEN?





####### LIBRERIAS #########
library(readr)
library(plotly)
library(xts)
library(reshape2)
library(gtools)
library(quantmod)
library(lmtest)
library(vars)
library(stats)
library(tseries)
library(purrr)
library(broom)
library(ggpubr)
library(knitr)
library(tidyverse)
# eliminar notacion cientifica
options(scipen=999)

###### LEER DATOS CRIPTO##########
    # DATOS CRYPTO

# Lectura de datos
data_raw <- read_csv("1Weas/Github/My-Rep/Cry/40 criptos ¿tesis/crypto.csv/crypto.csv")

# Filtrar los datos entre 31 de diciembre de 2020 a las 23:00 y la fecha más reciente
data_raw$date <- as.POSIXct(data_raw$date, format="%d/%m/%Y %H:%M") # cosa rara se establece formato al revés
start_date <- "2020-12-31 23:00:00" # Fecha filtro
end_date <- "2024-02-20 02:00:00" # Fecha final
data <- filter(data_raw, date>= start_date)
# Comprobar de formato fecha
#year(data$date[1])
#month(data$date[1])
#day(data$date[1])

# Función renombrar columnas eliminando el sufijo "USDT_close u otros" y crear variable con nombres de activos
get_assets <- function(data, suffix = "NA") {
  column_names <- colnames(data_close_h)
  assets <- gsub(suffix, "", column_names)
  return(assets)
}

####### FUNCIONES RENDS #########
# FUNCIÓN PARA CALCULAR RENDIMIENTOS

# Función para calcular rendimientos logarítmicos
calc_log_returns <- function(data) {
  if (colnames(data)[1] == "date") {
    log_returns <- data.frame(date= data$date[-1], lapply(data[,-1], function(x) diff(log(x))))
  } else {
    log_returns <- data.frame(lapply(data, function(x) diff(log(x))))
    log_returns <- log_returns[-1,] %>% as.xts()
  }
  return(log_returns)
}

# funcion para rendimientos en cambio porcentual
calc_pct_returns <- function(data) {
  if (colnames(data)[1] == "date") {
    pct_returns <- data.frame(date= data$date[-1], lapply(data[,-1], function(x) diff(x)/x[-length(x)]))
  } else {
    pct_returns <- data.frame(lapply(data, function(x) diff(x)/x[-length(x)]))
    pct_returns <- pct_returns[-1,] %>% as.xts()
  }
  return(pct_returns)
}

# Función para calcular rendimientos acumulados
calc_acum_returns <- function(data) {
  if (colnames(data)[1] == "date") {
    acum_rends <- data.frame(date= data$date, lapply(data[,-1], function(x) cumsum(x)))
  } else {
    acum_rends <- data.frame(lapply(data, function(x) cumsum(x))) %>% as.xts()
  }
  return(acum_rends)
}

# funcion xts a dataframe
xts_to_dataframe <- function(data) {
  data <- as.data.frame(data) %>% rownames_to_column(var="date")
  data$date <- as.Date(data$date, format = "%Y-%m-%d")
  return(data)
}





##SPACE####
##SPACE###
######DATOS BOLSA#########
      # Datos Indices
# Obtener datos horarios del syp500 y nasdaq desde yahoo finance
syp500_prices <- data.frame(getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE))
nasdaq_prices <- data.frame(getSymbols("^IXIC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE))

syp500_prices <- syp500_prices %>% rownames_to_column(var = "date")
nasdaq_prices <- nasdaq_prices %>% rownames_to_column(var = "date")

syp500_prices$date <- as.Date(syp500_prices$date, format = "%Y-%m-%d")
nasdaq_prices$date <- as.Date(nasdaq_prices$date, format = "%Y-%m-%d")


syp500_close <- syp500_prices %>% select(date, GSPC.Close) %>% rename(syp500 = GSPC.Close)
nasdaq_close <- nasdaq_prices %>% select(date, IXIC.Close) %>% rename(nasdaq = IXIC.Close)




########PREPARAR DATOS CRY########
    # PREPARACIÓN DE DATOS

# Seleccionar columnas de precios de cierre y volumen como nuevas df
data_price_h <- data %>% select(!contains("volume"))
data_close_h <- data %>% select(date, contains("close"))
data_vol_h <- data %>% select(date, contains("volume"))

data_log_h <- calc_log_returns(data_close_h)

# Obtener el nombre de los activos usados
assets <- get_assets(data_close_h, "USDT_close")
# Cambiar el nombre de columnas en Data frames para coincidir con los activos

colnames(data_close_h) <- get_assets(data_close_h, "USDT_close")
colnames(data_vol_h) <- get_assets(data_vol_h,"USDT_volume")

# Crear un dataframe con los volúmenes en USD
data_vol_usd_h <- data.frame(date=data_vol_h$date, mapply(function(vol, close) vol * close, data_vol_h[-1], data_close_h[-1]))


#######FUN CAMBIO TEMP#########
# FUNCION DE CAMBIO DE TEMPORALIDAD

# Función para cambiar de temporalidad (usando XTS)
change_timeframe <- function(data, timeframe, sum=FALSE, OHLC=FALSE) {
  data_xts <- xts(data[-1], order.by = data$date) # Convertir a tipo xts y eliminar columna de fecha, ademas asegurar orden por fecha
  if (sum==TRUE) {
    data_xts <- period.apply(data_xts, endpoints(data_xts, timeframe), colSums) # Caso volumen, obtener la suma de datos dentro del periodo
  }else {
    data_xts <- to.period(data_xts, period = timeframe, OHLC=OHLC) # Normal, cambio de temporalidad ultimo dato de periodo
  }
  data_xts <- as.data.frame(data_xts) %>% rownames_to_column(var="date")# Convertir a data frame
  data_xts$date <- data_xts$date %>% as.Date(format = "%Y-%m-%d")

  return(data_xts)
}

# en to.period el default es OHLC=TRUE, si se pone en FALSE se obtiene el precio de cierre del periodo asignado
###NOTAS####

#   PERIODO	    | MODIFICADOR  |            TIMEFRAME	          |    DESCRIPCION

#   Segundos    |  "seconds"   |   "secs" | "second"  | "sec"	  | Agrega datos por segundos
#   Minutos	    |  "minutes"   |   "mins" | "minute" | "min"	  | Agrega datos por minutos
#   Horas	      |  "hours"     |   "hour"  ___________________  | Agrega datos por horas
#   Días	      |  "days"      |   "day"   ___________________  | Agrega datos por días
#   Semanas	    |  "weeks"     |   "week"  ___________________  | Agrega datos por semanas
#   Meses	      |  "months"    |   "month" ___________________  | Agrega datos por meses
#   Trimestres  |	 "quarters"  |   "quarter" _________________  | Agrega datos por trimestres
#   Años	      |  "years"     |   "year"  ___________________  | Agrega datos por años



#     FUNCION             	DESCRIPCION

#     sum             |  Calcula la suma de los valores
#     mean            |  Calcula el promedio de los valores
#     median          |  Calcula la mediana de los valores
#     sd              |  Calcula la desviación estándar de los valores
#     min             |  Encuentra el valor mínimo
#     max             |  Encuentra el valor máximo
#     first           |  Encuentra el primer valor del segmento
#     last            |  Encuentra el último valor del segmento
#     colSums         |  Calcula la suma de cada columna
#     colMeans        |  Calcula el promedio de cada columna
#     colMins         |  Calcula el valor mínimo de cada columna
#     colMaxs         |  Calcula el valor máximo de cada columna
#     apply           |  Aplica una función a las filas o columnas de una matriz
#     custom_function |  Cualquier función personalizada definida por el usuario





#######UNIR CRY - IND#########
    # UNIR PRECIOS DIARIOS DE CRIPTOS CON INDICES
data_close_d <- change_timeframe(data_close_h, "day")

data_close_all <- merge(as.xts(syp500_close), as.xts(nasdaq_close), as.xts(data_close_d), all = F)


assets_all <- get_assets(data_close_all)

# Hacer que las series sean estacionarias en data_close_all
close_all_log <- log(as.data.frame(data_close_all))

all_lcrends <- calc_log_returns(data_close_all)
# los unicos estacionario son los rendimientos logaritmicos

all_lcrends2 <- xts_to_dataframe(all_lcrends)

######## VARIABLES CRY ########
# PREPARAR OTROS DATOS CRYPTO
btc_dom <- read_csv("1Weas/Github/My-Rep/Cry/40 criptos ¿tesis/btc_dominance.csv")
fear_greed_index <- read_csv("1Weas/Github/My-Rep/Cry/40 criptos ¿tesis/fear_greed_index.csv")
market_cap <- read_csv("1Weas/Github/My-Rep/Cry/40 criptos ¿tesis/market_cap.csv")

btc_dom <- rename(btc_dom, date = DateTime, btc_dom = BTC)
fear_greed_index <- rename(fear_greed_index, date = DateTime, fg_index = "Fear & greed index" )
market_cap <- rename(market_cap, date = DateTime, market_cap = "Market cap")

btc_dom$date <- as.Date(btc_dom$date, format = "%Y-%m-%d")
fear_greed_index$date <- as.Date(fear_greed_index$date, format = "%Y-%m-%d")
market_cap$date <- as.Date(market_cap$date, format = "%Y-%m-%d")

btc_dom <- btc_dom[1:2]
btc_close <- data_close_d 
btc_close2 <- btc_close[1:2]
eth_close <- btc_close[c(1,3)]

btc_w_fgi <- merge(as.xts(btc_close2),as.xts(fear_greed_index), all = F)
btc_w_dom <- merge(as.xts(btc_close2),as.xts(btc_dom), all = F)
btc_w_mcap <-merge(as.xts(btc_close2),as.xts(market_cap), all = F) 

eth_w_fgi <- merge(as.xts(eth_close),as.xts(btc_close), all = F)
eth_w_fgi <- merge(as.xts(eth_close),as.xts(fear_greed_index), all = F)
eth_w_dom <- merge(as.xts(eth_close),as.xts(btc_dom), all = F)
eth_w_mcap <-merge(as.xts(eth_close),as.xts(market_cap), all = F) 
eth_w_btc <- merge(as.xts(eth_close),as.xts(btc_close2), all = F)

# establecerlos como cambios logaritmicos y volverlos data frame
btc_w_lfgi <- calc_log_returns(btc_w_fgi)
  btc_w_lfgi <- xts_to_dataframe(btc_w_lfgi)
btc_w_ldom <- calc_log_returns(btc_w_dom)
  btc_w_ldom <- xts_to_dataframe(btc_w_ldom)
btc_w_lmcap <- calc_log_returns(btc_w_mcap)
  btc_w_lmcap <- xts_to_dataframe(btc_w_lmcap)

eth_w_lfgi <- calc_log_returns(eth_w_fgi)
  eth_w_lfgi <- xts_to_dataframe(eth_w_lfgi)
eth_w_ldom <- calc_log_returns(eth_w_dom)
  eth_w_ldom <- xts_to_dataframe(eth_w_ldom)
eth_w_lmcap <- calc_log_returns(eth_w_mcap)
  eth_w_lmcap <- xts_to_dataframe(eth_w_lmcap)
eth_w_lbtc <- calc_log_returns(eth_w_btc)
  eth_w_lbtc <- xts_to_dataframe(eth_w_lbtc)


#######Granger Causality #########
    # Granger Causality solo full

# verificar estacionalidad
adf.test(data_close_all$syp500)
adf.test(data_close_all$BTC)
adf.test(data_close_all$nasdaq)

# verificar estacionalidad en rendimientos log
adf.test(all_lcrends$syp500)
adf.test(all_lcrends$BTC)
adf.test(all_lcrends$nasdaq)


# determinar numero de lags
lag_selection <- VARselect(all_lcrends[, c("syp500", "BTC")], lag.max = 10, type = "const")
lag_selection$selection





###############TEST DE GRANGER######################
      # TEST DE GRANGER


#####Graficas precios#####


grafica <- data_close_all %>% xts_to_dataframe()

# Seleccionar las primeras 5 columnas (asumiendo que las columnas de interés están dentro de las primeras 5)
grafica <- grafica[, 1:5]

# Reestructurar los datos en formato largo y omitir NA
all_data_close_5 <- melt(grafica, id.vars = "date") %>% na.omit()

# Convertir la variable a carácter
all_data_close_5$variable <- as.character(all_data_close_5$variable)

# Crear la gráfica con ggplot2
precios <- ggplot(all_data_close_5, aes(x = date, y = value, color = variable)) +
  geom_line() +
  scale_color_manual(values = c("syp500" = "#CDAD00", "nasdaq" = "#528B8B", "BTC" = "#EE6AA7", "ETH" = "#9A32CD")) +
  facet_grid(variable ~ ., scales = "free_y") +
  # para poner escala logaritmica se usa scale_y_log10()
  scale_y_log10(breaks = function(x) pretty(x, n = 3)) + 
  labs(title = "Precios de Cierre Bitcoin, S&P 500 y NASDAQ 100",
       x = NULL,
       y = "Precio de Cierre",
       color = "Activo") +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

# Convertir la gráfica a un objeto plotly para interactividad
precios2 <- ggplotly(precios)%>% layout(legend = list(orientation = "h",   # Orientación horizontal
                                                      x = 0.5,              # Centrado horizontalmente
                                                      xanchor = "center",   # Alineación horizontal
                                                      y = -0.2)) 

# Mostrar la gráfica
print(precios2)












# Función para calcular el test de Granger a lo largo del tiempo
calcular_granger_temporal <- function(data, var_causa, var_efecto, ventana = 30) {
  n <- nrow(data)
  resultados <- data.frame(
    fecha = seq_len(n),
    p_value = rep(NA, n)
  )
  
  for (i in ventana:n) {
    subset_data <- data[(i - ventana + 1):i, ]
    tryCatch({
      test <- grangertest(formula(paste(var_efecto, "~", var_causa)), 
                          order = 1, 
                          data = subset_data)
      resultados$p_value[i] <- test$`Pr(>F)`[2]
    }, error = function(e) {
      message("Error en el cálculo de Granger para el punto ", i, ": ", e$message)
    })
  }
  resultados$date <- data$date %>% as.Date()
  return(resultados)
}

# Ejemplo de uso
resultados <- calcular_granger_temporal(all_lcrends2, "syp500", "BTC", ventana = 30)



ggplot(na.omit(resultados), aes(x = date, y = p_value)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  geom_point(aes(color = p_value < 0.05)) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  labs(title = "Causalidad de Granger: SP500 -> BTC",
       x = "Tiempo",
       y = "Valor p",
       color = "Significativo (p < 0.05)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Resumen de los resultados
summary(resultados)


granger_test_temporal_graficas <- function(data, var_causa, var_efecto, ventana = 30) {
  resultados <- calcular_granger_temporal(data, var_causa, var_efecto, ventana = 30)
    plot <- ggplot(na.omit(resultados), aes(x = date, y = p_value)) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
    geom_point(aes(color = p_value < 0.05)) +
    scale_color_manual(values = c("TRUE" = "#CDAD00", "FALSE" = "#528B8B")) +
    labs(title = sprintf("Causalidad de Granger: %s -> %s", var_causa, var_efecto), 
         subtitle = sprintf("Ventana Movil = %d", ventana),
         x = NULL,
         y = "Valor p",
         color = "Influencia Potencial -> (p < 0.05)") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  #plot2 <- ggplotly(plot) %>%
   #layout(legend = list(orientation = "h",  # orientación horizontal
                       #  x = 0.5,  # centra la leyenda horizontalmente
                       #  xanchor = "center",  # ancla la leyenda en el centro horizontal
                       #  y = -0.2))  # coloca la leyenda debajo del gráfico
  return(plot)
}

granger_test_temporal_graficas(all_lcrends2, "nasdaq", "BTC", ventana = 30)


####COMO INDICE#### 


# Función para calcular el índice de impacto
calcular_indice_impacto <- function(data, var_causa, var_efecto, ventana = 30) {
  n <- nrow(data)
  resultados <- data.frame(
    fecha = seq_len(n),
    indice_impacto = rep(NA, n)
  )
  
  for (i in ventana:n) {
    subset_data <- data[(i - ventana + 1):i, ]
    tryCatch({
      # Test de Granger de SP500 -> BTC
      test_sp500_btc <- grangertest(formula(paste(var_efecto, "~", var_causa)), 
                                    order = 1, 
                                    data = subset_data)
      p_value_sp500_btc <- test_sp500_btc$`Pr(>F)`[2]
      
      # Test de Granger de BTC -> SP500
      test_btc_sp500 <- grangertest(formula(paste(var_causa, "~", var_efecto)), 
                                    order = 1, 
                                    data = subset_data)
      p_value_btc_sp500 <- test_btc_sp500$`Pr(>F)`[2]
      
      # Calcular el índice de impacto
      if (p_value_sp500_btc < p_value_btc_sp500) {
        # SP500 impacta más a BTC
        indice <- -log10(p_value_sp500_btc)
      } else {
        # BTC impacta más a SP500 o no hay diferencia significativa
        indice <- log10(p_value_btc_sp500)
      }
      
      resultados$indice_impacto[i] <- indice
      
    }, error = function(e) {
      message("Error en el cálculo para el punto ", i, ": ", e$message)
    })
  }
  
  return(resultados)
}

# Ejemplo de uso
resultados <- calcular_indice_impacto(all_lcrends2, "syp500", "BTC", ventana = 30)
resultados$date <- all_lcrends2$Date %>% as.Date()

# Crear el gráfico
ggplot(resultados, aes(x = date, y = indice_impacto)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Índice de Impacto: SP500 vs BTC",
       x = "Tiempo",
       y = "Índice de Impacto",
       caption = "Valores positivos: SP500 impacta más a BTC\nValores negativos: BTC impacta más a SP500") +
  theme_minimal()

# Resumen de los resultados
summary(resultados)


## CON BTC
# syp500
btc_syp <- calcular_granger_temporal(all_lcrends2, "syp500", "BTC", ventana = 30)
# nasdaq
btc_nasdaq <- calcular_granger_temporal(all_lcrends2, "nasdaq", "BTC", ventana = 30)
# dominancia btc
btc_dom <- calcular_granger_temporal(btc_w_ldom, "btc_dom", "BTC", ventana = 30)
# fear greed index
btc_fgi <- calcular_granger_temporal(btc_w_lfgi, "fg_index", "BTC", ventana = 30)
# market cap
btc_mcap <- calcular_granger_temporal(btc_w_lmcap, "market_cap", "BTC", ventana = 30)



btc_syp <- btc_syp %>% mutate(Variable = "S&P 500")
btc_nasdaq <- btc_nasdaq %>% mutate(Variable = "NASDAQ")
btc_dom <- btc_dom %>% mutate(Variable = "BTC Dominance")
btc_fgi <- btc_fgi %>% mutate(Variable = "Fear & Greed Index")
btc_mcap <- btc_mcap %>% mutate(Variable = "Market Cap")

# Combinar todos los dataframes en uno solo
all_data_granger <- bind_rows(btc_syp, btc_nasdaq, btc_dom, btc_fgi, btc_mcap)

# Crear la gráfica con facet_grid
combined_plot_granger <- ggplot(all_data_granger, aes(x = date, y = p_value)) +
  geom_point(aes(color = p_value < 0.05)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("TRUE" = "#CDAD00", "FALSE" = "#528B8B")) +
  facet_grid(. ~ Variable , scales = "free_x") +
  labs(title = "Causalidad de Granger entre Bitcoin y Variables Independientes",
       subtitle = "Ventana Movil = 30",
       x = NULL,
       y = "Valor p",
       color = "Influencia Potencial -> (p < 0.05)") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(combined_plot_granger)




## CON ETH
# syp500
eth_syp <- calcular_granger_temporal(all_lcrends2, "syp500", "ETH", ventana = 30)
# btc
eth_btc <- calcular_granger_temporal(eth_w_lbtc, "BTC", "ETH", ventana = 30)
#dominancia btc
eth_dom <- calcular_granger_temporal(eth_w_ldom, "btc_dom", "ETH", ventana = 30)
#fear greed index
eth_fgi <- calcular_granger_temporal(eth_w_lfgi, "fg_index", "ETH", ventana = 30)
#market cap
eth_mcap <- calcular_granger_temporal(eth_w_lmcap, "market_cap", "ETH", ventana = 30)

eth_syp <- eth_syp %>% mutate(Variable = "S&P 500")
eth_btc <- eth_btc %>% mutate(Variable = "BTC")
eth_dom <- eth_dom %>% mutate(Variable = "BTC Dominance")
eth_fgi <- eth_fgi %>% mutate(Variable = "Fear & Greed Index")
eth_mcap <- eth_mcap %>% mutate(Variable = "Market Cap")

all_data_granger_eth <- bind_rows(eth_syp, eth_btc, eth_dom, eth_fgi, eth_mcap)

combined_plot_granger_eth <- ggplot(all_data_granger_eth, aes(x = date, y = p_value)) +
  geom_point(aes(color = p_value < 0.05)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("TRUE" = "#CDAD00", "FALSE" = "#528B8B")) +
  facet_grid(. ~ Variable , scales = "free_x") +
  labs(title = "Causalidad de Granger entre Ethereum y Variables Independientes",
       subtitle = "Ventana Movil = 30",
       x = NULL,
       y = "Valor p",
       color = "Influencia Potencial -> (p < 0.05)") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(combined_plot_granger_eth)

######MODELOS DE REGRESION######
  # MODELOS DE REGRESION 


# Crear gráfico combinado con facet_grid
all_data_lm <- bind_rows(
  mutate(all_lcrends2, variable = "S&P 500", x = syp500),
  mutate(all_lcrends2, variable = "NASDAQ", x = nasdaq),
  mutate(btc_w_ldom, variable = "BTC Dominance", x = btc_dom),
  mutate(btc_w_lfgi, variable = "Fear & Greed Index", x = fg_index),
  mutate(btc_w_lmcap, variable = "Market Cap", x = market_cap)
)

combined_plot_lm <- ggplot(all_data_lm, aes(x = x, y = BTC)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_grid(. ~ variable, scales = "free_x", ) +
  labs(title = "BTC vs Variables Independientes",
       x = "Variable Independiente",
       y = "BTC")+
  theme(plot.title = element_text(hjust = 0.5))
# para que las graficas con facet_grid aparezcan una sobre la otra de forma horizontal se usa 
print(combined_plot_lm)




# Definir las variables y sus dataframes correspondientes
variables <- list(
  syp500 = all_lcrends2,
  nasdaq = all_lcrends2,
  btc_dom = btc_w_ldom,
  fg_index = btc_w_lfgi,
  market_cap = btc_w_lmcap
)



# CON ETH
btc_model <- lm(ETH ~ BTC, data = eth_w_lbtc)
btc_results <- tidy(btc_model)


# Crear gráfico combinado con facet_grid
all_data_lm_eth <- bind_rows(
  mutate(eth_w_lbtc, variable = "BTC", x = BTC),
  mutate(eth_w_ldom, variable = "BTC Dominance", x = btc_dom),
  mutate(eth_w_lfgi, variable = "Fear & Greed Index", x = fg_index),
  mutate(eth_w_lmcap, variable = "Market Cap", x = market_cap)
)

combined_plot_lm_eth <- ggplot(all_data_lm_eth, aes(x = x, y = ETH)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_grid(. ~ variable, scales = "free_x", ) +
  labs(title = "ETH vs Variables Independientes",
       x = "Variable Independiente",
       y = "ETH")+
  theme(plot.title = element_text(hjust = 0.5))

print(combined_plot_lm_eth)

#####




get_regression_stats <- function(model) {
  summary_model <- summary(model)
  tibble(
    R_squared = summary_model$r.squared,
    Adj_R_squared = summary_model$adj.r.squared,
    F_statistic = summary_model$fstatistic[1],
    p_value = pf(summary_model$fstatistic[1], 
                 summary_model$fstatistic[2], 
                 summary_model$fstatistic[3], 
                 lower.tail = FALSE),
    std_error = summary_model$sigma
  )
}


# Definir las variables y sus dataframes correspondientes
variables <- list(
  syp500 = all_lcrends2,
  nasdaq = all_lcrends2,
  btc_dom = btc_w_ldom,
  fg_index = btc_w_lfgi,
  market_cap = btc_w_lmcap
)



# Regresión para syp500
syp500_model <- lm(BTC ~ syp500, data = all_lcrends2)
syp500_stats <- get_regression_stats(syp500_model)


# Regresión para nasdaq
nasdaq_model <- lm(BTC ~ nasdaq, data = all_lcrends2)
nasdaq_stats <- get_regression_stats(nasdaq_model)

# Regresión para BTC Dominance
btc_dom_model <- lm(BTC ~ btc_dom, data = btc_w_ldom)
btc_dom_stats <- get_regression_stats(btc_dom_model)

# Regresión para Fear & Greed Index
fg_index_model <- lm(BTC ~ fg_index, data = btc_w_lfgi)
fg_index_stats <- get_regression_stats(fg_index_model)

# Regresión para Market Cap
market_cap_model <- lm(BTC ~ market_cap, data = btc_w_lmcap)
market_cap_stats <- get_regression_stats(market_cap_model)

# Combinar resultados en una tabla
all_results3 <- bind_rows(
  mutate(syp500_stats, Variable = "S&P 500"),
  mutate(nasdaq_stats, Variable = "NASDAQ"),
  mutate(btc_dom_stats, Variable = "BTC Dominance"),
  mutate(fg_index_stats, Variable = "Fear & Greed Index"),
  mutate(market_cap_stats, Variable = "Market Cap")
) 

# Redondear todas las columnas numéricas a 4 decimales
all_results3 <- all_results3 %>% mutate(across(where(is.numeric), ~round(., 4)))

# Si 'p_value' es el nombre de la columna de valores p, redondear la columna p_value a 4 decimales
# y asegurarse de que los valores pequeños no se redondeen a 0.
all_results3 <- all_results3 %>% mutate(p_value = ifelse(p_value < 0.0001, format(p_value, scientific = TRUE), round(p_value, 4)))

# Imprimir la tabla de resultados
print(kable(all_results3, format = "markdown"))


  # CON ETH



# para btc
btc_model <- lm(ETH ~ BTC, data = eth_w_lbtc)
btc_stats <- get_regression_stats(btc_model)

# Regresión para BTC Dominance
eth_dom_model <- lm(ETH ~ btc_dom, data = eth_w_ldom)
eth_btc_dom_stats <- get_regression_stats(eth_dom_model)

# Regresión para Fear & Greed Index
eth_fg_index_model <- lm(ETH ~ fg_index, data = eth_w_lfgi)
eth_fg_index_stats <- get_regression_stats(eth_fg_index_model)

# Regresión para Market Cap
eth_market_cap_model <- lm(ETH ~ market_cap, data = eth_w_lmcap)
eth_market_cap_stats <- get_regression_stats(eth_market_cap_model)



# Combinar resultados en una tabla
all_results3_eth <- bind_rows(
  mutate(btc_stats, Variable = "BTC"),
  mutate(eth_btc_dom_stats, Variable = "BTC Dominance"),
  mutate(eth_fg_index_stats, Variable = "Fear & Greed Index"),
  mutate(eth_market_cap_stats, Variable = "Market Cap")
) 

# Redondear todas las columnas numéricas a 4 decimales
all_results3_eth <- all_results3_eth %>% mutate(across(where(is.numeric), ~round(., 4)))

# Si 'p_value' es el nombre de la columna de valores p, redondear la columna p_value a 4 decimales
# y asegurarse de que los valores pequeños no se redondeen a 0.
all_results3_eth <- all_results3_eth %>% mutate(p_value = ifelse(p_value < 0.0001, format(p_value, scientific = TRUE), round(p_value, 4)))

# Imprimir la tabla de resultados
print(kable(all_results3_eth, format = "markdown"))


######## TRANSMISION DE VOLATILIDAD DENTRO DE MERCADO CRIPTO ####










###################### MARKOWITZ##############################################
 # COSAS MARKOWITZ 

################
# DATAFRAMES DE RENDIMIENTOS ACUMULADOS A LARGO

# Horario
lcumrends_1h_melt <- melt(lcumrends_1h, id.vars = "date")











###
# Nota: esto dejo de ser sobre portafolio de markowitz


###
# ESTADÍSTICOS
# Media
medias_1h <- colMeans(lrends_1h[,-1], na.rm = TRUE)

# Desviacion estandar
dev_est_1h <- data.frame(lapply(lrends_1h[,-1], function(x) sd(x, na.rm = TRUE)))

#Varianza
var_1h <- data.frame(lapply(lrends_1h[,-1], function(x) var(x, na.rm = TRUE)))

# Correlación
corr_1h <- cor(lrends_1h[,-1], use = "complete.obs")

# Covarianza
covs_1h <- cov(lrends_1h[,-1], use = "complete.obs")
###
# Rango
#rangos_1h <- data.frame(lapply(lrends_1h[,-1], function(x) range(x, na.rm = TRUE)))

##
# Rangos chevishev
# 68%
chevi_68 <- data.frame(lapply(lrends_1h[,-1], function(x) c(mean(x, na.rm = TRUE) - 1 * sd(x, na.rm = TRUE), mean(x, na.rm = TRUE) + 1 * sd(x, na.rm = TRUE))))
# 95%
chevi_95 <- data.frame(lapply(lrends_1h[,-1], function(x) c(mean(x, na.rm = TRUE) - 2 * sd(x, na.rm = TRUE), mean(x, na.rm = TRUE) + 2 * sd(x, na.rm = TRUE))))
# 99%
chevi_99 <- data.frame(lapply(lrends_1h[,-1], function(x) c(mean(x, na.rm = TRUE) - 3 * sd(x, na.rm = TRUE), mean(x, na.rm = TRUE) + 3 * sd(x, na.rm = TRUE))))

chevi <- rbind(chevi_68,chevi_95,chevi_99)
# Cambiar el nombre de las filas 
rownames(chevi) <- c("-1dev%","+1dev%","-2dev%","+2dev%","-3dev%", "+3dev%")
# Summary
summary(lrends_1h[,-1])

# lrends eliminando datos a mas de 3 desviaciones estandard (outliers)
lrends_1h_norm <- data.frame(date = lrends_1h$date,lapply(lrends_1h[,-1], function(x) ifelse(x > mean(x, na.rm = TRUE) + 3 * sd(x, na.rm = TRUE), NA, ifelse(x < mean(x, na.rm = TRUE) - 3 * sd(x, na.rm = TRUE), NA, x))))
lrends_1h_norm <- lrends_1h_norm %>% drop_na()

# cambiar de columna a filas y filas a columnas de chevi
chevi <- as.data.frame(t(chevi))
# agregar una nueva fila al inicio de todo con los promedios de las columnas
chevi <- rbind(chevi, colMeans(chevi, na.rm = TRUE))
# cambiar el nombre de la nueva fila y moverla al inicio






###
# ITERACIONES PARA PORTAFOLIOS

# Funcion para iteraciones para portafolios markowitz con peso minimo, suma = 100% o 1
comb_portfolio_mark <- function(assets, ncomb, wmin) { # REQUIERE GTOOLS
  portfolios <- list()
  wmax <- 1 - (length(assets)*wmin) # Obtener el maximo peso posible de acuerdo a numero de activos y su peso
  
  for (i in 1:ncomb) {
    # Obtener las iteraciones con una distribucion de Dirichlet y repetirlo por el numero de actvios
    w <- rdirichlet(1, rep(1, length(assets)))[1, ] 
    # Limitar los pesos entre el minimo y maximo para asegurar suma 1
    w <- pmin(pmax(w, wmin), wmax) 
    # Normalizar los pesos para que sumen 1 después del clipping
    sum_w <- sum(w)
    w <- w / sum_w
    portfolios[[i]] <- w
  }
  portfolios_df <- as.data.frame(do.call(rbind, portfolios))
  colnames(portfolios_df) <- assets
  
  return(portfolios_df)
}






# METRICAS DE PORTAFOLIOS (Rendimiento esperado, Varianza, Riesgo, Ratio de Sharpe)

# Rendimientos esperdos del portafolio
rend_esp <- function(portfolios, data_rends) {
  rend_esp <- numeric(nrow(portfolios))
  media_rends <- colMeans(rends)
  
  for (i in 1:nrow(portfolios)) {
    row <- as.numeric(portfolios[i, ])
    
    rend_esp[i] <- sum(row * media_rends)
  }
  rend_esp <- data.frame(rend_esp=rend_esp)
  return(rend_esp)
}



# Varianza del portafolio
var_port <- function(portfolios, data_rends) {
  cov_rends <- cov(rends)
  var_port <- numeric(nrow(portfolios))
  
  for (i in 1:nrow(portfolios)) {
    row <- as.numeric(portfolios[i, ])
    
    var_port[i] <- t(row) %*% cov_rends %*% row
  }
  var_port <- data.frame(var_port=var_port)
  return(var_port)
}



# Riesgo del portafolio
risk_port <- function(portfolios) {
  risk <- numeric(nrow(portfolios))
  cov_rends <- cov(rends)
  var_port <- numeric(nrow(portfolios))
  
  for (i in 1:nrow(portfolios)) {
    row <- as.numeric(portfolios[i, ])
    
    var_port[i] <- t(row) %*% cov_rends %*% row
    risk <- numeric(nrow(portfolios))
    
  }
  risk <- data.frame(risk=risk)
  return(risk)
}



# Ratio sharpe
ratio_sharpe <- function(portfolios, risk_free_rate = 0.1) {
  ratio_sharpe <- numeric(nrow(portfolios))
  rend_esp <- rend_esp(portfolios)
  risk <- risk_port(portfolios)
  for (i in 1:nrow(portfolios)) {
    ratio_sharpe[i] <- (rend_esp[i] - risk_free_rate) / risk[i]
  }
  ratio_sharpe <- data.frame(ratio_sharpe=ratio_sharpe)
  return(ratio_sharpe)
}



# Funcion para metricas
portfolio_metrics <- function(portfolios, rends, risk_free_rate = 0.1) {
  media_rends <- colMeans(rends) 
  cov_rends <- cov(rends)
  
  rend_esp <- numeric(nrow(portfolios))
  risk <- numeric(nrow(portfolios))
  var_port <- numeric(nrow(portfolios))
  ratio_sharpe <- numeric(nrow(portfolios))
  
  for (i in 1:nrow(portfolios)) {
    row <- as.numeric(portfolios[i, ])
    
    rend_esp[i] <- sum(row * media_rends)
    var_port[i] <- t(row) %*% cov_rends %*% row
    risk[i] <- sqrt(var_port[i])
    ratio_sharpe[i] <- (rend_esp[i] - risk_free_rate) / risk[i]
  }
  portfolios <- portfolios
  portfolios$rend_esp <- rend_esp
  portfolios$var_port <- var_port
  portfolios$risk <- risk
  portfolios$ratio_sharpe <- ratio_sharpe
  return(portfolios)
}



# Funcion para Portafolio, iteraciones y metricas
portfolio_markowitz <- function(data , nport, wmin, risk_free=0.1) {
  assets <- colnames(data)
  portfolios <- comb_portfolio_mark(assets, nport, wmin)
  metrics <- portfolio_metrics(portfolios, data, risk_free)
  return(metrics)
}










################
# EXPORTAR DATOS A CSV o EXCEL


ggplot(lrends_1h, aes(x = BTC)) +
  geom_histogram(bins = 500, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Rendimientos Logarítmicos Horarios",
       x = "Rendimientos Logarítmicos",
       y = "Frecuencia") +
  theme_minimal()


# Histograma de rendimientos logarítmicos horarios
plot2 <- ggplot(melt(lrends_1h_norm, id.vars = "date"), aes(x = value)) +
  geom_histogram(bins = 500, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Rendimientos Logarítmicos Horarios",
       x = "Rendimientos Logarítmicos",
       y = "Frecuencia") +
  # añadir lineas rectas de desviaciones estandar con tabla de chevi, que sean las columnas de 1:6 y usando la fila llamada 50
  geom_vline(xintercept = chevi[50,1], linetype = "dashed", color = "red") +
  geom_vline(xintercept = chevi[50,2], linetype = "dashed", color = "red") +
  geom_vline(xintercept = chevi[50,3], linetype = "dashed", color = "blue") +
  geom_vline(xintercept = chevi[50,4], linetype = "dashed", color = "blue") +
  geom_vline(xintercept = chevi[50,5], linetype = "dashed", color = "yellow") +
  geom_vline(xintercept = chevi[50,6], linetype = "dashed", color = "yellow") +
  theme_minimal()
ggplotly(plot2)


# Crear la gráfica de líneas
ggplot(lcumrends_1h_melt, aes(x = date, y = value, color = variable)) +
  geom_line() +
  labs(title = "Rendimientos Acumulados de los Activos",
       x = "Fecha",
       y = "Rendimientos Acumulados",
       color = "Activos") +
  theme_minimal()


data_close_std <- data_close %>%
  mutate_if(is.numeric, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))



data_close_melt <- melt(data_close_std, id.vars = "date")
plot1 <- ggplot(data_close_melt, aes(x = date, y = value)) +
  geom_line() +
  labs(title = "Precio de cierre de los Activos",
       x = "Fecha",
       y = "Precio de cierre",
       color = "Activos") +
  theme_minimal()
ggplotly(plot1)
