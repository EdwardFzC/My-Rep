

# FILTRAR LAS DE MAYOR VOLUMEN?




################
library(readr)
library(plotly)
library(xts)
library(reshape2)
library(tidyverse)
library(gtools)
library(GGally)
library(quantmod)
library(lmtest)



################
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




###############
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




################
    # PREPARACIÓN DE DATOS

# Seleccionar columnas de precios de cierre y volumen como nuevas df
data_price_h <- data %>% select(!contains("volume"))
data_close_h <- data %>% select(date, contains("close"))
data_vol_h <- data %>% select(date, contains("volume"))



# Obtener el nombre de los activos usados
assets <- get_assets(data_close_h, "USDT_close")
# Cambiar el nombre de columnas en Data frames para coincidir con los activos

colnames(data_close_h) <- get_assets(data_close_h, "USDT_close")
colnames(data_vol_h) <- get_assets(data_vol_h,"USDT_volume")

# Crear un dataframe con los volúmenes en USD
data_vol_usd_h <- data.frame(date=data_vol_h$date, mapply(function(vol, close) vol * close, data_vol_h[-1], data_close_h[-1]))




################
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
#####

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





################
    # UNIR PRECIOS DIARIOS DE CRIPTOS CON INDICES
data_close_d <- change_timeframe(data_close_h, "day") %>%  as.xts()

data_close_all <- merge(as.xts(syp500_close), as.xts(nasdaq_close), data_close_d,  how = "inner", all = F)
data_close_all2 <- merge(syp500_close, nasdaq_close, data_close_d,  how = "inner", all = T)


assets_all <- get_assets(data_close_all)

# Hacer que las series sean estacionarias en data_close_all
close_all_log <- log(as.data.frame(data_close_all))




################
    # Granger Causality
data_call_ts <- data_close_all[,1:4] %>% ts()
btc_ts <- data_close_all[, "BTC"] %>% ts()
syp_ts <- data_close_all[, "syp500"] %>% ts()
nasdaq_ts <- data_close_all[, "nasdaq"] %>% ts()

lags <- 1:20  # Rango de lags
results <- data.frame(Lag = lags, P_Value = numeric(length(lags)))

for (lag in lags) {
  result <- grangertest(data_close_all$syp500 ~ data_close_all$BTC, order = lag)
  results$P_Value[lag] <- result$Pr[2]
}


a2 <-  calc_log_returns(data_close_all)
a3 <- calc_log_returns(data_close_h)
################
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











################
    # RENDIMIENTOS MULIPLE TIME FRAME

data_day <- change_timeframe(data_close, "day")
#lrends_d <- calc_log_returns(data_day)

vol_day1 <- change_timeframe(data_vol_usd, "day", sum=TRUE)
# Rendimientos logarítmicos horarios
lrends_1h <- calc_log_returns(data_close)






################
  #RENDIMIENTOS ACUMULADOS

# horarios
lcumrends_1h <- calc_acum_returns(lrends_1h)






################
    # DATAFRAMES DE RENDIMIENTOS ACUMULADOS A LARGO

# Horario
lcumrends_1h_melt <- melt(lcumrends_1h, id.vars = "date")





################
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
#####
# Rango
#rangos_1h <- data.frame(lapply(lrends_1h[,-1], function(x) range(x, na.rm = TRUE)))

#####
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












################
    # VOLATILIDAD

# Función para calcular la volatilidad






################
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





################
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
    # OPTIMIZACION DE PORTAFOLIOS

# Portafolio de pesos iguales

# Portafolio de minima varianza Markowitz

# Portafolio tangente



################
    # Drowdown



################
    # Evolucion de inversion en periodo, comparado con el activo de mayor y menor rentabilidad final de los activos



################
    # GRAFICAS

# Graficas de correlacion datos 1H
corr <- data.frame(cor(data_close[-1], use = "complete.obs"))
corr$assets <- assets[-1]

ggpairs(data=corr, 
        columns = 1:5,
        lower = list(continuous = "smooth"),
        aes(fill = 50))+
  labs(title = "Correlación de Precios de Cierre",
       x = "Activos",
       y = "Activos")
data_close_melted <- melt(data_close, id.vars = "date")

iris <- iris
ggpairs(data_close,
        columns = 2:5,
        aes(alpha=0.2))
# para cambiar el color en funcion de los assets se puede usar la funcion scale_fill_manual
# por ejemplo scale_fill_manual(values = c("BTC" = "red", "ETH" = "blue", "BNB" = "green", "ADA" = "yellow", "XRP" = "purple", "DOGE" = "orange", "DOT" = "brown", "UNI" = "pink", "BCH" = "grey", "LTC" = "black")

corr_log_rends <- cor(calc_log_returns(data_close)[-1], use = "complete.obs")
ggpairs(corr_log_rends, 
        columns = 1:10,
        lower = list(continuous = "smooth"))+
  labs(title = "Correlación de Rendimientos Logarítmicos Horarios",
       x = "Activos",
       y = "Activos")+
  theme_minimal()

corr_rends <- cor(calc_pct_returns(data_close)[-1], use = "complete.obs")
ggpairs(corr_rends, 
        columns = 1:10,
        lower = list(continuous = "smooth"))+
  labs(title = "Correlación de Rendimientos Porcentuales Horarias",
       x = "Activos",
       y = "Activos")+
  theme_minimal()


# Funcion de graficacion scatter

# Funcion de graficacion Drowdown

# Funcion de graficacion de rendimientos acumulados


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










