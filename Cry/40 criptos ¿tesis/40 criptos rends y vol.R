library(readr)
library(ggplot2)
library(tidyverse)
library(xts)
library(dplyr)
library(reshape2)
#################
# Lectura de datos
data_raw <- data.frame(read_csv("1Weas/Github/My-Rep/Cry/40 criptos ¿tesis/crypto.csv/crypto.csv"))

# Filtrar los datos entre 31 de diciembre de 2020 a las 23:00 y la fecha más reciente
data_raw$date <- as.POSIXct(data_raw$date, format="%d/%m/%Y %H:%M") # cosa rara se establece formato al revés
start_date <- "2020-12-31 23:00" # Fecha filtro
data <- filter(data_raw, date>= start_date)

# Comprobar de formato fecha
year(data$date[1])
month(data$date[1])
day(data$date[1])

# Convertir la columna de fechas a index 
data <- data %>% column_to_rownames(var = "date") # Establecer date como index


################
# Seleccionar columnas de precios de cierre y volumen como nuevas df
data_close <- data %>% select(contains("close"))
data_vol <- data %>% select(contains("volume"))

# Renombrar columnas eliminando el sufijo "_close y _volume" y crear variable con nombres de activos
column_names <- colnames(data_close)
assets <- gsub("_close", "", column_names)

    colnames(data_close) <- assets
    colnames(data_vol) <- assets


################   
# Función para calcular rendimientos logarítmicos
calc_log_returns <- function(data) {
  log_returns <- data.frame(lapply(data, function(x) diff(log(x))))
  rownames(log_returns) <- rownames(data)[-1] # Nuevo index y eliminar fila vacía
  
  return(log_returns)
}
# Rendimientos logarítmicos horarios
lrends_1h <- calc_log_returns(data_close)

# Rendimientos acumulados
lcumrends_1h <- cumsum(lrends_1h)
lcumrends_1h <- lcumrends_1h %>% rownames_to_column(var = "date") # Establecer index como columna date


# Convertir la columna 'date' a formato fecha
lcumrends_1h$date <- as.POSIXct(lcumrends_1h$date, format="%Y-%m-%d %H:%M")

plot(lcumrends_1h$date, lcumrends_1h$CRVUSDT, type = "l", col = "blue", xlab = "Fecha", ylab = "Rendimientos Acumulados", main = "Rendimientos Acumulados de CRVUSDT")

ggplot(lcumrends_1h, aes(x=date,y=CRVUSDT))+
  geom_line()



assets

# Crear la gráfica de líneas
ggplot(lcumrends_1h_melt, aes(x = date, y = value, color = variable)) +
  geom_line() +
  labs(title = "Rendimientos Acumulados de los Activos",
       x = "Fecha",
       y = "Rendimientos Acumulados",
       color = "Activos") +
  theme_minimal()













