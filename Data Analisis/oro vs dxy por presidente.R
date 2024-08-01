library(ggplot2)
library(quantmod)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)

# Download Gold and Dollar index data
dxy <- getSymbols("DX-Y.NYB", src = "yahoo", from = "2000-01-20", auto.assign = F)
gold <- getSymbols("GC=F", src = "yahoo", from = "2000-01-20", auto.assign = F)

data <- merge(Cl(dxy), Cl(gold), all = F)

colnames(data) <- c("DXY", "Gold")
data <- data[!is.na(data$DXY) & !is.na(data$Gold),]

scale_factor <- 20

xts_to_dataframe <- function(data) {
  data <- as.data.frame(data) %>% rownames_to_column(var="date")
  data$date <- as.Date(data$date, format = "%Y-%m-%d")
  return(data)
}

data1 <- xts_to_dataframe(data)

# Datos de los presidentes
presidents <- data.frame(
  President = c("Bush", "Obama", "Trump", "Biden"),
  Start = as.Date(c("2001-01-20", "2009-01-20", "2017-01-20", "2021-01-20")),
  End = as.Date(c("2009-01-19", "2017-01-19", "2021-01-19", "2024-07-26")),
  FillColor = c("#BAFFC9", "#BAE1FF", "#FFFFBA", "#FFD8B1")
)

# Filtro para obtener el dato del oro y dxy del inicio y del final para cada ciclo presidencial
# Función para obtener el valor más cercano a una fecha
get_closest_value <- function(date, data) {
  closest_date <- data[which.min(abs(difftime(index(data), date))),]
  return(as.numeric(closest_date))
}
?wich.min
?difftime

# Añadir valores de inicio y fin para DXY y Gold
presidents$DXY_Start <- sapply(presidents$Start, function(date) get_closest_value(date, data[,"DXY"]))
presidents$DXY_End <- sapply(presidents$End, function(date) get_closest_value(date, data[,"DXY"]))
presidents$Gold_Start <- sapply(presidents$Start, function(date) get_closest_value(date, data[,"Gold"]))
presidents$Gold_End <- sapply(presidents$End, function(date) get_closest_value(date, data[,"Gold"]))

# Calcular cambios porcentuales
presidents$DXY_Change <- (presidents$DXY_End - presidents$DXY_Start) / presidents$DXY_Start * 100
presidents$Gold_Change <- (presidents$Gold_End - presidents$Gold_Start) / presidents$Gold_Start * 100

# Mostrar el dataframe
print(presidents)





# Crear la gráfica
ggplot() +
  # Áreas de fondo para los presidentes
  geom_rect(data = presidents, aes(xmin = Start, xmax = End, ymin = -Inf, ymax = Inf, fill = President), alpha = 0.5, show.legend = FALSE) +
  # Líneas de datos
  geom_line(data = data, aes(x = index(data), y = Gold, color = "Gold Price"), linewidth = 0.8) +
  geom_line(data = data, aes(x = index(data), y = DXY * scale_factor, color = "Dollar Index"), linewidth = 0.8) +
  # Escalas del eje Y
  scale_y_continuous(
    name = "Price of Gold (USD)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Dollar Index (DXY)"),
    labels = dollar_format(prefix = "$")
  ) +
  # Definir colores y etiquetas de la leyenda
  scale_color_manual(
    values = c("Gold Price" = "#B03060", "Dollar Index" = "#4682B4"),
    name = "Indicators"
  ) +
  scale_fill_manual(values = setNames(presidents$FillColor, presidents$President), guide = "none") +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  # Etiquetas y tema
  labs(
    title = "Gold Price vs Dollar Index (2000-2024)",
    subtitle = "Comparison across U.S. Presidential Terms",
    x = "Year",
    caption = "Data source: Yahoo Finance"
  ) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    plot.title = element_text(face = "bold", size = 20, color = "#333333"),
    plot.subtitle = element_text(size = 14, color = "#666666"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#F0F0F0"),
    plot.background = element_rect(fill = "#FFFFFF")
  ) +
  annotate("text", x = as.Date("2004-06-01"), y = max(data$Gold), 
           label = "Bush", hjust = 0, vjust = 1, size = 5, color = "darkslategray", fontface = "bold")+
  annotate("text", x = as.Date("2012-06-01"), y = max(data$Gold), 
           label = "Obama", hjust = 0, vjust = 1, size = 5, color = "darkslategray", fontface = "bold")+
  annotate("text", x = as.Date("2018-06-01"), y = max(data$Gold), 
           label = "Trump", hjust = 0, vjust = 1, size = 5, color = "darkslategray", fontface = "bold")+
  annotate("text", x = as.Date("2022-01-01"), y = max(data$Gold), 
           label = "Biden", hjust = 0, vjust = 1, size = 5, color = "darkslategray", fontface = "bold")+
  geom_text(data = presidents, aes(x = Start + (End - Start)/2, y = Inf, 
                                   label = sprintf("DXY: %.1f%%\nGold: %.1f%%", 
                                                   DXY_Change, Gold_Change)),
            vjust = 1, size = 2.5, fontface = "bold")
