library(rvest)
library(tidyverse)

screeners <- list(
  all_index = "markets/indices/quotes-all/",
  major_index = "markets/indices/quotes-major/",
  us_index = "markets/indices/quotes-us/",
  snp_index = "markets/indices/quotes-snp/",
  currency_index = "markets/indices/quotes-currency/",
  all_stocks_US = "markets/stocks-usa/market-movers-all-stocks/"
)

# Selección del screener
screener_selected = "all_stocks_US"
base_url = "https://en.tradingview.com/"
url <- paste0(base_url, screeners[[screener_selected]])

# Funcion de busqueda
get_response_data <- function(url) {
  page <- read_html(url)
  
  table <- page %>% 
    html_node("table") %>% 
    html_table()
  
  if (is.null(table)) {
    stop("No se encontró la tabla en la página")
  }
  
  # Extraer ticker y nombre
  symbols <- page %>% 
    html_nodes("table tr td:first-child")
  
  tickers <- symbols %>% 
    html_nodes("a") %>% 
    html_text()
  
  names <- symbols %>% 
    html_nodes("sup") %>% 
    html_text()
  
  # Combinar los datos
  result <- table %>%
    select(-1) %>%  # Eliminar la primera columna (Symbol)
    mutate(Ticker = tickers, Name = names) %>%
    select(Ticker, Name, everything())
  as.data.frame(result)
  return(result)
}
t2 <- get_response_data(url)

# funcion de busqueda de datos de ticker o nombre especifico
get_data <- function(data_table ,ticker = NULL, nombre = NULL) {
  if (is.null(ticker) && is.null(nombre)) {
    stop("Debe especificar al menos un ticker o nombre")
  }
  
  if (!is.null(ticker)) {
    data_asset <- data_table %>% 
      filter(Ticker == ticker)
  }
  
  if (!is.null(nombre)) {
    data_asset <- data_table %>% 
      filter(Nombre == nombre)
  }
  
  return(data_asset)
}

get_data( t2,ticker = "A")


