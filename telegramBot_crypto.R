
library(telegram)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(lubridate)
library(tidyr)

# Criar um bot do telegram com a ajuda do BotFather.
bot <- TGBot$new(token = bot_token('alerta_cryptoBot'))

bot$getMe()

# Obtendo as mensagens enviadas no Telegram
bot$getUpdates()

# Encontrando o chat_id das mensagens 
msgs <- bot$getUpdates()
msgs$message$chat$id[1]

# Configurando o chat_id como padrão
bot$set_default_chat_id(514029595)


# API BINANCE -------------------------------------------------------------
safe_fromJSON <- safely(fromJSON, as.numeric(NA)) 
query <- safe_fromJSON("https://api.binance.com/api/v3/ticker/24hr")

cryptos <- c("BTCUSDT", "ETHBTC", "ETHUSDT", "LTCBTC", "LTCUSDT", "NANOBTC", "NANOUSDT")

nova_consulta <- query$result %>% 
  as_tibble() %>% mutate(timestamp = lubridate::now()) %>% 
  filter(symbol %in% cryptos) %>%
  select(1,3,6,13,14,15,22)


# LOOP INFINITO PARA ACOMPANHAR OS PREÇOS ---------------------------------

# inicializa o historico.RData
historico <- nova_consulta
save(historico, file = "historico.RData")

alert_crypto <- function(frequencia = 30) {
  load("historico.RData")
  
  while(TRUE) {
    # Extrai uma query com todas as cryptos disponíveis na Binance 
    query <- safe_fromJSON("https://api.binance.com/api/v3/ticker/24hr")
    
    # verifica se a API retorna uma lista 
    if("list" %in% class(query$result)) {
      nova_consulta <- query$result %>% 
        as.tibble %>%
        mutate(timestamp = lubridate::now()) %>%  
        select(1,3,24)
      
      # Suporte 1: faixa dos 6600
      s1 <- 6700
      if(nova_consulta$lastPrice[[3]] < s1) {
        bot$sendMessage(paste('Bitcoin (BTC) went below', s1, 'USD. This could be a good entry point.'))
        bot$sendMessage(nova_consulta$lastPrice[[3]])
      }
      
      # Resistência 1: faixa dos 7500
      r1 <- 7300
      if(nova_consulta$lastPrice[[3]] > r1) {
        bot$sendMessage(paste('Bitcoin (BTC) went above', r1, 'USD. The price hits a key resistance level.'))
        bot$sendMessage(nova_consulta$lastPrice[[3]])
      }
      
      # Suporte 2: faixa dos 5700
      s2 <- 5800
      if(nova_consulta$lastPrice[[3]] < s2) {
        bot$sendMessage(paste('Bitcoin (BTC) went below', s2, 'USD. Buy!!'))
        bot$sendMessage(nova_consulta$lastPrice[[3]])
      }
      
      historico <- bind_rows(historico, nova_consulta)
      save(historico, file = "historico.RData")
      
    } 
    Sys.sleep(frequencia)
  }
}
  






  