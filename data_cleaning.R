# Bibliotecas
library(tidyverse)
library(lubridate)

# Dados
data <- read_file('data/_chat.txt')

# Limpeza de Dados
data <- data %>% 
  str_replace_all('\n', ' ') %>% 
  str_replace_all('\r', ' ') %>% 
  str_split('.(?=\\[[0-9]{2}\\/[0-9]{2}\\/[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}\\])') %>% 
  setNames('mensagem') %>% 
  bind_rows() %>% 
  mutate()
