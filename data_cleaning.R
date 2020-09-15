# Bibliotecas
library(tidyverse)
library(lubridate)

# Dados
raw <- read_file('data/_chat.txt')

# Limpeza de Dados
data <- raw %>% 
  str_replace_all('\n', ' ') %>% 
  str_replace_all('\r', ' ') %>% 
  str_split('.(?=\\[[0-9]{2}\\/[0-9]{2}\\/[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}\\])') %>% 
  setNames('mensagem') %>% 
  bind_rows() %>% 
  mutate(data = str_extract(mensagem,
                            '\\[[0-9]{2}\\/[0-9]{2}\\/[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}\\]'),
         autor = str_extract(mensagem, 'Juju|Pedro Toledo'),
         mensagem = str_extract(mensagem, '(?<=Juju:|Pedro Toledo:).+')) %>% 
  mutate(across(everything(), ~str_trim(.x)),
         hora = str_extract(data, '[0-9]{2}:[0-9]{2}:[0-9]{2}'),
         data = dmy(str_extract(data, '[0-9]{2}\\/[0-9]{2}\\/[0-9]{4}'))) %>% 
  select(autor, data, hora, mensagem)

saveRDS(data, 'data/clean_data.RDS')