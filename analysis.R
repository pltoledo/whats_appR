# Bibliotecas
library(tidyverse)
library(lubridate)
library(stringdist)
library(UnidecodeR)
extrafont::loadfonts(device = 'win', T)

# Dados
data <- readRDS('data/clean_data.RDS')

# Ajusta o tema dos gráficos
theme_set(hrbrthemes::theme_ipsum_rc())
theme_update(plot.title.position = "plot",
                   plot.title = element_text(size = 25),
                   plot.subtitle = element_text(size = 16),
                   axis.title.x = element_text(size = 15, 
                                               margin = margin(r = 10,  t = 15),
                                               face = 'bold'),
                   axis.title.y= element_text(size = 15, 
                                               margin = margin(r = 10,  t = 15),
                                               face = 'bold'))

# Dia da Semana

df_doy <- data %>% 
  slice(-1) %>% 
  mutate(doy = str_to_title(wday(data, T, F))) %>% 
  mutate(doy = fct_rev(fct_infreq(doy, ordered = T))) %>% 
  count(doy)

df_doy %>% 
  ggplot(aes(x = n, y = reorder(doy, n), 
             fill = doy)) +
  geom_col() +
  labs(x = 'N° de Mensagens', y = 'Dia da Semana', 
       title = 'Qual o dia que mais conversamos?',
       subtitle = 'Quantidade de mensagens enviadas por dia da semana') + 
  scale_x_continuous(labels = scales::label_number()) +
  scale_fill_viridis_d( direction = -1, begin = 0.25, end = .75) +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

ggsave(filename = 'doy.pdf',
       device = cairo_pdf, width = 18, height = 11)

# Dia da Semana

data %>% 
  mutate(palavras = str_split(mensagem, ' ')) %>% 
  unchop(palavras) %>% 
  mutate(across(palavras, ~ str_trim(.x)),
         across(palavras, ~ str_to_lower(.x)),
         across(palavras, ~ stringi::stri_trans_general(.x, "Latin-ASCII")),
         across(palavras, ~ str_remove_all(.x, "[.!?\\-,\"\']")),
         len_words = str_length(palavras)) %>% 
  filter(between(len_words, 3, 8)) %>% 
  select(autor, palavras) %>% 
  mutate(pedro = stringdist(palavras, 'pedro', method = 'jw', p = 0.1)) %>% 
  filter(pedro <= 0.1) %>% 
  distinct(palavras)


df_nomes %>% 
  ggplot(aes(x = n, y = autor, fill = nome)) +
  geom_col(position = 'dodge') +
  labs(x = 'Menções', y = 'Nome',
       title = 'Quem falou mais o nome de quem?',
       subtitle = 'Quantidade de vezes que o nome foi mencionado por cada um',
       fill = NULL) +
  theme(axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())



df_nomes %>% 
  ggplot(aes(x = perc, y = nome, fill = autor)) +
  geom_col() +
  labs(x = 'Menções do Nome', 
       title = 'Quem falou mais o nome de quem?',
       fill = 'Autor') +
  guides(fill = guide_legend(title.position = 'top', 
                             title.hjust = 0.5,
                             title.theme = element_text(size = 15))) +
  scale_x_continuous(labels = scales::label_percent()) +
  theme(legend.position = 'top',
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())