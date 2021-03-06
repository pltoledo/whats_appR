# Setup -------------------------------------------------------------------
# Bibliotecas
library(tidyverse)
library(lubridate)
library(patchwork)
library(stringdist)
library(rvest)
library(glue)
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
                                               margin = margin(r = 15,  t = 15),
                                               face = 'bold'))

scale_autor <- scale_fill_manual(values = c('Pedro' = '#3B528BFF', 'Juju' = '#5DC863FF'))


# Quando mais conversamos? ------------------------------------------------
## Dia da Semana
df_dow <- data %>% 
  slice(-1) %>% 
  mutate(dow = str_to_title(wday(data, T, F))) %>% 
  mutate(dow = fct_rev(fct_infreq(dow, ordered = T))) %>% 
  count(data, dow) %>% 
  group_by(dow) %>% 
  summarise(mean = mean(n))

plot_dow <-df_dow %>% 
  ggplot(aes(x = mean, y = reorder(dow, mean), 
             fill = dow)) +
  geom_col() +
  labs(x = 'N° de Mensagens', y = 'Dia da Semana') + 
  scale_x_continuous(labels = scales::label_number()) +
  scale_fill_viridis_d( direction = -1, begin = 0.25, end = .75) +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

## Dia do Mês
df_dom <- data %>% 
  slice(-1) %>% 
  mutate(dom = str_to_title(mday(data))) %>% 
  mutate(dom = fct_inseq(dom, ordered = T)) %>% 
  count(data, dom) %>% 
  group_by(dom) %>% 
  summarise(mean = mean(n))

plot_dom <- df_dom %>% 
  ggplot(aes(y = mean, x = dom, 
             fill = dom)) +
  geom_col() +
  labs(y = 'N° de Mensagens', x = 'Dia do Mês') + 
  scale_y_continuous(labels = scales::label_number()) +
  scale_fill_viridis_d( direction = -1, begin = 0.25, end = .75) +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

## Horas do dia
df_horas <- data %>% 
  mutate(hora = factor(hour(hms(hora)))) %>% 
  mutate(hora = fct_rev(fct_inseq(hora))) %>% 
  count(data, hora) %>% 
  group_by(hora) %>% 
  summarise(mean = mean(n))

plot_horas <- df_horas %>% 
  ggplot(aes(y = hora, x = mean, color = hora)) +
  geom_point(size = 3) +
  scale_color_viridis_d( direction = -1, begin = 0.25, end = .75) +
  labs(y = 'Hora', x = 'N° Mensagens') +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

## Plot Final
(plot_dow + plot_horas)/plot_dom + 
  plot_annotation(title = 'Quando mais conversamos?',
                  subtitle = 'Média de mensagens enviadas por dia da semana, do mês e em cada hora do dia')

ggsave(filename = 'plots/quando.pdf',
       device = cairo_pdf, width = 18, height = 15)

# Nomes -------------------------------------------------------------------
df_nomes <- data %>% 
  mutate(palavras = str_split(mensagem, ' ')) %>% 
  unchop(palavras) %>% 
  mutate(across(palavras, ~ str_trim(.x)),
         across(palavras, ~ str_to_lower(.x)),
         across(palavras, ~ stringi::stri_trans_general(.x, "Latin-ASCII")),
         across(palavras, ~ str_remove_all(.x, "[.!?\\-,\"\'#_\\)\\(]")),
         len_words = str_length(palavras)) %>% 
  filter(between(len_words, 3, 8)) %>% 
  select(autor, palavras, mensagem) %>% 
  mutate(lv_p = stringdist(palavras, 'pedro', method = 'lv'),
         lv_juju = stringdist(palavras, 'juju', method = 'lv'),
         lv_j = stringdist(palavras, 'juliana', method = 'lv')) %>% 
  filter(lv_j <= 1 | lv_p <= 1 | lv_juju <= 1) %>%
  mutate(autor = case_when(autor == 'Pedro Toledo' ~ 'Pedro', T ~ autor),
         mencao = case_when(lv_p <= 1 & autor == 'Juju' ~ 'Pedro',
                            (lv_j <= 1 | lv_juju <= 1) & autor == 'Pedro' ~ 'Juju')) %>%
  filter(!is.na(mencao)) %>% 
  count(autor, mencao)


df_nomes %>% 
  ggplot(aes(x = n, y = reorder(autor, n), fill = autor)) +
  geom_col() +
  labs(x = 'Menções', y = NULL,
       title = 'Quem falou mais o nome do outro?',
       subtitle = 'Quantidade de mensagens em que o nome do outro foi mencionado',
       fill = NULL) +
  scale_autor +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

ggsave(filename = 'plots/nomes.pdf',
       device = cairo_pdf, width = 18, height = 11)


# Emojis ------------------------------------------------------------------
emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    read_html() %>%
    html_nodes("tr td a") %>%
    .[1] %>%
    html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    read_html() %>%
    html_node('div[class="vendor-image"] img') %>%
    html_attr("src")
}

link_to_img <- function(x, size = 25) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

df_emoji <- data %>% 
  filter(emo::ji_detect(mensagem)) %>% 
  mutate(emoji = emo::ji_extract_all(mensagem)) %>% 
  unchop(emoji) %>%
  distinct() %>% 
  group_by(autor, emoji) %>% 
  summarise(n = n(), .groups = 'drop_last') %>% 
  slice_max(order_by = n, n = 10) %>%
  ungroup() %>%
  mutate(url = map_chr(emoji, slowly(~emoji_to_link(.x), rate_delay(1))),
         label = link_to_img(url))

emoji_juju <- df_emoji %>%
  filter(autor == 'Juju') %>% 
  ggplot(aes(x = n, y = reorder(label, n))) + 
  geom_col(fill = "#5DC863FF") +
  geom_text(aes(label = n), nudge_x = 20) +
  labs(x = NULL, y = NULL,
       title = 'Juliana') +
  scale_x_continuous(limits = c(0, 750))+
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5,
                                  face = 'plain'),
        plot.title.position = 'panel',
        axis.text.y = ggtext::element_markdown(),
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())

emoji_pedro <- df_emoji %>%
  filter(autor == 'Pedro Toledo') %>% 
  ggplot(aes(x = -n, y = reorder(label, n))) + 
  geom_col(fill = "#3B528BFF") +
  geom_text(aes(label = n), nudge_x = -20) +
  labs(x = NULL, y = NULL,
       title = 'Pedro') +
  scale_y_discrete(position = "right") +
  scale_x_continuous(limits = c(-750, 0), labels = function(x) -x) +
  theme(plot.title = element_text(size = 20,
                                  hjust = 0.5, 
                                  face = 'plain'),
        plot.title.position = 'panel',
        axis.text.y.right = ggtext::element_markdown(),
        axis.text.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())

emoji_pedro + 
  emoji_juju + 
  plot_annotation(title = 'Qual o emoji mais usado?', 
                  subtitle = 'Top 10 emojis mais enviados por cada um')

ggsave(filename = 'plots/emoji.pdf',
       device = cairo_pdf, width = 18, height = 11)


# Estamos conversnado menos? ----------------------------------------------
## Mensagens por Dia

df_dias <- data %>% 
  count(data) %>% 
  filter(data >= '2018-09-05') %>% 
  right_join(tibble(data = seq(min(.$data), max(.$data), by = 'days')),
             'data') %>% 
  mutate(min_year = year(min(data)),
         max_year = year(max(data)),
         year_gap = (max_year - min_year),
         year = year(data),
         dif_to_max_year = max_year - year,
         ano = case_when(data >= glue('{year(data)}-09-05') & 
                           data <= glue('{year(data) + 1}-09-05') ~ year_gap - dif_to_max_year,
                         data < glue('{year(data)}-09-05') ~ year_gap - dif_to_max_year - 1)) %>% 
  replace_na(list(n = 0))

df_dias %>%
  ggplot(aes(x = data, y = n, color = factor(ano))) +
  geom_line(size = 1) +
  scale_color_viridis_d(direction = -1, begin = 0.25, end = .75) +
  guides(color = guide_legend(title = 'Anos de Namoro', title.position = 'top', title.hjust = 0.5, 
                              override.aes = list(size = 1.2))) +
  labs(x = 'Tempo', y = 'N° de Mensagens',
       title = 'Estamos conversando menos?',
       subtitle = 'Quantidade de mensagens diárias desde o início do namoro até agora') +
  theme(legend.position = 'top', 
        legend.justification = 'left',
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

ggsave(filename = 'plots/msg_dia.pdf',
       device = cairo_pdf, width = 20, height = 11)

## Mensagens por Ano

df_ano <- data %>% 
  slice(-1) %>% 
  count(data) %>% 
  filter(data >= '2018-09-05') %>%
  mutate(min_year = year(min(data)),
         max_year = year(max(data)),
         year_gap = (max_year - min_year),
         year = year(data),
         dif_to_max_year = max_year - year,
         ano = case_when(data >= glue('{year(data)}-09-05') & 
                           data <= glue('{year(data) + 1}-09-05') ~ year_gap - dif_to_max_year,
                         data < glue('{year(data)}-09-05') ~ year_gap - dif_to_max_year - 1)) %>% 
  group_by(ano) %>% 
  summarise(mean = mean(n))


df_ano %>% 
  ggplot(aes(x = factor(ano), y = mean, 
             fill = factor(ano))) +
  geom_col() +
  labs(y = 'N° de Mensagens', x = 'Dia do Mês', 
       title = 'Quando mais conversamos?',
       subtitle = 'Média de mensagens enviadas por dia do mês') + 
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_discrete(labels = function(x) str_c('Ano ', x)) +
  scale_fill_viridis_d( direction = -1, begin = 0.25, end = .75) +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid.minor = element_blank())

ggsave(filename = 'plots/dom.pdf',
       device = cairo_pdf, width = 18, height = 11)
