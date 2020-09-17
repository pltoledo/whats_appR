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

ggsave(filename = 'plots/doy.pdf',
       device = cairo_pdf, width = 18, height = 11)

# Nomes

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
  scale_fill_manual(values = c('#F8766D', "#619CFF")) +
  theme(legend.position = 'none',
        axis.text.y = element_text(size = 13),
        axis.text.x = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

ggsave(filename = 'plots/nomes.pdf',
       device = cairo_pdf, width = 18, height = 11)

# Emojis

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
  geom_col(fill = "#F8766D") +
  labs(x = NULL, y = NULL,
       subtitle = 'Juju') +
  theme(axis.text.y = ggtext::element_markdown(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

emoji_pedro <- df_emoji %>%
  filter(autor == 'Pedro Toledo') %>% 
  ggplot(aes(x = n, y = reorder(label, n))) + 
  geom_col(fill = "#619CFF") +
  labs(x = NULL, y = NULL,
       subtitle = 'Pedro') +
  theme(axis.text.y = ggtext::element_markdown(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

emoji_juju + 
  emoji_pedro + 
  plot_annotation(title = 'Qual o emoji mais usado?', 
                  subtitle = 'Top 10 emojis mais enviados por cada um') +
  scale_x_continuous(limits = c(0, 600), name = 'N° de Mensagens com o Emoji')

ggsave(filename = 'plots/emoji.pdf',
       device = cairo_pdf, width = 18, height = 11)

# Mensagens por Dia

df_dias <- data %>% 
  count(data) %>% 
  filter(data >= '2018-09-04') %>% 
  right_join(tibble(data = seq(min(.$data), max(.$data), by = 'days')),
             'data') %>% 
  mutate(min_year = year(min(data)),
         max_year = year(max(data)),
         year_gap = (max_year - min_year),
         year = year(data),
         dif_to_max_year = max_year - year,
         ano = case_when(data >= glue('{year(data)}-09-04') & 
                           data <= glue('{year(data) + 1}-09-04') ~ year_gap - dif_to_max_year,
                         data < glue('{year(data)}-09-04') ~ year_gap - dif_to_max_year - 1)) %>% 
  replace_na(list(n = 0))

df_dias %>%
  ggplot(aes(x = data, y = n, color = factor(ano))) +
  geom_line() +
  scale_color_brewer(palette = 'Set1') +
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
       device = cairo_pdf, width = 18, height = 11)
