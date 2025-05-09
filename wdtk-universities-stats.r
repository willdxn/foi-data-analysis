library(tidyverse)
library(tidytext)
library(data.table)
library(ggthemes)

csv_files <- list.files(
  path       = "wdtk-data",
  pattern    = "\\.csv$",
  full.names = TRUE
)

big_df <- csv_files %>%
  map(~ as_tibble(fread(.x))) %>%
  bind_rows(.id = "source") %>%
  distinct(title, .keep_all = TRUE)

universities_raw <- read_csv("https://www.whatdotheyknow.com/body/all-authorities.csv")

universities <- universities_raw %>%
  filter(str_detect(Tags, regex("university", ignore_case = TRUE)))

url_tokens <- universities %>%
  pull(`URL name`) %>%
  str_split("_") %>%
  unlist() %>%
  discard(~ .x == "") %>%
  unique() %>%
  enframe(name = NULL, value = "word") %>%
  mutate(lexicon = "url_database")

base_custom_stop <- tibble(
  word    = c("current", "details", "report", "wdtk", "data", "request", "statistics", "NA", "2024", "2023", "foi", "information", "undergraduate", "amp", "academic", "students", "common", "providers", "staff", NA, "2025", "past", "level", "2021", "2022", "2020", "a100", "hons", "ba", "list", "bsc", "student", "due"), # whatever else you want to prune
  lexicon = "custom"
)

my_custom_stop <- bind_rows(base_custom_stop, url_tokens)

cleaned_words <- big_df %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_custom_stop, by = "word")

overall_top <- cleaned_words %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 30)

p <- ggplot(overall_top, aes(n, fct_reorder(word, n))) +
  geom_col(fill = "#8a735a") +
  labs(
    title = "A big ask",
    subtitle = "Word frequency in titles of UK university FOI requests*",
    caption = "*Only those made through whatdotheyknow.com",
  )

e <- p + theme_fivethirtyeight() +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    legend.background  = element_rect(fill = "white", colour = NA),
    legend.key         = element_rect(fill = "white", colour = NA)
  )
ggsave(
  filename = "plot2.pdf",
  plot     = e,
  device   = "pdf",
  path     = "figures",
  width    = 6.65,
  height   = 8,
  units    = "in",
  dpi      = 300
)
