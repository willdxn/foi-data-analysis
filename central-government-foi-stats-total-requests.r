# this is based on the latest realease of FOI stats from the Cabinet Office:
# https://www.gov.uk/government/statistics/foi-statistics-annual-2024-published-data
# The data is available at https://assets.publishing.service.gov.uk/media/68108ef4b0d43971b07f5c84/foi-statistics-annual-2024-published-data.csv

library(data.table)
library(tidyverse)
library(ggthemes)

df <- fread("https://assets.publishing.service.gov.uk/media/68108ef4b0d43971b07f5c84/foi-statistics-annual-2024-published-data.csv")

df1 <- df %>%
  filter(
    !grepl("Q", `Quarter`)
  ) %>%
  group_by(Quarter) %>%
  summarise(sum = sum(as.numeric(`Total requests received (excluding on-hold and lapsed)`)))


p <- ggplot(df1, aes(x = Quarter, y = sum)) +
  geom_col(fill = "#8a735a") +
  labs(
    title = "Information overload",
    subtitle = "Number of FOI requests received within central government",
    caption = "Source: Cabinet Office"
  )

e <- p + theme_fivethirtyeight() +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    legend.background  = element_rect(fill = "white", colour = NA),
    legend.key         = element_rect(fill = "white", colour = NA)
  )

e

ggsave(
  filename = "plot3.svg",
  plot     = e,
  device   = "svg",
  path     = "figures/exported/",
  width    = 6.65,
  height   = 8,
  units    = "in",
  dpi      = 300
)
