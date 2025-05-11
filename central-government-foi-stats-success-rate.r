# this is based on the latest realease of FOI stats from the Cabinet Office:
# https://www.gov.uk/government/statistics/foi-statistics-annual-2024-published-data
# The data is available at https://assets.publishing.service.gov.uk/media/68108ef4b0d43971b07f5c84/foi-statistics-annual-2024-published-data.csv
# The graph is based on one that appeared in The Economist https://www.economist.com/britain/2017/11/09/freedom-of-information-requests-are-being-turned-down-more-often

library(data.table)
library(tidyverse)
library(ggthemes)

df <- fread("https://assets.publishing.service.gov.uk/media/68108ef4b0d43971b07f5c84/foi-statistics-annual-2024-published-data.csv")

df1 <- df %>%
  filter(`Government body` %in% c(
    "Department for Education",
    "Department for Digital, Culture, Media and Sport",
    "Department for Work and Pensions",
    "HM Treasury",
    "Home Office",
    "Foreign, Commonwealth and Development Office",
    "Cabinet Office",
    "Foreign and Commonwealth Office",
    "Department for Culture, Media and Sport"
  )) %>%
  mutate(
    `Government body` = case_when(
      `Government body` == "Department for Education" ~ "Education",
      `Government body` == "Department for Digital, Culture, Media and Sport" ~ "Culture",
      `Government body` == "Department for Work and Pensions" ~ "Work and Pensions",
      `Government body` == "HM Treasury" ~ "Treasury",
      `Government body` == "Home Office" ~ "Home Office",
      `Government body` == "Foreign, Commonwealth and Development Office" ~ "Foreign Office",
      `Government body` == "Foreign and Commonwealth Office" ~ "Foreign Office",
      `Government body` == "Department for Culture, Media and Sport" ~ "Culture",
      `Government body` == "Cabinet Office" ~ "Cabinet Office",
      TRUE ~ `Government body`
    ),
    `Percentage of resolvable requests granted in full` = as.numeric(`Percentage of resolvable requests granted in full`) # Ensure numeric
  ) %>%
  filter(
    !grepl("Q", `Quarter`)
  )

yearly_avg <- df1 %>%
  group_by(Quarter) %>%
  summarise(Average = mean(`Percentage of resolvable requests granted in full`, na.rm = TRUE))

dept_colors <- c(
  "Education" = "#6fa3b9",
  "Culture" = "#4c261a",
  "Work and Pensions" = "#f2bc8e",
  "Treasury" = "#f5d743",
  "Home Office" = "#406e6b",
  "Foreign Office" = "#c9767f",
  "Cabinet Office" = "#8a735a",
  "Average" = "#c5dfe8"
)

p <- ggplot(df1, aes(x = Quarter, y = `Percentage of resolvable requests granted in full`, group = `Government body`, color = `Government body`)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = dept_colors) +
  geom_line(data = yearly_avg, aes(x = Quarter, y = Average, group = 1), color = "grey", linetype = "solid", linewidth = 1.2) +
  geom_line(data = yearly_avg, aes(x = Quarter, y = Average, group = 1), color = "black", linetype = "dotted", linewidth = 1.2) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Dropping like FOIs",
    subtitle = "Share of FOI requests granted in full by government body %",
    caption = "Source: Cabinet Office",
    color = "Government Body"
  )

e <- p + theme_fivethirtyeight() +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    legend.background  = element_rect(fill = "white", colour = NA),
    legend.key         = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "plot.pdf",
  plot     = e,
  device   = "pdf",
  path     = "figures",
  width    = 6.65,
  height   = 8,
  units    = "in",
  dpi      = 300
)
