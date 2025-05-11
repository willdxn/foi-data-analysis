library(priceR)
library(tidyverse)
library(ggthemes)
library(data.table)

df <- fread("ico-casework-funding-data.csv")

inf_df <- retrieve_inflation_data(
  country = "GB"
)

df <- df %>%
  mutate(
    Year = as.numeric(str_extract(Year, "^\\d{4}")),
    `Cases received` = as.numeric(gsub(",", "", `Cases received`)),
    `Grant-in-aid` = as.numeric(`Grant-in-aid`) # Assuming this is already numeric, if not, add gsub
  )

df <- df %>%
  mutate(
    `Nominal Cost per Case (Millions)` = `Grant-in-aid` / `Cases received`
  )

inf_df_prepared <- inf_df %>%
  rename(Year = date) %>%
  mutate(Year = as.numeric(Year))


df_merged <- df %>%
  left_join(inf_df_prepared, by = "Year")


latest_year <- max(df_merged$Year, na.rm = TRUE)

df_merged <- df_merged %>%
  mutate(
    `Adjusted Cost per Case (Millions, 2023 prices)` = 1000000 * adjust_for_inflation(
      price = `Nominal Cost per Case (Millions)`,
      from_date = Year,
      country = "GB",
      to_date = latest_year
    )
  )

df_merged

p <- ggplot(df_merged, aes(x = Year, y = `Adjusted Cost per Case (Millions, 2023 prices)`)) +
  geom_line(linewidth = 1.2, color = "#8a735a") +
  labs(
    title = "On thin ICO",
    subtitle = "Cost per case received by the ICO (£, 2023)*",
    caption = "*Based on their grant-in-aid, which is mostly spent on FOI casework.",
  )

e <- p + theme_fivethirtyeight() +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    legend.background  = element_rect(fill = "white", colour = NA),
    legend.key         = element_rect(fill = "white", colour = NA)
  )

e

# Plot for Cases Received Over Time
p_cases <- ggplot(df_merged, aes(x = Year, y = `Cases received`)) +
  geom_line(linewidth = 1.2, color = "#c9767f") + # Added a color for distinction
  labs(
    title = "On thin ICO",
    subtitle = "Number of FOI cases recieved by the ICO",
    caption = "Source: ICO Annual Reports; Will Dixon"
  )

e_cases <- p_cases + theme_fivethirtyeight() +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    legend.background  = element_rect(fill = "white", colour = NA),
    legend.key         = element_rect(fill = "white", colour = NA)
  )

# Display the new plot
e_cases

# Optionally, save the new plot
ggsave(
  filename = "ICO1.pdf", # New filename
  plot     = e_cases,
  device   = "pdf",
  path     = "figures",
  width    = 6.65,
  height   = 8,
  units    = "in",
  dpi      = 300
)

ggsave(
  filename = "ICO2.pdf",
  plot     = e,
  device   = "pdf",
  path     = "figures",
  width    = 6.65,
  height   = 8,
  units    = "in",
  dpi      = 300
)
