# this is based on the latest realease of FOI stats from the Cabinet Office:
# https://www.gov.uk/government/statistics/foi-statistics-annual-2024-published-data
# The data is available at https://assets.publishing.service.gov.uk/media/68108ef4b0d43971b07f5c84/foi-statistics-annual-2024-published-data.csv

library(data.table)
library(tidyverse)
library(ggthemes)

df <- fread("https://assets.publishing.service.gov.uk/media/68108ef4b0d43971b07f5c84/foi-statistics-annual-2024-published-data.csv")

df_cost_refusals <- df %>%
  filter(
    !grepl("Q", `Quarter`)
  ) %>%
  group_by(Quarter) %>%
  summarise(
    total_requests = sum(as.numeric(`Total requests received (excluding on-hold and lapsed)`), na.rm = TRUE),
    cost_refusals = sum(as.numeric(`Fully refused - cost limit`), na.rm = TRUE)
  ) %>%
  mutate(
    percentage_cost_refusal = (cost_refusals / total_requests) * 100
  )

p_cost <- ggplot(df_cost_refusals, aes(x = Quarter, y = percentage_cost_refusal)) +
  geom_line(group = 1) +
  labs(
    title = "Percentage of FOI Requests Refused on Cost Grounds"
  )
p_cost

# Define the refusal columns to analyze
refusal_columns <- c(
  "Fully refused - vexatious",
  "Fully refused - repeated",
  "Fully refused - cost limit",
  "S(22) - Information intended for future publication",
  "S(22A) - Research intended for future publication",
  "S(23) - Information supplied by, or relating to, bodies dealing with security matters",
  "S(24) - National security",
  "S(26) - Defence",
  "S(27) - International relations",
  "S(28) - Relations within the United Kingdom",
  "S(29) - The economy",
  "S(30) - Investigations and proceedings conducted by public authorities",
  "S(31) - Law enforcement",
  "S(32) - Court records, etc",
  "S(33) - Audit functions",
  "S(34) - Parliamentary privilege",
  "S(35) - Formulation of Government policy, etc",
  "S(36) - Prejudice to effective conduct of public affairs",
  "S(37) - Communications with His Majesty, etc and honours",
  "S(38) - Health and Safety",
  "S(39) - Environmental information",
  "S(40) - Personal information",
  "S(41) - Information provided in confidence",
  "S(42) - Legal professional privilege",
  "S(43) - Commercial interests",
  "S(44) - Prohibitions on disclosure"
)

total_requests_annual <- df %>%
  filter(!grepl("Q", `Quarter`)) %>%
  mutate(Year_Str = as.character(`Quarter`)) %>%
  filter(Year_Str == "2017" | Year_Str == "2023") %>%
  group_by(Year_Str) %>%
  summarise(
    Total_Requests_For_Year = sum(as.numeric(`Total requests received (excluding on-hold and lapsed)`), na.rm = TRUE),
    .groups = "drop"
  )

df_rates_comparison <- df %>%
  filter(!grepl("Q", `Quarter`)) %>%
  mutate(Year_Str = as.character(`Quarter`)) %>%
  filter(Year_Str == "2017" | Year_Str == "2023") %>%
  select(Year_Str, all_of(refusal_columns)) %>%
  mutate(across(all_of(refusal_columns), as.numeric)) %>%
  group_by(Year_Str) %>%
  summarise(across(all_of(refusal_columns), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = all_of(refusal_columns), names_to = "Refusal_Reason", values_to = "Count") %>%
  left_join(total_requests_annual, by = "Year_Str") %>%
  mutate(
    Refusal_Rate_Percent = ifelse(Total_Requests_For_Year > 0, (Count / Total_Requests_For_Year) * 100, 0)
  ) %>%
  select(Refusal_Reason, Year_Str, Refusal_Rate_Percent) %>%
  pivot_wider(names_from = Year_Str, values_from = Refusal_Rate_Percent, names_prefix = "Rate_Percent_") %>%
  mutate(
    Change_In_Rate_pp_2017_to_2023 = `Rate_Percent_2023` - `Rate_Percent_2017`
  ) %>%
  arrange(desc(Change_In_Rate_pp_2017_to_2023))

print(df_rates_comparison)

fwrite(df_rates_comparison, "refusal-rates-government-comparison.csv")
