indicator <- c(
  "4.7",
  "4.4",
  "4.5",
  "4.6",
  "4.1",
  "4.2",
  
  "3.3",
  "3.4",
  "3.2",
  
  "4.8",
  "6.5",
  
  "4.3",
  "7.4",
  "8.6",
  "7.1",
  "8.4",
  "7.2",
  "6.4",
  "6.3"
)

data2table <- master_data %>%
  select(Country, Year, all_of(indicator)) %>%
  filter(Year %in% c("2020", "2023", "2024")) %>%
  pivot_longer(cols = !c(Country, Year), names_to = "indicator", values_to = "value2plot") %>%
  group_by(Country, Year) %>%
  summarise(value2table = mean(value2plot, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(Country), names_from = Year, values_from = value2table) %>%
  mutate(
    regional_ranking = rank(-`2024`),
    `Score in 2024` = sprintf("%.2f", `2024`),  # Use sprintf() for two decimals
    `Score in 2020` = sprintf("%.2f", `2020`),  # Use sprintf() for two decimals
    `% change in 5 years` = ifelse(
      (`2024` - `2020`) / `2020` > 0,
      paste0("+", round((`2024` - `2020`) / `2020`, 2) * 100, "%"),
      paste0(round((`2024` - `2020`) / `2020`, 2) * 100, "%")
    ),
    `% change last year` = ifelse(
      (`2024` - `2023`) / `2023` > 0,
      paste0("+", round((`2024` - `2023`) / `2023`, 2) * 100, "%"),
      paste0(round((`2024` - `2023`) / `2023`, 2) * 100, "%")
    ),
    ranking_2020 = rank(-`2020`)
  ) %>%
  select(`Regional ranking` = regional_ranking, 
         Country, 
         `Score in 2024`, 
         `% change last year`,
         `Score in 2020`, 
         `% change in 5 years`, 
         `Regional ranking in 2020` = ranking_2020) %>%
  arrange(`Regional ranking`)



writexl::write_xlsx(data2table, path = "table_south_asia.xlsx")
