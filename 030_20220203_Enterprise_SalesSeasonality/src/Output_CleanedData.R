# Output Sales Seasonality as csv
# Thu Apr 07 17:42:08 2022 ------------------------------

library(tidyverse)

load(file = here::here("data", "SalesPerc.rds"))

# ---- Date lookups -------
DatYear <- 2021


#-

Agg <- 
  AggeCenters %>% 
  ungroup() %>%
  filter(Year == DatYear) %>% 
  mutate(Month = lubridate::month(Date, label =T, abbr=T)) %>% 
  select(BldgGroupName, Month, TotalSales, PercYear) %>% 
  rename(`Percent of Sales` = PercYear,
         `Total Gross Sales` = TotalSales) %>% 
  pivot_longer(cols = c(3,4), names_to = "DataType") %>% 
  pivot_wider(names_from = Month, values_from = value) %>% 
  filter(DataType == "Percent of Sales")


Agg %>% 
  write_csv(., here::here("output", glue::glue("SalesSeasonality_{DatYear}.csv")))
