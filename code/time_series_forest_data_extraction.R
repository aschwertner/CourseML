rm(list = ls())
library(dplyr)

# Load dataset and select data of interest
Drca = read.csv("./dataraw/rca_data_2012_2022-06-29.csv") %>% 
  select(Date, CellID, DOAVEG_avg, WTEMP_avg, SAL_avg, CHLAVEG_avg) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Month = as.integer(format(Date, "%m")),
         Year = as.integer(format(Date, "%Y"))) %>% 
  filter(Year == 2012) %>% 
  group_by(CellID, Month) %>% 
  summarise_at(c("DOAVEG_avg", "WTEMP_avg", "SAL_avg", "CHLAVEG_avg"), mean)

head(Drca)

# Save data in RDS file
saveRDS(Drca, "./dataderived/Drca_forecasting.RDS")
