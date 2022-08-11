# Libraries ####################################################################

library(funtimes)
library(ggplot2)
library(tidyr)

# Data #########################################################################

data_all = read.csv("./dataderived/rca_DOAVEG_avg.csv")
data = data_all[1:12000, ]
data$Date = as.Date(data$Date)
head(data)

# Plotting time series #########################################################

save_figure = FALSE
if(save_figure){
  png(file="./figures/fig01.png", width=1920, height=1080)
}
ggplot(data) + 
  geom_line(aes(x = Date, y = DOAVEG_avg, color = CellID), size = 1) +
  ylab('DOAVEG_avg') +
  theme_bw()
if(save_figure){
  dev.off()
}

# Clustering ###################################################################

# Converts date to number
data$Date = as.numeric(gsub('-', '', data$Date))
is.numeric(data$Date)
head(data)

# Makes each time series a column
data2 = pivot_wider(data, names_from = CellID, values_from = DOAVEG_avg)
head(data2)
tail(data2)

# Clustering time series based on trend synchronism

cluster_trend = sync_cluster(data2 ~ t)
cluster_trend


