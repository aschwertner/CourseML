rm(list = ls())

# Libraries ####################################################################

library(dplyr)
library(ranger)
library(ggplot2)
library(purrr)

# Data #########################################################################

# Loading data
Drca = readRDS("./dataderived/Drca_forecasting.RDS")

# Taking a sample with 100 time series
CELLS = unique(Drca$CellID)
set.seed(123)
cells = sample(CELLS, 100)
drca = Drca %>% 
  filter(is.element(CellID, cells))

head(drca)

drca_mean = drca %>% 
  group_by(CellID) %>% 
  summarise_at(c("DOAVEG_avg", "WTEMP_avg", "SAL_avg", "CHLAVEG_avg"), mean)

head(drca_mean)

set.seed(123)

rf_drca = ranger(DOAVEG_avg ~ CHLAVEG_avg + WTEMP_avg + SAL_avg, data = drca_mean[,-1],
                 importance = 'permutation',
                 respect.unordered.factors = 'partition')

print(rf_drca)

ranimp <- importance_pvalues(rf_drca, method = "altmann",
                             num.permutations = 500,
                             formula = as.formula(paste("DOAVEG_avg ~ CHLAVEG_avg + WTEMP_avg + SAL_avg", collapse = " ", sep = " ~ ")),
                             data = drca_mean[,-1])
ranimp <- ranimp[order(ranimp[,1]),]
tmp <- ranimp[,1]
barplot(tmp,
        beside = TRUE, las = 1, xlim = c(0, 2.5),
        xlab = "Importance",
        col = 1, border = NA, cex.names = 0.6,
        horiz = TRUE)
