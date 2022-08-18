rm(list = ls())

library(dplyr)
library(neuralnet)

# # Load dataset and select data of interest
# Drca = read.csv("./dataraw/rca_data_2012_2022-06-29.csv") %>% 
#   select(Date, CellID, DOAVEG_avg, WTEMP_avg, SAL_avg, CHLAVEG_avg) %>% 
#   mutate(Date = as.Date(Date)) %>% 
#   mutate(Month = as.integer(format(Date, "%m")),
#          Year = as.integer(format(Date, "%Y"))) %>% 
#   filter(Year == 2012) %>% 
#   group_by(CellID, Month) %>% 
#   summarise_at(c("DOAVEG_avg", "WTEMP_avg", "SAL_avg", "CHLAVEG_avg"), mean)
# 
# head(Drca)
# 
# # Save data in RDS file
# saveRDS(Drca, "./dataderived/Drca_forecasting.RDS")

Drca <- readRDS("./dataderived/Drca_forecasting.RDS")

CELLS = unique(Drca$CellID)
set.seed(123)
cells = sample(CELLS, 1000)

drca = Drca %>% 
  filter(is.element(CellID, cells)) %>%
  group_by(CellID) %>% 
  summarise_at(c("DOAVEG_avg", "WTEMP_avg", "SAL_avg", "CHLAVEG_avg"), mean)


data <- data.frame(aggregate(drca[,-1], list(drca$CellID), FUN = 'mean', na.rm = TRUE)[, -1], row.names = unique(drca$CellID))

head(data)

index <- sample(1:nrow(data),round(0.75*nrow(data)))

train <- data[index,]
test <- data[-index,]

lm.fit <- glm(DOAVEG_avg~., data=train)

summary(lm.fit)

pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$DOAVEG_avg)^2)/nrow(test)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]

n <- names(train_)
f <- as.formula(paste("DOAVEG_avg ~", paste(n[!n %in% "DOAVEG_avg"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(6,3),linear.output=T)

plot(nn)

pr.nn <- compute(nn,test_[,1:4])
pr.nn_ <- pr.nn$net.result*(max(data$DOAVEG_avg)-min(data$DOAVEG_avg))+min(data$DOAVEG_avg)
test.r <- (test_$DOAVEG_avg)*(max(data$DOAVEG_avg)-min(data$DOAVEG_avg))+min(data$DOAVEG_avg)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))