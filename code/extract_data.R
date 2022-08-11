# Dataset
data = read.csv("./dataraw/rca_data_2012_2022-06-29.csv")

# Select columns of interest from dataset
columns = c("Date", "CellID", "DOAVEG_avg")

# Extract data from dataset
new_data = data[, columns]

# Save new dataset
write.csv(new_data,"./dataderived/rca_DOAVEG_avg.csv", row.names = FALSE)
