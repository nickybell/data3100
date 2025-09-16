# This script takes the data from https://jse.amstat.org/datasets/draft70yr.dat.txt and generates a csv for teaching use.

# Load the data
draft_data <- read.table("https://jse.amstat.org/datasets/draft70yr.dat.txt", header = FALSE)

# Assign column names
colnames(draft_data) <- c("sequential_day", "lottery_number", "month")

# Create a vector containing the day numbers for each month (including leap year day)
days_in_month <- c(January = 31, February = 29, March = 31, April = 30, May = 31, June = 30,
                   July = 31, August = 31, September = 30, October = 31, November = 30, December = 31)
draft_data$day <- unlist(sapply(days_in_month, seq_len))

write.csv(draft_data[,c(3,4,2)], "data/week4/1970_vietnam_draft_lottery.csv", row.names = FALSE)

