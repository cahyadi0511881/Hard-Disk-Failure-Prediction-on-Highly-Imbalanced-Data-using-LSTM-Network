# Combining all the csv files of Q1 2020 into 1 large file

library(ProjectTemplate)
library(data.table)
load.project()

# Collate all the files for the month of January 
Q1_Jan_2019_data = do.call(rbind, mget(ls(pattern="X2019.01*")))
Q1_Jan_2019_data = as.data.frame(Q1_Jan_2019_data)

list_of_columns = vector()
list_of_columns = c("date", "serial_number", "model", "capacity_bytes", "failure",
                    "smart_1_raw", "smart_3_raw", "smart_4_raw", "smart_5_raw", 
                    "smart_7_raw","smart_9_raw","smart_10_raw", "smart_12_raw", 
                    "smart_187_raw","smart_188_raw","smart_190_raw","smart_192_raw",
                    "smart_193_raw", "smart_194_raw", "smart_197_raw", "smart_198_raw",
                    "smart_199_raw", "smart_240_raw", "smart_241_raw", "smart_242_raw")

# Select only the necessary attributes 
Q1_Jan_2019_data = Q1_Jan_2019_data[, list_of_columns]

# Select only the ST4000DM000 model

Q1_Jan2019_ST4000DM000 = filter(Q1_Jan_2019_data, model == "ST4000DM000")

cache("Q1_Jan2019_ST4000DM000")


# Collate all the files for the month of February
Q1_Feb_2019_data = do.call(rbind, mget(ls(pattern="X2019.02*")))
Q1_Feb_2019_data = as.data.frame(Q1_Feb_2019_data)

# Select only the necessary attributes

Q1_Feb_2019_data = Q1_Feb_2019_data[,list_of_columns]

# Select only the ST4000DM000 model

Q1_Feb2019_ST4000DM000 = filter(Q1_Feb_2019_data, model == "ST4000DM000")

cache('Q1_Feb2019_ST4000DM000')


# Collate all the files for the month of March
Q1_March_2019_data = do.call(rbind, mget(ls(pattern="X2019.03*")))
Q1_March_2019_data = as.data.frame(Q1_March_2019_data)

# Select only the necessary attributes
Q1_March_2019_data = Q1_March_2019_data[,list_of_columns]

# Select only the ST4000DM000 model

Q1_March2019_ST4000DM000 = filter(Q1_March_2019_data, model == "ST4000DM000")

cache('Q1_March2019_ST4000DM000')

# Combined all the 3 months data 

Q1_2019_ST4000DM000 = rbind(Q1_Jan2019_ST4000DM000, Q1_Feb2019_ST4000DM000, Q1_March2019_ST4000DM000)

cache('Q1_2019_ST4000DM000')


# Write into a csv file

library(data.table)

fwrite(Q1_2019_ST4000DM000, "C:/Users/Aorus/Desktop/MSc_DataScience/Q1_2020_Processing/Q1_2020_Processing/data/Q1_2019_ST4000DM000.csv", row.names = FALSE)


# Combined all the Q1 up to Q4 of 2019 data for test data

All_Quarter_ST4000DM000_2019 = rbind(Q1_2019_ST4000DM000,Q2_2019_ST4000DM000,Q3_2019_ST4000DM000_data,Q4_2019_ST4000DM000)

All_Quarter_ST4000DM000_2019 = filter(All_Quarter_ST4000DM000_2019, capacity_bytes > 0)
cache('All_Quarter_ST4000DM000_2019')
