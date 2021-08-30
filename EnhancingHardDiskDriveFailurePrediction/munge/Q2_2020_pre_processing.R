# Binding all the individual files

library(ProjectTemplate)
library(data.table)
load.project()

# Collate the April data

Q2_April_2019_data = do.call(rbind,mget(ls(pattern= "X2019.04*")))
Q2_April_2019_data = as.data.frame(Q2_April_2019_data)

# Create a list of necessary attributes from Q4

list_of_columns = vector()
list_of_columns = c("date", "serial_number", "model", "capacity_bytes", "failure",
                    "smart_1_raw", "smart_3_raw", "smart_4_raw", "smart_5_raw", 
                    "smart_7_raw","smart_9_raw","smart_10_raw", "smart_12_raw", 
                    "smart_187_raw","smart_188_raw","smart_190_raw","smart_192_raw",
                    "smart_193_raw", "smart_194_raw", "smart_197_raw", "smart_198_raw",
                    "smart_199_raw", "smart_240_raw", "smart_241_raw", "smart_242_raw")

# Select only the necessary attributes
Q2_April_2019_data = Q2_April_2019_data[,list_of_columns]


Q2_April2019_ST4000DM000 = filter(Q2_April_2019_data, model == "ST4000DM000")
cache('Q2_April2019_ST4000DM000')


# Collate the May data

Q2_May_2019_data = do.call(rbind, mget(ls(pattern="X2019.05*")))
Q2_May_2019_data = as.data.frame(Q2_May_2019_data)

# Select only the necessary attributes
Q2_May_2019_data = Q2_May_2019_data[,list_of_columns]


# Only take the ST4000DM000 model
Q2_May2019_ST4000DM000 = filter(Q2_May_2019_data, model == "ST4000DM000")
cache('Q2_May2019_ST4000DM000')


# Collate the June data

Q2_June_2019_data = do.call(rbind, mget(ls(pattern="X2019.06*")))
Q2_June_2019_data = as.data.frame(Q2_June_2019_data)

# Select only the necessary attributes
Q2_June_2019_data = Q2_June_2019_data[,list_of_columns]

# Only take the ST4000DM000 model

Q2_June2019_ST4000DM000 = filter(Q2_June_2019_data, model == "ST4000DM000")

cache('Q2_June2019_ST4000DM000')

# Combined all the 3 months of Q2 2020
Q2_2019_ST4000DM000 = rbind(Q2_April2019_ST4000DM000, Q2_May2019_ST4000DM000, Q2_June2019_ST4000DM000)

cache('Q2_2019_ST4000DM000')

# Write into csv file

library(data.table)

fwrite(Q2_2019_ST4000DM000, "C:/Users/Aorus/Desktop/MSc_DataScience/Q2_2020_Processing/Q2_2020_Processing/data/Q2_2019_ST4000DM000.csv" ,row.names = FALSE)
