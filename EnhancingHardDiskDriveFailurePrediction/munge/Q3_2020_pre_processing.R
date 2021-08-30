# Binding all the individual files 

# Use library data-table for writing file
library(ProjectTemplate)
load.project()
library(data.table)

# Collate the July Data
Q3_July_2019_data = do.call(rbind, mget(ls(pattern="X2019.07*")))
Q3_July_2019_data = as.data.frame(Q3_July_2020_data) # Turn into data frame

list_of_smart = vector()
list_of_smart = c("date", "serial_number", "model", "capacity_bytes", "failure",
                  "smart_1_raw", "smart_3_raw", "smart_4_raw", "smart_5_raw", 
                  "smart_7_raw","smart_9_raw","smart_10_raw", "smart_12_raw", 
                  "smart_187_raw","smart_188_raw","smart_190_raw","smart_192_raw",
                  "smart_193_raw", "smart_194_raw", "smart_197_raw", "smart_198_raw",
                  "smart_199_raw", "smart_240_raw", "smart_241_raw", "smart_242_raw")


# Take only the ST4000DM000

Q3_July2019_ST4000DM000_data = filter(Q3_July_2019_data, model == "ST4000DM000")
Q3_July2019_ST4000DM000_data = Q3_July2019_ST4000DM000_data[,list_of_smart]
cache('Q3_July_2019_data')
cache('Q3_July2019_ST4000DM000_data')

# Collate the August Data

Q3_August_2019_data = do.call(rbind,mget(ls(pattern="X2019.08*")))

Q3_August_2019_data = as.data.frame(Q3_August_2019_data) # Turn into data frame

# Select only the raw attributes 
rawAttributesColumns = seq(7,131, by =2)
Q3_August_2019_data = Q3_August_2019_data[,list_of_smart]

# Take only the ST4000DM000

Q3_August2019_ST4000DM000_data = filter(Q3_August_2019_data, model == "ST4000DM000")

cache('Q3_August_2019_data')
cache('Q3_August2019_ST4000DM000_data')

# Collate the September data

Q3_Sept_2019_data = do.call(rbind,mget(ls(pattern = "X2019.09*")))

Q3_Sept_2019_data = as.data.frame(Q3_Sept_2019_data)


Q3_Sept_2019_data = Q3_Sept_2019_data[,list_of_smart]

# Take only the ST4000DM000 model
Q3_Sept2019_ST4000DM000_data = filter(Q3_Sept_2019_data, model == "ST4000DM000")

cache('Q3_Sept_2020_data')
cache('Q3_Sept2019_ST4000DM000_data')


# Combined the 3 months

Q3_2019_ST4000DM000_data = rbind(Q3_July2019_ST4000DM000_data, Q3_August2019_ST4000DM000_data, Q3_Sept2019_ST4000DM000_data) 

# Write into a csv file
fwrite(Q3_ST4000DM000_data, "C:/Users/Aorus/Desktop/MSc_DataScience/Q3_2020_Processing/Q3_2020_Processing/data/Q3_ST4000DM000.csv", row.names = FALSE)


Q3_ST4000DM000 = Q3_ST4000DM000[,list_of_smart]

fwrite(Q3_2019_ST4000DM000_data, "C:/Users/Aorus/Desktop/MSc_DataScience/Q3_2020_Processing/Q3_2020_Processing/data/Q32019_ST4000DM000.csv", row.names = FALSE)
cache('Q3_2019_ST4000DM000_data')
