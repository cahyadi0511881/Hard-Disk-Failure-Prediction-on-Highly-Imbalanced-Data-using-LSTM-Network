# Pre-Process the Q4 2019 data

# Compile all the data from October 2019
Q4_October_2019_data = do.call(rbind,mget(ls(pattern = "X2019.10*")))
Q4_October_2019_data = as.data.frame(Q4_October_2019_data)

# Select only the raw attributes
rawAttributesColumns = seq(7,131, by =2)

Q4_October_2019_data = Q4_October_2019_data[,c(1:5,rawAttributesColumns)]

# Select only the columns which are present in Q4 2020
list_of_columns = vector()
list_of_columns = c("date", "serial_number", "model", "capacity_bytes", "failure",
                    "smart_1_raw", "smart_3_raw", "smart_4_raw", "smart_5_raw", 
                    "smart_7_raw","smart_9_raw","smart_10_raw", "smart_12_raw", 
                    "smart_187_raw","smart_188_raw","smart_190_raw","smart_192_raw",
                    "smart_193_raw", "smart_194_raw", "smart_197_raw", "smart_198_raw",
                    "smart_199_raw", "smart_240_raw", "smart_241_raw", "smart_242_raw")

Q4_October_2019_data = Q4_October_2019_data[,list_of_columns]

Q4_October_2019_ST4000DM000 = filter(Q4_October_2019_data, model == "ST4000DM000")
cache('Q4_October_2019_ST4000DM000')



# Now compile the data from November 2019
Q4_November_2019_data = do.call(rbind,mget(ls(pattern = "X2019.11*")))
Q4_November_2019_data = as.data.frame(Q4_November_2019_data)

# Select only the necessary columns that are present in the 2020 data
Q4_November_2019_data = Q4_November_2019_data[,list_of_columns]


# Select only the ST4000DM000 model to be analysed
Q4_November_2019_ST4000DM000 = filter(Q4_November_2019_data, model == "ST4000DM000")
cache('Q4_November_2019_ST4000DM000')



# Compile the data from December 2019

Q4_December_2019_data = do.call(rbind,mget(ls(pattern = "X2019.12*")))
Q4_December_2019_data = as.data.frame(Q4_December_2019_data)

# Select only the necessary columns that are present in the 2020 data
Q4_December_2019_data = Q4_December_2019_data[,list_of_columns]


# Select only the ST4000DM000 model to be analysed
Q4_December_2019_ST4000DM000 = filter(Q4_December_2019_data, model == "ST4000DM000")
cache('Q4_December_2019_ST4000DM000')


# Combined the all the 3 months of Q4 2019
Q4_2019_ST4000DM000 = rbind(Q4_October_2019_ST4000DM000,Q4_November_2019_ST4000DM000,Q4_December_2019_ST4000DM000)
cache('Q4_2019_ST4000DM000')
