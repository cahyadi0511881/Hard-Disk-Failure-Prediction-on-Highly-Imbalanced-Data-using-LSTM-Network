# This file contains all the necessary code needed for the initial pre-processing
# of the data


# Set up the memory limit
memory.limit(size=12000000)

# Apply the rbind to collate all the data 
Q4_2020_data = do.call(rbind, mget(ls(pattern="X2020.*")))
Q4_2020_data = as.data.frame(Q4_2020_data)

# Check each columns missing values, for the first check we only check the RAW values of the SMART attributes

# List the columns that we want to check for missing values

rawAttributesColumns = seq(7,149, by =2)

# Create the loop in which it automatically scan for missing values in the columns

columnNames = colnames(Q4_2020_data[rawAttributesColumns])

naPercentageRaw = vector()

for (i in rawAttributesColumns){
  
  naPercentageColumn = 0
  naPercentageColumn = sum(is.na(Q4_2020_data[,i])) / nrow(Q4_2020_data) * 100
  naPercentageRaw = append(naPercentageRaw, naPercentageColumn)
  
}

# Compile it into a data frame
raw_na_Check = cbind(rawAttributesColumns, columnNames,naPercentageRaw)
raw_na_Check = as.data.frame(raw_na_Check)

# Select only columns which have less than 40% missing values

workableAttributes = raw_na_Check[raw_na_Check$naPercentageRaw <= 40 & raw_na_Check$naPercentageRaw !=100 ,]

workableAttributesColNumber = as.numeric(workableAttributes$rawAttributesColumns)

# Since normalized value of each SMART attributes located a column before the raw 
# Value therefore, we created a vector which is consisted of column numbers of the
# raw attributes that have less than 40% missing values columns - 1

workableAttributesNormColNumber = workableAttributesColNumber - 1

workableAttributesTotalColNumber = append(workableAttributesColNumber,workableAttributesNormColNumber)

Q4_2020_data_processed = Q4_2020_data[,c(1:5, workableAttributesTotalColNumber)]

cache("Q4_2020_data_processed")

Q4_2020_data_processed = Q4_2020_data_processed[,1:25]


# Since some of the hard disk drive experience failure at the first day of Q4,
# try adding the last 10 days of Q3 data. However, we might need to be careful 
# since the data from Q3 has different S.M.A.R.T attributes number than that of 
# the Q4

Q3_2020_data = do.call(rbind, mget(ls(pattern="X2020.09*")))
Q3_2020_data = as.data.frame(Q3_2020_data)

# Caching the Q3_2020_data. Bear in mind that this is not the entire Q3 data, 
# only the last 10 days data necessary for completing some hard disk observations 
# in Q4

cache("Q3_2020_data")

# Get only the raw values of the Q3 to check which SMART attributes is not available
# from the Q3

rawAttributesColumns = seq(7,131, by =2)

SMART_Q3 = colnames(Q3_2020_data[,rawAttributesColumns])

SMART_Q4 = colnames(Q4_2020_data_processed[,6:25])

availableQ4SMART =  SMART_Q4[ SMART_Q4 %in% SMART_Q3]

ColData = as.data.frame(availableQ4SMART)

# All the SMART attributes that are needed for the modelling that are present in
# Q4 are also present in the Q3 data despite the difference in number of total
# SMART attributes recorded. Therefore we could include the last 10 days of the Q3
# data for our modelling

# Subsetting the Q3 Data to only get the necessary attributes

Q3_2020_data_processed = Q3_2020_data[,c("date", "serial_number", "model", "capacity_bytes"
                                         , "failure", availableQ4SMART)]

cache('Q3_2020_data_processed')

# Now combine this data with the Q4 data
Q4_combined_data = rbind(Q3_2020_data_processed, Q4_2020_data_processed)


# Remove the negative capacity bytes in which it does not contribute to the model

Q4_combined_data = Q4_combined_data[Q4_combined_data$capacity_bytes > 0, ]

cache("Q4_combined_data")



