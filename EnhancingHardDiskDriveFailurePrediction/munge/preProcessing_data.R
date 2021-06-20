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

Q4_2020_data_processed_RAW = Q4_2020_data_processed[,1:25]



# Since some of the hard disk drive experienced failure on the first day of Q4,
# try adding the Q3 data as well

Q3_2020_data = do.call(rbind, mget(ls(pattern="X2020.09*")))
Q3_2020_data = as.data.frame(Q4_2020_data)