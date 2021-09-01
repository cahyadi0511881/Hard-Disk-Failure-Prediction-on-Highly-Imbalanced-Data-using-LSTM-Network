# Pre-Processing File 

# Compiling all the Hard Disk data into a dataframe

# Set up the memory limit
memory.limit(size=12000000)

# Apply the rbind to collate all the data 
Q4_2020_data = do.call(rbind, mget(ls(pattern="X2020.*")))
Q4_2020_data = as.data.frame(Q4_2020_data)


# Pre-Processing the data further after some EDA

# Checking how many hard disk in total
length(unique(Q4_2020_data$serial_number))

# There are total recorded 171895 hard disk 

# Checking how many hard disk models are present in the Q4 2020 Data
length(unique(Q4_2020_data$model))

# There are 59 recorded models listed in the Q4 2020 Data

# Checking how many failures recorded throughout the span of Q4 2020
failedHardDisk = Q4_2020_data[Q4_2020_data$failure == 1,]

# There are 404 hard disk that are experiencing failure in this data set

length(unique(failedHardDisk$model))

# There are only 21 models that experience failure

table(failedHardDisk$model)

# Produce the histogram of the failed model
ggplot(data= failedHardDisk, aes(x=model, fill= model)) + 
  geom_histogram(stat= "count") + coord_flip() + guides(fill=FALSE) + 
  labs(title = "Failed Hard Disk Count")

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

# Caching the processed data so that it would be faster to be reproduced
cache("Q4_2020_data_processed")
