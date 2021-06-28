# EDA on the raw file

library(ProjectTemplate)
load.project()

# Divide Normalised value with Raw Value

# Raw value of smart attributes
RAWAttributesOnly = Q4_2020_data_processed[,1:25]

# Normalised value of smart attributes
NormalisedAttributesOnly = Q4_2020_data_processed[,c(1:5,26:45)]

# Removing row names from the data frames
RAWAttributesOnly = as.data.frame(RAWAttributesOnly, row.names = NULL)

NormalisedAttributesOnly = as.data.frame(NormalisedAttributesOnly, row.names = NULL)

# Check what models dominates the data set
ggplot(data= Q4_2020_data_processed, aes(x=model, fill= model)) + 
  geom_histogram(stat= "count") + coord_flip() + guides(fill=FALSE) + 
  labs(title = "Model Count")

# Trying to pivot wider so that the data frames become a time series model

# The pivot wider caused duplicates 

# Since it seems that the duplicates might be caused by the capacity_bytes registered as negative
# we would try to look whether this caused the hard disk to failed

failedHardDiskQ4 = RAWAttributesOnly[RAWAttributesOnly$failure == 1,]

failedHardDiskQ4Serial_number = unique(failedHardDiskQ4$serial_number)

# It seems there are only a few serial numbers, that has an error which capacity_bytes is -1
RAWDataOnlyFailed = RAWAttributesOnly[RAWAttributesOnly$serial_number %in% failedHardDiskQ4Serial_number, ]

# Finding all the hard disk which has negative capacity bytes

negativeCapacityRawData = RAWAttributesOnly[RAWAttributesOnly$capacity_bytes < 0, ]

# Finding which serial numbers actually the ones with negative capacity are 

negativeSerial_number = unique(negativeCapacityRawData$serial_number)

# There are 8506 unique serial_number therefore we could not conclude whether this
# might indicate failures in the future

# Checking whether the failed hard disk each has experienced the capacity listed 
# as negative

checkData = failedHardDiskQ4[failedHardDiskQ4$serial_number %in% negativeSerial_number,]

# Only 7 out of 404 hard Disk that failed actually has its capacity listed as negative

# All the negative capacity bytes showing all NA for all SMART Attributes, consider 
# for removal for cleaning the data set

CheckData2 = RAWAttributesOnly[RAWAttributesOnly$serial_number %in% negativeSerial_number, ]

# From roughly looking at the data some of the missing values within some attributes
# correlated with the models, therefore checking the models for smart attributes
# 187 - 188

missing187188 = subset(RAWAttributesOnly, is.na(RAWAttributesOnly$smart_187_raw) == TRUE & is.na(RAWAttributesOnly$smart_188_raw) == TRUE)



# The results still inconclusive due to many models (45 models) listed to have 
# missing values in both 187 and 188 attributes. However, it seems that these
# 2 attributes would almost always go in pairs in which wherever one attribute may
# be NA, the other attribute would also be NA. Only 128 observations found to be
# the exception. In which this only apply solely to MTFDDAV240TDU model

MTFDDAV240TDUModelData =  subset(RAWAttributesOnly, RAWAttributesOnly$model == "MTFDDAV240TDU")

# Checking the Toshiba models since it seems they harbor no smart attribute 187 188

ToshibaModelsData = RAWAttributesOnly[grepl("TOSHIBA",RAWAttributesOnly$model),]

Notmissing187188Toshiba = subset(ToshibaModelsData, is.na(ToshibaModelsData$smart_187_raw) == FALSE 
                                 & is.na(ToshibaModelsData$smart_188_raw) == FALSE)


# It seems for Toshiba models there are no SMART Atttribute 187 and 188 recorded

# Therefore we would like to check which models have both 187 and 188 attribute 
# recorded
available187188 = subset(RAWAttributesOnly, is.na(RAWAttributesOnly$smart_187_raw) == FALSE & is.na(RAWAttributesOnly$smart_188_raw) == FALSE)

# In a matter of fact, only ST models have both SMART Attributes 187 and 188 recorded 

# Check if only one of them missing

oneAvailable187188 = subset(RAWAttributesOnly, is.na(RAWAttributesOnly$smart_187_raw) == TRUE & is.na(RAWAttributesOnly$smart_188_raw) == FALSE)

# when only 187 attribute is available, only MTFDDAV240 TDU model are present

oneAvailable187188_2 = subset(RAWAttributesOnly, is.na(RAWAttributesOnly$smart_187_raw) == FALSE & is.na(RAWAttributesOnly$smart_188_raw) == TRUE)

# When only 188 attribute is available, there are no data 

# It seems that SMART attribute 190 would also be missing whenever SMART attribute
# 187 and SMART attribute 188 are missing, therefore we would check if this 
# is the case
available187188190 = subset(RAWAttributesOnly, is.na(RAWAttributesOnly$smart_187_raw) == FALSE 
                            & is.na(RAWAttributesOnly$smart_188_raw) == FALSE &
                              is.na(RAWAttributesOnly$smart_190_raw) == FALSE)

# From checking, it seems that these 3 SMART attributes are somewhat closely related
# since whenever one is missing, the

# Based on these EDA, we would procceed by removing the SMART attribute 187 and
# SMART attribute 188 and 190

# Removing the Attribute 187 and Attribute 188
RAWAttributesOnly = RAWAttributesOnly[,-c(14,15)]

NormalisedAttributesOnly = NormalisedAttributesOnly[,-c(14,15)]

# Now we are looking at the remaining SMART attributes and checking whether 
# other SMART attributes might be exclusive to certain models

onlyAvailable1 = subset(RAWAttributesOnly, is.na(RAWAttributesOnly$smart_1_raw) == FALSE)

# For the SMART Attribute 1, all models are present, therefore, we could still 
# use this variable for prediction

# Now we look at SMART attribute 240 which has 23.515% missing values
available240 = subset(RAWAttributesOnly, is.na(RAWAttributesOnly$smart_240_raw) == FALSE)

# It seems there are 31 models which has this SMART attribute recorded, so it might
# be worth considering to either remove or include this SMART attribute

# Now we look at SMART attribute 241 which has 33.855% missing values
available241 = subset(RAWAttributesOnly, is.na(RAWAttributesOnly$smart_241_raw) == FALSE)

# There are 33 models that are recorded to have SMART attribute 241

# Now we look at SMART Attribute 242 which has 33.85682656% missing values
available242 = subset(RAWAttributesOnly, is.na(RAWAttributesOnly$smart_242_raw) == FALSE)

# It seems that other than ST models, there are other models which has 
# SMART attribute 242

nas241242 = subset(RAWAttributesOnly, is.na(RAWAttributesOnly$smart_241_raw) == FALSE & 
                     is.na(RAWAttributesOnly$smart_242_raw) == TRUE)

# Checking the PCA

# Turn the Failure column as factor
RAWAttributesOnly$failure = as.factor(RAWAttributesOnly$failure)

# Since PCA for the whole data is too massive, we only try using one model

ST12000NM001GData = subset(RAWAttributesOnly, RAWAttributesOnly$model == "ST12000NM001G")

# PCA for ST12000NM001G Model
PCA_ST12000NM001G = prcomp(ST12000NM001GData[,4:25], scale = TRUE)


# Get only the ST models 

check_data_frame = Q4_combined_data[grep(pattern = "^ST", Q4_combined_data$model), ]

summary(check_data_frame)
      

test_na = check_data_frame[is.na(check_data_frame$smart_187_raw) == TRUE, ]
