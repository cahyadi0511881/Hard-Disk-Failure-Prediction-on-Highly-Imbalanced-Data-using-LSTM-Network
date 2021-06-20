library(ProjectTemplate)
load.project()

# Since most of the papers used only the RAW values of the S.M.A.R.T Attributes,
# we would only use the RAW attributes as well


# Subsetting only the RAW attributes
Q4_2020_data_processed = Q4_2020_data_processed[,1:25]

# Since some of the hard disk drive experience failure at the first day of Q4,
# try adding the last 10 days of Q3 data. However, we might need to be careful 
# since the data from Q3 has different S.M.A.R.T attributes number than that of 
# the Q4

# Removing unnecessary load on the computer memory
gc()

# To test the model, we used only the most commonly found hard disk drive model
# within this data set

# Check the Model count
table(Q4_2020_data_processed$model)

# It seems that ST4000DM000 model is the most frequent model, therefore we would test
# our prediction model using this model as the training variable

Q4_2020_data_processed = filter(Q4_2020_data_processed, model ==  "ST4000DM000")

# Check the data set using summary 
summary(Q4_2020_data_processed)

# It seems that there are 126 missing values, therefore it might be worth checking about the negative capacity bytes

# Removing the negative capacity record
Q4_2020_data_processed = filter(Q4_2020_data_processed, capacity_bytes > 0 )

# Check the missing values again
summary(Q4_2020_data_processed)

# It seems that there no NA's when the negative capacity records are deleted

# Clean the unnecessary space once more
gc()

# Check how many individual hard disk within this data set
length(unique(Q4_2020_data_processed$serial_number))
# There are 19022 distinct serial number

# Put all the serial number as a vector
ST4000DM000Serial_Number = unique(Q4_2020_data_processed$serial_number)

# Loop to only take last 10 days of each Hard disk

# Use the xts library
library(xts)

# Loop all the serial number

# Setting up the final data set
ST4000DM000_Last10DaysData = data.frame()
LessThan10DaysSerialNumber = vector()

for(s in ST4000DM000Serial_Number){
  
  # Set up the temporary data storage for each hard disk serial number
  tempSerialDataStorage = data.frame()
  tempLast10DataStorage = data.frame()
  tempSerialDataStorage = filter(Q4_2020_data_processed, serial_number == s)
  tempLast10DataStorage = tail(tempSerialDataStorage, n = 10)
  
  if (nrow(tempLast10DataStorage) < 10){
    
    LessThan10DaysSerialNumber = append(LessThan10DaysSerialNumber, unique(tempLast10DataStorage$serial_number))
    
  }
  
  
  ST4000DM000_Last10DaysData = rbind(ST4000DM000_Last10DaysData, tempLast10DataStorage)
  
}

# Check how many serial number is not having 10 days

LessThan10DaysSerialNumber = as.vector(LessThan10DaysSerialNumber)

LessThan10DaysSerialNumber$LessThan10DaysSerialNumber

# Check how many days are present for this 6 variables

check_data = ST4000DM000_Last10DaysData[ST4000DM000_Last10DaysData$serial_number %in% LessThan10DaysSerialNumber$LessThan10DaysSerialNumber,]

# Cache the 10 days data set for future analyses

cache("ST4000DM000_Last10DaysData")

# RNN modelling
# Use the library Keras for creating the RNN model

lib
# Set up the failure as a factor instead of continuous variable
ST4000DM000_Last10DaysData$failure = as.factor(ST4000DM000_Last10DaysData$failure)

# Set up the X and Y 
X = ST4000DM000_Last10DaysData[,c(1:4,6:25)]
Y = ST4000DM000_Last10DaysData$failure



# set some parameters for our model
max_len <- 10 # the number of previous examples we'll look at
batch_size <- 32 # number of sequences to look at at one time during training
total_epochs <- 15 # how many times we'll look @ the whole dataset while training our model

# set a random seed for reproducability
set.seed(123)

# Set the model

library(keras)
library(caret)

model <- keras_model_sequential()

model %>% 
  layer_simple_rnn(units = 6)


# Convert the ST4000DM000 data into a 3D array

library(tidyverse)
ThreeD_array_data = ST4000DM000_Last10DaysData %>%
  group_by(serial_number) %>%
  nest()
ThreeD_array_X = ST4000DM000_Last10DaysData[ , -5] %>%
  group_by(serial_number) %>%
  nest()
ThreeD_array_Y = ST4000DM000_Last10DaysData[,c(2,5)] %>%
  group_by(serial_number) %>%
  nest()

# For the X or predictor variable, only take the data
X_test = ThreeD_array_X$data
X_test_matrix = as.matrix(X_test)

# For the Y or the target variable, only take the data
Y_test = ThreeD_array_Y$data
Y_test_matrix = as.matrix(Y_test)


library(randomForest)

randomForest(ST4000DM000_Last10DaysData, x = ST4000DM000_Last10DaysData[,c(4,6:25)],
             y = ST4000DM000_Last10DaysData[,5], importance = TRUE, ntree = 50)
