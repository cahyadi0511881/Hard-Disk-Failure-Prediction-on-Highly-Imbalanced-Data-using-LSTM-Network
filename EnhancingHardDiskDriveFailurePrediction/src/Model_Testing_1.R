library(ProjectTemplate)
load.project()

# Since most of the papers used only the RAW values of the S.M.A.R.T Attributes,
# we would only use the RAW attributes as well


# Subsetting only the RAW attributes
Q4_2020_data_processed = Q4_2020_data_processed[,1:25]


# Removing unnecessary load on the computer memory
gc()

# To test the model, we used only the most commonly found hard disk drive model
# within this data set

# Check the Model count
table(Q4_2020_data_processed$model)

# It seems that ST4000DM000 model is the most frequent model, therefore we would test
# our prediction model using this model as the training variable

Q4_2020_data_ST4000DM000 = filter(Q4_2020_data_processed, model ==  "ST4000DM000")

# Remove the negative capacity bytes out of the data
Q4_2020_data_ST4000DM000 = Q4_2020_data_ST4000DM000[Q4_2020_data_ST4000DM000$capacity_bytes > 0, ]

# Check the data set using summary 
summary(Q4_2020_data_ST4000DM000)


# Clean the unnecessary space once more
gc()


# Put all the serial number as a vector
ST4000DM000Serial_Number = unique(Q4_2020_data_ST4000DM000$serial_number)

# Loop to only take last 10 days of each Hard disk. This time use the Q4_combined_data
# which also takes into account the last 10 days of Q3. In case there are hard disks
# which failed on the first day of Q4


# Loop all the serial number

# Setting up the final data set
ST4000DM000_Last10DaysData = data.frame()
LessThan10DaysSerialNumber = vector()

for(s in ST4000DM000Serial_Number){
  
  # Set up the temporary data storage for each hard disk serial number
  tempSerialDataStorage = data.frame()
  tempLast10DataStorage = data.frame()
  tempSerialDataStorage = filter(Q4_combined_data, serial_number == s)
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

# Transform the failure as factor

ST4000DM000_Last10DaysData$failure = as.factor(ST4000DM000_Last10DaysData$failure)


# Separate the serial numbers into training and test data

# Store the serial number within a vector
ST4000DM000Serial_Number = unique(ST4000DM000_Last10DaysData$serial_number)

# Store a vector which hold number from 1 up to 19022 for index
serial_number_index = 1:19022

# Create a data frame 

ST4000DM000_Serial_Indexing = data.frame(Serial_number = ST4000DM000Serial_Number,
                                         index = serial_number_index)

# Split the data frame

library(caTools)

set.seed(123)

ST4000DM000_Serial_Indexing = ST4000DM000_Serial_Indexing[,1:2]

ST4000DM000_Serial_Indexing$splt = sample.split(ST4000DM000_Serial_Indexing$index, SplitRatio = 0.8)

# Create the data frame which contains all the training serial number
training_serial_number_data = filter(ST4000DM000_Serial_Indexing, splt == "TRUE")

# Extract only the serial number as vector
training_serial_number = training_serial_number_data$Serial_number

# Create the data frame which contains all the test serial number
test_serial_number_data = filter(ST4000DM000_Serial_Indexing, splt == "FALSE")

# Extract only the serial number as vector
test_serial_number = test_serial_number_data$Serial_number


# Produce the training data frame

training_ST4000DM000 = filter(ST4000DM000_Last10DaysData, serial_number %in% training_serial_number)


# Produce the test data frame

test_ST4000DM000 = filter(ST4000DM000_Last10DaysData, serial_number %in% test_serial_number)




lib
# Set up the failure as a factor instead of continuous variable
ST4000DM000_Last10DaysData$failure = as.factor(ST4000DM000_Last10DaysData$failure)

# Set up the X and Y 
X = ST4000DM000_Last10DaysData[,c(1:4,6:25)]
Y = ST4000DM000_Last10DaysData$failure





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

# Separate the training and test set
X_train_3d = ThreeD_array_X[ThreeD_array_X$serial_number %in% unique(training_ST4000DM000$serial_number), ]

Y_train_3d = ThreeD_array_Y[ThreeD_array_Y$serial_number %in% unique(training_ST4000DM000$serial_number), ]

X_test_3d = ThreeD_array_X[ThreeD_array_X$serial_number %in% unique(test_ST4000DM000$serial_number), ]


library(randomForest)

training_ST4000DM000$failure = as.factor(training_ST4000DM000$failure)

test_ST4000DM000$failure = as.factor(test_ST4000DM000$failure)

randomForestModel = randomForest(x = training_ST4000DM000[, -5],
             y = training_ST4000DM000[,5], xtest = test_ST4000DM000[,-5], 
             ytest = test_ST4000DM000[,5], importance = TRUE, ntree = 500)
randomForestModel$confusion

# There are 0 True negative out of 64 supossed negative. Therefore we might consider
# transforming the data so that for all the failed hard disk, the failure is set to 1
# if it experience failure

ST4000DM000_Last10DaysData_transformed = data.frame()

for (i in unique(ST4000DM000_Last10DaysData$serial_number)){
  
  tempDataStorage = data.frame()
  tempDataStorage = filter(ST4000DM000_Last10DaysData, serial_number == i )
  
  if(tempDataStorage[10,5] == 1){
    
    tempDataStorage$failure = 1
    
  }
  
  ST4000DM000_Last10DaysData_transformed = rbind(ST4000DM000_Last10DaysData_transformed, tempDataStorage)
  
}

ST4000DM000_Last10DaysData_transformed$failure = as.factor(ST4000DM000_Last10DaysData_transformed$failure)

# Separate into training and test

training_transformed = filter(ST4000DM000_Last10DaysData_transformed, 
                              serial_number %in% unique(training_ST4000DM000$serial_number))


test_transformed = filter(ST4000DM000_Last10DaysData_transformed, 
                          serial_number %in% unique(test_ST4000DM000$serial_number))


# Redo the random forest classification using this transformed data to check whether the accuracy improved

randomForestModel2 = randomForest(x = training_transformed[, -5],
                                 y = training_transformed[,5], xtest = test_transformed[,-5], 
                                 ytest = test_transformed[,5], importance = TRUE, ntree = 500)

# It seems transforming the failed hard disks as 1 makes the model much better

randomForestImportance = randomForestModel2$importance

randomForestImportanceSD = randomForestModel2$importanceSD

# Create an MCC function that would be used to evaluate models
MCC = function(TP,TN,FP,FN){
  MCC_score = (TP*TN - FP*FN) / sqrt((TP+FN)*(TP+FP)*(TN+FP)*(TN+FN))
  return(MCC_score)
}

cache("MCC")

# MCC for the first random forest model (Without changing the failure condition
# for failed hard disk to 1 for all observations)
MCC(151530,633,0,7)

# MCC for the second random forest model (After changing the failure condition 
# for failed hard disk to 1 for all observations)
MCC(152105,0,1,64)

# The random forest classifier used the date as the most important feature,
# therefore try to change the date into days instead ranging from 1-10 

ST4000DM000_Last10DaysData_transformed_2 = data.frame()

for (i in unique(ST4000DM000_Last10DaysData$serial_number)){
  
  tempDataStorage = data.frame()
  tempDataStorage = filter(ST4000DM000_Last10DaysData, serial_number == i )
  
  if(tempDataStorage[10,5] == 1){
    
    tempDataStorage$failure = 1
    
  }
  
  tempDataStorage = tempDataStorage[order(tempDataStorage$date),]
  tempDataStorage$day = c(1:10)
  
  ST4000DM000_Last10DaysData_transformed_2 = rbind(ST4000DM000_Last10DaysData_transformed_2, tempDataStorage)
  
}

ST4000DM000_Last10DaysData_transformed$failure_2 = as.factor(ST4000DM000_Last10DaysData_transformed_2$failure)

# Separate into training and test

training_transformed_2 = filter(ST4000DM000_Last10DaysData_transformed_2, 
                              serial_number %in% unique(training_ST4000DM000$serial_number))


test_transformed_2 = filter(ST4000DM000_Last10DaysData_transformed_2, 
                          serial_number %in% unique(test_ST4000DM000$serial_number))

randomForestModel3 = randomForest(x = training_transformed_2[, -c(1,5)],
                                  y = training_transformed_2[,5], xtest = test_transformed_2[,-c(1,5)], 
                                  ytest = test_transformed_2[,5], importance = TRUE, ntree = 500)


MCC(623,15130,0,17)

# Check whether there is a similar serial number in test and training set

similarSerial_number = filter(training_transformed_2, serial_number %in% unique(test_transformed_2$serial_number))

# There are no double serial number

# RNN modelling
# Use the library Keras for creating the RNN model

library(keras)
library(caret)

# We transform the data to that of a list of lists based on the serial number

training_x_transformed = training_transformed[ , -5] %>%
  group_by(serial_number) %>%
  nest()

# Transform it into array
training_X_transformed = array(training_x_transformed$data, dim = c(15217,10,25))

training_y_transformed = training_transformed[ ,c(2,5)] %>%
  group_by(serial_number) %>%
  nest()

# Transform it into array
training_Y_transformed = array(training_y_transformed$data, dim = c(1527,10,1))

# Similar transformation for the test set
test_x_transformed = test_transformed[ , -5] %>%
  group_by(serial_number) %>%
  nest()

# Transform into 3D array
test_X_transformed = array(test_x_transformed$data, dim = c(3805,10,25))

test_y_transformed = test_transformed[ ,c(2,5)] %>%
  group_by(serial_number) %>%
  nest()

# Transform into 3D array
test_Y_transformed = array(test_y_transformed$data, dim = c(3805,10,1))


dim(training_X_transformed)

dim(test_X_transformed)

# set some parameters for our model
max_len <- 10 # the number of previous examples we'll look at
batch_size <- 72 # number of sequences to look at at one time during training
total_epochs <- 100 # how many times we'll look @ the whole dataset while training our model

# set a random seed for reproducability
set.seed(123)

# Set the model

?keras_

model <- keras_model_sequential()

model %>% 
  layer_simple_rnn(units = 6)



