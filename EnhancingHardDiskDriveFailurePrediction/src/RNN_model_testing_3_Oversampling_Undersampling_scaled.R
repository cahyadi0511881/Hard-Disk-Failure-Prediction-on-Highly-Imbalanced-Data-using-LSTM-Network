# RNN model testing 3

# Oversampling and undersampling

# In this file, the oversampling is done not by duplication, but by using slices
# of data from the observed failed attributes

# Load the library

library(ProjectTemplate)
load.project()

scaled_data = All_2020_ST4000DM000 %>% mutate_at(c("smart_1_raw", "smart_3_raw", "smart_4_raw", "smart_5_raw", 
                                                   "smart_7_raw","smart_9_raw","smart_10_raw", "smart_12_raw", 
                                                   "smart_187_raw","smart_188_raw","smart_190_raw","smart_192_raw",
                                                   "smart_193_raw", "smart_194_raw", "smart_197_raw", "smart_198_raw",
                                                   "smart_199_raw", "smart_240_raw", "smart_241_raw", "smart_242_raw"),
                                                 ~ (scale(.) %>% as.vector))

scaled_test = Q4_2019_ST4000DM000 %>% mutate_at(c("smart_1_raw", "smart_3_raw", "smart_4_raw", "smart_5_raw", 
                                                  "smart_7_raw","smart_9_raw","smart_10_raw", "smart_12_raw", 
                                                  "smart_187_raw","smart_188_raw","smart_190_raw","smart_192_raw",
                                                  "smart_193_raw", "smart_194_raw", "smart_197_raw", "smart_198_raw",
                                                  "smart_199_raw", "smart_240_raw", "smart_241_raw", "smart_242_raw"),
                                                ~ (scale(.) %>% as.vector))

failed_data = slice_days(scaled_data)

# Set the random seed
set.seed(123)

# Failed Serial number

failed_data_processing = filter(All_2020_ST4000DM000, failure == 1)
failed_serial_number = unique(failed_data_processing$serial_number)

non_failed_data = scaled_data[!(scaled_data$serial_number %in% failed_serial_number),]

# Sample only the same amount of non failed serial number with the failed serial number (1406)

non_failed_serial_number = unique(non_failed_data$serial_number)

non_failed_sample = sample(non_failed_serial_number, size = 1408)

non_failed_data_processed = filter(non_failed_data, serial_number %in% non_failed_sample)

# Take only the last 10 days 
non_failed_final = data.frame()
for (s in unique(non_failed_data_processed$serial_number)){
  
  # Set up the temporary data storage for each hard disk serial number
  tempSerialDataStorage = data.frame()
  tempLast10DataStorage = data.frame()
  tempSerialDataStorage = filter(non_failed_data_processed, serial_number == s)
  
  if (nrow(tempSerialDataStorage) < 10){
    
    next
    
  }
  
  tempLast10DataStorage = tail(tempSerialDataStorage, n = 10)
  
  non_failed_final = rbind(non_failed_final, tempLast10DataStorage)
  
}

# Transform the failure as factor
non_failed_final$failure = as.factor(non_failed_final$failure)

training_data_3 = rbind(failed_data,non_failed_final)

# Remove NA's in the data

training_data_3 = filter(training_data_3, capacity_bytes > 0)


# Apply the similar procedure as the RNN model testing file

# Load the Keras library
library(keras)
library(kerasR)
library(caret)
library(tidyverse)
library(tensorflow)

training_x = training_data_3[ , -c(5)] %>% 
  group_by(serial_number) %>% nest() 

# Transforming the data into 3D array using simplify2array function (Option 2)
training_x_array = simplify2array(by(training_data_3[,-c(1,3,5,7,12)],training_data_3[,-c(1,3,5,7,12)]$serial_number,as.matrix))

# Remove the serial number as one of the variable
training_x_array = training_x_array[,-1,]

# Reshape the 3D array
training_x_test_array = aperm(training_x_array,c(3,1,2))

# Change it into numeric
training_x_test_array_numeric = as.numeric(training_x_test_array)

training_x_test_array_numeric = array(training_x_test_array_numeric, dim= c(nrow(training_x),10,19))

# For the y or target variable
# Now to set the target variable (y) into array
training_y_serial = training_data_3[,c(2,5)] %>%
  group_by(serial_number)

# Only selecting the unique value of the target variable. Meaning only 1 output per
# serial number
training_y_serial = unique(training_y_serial)

training_y_serial_pre_array = simplify2array(by(training_y_serial,unique(training_data_3$serial_number), as.matrix))


training_y_array = array(training_y_serial_pre_array[,-1,], dim=c(1,1,nrow(training_y_serial)))

training_y_test_array = aperm(training_y_array,c(3,1,2))

training_y_test_array_numeric = as.numeric(training_y_test_array)

training_y_test_array_numeric = array(training_y_test_array_numeric, dim=c(nrow(training_y_serial),1,1))



# Now for the test data we use the Q4 2019 ST4000DM000 model to be predicted
Q4_2019_serial_number = unique(scaled_test$serial_number)

# Again the loop to get the last 10 days of each serial number

Q4_2019_Last_10Days = data.frame()

for(s9 in Q4_2019_serial_number){
  
  # Set up the temporary data storage for each hard disk serial number
  tempSerialDataStorage = data.frame()
  tempLast10DataStorage = data.frame()
  tempSerialDataStorage = filter(scaled_test, serial_number == s9)
  
  if (nrow(tempSerialDataStorage) < 10){
    
    next
    
  }
  
  tempSerialDataStorage = tempSerialDataStorage[order(tempSerialDataStorage$date),]
  tempLast10DataStorage = tail(tempSerialDataStorage, n = 10)
  tempLast10DataStorage = tempLast10DataStorage[order(tempLast10DataStorage$date),]
  
  if (tempLast10DataStorage[10,5] == 1){
    
    tempLast10DataStorage$failure = 1
    
  }
  
  Q4_2019_Last_10Days = rbind(Q4_2019_Last_10Days, tempLast10DataStorage)
  
}

# Change the failure section for the failed hard disk as one

Q4_2019_Last_10Days_Processed = Q4_2019_Last_10Days

Q4_2019_test_serial = sample(unique(Q4_2019_Last_10Days_Processed$serial_number), size = 2812)

Q4_2019_Last_10Days_Processed = filter(Q4_2019_Last_10Days_Processed, serial_number %in% Q4_2019_test_serial)

Q4_2019_Last_10Days_Processed$failure = as.factor(Q4_2019_Last_10Days_Processed$failure)

# Similar transformation for the test set
test_x_transformed = Q4_2019_Last_10Days_Processed[ , -5] %>%
  group_by(serial_number) %>%
  nest()

# Transform into 3D array
test_x_array = simplify2array(by(Q4_2019_Last_10Days_Processed[,-c(1,3,5,7,12)],Q4_2019_Last_10Days_Processed[,-c(1,3,5,7,12)]$serial_number, as.matrix))

# Remove the serial number from variables
test_x_array = test_x_array[,-1,]

# Reshape the array
test_x_test_array = aperm(test_x_array,c(3,1,2))

# Change it into numeric

test_x_test_array_numeric = as.numeric(test_x_test_array)

test_x_test_array_numeric = array(test_x_test_array_numeric, dim=c(nrow(test_x_transformed),10,19))

# Now for the target variable

test_y_serial = Q4_2019_Last_10Days_Processed[,c(2,5)] %>%
  group_by(serial_number) 

# Get only the unique values of this data frame. Meaning only take 1 output per serial
# number
test_y_serial = unique(test_y_serial)

# Transform it into array using simplify2array function

test_y_serial_pre_array = simplify2array(by(test_y_serial,unique(Q4_2019_Last_10Days_Processed$serial_number), as.matrix))

# Tidy it up to 1 1 2812

test_y_array = array(test_y_serial_pre_array[,-1,], dim=c(1,1,nrow(test_y_serial)))

# Reshape the array
test_y_test_array = aperm(test_y_array, c(3,1,2))

# Convert it into numeric
test_y_test_array_numeric = as.numeric(test_y_test_array)

# Revert back to array
test_y_test_array_numeric = array(test_y_test_array_numeric, dim=c(nrow(test_y_serial),1,1))


# Building the Neural network

set.seed(123)

model <- keras_model_sequential()

model %>% layer_lstm(input_shape = dim(training_x_test_array_numeric)[2:3], units = 210,
                     return_sequences = TRUE) %>% 
  layer_dropout(rate = 0.01) %>%
  layer_lstm(units = 50, return_sequences = FALSE ) %>%
  layer_dense(units = 1, activation = "sigmoid") 
  
model %>% compile(loss = 'binary_crossentropy', 
                  optimizer = 'adam', 
                  metrics = c('accuracy'))

trained_model <- model %>% fit(
  x = training_x_test_array_numeric, # Predictors sequence
  y = training_y_test_array_numeric, # Target sequence
  batch_size = 10, # How many samples passed into the data at once
  epochs = 20, # Number of times the model look at the data
  validation_split = 0.25, # Splitting the data into validation set
  shuffle = FALSE)

y_pred_2 <- model %>% predict(test_x_test_array_numeric) 

y_pred = ifelse(y_pred_2 > 0.5, 1, 0)



table(test_y_test_array_numeric, y_pred)

