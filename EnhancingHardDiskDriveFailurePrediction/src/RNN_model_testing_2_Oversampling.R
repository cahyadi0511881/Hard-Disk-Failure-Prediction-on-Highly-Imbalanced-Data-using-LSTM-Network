# Oversampling for imbalanced data set

library(ProjectTemplate)
load.project()

# We would still use the same model which is the ST4000DM000 model for the training
# Since the data is imblanced with more healthy rather than failure, we would 
# then apply oversampling as one of the way to mitigate the issue


# First, we take the serial number of all hard disk that experience failuyre
training_ST4000DM000_Failed_serial = (filter(training_ST4000DM000, failure == 1))$serial_number

# Set up a copy data which contains only data for the failed serial number. This copy
# would then multiplied to balanced the data

Failed_Serial_ST4000DM000_Copy = filter(training_transformed_2, serial_number %in% training_ST4000DM000_Failed_serial)


training_ST4000DM000_Oversampled_1 = training_transformed_2


for ( i in 1:235){
  
  tempDataStorage = data.frame()
  tempDataStorage = Failed_Serial_ST4000DM000_Copy
  tempDataStorage$serial_number = paste (tempDataStorage$serial_number , as.character(i), sep=',' )
  training_ST4000DM000_Oversampled_1 = rbind(training_ST4000DM000_Oversampled_1,tempDataStorage)
  
}

failed_oversampled = filter(training_ST4000DM000_Oversampled_1, failure == 1)
length(unique(failed_oversampled$serial_number))

non_failed_oversampled = filter(training_ST4000DM000_Oversampled_1, !(serial_number %in% failed_oversampled$serial_number) )

# Apply the similar procedure as the RNN model testing file


# Load the Keras library
library(keras)
library(kerasR)
library(caret)
library(tidyverse)
library(tensorflow)

# We transform the data to that of a list of lists based on the serial number 
# using nest() command (Option 1)

training_x = training_ST4000DM000_Oversampled_1[ , -c(5,26)] %>% 
  group_by(serial_number) %>% nest() 

training_x$data = as.matrix(training_x$data)

# Transforming the data into 3D array using simplify2array function (Option 2)
training_x_array = simplify2array(by(training_ST4000DM000_Oversampled_1[,-c(1,3,5,26)],training_ST4000DM000_Oversampled_1[,-c(1,3,5,26)]$serial_number,as.matrix))

# Remove the serial number as one of the variable
training_x_array = training_x_array[,-1,]

# Reshape the 3D array
training_x_test_array = aperm(training_x_array,c(3,1,2))

# Change it into numeric
training_x_test_array_numeric = as.numeric(training_x_test_array)

training_x_test_array_numeric = array(training_x_test_array_numeric, dim= c(302570,10,21))

# Now to set the target variable (y) into array
training_y_serial = training_ST4000DM000_Oversampled_1[,c(2,5)] %>%
  group_by(serial_number)

# Only selecting the unique value of the target variable. Meaning only 1 output per
# serial number
training_y_serial = unique(training_y_serial)

training_y_serial_pre_array = simplify2array(by(training_y_serial,unique(training_ST4000DM000_Oversampled_1$serial_number), as.matrix))


training_y_array = array(training_y_serial_pre_array[,-1,], dim=c(1,1,302570))

training_y_test_array = aperm(training_y_array,c(3,1,2))

training_y_test_array_numeric = as.numeric(training_y_test_array)

training_y_test_array_numeric = array(training_y_test_array_numeric, dim=c(302570,1,1))

# Similar transformation for the test set
test_x_transformed = test_transformed[ , -5] %>%
  group_by(serial_number) %>%
  nest()

# Transform into 3D array
test_x_array = simplify2array(by(test_transformed_2[,-c(1,3,5,26)],test_transformed_2[,-c(1,3,5,26)]$serial_number, as.matrix))

# Remove the serial number from variables
test_x_array = test_x_array[,-1,]

# Reshape the array
test_x_test_array = aperm(test_x_array,c(3,1,2))

# Change it into numeric

test_x_test_array_numeric = as.numeric(test_x_test_array)

test_x_test_array_numeric = array(test_x_test_array_numeric, dim=c(3805,10,21))

# Now for the target variable

test_y_serial = test_transformed_2[,c(2,5)] %>%
  group_by(serial_number)

# Get only the unique values of this data frame. Meaning only take 1 output per serial
# number
test_y_serial = unique(test_y_serial)

# Transform it into array using simplify2array function

test_y_serial_pre_array = simplify2array(by(test_y_serial,unique(test_transformed_2$serial_number), as.matrix))

# Tidy it up to 1 1 3805 dimension

test_y_array = array(test_y_serial_pre_array[,-1,], dim=c(1,1,3805))

# Reshape the array
test_y_test_array = aperm(test_y_array, c(3,1,2))

# Convert it into numeric
test_y_test_array_numeric = as.numeric(test_y_test_array)

# Revert back to array
test_y_test_array_numeric = array(test_y_test_array_numeric, dim=c(3805,1,1))

# Set up the model for the RNN


model <- keras_model_sequential()

model %>% layer_lstm(input_shape = dim(training_x_test_array)[2:3], units = 100,
                     return_sequences = TRUE,dropout = 0.01) %>% 
  layer_lstm(units = 100, return_sequences = TRUE) %>%
  layer_lstm(units = 100, return_sequences = TRUE) %>% 
  layer_lstm(units = 50, return_sequences = FALSE) %>%
  layer_dense(units = dim(training_y_test_array)[2], activation = "sigmoid")

model %>% compile(loss = 'binary_crossentropy', 
                  optimizer = 'Adam', 
                  metrics = c('accuracy'))

trained_model <- model %>% fit(
  x = training_x_test_array_numeric, # Predictors sequence
  y = training_y_test_array_numeric, # Target sequence
  batch_size = 50, # How many samples passed into the data at once
  epochs = 25, # Number of times the model look at the data
  validation_split = 0.25)# Splitting the data into validation set

y_pred_2 <- model %>% predict(test_x_test_array_numeric, batch = 50) 

y_pred = ifelse(y_pred_2 > 0.5, 1, 0)



table(test_y_test_array_numeric, y_pred)
