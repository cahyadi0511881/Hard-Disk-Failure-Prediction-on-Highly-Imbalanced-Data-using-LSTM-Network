# RNN Model Testing
library(ProjectTemplate)
load.project()

# Load the Keras library
library(keras)
library(kerasR)
library(caret)
library(tidyverse)
library(tensorflow)

# Transform the existing training and test data set into a 3D array of 
# (observations, timestamp, variables) format. 

# We transform the data to that of a list of lists based on the serial number 
# using nest() command (Option 1)

training_x = training_transformed_2[ , -c(5,26)] %>% 
  group_by(serial_number) %>% nest() 

training_x$data = as.matrix(training_x$data)

# Transforming the data into 3D array using simplify2array function (Option 2)
training_x_array = simplify2array(by(training_transformed_2[,-c(1,3,5,26)],training_transformed_2[,-c(1,3,5,26)]$serial_number,as.matrix))

# Remove the serial number as one of the variable
training_x_array = training_x_array[,-1,]

# Reshape the 3D array
training_x_test_array = aperm(training_x_array,c(3,1,2))

# Change it into numeric
training_x_test_array_numeric = as.numeric(training_x_test_array)

training_x_test_array_numeric = array(training_x_test_array_numeric, dim= c(15217,10,21))

# Now to set the target variable (y) into array
training_y_serial = training_transformed_2[,c(2,5)] %>%
  group_by(serial_number)

# Only selecting the unique value of the target variable. Meaning only 1 output per
# serial number
training_y_serial = unique(training_y_serial)

training_y_serial_pre_array = simplify2array(by(training_y_serial,unique(training_transformed_2$serial_number), as.matrix))


training_y_array = array(training_y_serial_pre_array[,-1,], dim=c(1,1,15217))

training_y_test_array = aperm(training_y_array,c(3,1,2))

training_y_test_array_numeric = as.numeric(training_y_test_array)

training_y_test_array_numeric = array(training_y_test_array_numeric, dim=c(15217,1,1))

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
input_shape_ = dim(training_x_test_array_numeric)[2:3]

model <- keras_model_sequential()

model %>% layer_lstm(input_shape = input_shape_, units = 100,
                     return_sequences = TRUE, dropout = 0.01) %>% 
              layer_lstm(units = 100, return_sequences =  TRUE) %>% 
              layer_lstm(units = 100, return_sequences =  TRUE) %>% 
              layer_lstm(units = 50, return_sequences = FALSE) %>%
                    layer_dense(units = dim(training_y_test_array_numeric)[2], activation = "sigmoid")

model %>% compile(loss = 'binary_crossentropy', 
                  optimizer = 'Adam', 
                  metrics = c('accuracy'),
                  sample_weight_mode = 'temporal')

trained_model <- model %>% fit(
  x = training_x_test_array_numeric, # sequence we're using for prediction 
  y = training_y_test_array_numeric, # sequence we're predicting
  batch_size = 72, # how many samples to pass to our model at a time
  epochs = 100, # how many times we'll look @ the whole dataset
  validation_split = 0.25) # how much data to hold out for testing as we go along

y_pred_2 <- model %>% predict(test_x_test_array_numeric) 

y_pred = ifelse(y_pred_2 > 0.5, 1, 0)



table(test_y_test_array_numeric, y_pred)



