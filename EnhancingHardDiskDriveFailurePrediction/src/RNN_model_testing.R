# RNN Model Testing
library(ProjectTemplate)
load.project()

# Load the Keras library
library(keras)
library(caret)
library(tidyverse)

# Transform the existing training and test data set into a 3D array of 
# (observations, timestamp, variables) format. 

# We transform the data to that of a list of lists based on the serial number

training_x = training_transformed_2[ , -c(1,5,26)] %>%
  group_by(serial_number) %>% nest()

test_training_data = simplify2array(by(training_transformed_2[,-c(1,5,26)],training_transformed_2[,-c(1,5,26)]$serial_number,as.matrix))
# Transform it into array
training_X_array = split(training_transformed_2[,-c(1,5,26)],training_transformed_2$serial_number)

training_X_array = as.array(training_X_array)

test = as.matrix(training_x)
dim(test)

training_y = training_transformed_2[ ,c(2,5)] %>%
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

