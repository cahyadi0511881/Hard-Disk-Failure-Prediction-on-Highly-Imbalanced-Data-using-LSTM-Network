# Oversampling for imbalanced data set

library(ProjectTemplate)
load.project()

# We would still use the same model which is the ST4000DM000 model for the training
# Since the data is imblanced with more healthy rather than failure, we would 
# then apply oversampling as one of the way to mitigate the issue


# First, we take the serial number of all hard disk that experience failuyre
training_oversampled_failed_raw = filter(normal_training_data_scaled, failure == 1 )

# Get the serial number
failed_training_serial_number = unique(training_oversampled_failed_raw$serial_number)

# There are 198 failed serial number compared to 18943 non-failed serial number.
# Roughly 95 times the number of failure

# Set up a copy data which contains only data for the failed serial number. This copy
# would then multiplied to balanced the data

failed_oversampled_copied = data.frame()

for ( i in 1:95){
  
  tempDataStorage = data.frame()
  tempDataStorage = training_oversampled_failed_raw
  tempDataStorage$serial_number = paste (tempDataStorage$serial_number , as.character(i), sep=',' )
  failed_oversampled_copied = rbind(failed_oversampled_copied,tempDataStorage)
  
}


non_failed_oversampled = filter(normal_training_data_scaled, !(serial_number %in% failed_training_serial_number) )

training_oversampled = rbind(failed_oversampled_copied,non_failed_oversampled)

# Apply the similar procedure as the RNN model testing file

training_x = training_oversampled[ , -c(5)] %>% 
  group_by(serial_number) %>% nest() 

# Transforming the data into 3D array using simplify2array function (Option 2)
training_x_array = simplify2array(by(training_oversampled[,-c(1,3,4,5,7,12)],training_oversampled[,-c(1,3,4,5,7,12)]$serial_number,as.matrix))

# Remove the serial number as one of the variable
training_x_array = training_x_array[,-1,]

# Reshape the 3D array
training_x_test_array = aperm(training_x_array,c(3,1,2))

# Change it into numeric
training_x_test_array_numeric = as.numeric(training_x_test_array)

training_x_test_array_numeric = array(training_x_test_array_numeric, dim= c(nrow(training_x),dim(training_x_test_array)[2],dim(training_x_test_array)[3]))

# For the y or target variable
# Now to set the target variable (y) into array
training_y_serial = training_oversampled[,c(2,5)] %>%
  group_by(serial_number)

# Only selecting the unique value of the target variable. Meaning only 1 output per
# serial number
training_y_serial = unique(training_y_serial)

training_y_serial_pre_array = simplify2array(by(training_y_serial,unique(training_oversampled$serial_number), as.matrix))


training_y_array = array(training_y_serial_pre_array[,-1,], dim=c(1,1,nrow(training_y_serial)))

training_y_test_array = aperm(training_y_array,c(3,1,2))

training_y_test_array_numeric = as.numeric(training_y_test_array)

training_y_test_array_numeric = array(training_y_test_array_numeric, dim=c(nrow(training_y_serial),1,1))


# Use the same test data as previous tests

# Get the serial number in test data
serial_number_for_test = unique(test_data$serial_number)

# Set up the raw test data
raw_test_data = filter(All_Quarter_ST4000DM000_2019, serial_number %in% serial_number_for_test)

pre_processed_test_data = prediction_days_processing_normal(raw_test_data,100,10)
pre_processed_test_data_scaled = pre_processed_test_data %>% mutate_at(c(6,8:11,13:25), funs(c(scale(.))))


test_x = pre_processed_test_data_scaled[,-c(5)] %>% 
  group_by(serial_number) %>% nest() 

# Transforming the data into 3D array using simplify2array function (Option 2)
test_x_array = simplify2array(by(pre_processed_test_data_scaled[,-c(1,3,4,5,7,12)],
                                 pre_processed_test_data_scaled[,-c(1,3,4,5,7,12)]$serial_number,as.matrix))

# Remove the serial number as one of the variable
test_x_array = test_x_array[,-1,]

# Reshape the 3D array
test_x_test_array = aperm(test_x_array,c(3,1,2))

# Change it into numeric
test_x_test_array_numeric = as.numeric(test_x_test_array)

test_x_test_array_numeric = array(test_x_test_array_numeric, dim= c(nrow(test_x),dim(test_x_test_array)[2],dim(test_x_test_array)[3]))

# For the y or target variable
# Now to set the target variable (y) into array
test_y_serial = pre_processed_test_data_scaled[,c(2,5)] %>%
  group_by(serial_number)

# Only selecting the unique value of the target variable. Meaning only 1 output per
# serial number
test_y_serial = unique(test_y_serial)

test_y_serial_pre_array = simplify2array(by(test_y_serial,unique(pre_processed_test_data_scaled$serial_number), as.matrix))


test_y_array = array(test_y_serial_pre_array[,-1,], dim=c(1,1,nrow(test_y_serial)))

test_y_test_array = aperm(test_y_array,c(3,1,2))

test_y_test_array_numeric = as.numeric(test_y_test_array)

test_y_test_array_numeric = array(test_y_test_array_numeric, dim=c(nrow(test_y_serial),1,1))


# Load the Keras library
library(keras)
library(kerasR)
library(caret)
library(tidyverse)
library(tensorflow)

set.seed(123)

model <- keras_model_sequential()

model %>% layer_lstm(input_shape = dim(training_x_test_array_numeric[2:3]),
                     units = 100, return_sequences = TRUE) %>%
  layer_dropout(rate=0.25) %>%
  layer_lstm(units = 100, return_sequences = TRUE) %>%
  layer_lstm(units=50, return_sequences = FALSE) %>%
  layer_dense(1,activation = "sigmoid") 

model %>% compile(loss = 'binary_crossentropy', 
                  optimizer = optimizer_adam(lr=0.001), 
                  metrics = c('accuracy'))

trained_model <- model %>% fit(
  x = training_x_test_array_numeric, # Predictors sequence
  y = training_y_test_array_numeric, # Target sequence
  batch_size = 50, # How many samples passed into the data at once
  epochs = 100, # Number of times the model look at the data
  validation_split = 0.25)# Splitting the data into validation set

y_pred_2 <- model %>% predict(test_x_test_array_numeric) 

y_pred = ifelse(y_pred_2 > 0.5, 1, 0)

table(test_y_test_array_numeric, y_pred)


