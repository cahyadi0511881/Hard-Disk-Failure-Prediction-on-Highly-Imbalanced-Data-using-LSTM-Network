# Testing the new function 

training_data_test = prediction_days_processing(All_2020_ST4000DM000,60,10)

failed_data = filter(All_2020_ST4000DM000, failure == 1)
non_failed_data = filter(All_2020_ST4000DM000, !(serial_number %in% unique(failed_data$serial_number)))

non_failed_processed = data.frame()

for (s in unique(non_failed_data$serial_number)){
  
  # Set up the temporary data frame within the loop that would refresh at 
  # every start of the loop
  temp_non_failed_data      = data.frame()
  iteration_non_failed_data = data.frame()
  
  # Subset per serial number 
  temp_non_failed_data = filter(non_failed_data, serial_number == s)
  
  # Remove any serial hard disk that does not have enough length of observations
  if (nrow(temp_non_failed_data) < 60){
    
    next
    
  }
  
  # We get the last n days of observations for the particular hard disk
  iteration_non_failed_data = head(temp_non_failed_data, n = 60)
  # We subtract the observations day to n days before the verdict (healthy or failed) 
  # or the last observation days
  iteration_non_failed_data = head(temp_non_failed_data, n = 60 - 10)
  
  # Combine everything at the final data frame
  
  non_failed_processed = rbind(non_failed_processed, iteration_non_failed_data)
  
}

# This function will use undersampling therefore we only take the same amount of
# healthy sample as the failed sample

non_failed_sampled_serial_number = sample(unique(non_failed_processed$serial_number) , 
                                          size = length(unique(training_data_failed$serial_number)))

sampled_healthy_data = filter(non_failed_processed, serial_number %in% non_failed_sampled_serial_number)

training_data = rbind(training_data_failed,sampled_healthy_data)

training_data_new_2 = training_data
cache('training_data_new_2')


# Convert to arrray

training_x = training_data_test[ , -c(5)] %>% 
  group_by(serial_number) %>% nest() 

# Transforming the data into 3D array using simplify2array function (Option 2)
training_x_array = simplify2array(by(training_data_test[,-c(1,3,4,5,7,12)],training_data_test[,-c(1,3,4,5,7,12)]$serial_number,as.matrix))

# Remove the serial number as one of the variable
training_x_array = training_x_array[,-1,]

# Reshape the 3D array
training_x_test_array = aperm(training_x_array,c(3,1,2))

# Change it into numeric
training_x_test_array_numeric = as.numeric(training_x_test_array)

training_x_test_array_numeric = array(training_x_test_array_numeric, dim= c(nrow(training_x),50,18))

# For the y or target variable
# Now to set the target variable (y) into array
training_y_serial = training_data_test[,c(2,5)] %>%
  group_by(serial_number)

# Only selecting the unique value of the target variable. Meaning only 1 output per
# serial number
training_y_serial = unique(training_y_serial)

training_y_serial_pre_array = simplify2array(by(training_y_serial,unique(training_data_test$serial_number), as.matrix))


training_y_array = array(training_y_serial_pre_array[,-1,], dim=c(1,1,nrow(training_y_serial)))

training_y_test_array = aperm(training_y_array,c(3,1,2))

training_y_test_array_numeric = as.numeric(training_y_test_array)

training_y_test_array_numeric = array(training_y_test_array_numeric, dim=c(nrow(training_y_serial),1,1))

# Load the Keras library
library(keras)
library(kerasR)
library(caret)
library(tidyverse)
library(tensorflow)

set.seed(123)

model <- keras_model_sequential()

model %>% layer_lstm(input_shape = dim(training_x_test_array_numeric[2:3]),
                     units = 50, return_sequences = TRUE) %>%
  layer_lstm(units = 20, return_sequences = TRUE) %>%
  layer_lstm(units = 10, return_sequences = FALSE) %>%
  layer_dense(1,activation = "sigmoid") 

model %>% compile(loss = 'binary_crossentropy', 
                  optimizer = 'adam', 
                  metrics = c('accuracy'))

trained_model <- model %>% fit(
  x = training_x_test_array_numeric, # Predictors sequence
  y = training_y_test_array_numeric, # Target sequence
  batch_size = 50, # How many samples passed into the data at once
  epochs = 100, # Number of times the model look at the data
  validation_split = 0.25,
  shuffle = TRUE)# Splitting the data into validation set

y_pred_2 <- model %>% predict(training_x_test_array_numeric) 

y_pred = ifelse(y_pred_2 > 0.5, 1, 0)



table(training_y_test_array_numeric, y_pred)