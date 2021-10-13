# Function to convert the data frame into a 3D array 


array_transformation_function = function(specified_dataframe){
  # Create a data frame which grouped all the observations by their serial number
  # This data frame is meant to check the number of serial numbers matched up
  # at the end
  training_x_grouped = specified_dataframe[ , -c(5)] %>% 
  group_by(serial_number) %>% nest() 

  # Transfrming the data into array form using the simplify2array function
  training_x_array = simplify2array(by(specified_dataframe[,-c(1,3,4,5,7,12)],specified_dataframe[,-c(1,3,4,5,7,12)]$serial_number,as.matrix))

  # Remove the serial number as one of the variable
  training_x_array = training_x_array[,-1,]

  # Reshape the 3D array
  training_x_array_reshaped = aperm(training_x_array,c(3,1,2))

  # Change it into numeric
  training_x_array_numeric = as.numeric(training_x_array_reshaped)

  training_x_array_final = array(training_x_test_array_numeric, dim= c(nrow(training_x_grouped),dim(training_x_array_reshaped)[2],dim(training_x_array_reshaped)[3]))

  # For the y or target variable
  # Now to set the target variable (y) into array
  training_y_grouped = specified_dataframe[,c(2,5)] %>%
    group_by(serial_number)

  # Only selecting the unique value of the target variable. Meaning only 1 output per
  # serial number
  training_y_serial = unique(training_y_grouped)

  training_y_serial_pre_array = simplify2array(by(training_y_serial,unique(specified_dataframe$serial_number), as.matrix))


  training_y_array = array(training_y_serial_pre_array[,-1,], dim=c(1,1,nrow(training_y_grouped)))

  training_y_array_reshaped = aperm(training_y_array,c(3,1,2))

  training_y_array_numeric = as.numeric(training_y_array_reshaped)

  training_y_array_final = array(training_y_array_numeric, dim=c(nrow(training_y_grouped),1,1))
  
  # Return as list
  data_array = list(x_array = training_x_array_final, y_array = training_y_array_final)
  
  return(data_array)
}

cache('array_transformation_function')
