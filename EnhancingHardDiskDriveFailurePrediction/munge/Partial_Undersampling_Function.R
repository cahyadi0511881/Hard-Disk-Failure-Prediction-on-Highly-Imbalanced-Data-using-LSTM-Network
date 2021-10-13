# Function for 4:1 ratio partial undersampling method

prediction_days_processing_undersampled_x4 = function(dataframe,observation_days,prediction_days){
  
  # First subset the failed data
  failed_hard_disk = filter(dataframe, failure == 1)
  # Extract the serial numbers of the failed hard disk
  failed_hard_disk_serial_number = unique(failed_hard_disk$serial_number)
  
  # Get the data frame that contains all the hard disk which eventually will fail
  failed_data = filter(dataframe, serial_number %in% failed_hard_disk_serial_number)
  
  # Convert the failure status as factor then change it to 1
  failed_data$failure = as.factor(failed_data$failure)
  failed_data$failure = 1
  
  
  # Get the healthy hard disk serial numbers
  non_failed_hard_disk_data = filter(dataframe, !(serial_number %in% failed_hard_disk_serial_number))
  non_failed_hard_disk_serial_number = unique(non_failed_hard_disk_data$serial_number)
  
  # Change the failure variable as factor for the healthy hard disk data
  non_failed_hard_disk_data$failure = as.factor(non_failed_hard_disk_data$failure)
  
  # Processed the failed data depending on the length of desired observations and
  # the prediction windows. The processing is done via looping the serial numbers
  
  # Set up the final data frame of the loop
  failed_data_processed = data.frame()
  
  for (i in failed_hard_disk_serial_number ){
    
    # Set up the temporary data in the loop that would be refreshed at the start
    # of the loop
    temp_failed_data = data.frame()
    
    iteration_failed_data = data.frame()  # The final data frame of the current loop
    # that would be combined into the final data frame
    
    # Subset per serial number
    temp_failed_data = filter(failed_data, serial_number == i)
    
    # Remove any serial hard disk that does not have enough length of observations
    if (nrow(temp_failed_data) < observation_days){
      
      next
      
    }
    
    
    # As the LSTM required a fix amount of sequence per input, we would subset
    # each hard disk observations to a certain desired length
    
    iteration_failed_data = tail(temp_failed_data, n = observation_days + 1)
    
    # As Making Disk Failure Prediction SMARTer paper mentioned that they don't take the 
    # observations when the hard disk itself failed, we only take the number of observations
    # before the exact failure time
    iteration_failed_data = head(iteration_failed_data, n = observation_days)
    
    # Now subset it further by the amount of how many days before failure occurs
    # the data will hold the information
    
    iteration_failed_data = head(iteration_failed_data, n = observation_days - prediction_days)
    
    # Now we combined the dataframe into the final data frame using rbind function
    failed_data_processed = rbind(failed_data_processed, iteration_failed_data)
    
  }
  
  # Now moving on to processing the healthy disk data
  
  # Sampled the healthy hard disk serial number to alleviate some of the burden
  non_failed_hard_disk_serial_number = sample(non_failed_hard_disk_serial_number, size = 1000)
  
  
  # Again with the same step as the failed data
  
  # Set up the final processed data frame
  non_failed_processed = data.frame()
  
  for (s in non_failed_hard_disk_serial_number){
    
    # Set up the temporary data frame within the loop that would refresh at 
    # every start of the loop
    temp_non_failed_data      = data.frame()
    iteration_non_failed_data = data.frame()
    
    # Subset per serial number 
    temp_non_failed_data = filter(non_failed_hard_disk_data, serial_number == s)
    
    # Remove any serial hard disk that does not have enough length of observations
    if (nrow(temp_non_failed_data) < observation_days){
      
      next
      
    }
    
    # We get the last n days of observations for the particular hard disk
    iteration_non_failed_data = tail(temp_non_failed_data, n = observation_days)
    # We subtract the observations day to n days before the verdict (healthy or failed) 
    # or the last observation days
    iteration_non_failed_data = head(iteration_non_failed_data, n = observation_days - prediction_days)
    
    # Combine everything at the final data frame
    
    non_failed_processed = rbind(non_failed_processed, iteration_non_failed_data)
    
  }
  
  
  
  
  # Combine the failed and non failed data
  undersampled_data_3 = rbind(failed_data_processed, non_failed_processed)
  
  
  return(undersampled_data_3)
  
}

cache('prediction_days_processing_undersampled_x4')