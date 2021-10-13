# Undersampling Test

slice_days_under = function(dataframe){
  
  # Separate the dataframe into Failed and Healthy disk
  
  # Find the hard disk that experience failure
  failed_hardDisk = filter(dataframe, failure == 1)
  
  # Extract the serial number
  failed_hardDisk_serialNumber = unique(failed_hardDisk$serial_number)
  
  # Subset the main data frame that contains only the failed hard disk serial numbers
  failed_Data = filter(dataframe, serial_number %in% failed_hardDisk_serialNumber)
  
  # Now for the healthy data
  non_failed_Data = filter(dataframe, !(serial_number %in% failed_hardDisk_serialNumber))
  non_failed_serial = unique(non_failed_Data$serial_number)
  
  # Processed the data by looping and analysing per serial numbers. These process
  # would be done to the healthy data and the failed data separately
  
  failed_data_processed = data.frame()
  for (i in failed_hardDisk_serialNumber){
    
    tempDataStorage = data.frame()
    iterationDataStorage = data.frame()
    
    # Subset per serial number
    tempDataStorage = filter(failed_Data, serial_number == i) 
    
    # Set the failure status as 1 to define that the hard disk experience failure at the end
    tempDataStorage$failure = 1
    tempDataStorage$failure = as.factor(tempDataStorage$failure)
    
    if ( nrow(tempDataStorage) < 11){
      
      next
      
    }
    
    iterationDataStorage = tail(tempDataStorage, n = 11)
    iterationDataStorage = head(iterationDataStorage, n = 10)
    
    failed_data_processed = rbind(failed_data_processed, iterationDataStorage)
  }
  
  non_failed_processed = data.frame()
  
  for(n in non_failed_serial){
    
    tempData_nonfailed_storage = data.frame()
    iteration_nonfailed_storage = data.frame()
    
    # Subset per serial_number
    tempData_nonfailed_storage = filter(non_failed_Data, serial_number == n)
    
    # Set the failure status as factor
    tempData_nonfailed_storage$failure = as.factor(tempData_nonfailed_storage$failure)
    
    if (nrow(tempData_nonfailed_storage) < 10){
      
      next
      
    }
    
    iteration_nonfailed_storage = tail(tempData_nonfailed_storage, n = 10)
    
    # Rbind the data
    non_failed_processed = rbind(non_failed_processed, iteration_nonfailed_storage)
    
  }
  
  
  non_failed_equal_serial_number = sample(unique(non_failed_Data$serial_number), size = length(unique(failed_data_processed$serial_number)))
  non_failed_data_sampled = filter(non_failed_processed, serial_number %in% non_failed_equal_serial_number)
  
  # Combine the failed and non failed data
  undersampled_data = rbind(failed_data_processed,non_failed_data_sampled)
  
  
  return(undersampled_data)
}

cache('slice_days_under')
