# This file is for the oversampling 

# Create a function for slicing the data based on how many days

slice_days = function(dataframe){
  
  # Separate the dataframe into Failed and Healthy disk
  
  # Find the hard disk that experience failure
  failed_hardDisk = filter(dataframe, failure == 1)
  
  # Extract the serial number
  failed_hardDisk_serialNumber = unique(failed_hardDisk$serial_number)
  
  # Subset the main data frame that contains only the failed hard disk serial numbers
  failed_Data = filter(dataframe, serial_number %in% failed_hardDisk_serialNumber)
  
  # Now for the healthy data
  non_failed_Data = filter(dataframe, !(serial_number %in% failed_hardDisk_serialNumber))
  
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
    
    # Create a conditional based on number of rows within the data frame (range of dates available)
    if ( nrow(tempDataStorage) >= 60 ){
      
      iterationDataStorage = tail(tempDataStorage, n = 60)
      iterationDataStorage[1:10,2] = paste(i,  as.character(1), sep=',')
      iterationDataStorage[11:20,2] = paste(i,  as.character(2), sep=',')
      iterationDataStorage[21:30,2] = paste(i,  as.character(3), sep=',')
      iterationDataStorage[31:40,2] = paste(i,  as.character(4), sep=',')
      iterationDataStorage[41:50,2] = paste(i,  as.character(5), sep=',')
      iterationDataStorage[51:60,2] = paste(i,  as.character(6), sep=',')
      
    }else if ( 50 <= nrow(tempDataStorage) & nrow(tempDataStorage) < 60 ){
      
      iterationDataStorage = tail(tempDataStorage, n = 50)
      iterationDataStorage[1:10,2] = paste(i,  as.character(1), sep=',')
      iterationDataStorage[11:20,2] = paste(i,  as.character(2), sep=',')
      iterationDataStorage[21:30,2] = paste(i,  as.character(3), sep=',')
      iterationDataStorage[31:40,2] = paste(i,  as.character(4), sep=',')
      iterationDataStorage[41:50,2] = paste(i,  as.character(5), sep=',')
      
    }else if ( 40 <= nrow(tempDataStorage) & nrow(tempDataStorage) < 50){
      
      iterationDataStorage = tail(tempDataStorage, n = 40)
      iterationDataStorage[1:10,2] = paste(i,  as.character(1), sep=',')
      iterationDataStorage[11:20,2] = paste(i,  as.character(2), sep=',')
      iterationDataStorage[21:30,2] = paste(i,  as.character(3), sep=',')
      iterationDataStorage[31:40,2] = paste(i,  as.character(4), sep=',')
      
    }else if (30 <= nrow(tempDataStorage) & nrow(tempDataStorage) < 40){
      
      iterationDataStorage = tail(tempDataStorage, n = 30)
      iterationDataStorage[1:10,2] = paste(i,  as.character(1), sep=',')
      iterationDataStorage[11:20,2] = paste(i,  as.character(2), sep=',')
      iterationDataStorage[21:30,2] = paste(i,  as.character(3), sep=',')
      
    }else if (20 <= nrow(tempDataStorage) & nrow(tempDataStorage) < 30){
      
      iterationDataStorage = tail(tempDataStorage, n = 20)
      iterationDataStorage[1:10,2] = paste(i,  as.character(1), sep=',')
      iterationDataStorage[11:20,2] = paste(i,  as.character(2), sep=',')
      
    }else if (10 <= nrow(tempDataStorage) & nrow(tempDataStorage) < 20){
      
      iterationDataStorage = tail(tempDataStorage, n = 10)
      
    }else if ( nrow(tempDataStorage) < 10){
      
      next
      
    }
    
    
    # Append the result of the operations above into the final data frame
    failed_data_processed = rbind(failed_data_processed, iterationDataStorage)
    
  }
  
  return(failed_data_processed)
  
}


# Now test the function

function_test_failed_data = slice_days(All_2020_ST4000DM000)
