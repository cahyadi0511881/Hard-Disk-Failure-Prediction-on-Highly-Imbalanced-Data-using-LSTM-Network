library(ProjectTemplate)
load.project()

table(Q4_combined_data$model)

smart_1_NA = Q4_combined_data[is.na(Q4_combined_data$smart_1_raw) == TRUE, ]

# Check which model has NA in for the SMART_1 attribute
table(smart_1_NA$model)

DELLBOSS_VD_data = Q4_combined_data[Q4_combined_data$model == "DELLBOSS VD",]

# Delete DELLBOSS VD model since all it contains are NAs

# Check for missing SMART_3 attribute

smart_3_NA = Q4_combined_data[is.na(Q4_combined_data$smart_3_raw) == TRUE, ]

table(smart_3_NA$model)

# Seagate Barracuda models does not have any SMART 3 attributes

smart_4_na = Q4_combined_data[is.na(Q4_combined_data$smart_4_raw) == TRUE, ]

# SMART 3 and SMART 4 attributes are assumed to have high correlation since 
# if SMART 3 is missing then it is high chance that SMART 4 attribute to be missing


failed_data = filter(Q4_combined_data, serial_number %in% unique(Q4_2020_data_processed$serial_number) & failure == 1)

table(failed_data$model)
