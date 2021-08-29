# Results table

library(ProjectTemplate)
library(ggplot2)
library(ggpmisc)
library(grid)
library(gridExtra)
library(ggpubr)
load.project()

# Record the results of the Deep Learning

# Create data frame to record the results of experimenting with different sampling method

samplingMethods_Table = data.frame(Sampling_Method = c("Normal (Without Sampling)","Undersampling (Severe)", "Partial Undersampling (4:1)", "Oversampling"),
                                   Sequence_Length = c(90,90,90,90), MCC = c(0.5853229, 0.3851973, 0.7114582, 0.5986144))
# The histogram Plot

samplingMethodsResultsPlot = ggplot(data = samplingMethods_Table, aes(x=Sampling_Method, y= MCC, fill = Sampling_Method)) + geom_bar(stat = "identity") +
                              labs(title = "MCC Score of Different Sampling Methods", x = "Sampling Methods", y="MCC Score") + theme_bw() +
                              theme(legend.position = "none", axis.text.x=element_text(angle=45, hjust=1), plot.title = element_text(hjust=0.5), text = element_text(size=14)) + 
                              geom_text(aes(label = MCC), vjust = 2) 
samplingMethodsResultsPlot

# Create the table display

colnames(samplingMethods_Table) <- c("Sampling Method", "Sequence Length", "MCC Score") # Changing the display name for it to be neater
samplingMethodResultTable = ggtexttable(samplingMethods_Table, rows = NULL, theme = ttheme(base_size = 14))

# Bold the best MCC Score
samplingMethodResultTable <- table_cell_font(samplingMethodResultTable, row = 4, column = 3, size= 14, face = "bold") 

# Arrange the table and plot side by side
samplingMethodResultArranged = ggarrange(samplingMethodsResultsPlot,samplingMethodResultTable, nrow =1, ncol = 2)
samplingMethodResultArranged

# Create data frame to record the results of the sequence length experiment in this project
sequenceLength_Table = data.frame(Length_of_Sequence = c(90,60,30,10), 
                                  MCC_Score = c(0.7114582,0.7089598, 0.6856193, 0.683051))

# For the graph plot
sequenceLengthResultPlot = ggplot(data=sequenceLength_Table, 
                                  aes(x=Length_of_Sequence,y=MCC_Score)) + geom_line() + geom_point() + coord_cartesian(xlim=c(0,100), ylim=c(0,1)) +
                            labs(title = "MCC Score of Different Sequence Length", x = "Length of Sequence", y = "MCC Score") + theme_bw() + 
                            theme(plot.title = element_text(hjust=0.5), text = element_text(size=14))
sequenceLengthResultPlot
# Change column names for display
colnames(sequenceLength_Table) <- c("Length of Sequence", "MCC Score")

sequenceLength_Table_Plot = ggtexttable(sequenceLength_Table, rows = NULL, theme = ttheme(base_size = 14))
# Bold the best MCC Score
sequenceLength_Table_Plot <- table_cell_font(sequenceLength_Table_Plot, row = 2 , column =2, size = 14, face = "bold")


sequenceLengthResultPlot_arranged = ggarrange(sequenceLengthResultPlot,sequenceLength_Table_Plot, nrow = 1, ncol =2)

# Run the code and plot the graph
sequenceLengthResultPlot_arranged

