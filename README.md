# Hard Disk Failure Prediction on Highly Imbalanced Data Using LSTM Network
  
## Introduction 

<p align="justify">&nbsp;&nbsp;&nbsp; The exponential increase of the amount of available data and its utilization throughout all fields of business and services, has subsequently led to the rise of demand and popularity of data storage systems. Data storage systems act as the main pillar for data utilization and currently, most of the data that are available are stored within magnetic disks which are dominated by Hard Disk drives (Pinheiro et al., 2007). Being the pillar that supports the utilization of data, a downtime of this system could be detrimental to the business, as one study group suggest that revenue loss due to a downtime of such an imperative system averaged around $70,000 per hour for a mid-size company and it could get even higher for larger companies (Boggs & Bozman, 2009). Therefore, the ability to predict when such failure would occur is crucial to minimize loss. </p>  
<p align="justify">&nbsp;&nbsp;&nbsp; Throughout the years there have been several works and currently increasing in numbers that focus on creating an effective and accurate prediction model for hard disk failure (Lu et al., 2020; Shen et al., 2018; Hu et al., 2020; Shi et al., 2021; Tomer et al., 2021; Wang et al., 2021; Chaves et al., 2018; Shen et al., 2021; Pinheiro et al., 2007; Yu, 2019; Rincón et al., 2017; He et al., 2020; Mittman et al., 2019; Kesavan & Mahamuni, 2021). Most of the prediction models utilize Self-Monitoring Analysis and Reporting Technology (S.M.A.R.T) attributes from each hard drive to produce a prediction model. However, there are some instances in which external parameters such as the location of the hard drive and performance metrics are utilized to enhance the prediction accuracy (Lu et al., 2020).</p>   
<p align="justify">&nbsp;&nbsp;&nbsp;Most of the existing prediction models have achieved high accuracy in predicting failures of hard disks through the various implementation and combination of both deep learning and machine learning. However, there is currently no record that shows the implementation of various imputation methods to further enhance the accuracy of the prediction model. Therefore, this project aims to produce an enhanced prediction model while also investigating the effect of various imputation methods on prediction accuracy.</p>  

## Running the Analyses

Steps Required to Run the Analyses:  
  * Run project template
  * Run the analyses files in src folder. Select the sampling methods used based on the files. Use Normal for using raw data
  # All the necessary functions and its description are listed in the munge files. Munge files also contained the necessary initial eda performed in this project
  * Modify the desired observations length as the function is made to take into consideration (observation windows and prediction window)
