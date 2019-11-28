# Getting-and-Cleaning-Data-Coure-Project
---------------------------
About this repository
---------------------------
This repository was created for the peer-graded assignment of:

Course 3: Getting And Cleaning Data, from Data Science Specialization, by Johns Hopkins University, on coursera

Contents in repository
--------------------------
  README.md             - The file while you are reading now  
  CodeBook.md           - Describes the experiment data and the steps taken to clean the data  
  final_tidy_data.txt   - Output file generated by the run_analysis.R script  
  run_analysis.R        - R script to merge, clean-up, transform, and summarize the experiment data  
How to run the script
--------------------------
1. Download the data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  
2. Extract the .zip file downloaded in Step 1  
3.Edit run_analysis.r first line so the setwd() function is pointing to the location of the "UCI HAR Dataset" folder extracted in Step 2. For example: setwd("~/downloads/UCI HAR Dataset")  
4.Run the script  
5.The script will create a tidydata.txt in the "UCI HAR Dataset" folder  
Dependency
--------------------------
The R package dplyr is required to run this script.  
To install the dplyr package,input the following command in R Console:  
  install.packages("dplyr")  
