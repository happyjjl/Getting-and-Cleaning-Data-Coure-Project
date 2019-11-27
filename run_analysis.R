## run_analysis.R
## version:	1.0
## author:	jin.jiangli@mayo.edu

## The script was created:
##           in RStudio Version 1.2.5001
##           with R version 3.6.1
##           and used dplyr 0.8.3
## The script was tested:
##           macOS Catalina 10.15.1

## This script clean the data sets with the following rules:
##      1. Merges the training and the test sets to create one data set.
##      2. Extracts only the measurements on the mean and standard deviation for each measurement.
##      3. Uses descriptive activity names to name the activities in the data set
##      4. Appropriately labels the data set with descriptive variable names.
##      5. From the data set in step 4, creates a second, independent tidy data set with the 
##         average of each variable for each activity and each subject.

## The original data set was downloaded from  url:
## "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

## Before running this script,you should download the data set from above url first,and then
## unzip the file downloaded to your working directory,make sure the following folder & file 
## exist:
##        A folder with name 'UCI HAR Dataset' ,
##       that contains the following data files:
##              UCI HAR Dataset/activity_labels.txt
##              UCI HAR Dataset/features.txt
##              UCI HAR Dataset/test/subject_test.txt
##              UCI HAR Dataset/test/X_test.txt
##              UCI HAR Dataset/test/y_test.txt
##              UCI HAR Dataset/train/subject_train.txt
##              UCI HAR Dataset/train/X_train.txt
##              UCI HAR Dataset/train/y_train.txt

##
## RESULT: 
##    tidy data is saved as "final_tidy_data.txt" in the working directory
##

## load package needed
library(dplyr)

## Load the data sets from files to data.frames &
## Labels the data set with descriptive variable names.

features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("id","feature"))
activities <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("id", "activity"))
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features$feature)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "activity")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features$feature)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names = "activity")

## 1. Merge the training and the test sets to create one data set

x <- cbind(subject_test,y_test,x_test)
y <- cbind(subject_train,y_train,x_train)
merged_data <- rbind(y,x)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement
merged_data <- tbl_df(merged_data)
tidy_data <- select(merged_data,subject,activity,contains("mean"),contains("std"))

## 3. Uses descriptive activity names to name the activities in the data set
tidy_data$activity <- factor(tidy_data$activity, levels = activities$id,labels = activities$activity)

## 4. Appropriately labels the data set with descriptive variable names
names(tidy_data) <- gsub("\\.+", "", names(tidy_data))  # omit the "." or "..."
names(tidy_data) <- gsub("^t", "time", names(tidy_data))
names(tidy_data) <- gsub("^f", "frequency", names(tidy_data))
names(tidy_data) <- gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data) <- gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data) <- gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data) <- gsub("mean", "Mean", names(tidy_data))
names(tidy_data) <- gsub("std", "Std", names(tidy_data))


## 5. Creates a second, independent tidy data set with the average of each variable for each activity 
## and each subject
final_tidy_data <- tidy_data %>% group_by(subject, activity) %>% summarise_all(funs(mean)) %>% ungroup()

write.table(final_tidy_data,"final_tidy_data.txt", row.names = FALSE, quote = FALSE)