## You should create one R script called run_analysis.R that does the following. 
## 1.Merges the training and the test sets to create one data set.
## 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3.Uses descriptive activity names to name the activities in the data set
## 4.Appropriately labels the data set with descriptive variable names. 
## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

setwd("C:/Users/200008412/Desktop/Analytics Engineer Program 2015/Coursera/Getting_and_Cleaning_Data")

## Read all the data
training_set <- read.table("train/X_train.txt")
training_activity <- read.table("train/y_train.txt")
training_subject <- read.table("train/subject_train.txt")
test_set <- read.table("test/X_test.txt")
test_activity <- read.table("test/y_test.txt")
test_subject <- read.table("test/subject_test.txt")
x_labels <- read.table("features.txt")
x_labels <- t(x_labels[,2])

## Add column names to the data
colnames(training_set)<-x_labels
colnames(training_activity)<-"Activity"
colnames(training_subject)<-"Subject"
colnames(test_set)<-x_labels
colnames(test_activity)<-"Activity"
colnames(test_subject)<-"Subject"

## Combine the data
training_data <- cbind(training_subject,training_activity,training_set)
test_data <- cbind(test_subject,test_activity,test_set)
all_data <- rbind(training_data,test_data)

## select the requested columns and order the columns by Subject and Activity
library(dplyr)
valid_column_names <- make.names(names=names(all_data), unique=TRUE, allow_ = TRUE)
names(all_data) <- valid_column_names
dataset <- all_data %>% select(Subject, Activity, contains("mean."),contains("std"))

## Replace the number in the "Activity" column with the actual activity
## Activity: 
## 1 WALKING
## 2 WALKING_UPSTAIRS
## 3 WALKING_DOWNSTAIRS
## 4 SITTING
## 5 STANDING
## 6 LAYING
dataset = dataset %>% mutate(Activity = ifelse(Activity==1,"WALKING",
                                            ifelse(Activity==2,"WALKING_UPSTAIRS",
                                              ifelse(Activity==3,"WALKING_DOWNSTAIRS",
                                                ifelse(Activity==4,"SITTING",
                                                  ifelse(Activity==5,"STANDING","LAYING"))))))      

## Calculate the mean and standard deviation grouped by Subject and Activity
tidy_data = dataset %>% group_by(Subject, Activity) %>% summarise_each(funs(mean,sd)) 
names(tidy_data) <- names(tidy_data[,c(1,2,order(names(tidy_data[,3:148])))])

write.table(tidy_data,file="tidy_data.txt",row.names=FALSE)

