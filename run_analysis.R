##Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set from the given directory of files .txt files
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#Clear up the environment
rm(list=ls())

#Read the test and train files
require("data.table")
setwd("C:/Users/priya/Documents/GitHub/CleaningData/UCI HAR Dataset")
getwd()
library(plyr)
library(data.table)
subject_Train<- read.table('./train/subject_train.txt',header=FALSE)
x_Train<- read.table('./train/x_train.txt',header=FALSE)
y_Train<- read.table('./train/y_train.txt',header=FALSE)

subject_Test <- read.table('./test/subject_test.txt',header=FALSE)
x_Test = read.table('./test/x_test.txt',header=FALSE)
y_Test = read.table('./test/y_test.txt',header=FALSE)

#Combine Test and Train Data
x_Data <- rbind(x_Train, x_Test)
y_Data <- rbind(y_Train, y_Test)
subject_Data <- rbind(subject_Train, subject_Test)
dim(x_Data)    # 10299   561 
dim(y_Data)    #10299     1
dim(subject_Data)   #10299     1

#Exract mean and SD
features<-read.table("features.txt")[, 2]
xData_mean_std <- x_Data[, grep("-(mean|std)\\(\\)", features )]
#Assign Names
names(xData_mean_std) <- read.table("features.txt")[grep("-(mean|std)\\(\\)", read.table("features.txt")[, 2]), 2] 
View(xData_mean_std)
dim(xData_mean_std)

#Name the activities in the dataset
y_Data[, 1] <- read.table("activity_labels.txt")[y_Data[, 1], 2]
names(y_Data) <- "Activity"
View(y_Data)

#Label the dataset

names(subject_Data) <- "Subject"
summary(subject_Data)

# Organize and combine all data sets into single one

single_Data <- cbind(xData_mean_std, y_Data, subject_Data)

# Defining descriptive names for all variables.

names(single_Data) <- make.names(names(single_Data))
names(single_Data) <- gsub('Acc',"Acceleration",names(single_Data))
names(single_Data) <- gsub('GyroJerk',"AngularAcceleration",names(single_Data))
names(single_Data) <- gsub('Gyro',"AngularSpeed",names(single_Data))
names(single_Data) <- gsub('Mag',"Magnitude",names(single_Data))
names(single_Data) <- gsub('^t',"TimeDomain.",names(single_Data))
names(single_Data) <- gsub('^f',"FrequencyDomain.",names(single_Data))
names(single_Data) <- gsub('\\.mean',".Mean",names(single_Data))
names(single_Data) <- gsub('\\.std',".StandardDeviation",names(single_Data))
names(single_Data) <- gsub('Freq\\.',"Frequency.",names(single_Data))
names(single_Data) <- gsub('Freq$',"Frequency",names(single_Data))

View(single_Data)

#Create a second, independent tidy data set with the average of each variable for each activity and each subject
names(single_Data)

Data2<-aggregate(. ~Subject + Activity, single_Data, mean)
Data2<-Data2[order(Data2$Subject,Data2$Activity),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)
