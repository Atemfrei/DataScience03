##################################################
## 0. Preamble                                  ##
##################################################

# Clear Environment
 rm(list=ls(all=TRUE)) 
 graphics.off()

# Set Working Directory
 WD <- dirname(sys.frame(1)$ofile)
 setwd (WD)

# Load packages
 library(dplyr)

##########################################################################
## 1. Merge the training and the test sets to create one data set.      ##           
##########################################################################

# Read in the TEST sets (x), labels (y), and subjects (subject)
 test.x       <- read.table("data/UCI HAR Dataset/test/X_test.txt")
 test.y       <- read.table("data/UCI HAR Dataset/test/y_test.txt")
 test.subject <- read.table("data/UCI HAR Dataset/test/subject_test.txt")

# Read in the TRAINING sets (x), labels (y), and subjects (subject)
 train.x       <- read.table("data/UCI HAR Dataset/train/X_train.txt")
 train.y       <- read.table("data/UCI HAR Dataset/train/y_train.txt")
 train.subject <- read.table("data/UCI HAR Dataset/train/subject_train.txt")

# Merge the TEST and TRAINING datasets into single dataframes and combine them to one dataset 'data'
 test  <- data.frame(test.subject,  test.y,  test.x)
 train <- data.frame(train.subject, train.y, train.x)
 data  <- rbind(train, test)

##########################################################################################
## 2. Set appropriate labels for the data set with descriptive variable names.          ##
##########################################################################################

# Read in the labels dataset (features) and use these for the column names
 features       <- read.table("data/UCI HAR Dataset/features.txt")
 column.names   <- as.character(features[, 2])
 colnames(data) <- c("subject_id", "activity", column.names)

##################################################################################################
## 3. Extract only the measurements on the mean and standard deviation for each measurement.    ##
################################################################################################## 
  
# Identify columns that contain mean or standard deviations.
 index1  <- grep("mean()", colnames(data), fixed=T)
 index2  <- grep("std()", colnames(data), fixed=T)
 index   <- c(index1,index2)
 index   <- index[order(index)]
# Extract respective columns  for mean and sd and keep subject_id (col1) and activity_labels (col2)  
 data   <- data[ ,c(1,2,index)]

##################################################################################
## 4. Use descriptive activity names to name the activities in the data set.    ##
##################################################################################
 
# Read in the activity labels dataset
 activity.labels <- read.table("data/UCI HAR Dataset/activity_labels.txt")
 
# Replace the activity codes with the activity labels by using the match()-function
 matched.labels  <- as.character(activity.labels[match(data$activity, activity.labels$V1), 'V2'])
 data$activity   <- matched.labels

##########################################################################################################################
## 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. ##
##########################################################################################################################
 
# Calculate the mean for every measurement after grouping the dataset by subject and activity
 data.tidy <- data %>% group_by(subject_id, activity) %>% summarise_each(funs(mean)) 

# Write the tidy data to a new file
 write.table(data.tidy, file="run_analysis_data.txt", row.name=F)