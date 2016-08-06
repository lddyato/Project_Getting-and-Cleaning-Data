#download the file and load it into R
if(!file.exists("./data3")) dir.create("./data3")
fileurl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl, destfile="./data3/projectdata.zip")
zipdata <- unzip("./data3/projectdata.zip",exdir="./data3")
trainX <- read.table("./data3/UCI HAR Dataset/train/X_train.txt")
trainY <- read.table("./data3/UCI HAR Dataset/train/y_train.txt")
trainSubject <- read.table("./data3/UCI HAR Dataset/train/subject_train.txt")
testX <- read.table("./data3/UCI HAR Dataset/test/X_test.txt")
testY <- read.table("./data3/UCI HAR Dataset/test/y_test.txt")
testSubject <- read.table("./data3/UCI HAR Dataset/test/subject_test.txt")
features <- read.table("./data3/UCI HAR Dataset/features.txt", stringsAsFactor = FALSE)
activity <- read.table("./data3/UCI HAR Dataset/activity_labels.txt")

#1.Merges the training and the test sets tocreate one data set.
trainData <- cbind(trainX, trainSubject, trainY)
testData <- cbind(testX, testSubject, testY)
data <- rbind(trainData, testData)
dataLables <- rbind(rbind(features,c(562, "Subject")), c(563, "Activityid"))[,2]
names(data) <- dataLables

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
extractdata <- data[, grep("mean|std|Subject|Activityid", names(data))]

#3.Uses descriptive activity names to name the activities in the data set
colnames(activity) <- c("Activityid", "Activity")
extractdata <- merge(extractdata, activity, by = "Activityid")
extractdata <- extractdata[, -1]

# 4. Appropriately labels the data set with descriptive variable names.

names(extractdata) <- gsub("\\()", "", names(extractdata))
names(extractdata) <- gsub("^t", "time", names(extractdata))
names(extractdata) <- gsub("^f", "frequence", names(extractdata))
names(extractdata) <- gsub("-mean", "Mean", names(extractdata))
names(extractdata) <- gsub("-std", "Std", names(extractdata))
names(extractdata) <- gsub("Acc", "Acceleration", names(extractdata))
names(extractdata) <- gsub("Mag", "Magnitude", names(extractdata))
names(extractdata) <- gsub("Gyro", "AngularVelocity", names(extractdata))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
sum(is.na(extractdata))
tidyDataMean <- extractdata %>% 
                group_by( Subject, Activity) %>%
                summarise_each(funs(mean))
write.table(tidyDataMean, "./data3/tidyDataMean.txt", row.name=FALSE)
