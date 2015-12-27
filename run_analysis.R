#Performing Analysis on the Human Activity Recognition Using Smartphones Dataset

#load the appropriate package for analysis
library(dplyr)

#unzip the file from the working directory
unzip ("getdata-projectfiles-UCI HAR Dataset.zip", exdir = "./")

#Read in the test data files
testdataX <- read.table("UCI HAR Dataset/test/X_test.txt")
testdataY <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

#Look at the structure of the test data files
str(testdataY)
str(testdataX)

#Read in the train data files
traindataX <- read.table("UCI HAR Dataset/train/X_train.txt")
traindataY <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

#Look at the structure of the train data files
str(traindataX)
str(traindataY)

#Read in the activity labels  and features text files
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

#Join the activity labels with the activities performed by the subjects in the test set
testdataY_activitylabels <- inner_join(testdataY, activitylabels, by = "V1")

#Remove the first column from the result and rename using the descriptive activity labels
testdataY_activitylabels$V1 <- NULL
names(testdataY_activitylabels) <- c("activity.labels")

#Rename the test data columns with the features text
names(testdataX) <- make.names(features$V2, unique = TRUE)

#Check the rows of all the test data files for compatibility with cbind()
nrow(testdataY_activitylabels)
nrow(testdataY)
nrow(testdataX)

#Column bind the X portion of the test data with the descriptive activities
testdata_merged <- cbind(testdataY_activitylabels,testdataX)

#Rename the column to Subject and column bind with the merged test data 
names(subject_test) <- c("Subject")
testdata_final <- cbind(subject_test, testdata_merged)

#Look at the structure of the train data files
str(traindataX)
str(traindataY)

#Join the activity labels with the activities performed by the subjects in the train set
traindataY_activitylabels <- inner_join(traindataY, activitylabels, by = "V1")

#Remove the first column from the result and rename using the descriptive activity labels
traindataY_activitylabels$V1 <- NULL
names(traindataY_activitylabels) <- c("activity.labels")

#Rename the train data columns with the features text
names(traindataX) <- make.names(features$V2, unique = TRUE)

#Check the rows of all the train data files for compatibility with cbind()
nrow(traindataY_activitylabels)
nrow(traindataY)
nrow(traindataX)

#Column bind the X portion of the train data with the descriptive activities
traindata_merged <- cbind(traindataY_activitylabels,traindataX)

#Rename the column to Subject and column bind with the merged train data 
names(subject_train) <- c("Subject")
traindata_final <- cbind(subject_train, traindata_merged)

#Perform the final merge of the test and train data
final_data_merge <- rbind(traindata_final, testdata_final)

#Check to see that the sum of the rows from the test and train datasets equal the new dataset
nrow(traindata_final)
nrow(testdata_final)
nrow(final_data_merge)

#Select out only the columns containing the mean and standard deviation data
final_data_mean_std <- select(final_data_merge, matches("[.]mean[..]|[.]std[..]|activity|Subject"))

#Create the final tidy data set grouping first by subject and then by activity
#The resulting tidy data set is the wide form containg 68 variables and 180 observations
tidy_dataset <- final_data_mean_std %>% group_by(Subject, activity.labels) %>% summarise_each(funs(mean))

#Rename variables to be easier to understand expanding Accel and removing ".."
rename_variables_1 <- names(tidy_dataset)
rename_variables_2 <- sub("Acc","Accel",rename_variables_1)
rename_variables_3 <- sub("[.][.]","",rename_variables_2)
names(tidy_dataset) <- rename_variables_3

#write the tidy data set to a text file
write.table(tidy_dataset, file = "tidy_dataset.txt")

#Reading in the tidy data file and viewing
tidy_data <- read.table("tidy_dataset.txt", header = TRUE) 
View(tidy_data)
