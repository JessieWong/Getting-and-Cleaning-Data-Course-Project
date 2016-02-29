#set working directory
setwd('C:/Users/jwong/Desktop/coursera')

#download the zip file from the url given and name it as Dataset.zip
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(), "Dataset.zip")
download.file(url, f)

#unzip the file
unzip("Dataset.zip")

rm(list=ls())# remove all data store in the Data Environment

#Question 1:Merges the training and the test sets to create one data set.
#Named the files
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE) 
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE) 
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE) 
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "") 
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE) 
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
features <- read.table('./UCI HAR Dataset/features.txt',header=FALSE)
activitylabel <- read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE)

# Assign column names to the activity labels
colnames(activitylabel)  = c('activityId','activityType')

# Assigin column names to the train data imported above
colnames(y_train)        = "activityId"
colnames(subject_train)  = "subjectId"
colnames(x_train)        = features[,2]

# Assign column names to the test data imported above
colnames(y_test)       = "activityId"
colnames(subject_test) = "subjectId"
colnames(x_test)       = features[,2]

# concatenate by column. 
train=cbind(y_train, subject_train, x_train)
test=cbind(y_test, subject_test, x_test )

#combine the train dataset & test dataset by row
data=rbind(train,test)

#Question 2:Extracts only the measurements on the mean and standard deviation for each measurement.
# Create column names which will be used to select the desired means & standard deviation columns
colNames  = colnames(data)

# Create a logicalVector that contains TRUE values for the ID, means & standard deviation and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Subset data table based on the logicalVector to keep only desired columns
data = data[logicalVector==TRUE]

#Question 3:Uses descriptive activity names to name the activities in the data set
# Merge the finalData set with the acitivityType table to include descriptive activity names
Q3data = merge(data,activitylabel,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(Q3data); 

#Q4.Appropriately labels the data set with descriptive variable names.
# Cleaning up the variable names
#\\ is escape character
for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StandardDeviation",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","TimeDomain",colNames[i])
        colNames[i] = gsub("^(f)","Frequency",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
# Reassigning the new descriptive column names to the finalData set
colnames(Q3data) = colNames

#Q5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Create a new table without the activity type column
final  = Q3data[,names(Q3data) != 'activityType']

# Summarizing the final table to include just the mean of each variable for each activity and each subject
final2  = aggregate(final[,names(final) != c('activityId','subjectId')],by=list(activityId=final$activityId, subjectId = final$subjectId),mean)

# Merging the final2 with activityType to include descriptive acitvity names
final3   = merge(final2,activitylabel,by='activityId',all.x=TRUE)

# Export the final3 data set 
write.table(final3, './UCI HAR Dataset/tidyData.txt',row.names=FALSE,sep='\t')
