##Install needed packages

#install.packages("gdata")
library(gdata)
library(dplyr)

# Merge trainining and test-set

getwd()

# Load Files in Global Environment

testfile = read.table("~/Downloads/UCIDA/test/X_test.txt")
trainfile = read.table("~/Downloads/UCIDA/train/X_train.txt")
features     = read.table('~/Downloads/UCIDA/features.txt',header=FALSE); #imports features.txt
activityType = read.table('~/Downloads/UCIDA/activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('~/Downloads/UCIDA/train/subject_train.txt',header=FALSE); #imports subject_train.txt


# View Contents

str(testfile)
str(trainfile)

names(testfile)
head(testfile)


# Add Labels

testlabel = read.table ("~/Downloads/UCIDA/test/y_test.txt")
trainlabel = read.table("~/Downloads/UCIDA/train/y_train.txt")

#Rename Labels
activity <- rbind(testlabel,trainlabel)


# merge train and test using the join command.

mrgdata1 <- full_join(testfile,trainfile,by = NULL)

mrgdat <- cbind(mrgdata1,activity)


# Apply the measurement labels as column names to the combined
# running dataset
column.names <- as.vector(features[, 2])
colnames(mrgdat) <- c(column.names, "activity labels")


# View Output
tail(mrgdat)

# Choose the columns that contain mean or standard deviations.

logicalVector = (grepl("activity labels..",mrgdat) | grepl("subject..",mrgdat) | grepl("-mean..",mrgdat) & !grepl("-meanFreq..",mrgdat) & !grepl("mean..-",mrgdat) | grepl("-std..",mrgdat) & !grepl("-std()..-",mrgdat));
logicalVector

head(mrgdat)

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = mrgdat[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');
# Read in the activity labels dataset
activity.labels <- read.csv("UCI HAR Dataset/activity_labels.txt", 
                            sep="", header=FALSE)

# Replace the activity codes in the trimmed down running
# dataset with the labels from the activity labels dataset.
run.data$activity_labels <- as.character(activity.labels[
  match(run.data$activity_labels, activity.labels$V1), 'V2'])
