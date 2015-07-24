#### First you need to get the data ####

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp,mode="wb")
unzip(temp)

#### 1. Merges the training and the test sets to create one data set. ####

#Clean out all the existing objects (this is to help during script development!)

remove(list = ls())

#Read in the description datasets (these will be used for labels)

features = read.table('./UCI HAR Dataset/features.txt',header=FALSE);
activity_labels = read.table('./UCI HAR Dataset/activity_labels.txt',header=FALSE);

# read in the training data sets

x_train = read.table('./UCI HAR Dataset/train/x_train.txt',header=FALSE);
subject_train = read.table('./UCI HAR Dataset/train/subject_train.txt',header=FALSE);
y_train = read.table('./UCI HAR Dataset/train/y_train.txt',header=FALSE);

#Assign Column Names to the data loaded

colnames(activity_labels) <- c("activity_id","activity_name")   #these are friendly names to aid later understanding
colnames(features) <- c("feature_id","feature_name")   #these are friendly names to aid later understanding
colnames(y_train) <- "activity_id" #this is the same as the acivity_labels so we can merge them
colnames(subject_train) <- "subject_id" #simple id column

#Assign column names to the x_train dataset by using the descriptions from the features table

colnames(x_train) <- features$feature_name

#load in the test data set

x_test = read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE);
subject_test = read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE);
y_test = read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE);

#Put column names on test data

colnames(x_test) <- features$feature_name;
colnames(y_test) <- "activity_id";
colnames(subject_test) <- "subject_id";

# join the test data to the training data

Final_X <- rbind(x_train,x_test);
Final_Y <- rbind(y_train,y_test);
Final_Subject <- rbind(subject_train,subject_test);

# combine into one dataframe

FinalDataLong <- cbind(Final_Subject,Final_Y,Final_X)

#### 2. Extracts only the measurements on the mean and standard deviation for each measurement ####

# subset the data and return columns that have the word "mean","std","subject_id" or "activity_id
# for mean and std make sure we dont pick up any other similar names by using word boundries i.e \\b

FinalDataSub<- FinalDataLong[,(grep(paste(c("\\bmean\\b","\\bstd\\b","subject_id","activity_id"),collapse='|'), colnames(FinalDataLong)))]

#### 3. Uses descriptive activity names to name the activities in the data set ####

# Merge the FinalData within the activity table to bring through descriptive names
FinalData <- merge(FinalDataSub,activity_labels,key="activity_id")

# Reorded the columns so the activity name is next to the ativity id
FinalData<-FinalData[,c(1,69,2:68)]

#### 4.Appropriately labels the data set with descriptive variable names. ####

#Loop over the col names and apply a series of transformations

for (i in 1:length(names(FinalData)))
{
  #Deal with the first part -  the measure type
  
  names(FinalData)[i] = gsub("^(t)","Time-",names(FinalData)[i])
  names(FinalData)[i] = gsub("^(f)","Freq-",names(FinalData)[i])
  
  #Deal with the second part -  the measure target
  
  names(FinalData)[i] = gsub("([Gg]ravity)","Gravity",names(FinalData)[i])
  names(FinalData)[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",names(FinalData)[i])
  names(FinalData)[i] = gsub("[Gg]yro","Gyro",names(FinalData)[i])
  names(FinalData)[i] = gsub("AccMag","AccMagnitude",names(FinalData)[i])
  names(FinalData)[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",names(FinalData)[i])
  names(FinalData)[i] = gsub("JerkMag","JerkMagnitude",names(FinalData)[i])
  names(FinalData)[i] = gsub("GyroMag","GyroMagnitude",names(FinalData)[i])
  
  #Deal with the end part - the value returned
  
  names(FinalData)[i] = gsub("\\()","",names(FinalData)[i])
  names(FinalData)[i] = gsub("-std","-StdDev",names(FinalData)[i])
  names(FinalData)[i] = gsub("-mean","-Mean",names(FinalData)[i])

};

#### 5.From the data set in step 4, creates a second, independent tidy data set with the average of each   ####
####   variable for each activity and each subject..                                                       ####

#perform the aggregation on all the variables (cols 4 through 69) but Subject (subject_id) and Activity (activity_name)

AggregatedMeans <-aggregate(FinalData[,4:69], by=list(Subject=FinalData$subject_id,Activity=FinalData$activity_name),FUN=mean)


##########################################################################

# Export the tidyData set
write.table(AggregatedMeans, './my_samsung_data.txt',row.names=FALSE,sep='\t')

##########################################################################