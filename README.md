
Project Notes
====

## run_analysis.R

This script is designed to do the follow steps

* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement. 
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive variable names. 
* From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.   

On the data set   

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip    

The analysis assumes that the Samsung data is in your working directory.   

The output of the data is saved in a txt file labelled "my_samsung_data"   

A Code book is provided in the Github Repo, with the changes related to this script under the heading "Results of Running the run_analysis.R Script"   

## Background

One of the most exciting areas in all of data science right now is wearable computing - see for example this article   

http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/   

Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:    

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones    
