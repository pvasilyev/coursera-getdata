# The aim of this script is to accompish Course Project on GetData course. Task is as follows:
#
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# main function to run the analysis:
run_analysis <- function() {
  setwd("~/dev/data-science/get-data/coursera-getdata/")
  ensure_data_folder_exists()
  
  zipFile <- "./data/getdata-projectfiles-UCI HAR Dataset.zip"
  ensure_zip_file_is_present(zipFile)
  
  unpack_zip_file(zipFile)
  
  features <- read.table('./data/UCI HAR Dataset/features.txt', col.names=c('feature_id', 'feature_name'))
  activities <- read.table('./data/UCI HAR Dataset/activity_labels.txt',col.names=c('activity_id', 'activity_name'))
  
  merged_data <- merge_train_and_test_data(features, activities)
    
  required_columns <- get_required_columns(merged_data)  
  data_means_and_std <- select(merged_data, required_columns)
  
  data_summarized_by_activity_and_subject <- group_by(data_means_and_std, activity_name, subject_id) %>% summarize_each(funs(mean))
  
  write.table(data_summarized_by_activity_and_subject, file="./data/tidy_dataset.txt", row.name=FALSE)
}

ensure_data_folder_exists <- function() {
  if (!file.exists("./data")) {
    dir.create("data")
  }
}

ensure_zip_file_is_present <- function(zipFile) {
  if (!file.exists(zipFile)) {
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, destfile = zipFileAndPath, method = "curl")
  }
}

unpack_zip_file <- function(zipFile) {
  if (!file.exists("./data/UCI HAR Dataset")) {
    unzip(zipfile = zipFile, exdir = "./data")
  }
}

merge_train_and_test_data <- function(features, activities) {
  x_train <- read.table('./data/UCI HAR Dataset/train/X_train.txt', col.names=features$feature_name)
  y_train <- read.table('./data/UCI HAR Dataset/train/y_train.txt', col.names='activity_id')
  x_test <- read.table('./data/UCI HAR Dataset/test/X_test.txt', col.names=features$feature_name)
  y_test <- read.table('./data/UCI HAR Dataset/test/y_test.txt', col.names='activity_id')
  subject_train <- read.table('./data/UCI HAR Dataset/train/subject_train.txt', col.names='subject_id')
  subject_test <- read.table('./data/UCI HAR Dataset/test/subject_test.txt', col.names='subject_id')
  
  train_data_bound <- cbind(y_train, subject_train, x_train)
  test_data_bound <- cbind(y_test, subject_test, x_test)
  train_and_test_data_bound <- rbind(train_data_bound, test_data_bound)
  
  merge(activities, train_and_test_data_bound)
}

get_required_columns <- function(merged_data) {
  pattern_mean <- '*ean*'
  pattern_std <- '*std*'
  
  sort(c(2, 3, grep(pattern_mean, names(merged_data)), grep(pattern_std, names(merged_data))))
}