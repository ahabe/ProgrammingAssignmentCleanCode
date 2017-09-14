## Analysis of wearable device data. 

## It takes an optional argument of a directory
## with traning and test data of Samsung Galaxy S 
## smartphone. 
## The following steps are performed:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
run_analysis <- function() {
  # Read the feature labels
  feature       <- read.table("features.txt", col.names = c("index", "name"))
  
  # Extract only the intressting variables
  featureSel    <- grep("(std|mean)\\(\\)", feature$name)
  
  #make the variables more descriptive
  # replace t by Time
  feature$name   <- sub( "^t", "Time", feature$name)
  # replace f by Frequency
  feature$name <- sub( "^f", "Frequency", feature$name)
  feature$name <- sub('.mean', 'Mean', feature$name)
  feature$name <- sub('.std', 'Std', feature$name)
  feature$name <- gsub('[()-]', '', feature$name)

  # read the activity lables
  activity      <- read.table("activity_labels.txt", col.names = c("index", "name"))
  # make the names more descriptive
  # keep the name lowercase
  activity$name  <- tolower(activity$name)
  
  # read the train data
  train_data    <- read_data_set("train", feature, featureSel)
  # read the test data 
  test_data     <- read_data_set("test", feature, featureSel)
  
  # merge the data togehter
  data <- rbind(train_data, test_data);
  
  # transform the activity to descriptive values
  toName        <- function(x) { activity[x,2]}
  data$Activity     <- sapply(data$Activity, toName)
  
  # make activity as factors
  data$Activity   <- as.factor(data$Activity)
  
  data[["Subject"]] <- as.factor(data[, "Subject"])
  
  data <- reshape2::melt(data = data, id = c("Subject", "Activity"))
  data <- reshape2::dcast(data = data, Subject + Activity ~ variable, fun.aggregate = mean)
  
  data.table::fwrite(x = data, file = "resultData.txt", quote = FALSE, row.name=FALSE)
  data
}

## read a specifc data set including, subject and activity
## dataset: char vector, either "test" or "train"
## feature: the variable names
## sel: the wanted variabkes
read_data_set <- function (dataset, feature, sel)
{
  # build data path
  path_y   <- sprintf("%s/y_%s.txt",       dataset,dataset)
  path_sub <- sprintf("%s/subject_%s.txt", dataset,dataset)
  path_x   <- sprintf("%s/X_%s.txt",       dataset,dataset)
  
  # read the activity coloumn
  data_y   <- read.table(path_y)
  
  # read the suject table
  data_sub <- read.table(path_sub)
  
  # read the observations
  data_x   <- read.table(path_x, col.names = feature$name)[, sel]
  
  # apply subject, activity and data_x in one data
  data <- cbind(data_sub, data_y, data_x)
  colnames(data)[1] <- "Subject"
  colnames(data)[2] <- "Activity"
  data
}
