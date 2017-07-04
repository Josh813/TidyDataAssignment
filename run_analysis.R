# 1 Merge the training and the test sets to create one data set.
data_test <- read.table("test/X_test.txt")
data_train <- read.table("train/X_train.txt")
data <- rbind(data_test,data_train)

# 2 Extract only the measurements on the mean and standard deviation 
# for each measurement.

features<- read.table("features.txt")
desired_features <- grep("mean\\(\\)|std\\(\\)", features$V2)
data <- data[,desired_features]

# 3 Use descriptive activity names to name the activities in the 
# data set.

activity_test <- read.table("test/y_test.txt")
activity_train <- read.table("train/y_train.txt")
activity <- rbind(activity_test, activity_train)

activity_label <- read.table("activity_labels.txt")
activity_func <- function(x) activity_label[x,2]
activity_col <- sapply(activity$V1, activity_func)

data <- cbind(activity_col, data)

# 4 Appropriately labels the data set with descriptive variable names
index <- sub("V","",names(data))
variable_func <- function(x) features[x,2]
names(data) <- sapply(index, variable_func)
names(data)[1] <- "activity"

# 5 From the data set in step 4, creates a second, independent tidy 
# data set with the average of each variable for each activity and 
# each subject.

subject_test <- read.table("test/subject_test.txt")
subject_train <- read.table("train/subject_train.txt")
subject_col <- rbind(subject_test, subject_train)

data <- cbind(subject_col, data)
names(data)[1] <- "subject"

library(dplyr)

tidydata <- melt(data, id=c("subject","activity"), measure.vars = names(data)[3:68])
tidydata <- group_by(tidydata, subject)
tidydata <- group_by(tidydata, activity, add=TRUE)
tidydata <- group_by(tidydata, variable, add=TRUE)

tidydata <- summarize (tidydata, mean(value))
write.table(tidydata, "tidydata.txt", row.names=FALSE)
