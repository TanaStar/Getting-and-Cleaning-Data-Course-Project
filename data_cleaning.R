library(dplyr)

# Download and unzip
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "UCI_HAR_Dataset.zip"
unzip_dir <- "UCI_HAR_Dataset"
download.file(url, destfile, mode = "wb")
unzip(destfile, exdir = unzip_dir)

# Paths
data_dir <- file.path(unzip_dir, "UCI HAR Dataset")
train_dir <- file.path(data_dir, "train")
test_dir <- file.path(data_dir, "test")

# Read features and activity labels
features <- read.table(file.path(data_dir, "features.txt"), col.names = c("index", "feature"))
activity_labels <- read.table(file.path(data_dir, "activity_labels.txt"), col.names = c("code", "activity"))

# Reading data
X_train <- read.table(file.path(train_dir, "X_train.txt"))
y_train <- read.table(file.path(train_dir, "y_train.txt"), col.names = "activity")
subject_train <- read.table(file.path(train_dir, "subject_train.txt"), col.names = "subject")

X_test <- read.table(file.path(test_dir, "X_test.txt"))
y_test <- read.table(file.path(test_dir, "y_test.txt"), col.names = "activity")
subject_test <- read.table(file.path(test_dir, "subject_test.txt"), col.names = "subject")

# Merge train and test sets (task 1)
X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
colnames(X) <- features$feature

# Extracting mean and std measurements (task 2)
mean_std_cols <- grep("mean\\(\\)|std\\(\\)", features$feature)
X_mean_std <- X[, mean_std_cols]

# Combining the data
data <- cbind(subject, y, X_mean_std)

# Names of activities (task 3)
data <- merge(data, activity_labels, by.x = "activity", by.y = "code", all.x = TRUE)
data$activity <- data$activity.y
data$activity.y <- NULL
data$activity.x <- NULL

# Variable names (task 4)
clean_names <- function(names_vec) {
  names_vec <- gsub("^t", "Time", names_vec)
  names_vec <- gsub("^f", "Frequency", names_vec)
  names_vec <- gsub("Acc", "Accelerometer", names_vec)
  names_vec <- gsub("Gyro", "Gyroscope", names_vec)
  names_vec <- gsub("Mag", "Magnitude", names_vec)
  names_vec <- gsub("BodyBody", "Body", names_vec)
  names_vec <- gsub("-mean\\(\\)", "Mean", names_vec)
  names_vec <- gsub("-std\\(\\)", "StdDev", names_vec)
  names_vec <- gsub("-", "", names_vec)
  names_vec
}
colnames(data)[-(1:2)] <- clean_names(colnames(data)[-(1:2)])

# Tidying (task 5)
tidy_data <- data %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean), .groups = "drop")

# Save the output file
write.table(tidy_data, "tidy.txt", row.names = FALSE)