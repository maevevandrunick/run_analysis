
library(dplyr)

data_path <- "UCI HAR Dataset"

features <- read.table(
  file.path(data_path, "features.txt"),
  stringsAsFactors = FALSE
)

activity_labels <- read.table(
  file.path(data_path, "activity_labels.txt"),
  stringsAsFactors = FALSE
)

x_train <- read.table(
  file.path(data_path, "train", "X_train.txt")
)

y_train <- read.table(
  file.path(data_path, "train", "y_train.txt")
)

subject_train <- read.table(
  file.path(data_path, "train", "subject_train.txt")
)

x_test <- read.table(
  file.path(data_path, "test", "X_test.txt")
)

y_test <- read.table(
  file.path(data_path, "test", "y_test.txt")
)

subject_test <- read.table(
  file.path(data_path, "test", "subject_test.txt")
)

colnames(x_train) <- features$V2
colnames(x_test) <- features$V2

colnames(y_train) <- "activity"
colnames(y_test) <- "activity"

colnames(subject_train) <- "subject"
colnames(subject_test) <- "subject"

train_data <- cbind(subject_train, y_train, x_train)
test_data <- cbind(subject_test, y_test, x_test)

merged_data <- rbind(train_data, test_data)

mean_std_cols <- grepl("mean\\(\\)|std\\(\\)", names(merged_data))

tidy_data <- merged_data[, c(TRUE, TRUE, mean_std_cols)]

tidy_data$activity <- factor(
  tidy_data$activity,
  levels = activity_labels$V1,
  labels = activity_labels$V2
)

names(tidy_data) <- names(tidy_data) %>%
  gsub("^t", "Time", .) %>%
  gsub("^f", "Frequency", .) %>%
  gsub("Acc", "Accelerometer", .) %>%
  gsub("Gyro", "Gyroscope", .) %>%
  gsub("Mag", "Magnitude", .) %>%
  gsub("BodyBody", "Body", .)

tidy_data_average <- tidy_data %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean), .groups = "drop")

write.table(
  tidy_data_average,
  file = "tidy_data.txt",
  row.names = FALSE
)
