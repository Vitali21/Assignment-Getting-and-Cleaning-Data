if(!file.exists("data")){dir.create("data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile="./data/dataset.zip", mode="wb") 
setwd("./data")
unzip("dataset.zip", unzip = "internal")
file2 <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")

##########################    TASK#1   ############################
file2 <- read.table(file = "./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
features <- read.table(file = "./UCI HAR Dataset/features.txt", header = FALSE, sep = "")

#adding to table X_train columns names from file features.txt
colnames(file2) <- features$V2 

#indexing roews in table X_train(i.e file2)
file2 <- cbind(rownames(file2), file2)
names(file2)[1] <- "id"

#adding to table file2 column participant_id
subject_train <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "")
file2 <- cbind(subject_train$V1, file2)
names(file2)[1] <- "participant_id"

#adding to table file2 column activity_label
y_train <- read.table(file = "./UCI HAR Dataset/y_train.txt", header = FALSE, sep = "")
file2 <- cbind(y_train$V1, file2)
names(file2)[1] <- "activity_label"
#adding to all columns name "train_"
names(file2) <- paste("train", names(file2), sep = "_")



test_file2 <- read.table(file = "./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")
features <- read.table(file = "./UCI HAR Dataset/features.txt", header = FALSE, sep = "")

#adding to table X_test columns names from file features.txt
colnames(test_file2) <- features$V2 
test_file2 <- cbind(rownames(test_file2), test_file2) 
names(test_file2)[1] <- "id"

#adding to table test_file2 column participant_id
subject_test <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "")
test_file2 <- cbind(subject_test$V1, test_file2)
names(test_file2)[1] <- "participant_id"

#adding to table test_file2 column activity_label
y_test <- read.table(file = "./UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "")
test_file2 <- cbind(y_test$V1, test_file2)
names(test_file2)[1] <- "activity_label"

#adding to all columns name "test_"
names(test_file2) <- paste("test", names(test_file2), sep = "_")

#mergig train and test file to one file merged_file2 by train_id and test_id
merged_file2 <- merge(file2, test_file2, by.x = "train_id", by.y = "test_id")
##########################    TASK#1   ############################





##########################    TASK#2   ############################
#extracting only mean columns indexes
mean_columns <- grep("mean", names(merged_file2))

#extracting only mean columns names
mean_columns_table <- names(merged_file2)[mean_columns]

#extracting only standard deviation columns indexes
std_columns <- grep("std", names(merged_file2))

#extracting only standard deviation columns names
std_columns_table <- names(merged_file2)[std_columns]

#extracting from test and train file only mean and standard deviation columns 
merged_file2_task2 <- merged_file2[, c(mean_columns_table, std_columns_table)]
##########################    TASK#2   ############################




##########################    TASK#3   ############################
#Using descriptive activity names to name the activities in the data set
merged_file2$train_activity_label <- replace(merged_file2$train_activity_label,grep("1", merged_file2$train_activity_label),"WALKING")
merged_file2$train_activity_label <- replace(merged_file2$train_activity_label,grep("2", merged_file2$train_activity_label),"WALKING_UPSTAIRS")
merged_file2$train_activity_label <- replace(merged_file2$train_activity_label,grep("3", merged_file2$train_activity_label),"WALKING_DOWNSTAIRS")
merged_file2$train_activity_label <- replace(merged_file2$train_activity_label,grep("4", merged_file2$train_activity_label),"SITTING")
merged_file2$train_activity_label <- replace(merged_file2$train_activity_label,grep("5", merged_file2$train_activity_label),"STANDING")
merged_file2$train_activity_label <- replace(merged_file2$train_activity_label,grep("6", merged_file2$train_activity_label),"LAYING")

merged_file2$test_activity_label <- replace(merged_file2$test_activity_label,grep("1", merged_file2$test_activity_label),"WALKING")
merged_file2$test_activity_label <- replace(merged_file2$test_activity_label,grep("2", merged_file2$test_activity_label),"WALKING_UPSTAIRS")
merged_file2$test_activity_label <- replace(merged_file2$test_activity_label,grep("3", merged_file2$test_activity_label),"WALKING_DOWNSTAIRS")
merged_file2$test_activity_label <- replace(merged_file2$test_activity_label,grep("4", merged_file2$test_activity_label),"SITTING")
merged_file2$test_activity_label <- replace(merged_file2$test_activity_label,grep("5", merged_file2$test_activity_label),"STANDING")
merged_file2$test_activity_label <- replace(merged_file2$test_activity_label,grep("6", merged_file2$test_activity_label),"LAYING")
##########################    TASK#3   ############################


##########################    TASK#4   ############################

# is already done in TASK#1: colnames(file2) <- features$V2 

##########################    TASK#4  ############################




##########################    TASK#5   ############################
#select mean columns
#mean columns indexes are defined in task#2: mean_columns <- grep("mean", names(merged_file2))
test <- merged_file2[mean_columns]

#select also columns: "train_id", "train_activity_label" and "train_participant_id"
test2 <- merged_file2[c(1,2,3)]
#select also columns: "test_activity_label" and "test_participant_id"
test3 <- merged_file2[c(565, 566)]

# PEREIMENOVAT TEST

#add 5 above selected columns to file test
test <- cbind(test2, test)
test <- cbind(test3, test)

#remove train_id colum because it is unnecessary
test$train_id <- NULL

#changing all columns except first 4 columns to one column: features
test10 <- gather(test, features, mean, -c(1:4))
#changing columns test_activity_label and train_activity_label to columns: type and activity
test11 <- gather(test10, type, activity, -c(test_participant_id, train_participant_id, features, mean))
#separating column type to columns: type and label
test12 <- separate(test11, type, c("type", "label"))
#removing column: label
test12$label <- NULL
#renaming columns test_participantId and train_participantId so later it will be easy to separate thos columns into columns
names(test12)[1] <- "test_participantId"
names(test12)[2] <- "train_participantId"

#changing columns test_participantId and train_participantId to column: type2
test14 <- gather(test12, type2, subject, -c(features, mean, type, activity))

#renaming columns activity to activity_type and subkect to subject_type
names(test14)[3] <- "activity_type"
names(test14)[5] <- "subject_type"

#separating column subject_type to columns: subject_type and column
test15 <- separate(test14, subject_type, c("subject_type", "column"))
#removing column: column
test15$column <- NULL

#separating column features to columns: feature_type and feature
test16 <- separate(test15, features, c("feature_type", "feature"), sep = "_")

#separating column feature to columns: feature_type2, mean and XYZ
test17 <- separate(test16, feature, c("feature_type2", "mean", "XYZ"))
#renaming column feature_type2 to feature
names(test17)[2] <- "feature"
#removing column mean
test17$mean <- NULL
##########################    TASK#5   ############################














