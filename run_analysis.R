##################
### Setup env. ###
##################

# Clear environment #

rm(list = ls())

# load packages #

library(dplyr)

#################
### Read data ###
#################

# read train data # 
datalocation <- "UCI HAR Dataset"
subject_train <- read.table(file.path(datalocation, "train", "subject_train.txt"))
value_train <- read.table(file.path(datalocation, "train", "X_train.txt"))
activity_train <- read.table(file.path(datalocation, "train", "y_train.txt"))

# read test data #
subject_test <- read.table(file.path(datalocation, "test", "subject_test.txt"))
value_test <- read.table(file.path(datalocation, "test", "X_test.txt"))
activity_test <- read.table(file.path(datalocation, "test", "y_test.txt"))

# read features and activities data # 

features <- read.table(file.path(datalocation, "features.txt"), as.is = TRUE)

activities <- read.table(file.path(datalocation, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


##################
### Merge Data ###
##################

# Merge tables into one #

human_activity <- rbind(cbind(subject_train, value_train, activity_train),
                        cbind(subject_test, value_test, activity_test))

# edit col. names #

colnames(human_activity) <- c("subject", features[, 2], "activity")

############################
### Extract Measurements ###
############################

# Select mean and SD and then filter #

mean_SD <- grepl("subject|activity|mean|std", colnames(human_activity))
human_activity <- human_activity[, mean_SD]

###################################
#### Descriptive Activity Names ###
###################################

# change activity values into factors

human_activity$activity <- factor(human_activity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

##################################
### Descriptive Variable Names ###
##################################

# assign col. names #

human_activity_col <- colnames(human_activity)

# remove spec. characters #

human_activity_col <- gsub("[\\(\\)-]", "", human_activity_col)

# Spell out abbreviations and clean up #

human_activity_col <- gsub("^f", "Frequency_domain", human_activity_col)
human_activity_col <- gsub("^t", "Time", human_activity_col)
human_activity_col <- gsub("Acc", "Accelerometer", human_activity_col)
human_activity_col <- gsub("Gyro", "Gyroscope", human_activity_col)
human_activity_col <- gsub("Mag", "Magnitude", human_activity_col)
human_activity_col <- gsub("Freq", "Frequency", human_activity_col)
human_activity_col <- gsub("mean", "Mean", human_activity_col)
human_activity_col <- gsub("std", "Standard_deviation", human_activity_col)

# correct col. name for body #
human_activity_col <- gsub("BodyBody", "Body", human_activity_col)

# apply new labels to col. names #
colnames(human_activity) <- human_activity_col

#############################
### Second, Tidy Data Set ###
#############################

# group by subject / activity and summarise by means #
human_activity_means <- human_activity %>% 
    group_by(subject, activity) %>%
    summarise_all(funs(mean))

# write to file "tidy_data.txt" #
write.table(human_activity_means, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)

