## runs data manipulation to create a tidy dataset from raw data
library(plyr); library(dplyr)
##Step 1: Merge the training and the test sets to create one data set.
## read in training measurement data
x_train_data <- read.table("./train/X_train.txt")

## read in labels for the measurement data
features <- read.table("features.txt", col.names = c("featureID","featureDesc"))

## Assign labels to data columns
names(x_train_data) <- features$featureDesc

## read in training activity id
## (values are 1-6. ex. 1 = WALKING, 2 = WALKING_UPSTAIRS, etc.)
y_train_activity_id <- read.table("./train/y_train.txt", col.names = "activityID")

## read in training people (aka subjects)
subject_train <- read.table("./train/subject_train.txt", col.names = "subjectID")

## Combine train subject, activity and data
train1 <- cbind(subject_train,y_train_activity_id,x_train_data)


## read in test measurement data
x_test_data <- read.table("./test/X_test.txt")

## Assign labels to data columns
names(x_test_data) <- features$featureDesc

## read in training activity id
## (values are 1-6. ex. 1 = WALKING, 2 = WALKING_UPSTAIRS, etc.)
y_test_activity_id <- read.table("./test/y_test.txt", col.names = "activityID")

## read in test people (aka subjects)
subject_test <- read.table("./test/subject_test.txt", col.names = "subjectID")

## Combine test subject, activity and data
test1 <- cbind(subject_test,y_test_activity_id,x_test_data)

## Combine test and training data
combined_test_train <- rbind(train1,test1)

## Step 2: Extract only the measurements on the mean and standard deviation for each measurement.
## get only columns for mean and standard deviation
combined_smaller <- combined_test_train[,c(1,2,grep("mean|std",names(combined_test_train)))]

## Step 3: Use descriptive activity names to name the activities in the data set.
activities <- read.table(file = "activity_labels.txt", sep = " ", col.names = c("activityID", "activitydesc"))
combined_smaller$activityID <- factor(x = combined_smaller$activityID, levels = c(1,2,3,4,5,6), labels = activities$activitydesc)
colnames(combined_smaller)[2] <- "activitydesc"

## Step 4: Clean up the variable names for duplicate "body" text
names(combined_smaller) <- gsub("BodyBody","Body",names(combined_smaller))

## Step 5: From the data set in step 4, creates a second, independent tidy data set with
## the average of each variable for each activity and each subject.

## Created melted data frame
step5DF <- melt(combined_smaller, id=c("subjectID","activitydesc"), measure.vars = colnames(combined_smaller)[3:81])
## summarize usinge mean
step5DF_Final <- dcast(step5DF, subjectID + activitydesc ~ variable,mean)
write.table(step5DF_Final, file = "step5DS.txt", row.names = FALSE)
step5DF_Final


