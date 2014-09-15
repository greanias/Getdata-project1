#Local working directory for development.  Commented out for final submission.
#setwd("../Downloads/cleaningdatap1")

#Part 0 - Load test and train data files, putting headers in place

headernames <- read.table("features.txt",header=FALSE)
testdata <- read.table("X_test.txt",header=FALSE)
names(testdata) <- headernames$V2
traindata <- read.table("X_train.txt",header=FALSE)
names(traindata) <- headernames$V2

#Part 1 - Merge train and test data sets.  Able to use rbind as both data sets have the same set of columns.
commondata <- rbind(testdata,traindata)

#Part 2 - Extract mean and standard deviation fields into a new data table.
#  Only columns which were means or standard deviations of explicit values are included per project notes.
#  Means of averaged values were excluded.
#
#  Check each column name in the table.  If it contains the string "mean()" or "std()" then it conforms to
#  one of the two values defined in step 2.  It will be added to the new data table.

measuredata <- NULL

for (index in 1:ncol(commondata)) {
  if ((grepl("mean()",names(commondata[index])) & !(grepl("meanFreq()",names(commondata[index])))) | (grepl("std()",names(commondata[index])))) {
    if (is.null(measuredata)) {measuredata <- commondata[index]}
        else { measuredata <- cbind(measuredata,commondata[index])}
  }
}

#Part 3 - Apply activity names to activities
#  There are 6 defined activities.  Those values are read in from files, concatenated together into a single list,
#  replaced with meaningful names, and finally matched up to their rows.

y_test <- read.table("y_test.txt")
y_train <- read.table("y_train.txt")
y_full <- rbind(y_test,y_train)

activity <- NULL

for (index in 1:nrow(y_full)) {
  if (y_full[[1]][index] == 1) {activity[index] <- "Walking"}
  else if (y_full[[1]][index] == 2) {activity[index] <- "Walking Up Stairs"}
  else if (y_full[[1]][index] == 3) {activity[index] <- "Walking Down Stairs"}
  else if (y_full[[1]][index] == 4) {activity[index] <- "Sitting"}
  else if (y_full[[1]][index] == 5) {activity[index] <- "Standing"}
  else { activity[index] <- "Lying Down"}
}

nameddata <- cbind(activity,measuredata)

#Part 4 - Label data set with descriptive variables
#  There are 66 variables for mean and standard deviation and one for the activity.
#  Per the read process in my step 0, the variable names were read in and applied properly to the columns.
#  The variable for the first column, activity, was given a descriptive name to allow the cbind to address
#  the need in that case.

#  No additional steps required here due to previous processes.

#Part 5 - Create new data set with averages of each variable.  Group the table according to the six activities.
#  On each of these activities, take the mean.

finalgroup <- group_by(nameddata,activity)
finalmean <- summarise_each(finalgroup,funs(mean))

#Part Submission

write.table(finalmean, "combined_means.txt",row.name=FALSE)
