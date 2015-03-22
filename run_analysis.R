##########################################################################################################
# Filename: run_analysis.R
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of
#    each variable for each activity and each subject.
##########################################################################################################

## BEGIN SCRIPT ##

###########################
# Setting the environment #
###########################

# The data for the project... (the remote file to be downloaded)
fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
myZIPFile <- "./ProjectFiles.zip"                              # Local data file
dataDirectory <- "./UCI HAR Dataset"                           # Data folder or data directory

# Data file paths
xTrainPath <- paste(dataDirectory, "/train/X_train.txt", sep="")
xTestPath <- paste(dataDirectory, "/test/X_test.txt", sep="")
yTrainPath <- paste(dataDirectory, "/train/y_train.txt", sep="")
yTestPath <- paste(dataDirectory, "/test/y_test.txt", sep="")
subjectTrainPath <- paste(dataDirectory, "/train/subject_train.txt", sep="")
subjectTestPath <- paste(dataDirectory, "/test/subject_test.txt", sep="")
featuresPath <- paste(dataDirectory, "/features.txt", sep="")
activityLabelsPath <- paste(dataDirectory, "/activity_labels.txt", sep="")

# The tidy data
# Provide another data directory for the tidy data?
tidyDataFile <- "./tidyDataset.txt"           # Preparation for STEP 1.
tidyTxtAVGDataFile <- "./tidyAVGDataset.txt"  # Preparation for STEP 5.

#############################################
# Making the dataset localy available for R #
#############################################
if (file.exists(myZIPFile)==FALSE) {                       # Downloading the zipped dataset...
  download.file(fileURL, destfile = myZIPFile, mode="wb")  # mode="wb" to ensure download as a binary file.
}
if (file.exists(dataDirectory)==FALSE) {                   # Unzipping the dataset...
  unzip(myZIPFile)
}

#########################################
# Reading the files into their datasets #
#########################################
subjectTrain <- read.table(subjectTrainPath, header=FALSE)       # 7352 x 1
subjectTest <- read.table(subjectTestPath, header=FALSE)         # 2947 x 1
xTrain <- read.table(xTrainPath, header=FALSE)                   # 7352 x 561
xTest <- read.table(xTestPath, header=FALSE)                     # 2847 x 561
yTrain <- read.table(yTrainPath, header=FALSE)                   # 7352 x 1
yTest <- read.table(yTestPath, header=FALSE)                     # 2947 x 1
featuresDF <- read.table(featuresPath, header=FALSE)             # 561 x 2
activityLabelsDF <- read.table(activityLabelsPath, header=FALSE) # 6 x 2

# STEP 3: Uses descriptive activity names to name the activities in the data set.
# STEP 4: Appropriately labels the data set with descriptive variable names. 
names(featuresDF) <- c('feature_id', 'feature_description')  # Giving readable names to featuresDF columns
names(activityLabelsDF) <- c('activity_id', 'activity_name') # Giving readable names to activityLabelsDF columns

####################################################
# Preparing to make it all fit in a single dataset #
####################################################
# Preparing STEP 1: Merges the training and the test sets to create one data set.
# Binding tables one under the other, according to their subject...
xDF <- rbind(xTrain, xTest)                                # 10299 x 561
yDF <- rbind(yTrain, yTest)                                # 10299 x 1
subjectDF <- rbind(subjectTrain, subjectTest)              # 10299 x 1
names(yDF) <- "Activity"                                   # Giving readable names to yDF columns
names(subjectDF) <- "Subject"                              # Giving readable names to subjectDF columns

# Preparing STEP 2: Extracts only the measurements on the mean and standard deviation
#                   for each measurement.
featuresDFIndex <- grep("-mean\\(\\)|-std\\(\\)"
                       , featuresDF$feature_description) # Finding the matches for featuresDF$feature_description containing mean (mean) or standard deviation (std)...
xDF <- xDF[, featuresDFIndex]                            # featuresDF receives only its own columns that contain the measurements on the mean and standard deviation for each measurement.
                                                         # str(xDF) => 10299 obs. of  66 variables <= number of xDF observations is reduced now.
names(xDF) <- gsub("\\(|\\)", ""
                   , (featuresDF[featuresDFIndex, 2]))   # Replaces all numbered column names with its respective feature name column in featuresDF that match the featuresDFIndex.

# STEP 4: Appropriately labels the data set with descriptive activity names.
yDF[, 1] = activityLabelsDF[yDF[, 1], 2]                 # Replacing the codes in yDF with the propper activity names.

# STEP 1: Merges the training and the test sets to create one data set.
tidyDF <- cbind(subjectDF, yDF, xDF)                     # Now we have only one dataset with subject, activity, and mesures.
                                                         # 10299 obs. of  68 variables

# Preparing STEP 5: From the data set in step 4, creates a second, independent tidy data set with the average of
#                   each variable for each activity and each subject.
avgDF <- tidyDF[, 3:dim(tidyDF)[2]]                                       # 10299 obs. of  66 variables
tidyAVGDF <- aggregate(avgDF,list(tidyDF$Subject, tidyDF$Activity), mean) # Calculating the mean for each triple (variable, activity, subject)
                                                                          # 180 obs. of  68 variables
# Writing the tidy data sets...
# STEP 1: Merges the training and the test sets to create one data set.
write.table(tidyDF, tidyDataFile)

# STEP 5: From the data set in step 4, creates a second, independent tidy data set with the average of
#         each variable for each activity and each subject.
#         Detail: write.table() using row.names=FALSE
#                                               # Despite I'm using row.names = FALSE...
names(tidyAVGDF)[1] <- "Subject"                # The column names are shown in the final file...
names(tidyAVGDF)[2] <- "Activity"               # So, I'm making it more meaninful, giving them readable names.
write.table(tidyAVGDF, tidyTxtAVGDataFile, row.names=FALSE)

## END SCRIPT ##
