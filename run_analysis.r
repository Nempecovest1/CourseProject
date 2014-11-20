##############################
# Getting and Cleaning Data  #
# Course project             #
##############################

# Step 1: combining the test and train sets
test_subject = read.table("UCI HAR Dataset/test/subject_test.txt")
train_subject = read.table("UCI HAR Dataset/train/subject_train.txt")
test_y = read.table("UCI HAR Dataset/test/y_test.txt")
train_y = read.table("UCI HAR Dataset/train/y_train.txt")
test_x = read.table("UCI HAR Dataset/test/X_test.txt")
train_x = read.table("UCI HAR Dataset/train/X_train.txt")

combined = rbind(cbind(test_subject, test_y, test_x), cbind(train_subject, train_y, train_x))

# Step 2: extracting mean and standard deviation
var_names = read.table("UCI HAR Dataset/features.txt")
mean_vars = var_names[grepl("mean\\(\\)", var_names$V2),"V1"] + 2
std_vars = var_names[grepl("std\\(\\)", var_names$V2),"V1"] + 2
# Note: adding 2 is necessary to align indices with the data set's indices,
# the first two columns are already taken by activity and subject.

combined.mean_std = combined[,c(1, 2, mean_vars, std_vars)]

# Step 3: labels for activities
activity_labels = read.table("UCI HAR Dataset/activity_labels.txt")
combined[,2] = factor(combined[,2], levels=activity_labels[,1], labels=activity_labels[,2])
combined.mean_std[,2] = factor(combined.mean_std[,2], levels=activity_labels[,1], labels=activity_labels[,2])

# Step 4: labels for variables
colnames(combined.mean_std) = c("subject", "activity", as.vector(var_names[c(mean_vars-2, std_vars-2),2]))

# Step 5: means by subject/activity pair
subjects = unique(combined.mean_std$subject)
no_of_rows = length(subjects) * length(activity_labels[,1])
means_df = matrix(nrow=no_of_rows, ncol = length(combined.mean_std))
current_row = 1

for (subject in subjects) {
	subject_data = combined.mean_std[combined.mean_std$subject == subject,]
	for (activity in activity_labels[,2]) {
		means_df[current_row, 1] = subject
		if (activity %in% subject_data$activity) {
			means_df[current_row, 2] = activity
			for (column in 3:length(subject_data)) {
				mean = mean(subject_data[subject_data$activity == activity,column])
				means_df[current_row, column] = mean
			}
			current_row = current_row + 1
		}
	}
}
means_df = as.data.frame(means_df)
colnames(means_df) = colnames(combined.mean_std)

# Question 1: writing table
write.table(means_df, file="tidy_data.txt", row.names = FALSE)

# Note 1: the two main data sets of interest are combined.mean_std and means_df
# The former contains tidy data as of Step 4
# The latter contains the means of Step 5

# Note 2: rows in the means_df data set are NOT ordered by subject
# So just because it starts with subject 2 doesn't mean subject 1 was skipped