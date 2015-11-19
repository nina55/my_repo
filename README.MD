
The file (README.md) describes files used for creating the script run_analysis.R.

FILES:
==========================

- 'features_info.txt':		Information about the variables used on the feature vector.
- 'features.txt': 		List of all features.
- 'activity_labels.txt': 	Labels connected with their activity names.

- 'train/X_train.txt': 		Training dataset of measurements.
- 'train/y_train.txt': 		Training labels of activities. (Each row identifies the activity performed by
					the subject for each window sample. Its range is from 1 to 6.)
- 'train/subject_train.txt': 	Training numbers of subjects. (Each row identifies the subject who performed 
					the activity for each window sample. Its range is from 1 to 30.)

- 'test/X_test.txt': 		Test dataset of measurements.
- 'test/y_test.txt': 		Test labels of activities. (Each row identifies the activity performed by
					the subject for each window sample. Its range is from 1 to 6.)
- 'test/subject_test.txt': 	Test numbers of subjects. (Each row identifies the subject who performed 
					the activity for each window sample. Its range is from 1 to 30.)

NOTES:
==========================

R script run_analysis.R does the following. 

 1)   Merges the training and the test sets to create one data set.
 2)   Extracts only the measurements on the mean and standard deviation for each measurement. 
 3)   Uses descriptive activity names to name the activities in the data set.
 4)   Appropriately labels the data set with descriptive variable names. 
 5)   From the data set in step 4, creates a second, independent tidy data set
      with the average of each variable for each activity and each subject.


The detailed steps of performing these tasks are described in the comments of the script run_analysis.R.
