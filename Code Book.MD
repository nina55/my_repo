             The code book for the data set created by the script run_analysis.R
             ----------------------------------------------------------------------
 Data set (new1) produced as a result of running the script run_analysis.R is saved in the file My_dataset.txt.

 Variables (the names of columns):
 --------------------------------
 1 sub.activity 			- combination of a subject (an integer from 1 to 30 representing the person) and
                                a label for the activity (walking, walkingupstairs, walkingdownstairs,
                                sitting, standing and laying)
 2 tBodyAcc-mean()-X		- mean of the body's acceleration along the x-axis 
                                (prefix "t" means a function of time)
 3 tBodyAcc-mean()-Y		- mean of the body's acceleration along the y-axis 
 4 tBodyAcc-mean()-Z		- mean of the body's acceleration along the z-axis
 5 tBodyAcc-std()-X		- standard deviation of the body's acceleration along the x-axis 
 6 tBodyAcc-std()-Y 		- standard deviation of the body's acceleration along the y-axis
 7 tBodyAcc-std()-Z		- standard deviation of the body's acceleration along the z-axis
 8 tGravityAcc-mean()-X		- mean of the body's acceleration along the x-axis in relation to gravity
 9 tGravityAcc-mean()-Y		- mean of the body's acceleration along the y-axis in relation to gravity
10 tGravityAcc-mean()-Z		- mean of the body's acceleration along the z-axis in relation to gravity
11 tGravityAcc-std()-X		- standard deviation of the body's acceleration along the x-axis in relation to gravity
12 tGravityAcc-std()-Y		- standard deviation of the body's acceleration along the y-axis in relation to gravity
13 tGravityAcc-std()-Z		- standard deviation of the body's acceleration along the z-axis in relation to gravity
14 tBodyAccJerk-mean()-X	- mean of the body's acceleration along the x-axis in relation to jerk signals
15 tBodyAccJerk-mean()-Y	- mean of the body's acceleration along the y-axis in relation to jerk signals
16 tBodyAccJerk-mean()-Z	- mean of the body's acceleration along the z-axis in relation to jerk signals
17 tBodyAccJerk-std()-X		- standard deviation of the body's acceleration along the x-axis in relation to jerk signals
18 tBodyAccJerk-std()-Y		- standard deviation of the body's acceleration along the y-axis in relation to jerk signals
19 tBodyAccJerk-std()-Z		- standard deviation of the body's acceleration along the z-axis in relation to jerk signals
20 tBodyGyro-mean()-X		- mean of the body's gyroscopic movement along the x-axis
21 tBodyGyro-mean()-Y		- mean of the body's gyroscopic movement along the y-axis
22 tBodyGyro-mean()-Z		- mean of the body's gyroscopic movement along the z-axis
23 tBodyGyro-std()-X		- standard deviation of the body's gyroscopic movement along the x-axis
24 tBodyGyro-std()-Y		- standard deviation of the body's gyroscopic movement along the y-axis
25 tBodyGyro-std()-Z		- standard deviation of the body's gyroscopic movement along the z-axis
26 tBodyGyroJerk-mean()-X	- mean of the body's gyroscopic movement in relation to jerk signals along the x-axis
27 tBodyGyroJerk-mean()-Y	- mean of the body's gyroscopic movement in relation to jerk signals along the y-axis
28 tBodyGyroJerk-mean()-Z	- mean of the body's gyroscopic movement in relation to jerk signals along the z-axis
29 tBodyGyroJerk-std()-X	- standard deviation of the body's gyroscopic movement in relation to jerk signals along the x-axis
30 tBodyGyroJerk-std()-Y	- standard deviation of the body's gyroscopic movement in relation to jerk signals along the y-axis
31 tBodyGyroJerk-std()-Z	- standard deviation of the body's gyroscopic movement in relation to jerk signals along the z-axis
32 tBodyAccMag-mean()		- mean of the magnitude of the body's acceleration
33 tBodyAccMag-std()		- standard deviation of the magnitude of the body's acceleration
34 tGravityAccMag-mean()	- mean of the magnitude of the body's acceleration in relation to gravity
35 tGravityAccMag-std()		- standard deviation of the magnitude of the body's acceleration in relation to gravity
36 tBodyAccJerkMag-mean()	- mean of the magnitude of the body's acceleration in relation to jerk signals
37 tBodyAccJerkMag-std()	- standard deviation of the magnitude of the body's acceleration in relation to jerk signals
38 tBodyGyroMag-mean()		- mean of the magnitude of the body's gyroscopic movement
39 tBodyGyroMag-std()		- standard deviation of the magnitude of the body's gyroscopic movement
40 tBodyGyroJerkMag-mean()	- mean of the magnitude of the body's gyroscopic movement in relation to jerk signals
41 tBodyGyroJerkMag-std()	- standard deviation of the magnitude of the body's gyroscopic movement in relation to jerk signals
42 fBodyAcc-mean()-X		- mean of the body's acceleration along the x-axis
					  (prefix "f" means a function of frequency)
43 fBodyAcc-mean()-Y		- mean of the body's acceleration along the y-axis
44 fBodyAcc-mean()-Z		- mean of the body's acceleration along the z-axis
45 fBodyAcc-std()-X		- standard deviation of the body's acceleration along the x-axis
46 fBodyAcc-std()-Y		- standard deviation of the body's acceleration along the y-axis
47 fBodyAcc-std()-Z		- standard deviation of the body's acceleration along the z-axis
48 fBodyAccJerk-mean()-X	- mean of the body's acceleration along the x-axis in relation to jerk signals
49 fBodyAccJerk-mean()-Y	- mean of the body's acceleration along the y-axis in relation to jerk signals
50 fBodyAccJerk-mean()-Z	- mean of the body's acceleration along the z-axis in relation to jerk signals
51 fBodyAccJerk-std()-X		- standard deviation of the body's acceleration along the x-axis in relation to jerk signals
52 fBodyAccJerk-std()-Y		- standard deviation of the body's acceleration along the y-axis in relation to jerk signals
53 fBodyAccJerk-std()-Z		- standard deviation of the body's acceleration along the z-axis in relation to jerk signals
54 fBodyGyro-mean()-X		- mean of the body's gyroscopic movement along the x-axis
55 fBodyGyro-mean()-Y		- mean of the body's gyroscopic movement along the y-axis
56 fBodyGyro-mean()-Z		- mean of the body's gyroscopic movement along the z-axis
57 fBodyGyro-std()-X		- standard deviation of the body's gyroscopic movement along the x-axis
58 fBodyGyro-std()-Y		- standard deviation of the body's gyroscopic movement along the y-axis
59 fBodyGyro-std()-Z		- standard deviation of the body's gyroscopic movement along the z-axis
60 fBodyAccMag-mean()		- mean of the magnitude of the body's acceleration
61 fBodyAccMag-std()		- standard deviation of the magnitude of the body's acceleration
62 fBodyBodyAccJerkMag-mean()	- mean of the magnitude of the body's acceleration in relation to jerk signals
63 fBodyBodyAccJerkMag-std()	- standard deviation of the magnitude of the body's acceleration in relation to jerk signals
64 fBodyBodyGyroMag-mean()	- mean of the magnitude of the body's gyroscopic movement
65 fBodyBodyGyroMag-std()	- standard deviation of the magnitude of the body's gyroscopic movement
66 fBodyBodyGyroJerkMag-mean()- mean of the magnitude of the body's gyroscopic movement in relation to jerk signals
67 fBodyBodyGyroJerkMag-std() - standard deviation of the magnitude of the body's gyroscopic movement in relation to jerk signals

---------------------------------------------------------------------------------------------------------
All measurements are unitless (they have been normalized between -1 and 1).

