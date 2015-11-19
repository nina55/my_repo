## 1.   Merge the training and the test sets to create one data set.
## =====================================================================

## Read the files of participants (subjects) of this experiment:
## --------------------------------------------------

f_test <- './UCI HAR Dataset/test/subject_test.txt'
sub_t <- read.table(f_test)
## dim(sub_t)

f_tr <- './UCI HAR Dataset/train/subject_train.txt'
sub_tr <- read.table(f_tr)
## dim(sub_tr)

## Merge the tables of subjects
## ----------------------------

sub_ <- rbind(sub_t, sub_tr) ## will be used as factor 1
## dim(sub_)

## Read the files of activities
## -----------------------------------------

f_y_test <- './UCI HAR Dataset/test/y_test.txt'
y_t <- read.table(f_y_test)
## dim(y_t)

f_y_train <- './UCI HAR Dataset/train/y_train.txt'
y_tr <- read.table(f_y_train)
## dim(y_tr)

## Merge the tables of activities
## ------------------------------

y_ <- rbind(y_t, y_tr)
## dim(y_)  ## [1] 10299     1

## Read the files of processed measurements
## ------------------------------------------------
f_X_test <- './UCI HAR Dataset/test/X_test.txt'
X_t <- read.table(f_X_test)
## dim(X_t)

f_X_train <- './UCI HAR Dataset/train/X_train.txt'
X_tr <- read.table(f_X_train)
## dim(X_tr)

## Merge the tables of of processed measurements
## -----------------------------------------------
X_ <- rbind(X_t, X_tr)
## dim(X_)
## Result: [1] 10299   561

## 2. Extract only the measurements on the mean and standard deviation.
## =====================================================================

## Read the file of features
## ---------------------------------
f4 <- './UCI HAR Dataset/features.txt'
sub4 <- read.table(f4)
## dim(sub4)
## sub4[1,1]
## ----------------------------------------

## Create vectors of of features (v_mean, v_mean_char), connected with 
## functions mean() è std():

## grepl(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
##                                                     useBytes = FALSE)
## grepl("mean()", s4, ignore.case = FALSE, fixed = TRUE)
## grepl("std()", s4, ignore.case = FALSE, fixed = TRUE)
## -----------------------------------------------------
v_mean <- vector("numeric",0)
v_mean_char <- vector("character",0)
n <- 1
for(i in 1:561){
	if(grepl("mean()", sub4[i,2], ignore.case = FALSE, fixed = TRUE) | 
	   grepl("std()", sub4[i,2], ignore.case = FALSE, fixed = TRUE)){
		v_mean <- c(v_mean, i)
		v_mean_char <- c(v_mean_char, as(sub4[i,2],"character"))
	}
}
## v_mean
## [1]   1   2   3   4   5   6  41  42  43  44  45  46  81  82  83  84  85
## [18]  86 121 122 123 124 125 126 161 162 163 164 165 166 201 202 214 215
## [35] 227 228 240 241 253 254 266 267 268 269 270 271 345 346 347 348 349
## [52] 350 424 425 426 427 428 429 503 504 516 517 529 530 542 543
## length(v_mean) ## => 66

## Extracting the necessary measurements
## -----------------------------------------

ex <- X_[, v_mean]

## dim(ex)
## [1] 10299    66

## 4. Label the data set (ex_ac ) with descriptive variable names
## ================================================================

colnames(ex) <- v_mean_char

## 3. Use descriptive activity names to name the activities in the data set
## ================================================================

## Read the reference table of activities
## ------------------------------------------
f_a <- './UCI HAR Dataset/activity_labels.txt'
act <- read.table(f_a)
## act
v_ac <- vector("character",6)
v_ac <- as(act[,2], "character")

y_a <- y_[,1]  ## vector 
v_y_a <- vector("character",10299) ## vector of characters for activities 
for(i in 1:10299){
	code <- y_a[i]
	v_y_a[i] <- v_ac[code] ## will be used as factor 2
}

## 5. From the data set in step 4, create a second, independent tidy data set
##    with the average of each variable for each activity and each subject.
## ================================================================

## The list of factors:
## ------------------------
lf <- list(sub_[,1], v_y_a)

ex_split <- split(ex, lf, drop = TRUE) ## splitting by person & activity
## print("length(ex_split)=")
## print(length(ex_split)) ## [1] 180

new <- ex[1,] ## new dataset with the same structure as dataset ex
for(i in 1:length(ex_split)){
	new[i,] <- sapply(ex_split[[i]], mean)
}
sub.activity <- names(ex_split) ## names of parts of splitting 
new1 <- cbind(sub.activity, new) ## insert the 1-st column

## 6. Create txt-file with write.table() using row.name=FALSE for uploading dataset new1 
## =====================================================================================

## write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",
##             eol = "\n", na = "NA", dec = ".", row.names = TRUE,
##             col.names = TRUE, qmethod = c("escape", "double"),
##             fileEncoding = "")

## write.table(new1, file = "My_dataset.txt", row.name=FALSE)