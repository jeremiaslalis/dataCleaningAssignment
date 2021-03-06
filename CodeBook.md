﻿# Human Activity Recognition Using Smartphones Analyzed Dataset
## Data Dictionary

> INFORMATION ABOUT THE DATASET:  
> The tidy dataset described in this Code Book was created using the following files:  
> + 'features_info.txt': Shows information about the variables used on the feature vector.  
> + 'activity_labels.txt': Links the class labels with their activity name.  
> + 'train/X_train.txt': Training set.  
> + 'train/y_train.txt': Training labels.  
> + 'test/X_test.txt': Test set.  
> + 'test/y_test.txt': Test labels.  

> The following files are available for the train and test data. Their descriptions are equivalent.  
+ 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.  

> Note: Each feature vector is a row on the text file.


| **subject** | **2** |    
| --- | --- |
Subject Label 
+ 1..30 .Unique Identifier assigned for each subject who performed the activity for each window sample.                  

| **activity_names** |	**18**  
| --- | --- |
Activity Label  
+ Six activities performed by each subject wearing a smartphone. Converted from its original value (1, 2, 3, 4, 5, 6) to:  
WALKING  	
WALKING_UPSTAIRS  
WALKING_DOWNSTAIRS  
SITTING  
STANDING  
LAYING  

Respectively.	 
 

  
>NOTES FOR THE FEATURES ONWARD:

>The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

>Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

>Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

>These signals were used to estimate variables of the feature vector for each pattern  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions:

tBodyAcc-XYZ  
tGravityAcc-XYZ  
tBodyAccJerk-XYZ  
tBodyGyro-XYZ  
tBodyGyroJerk-XYZ  
tBodyAccMag  
tGravityAccMag  
tBodyAccJerkMag  
tBodyGyroMag  
tBodyGyroJerkMag  
fBodyAcc-XYZ  
fBodyAccJerk-XYZ    
fBodyGyro-XYZ  
fBodyAccMag  
fBodyAccJerkMag  
fBodyGyroMag  
fBodyGyroJerkMag  

>The mean (**mean()**) and standard deviation (**std()**) of the above signals were then estimated and selected. Finally, the average of each signal for each activity and each subject were calculated for the following set of variables:          

| **tBodyAcc-mean()-X** | **11** |
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.        

| **tBodyAcc-mean()-Y** | **11** |      
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAcc-mean()-Z** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAcc-std()-X** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAcc-std()-Y** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.        

| **tBodyAcc-std()-Z** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.  

| **tGravityAcc-mean()-X** | **11** | 
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.      

| **tGravityAcc-mean()-Y** | **11** | 
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.     

| **tGravityAcc-mean()-Z** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tGravityAcc-std()-X** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tGravityAcc-std()-Y** | **11** |  
| --- | --- |
+ 1..1 .Normalized and bounded value of the feature.

tGravityAcc-std()-Z** | **11** |       
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAccJerk-mean()-X** | **11** |    
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAccJerk-mean()-Y** | **11** |     
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAccJerk-mean()-Z** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAccJerk-std()-X** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAccJerk-std()-Y** | **11** |    
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAccJerk-std()-Z** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyro-mean()-X** | **11** |     
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyro-mean()-Y** | **11** |    
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyro-mean()-Z** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyro-std()-X** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyro-std()-Y** | **11** | 
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyro-std()-Z** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyroJerk-mean()-X** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyroJerk-mean()-Y** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyroJerk-mean()-Z** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyroJerk-std()-X** | **11** |      
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyroJerk-std()-Y** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyroJerk-std()-Z** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAccMag-mean()** | **11** |      
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.


| **tBodyAccMag-std()** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tGravityAccMag-mean()** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tGravityAccMag-std()** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAccJerkMag-mean()** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyAccJerkMag-std()** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyroMag-mean()** | **11** |       
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyroMag-std()** | **11** |    
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyroJerkMag-mean()** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **tBodyGyroJerkMag-std()** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAcc-mean()-X** | **11** |      
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAcc-mean()-Y** | **11** |    
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAcc-mean()-Z** | **11** | 
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAcc-std()-X** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAcc-std()-Y** | **11** |  
| --- | --- |
-1..1 .Normalized and bounded value of the feature.

| **fBodyAcc-std()-Z** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAccJerk-mean()-X** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAccJerk-mean()-Y** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAccJerk-mean()-Z** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAccJerk-std()-X** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAccJerk-std()-Y** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAccJerk-std()-Z** | **11** |     
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyGyro-mean()-X** | **11** |    
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyGyro-mean()-Y** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyGyro-mean()-Z** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyGyro-std()-X** | **11** |    
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyGyro-std()-Y** | **11** |     
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyGyro-std()-Z** | **11** |    
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAccMag-mean()** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyAccMag-std()** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyBodyAccJerkMag-mean()** | **11** |  
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyBodyAccJerkMag-std()** | **11** |
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyBodyGyroMag-mean()** | **11** |   
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyBodyGyroMag-std()** | **11** |
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyBodyGyroJerkMag-mean()** | **11** |
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.

| **fBodyBodyGyroJerkMag-std()** | **11** |
| --- | --- |
+ -1..1 .Normalized and bounded value of the feature.



>**NOTE:**  

>This Code book is an updated/modified version of the Code book provided in:

>*Human Activity Recognition Using Smartphones Dataset*  
>*Version 1.0*

>**Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto**
>Smartlab - Non Linear Complex Systems Laboratory
>DITEN - Università degli Studi di Genova.
>Via Opera Pia 11**A, I-16145, Genoa, Italy.
>activityrecognition@smartlab.ws
>www.smartlab.ws