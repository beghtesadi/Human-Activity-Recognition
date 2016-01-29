Summary
=======

In this project we use the data from accelerometers on the belt,
forearm, arm, and dumbell of 6 participants. The participants have done
the lifting in five different ways. The goal is to determine the quality
of the lifting for twenty movements given in the test data set. In this
report, we present how the data is obtained, and processed. The original
training data set contains 19622 observations with 160 variables. After
processing data, we fit the Random Forest method to the training data
set using cross validation.

Obtaining data and cleaning
===========================

We first load the caret library. Then, we download the data and read the
files as follows:

    library(caret)

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    setwd('/home/bahar/COURSERA/MachineLearning')

    # getting the training and testing data
    if (!file.exists("/home/bahar/COURSERA/Data/pml-training.csv"))
    {
      DataFile <- download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                               destfile = "/home/bahar/COURSERA/Data/pml-training.csv" , method = "curl")
    }

    if (!file.exists("/home/bahar/COURSERA/Data/pml-testing.csv"))
    {
      DataFile <- download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                                destfile = "/home/bahar/COURSERA/Data/pml-testing.csv" , method = "curl")
    }

    TrainData <- read.csv("/home/bahar/COURSERA/Data/pml-training.csv" , sep="," , header = TRUE)
    TestData <- read.csv("/home/bahar/COURSERA/Data/pml-testing.csv" , sep="," , header = TRUE)

First we let's see the columns of the training data set:

    dim(TrainData)

    ## [1] 19622   160

    head(names(TrainData), 40)

    ##  [1] "X"                    "user_name"            "raw_timestamp_part_1"
    ##  [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
    ##  [7] "num_window"           "roll_belt"            "pitch_belt"          
    ## [10] "yaw_belt"             "total_accel_belt"     "kurtosis_roll_belt"  
    ## [13] "kurtosis_picth_belt"  "kurtosis_yaw_belt"    "skewness_roll_belt"  
    ## [16] "skewness_roll_belt.1" "skewness_yaw_belt"    "max_roll_belt"       
    ## [19] "max_picth_belt"       "max_yaw_belt"         "min_roll_belt"       
    ## [22] "min_pitch_belt"       "min_yaw_belt"         "amplitude_roll_belt" 
    ## [25] "amplitude_pitch_belt" "amplitude_yaw_belt"   "var_total_accel_belt"
    ## [28] "avg_roll_belt"        "stddev_roll_belt"     "var_roll_belt"       
    ## [31] "avg_pitch_belt"       "stddev_pitch_belt"    "var_pitch_belt"      
    ## [34] "avg_yaw_belt"         "stddev_yaw_belt"      "var_yaw_belt"        
    ## [37] "gyros_belt_x"         "gyros_belt_y"         "gyros_belt_z"        
    ## [40] "accel_belt_x"

As can be seen, the first columns with the id, names, and timestamp
windows are not useful in the prediction. Therefore, they need to be
removed from the data set. Also, all the columns that contain NA values
in more than 60% percent of the rows are removed as well.

    Nuseful <- 1:7
    TrainData <- TrainData[ , -Nuseful]
    TestData <- TestData[, -Nuseful]

    # Write a function to determin how many of the columns are na
    NAColumns <- function (column)
    {
      if  ( sum(is.na(TrainData[, column])) > 0.6 * nrow(TrainData) )
      {
        return(TRUE)
      }
      else
      {
        return(FALSE)
      }
    }

    RemoveCol <- sapply(colnames(TrainData), NAColumns)
    TrainData <- TrainData[, !RemoveCol]
    TestData <- TestData[, !RemoveCol]
    TestData <- TestData[, -length(TestData)]

In the next step, we remove those columns that have low variance. This
helps to reduce the dimension of the data set.

    lowvar <- nearZeroVar(TrainData , saveMetrics = TRUE)
    TrainData <- TrainData[,!lowvar$nzv]

Fitting the model
=================

We apply Random Forest method to the training set as it is a robust
method with generaly high accuracy in the predictions. We use the
trainig data set to fit the model. Also, in order to obtain a better
estimate of the errors, we use cross validation. We apply the
trainControl function to generate 5 folds in the training data.

    set.seed(123)
    rf_fit <- train(TrainData$classe ~. , data = TrainData , method = 'rf' , trControl = trainControl(method = 'cv' , 5) , 
                    ntree = 200)

    ## Loading required package: randomForest

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    rf_fit

    ## Random Forest 
    ## 
    ## 19622 samples
    ##    52 predictors
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 15697, 15698, 15697, 15697, 15699 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
    ##    2    0.9952094  0.9939399  0.001605887  0.002031800
    ##   27    0.9938842  0.9922635  0.001775458  0.002246641
    ##   52    0.9887879  0.9858169  0.002624441  0.003319307
    ## 
    ## Accuracy was used to select the optimal model using  the largest value.
    ## The final value used for the model was mtry = 2.

Results and Conclusion
======================

The accuray is 99% and the in-sample error produced by the cross
validation is good enough to use this method to predict the test sample.
As can be seen, the predictions for the test sample are 100% correct.

    pred_test <- predict(rf_fit , TestData)
    pred_test

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E
