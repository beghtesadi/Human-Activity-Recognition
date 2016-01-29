library(caret)

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


# First seven columns are not useful in prediction
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


# Partition training data set

# trainIN <- createDataPartition(TrainData$classe , p=0.75 , list = FALSE)
# training <- TrainData[trainIN ,]
# testing <- TrainData[-trainIN ,]

for (i in 1:length(TestData) ) {
  for(j in 1:length(TrainData)) {
    if( length( grep(names(TrainData[i]), names(TestData)[j]) ) ==1)  {
      class(TestData[j]) <- class(TrainData[i])
    }      
  }      
}



set.seed(123)
rf_fit <- train(TrainData$classe ~. , data = TrainData , method = 'rf' , trControl = trainControl(method = 'cv' , 5) , 
                ntree = 100 , do.trace=TRUE)



pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(pred_test)

