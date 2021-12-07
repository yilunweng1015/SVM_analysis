play_svm_avgcond <- function(subid,seednum){
  library(reshape)
  library(reshape2)
  library(ISLR)
  library(caret) # R package for SVM
  # Read file
  df <- read.csv(subid, header = TRUE)
  # Split the whole data into 146 time points
  df$condition<-as.factor(df$condition)
  data_split <- split(df, df$timepoint)
  # Start running Support Vector Machine
  svm <- function(df){
    # Set seed
    set.seed(seednum)
    # Split the data into training and test set
    intrain <- createDataPartition(y = df$condition, p=8/9, list = FALSE) # 8/9
    training <- df[intrain,]
    testing <- df[-intrain,]
    trctrl <- trainControl(method = "LOOCV") # Use Leave-one-out cross validation
    # Fit the model on the training data
    set.seed(seednum)
    svm_Linear <- train(condition ~., data = training, method = "svmLinear", 
                        trControl=trctrl,
                        preProcess = c("center", "scale"))
    # Make prediction on the test data
    pred <- predict(svm_Linear,testing)
    # Print stats of the result
    mat <- confusionMatrix(pred, testing$condition)
    res <- mat$overall[1]
  }
  
  acc <- matrix(0, nrow=146, ncol=1)
  for (i in 1:length(data_split)) {
    acc[i,]<- svm(data_split[[i]])
  }
  
  write.csv(acc,file=paste0("svm_3cond_avg_acc_50seed_",seednum,subid,sep=""), row.names = TRUE)
}

# We try 50 different seeds and then compare the results
id_seed <- function(id){
 play_svm_avgcond(id,1)
 play_svm_avgcond(id,3)
 play_svm_avgcond(id,6)
 play_svm_avgcond(id,7)
 play_svm_avgcond(id,9)
 play_svm_avgcond(id,10)
 play_svm_avgcond(id,24)
 play_svm_avgcond(id,35)
 play_svm_avgcond(id,31)
 play_svm_avgcond(id,32)
 play_svm_avgcond(id,21)
 play_svm_avgcond(id,20)
 play_svm_avgcond(id,34)
 play_svm_avgcond(id,35)
 play_svm_avgcond(id,38)
 play_svm_avgcond(id,39)
 play_svm_avgcond(id,44)
 play_svm_avgcond(id,40)
 play_svm_avgcond(id,41)
 play_svm_avgcond(id,42)
 play_svm_avgcond(id,45)
 play_svm_avgcond(id,46)
 play_svm_avgcond(id,48)
 play_svm_avgcond(id,49)
 play_svm_avgcond(id,50)
 play_svm_avgcond(id,51)
 play_svm_avgcond(id,52)
 play_svm_avgcond(id,55)
 play_svm_avgcond(id,56)
 play_svm_avgcond(id,57)
 play_svm_avgcond(id,58)
 play_svm_avgcond(id,59)
 play_svm_avgcond(id,60)
 play_svm_avgcond(id,63)
 play_svm_avgcond(id,64)
 play_svm_avgcond(id,65)
 play_svm_avgcond(id,66)
 play_svm_avgcond(id,67)
 play_svm_avgcond(id,68)
 play_svm_avgcond(id,69)
 play_svm_avgcond(id,70)
 play_svm_avgcond(id,71)
 play_svm_avgcond(id,72)
 play_svm_avgcond(id,73)
 play_svm_avgcond(id,74)
 play_svm_avgcond(id,75)
 play_svm_avgcond(id,76)
 play_svm_avgcond(id,77)
 play_svm_avgcond(id,78)
 play_svm_avgcond(id,80)
}

