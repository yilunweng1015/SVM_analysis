play_svm_avgcond <- function(subid,seednum){
  library(dplyr)
  library(tidyr)
  library(reshape)
  library(reshape2)
  library(ISLR)
  library(caret)
  df <- read.csv(subid, header = TRUE)
  # Split the data into each timepoint
  df$condition<-as.factor(df$condition)
  data_split <- split(df, df$timepoint)

  svm <- function(df){
    # Split the data into training and test set
    set.seed(seednum)
    intrain <- createDataPartition(y = df$condition, p=1/9, list = FALSE) # 1/18
    training <- df[intrain,]
    testing <- df[-intrain,]
    trctrl <- trainControl(method = "LOOCV", number=10) # Control the computational nuances of the train() method
    # Fit the model on the training data
    set.seed(seednum)
    svm_Linear <- train(condition ~., data = training, method = "svmLinear", 
                        trControl=trctrl,
                        preProcess = c("center", "scale"),
                        tuneLength = 10)
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
  
  write.csv(acc,file=paste0("svm_3cond_avg_acc_",seednum,subid,sep=""), row.names = TRUE)
}


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

id_seed("svm_3cond_avg_play_001_n1.csv")
id_seed("svm_3cond_avg_play_002_n1.csv")
id_seed("svm_3cond_avg_play_003_n1.csv")
id_seed("svm_3cond_avg_play_004_n1.csv")
id_seed("svm_3cond_avg_play_005_n1.csv")
id_seed("svm_3cond_avg_play_006_n1.csv")
id_seed("svm_3cond_avg_play_007_n1.csv")
id_seed("svm_3cond_avg_play_008_n1.csv")
id_seed("svm_3cond_avg_play_009_n1.csv")
id_seed("svm_3cond_avg_play_010_n1.csv")
id_seed("svm_3cond_avg_play_011_n1.csv")
id_seed("svm_3cond_avg_play_012_n1.csv")
id_seed("svm_3cond_avg_play_013_n1.csv")
id_seed("svm_3cond_avg_play_015_n1.csv")
id_seed("svm_3cond_avg_play_016_n1.csv")
id_seed("svm_3cond_avg_play_017_n1.csv")
id_seed("svm_3cond_avg_play_018_n1.csv")
id_seed("svm_3cond_avg_play_019_n1.csv")
id_seed("svm_3cond_avg_play_020_n1.csv")
id_seed("svm_3cond_avg_play_022_n1.csv")
id_seed("svm_3cond_avg_play_023_n1.csv")
id_seed("svm_3cond_avg_play_024_n1.csv")
id_seed("svm_3cond_avg_play_025_n1.csv")
id_seed("svm_3cond_avg_play_026_n1.csv")
id_seed("svm_3cond_avg_play_027_n1.csv")