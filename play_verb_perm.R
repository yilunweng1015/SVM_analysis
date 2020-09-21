library(dplyr)
library(tidyr)
library(reshape)
library(reshape2)
library(ISLR)
library(caret)

#Load and create Subject id
sub_id = list.files(pattern="*verb.csv")
sub_list <- vector("list", "length" = length(sub_id))

#Split all the time points for each subject
for (i in seq_along(sub_id)) {
  filename = sub_id[[i]]
  df <- read.csv(filename, header = TRUE)
  df <- subset(df, condition=="mod"|condition=="Inst",select=c(1:31,34))
  df$click_code = factor(df$click_code, levels = c(0, 1))
  data_split <- split(df, df$timepoint)
  new_names <- c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12","t13","t14","t15","t16","t17","t18","t19","t20",
                 "t21","t22","t23","t24","t25","t27","t28","t29","t30","t31","t32","t33","t34","t35","t36","t37","t38",
                 "t39","t40","t41","t42","t43","t44","t45","t46","t47","t48","t49","t50","t51","t52","t53","t54","t55",
                 "t56","t57","t58","t59","t60","t61","t62","t63","t64","t65","t66","t67","t68","t69","t70","t71","t72",
                 "t73","t74","t75","t76","t77","t78","t79","t80","t81","t82","t83","t84","t85","t86","t87","t88","t89",
                 "t90","t91","t92","t93","t94","t95","t96","t97","t98","t99","t100","t101","t102","t103","t104","t105","t106",
                 "t107","t108","t109","t110","t111","t112","t113","t114","t115","t116","t117","t118","t119","t120","t121",
                 "t122","t123","t124","t125","t126","t127","t128","t129","t130","t131","t132","t133","t134","t135","t136",
                 "t137","t138","t139","t140","t141","t142","t143","t144","t145","t146")
  for (j in 1:length(data_split)) {
    assign(new_names[j], data_split[[j]])
  }

  
svmperm <- function(df, permnum){
  res <- numeric(length=permnum) 
  N <- length(df$click_code)
  y <- matrix(0, nrow=N, ncol=permnum)
  for (i in 1:permnum) {
    y[,i] <- sample(df$click_code, size= 35, replace = FALSE) 
    intrain <- createDataPartition(y = y[,i], p=0.7, list = FALSE) 
    training <- df[intrain,]
    testing <- df[-intrain,]
    trctrl <- trainControl(method = "LOOCV") 
    svm_Linear <- train(click_code ~., data = training, method = "svmLinear",
                        trControl=trctrl)
    pred <- predict(svm_Linear,testing)
    mat <- confusionMatrix(pred, testing$click_code)
    res[i] <- mat$overall
  }
  res
}

# Perform the permutation test on each time points (145 time points in total)
  permacc<- matrix(0, nrow=145, ncol=2)
  for (i in 1:length(data_split)) {
    permacc[i,]<- svmperm(data_split[[i]],2)
  }
  write.csv(permacc,paste0("permres_",filename,".csv"), row.names = TRUE)
}
