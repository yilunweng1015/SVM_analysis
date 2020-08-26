####### Actual prediction accuracy #######
df = t1 # Load data. t1 = the first time point, t2 = the second time point...etc
set.seed(100)
intrain <- createDataPartition(y = sample(df$click_code), p= 0.7, list = FALSE) 
training <- df[intrain,]
testing <- df[-intrain,]
trctrl <- trainControl(method = "LOOCV")
svm_Linear <- train(click_code ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess=c("center","scale"))
pred <- predict(svm_Linear,testing)
mat <- confusionMatrix(pred, testing$click_code)
svm_acc <- mat$overall[1]



####### Permutation test of prediction accuracy #######
library(permute)
df = t1 # Load data. t1 = the first time point, t2 = the second time point...etc
perm_num <- 50 # The Number of permutations
res <- numeric(length = 50) # Initialize a matrix to store the permutation data
N <- nrow(df) # The number of rows of data
set.seed(42) # Make sure our work is reproducible
y <- matrix(0, nrow=N, ncol=50)

# Start training the SVM model
for (i in seq_len(length(res))){ 
  y[,i] <- df$click_code[shuffle(N)] # Shuffle the click responses
}

  perm <- shuffle(N)
  set.seed(40) # Make sure our work is reproducible
  y[,i] <- df$click_code[shuffle(N)] # Shuffle the click responses
  #intrain[,i] <- createDataPartition(y = df$click_code[shuffle(N)], p=0.7, list = FALSE) # Shuffle the click responses
  training <- df[intrain,]
  testing <- df[-intrain,]
  trctrl <- trainControl(method = "LOOCV") # Leave-one-out method
  svm_Linear <- train(click_code ~., data = training, method = "svmLinear",
                      trControl=trctrl,
                      preProcess=c("center","scale"))
  pred <- predict(svm_Linear,testing)
  mat <- confusionMatrix(pred, testing$click_code)
  res[i] <- mat$overall # Construct a null distribution of prediction accuracy. If the number of permutations is 50, we will get 50 prediction accuracy results.
  perm_acc <- res # Average all the prediction accuracy


# Do Wilcox.test to compare the actual accuracy with the null distribution.
# Compare one number to the null distribution.
ttest_res <- wilcox.test(perm_acc, mu = svm_acc)
# Get the p-value.
pvalue <- ttest_res$p.value

