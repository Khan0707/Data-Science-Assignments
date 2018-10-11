library(kernlab)      ## For SVM model building
library(readr)        ## Importing the files in R
library(caret)        ## For Preprocess and algorthim
library(dplyr)        ## data manipulation
library(caTools)      ## mainly for sample.split function
library(ggplot2)      ## graphs
library(gridExtra)    ## arrange multiple grid-based plots on a page
library(doParallel)   ## For Parallel Process

#setwd("F:/BIG DATA/IIITb/C4.PA II/SVM/assignment/SVM Dataset")

### Importing Training Data

TrainData<- read_csv('mnist_train.csv',col_names = FALSE,na="NA")

#Understanding Dimensions

dim(TrainData)     ###60000 observations & 785 columns

#Structure of the dataset

str(TrainData)


#### Importing Test_data

TestData<- read_csv('mnist_test.csv',col_names = FALSE,na="NA")

#Understanding Dimensions

dim(TestData)  #### 10000 rows and 785 coloums

#Structure of data set

str(TestData)


# Changing the column name of the first column for both train and test

colnames(TrainData)[1] <- "Number"
colnames(TestData)[1] <- "Number"

# Adding  a column signifying train/test data in each data frame so to use it as filter 

TrainData$Type <- "train"
TestData$Type<- "test"

# combining both test and train together for data undestanding 

CombinedDataSet <- rbind(TrainData,TestData)   #### 70000 rows and 786 coloums

#checking missing value

colSums(is.na(CombinedDataSet))   # No na's our data

### Exploring Comined Data set:

summary(factor(CombinedDataSet$Number)) ### Total 10 classes (0-9)



##Check for same values

SameValue<- which(sapply(CombinedDataSet,function(x) length(unique(x)) == 1))
length(SameValue) # 65 columns 

# Here in this case, each column represents the pixel values..
# Hence can't remove  these columns




# Lets get the data back in train and test sets now.
TrainData2 <- filter(CombinedDataSet,Type=="train")
TrainData2 <- TrainData2[-786]

TestData2<- filter(CombinedDataSet,Type=="test")
TestData2 <- TestData2[-786]

# Converting the number column to factors for both data set
TrainData2$Number<- as.factor(TrainData2$Number)
TestData2$Number <- as.factor(TestData2$Number)

# Splitting  train  set to 15% of total i.e 9000 rows

set.seed(100)
train.indices = sample(1:nrow(TrainData2), 0.15*nrow(TrainData2))

test.indices = sample(1:nrow(TestData2), nrow(TestData2))

train = TrainData2[train.indices, ]
test = TestData2[test.indices, ]

##########################Constructing Model#################################

#Using Linear Kernel

Model_linear <- ksvm(Number~ ., data = train, scaled = FALSE, kernel = "vanilladot")

# Lets check the training accuracy 

eval_train_linear_model_1 <- predict(Model_linear,train)
confusionMatrix(eval_train_linear_model_1,train$Number) ###### Accuracy : 1.0

# Lets check test accuracy using #confusion matrix - Linear Kernel

Eval_linear<- predict(Model_linear, test)
confusionMatrix(Eval_linear,test$Number)   ###Accuracy : 0.9182


### Difference Between  Train and test accuracy  = 0.0818

# ------ NON-LINEAR (RBF - Kernel) SVM and performance check using Default Parameters ------ #
# Lets build a RBF kernel model with default parameters and see if performance increases #

#Using RBF Kernel

Model_RBF <- ksvm(Number~ ., data = train, scaled = FALSE, kernel = "rbfdot")


# Lets check the training accuracy

eval_train_RBF <- predict(Model_RBF,train)
confusionMatrix(eval_train_RBF,train$Number) ###### Accuracy : 0.9826 




#Lets check testing accuracy by confusion matrix - RBF Kernel

Eval_RBF<- predict(Model_RBF, test)
confusionMatrix(Eval_RBF,test$Number)  ####### Accuracy : 0.9555

####### Difference Between  Train and test accuracy  = 0.0271


# It seems the RBF model is performing better (even with default kernel parameters) 
# This is because, there is larger difference between train accuracy and test accuracy with linear model.
# With non-linear model (default parameters), the difference is small, and hence
# we can tune non-linear model through cross validation for better results.

############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 3 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=3)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid <- expand.grid(.sigma = c(0.63e-7,1.63e-7,2.63e-7),.C=c(1,2,3))




#register cluster for parallel processing to reduce the model running time

cl = makePSOCKcluster(6)
registerDoParallel(cl)


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(Number~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm) 


#Resampling results across tuning parameters:
  
#  sigma     C  Accuracy   Kappa    
#6.30e-08  1  0.9329985  0.9255409
#6.30e-08  2  0.9407766  0.9341837
#6.30e-08  3  0.9433320  0.9370238
#1.63e-07  1  0.9531100  0.9478910
#1.63e-07  2  0.9591100  0.9545591
#1.63e-07  3  0.9621102  0.9578929
#2.63e-07  1  0.9604433  0.9560413
#2.63e-07  2  0.9658880  0.9620915
#2.63e-07  3  0.9665549  0.9628327



#Accuracy was used to select the optimal model using  the largest value.
#The final values used for the model are sigma = 2.63e-07 and C = 3.

plot(fit.svm)

# Final Accuracy= 0.9665549


# Lets build a model with C = 3 and sigma = 2.63e-07

RBF_Final<- ksvm(Number~.,data=train,kernel="rbfdot",scaled=FALSE,C=3,kpar=list(sigma=2.63e-7))

RBF_Final



# Lets check training accuracy #

eval_RBF_Final_train<- predict(RBF_Final,train)
confusionMatrix(eval_RBF_Final_train,train$Number)  # Net Train Accuracy = 0.9992 

# Lets check the test accuracy 

eval_RBF_Final_test <- predict(RBF_Final,test)
confusionMatrix(eval_RBF_Final_test,test$Number)    # Net test Accuracy =  0.9669


# Difference between train and test accuracy is very small, hence we can say that over-fitting
# is not the case, our model is able to predict correct digits using Non-Linear SVM to a large extent.

# Final Conclusion #

# Based on this input, after performing cross validation for rbfdot kernel,found that
# maximum accuracy can be achieved with C=3 and sigma=2.63e-7.


# Final Hyper Parameters :
# C=3
# sigma = 2.63e-7










