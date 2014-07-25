## Course Project for Practical Machine Learning

### Introduction

#### Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

### Data Processing

#### Download data file and load data into R 


```r
if (!file.exists("c:/coursera/DS_PML/pml-training.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
        destfile = "c:/coursera/DS_PML/pml-training.csv")
}
if (!file.exists("c:/coursera/DS_PML/pml-testing.csv")) {
    download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
        destfile = "c:/coursera/DS_PML/pml-testing.csv")
}

trainraw <- read.table("c:/coursera/DS_PML/pml-training.csv", sep = ",", header=T, stringsAsFactors=F)
testraw<- read.table("c:/coursera/DS_PML/pml-testing.csv", sep = ",", header=T, stringsAsFactors=F)
```
#### Inspect and tidy up data sets 
##### By compare the the whole dataset and complete cases.There are many NA values in the data.  

```r
dim(trainraw)
```

```
## [1] 19622   160
```

```r
sum(complete.cases(trainraw))
```

```
## [1] 406
```
##### First, we seperate the classe column from other indepent variables

```r
target<-trainraw[,which(names(trainraw) %in% c("classe"))]
indepent<-trainraw[,-which(names(trainraw) %in% c("classe"))]
```
##### Clean up the indepent variable, remove the NA and character column from indepent data set

```r
indepent<-indepent[,colSums(is.na(indepent))==0]
charset<-sapply(names(indepent),function(x) inherits(indepent[,x],c("character")))
indepent<-indepent[,-which(names(indepent) %in% names(which(charset==TRUE)))]
```
##### According to data source http://groupware.les.inf.puc-rio.br/har, remove irrelevant columns

```r
indepent<-indepent[5:56]
```
##### Combine target data with indepent data set to get the final trainset

```r
trainset<-cbind(indepent,target)
```
##### Get the same columns from raw testset and tidy up the test set

```r
testset<-testraw[,which(names(testraw) %in% names(trainset))]
```

### Build the predictive model
##### Load library

```r
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(randomForest)
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```
##### partition the training data for cross-validation

```r
intrain<-createDataPartition(trainset$target,p=0.7,list=FALSE)
training<-trainset[intrain,]
testing<-trainset[-intrain,]
```
##### Build the model with Random Forest method

```r
set.seed(3638)
model<-train(target~.,data=training,method="rf",trControl=trainControl(method="cv",number=4))
```
### Out of sample error estimation with cross validation
##### Validate the predicted results with cofusionMatrix function

```r
predval <- predict(model, newdata=testing)
confu<-confusionMatrix(testing$target,predval)
confu
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    0    0    0    0
##          B    8 1130    1    0    0
##          C    0    9 1016    1    0
##          D    0    0   21  941    2
##          E    0    0    0    4 1078
## 
## Overall Statistics
##                                        
##                Accuracy : 0.992        
##                  95% CI : (0.99, 0.994)
##     No Information Rate : 0.286        
##     P-Value [Acc > NIR] : <2e-16       
##                                        
##                   Kappa : 0.99         
##  Mcnemar's Test P-Value : NA           
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.995    0.992    0.979    0.995    0.998
## Specificity             1.000    0.998    0.998    0.995    0.999
## Pos Pred Value          1.000    0.992    0.990    0.976    0.996
## Neg Pred Value          0.998    0.998    0.995    0.999    1.000
## Prevalence              0.286    0.194    0.176    0.161    0.184
## Detection Rate          0.284    0.192    0.173    0.160    0.183
## Detection Prevalence    0.284    0.194    0.174    0.164    0.184
## Balanced Accuracy       0.998    0.995    0.988    0.995    0.999
```


```r
accu<-confu$overall["Accuracy"]
err<-1-accu
```
##### The model accuracy is 0.9922
##### The out of sample error is 0.007816

### Predict the results
##### Use the built model to predict the result for testset

```r
result<-predict(model,newdata=testset)
result
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```


