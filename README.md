# Predict activity quality based on the activity monitors

## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively.
These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks.
One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it.
In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.
They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

The goal of this project is to predict the manner in which they did the exercise.
This is the "classe" variable in the training set.

## Sample data

The result variable is "classe" and it is a factor variable with 5 levels.
Six young health Participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in 5 different fashions.

- Exactly according to the specification (Class A)
- Throwing the elbows to the front (Class B)
- Lifting the dumbbell only halfway (Class C)
- Lowering the dumbbell only halfway (Class D)
- Throwing the hips to the front (Class E)

## Prepare the data

Download the data files and load some required packages to initialize variables.


```r
# Data file variables
testCaseFileUrl <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
trainingDataFileUrl <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
testCaseFile <- 'C:\Users\spark155156\Desktop\PML\data\pmlTestCases.csv'
trainingDataFile <- 'C:\Users\spark155156\Desktop\PML\data\pmlTrainingData.csv'

# Initialize R packages
isCaretInstalled <- require("caret")
```

```r
if(!isCaretInstalled){
    install.packages("caret")
    library("caret")
}

isRandomForestInstalled <- require("randomForest")
```

```r
if(!isRandomForestInstalled){
    install.packages("randomForest")
    library("randomForest")
}

isRpartInstalled <- require("rpart")
```

```r
if(!isRpartInstalled){
    install.packages("rpart")
    library("rpart")
}

isRpartPlotInstalled <- require("rpart.plot")
```

```r
if(!isRpartPlotInstalled){
    install.packages("rpart.plot")
    library("rpart.plot")
}

set.seed(9999)
```

## Process the data
Download the data files and purify the data sets to make fit.
Derive training and testing sets from the training data file `pml-training.csv`.
Predict and answer the 20 questions based on the trained model with the test case file `pml-test.csv`.


```r
# Download data files
download.file(testCaseFileUrl, testCaseFile)
download.file(trainingDataFileUrl, trainingDataFile)

# Clean data files
trainingData <- read.csv(trainingDataFile, na.strings=c("NA", "#DIV/0!", ""))
testCaseData <- read.csv(testCaseFile , na.strings=c("NA", "#DIV/0!", ""))
trainingData <- trainingData[,colSums(is.na(trainingData)) == 0]
testCaseData <- testCaseData[,colSums(is.na(testCaseData)) == 0]

# Subset data
trainingData <- trainingData[,-c(1:7)]
testCaseData <- testCaseData[,-c(1:7)]
```

## Double-check
Split the training data in training (75%) and testing (25%) data to double-check the test result.


```r
subSamples <- createDataPartition(y=trainingData$classe, p=0.75, list=FALSE)
subTraining <- trainingData[subSamples, ] 
subTesting <- trainingData[-subSamples, ]
```

## Exploratory data analysis
The result variable "classe" contains 5 levels.
In the plot of the outcome variable in exploranalysis.png, it shows the frequency of each levels in the subTraining data.
Based on the plot, level A is the most frequent classe and D is the least frequent one.

```r
plot(subTraining$classe, col="orange", main="Levels of the variable classe", xlab="classe levels", ylab="Frequency")
```

## Select prediction model
Select and apply a decision tree and random forest.

### Decision tree

```r
# Fit model
modFitDT <- rpart(classe ~ ., data=subTraining, method="class")

# Perform prediction
predictDT <- predict(modFitDT, subTesting, type = "class")

# Plot result
rpart.plot(modFitDT, main="Classification Tree", extra=102, under=TRUE, faclen=0)
```

Below confusion matrix shows the errors of the prediction algorithm.


```r
confusionMatrix(predictDT, subTesting$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1266  208   25   91   29
##          B   33  535   71   30   67
##          C   28   90  676  130   94
##          D   45   72   59  501   43
##          E   23   44   24   52  668
## 
## Overall Statistics
##                                          
##                Accuracy : 0.7435         
##                  95% CI : (0.731, 0.7557)
##     No Information Rate : 0.2845         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.6738         
##  Mcnemar's Test P-Value : < 2.2e-16      
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9075   0.5638   0.7906   0.6231   0.7414
## Specificity            0.8994   0.9492   0.9155   0.9466   0.9643
## Pos Pred Value         0.7820   0.7269   0.6640   0.6958   0.8237
## Neg Pred Value         0.9607   0.9007   0.9539   0.9276   0.9431
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2582   0.1091   0.1378   0.1022   0.1362
## Detection Prevalence   0.3301   0.1501   0.2076   0.1468   0.1654
## Balanced Accuracy      0.9035   0.7565   0.8531   0.7849   0.8528
```

### Random forest

```r
# Fit model
modFitRandomForest <- randomForest(classe ~ ., data=subTraining, method="class")

# Perform prediction
predictRandonForest <- predict(modFitRandomForest, subTesting, type = "class")
```

Below confusion matrix shows the errors of the prediction algorithm.


```r
confusionMatrix(predictRF, subTesting$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1394    2    0    0    0
##          B    1  946    8    0    0
##          C    0    1  846    6    0
##          D    0    0    1  796    1
##          E    0    0    0    2  900
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9955          
##                  95% CI : (0.9932, 0.9972)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9943          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9993   0.9968   0.9895   0.9900   0.9989
## Specificity            0.9994   0.9977   0.9983   0.9995   0.9995
## Pos Pred Value         0.9986   0.9906   0.9918   0.9975   0.9978
## Neg Pred Value         0.9997   0.9992   0.9978   0.9981   0.9998
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2843   0.1929   0.1725   0.1623   0.1835
## Detection Prevalence   0.2847   0.1947   0.1739   0.1627   0.1839
## Balanced Accuracy      0.9994   0.9973   0.9939   0.9948   0.9992
```

## Conclusion

### Result of the prediction

The Random Forest algorithm shows better performance than decision trees based on the result from confusion matrices.
The accuracy for the Random Forest model was about 0.995 (95% CI: (0.993, 0.997)) compared to 0.739 (95% CI: (0.727, 0.752)) for Decision Tree model.
So, the Random Forest model is choosen.

## Submission result files
Using the Random Forest algorithm on the test case data, generate files for submission.


```r
# Perform prediction
predictSubmission <- predict(modFitRandomForestF, testCaseData, type="class")
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

```r
# Write files for submission
writePmlFiles = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("C:\Users\spark155156\Desktop\PML\test_id",i,".txt")
    write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, col.names=FALSE)
  }
}

writePmlFiles(predictSubmission)
```
