#1. Use the below given data set
#DataSet
cs2m <- read.csv("D:\\BIG DATA\\DATA ANALYTICS WITH R, EXCEL & TABLEAU\\17 ENSEMBLE MODELS\\cs2m.csv")
View(cs2m)
#2. Perform the below given activities:
#a. Create classification model using different classifiers
#b. Verify model goodness of fit
#c. Apply all the model validation techniques.

#Answers for a),b),c),  using above dataset (same as assignment 18) 


names(cs2m)
nrow(cs2m)
ncol(cs2m)
str(cs2m)

#classification 
library(caTools)
library(tree)
#splitting
set.seed(1)
split<- sample.split(cs2m$classe,SplitRatio = 0.70)
cs2mTrain <- subset(cs2m,split == TRUE)
cs2mTest<- subset(cs2m, split == FALSE)

table(cs2m$classe)

table(cs2mTrain$classe)

table(cs2mTest$classe)

prop.table(table(cs2mTest$classe))

table(cs2mTest$classe)

prop.table(table(cs2mTrain$classe))

modelClassTree<- tree(classe~cvtd_timestamp+total_accel_belt+yaw_dumbbell+roll_forearm+accel_forearm_y,data = cs2mTrain)
plot(modelClassTree)

text(modelClassTree,pretty = 0 ,cex=0.75)

pred<- predict(modelClassTree,newdata= cs2mTest)
head(pred,3)
cs2m$predict <- predict
cs2m$predictROUND<- round(predict,digits = 0)

#confusion matrix
table(cs2m$classe,predict>= 0.5)

sum<- sum(table(cs2m$classe,predict>= 0.5))

#interpretation, Accuracy and model goodness  of our model
#accuracy of our model
accuracy<- (1185+679)/(2266)
accuracy
#0.8225949

#model goodness
library(verification)
predictTrain<- predict(model,cs2m,type="response")
table(cs2m$classe,predictTrain >=0.5)
head(predictTrain,3)
auc(cs2m$classe,predictTrain)

#conclusions
#****NOTE****
#Area under the curve: 0.9333333
#also our accuracy of our model is 0.8225949
#also by seeing various measures like ME,RSS,RMSE,MAPE of our tree which is godd
#by this all things we conclude that our model is good and fit
