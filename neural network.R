install.packages("neuralnet") 
library(neuralnet) 


mydata = read.csv("C:/R_data/winedata2.csv", sep = ",")

#normalizing to 0 and 1  
mydata$WineClass<-ifelse(mydata$WineClass=="2", 1,0)
mydata

#training orgat input
trainin = rbind(c(1,1), c(1,-1), c(-1,1), c(-1, -1));

#trainout orgate outut
trainout = rbind(1,1,1,0);

#combined OR gate data
ORdat =cbind(trainout, trainin)

#fit neural network with no hidden layers 
set.seed(2)
NN = neuralnet(ORdat[,1]~., ORdat[,-1], hidden = 0, threshold = 0.001, stepmax = 1e+05, linear.output = FALSE)
#visualize the NN
plot(NN)

#checking for biases and random weights 
NN$weights

#testing the neuralnet (1,1)
testin = rbind(c(1,1))
predict_testNN = compute(NN, testin)

#activation of the neuralnet
predict_testNN$net.result

#3
#using two variables
mydataClass = mydata[1:1]
mydataValues = mydata[2:3]

#setup training
mydataClassTrain =mydata[1:65,]
mydataValuesTrain =mydata[1:65,]

#setup test set
mydataClassTest =mydata[65:130,]
mydataValuesTest =mydata[65:130,]

#training the neuralnet
NN1 = neuralnet(WineClass~ Ash + Alcohol,mydataValuesTrain, hidden = c(3,3), threshold = 0.001, stepmax = 1e+05, linear.output = FALSE)

plot(NN1) 

#predicting the test.
predict_testNN1 = compute(NN1, mydataValuesTest[,-1]) 
predict_testNN1$neurons 
predict_testNN1$net.result 
predict_out = as.numeric(predict_testNN1$net.result>0.5) 
predict_out 


#accurancy
num = length(mydataValuesTest[,1]) #the number of test cases 
ncorrect = sum(predict_out==mydataValuesTest[,1]) #the number of correctly predicted
accuracy=ncorrect/num 
print(accuracy)