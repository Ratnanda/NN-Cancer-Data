set.seed(100)

library(neuralnet)
library(caTools) #splitting
library(MLmetrics) 
library(caret) #confusion matrix

setwd("D:/Ratnanda")
data = read.csv("Data Cancer.csv")

#Change Data Type
Clump.Thickness = as.numeric(data$Clump.Thickness)
Uniformity.of.Cell.Size = as.numeric(data$Uniformity.of.Cell.Size)
Uniformity.of.Cell.Shape = as.numeric(data$Uniformity.of.Cell.Shape)
Marginal.Adhesion = as.numeric(data$Marginal.Adhesion)
Single.Epithelial.Cell.Size = as.numeric(data$Single.Epithelial.Cell.Size)
Bland.Chromatin = as.numeric(data$Bland.Chromatin)
Normal.Nucleoli = as.numeric(data$Marginal.Adhesion)
Mitoses = as.numeric(data$Mitoses)
Class = as.factor(data$Class)

data1 = data.frame(Clump.Thickness, Uniformity.of.Cell.Size,
                   Uniformity.of.Cell.Shape, Marginal.Adhesion, 
                   Single.Epithelial.Cell.Size, Bland.Chromatin,
                   Normal.Nucleoli, Mitoses, Class)
str(data1)
View(data1)

split = sample.split(data1,SplitRatio = 0.8)
train = subset(data1, split == TRUE)
test = subset(data1, split == FALSE)

#Building Model
nn_model = neuralnet(Class ~ . , data = train, 
                     hidden = c(5,5), act.fct = "logistic",
                     linear.output = FALSE)
plot(nn_model)

#Predicting
net.predict = predict(nn_model, test)
net.prediction = c("2", "4")[apply(net.predict, 1, which.max)]
predict.table = table(test$Class, net.prediction)
predict.table

#Evaluation
confusionMatrix(predict.table)
F1_Score(test$Class,net.prediction)

