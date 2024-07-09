##################### DATA INTEGRATION #################
library(readxl)
library(OneR)
library(splitTools)
library(ggplot2)
#install.packages("lattice")
library(lattice)
#install.packages("reshape2")
library(reshape2)
#install.packages("dplyr")
library(dplyr)
#install.packages("plotrix")
library(plotrix)
#install.packages("openxlsx")
library(openxlsx)
#install.packages("ROSE")
library(ROSE)
#install.packages("smotefamily")
library(smotefamily)
#install.packages("pROC")
library(pROC)
#install.packages("FNN")
library(FNN)
#install.packages("MASS")
library(MASS)
#install.packages("praznik")
library(praznik)
#install.packages('OneR')
library(OneR)
#install.packages('smotefamily')
library(smotefamily)
#install.packages("splitTools")
library(splitTools)
#install.packages('caret')
library(caret)
#install.packages('nnet')
library(nnet)
#install.packages("pROC") 
library(pROC)
#install.packages("e1071") 
library(e1071)
############ DECISION TREES
#install.packages("rpart") 
library(rpart)
#install.packages("rpart.plot") 
library(rpart.plot)
#install.packages("randomForest") 
library(randomForest)
#install.packages("writexl")
library(writexl)
library(class)
#install.packages("xgboost")
library(xgboost)
#install.packages("neuralnet")
library(neuralnet)

set.seed(123)
#setwd("/Users/rodrigocosta/Desktop/TrabalhoAE")
setwd("C:/Users/Rodrigo/Desktop/4A2S/AE/Assignment/Maintenance")
data=read.csv("predictive_maintenance.csv")
data=data.frame(data)

###################### DATA PREPARATION #########################
#Arranging Data Types and Attribute Names
colnames(data)[2] = "ID"
colnames(data)[4] = "AirT"
colnames(data)[5] = "ProcessT"
colnames(data)[6] = "RotSpeed"
colnames(data)[7] = "Torque"
colnames(data)[8] = "ToolWear"
colnames(data)[10] = "FailType"

data$Type = as.factor(data$Type)
data$AirT = as.double(data$AirT)
data$ProcessT = as.double(data$ProcessT)
data$RotSpeed = as.integer(data$RotSpeed)
data$Torque = as.double(data$Torque)
data$ToolWear = as.integer(data$ToolWear)
data$Target = as.factor(as.numeric(data$Target))
data$FailType = as.factor(data$FailType)

all_data = data.frame(data)

# Replace "?", "", and "-" with NA
all_data[all_data == "?" | all_data == "-" | all_data == ""] = NA

#Count the number of missing values (NA's)
sum(is.na(all_data))

#Removing Irrelevant variables for the analysis
data=data[,-c(1:2)]


#  Understanding the Inconsistencies
## Initialize an empty vector to store incongruent indices
incongruence = vector("integer", 0)

## Initialize data frame to store incongruent data
incongruent_data = data.frame(index = integer(), Target = integer(), FailType = character())

for (i in 1:nrow(all_data)) {
  ### Check conditions for incongruence
  if ((all_data$Target[i] == 1 && all_data$FailType[i] == "No Failure") ||
      (all_data$Target[i] == 0 && all_data$FailType[i] != "No Failure")) {
    ### Store the index of the incongruent row
    incongruence = c(incongruence, i)
    
    ### Store the values of Target and FailType for the incongruent row
    incongruent_data = rbind(incongruent_data, data.frame(index = i, Target = all_data$Target[i], FailType = all_data$FailType[i]))
  }
}


##Getting the indexes where Target=0 and FailType = "Random Failure"
ind_t0 = incongruent_data[incongruent_data$Target == 0, 1]
ind_t1 = incongruent_data[incongruent_data$Target == 1, 1]

##Fixing the inconsistencies
data$Target[ind_t0] = 1
data = data[-ind_t1,]

#Setting Data

#Adding Temperature difference as attribute
data = cbind(data[,1:6], Temp_Dif = data$ProcessT-data$AirT, data[,7:8])
#Adding Temperature difference as attribute
data = cbind(data[,1:7], Power = data$RotSpeed*data$Torque, data[,8:9])



#Feature Selection
##Backwards Feature Selection
data_wdummies = data
###Create Dummies
####For variable Type
data_wdummies=cbind(data_wdummies[,1:(match("Target",names(data_wdummies))-1)],is_type_L=as.factor(as.numeric(data_wdummies$Type=="L")),
                    data_wdummies[,match("Target",names(data_wdummies)):ncol(data_wdummies)])
data_wdummies=cbind(data_wdummies[,1:(match("Target",names(data_wdummies))-1)],is_type_M=as.factor(as.numeric(data_wdummies$Type=="M")),
                    data_wdummies[,match("Target",names(data_wdummies)):ncol(data_wdummies)])
####For variable Failure Type
data_wdummies=cbind(data_wdummies[,1:(match("Target",names(data_wdummies))-1)],is_failType_No=as.factor(as.numeric(data_wdummies$FailType=="No Failure")),
                    data_wdummies[,match("Target",names(data_wdummies)):ncol(data_wdummies)])
data_wdummies=cbind(data_wdummies[,1:(match("Target",names(data_wdummies))-1)],is_failType_Heat=as.factor(as.numeric(data_wdummies$FailType=="Heat Dissipation Failure")),
                    data_wdummies[,match("Target",names(data_wdummies)):ncol(data_wdummies)])
data_wdummies=cbind(data_wdummies[,1:(match("Target",names(data_wdummies))-1)],is_failType_Overstrain=as.factor(as.numeric(data_wdummies$FailType=="Overstrain Failure")),
                    data_wdummies[,match("Target",names(data_wdummies)):ncol(data_wdummies)])
data_wdummies=cbind(data_wdummies[,1:(match("Target",names(data_wdummies))-1)],is_failType_Power=as.factor(as.numeric(data_wdummies$FailType=="Power Failure")),
                    data_wdummies[,match("Target",names(data_wdummies)):ncol(data_wdummies)])
data_wdummies=cbind(data_wdummies[,1:(match("Target",names(data_wdummies))-1)],is_failType_ToolWear=as.factor(as.numeric(data_wdummies$FailType=="Tool Wear Failure")),
                    data_wdummies[,match("Target",names(data_wdummies)):ncol(data_wdummies)])
data_wdummies=cbind(data_wdummies[,1:(match("Target",names(data_wdummies))-1)],is_failType_Random=as.factor(as.numeric(data_wdummies$FailType=="Random Failures")),
                    data_wdummies[,match("Target",names(data_wdummies)):ncol(data_wdummies)])


#Adding Type Numeric
Type_Num <- as.numeric(data_wdummies$Type=="H") + as.numeric(data_wdummies$Type=="M")/2
data_wdummies <- cbind("Type"=data[,1], Type_Num,data_wdummies[,c(2:ncol(data_wdummies))])


data_wdummies_no_norm=data_wdummies

#Normalization
numeric_cols = sapply(data_wdummies, is.numeric)
numeric_data <- data_wdummies[, numeric_cols]
means <- apply(numeric_data, 2, mean)
sds <- apply(numeric_data, 2, sd)
normalized_data <- scale(numeric_data, center = means, scale = sds)
data_wdummies[, numeric_cols] <- normalized_data

###Arranging data
data_wdummies[,8] = as.numeric(data_wdummies[,8]) -1
data_wdummies[,9] = as.numeric(data_wdummies[,9]) -1
data_wdummies[,16] = as.numeric(data_wdummies[,16]) - 1




### Perform stepwise regression - Target
model <- step(glm(data_wdummies$Target ~., data = data_wdummies[, c(2:11, 18)], family = binomial), direction = "backward", trace = TRUE)

### Print the summary of the final model
summary(model)
summary(model)$coefficients[,4] #get the p values
step_results =  data.frame(COEFFICIENTS = summary(model)$coefficients[,1], PVALUES = summary(model)$coefficients[,4]) #get the coefficients of each feature
write_xlsx(step_results, "C:\\Users\\Rodrigo\\Desktop\\4A2S\\AE\\Assignment\\Maintenance\\Results\\ResultsStepBack.xlsx")



##MRMR - Minimum Redundancy Maximum Relevance
MRMR(data_wdummies[c('Type_Num','AirT','ProcessT','RotSpeed','Torque','ToolWear','Temp_Dif','Power','is_type_L','is_type_M')],data_wdummies$Target,4)
MRMR(data_wdummies[c('Type_Num','AirT','ProcessT','RotSpeed','Torque','ToolWear','Temp_Dif','Power','is_type_L','is_type_M')],data_wdummies$is_failType_Heat,4)
MRMR(data_wdummies[c('Type_Num','AirT','ProcessT','RotSpeed','Torque','ToolWear','Temp_Dif','Power','is_type_L','is_type_M')],data_wdummies$is_failType_Overstrain,4)
MRMR(data_wdummies[c('Type_Num','AirT','ProcessT','RotSpeed','Torque','ToolWear','Temp_Dif','Power','is_type_L','is_type_M')],data_wdummies$is_failType_Power,4)
MRMR(data_wdummies[c('Type_Num','AirT','ProcessT','RotSpeed','Torque','ToolWear','Temp_Dif','Power','is_type_L','is_type_M')],data_wdummies$is_failType_ToolWear,4)
MRMR(data_wdummies[c('Type_Num','AirT','ProcessT','RotSpeed','Torque','ToolWear','Temp_Dif','Power','is_type_L','is_type_M')],data_wdummies$is_failType_Random,4)


#################### Spliting data ####################################

# W/ normalized data
set.seed(123)
partitions_norm <- partition(data_wdummies$Target, c(train=0.7, test=0.3), type="stratified")
train_data_norm <- data_wdummies[partitions_norm[[1]],]
test_data_norm <- data_wdummies[partitions_norm[[2]],]


# W/o normalized data
set.seed(123)
partitions <- partition(data_wdummies_no_norm$Target, c(train=0.7, test=0.3), type="stratified")
train_data <- data_wdummies_no_norm[partitions[[1]],]
test_data <- data_wdummies_no_norm[partitions[[2]],]


#Create Folds
num_folds <- 10
set.seed(123)
folds <- createFolds(train_data_norm$FailType, k = num_folds)



#Function to balance the data
balance <- function(data){
  
  train_fold <<- data.frame()
  train_validation <<- data.frame()
  aux <- data.frame()
  aux_validation <- data.frame()
  
  for (k in 1:num_folds) {
    
    aux_fold_train <- data[-folds[[k]],]
    
    aux_fold_nofailure <- aux_fold_train[aux_fold_train$is_failType_No==1,c('Type_Num','AirT',
                                                                            'ProcessT','RotSpeed', 'Torque', 'ToolWear', 'Temp_Dif', 
                                                                            'Power')][sample(1:nrow(aux_fold_train[aux_fold_train$is_failType_No==1,]), size = 250),]
    aux_fold_nofailure$FailType <- "No Failure"
    
    aux_fold_heat <- rbind(SMOTE(X = aux_fold_train[,c('Type_Num','AirT', 'ProcessT',
                                                       'RotSpeed','Torque', 'ToolWear', 'Temp_Dif', 'Power')], 
                                 target = aux_fold_train[,'is_failType_Heat'], K = 1 , 
                                 dup_size = 250/sum(aux_fold_train[,'is_failType_Heat']==1))[[2]],
                           SMOTE(X = aux_fold_train[,c('Type_Num','AirT', 'ProcessT','RotSpeed',
                                                       'Torque', 'ToolWear', 'Temp_Dif', 'Power')], target = aux_fold_train[,'is_failType_Heat'], 
                                 K = 1 ,dup_size = 250/sum(aux_fold_train[,'is_failType_Heat']==1))[[4]])
    aux_fold_heat$class <- NULL
    aux_fold_heat$FailType <- "Heat Dissipation Failure"
    
    aux_fold_overstrain <- rbind(SMOTE(X = aux_fold_train[,c('Type_Num','AirT', 'ProcessT',
                                                             'RotSpeed','Torque', 'ToolWear', 'Temp_Dif', 'Power')], 
                                       target = aux_fold_train[,'is_failType_Overstrain'], K = 1 , 
                                       dup_size = 250/sum(aux_fold_train[,'is_failType_Overstrain']==1))[[2]], 
                                 SMOTE(X = aux_fold_train[,c('Type_Num','AirT', 'ProcessT',
                                                             'RotSpeed','Torque', 'ToolWear', 'Temp_Dif', 'Power')],
                                       target = aux_fold_train[,'is_failType_Overstrain'], K = 1 , 
                                       dup_size = 250/sum(aux_fold_train[,'is_failType_Overstrain']==1))[[4]])
    aux_fold_overstrain$class <- NULL
    aux_fold_overstrain$FailType <- "Overstrain Failure"
    
    aux_fold_power <- rbind(SMOTE(X = aux_fold_train[,c('Type_Num','AirT','ProcessT',
                                                        'RotSpeed','Torque', 'ToolWear', 'Temp_Dif', 'Power')],
                                  target = aux_fold_train[,'is_failType_Power'], K = 1 , 
                                  dup_size = 250/sum(aux_fold_train[,'is_failType_Power']==1))[[2]], 
                            SMOTE(X = aux_fold_train[,c('Type_Num','AirT', 'ProcessT',
                                                        'RotSpeed','Torque', 'ToolWear', 'Temp_Dif', 'Power')],
                                  target = aux_fold_train[,'is_failType_Power'], K = 1 , 
                                  dup_size = 250/sum(aux_fold_train[,'is_failType_Power']==1))[[4]])
    aux_fold_power$class <- NULL
    aux_fold_power$FailType <- "Power Failure"
    
    aux_fold_toolwear <- rbind(SMOTE(X = aux_fold_train[,c('Type_Num','AirT', 'ProcessT',
                                                           'RotSpeed','Torque', 'ToolWear', 'Temp_Dif', 'Power')], 
                                     target = aux_fold_train[,'is_failType_ToolWear'], 
                                     K = 1 , dup_size = 250/sum(aux_fold_train[,'is_failType_ToolWear']==1))[[2]], 
                               SMOTE(X = aux_fold_train[,c('Type_Num','AirT', 'ProcessT',
                                                           'RotSpeed','Torque', 'ToolWear', 'Temp_Dif', 'Power')],
                                     target = aux_fold_train[,'is_failType_ToolWear'], K = 1 , 
                                     dup_size = 250/sum(aux_fold_train[,'is_failType_ToolWear']==1))[[4]])
    aux_fold_toolwear$class <- NULL
    aux_fold_toolwear$FailType <- "Tool Wear Failure"
    
    aux_fold_random <- rbind(SMOTE(X = aux_fold_train[,c('Type_Num','AirT','ProcessT',
                                                         'RotSpeed','Torque', 'ToolWear', 'Temp_Dif', 'Power')],
                                   target = aux_fold_train[,'is_failType_Random'], 
                                   K = 1 , dup_size = 250/sum(aux_fold_train[,'is_failType_Random']==1))[[2]], 
                             SMOTE(X = aux_fold_train[,c('Type_Num','AirT', 'ProcessT',
                                                         'RotSpeed','Torque', 'ToolWear', 'Temp_Dif', 'Power')],
                                   target = aux_fold_train[,'is_failType_Random'], K = 1 , 
                                   dup_size = 250/sum(aux_fold_train[,'is_failType_Random']==1))[[4]])
    aux_fold_random$class <- NULL
    aux_fold_random$FailType <- "Random Failures"
    
    rm(aux)
    aux <- rbind(aux_fold_nofailure, aux_fold_heat, aux_fold_overstrain, aux_fold_power, aux_fold_toolwear, aux_fold_random)
    aux$fold <- k
    
    train_fold <<- rbind(train_fold, aux)
    train_fold$FailType<<-as.factor(train_fold$FailType)
    
    rm(aux_validation)
    aux_validation <- data[folds[[k]],]
    aux_validation$fold <- k
    
    train_validation <<- rbind(train_validation, aux_validation)
  }
  
}


# Function to calculate the weight of the weighted f1 score
F1_weights<-function(type,dataSet){
  sum(dataSet$FailType==type)/sum(dataSet$FailType!="No Failure")
}

#Perform Undersampling
##Function
undersample_data <- function(data, target) {
  # Get the class counts
  class_counts <- table(data[[target]])
  
  # Find the minority class
  minority_class <- names(which.min(class_counts))
  
  # Get the indices of the minority class
  minority_indices <- which(data[[target]] == minority_class)
  
  # Randomly sample from the majority classes
  majority_indices <- sapply(names(class_counts)[names(class_counts) != minority_class], 
                             function(x) sample(which(data[[target]] == x), 
                                                length(minority_indices), 
                                                replace = FALSE))
  
  # Combine the indices
  indices <- c(minority_indices, unlist(majority_indices))
  
  # Return the undersampled data
  return(data[indices, ])
}


#Balanced dataframes
balance(train_data_norm)

train_fold_norm <- train_fold
train_validation_norm <- train_validation[,c(2:9,19,20)]

balance(train_data)
train_validation <- train_validation[,c(2:9,19,20)]

train_data <- train_data[,c(2:9,19)]
train_data_norm <- train_data_norm[,c(2:9,19)]

test_data <- test_data[,c(2:9,19)]
test_data_norm <- test_data_norm[,c(2:9,19)]
under_train_norm <- undersample_data(train_data_norm, "FailType")
folds_over = createFolds(train_fold_norm$FailType, k = num_folds)

###################### Scenarios ########################################

#Todas as variáveis 
features_1 <- colnames(train_data)
print(features_1)
#Retirar as 2 temperaturas (que estão correlacionadas)
features_2 <- features_1[-c(which(features_1=="AirT"), which(features_1=="ProcessT"))]
print(features_2)
#Retirar Toque e TotSpeed
features_3 <- features_1[-c(which(features_1=="Torque"), which(features_1=="RotSpeed"))]
print(features_3)
#Retirar Power
features_4 <- features_1[-c(which(features_1=="Power"))]
print(features_4)
#Retirar Temp_Dif
features_5 <- features_1[-c(which(features_1=="Temp_Dif"))]
print(features_5)
# retirar as 4 variáveis correlacionadas
features_6 <- features_1[-c(which(features_1=="AirT"), which(features_1=="ProcessT")
                            , which(features_1=="Torque"), which(features_1=="RotSpeed"))]
print(features_6)
#só usar estas que estão correlacionadas
features_7 <- c("RotSpeed","ToolWear","Temp_Dif","Power","Torque","FailType")
print(features_7)

features_8 <- c("Type_Num","RotSpeed","ToolWear","Temp_Dif","FailType")
print(features_8)

features_9 <- c("Type_Num","Torque","ToolWear","Temp_Dif","FailType")
print(features_9)

scenarios <- list(features_1, features_2, features_3, features_4, features_5, features_6, features_7,features_8,features_9 )


################################# MODELLING ###################################


############## ONE-R ##################

##OneR model to predict FailType - Random - com normalização
### Random
oner_data = data_wdummies[,c(2:9,17)]
###Doing 100 iterations
number_iterations = 100
accuracy<-vector(length=number_iterations)
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Random, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_Random ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_Random ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_Random))/nrow(oner_test_data)
}
oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy


##OneR model to predict FailType - Random - sem normalização 
### Random
oner_data = data_wdummies_no_norm[,c(2:9,17)]
###Doing 100 iterations
number_iterations = 100
accuracy<-vector(length=number_iterations)
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Random, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_Random ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_Random ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_Random))/nrow(oner_test_data)
}
oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy




##Doing SMOTE instead of oversampling - We won't use Type variable
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Random, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling - SMOTE
  oner_train_data = SMOTE(X = oner_train_data[,c(1:8)], target = oner_train_data$is_failType_Random, K = 3, dup_size = 0)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = class ~., data = oner_train_data[,], verbose = T)
  ####Predicting Values
  oner_pred_num<-predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(oner_pred_num==oner_test_data$class)/nrow(oner_test_data)
}

oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy


##OneR model to predict FailType  - Toolwear - com normalização
### ToolWear
oner_data = data_wdummies[,c(2:9,16)]
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_ToolWear, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_ToolWear ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_ToolWear ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_ToolWear))/nrow(oner_test_data)
}

oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy

##OneR model to predict FailType  - Toolwear - sem normalização
### ToolWear
oner_data = data_wdummies_no_norm[,c(2:9,16)]
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_ToolWear, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_ToolWear ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_ToolWear ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_ToolWear))/nrow(oner_test_data)
}

oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy



##Doing SMOTE instead of oversampling - We won't use Type variable
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations) {
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_ToolWear, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling - SMOTE
  oner_train_data = SMOTE(X = oner_train_data[,c(2:6)], target = oner_train_data$is_failType_ToolWear, K = 3, dup_size = 0)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = class ~., data = oner_train_data[,-9], verbose = T)
  ####Predicting Values
  oner_pred_num<-predict(model_oner_num, oner_test_data[,-9])
  accuracy[j]<-sum(oner_pred_num==oner_test_data$class)/nrow(oner_test_data)
}

oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy



##OneR model to predict FailType  - Power - com normalização
### Power
oner_data = data_wdummies[,c(2:9,15)]
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Power, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_Power ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_Power ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_Power))/nrow(oner_test_data)
}
oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy


##OneR model to predict FailType  - Power - sem normalização
### Power
oner_data = data_wdummies_no_norm[,c(2:9,15)]
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Power, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_Power ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_Power ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_Power))/nrow(oner_test_data)
}
oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy

##Doing SMOTE instead of oversampling - We won't use Type variable
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations) {
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Power, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling - SMOTE
  oner_train_data = SMOTE(X = oner_train_data[,c(2:6)], target = oner_train_data$is_failType_Power, K = 3, dup_size = 0)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = class ~., data = oner_train_data[,-9], verbose = T)
  ####Predicting Values
  oner_pred_num<-predict(model_oner_num, oner_test_data[,-9])
  accuracy[j]<-sum(oner_pred_num==oner_test_data$class)/nrow(oner_test_data)
}


oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy

##OneR model to predict FailType  - Overstrain - com normalização
### Overstrain
oner_data = data_wdummies[,c(2:9,14)]
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Overstrain, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_Overstrain ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_Overstrain ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_Overstrain))/nrow(oner_test_data)
}

oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy


##OneR model to predict FailType  - Overstrain - sem normalização
### Overstrain
oner_data = data_wdummies_no_norm[,c(2:9,14)]
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Overstrain, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_Overstrain ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_Overstrain ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_Overstrain))/nrow(oner_test_data)
}

oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy




##Doing SMOTE instead of oversampling - We won't use Type variable
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations) {
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Overstrain, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling - SMOTE
  oner_train_data = SMOTE(X = oner_train_data[,c(2:6)], target = oner_train_data$is_failType_Overstrain, K = 3, dup_size = 0)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = class ~., data = oner_train_data[,-9], verbose = T)
  ####Predicting Values
  oner_pred_num<-predict(model_oner_num, oner_test_data[,-9])
  accuracy[j]<-sum(oner_pred_num==oner_test_data$class)/nrow(oner_test_data)
}

oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy



##OneR model to predict FailType  - Heat - com normalização
oner_data = data_wdummies[,c(2:9,13)]
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Heat, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_Heat ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_Heat ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_Heat))/nrow(oner_test_data)
}
oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy

##OneR model to predict FailType  - Heat - sem normalização
oner_data = data_wdummies_no_norm[,c(2:9,13)]
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Heat, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_Heat ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_Heat ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_Heat))/nrow(oner_test_data)
}
oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy



##Doing SMOTE instead of oversampling - We won't use Type variable
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations) {
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_Heat, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling - SMOTE
  oner_train_data = SMOTE(X = oner_train_data[,c(2:6)], target = oner_train_data$is_failType_Heat, K = 3, dup_size = 0)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = class ~., data = oner_train_data[,-9], verbose = T)
  ####Predicting Values
  oner_pred_num<-predict(model_oner_num, oner_test_data[,-9])
  accuracy[j]<-sum(oner_pred_num==oner_test_data$class)/nrow(oner_test_data)
}

oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy




##OneR model to predict FailType  - No failure - com normalização
### No failure
oner_data = data_wdummies[,c(2:9,12)]
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_No, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_No ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_No ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_No))/nrow(oner_test_data)
}
oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy

##OneR model to predict FailType  - No failure - sem normalização
### No failure
oner_data = data_wdummies_no_norm[,c(2:9,12)]
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations){
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_No, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling
  oner_train_data = ovun.sample(is_failType_No ~ ., data = oner_train_data, method = "over", N = nrow(oner_train_data)*2, seed = 123)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = is_failType_No ~., data = oner_train_data, verbose = T)
  ####Predicting the values
  oner_pred_num <- predict(model_oner_num, oner_test_data[,])
  accuracy[j]<-sum(as.numeric(oner_pred_num) == as.numeric(oner_test_data$is_failType_No))/nrow(oner_test_data)
}
oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy


##Doing SMOTE instead of oversampling - We won't use Type variable
###Doing 100 iterations
accuracy<-vector(length=number_iterations)
number_iterations = 100
for (j in 1:number_iterations) {
  set.seed(j)
  ####Data splitting
  partitions <- partition(oner_data$is_failType_No, p = c(train = 0.7, test = 0.3))
  oner_train_data <- oner_data[partitions$train,]
  ####Oversampling - SMOTE
  oner_train_data = SMOTE(X = oner_train_data[,c(2:6)], target = oner_train_data$is_failType_No, K = 3, dup_size = 0)$data
  oner_test_data <- oner_data[partitions$test,]
  ####Creating the model
  model_oner_num = OneR(formula = class ~., data = oner_train_data[,-9], verbose = T)
  ####Predicting Values
  oner_pred_num<-predict(model_oner_num, oner_test_data[,-9])
  accuracy[j]<-sum(oner_pred_num==oner_test_data$class)/nrow(oner_test_data)
}

oner_mean_accuracy<-mean(accuracy)
oner_mean_accuracy


################# NAIVE BAYES ##############

results_NB <- data.frame()
folds1 <- createFolds(under_train_norm$FailType, k = num_folds)

#Undersampled data com normalização
scenario_f1<-0


for (m in 1:length(scenarios)) {
  
  # Parameter Tuning
  
  laplace_values <- seq(0, 20, by=5)
  threshold_values <- seq(0.1, 0.5, by=0.1)
  
  parameters_nb1 <- as.table(matrix(nrow = length(threshold_values), ncol = length(laplace_values), 
                                    dimnames = list(THRESHOLD=threshold_values, LAPLACE=laplace_values)))
  
  
  max_F1 <- 0
  final_laplace1 <- 0
  final_threshold1 <- 0
  
  for (i in 1:length(threshold_values)) {
    
    for (j in 1:length(laplace_values)) {
      
      weightedF1_mean <- vector()
      accuracy_mean <- vector()
      recall_mean <- vector()
      precision_mean <- vector()
      
      for (k in 1:num_folds) {
        
        set.seed(123)
        model_nb <- naiveBayes(FailType~., data=under_train_norm[-folds1[[k]],scenarios[[m]]],laplace = laplace_values[j])
        prediction <- predict(model_nb, under_train_norm[folds1[[k]],scenarios[[m]]], threshold = threshold_values[i])
        predictions_nb<-table(OBSERVED=under_train_norm[folds1[[k]],]$FailType, PREDICTED=prediction)
        
        confusion<-confusionMatrix(predictions_nb)$byClass
        confusion[is.na(confusion)] <- 0
        
        weightedF1<-F1_weights("Heat Dissipation Failure",under_train_norm[folds1[[k]],])*confusion["Class: Heat Dissipation Failure","F1"]+
          F1_weights("Overstrain Failure",under_train_norm[folds1[[k]],])*confusion["Class: Overstrain Failure","F1"]+
          F1_weights("Power Failure",under_train_norm[folds1[[k]],])*confusion["Class: Power Failure","F1"]+
          F1_weights("Random Failures",under_train_norm[folds1[[k]],])*confusion["Class: Random Failures","F1"]+
          F1_weights("Tool Wear Failure",under_train_norm[folds1[[k]],])*confusion["Class: Tool Wear Failure","F1"]
        
        accuracy<-sum(under_train_norm[folds1[[k]],]$FailType==prediction)/nrow(under_train_norm[folds1[[k]],])
        mean_recall<-mean(confusion[,"Recall"])
        mean_precision<-mean(confusion[,"Precision"])
        
        weightedF1_mean[k] <- weightedF1
        accuracy_mean[k] <- accuracy
        recall_mean[k] <- mean_recall
        precision_mean[k] <- mean_precision
        
      }
      
      parameters_nb1[i,j] <- mean(weightedF1_mean)
      
      if (mean(weightedF1_mean)>max_F1) {
        max_F1 <- mean(weightedF1_mean)
        final_laplace1 <- laplace_values[j]
        final_threshold1 <- threshold_values[i]
      }
      
    }
    
  }
  
  
  # Final model
  
  set.seed(123)
  model_nb1 <- naiveBayes(FailType~., data=under_train_norm[-folds1[[1]],scenarios[[m]]], laplace = final_laplace1)
  prediction <- predict(model_nb1, test_data[,scenarios[[m]]], threshold = final_threshold1)
  predictions_nb1<-table(OBSERVED=test_data$FailType, PREDICTED=prediction)
  
  confusion_nb1<-confusionMatrix(predictions_nb1)$byClass
  confusion_nb1[is.na(confusion_nb1)] <- 0
  
  weightedF1_nb1<-F1_weights("Heat Dissipation Failure",test_data)*confusion_nb1["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data)*confusion_nb1["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data)*confusion_nb1["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data)*confusion_nb1["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data)*confusion_nb1["Class: Tool Wear Failure","F1"]
  
  accuracy_nb1<-sum(test_data$FailType==prediction)/nrow(test_data)
  mean_recall_nb1<-mean(confusion_nb1[,"Recall"])
  mean_precision_nb1<-mean(confusion_nb1[,"Precision"])
  
  auc_nb1 <- multiclass.roc(as.numeric(test_data$FailType), as.numeric(prediction))
  
  if (weightedF1_nb1>scenario_f1) {
    scenario_f1<-weightedF1_nb1
    prediction_NB1 <- prediction
  }
  
  # Summarizing the results of all scenarios
  results_NB<-rbind(results_NB, data.frame(MODEL="Naive Bayes", DATA="Balanced_Under",SCENARIO=m, NORMALIZED = "YES",
                                                               LAPLACE = final_laplace1, THRESHOLD = final_threshold1, ACCURACY=accuracy_nb1,AUC=auc_nb1$auc[1],MEAN_RECALL=mean_recall_nb1, 
                                                               MEAN_PRECISION=mean_precision_nb1,WEIGHTED_F1= weightedF1_nb1))
}




# Unbalanced data com normalização 

scenario_f1<-0

for (m in 1:length(scenarios)) {
  
  # Parameter Tuning
  
  laplace_values <- seq(0, 20, by=5)
  threshold_values <- seq(0.1, 0.5, by=0.1)
  
  parameters_nb1 <- as.table(matrix(nrow = length(threshold_values), ncol = length(laplace_values), 
                                    dimnames = list(THRESHOLD=threshold_values, LAPLACE=laplace_values)))
  
  
  max_F1 <- 0
  final_laplace1 <- 0
  final_threshold1 <- 0
  
  for (i in 1:length(threshold_values)) {
    
    for (j in 1:length(laplace_values)) {
      
      weightedF1_mean <- vector()
      accuracy_mean <- vector()
      recall_mean <- vector()
      precision_mean <- vector()
      
      for (k in 1:num_folds) {
        
        set.seed(123)
        model_nb <- naiveBayes(FailType~., data=train_data_norm[-folds[[k]],scenarios[[m]]],laplace = laplace_values[j])
        prediction <- predict(model_nb, train_data_norm[folds[[k]],scenarios[[m]]], threshold = threshold_values[i])
        predictions_nb<-table(OBSERVED=train_data_norm[folds[[k]],]$FailType, PREDICTED=prediction)
        
        confusion<-confusionMatrix(predictions_nb)$byClass
        confusion[is.na(confusion)] <- 0
        
        weightedF1<-F1_weights("Heat Dissipation Failure",train_data_norm[folds[[k]],])*confusion["Class: Heat Dissipation Failure","F1"]+
          F1_weights("Overstrain Failure",train_data_norm[folds[[k]],])*confusion["Class: Overstrain Failure","F1"]+
          F1_weights("Power Failure",train_data_norm[folds[[k]],])*confusion["Class: Power Failure","F1"]+
          F1_weights("Random Failures",train_data_norm[folds[[k]],])*confusion["Class: Random Failures","F1"]+
          F1_weights("Tool Wear Failure",train_data_norm[folds[[k]],])*confusion["Class: Tool Wear Failure","F1"]
        
        accuracy<-sum(train_data_norm[folds[[k]],]$FailType==prediction)/nrow(train_data_norm[folds[[k]],])
        mean_recall<-mean(confusion[,"Recall"])
        mean_precision<-mean(confusion[,"Precision"])
        
        weightedF1_mean[k] <- weightedF1
        accuracy_mean[k] <- accuracy
        recall_mean[k] <- mean_recall
        precision_mean[k] <- mean_precision
        
      }
      
      parameters_nb1[i,j] <- mean(weightedF1_mean)
      
      if (mean(weightedF1_mean)>max_F1) {
        max_F1 <- mean(weightedF1_mean)
        final_laplace1 <- laplace_values[j]
        final_threshold1 <- threshold_values[i]
      }
      
    }
    
  }
  
  
  # Final model
  
  set.seed(123)
  model_nb1 <- naiveBayes(FailType~., data=train_data_norm[,scenarios[[m]]], laplace = final_laplace1)
  prediction <- predict(model_nb1, test_data_norm[,scenarios[[m]]], threshold = final_threshold1)
  predictions_nb1<-table(OBSERVED=test_data_norm$FailType, PREDICTED=prediction)
  
  confusion_nb1<-confusionMatrix(predictions_nb1)$byClass
  confusion_nb1[is.na(confusion_nb1)] <- 0
  
  weightedF1_nb1<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_nb1["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_nb1["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_nb1["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_nb1["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_nb1["Class: Tool Wear Failure","F1"]
  
  accuracy_nb1<-sum(test_data_norm$FailType==prediction)/nrow(test_data_norm)
  mean_recall_nb1<-mean(confusion_nb1[,"Recall"])
  mean_precision_nb1<-mean(confusion_nb1[,"Precision"])
  
  auc_nb1 <- multiclass.roc(as.numeric(test_data_norm$FailType), as.numeric(prediction))
  
  if (weightedF1_nb1>scenario_f1) {
    scenario_f1<-weightedF1_nb1
    prediction_NB1 <- prediction
  }
  
  # Summarizing the results of all scenarios
  results_NB<-rbind(results_NB, data.frame(MODEL="Naive Bayes", DATA="Unbalanced",SCENARIO=m, NORMALIZED = "YES",
                                                               LAPLACE = final_laplace1, THRESHOLD = final_threshold1, ACCURACY=accuracy_nb1,AUC=auc_nb1$auc[1],MEAN_RECALL=mean_recall_nb1, 
                                                               MEAN_PRECISION=mean_precision_nb1,WEIGHTED_F1= weightedF1_nb1))
}



# Balanced data com normalização 

scenario_f1<-0


for (m in 1:length(scenarios)) {
  
  # Parameter Tuning
  
  laplace_values <- seq(0, 20, by=5)
  threshold_values <- seq(0.1, 0.5, by=0.1)
  
  parameters_nb1 <- as.table(matrix(nrow = length(threshold_values), ncol = length(laplace_values), 
                                    dimnames = list(THRESHOLD=threshold_values, LAPLACE=laplace_values)))
  
  
  
  max_F1 <- 0
  final_laplace1 <- 0
  final_threshold1 <- 0
  
  for (i in 1:length(threshold_values)) {
    
    for (j in 1:length(laplace_values)) {
      
      weightedF1_mean <- vector()
      accuracy_mean <- vector()
      recall_mean <- vector()
      precision_mean <- vector()
      
      for (k in 1:num_folds) {
        
        set.seed(123)
        model_nb <- naiveBayes(FailType~., data=train_fold_norm[train_fold_norm$fold==k,scenarios[[m]]],laplace = laplace_values[j])
        prediction <- predict(model_nb, train_validation_norm[train_validation_norm$fold==k,scenarios[[m]]], threshold = threshold_values[i])
        predictions_nb<-table(OBSERVED=train_validation[train_validation$fold==k,]$FailType, PREDICTED=prediction)
        
        
        confusion<-confusionMatrix(predictions_nb)$byClass
        confusion[is.na(confusion)] <- 0
        
        weightedF1<-F1_weights("Heat Dissipation Failure",train_validation_norm[folds[[k]],])*confusion["Class: Heat Dissipation Failure","F1"]+
          F1_weights("Overstrain Failure",train_validation_norm[folds[[k]],])*confusion["Class: Overstrain Failure","F1"]+
          F1_weights("Power Failure",train_validation_norm[folds[[k]],])*confusion["Class: Power Failure","F1"]+
          F1_weights("Random Failures",train_validation_norm[folds[[k]],])*confusion["Class: Random Failures","F1"]+
          F1_weights("Tool Wear Failure",train_validation_norm[folds[[k]],])*confusion["Class: Tool Wear Failure","F1"]
        
        accuracy<-sum(train_validation_norm[folds[[k]],]$FailType==prediction)/nrow(train_validation_norm[folds[[k]],])
        mean_recall<-mean(confusion[,"Recall"])
        mean_precision<-mean(confusion[,"Precision"])
        
        weightedF1_mean[k] <- weightedF1
        accuracy_mean[k] <- accuracy
        recall_mean[k] <- mean_recall
        precision_mean[k] <- mean_precision
        
      }
      
      parameters_nb1[i,j] <- mean(weightedF1_mean)
      
      if (mean(weightedF1_mean)>max_F1) {
        max_F1 <- mean(weightedF1_mean)
        final_laplace1 <- laplace_values[j]
        final_threshold1 <- threshold_values[i]
      }
      
    }
    
  }
  
  
  # Final model
  
  set.seed(123)
  model_nb1 <- naiveBayes(FailType~., data=train_fold_norm[-folds[[1]],scenarios[[m]]], laplace = final_laplace1)
  prediction <- predict(model_nb1, test_data_norm[,scenarios[[m]]], threshold = final_threshold1)
  predictions_nb1<-table(OBSERVED=test_data_norm$FailType, PREDICTED=prediction)
  
  confusion_nb1<-confusionMatrix(predictions_nb1)$byClass
  confusion_nb1[is.na(confusion_nb1)] <- 0
  
  weightedF1_nb1<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_nb1["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_nb1["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_nb1["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_nb1["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_nb1["Class: Tool Wear Failure","F1"]
  
  accuracy_nb1<-sum(test_data_norm$FailType==prediction)/nrow(test_data)
  mean_recall_nb1<-mean(confusion_nb1[,"Recall"])
  mean_precision_nb1<-mean(confusion_nb1[,"Precision"])
  
  auc_nb1 <- multiclass.roc(as.numeric(test_data_norm$FailType), as.numeric(prediction))
  
  if (weightedF1_nb1>scenario_f1) {
    scenario_f1<-weightedF1_nb1
    prediction_NB1 <- prediction
  }
  
  # Summarizing the results of all scenarios
  results_NB<-rbind(results_NB, data.frame(MODEL="Naive Bayes", DATA="Balanced",SCENARIO=m, NORMALIZED = "YES",
                                           LAPLACE = final_laplace1, THRESHOLD = final_threshold1, ACCURACY=accuracy_nb1,AUC=auc_nb1$auc[1],MEAN_RECALL=mean_recall_nb1, 
                                           MEAN_PRECISION=mean_precision_nb1,WEIGHTED_F1= weightedF1_nb1))
}

# Unbalanced data sem normalização 

scenario_f1<-0


for (m in 1:length(scenarios)) {
  
  # Parameter Tuning
  
  laplace_values <- seq(0, 20, by=5)
  threshold_values <- seq(0.1, 0.5, by=0.1)
  
  parameters_nb1 <- as.table(matrix(nrow = length(threshold_values), ncol = length(laplace_values), 
                                    dimnames = list(THRESHOLD=threshold_values, LAPLACE=laplace_values)))
  
  
  max_F1 <- 0
  final_laplace1 <- 0
  final_threshold1 <- 0
  
  for (i in 1:length(threshold_values)) {
    
    for (j in 1:length(laplace_values)) {
      
      weightedF1_mean <- vector()
      accuracy_mean <- vector()
      recall_mean <- vector()
      precision_mean <- vector()
      
      for (k in 1:num_folds) {
        
        set.seed(123)
        model_nb <- naiveBayes(FailType~., data=train_data[-folds[[k]],scenarios[[m]]],laplace = laplace_values[j])
        prediction <- predict(model_nb, train_data[folds[[k]],scenarios[[m]]], threshold = threshold_values[i])
        predictions_nb<-table(OBSERVED=train_data[folds[[k]],]$FailType, PREDICTED=prediction)
        
        confusion<-confusionMatrix(predictions_nb)$byClass
        confusion[is.na(confusion)] <- 0
        
        weightedF1<-F1_weights("Heat Dissipation Failure",train_data[folds[[k]],])*confusion["Class: Heat Dissipation Failure","F1"]+
          F1_weights("Overstrain Failure",train_data[folds[[k]],])*confusion["Class: Overstrain Failure","F1"]+
          F1_weights("Power Failure",train_data[folds[[k]],])*confusion["Class: Power Failure","F1"]+
          F1_weights("Random Failures",train_data[folds[[k]],])*confusion["Class: Random Failures","F1"]+
          F1_weights("Tool Wear Failure",train_data[folds[[k]],])*confusion["Class: Tool Wear Failure","F1"]
        
        accuracy<-sum(train_data[folds[[k]],]$FailType==prediction)/nrow(train_data[folds[[k]],])
        mean_recall<-mean(confusion[,"Recall"])
        mean_precision<-mean(confusion[,"Precision"])
        
        weightedF1_mean[k] <- weightedF1
        accuracy_mean[k] <- accuracy
        recall_mean[k] <- mean_recall
        precision_mean[k] <- mean_precision
        
      }
      
      parameters_nb1[i,j] <- mean(weightedF1_mean)
      
      if (mean(weightedF1_mean)>max_F1) {
        max_F1 <- mean(weightedF1_mean)
        final_laplace1 <- laplace_values[j]
        final_threshold1 <- threshold_values[i]
      }
      
    }
    
  }
  
  
  # Final model
  
  set.seed(123)
  model_nb1 <- naiveBayes(FailType~., data=train_data[,scenarios[[m]]], laplace = final_laplace1)
  prediction <- predict(model_nb1, test_data[,scenarios[[m]]], threshold = final_threshold1)
  predictions_nb1<-table(OBSERVED=test_data$FailType, PREDICTED=prediction)
  
  confusion_nb1<-confusionMatrix(predictions_nb1)$byClass
  confusion_nb1[is.na(confusion_nb1)] <- 0
  
  weightedF1_nb1<-F1_weights("Heat Dissipation Failure",test_data)*confusion_nb1["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data)*confusion_nb1["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data)*confusion_nb1["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data)*confusion_nb1["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data)*confusion_nb1["Class: Tool Wear Failure","F1"]
  
  accuracy_nb1<-sum(test_data$FailType==prediction)/nrow(test_data)
  mean_recall_nb1<-mean(confusion_nb1[,"Recall"])
  mean_precision_nb1<-mean(confusion_nb1[,"Precision"])
  
  auc_nb1 <- multiclass.roc(as.numeric(test_data$FailType), as.numeric(prediction))
  
  if (weightedF1_nb1>scenario_f1) {
    scenario_f1<-weightedF1_nb1
    prediction_NB1 <- prediction
  }
  
  # Summarizing the results of all scenarios
  results_NB<-rbind(results_NB, data.frame(MODEL="Naive Bayes", DATA="Unbalanced",SCENARIO=m, NORMALIZED = "NO",
                                           LAPLACE = final_laplace1, THRESHOLD = final_threshold1, ACCURACY=accuracy_nb1,AUC=auc_nb1$auc[1],MEAN_RECALL=mean_recall_nb1, 
                                           MEAN_PRECISION=mean_precision_nb1,WEIGHTED_F1= weightedF1_nb1))
}


# Balanced data sem normalização 

scenario_f1<-0


for (m in 1:length(scenarios)) {
  
  # Parameter Tuning
  
  laplace_values <- seq(0, 20, by=5)
  threshold_values <- seq(0.1, 0.5, by=0.1)
  
  parameters_nb1 <- as.table(matrix(nrow = length(threshold_values), ncol = length(laplace_values), 
                                    dimnames = list(THRESHOLD=threshold_values, LAPLACE=laplace_values)))
  
  
  
  max_F1 <- 0
  final_laplace1 <- 0
  final_threshold1 <- 0
  
  for (i in 1:length(threshold_values)) {
    
    for (j in 1:length(laplace_values)) {
      
      weightedF1_mean <- vector()
      accuracy_mean <- vector()
      recall_mean <- vector()
      precision_mean <- vector()
      
      for (k in 1:num_folds) {
        
        set.seed(123)
        model_nb <- naiveBayes(FailType~., data=train_fold[train_fold$fold==k,scenarios[[m]]],laplace = laplace_values[j])
        prediction <- predict(model_nb, train_validation[train_validation$fold==k,scenarios[[m]]], threshold = threshold_values[i])
        predictions_nb<-table(OBSERVED=train_validation[train_validation$fold==k,]$FailType, PREDICTED=prediction)
        
        confusion<-confusionMatrix(predictions_nb)$byClass
        confusion[is.na(confusion)] <- 0
        
        weightedF1<-F1_weights("Heat Dissipation Failure",train_validation[folds[[k]],])*confusion["Class: Heat Dissipation Failure","F1"]+
          F1_weights("Overstrain Failure",train_validation[folds[[k]],])*confusion["Class: Overstrain Failure","F1"]+
          F1_weights("Power Failure",train_validation[folds[[k]],])*confusion["Class: Power Failure","F1"]+
          F1_weights("Random Failures",train_validation[folds[[k]],])*confusion["Class: Random Failures","F1"]+
          F1_weights("Tool Wear Failure",train_validation[folds[[k]],])*confusion["Class: Tool Wear Failure","F1"]
        
        accuracy<-sum(train_validation[folds[[k]],]$FailType==prediction)/nrow(train_validation[folds[[k]],])
        mean_recall<-mean(confusion[,"Recall"])
        mean_precision<-mean(confusion[,"Precision"])
        
        weightedF1_mean[k] <- weightedF1
        accuracy_mean[k] <- accuracy
        recall_mean[k] <- mean_recall
        precision_mean[k] <- mean_precision
        
      }
      
      parameters_nb1[i,j] <- mean(weightedF1_mean)
      
      if (mean(weightedF1_mean)>max_F1) {
        max_F1 <- mean(weightedF1_mean)
        final_laplace1 <- laplace_values[j]
        final_threshold1 <- threshold_values[i]
      }
      
    }
    
  }
  
  
  # Final model
  
  set.seed(123)
  model_nb1 <- naiveBayes(FailType~., data=train_fold[-folds[[1]],scenarios[[m]]], laplace = final_laplace1)
  prediction <- predict(model_nb1, test_data[,scenarios[[m]]], threshold = final_threshold1)
  predictions_nb1<-table(OBSERVED=test_data$FailType, PREDICTED=prediction)
  
  confusion_nb1<-confusionMatrix(predictions_nb1)$byClass
  confusion_nb1[is.na(confusion_nb1)] <- 0
  
  weightedF1_nb1<-F1_weights("Heat Dissipation Failure",test_data)*confusion_nb1["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data)*confusion_nb1["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data)*confusion_nb1["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data)*confusion_nb1["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data)*confusion_nb1["Class: Tool Wear Failure","F1"]
  
  accuracy_nb1<-sum(test_data$FailType==prediction)/nrow(test_data)
  mean_recall_nb1<-mean(confusion_nb1[,"Recall"])
  mean_precision_nb1<-mean(confusion_nb1[,"Precision"])
  
  auc_nb1 <- multiclass.roc(as.numeric(test_data$FailType), as.numeric(prediction))
  
  if (weightedF1_nb1>scenario_f1) {
    scenario_f1<-weightedF1_nb1
    prediction_NB1 <- prediction
  }
  
  # Summarizing the results of all scenarios
  results_NB<-rbind(results_NB, data.frame(MODEL="Naive Bayes", DATA="Balanced",SCENARIO=m, NORMALIZED = "NO",
                                           LAPLACE = final_laplace1, THRESHOLD = final_threshold1, ACCURACY=accuracy_nb1,AUC=auc_nb1$auc[1],MEAN_RECALL=mean_recall_nb1, 
                                           MEAN_PRECISION=mean_precision_nb1,WEIGHTED_F1= weightedF1_nb1))
}

#write_xlsx(results_NB, "C:\\Users\\Rodrigo\\Desktop\\4A2S\\AE\\Assignment\\Maintenance\\Results\\ResultsNaiveBayes.xlsx")



###################################Multinomial Logistic Regression########################################################

results_LR <- data.frame()
# Unbalanced data  COM normalização 
scenario_f1<-0
for (m in 1:length(scenarios)) {
  
  set.seed(123)
  model_logistic1 <- multinom(FailType~., data=train_data_norm[,scenarios[[m]]])
  prediction <- predict(model_logistic1, test_data_norm[,scenarios[[m]]])
  predictions_logistic1<-table(OBSERVED=test_data_norm$FailType, PREDICTED=prediction)
  
  confusion_LR1<-confusionMatrix(predictions_logistic1)$byClass
  confusion_LR1[is.na(confusion_LR1)] <- 0
  
  weightedF1_LR1<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_LR1["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_LR1["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_LR1["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_LR1["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_LR1["Class: Tool Wear Failure","F1"]
  
  accuracy_LR1<-sum(test_data_norm$FailType==prediction)/nrow(test_data_norm)
  mean_recall_LR1<-mean(confusion_LR1[,"Recall"])
  mean_precision_LR1<-mean(confusion_LR1[,"Precision"])
  
  auc_LR1 <- multiclass.roc(as.numeric(test_data_norm$FailType), as.numeric(prediction))
  
  if (weightedF1_LR1>scenario_f1) {
    scenario_f1<-weightedF1_LR1
    prediction_LR1 <- prediction
  }
  
  # Summarizing the results of all scenarios
  results_LR<-rbind(results_LR, data.frame(MODEL="Multinomial Logistic Regression", DATA="Unbalanced", SCENARIO=m, 
                                                               NORMALIZED = "YES", ACCURACY=accuracy_LR1, AUC=auc_LR1$auc[1], MEAN_RECALL=mean_recall_LR1, 
                                                               MEAN_PRECISION=mean_precision_LR1,WEIGHTED_F1= weightedF1_LR1))
}



# Balanced Data COM Normalização
scenario_f1<-0

for (m in 1:length(scenarios)) {
  
  set.seed(123)
  model_logistic2 <- multinom(FailType~., data=train_fold_norm[,scenarios[[m]]])
  prediction <- predict(model_logistic2, test_data_norm[,scenarios[[m]]])
  predictions_logistic2<-table(OBSERVED=test_data_norm$FailType, PREDICTED=prediction)
  
  confusion_LR2<-confusionMatrix(predictions_logistic2)$byClass
  confusion_LR2[is.na(confusion_LR2)] <- 0
  
  weightedF1_LR2<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_LR2["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_LR2["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_LR2["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_LR2["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_LR2["Class: Tool Wear Failure","F1"]
  
  accuracy_LR2<-sum(test_data_norm$FailType==prediction)/nrow(test_data_norm)
  mean_recall_LR2<-mean(confusion_LR2[,"Recall"])
  mean_precision_LR2<-mean(confusion_LR2[,"Precision"])
  
  auc_LR2 <- multiclass.roc(as.numeric(test_data_norm$FailType), as.numeric(prediction))
  
  if (weightedF1_LR2>scenario_f1) {
    scenario_f1<-weightedF1_LR2
    prediction_LR2 <- prediction
  }
  
  # Summarizing the results of all scenarios
  results_LR<-rbind(results_LR, data.frame(MODEL="Multinomial Logistic Regression", DATA="Balanced", SCENARIO=m,
                                                               NORMALIZED = "YES", ACCURACY=accuracy_LR2, AUC=auc_LR2$auc[1], MEAN_RECALL=mean_recall_LR2, 
                                                               MEAN_PRECISION=mean_precision_LR2, WEIGHTED_F1=weightedF1_LR2))
}

#results_LR = cbind(results_LR, normalized = rep("No", length(scenarios)*2))
#results_LR_data_norm = cbind(results_LR_data_norm, normalized = rep("Yes", length(scenarios)*2))
#write_xlsx(rbind(results_LR, results_LR_data_norm), "C:\\Users\\Rodrigo\\Desktop\\4A2S\\AE\\Assignment\\Maintenance\\ResultsMLogisticReg.xlsx")


#########################K Nearest Neighbours################################
results_KNN <- data.frame()

#Unbalanced Data Normalized

scenario_f1<-0

for (m in 1:length(scenarios)) {
  
  # Parameter tuning
  max_num_neighbours<-40
  performance_summary1<-data.frame()
  
  
  # Unbalanced data
  
  for (j in 1:max_num_neighbours){
    
    F1_vector         <- vector()
    weightedF1_vector <- vector()
    accuracy_vector   <- vector()
    recall_vector     <- vector()
    precision_vector  <- vector()
    
    for (k in 1:num_folds){ 
      
      set.seed(20)
      model_knn<-knn(train_data_norm[-folds[[k]], scenarios[[m]][-length(scenarios[[m]])]],
                     train_validation_norm[train_validation_norm$fold==k, scenarios[[m]][-length(scenarios[[m]])]], 
                     as.numeric(train_data_norm[-folds[[k]],"FailType"]), k = j, prob=TRUE)
      
      confusion <- confusionMatrix(factor(model_knn,levels = 1:6),factor(as.numeric(train_data_norm[folds[[k]],]$FailType),
                                                                         levels = 1:6))
      
      confusion_ByClass <- confusion$byClass
      confusion_ByClass[is.na(confusion_ByClass)] <- 0
      
      mean_F1 <- mean(confusion_ByClass[,"F1"])
      
      weightedF1<-F1_weights("Heat Dissipation Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion_ByClass["Class: 1","F1"]+
        F1_weights("Overstrain Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion_ByClass["Class: 3","F1"]+
        F1_weights("Power Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion_ByClass["Class: 4","F1"]+
        F1_weights("Random Failures",train_validation_norm[train_validation_norm$fold==k,])*confusion_ByClass["Class: 5","F1"]+
        F1_weights("Tool Wear Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion_ByClass["Class: 6","F1"]
      
      accuracy <- confusion$overall[1]
      
      mean_recall<-mean(confusion_ByClass[,"Recall"])
      
      mean_precision<-mean(confusion_ByClass[,"Precision"])
      
      F1_vector[k]         <- mean_F1
      weightedF1_vector[k] <- weightedF1
      accuracy_vector[k]   <- accuracy
      recall_vector[k]     <- mean_recall
      precision_vector[k]  <- mean_precision
      
    }
    
    #print(ncol(performance_summary1))
    #print(ncol(results_KNN))
    
    performance_summary1<-rbind(performance_summary1, data.frame(NUM_NEIGHBORS=j, ACCURACY=mean(accuracy_vector), 
                                                                 PRECISION=mean(precision_vector), 
                                                                 RECALL=mean(recall_vector), F1= mean(F1_vector), 
                                                                 WEIGHTED_F1=mean(weightedF1)))
  }
  
  num_neighbors1 <- performance_summary1$NUM_NEIGHBORS[which.max(performance_summary1$WEIGHTED_F1)]
  
  
  # Final Model
  
  set.seed(20)
  model_knn1<-knn(train_data_norm[, scenarios[[m]][-length(scenarios[[m]])]], test_data_norm[, scenarios[[m]][-length(scenarios[[m]])]], 
                  as.numeric(train_data_norm[,"FailType"]), k = num_neighbors1, prob=TRUE)
  
  confusion_knn1 <- confusionMatrix(factor(model_knn1,levels=1:6),factor(as.numeric(test_data_norm$FailType),levels=1:6))
  
  confusion_ByClass_knn1 <- confusion_knn1$byClass
  confusion_ByClass_knn1[is.na(confusion_ByClass_knn1)] <- 0
  
  accuracy_knn1 <- confusion_knn1$overall[1]
  precision_knn1 <- mean(confusion_ByClass_knn1[,"Precision"])
  recall_knn1 <- mean(confusion_ByClass_knn1[,"Recall"])
  
  weightedF1_knn1 <- F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_ByClass_knn1["Class: 1","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_ByClass_knn1["Class: 3","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_ByClass_knn1["Class: 4","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_ByClass_knn1["Class: 5","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_ByClass_knn1["Class: 6","F1"]
  
  auc_knn1 <- multiclass.roc(as.numeric(test_data_norm$FailType), as.numeric(factor(model_knn1,levels=1:6)))
  
  if (weightedF1_knn1>scenario_f1) {
    scenario_f1<-weightedF1_knn1
    prediction_KNN1 <- factor(model_knn1,levels=1:6)
  }
  
  results_KNN<-rbind(results_KNN, data.frame(MODEL="K-NN", DATA="Unbalanced", SCENARIO=m,NORMALIZED ="YES", ACCURACY=accuracy_knn1, 
                                             K = num_neighbors1, AUC=auc_knn1$auc[1], MEAN_RECALL=recall_knn1, MEAN_PRECISION=precision_knn1,
                                             WEIGHTED_F1= weightedF1_knn1))
}


#Balanced Data Normalized


scenario_f1<-0

# Parameter tuning
for (m in 1:length(scenarios)) {
  max_num_neighbours2<-40
  performance_summary2<-data.frame()
  
  
  # Balanced data
  for (j in 1:max_num_neighbours2){
    
    F1_vector         <- vector()
    weightedF1_vector <- vector()
    accuracy_vector   <- vector()
    recall_vector     <- vector()
    precision_vector  <- vector()
    
    for (k in 1:num_folds){ 
      
      #print(nrow(train_fold_norm[train_fold_norm$fold == k, scenarios[[m]][-length(scenarios[[m]])]]))
      #print(length(as.numeric(train_fold_norm[train_fold_norm$fold == k, "FailType"])))
      
      set.seed(20)
      model_knn<-knn(train_fold_norm[train_fold_norm$fold==k, scenarios[[m]][-length(scenarios[[m]])]],
                     train_validation_norm[train_validation_norm$fold==k, scenarios[[m]][-length(scenarios[[m]])]], 
                     as.numeric(train_fold_norm[train_fold_norm$fold==k,"FailType"]), k = j, prob=TRUE)
      
      confusion <- confusionMatrix(factor(model_knn,levels = 1:6),
                                   factor(as.numeric(train_validation_norm[train_validation_norm$fold==k,"FailType"]),
                                          levels = 1:6))
      
      confusion_ByClass <- confusion$byClass
      confusion_ByClass[is.na(confusion_ByClass)] <- 0
      
      mean_F1 <- mean(confusion_ByClass[,"F1"])
      
      weightedF1<-F1_weights("Heat Dissipation Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion_ByClass["Class: 1","F1"]+
        F1_weights("Overstrain Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion_ByClass["Class: 3","F1"]+
        F1_weights("Power Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion_ByClass["Class: 4","F1"]+
        F1_weights("Random Failures",train_validation_norm[train_validation_norm$fold==k,])*confusion_ByClass["Class: 5","F1"]+
        F1_weights("Tool Wear Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion_ByClass["Class: 6","F1"]
      
      accuracy <- confusion$overall[1]
      
      mean_recall<-mean(confusion_ByClass[,"Recall"])
      
      mean_precision<-mean(confusion_ByClass[,"Precision"])
      
      F1_vector[k]         <- mean_F1
      weightedF1_vector[k] <- weightedF1
      accuracy_vector[k]   <- accuracy
      recall_vector[k]     <- mean_recall
      precision_vector[k]  <- mean_precision
      
    }
    
    print(ncol(performance_summary2))
    print(ncol(results_KNN))
    
    performance_summary2<-rbind(performance_summary2, data.frame(NUM_NEIGHBORS=j, ACCURACY=mean(accuracy_vector), 
                                                                 PRECISION=mean(precision_vector), 
                                                                 RECALL=mean(recall_vector), F1= mean(F1_vector), 
                                                                 WEIGHTED_F1=mean(weightedF1)))
  }
  
  num_neighbors2 <- performance_summary2$NUM_NEIGHBORS[which.max(performance_summary2$WEIGHTED_F1)]
  
  
  
  ##### Final Model
  
  set.seed(20)
  model_knn2<-knn(train_fold_norm[, scenarios[[m]][-length(scenarios[[m]])]], test_data_norm[, scenarios[[m]][-length(scenarios[[m]])]], 
                  as.numeric(train_fold_norm[,"FailType"]), k = num_neighbors2, prob=TRUE)
  
  confusion_knn2 <- confusionMatrix(factor(model_knn2,levels = 1:6),factor(as.numeric(test_data_norm$FailType),levels = 1:6))
  
  confusion_ByClass_knn2 <- confusion_knn2$byClass
  confusion_ByClass_knn2[is.na(confusion_ByClass_knn2)] <- 0
  
  accuracy_knn2   <- confusion_knn2$overall[1]
  precision_knn2  <- mean(confusion_ByClass_knn2[,"Precision"])
  recall_knn2     <- mean(confusion_ByClass_knn2[,"Recall"])
  weightedF1_knn2 <- F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_ByClass_knn2["Class: 1","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_ByClass_knn2["Class: 3","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_ByClass_knn2["Class: 4","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_ByClass_knn2["Class: 5","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_ByClass_knn2["Class: 6","F1"]
  
  auc_knn2 <- multiclass.roc(as.numeric(test_data_norm$FailType), as.numeric(factor(model_knn2,levels=1:6)))
  
  if (weightedF1_knn2>scenario_f1) {
    scenario_f1<-weightedF1_knn2
    prediction_KNN2 <- factor(model_knn2,levels=1:6)
  }
  
  results_KNN<-rbind(results_KNN, data.frame(MODEL="K-NN", DATA="Balanced", SCENARIO=m, NORMALIZED ="YES", ACCURACY=accuracy_knn2, 
                                             K = num_neighbors2, AUC=auc_knn2$auc[1], MEAN_RECALL=recall_knn2, 
                                             MEAN_PRECISION=precision_knn2,WEIGHTED_F1= weightedF1_knn2))
}

levels(prediction_KNN2) <- c("Heat Dissipation Failure", "No Failure", "Overstrain Failure", "Power Failure", "Random Failures", "Tool Wear Failure")

write_xlsx(results_KNN, "C:\\Users\\Rodrigo\\Desktop\\4A2S\\AE\\Assignment\\Maintenance\\ResultsKNN.xlsx")



#################### Decision Tree  ########################

results_DT <- data.frame()

#Unbalanced Data Normalized

scenario_f1<-0

for (m in 1:length(scenarios)) {
  
  #Parameter tuning
  performance_summary_dt1<-data.frame()
  criterion_dt <- c('information','gini')
  min_num_objects_dt <- 20 
  
  
  # Unbalanced data
  for (l in criterion_dt)
  {
    for (i in c(0.01,0))
    {
      for (j in 2:min_num_objects_dt)
      {
        
        predictions_summary_dt1<-data.frame()
        weightedF1_mean <- vector()
        accuracy_mean <- vector()
        recall_mean <- vector()
        precision_mean <- vector()
        
        for (k in 1:num_folds)  {
          
          set.seed(123)
          model_dt<-rpart(FailType ~., data = train_data_norm[-folds[[k]],scenarios[[m]]],   
                          parms = list(split = l), minbucket = j, cp=i)
          
          prediction <- predict(model_dt, newdata = train_data_norm[folds[[k]], scenarios[[m]]], type = "class")
          
          predictions_dt<-table(OBSERVED=train_data_norm[folds[[k]],]$FailType, PREDICTED=prediction)
          
          confusion<-confusionMatrix(predictions_dt)$byClass
          confusion[is.na(confusion)] <- 0
          
          weightedF1<-F1_weights("Heat Dissipation Failure",train_data_norm[folds[[k]],])*confusion["Class: Heat Dissipation Failure","F1"]+
            F1_weights("Overstrain Failure",train_data_norm[folds[[k]],])*confusion["Class: Overstrain Failure","F1"]+
            F1_weights("Power Failure",train_data_norm[folds[[k]],])*confusion["Class: Power Failure","F1"]+
            F1_weights("Random Failures",train_data_norm[folds[[k]],])*confusion["Class: Random Failures","F1"]+
            F1_weights("Tool Wear Failure",train_data_norm[folds[[k]],])*confusion["Class: Tool Wear Failure","F1"]
          
          accuracy<-sum(train_data_norm[folds[[k]],]$FailType==prediction)/nrow(train_data_norm[folds[[k]],])
          mean_recall<-mean(confusion[,"Recall"])
          mean_precision<-mean(confusion[,"Precision"])
          
          weightedF1_mean[k] <- weightedF1
          accuracy_mean[k] <- accuracy
          recall_mean[k] <- mean_recall
          precision_mean[k] <- mean_precision
        }
        
        performance_summary_dt1<-rbind(performance_summary_dt1, data.frame(criterion = l , cp = i, min_objects = j, 
                                                                           accuracy= mean(accuracy_mean), f1= mean(weightedF1_mean) ))
      }
    }
  }
  
  #Defining parameters
  l<-performance_summary_dt1[which(performance_summary_dt1$f1==max(performance_summary_dt1$f1))[1],"criterion"]
  i<-performance_summary_dt1[which(performance_summary_dt1$f1==max(performance_summary_dt1$f1))[1],"cp"]
  j<-performance_summary_dt1[which(performance_summary_dt1$f1==max(performance_summary_dt1$f1))[1],"min_objects"]
  
  
  #Final model
  
  set.seed(123)
  model_dt1 <- rpart(FailType ~., data=train_data_norm[,scenarios[[m]]], parms = list(split = l), minbucket = as.numeric(j), cp=i)
  prediction <- predict(model_dt1, newdata = test_data_norm , type = "class")
  
  predictions_summary_dt1<-data.frame(FailType = test_data_norm$FailType, prediction=prediction)
  
  predictions_dt1<-table(OBSERVED=test_data_norm$FailType, PREDICTED=prediction)
  
  confusion_dt1<-confusionMatrix(predictions_dt1)$byClass
  confusion_dt1[is.na(confusion_dt1)] <- 0
  
  weightedF1_dt1<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_dt1["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_dt1["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_dt1["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_dt1["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_dt1["Class: Tool Wear Failure","F1"]
  
  accuracy_dt1<-sum(test_data_norm$FailType==prediction)/nrow(test_data_norm)
  mean_recall_dt1<-mean(confusion_dt1[,"Recall"])
  mean_precision_dt1<-mean(confusion_dt1[,"Precision"])
  
  auc_dt1 <- multiclass.roc(as.numeric(test_data_norm$FailType), as.numeric(prediction))
  
  if (weightedF1_dt1>scenario_f1) {
    scenario_f1<-weightedF1_dt1
    prediction_DT1 <- prediction
    prp(model_dt1)
  }
  
  # Summarizing the results of all scenarios
  results_DT <- rbind(results_DT, data.frame(MODEL="DT", DATA="Unbalanced",SCENARIO=m,
                                             ACCURACY=accuracy_dt1, AUC=auc_dt1$auc[1], MEAN_RECALL=mean_recall_dt1, 
                                             MEAN_PRECISION=mean_precision_dt1, WEIGHTED_F1= weightedF1_dt1, CRITERION = l, CP = i, MIN_OBJ = j))
}


# Balanced data

scenario_f1<-0


for (m in 1:length(scenarios)) {
  
  performance_summary_dt2<-data.frame()
  
  for (l in criterion_dt)
  {
    for (i in c(0.01,0))
    {
      for (j in 2:min_num_objects_dt)
      {
        
        predictions_summary_dt2<-data.frame()
        weightedF1_mean <- vector()
        accuracy_mean <- vector()
        recall_mean <- vector()
        precision_mean <- vector()
        
        for (k in 1:num_folds) {
          
          set.seed(123)
          model_dt<-rpart(FailType~. , data = train_fold_norm[ train_fold_norm$fold == k,scenarios[[m]]], parms = list(split = l), 
                          minbucket = j, cp=i)
          
          prediction <- predict(model_dt, newdata = train_validation_norm[train_validation_norm$fold == k,scenarios[[m]]], type = "class")
          
          predictions_dt<-table(OBSERVED=train_validation_norm[train_validation_norm$fold==k,]$FailType, PREDICTED=prediction)
          
          confusion<-confusionMatrix(predictions_dt)$byClass
          confusion[is.na(confusion)] <- 0
          
          weightedF1<-F1_weights("Heat Dissipation Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion["Class: Heat Dissipation Failure","F1"]+
            F1_weights("Overstrain Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion["Class: Overstrain Failure","F1"]+
            F1_weights("Power Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion["Class: Power Failure","F1"]+
            F1_weights("Random Failures",train_validation_norm[train_validation_norm$fold==k,])*confusion["Class: Random Failures","F1"]+
            F1_weights("Tool Wear Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion["Class: Tool Wear Failure","F1"]
          
          accuracy<-sum(train_validation_norm[train_validation_norm$fold==k,]$FailType==prediction)/nrow(train_validation_norm[train_validation_norm$fold==k,])
          mean_recall<-mean(confusion[,"Recall"])
          mean_precision<-mean(confusion[,"Precision"])
          
          weightedF1_mean[k] <- weightedF1
          accuracy_mean[k] <- accuracy
          recall_mean[k] <- mean_recall
          precision_mean[k] <- mean_precision
        }
        
        performance_summary_dt2<-rbind(performance_summary_dt2, data.frame(criterion = l , cp = i, min_objects = j, 
                                                                           accuracy= mean(accuracy_mean), f1= mean(weightedF1_mean) ))
      }
    }
  }
  
  
  #Defining parameters
  l<-performance_summary_dt2[which(performance_summary_dt2$f1==max(performance_summary_dt2$f1))[1],"criterion"]
  i<-performance_summary_dt2[which(performance_summary_dt2$f1==max(performance_summary_dt2$f1))[1],"cp"]
  j<-performance_summary_dt2[which(performance_summary_dt2$f1==max(performance_summary_dt2$f1))[1],"min_objects"]
  
  
  #Final model
  
  set.seed(123)
  model_dt2 <- rpart(FailType ~., data=train_fold_norm[,scenarios[[m]]], parms = list(split = l), 
                     minbucket = as.numeric(j), cp=i)
  
  prediction <- predict(model_dt2, newdata = test_data_norm[,scenarios[[m]]] , type = "class")
  
  predictions_summary_dt2<-data.frame(FailType = test_data_norm$FailType, prediction=prediction)
  
  predictions_dt2<-table(OBSERVED=test_data_norm$FailType, PREDICTED=prediction)
  
  confusion_dt2<-confusionMatrix(predictions_dt2)$byClass
  confusion_dt2[is.na(confusion_dt2)] <- 0
  
  weightedF1_dt2<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_dt2["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_dt2["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_dt2["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_dt2["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_dt2["Class: Tool Wear Failure","F1"]
  
  accuracy_dt2<-sum(test_data_norm$FailType==prediction)/nrow(test_data_norm)
  mean_recall_dt2<-mean(confusion_dt2[,"Recall"])
  mean_precision_dt2<-mean(confusion_dt2[,"Precision"])
  
  auc_dt2 <- multiclass.roc(as.numeric(test_data_norm$FailType), as.numeric(prediction))
  
  if (weightedF1_dt2>scenario_f1) {
    scenario_f1<-weightedF1_dt2
    prediction_DT2 <- prediction
  }
  
  # Summarizing the results of all scenarios
  results_DT <- rbind(results_DT, data.frame(MODEL="DT", DATA="Balanced",SCENARIO=m,
                                             ACCURACY=accuracy_dt2, AUC=auc_dt2$auc[1], MEAN_RECALL=mean_recall_dt2, 
                                             MEAN_PRECISION=mean_precision_dt2,WEIGHTED_F1= weightedF1_dt2, CRITERION = l, CP = i, MIN_OBJ = j))
}


#Draw th tree of the best scenario
best_tree = rpart(FailType ~., data=train_data_norm[,scenarios[[7]]], parms = list(split = "gini"), minbucket = as.numeric(2), cp=0)
prp(best_tree)

#write_xlsx(results_DT, "C:\\Users\\Rodrigo\\Desktop\\4A2S\\AE\\Assignment\\Maintenance\\Results\\ResultsDecisionTrees.xlsx")

################################ Random Forest ################

#Creating the model for each scenario with Hyper Parameter tuning
#Hyper Parameter Tuning
scenarios_data = list(
  list(data = train_fold_norm, test_data = test_data_norm, name = "Balanced", normal = "Yes"), # Balanced data with normalization
  list(data = train_data_norm, test_data = test_data_norm, name = "Unbalanced", normal = "Yes") # Unbalanced data with normalization
)
results_RF_HP = data.frame()


##Balanced Data with Normalization
data_info = scenarios_data[[1]]
data = data_info$data
test_data = data_info$test_data
name = data_info$name
normal = data_info$normal

mtry_values = c(1,3,5,10)
ntree_values = c(250, 500, 750, 1000)

max_F1 = 0
final_ntree = 0
final_mtry = 0

for (m in 1:length(scenarios)){
  
  for (n in 1:length(ntree_values)) {
    for (j in 1:length(mtry_values)) {
      for (k in 1:num_folds){
        model_rf = randomForest(FailType ~., data = data[-folds_over[[k]], scenarios[[m]]], mtry = mtry_values[j], ntree = ntree_values[n])
        prediction = predict(model_rf, data[folds_over[[k]], scenarios[[m]]], type = "class")
        predictions_rf = table(OBSERVED = data[folds_over[[k]], scenarios[[m]]]$FailType, PREDICTED = prediction)
        confusion_rf = confusionMatrix(predictions_rf)$byClass
        confusion_rf[is.na(confusion_rf)] = 0
        
        weightedF1_rf = F1_weights("Heat Dissipation Failure", data[folds_over[[k]],]) * confusion_rf["Class: Heat Dissipation Failure", "F1"] +
          F1_weights("Overstrain Failure", data[folds_over[[k]],]) * confusion_rf["Class: Overstrain Failure", "F1"] +
          F1_weights("Power Failure", data[folds_over[[k]],]) * confusion_rf["Class: Power Failure", "F1"] +
          F1_weights("Random Failures", data[folds_over[[k]],]) * confusion_rf["Class: Random Failures", "F1"] +
          F1_weights("Tool Wear Failure", data[folds_over[[k]],]) * confusion_rf["Class: Tool Wear Failure", "F1"]
        
        if (weightedF1_rf > max_F1) {
          max_F1 = weightedF1_rf
          final_ntree = ntree_values[n]
          final_mtry = mtry_values[j]
        }
      }
    }
  }
  
  model_rf = randomForest(FailType ~., data = data[, scenarios[[m]]], mtry = final_mtry, ntree = final_ntree)
  prediction = predict(model_rf, test_data[, scenarios[[m]]], type = "class")
  predictions_rf = table(OBSERVED = test_data[, scenarios[[m]]]$FailType, PREDICTED = prediction)
  confusion_rf = confusionMatrix(predictions_rf)$byClass
  confusion_rf[is.na(confusion_rf)] = 0
  
  weightedF1_rf = F1_weights("Heat Dissipation Failure", test_data[, scenarios[[m]]]) * confusion_rf["Class: Heat Dissipation Failure", "F1"] +
    F1_weights("Overstrain Failure", test_data[, scenarios[[m]]]) * confusion_rf["Class: Overstrain Failure", "F1"] +
    F1_weights("Power Failure", test_data[, scenarios[[m]]]) * confusion_rf["Class: Power Failure", "F1"] +
    F1_weights("Random Failures", test_data[, scenarios[[m]]]) * confusion_rf["Class: Random Failures", "F1"] +
    F1_weights("Tool Wear Failure", test_data[, scenarios[[m]]]) * confusion_rf["Class: Tool Wear Failure", "F1"]
  
  accuracy_rf = sum(test_data[, scenarios[[m]]]$FailType == prediction) / nrow(test_data[, scenarios[[m]]])
  mean_recall_rf = mean(confusion_rf[, "Recall"])
  mean_precision_rf = mean(confusion_rf[, "Precision"])
  
  auc_rf = multiclass.roc(as.numeric(test_data[, scenarios[[m]]]$FailType), as.numeric(prediction))
  
  results_RF_HP = rbind(results_RF_HP, data.frame(MODEL = "Random Forest", DATA = name, SCENARIOS = m, NORMALIZED = normal,
                                                  ACCURACY = accuracy_rf, AUC = auc_rf$auc[1], MEAN_RECALL = mean_recall_rf,
                                                  MEAN_PRECISION = mean_precision_rf, WEIGHTED_F1 = weightedF1_rf,
                                                  MTRY = final_mtry, NTREE = final_ntree))
}

##Unbalanced Data with Normalization
data_info = scenarios_data[[2]]
data = data_info$data
test_data = data_info$test_data
name = data_info$name
normal = data_info$normal

mtry_values = c(1,3,5)
ntree_values = c(250, 500, 750, 1000)

max_F1 = 0
final_ntree = 0
final_mtry = 0

for (m in 1:length(scenarios)){
  
  for (n in 1:length(ntree_values)) {
    for (j in 1:length(mtry_values)) {
      for (k in 1:num_folds){
        model_rf = randomForest(FailType ~., data = data[-folds[[k]], scenarios[[m]]], mtry = mtry_values[j], ntree = ntree_values[n])
        prediction = predict(model_rf, data[folds[[k]], scenarios[[m]]], type = "class")
        predictions_rf = table(OBSERVED = data[folds[[k]], scenarios[[m]]]$FailType, PREDICTED = prediction)
        confusion_rf = confusionMatrix(predictions_rf)$byClass
        confusion_rf[is.na(confusion_rf)] = 0
        
        weightedF1_rf = F1_weights("Heat Dissipation Failure", data[folds[[k]],]) * confusion_rf["Class: Heat Dissipation Failure", "F1"] +
          F1_weights("Overstrain Failure", data[folds[[k]],]) * confusion_rf["Class: Overstrain Failure", "F1"] +
          F1_weights("Power Failure", data[folds[[k]],]) * confusion_rf["Class: Power Failure", "F1"] +
          F1_weights("Random Failures", data[folds[[k]],]) * confusion_rf["Class: Random Failures", "F1"] +
          F1_weights("Tool Wear Failure", data[folds[[k]],]) * confusion_rf["Class: Tool Wear Failure", "F1"]
        
        if (weightedF1_rf > max_F1) {
          max_F1 = weightedF1_rf
          final_ntree = ntree_values[n]
          final_mtry = mtry_values[j]
        }
      }
    }
  }
  
  model_rf = randomForest(FailType ~., data = data[, scenarios[[m]]], mtry = final_mtry, ntree = final_ntree)
  prediction = predict(model_rf, test_data[, scenarios[[m]]], type = "class")
  predictions_rf = table(OBSERVED = test_data[, scenarios[[m]]]$FailType, PREDICTED = prediction)
  confusion_rf = confusionMatrix(predictions_rf)$byClass
  confusion_rf[is.na(confusion_rf)] = 0
  
  weightedF1_rf = F1_weights("Heat Dissipation Failure", test_data[, scenarios[[m]]]) * confusion_rf["Class: Heat Dissipation Failure", "F1"] +
    F1_weights("Overstrain Failure", test_data[, scenarios[[m]]]) * confusion_rf["Class: Overstrain Failure", "F1"] +
    F1_weights("Power Failure", test_data[, scenarios[[m]]]) * confusion_rf["Class: Power Failure", "F1"] +
    F1_weights("Random Failures", test_data[, scenarios[[m]]]) * confusion_rf["Class: Random Failures", "F1"] +
    F1_weights("Tool Wear Failure", test_data[, scenarios[[m]]]) * confusion_rf["Class: Tool Wear Failure", "F1"]
  
  accuracy_rf = sum(test_data[, scenarios[[m]]]$FailType == prediction) / nrow(test_data[, scenarios[[m]]])
  mean_recall_rf = mean(confusion_rf[, "Recall"])
  mean_precision_rf = mean(confusion_rf[, "Precision"])
  
  auc_rf = multiclass.roc(as.numeric(test_data[, scenarios[[m]]]$FailType), as.numeric(prediction))
  
  results_RF_HP = rbind(results_RF_HP, data.frame(MODEL = "Random Forest", DATA = name, SCENARIOS = m, NORMALIZED = normal,
                                                  ACCURACY = accuracy_rf, AUC = auc_rf$auc[1], MEAN_RECALL = mean_recall_rf,
                                                  MEAN_PRECISION = mean_precision_rf, WEIGHTED_F1 = weightedF1_rf,
                                                  MTRY = final_mtry, NTREE = final_ntree))
}

write_xlsx(results_RF_HP, "C:\\Users\\Rodrigo\\Desktop\\4A2S\\AE\\Assignment\\Maintenance\\Results\\ResultsRandomFOrest.xlsx")



################# Support Vector Machines###########
results_SVM <- data.frame()
scenario_f1<-0

for (m in 1:length(scenarios)) {
  
  # Parameter tuning
  gamma_values <- c(1, 0.1, 0.01, 0.001, 0.0001)
  cost_values <- c(0.01, 0.1, 1, 10, 100, 1000)
  
  parameters_svm1 <- as.table(matrix(nrow = length(gamma_values), ncol = length(cost_values), 
                                     dimnames = list(GAMMA=gamma_values, COST=cost_values)))
  
  # Unbalanced data
  max_F1 <- 0
  final_gamma_1 <- 0
  final_cost_1 <- 0
  
  for (i in 1:length(gamma_values)) {
    
    for (j in 1:length(cost_values)) {
      
      weightedF1_mean <- vector()
      accuracy_mean <- vector()
      recall_mean <- vector()
      precision_mean <- vector()
      
      for (k in 1:num_folds) {
        
        set.seed(20)
        model_svm <- svm(FailType~., data=train_data_norm[-folds[[k]],scenarios[[m]]], gamma = gamma_values[i], cost = cost_values[j])
        prediction <- predict(model_svm, train_data_norm[folds[[k]],scenarios[[m]]])
        predictions_svm<-table(OBSERVED=train_data_norm[folds[[k]],]$FailType, PREDICTED=prediction)
        
        confusion<-confusionMatrix(predictions_svm)$byClass
        confusion[is.na(confusion)] <- 0
        
        weightedF1<-F1_weights("Heat Dissipation Failure",train_data_norm[folds[[k]],])*confusion["Class: Heat Dissipation Failure","F1"]+
          F1_weights("Overstrain Failure",train_data_norm[folds[[k]],])*confusion["Class: Overstrain Failure","F1"]+
          F1_weights("Power Failure",train_data_norm[folds[[k]],])*confusion["Class: Power Failure","F1"]+
          F1_weights("Random Failures",train_data_norm[folds[[k]],])*confusion["Class: Random Failures","F1"]+
          F1_weights("Tool Wear Failure",train_data_norm[folds[[k]],])*confusion["Class: Tool Wear Failure","F1"]
        
        accuracy<-sum(train_data_norm[folds[[k]],]$FailType==prediction)/nrow(train_data_norm[folds[[k]],])
        mean_recall<-mean(confusion[,"Recall"])
        mean_precision<-mean(confusion[,"Precision"])
        
        weightedF1_mean[k] <- weightedF1
        accuracy_mean[k] <- accuracy
        recall_mean[k] <- mean_recall
        precision_mean[k] <- mean_precision
        
      }
      
      parameters_svm1[i,j] <- mean(weightedF1_mean)
      
      if (mean(weightedF1_mean)>max_F1) {
        max_F1 <- mean(weightedF1_mean)
        final_cost1 <- cost_values[j]
        final_gamma1 <- gamma_values[i]
      }
      
    }
    
  }
  
  # Final model
  set.seed(20)
  model_svm1 <- svm(FailType~., data=train_data_norm[-folds[[1]],scenarios[[m]]], gamma = final_gamma1, cost = final_cost1)
  prediction <- predict(model_svm1, test_data_norm[,scenarios[[m]]])
  predictions_svm1<-table(OBSERVED=test_data_norm$FailType, PREDICTED=prediction)
  
  confusion_svm1<-confusionMatrix(predictions_svm1)$byClass
  confusion_svm1[is.na(confusion_svm1)] <- 0
  
  weightedF1_svm1<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_svm1["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_svm1["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_svm1["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_svm1["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_svm1["Class: Tool Wear Failure","F1"]
  
  accuracy_svm1<-sum(test_data_norm$FailType==prediction)/nrow(test_data_norm)
  mean_recall_svm1<-mean(confusion_svm1[,"Recall"])
  mean_precision_svm1<-mean(confusion_svm1[,"Precision"])
  
  auc_svm1 <- multiclass.roc(as.numeric(test_data$FailType), as.numeric(prediction))
  
  if (weightedF1_svm1>scenario_f1) {
    scenario_f1<-weightedF1_svm1
    prediction_SVM1 <- prediction
  }
  
  # Summarizing the results of all scenarios
  results_SVM<-rbind(results_SVM, data.frame(MODEL="SVM", DATA="Unbalanced", SCENARIO=m,
                                             ACCURACY=accuracy_svm1, AUC=auc_svm1$auc[1], MEAN_RECALL=mean_recall_svm1,
                                             MEAN_PRECISION=mean_precision_svm1,WEIGHTED_F1= weightedF1_svm1, GAMMA = final_gamma1, COST = final_cost1))
}


# Balanced data Normalized
scenario_f1<-0

for (m in 1:length(scenarios)) {
  
  max_F1 <- 0
  final_gamma2 <- 0
  final_cost2 <- 0
  
  parameters_svm2 <- as.table(matrix(nrow = length(gamma_values), ncol = length(cost_values), 
                                     dimnames = list(GAMMA=gamma_values, COST=cost_values)))
  
  
  for (i in 1:length(gamma_values)) {
    
    for (j in 1:length(cost_values)) {
      
      weightedF1_mean <- vector()
      accuracy_mean <- vector()
      recall_mean <- vector()
      precision_mean <- vector()
      
      for (k in 1:num_folds) {
        set.seed(20)
        model_svm <- svm(FailType~., data=train_fold_norm[train_fold_norm$fold==k,scenarios[[m]]], gamma = gamma_values[i], cost = cost_values[j])
        prediction <- predict(model_svm, train_validation_norm[train_validation_norm$fold==k,scenarios[[m]]])
        predictions_svm<-table(OBSERVED=train_validation_norm[train_validation_norm$fold==k,]$FailType, PREDICTED=prediction)
        
        confusion<-confusionMatrix(predictions_svm)$byClass
        confusion[is.na(confusion)] <- 0
        
        weightedF1<-F1_weights("Heat Dissipation Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion["Class: Heat Dissipation Failure","F1"]+
          F1_weights("Overstrain Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion["Class: Overstrain Failure","F1"]+
          F1_weights("Power Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion["Class: Power Failure","F1"]+
          F1_weights("Random Failures",train_validation_norm[train_validation_norm$fold==k,])*confusion["Class: Random Failures","F1"]+
          F1_weights("Tool Wear Failure",train_validation_norm[train_validation_norm$fold==k,])*confusion["Class: Tool Wear Failure","F1"]
        
        accuracy<-sum(train_validation_norm[train_validation_norm$fold==k,]$FailType==prediction)/nrow(train_validation_norm[train_validation_norm$fold==k,])
        mean_recall<-mean(confusion[,"Recall"])
        mean_precision<-mean(confusion[,"Precision"])
        
        weightedF1_mean[k] <- weightedF1
        accuracy_mean[k] <- accuracy
        recall_mean[k] <- mean_recall
        precision_mean[k] <- mean_precision
        
      }
      
      parameters_svm2[i,j] <- mean(weightedF1_mean)
      if (mean(weightedF1_mean)>max_F1) {
        max_F1 <- mean(weightedF1_mean)
        final_gamma2 <- gamma_values[i]
        final_cost2 <- cost_values[j]
        
      }
      
    }
  }
  
  
  # Final model
  set.seed(20)
  model_svm2 <- svm(FailType~., data=train_fold_norm[train_fold_norm$fold==1,scenarios[[m]]], 
                    gamma = final_gamma2, cost = final_cost2)
  prediction <- predict(model_svm2, test_data_norm[,scenarios[[m]]])
  predictions_svm2 <-table(OBSERVED=test_data_norm$FailType, PREDICTED=prediction)
  
  confusion_svm2<-confusionMatrix(predictions_svm2)$byClass
  confusion_svm2[is.na(confusion_svm2)] <- 0
  
  weightedF1_svm2<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_svm2["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_svm2["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_svm2["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_svm2["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_svm2["Class: Tool Wear Failure","F1"]
  
  accuracy_svm2<-sum(test_data_norm$FailType==prediction)/nrow(test_data_norm)
  mean_recall_svm2<-mean(confusion_svm2[,"Recall"])
  mean_precision_svm2<-mean(confusion_svm2[,"Precision"])
  
  auc_svm2 <- multiclass.roc(as.numeric(test_data$FailType), as.numeric(prediction))
  
  if (weightedF1_svm2>scenario_f1) {
    scenario_f1<-weightedF1_svm2
    prediction_SVM2 <- prediction
  }
  
  # Summarizing the results of all scenarios
  results_SVM<-rbind(results_SVM, data.frame(MODEL = "SVM", DATA = "Balanced", SCENARIO=m,
                                             ACCURACY = accuracy_svm2, AUC=auc_svm2$auc[1], MEAN_RECALL = mean_recall_svm2,
                                             MEAN_PRECISION = mean_precision_svm2, WEIGHTED_F1 = weightedF1_svm2, GAMMA = final_gamma2, COST = final_cost2))
}

write_xlsx(results_SVM, "C:\\Users\\Rodrigo\\Desktop\\4A2S\\AE\\Assignment\\Maintenance\\Results\\ResultsSVMachines.xlsx")







#################### XGBoost############

results_XGB <- data.frame()
scenarios_xgb = list(scenarios[[1]], scenarios[[5]], scenarios[[6]], scenarios[[7]], scenarios[[8]])
scenarios
# Unbalanced data

# Parameter tuning for scenario 1
depth<-c(4,8)
child_weight<-c(1,5)
eta_param<-c(0.001,0.05)
gamma_param<-c(0,5)
xgboost_paramTuning_unbal<-data.frame()

max_F1 <-0
final_depth<-0
final_child_weight<-0
final_eta<-0
final_gamma<-0

for (j in 1:length(depth)) {
  
  for (k in 1:length(child_weight)) {
    
    for (l in 1:length(eta_param)) {
      
      for (m in 1:length(gamma_param)) {
        
        weightedF1_mean <- vector()
        accuracy_mean <- vector()
        recall_mean <- vector()
        precision_mean <- vector()
        
        for (x in 1:num_folds) {
          xgboost_data<-train_data_norm[-folds[[x]],]
          
          xgboost_data$FailType<-as.numeric(xgboost_data$FailType)
          
          xgboost_data<-as.matrix(xgboost_data)
          train_matrix<-xgb.DMatrix(data = xgboost_data[,-ncol(xgboost_data)], label = xgboost_data[,ncol(xgboost_data)]-1)
          
          
          xgboost_test<-train_data_norm[folds[[x]],]
          
          xgboost_test$FailType<-as.numeric(xgboost_test$FailType)
          
          xgboost_test<-as.matrix(xgboost_test)
          test_matrix<-xgb.DMatrix(data = xgboost_test[,-ncol(xgboost_data)], label = xgboost_test[,ncol(xgboost_data)]-1)
          
          numberOfClasses<-length(levels(train_data_norm$FailType))
          
          params = list(
            booster="gbtree",
            eta=eta_param[l],
            max_depth=depth[j],
            gamma=gamma_param[m],
            min_child_weight=child_weight[k],
            objective="multi:softprob",
            eval_metric="mlogloss",
            num_class=numberOfClasses,
            silent=1
          )
          
          set.seed(20)
          xgb.fit=xgb.train(
            params=params,
            data=train_matrix,
            nrounds=7000,
            watchlist=list(val1=train_matrix,val2=test_matrix),
            verbose=1
          )
          
          xgb.pred = predict(xgb.fit,test_matrix,reshape=T)
          xgb.pred = as.data.frame(xgb.pred)
          colnames(xgb.pred) = levels(train_fold$FailType)
          
          xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
          xgb.pred$label = levels(train_fold$FailType)[xgboost_test[,ncol(xgboost_test)]]
          
          
          predictions_xgb<-table(OBSERVED=xgb.pred$label, PREDICTED=xgb.pred$prediction)
          confusion <- confusionMatrix(factor(as.factor(xgb.pred$prediction),levels = c("Heat Dissipation Failure",
                                                                                        "Overstrain Failure",
                                                                                        "Power Failure",
                                                                                        "Random Failures",
                                                                                        "Tool Wear Failure")),
                                       factor(as.factor(train_validation[train_validation_norm$fold==x,"FailType"]),
                                              levels = c("Heat Dissipation Failure",
                                                         "Overstrain Failure",
                                                         "Power Failure",
                                                         "Random Failures",
                                                         "Tool Wear Failure")))$byClass
          
          confusion[is.na(confusion[,"F1"])] = 0
          confusion[is.na(confusion[,"Recall"])] = 0
          
          weightedF1_xgb<-F1_weights("Heat Dissipation Failure",train_data_norm[folds[[x]],])*confusion["Class: Heat Dissipation Failure","F1"]+
            F1_weights("Overstrain Failure",train_data_norm[folds[[x]],])*confusion["Class: Overstrain Failure","F1"]+
            F1_weights("Power Failure",train_data_norm[folds[[x]],])*confusion["Class: Power Failure","F1"]+
            F1_weights("Random Failures",train_data_norm[folds[[x]],])*confusion["Class: Random Failures","F1"]+
            F1_weights("Tool Wear Failure",train_data_norm[folds[[x]],])*confusion["Class: Tool Wear Failure","F1"]
          
          accuracy_xgb<-sum(train_data_norm[folds[[x]],"FailType"]==xgb.pred$prediction)/nrow(train_data_norm[folds[[x]],])
          recall_xgb<-mean(confusion[,"Recall"])
          precision_xgb<-mean(confusion[,"Precision"])
          
          weightedF1_mean[x] <- weightedF1_xgb
          accuracy_mean[x] <- accuracy_xgb
          recall_mean[x] <- recall_xgb
          precision_mean[x] <- precision_xgb
        }
        xgboost_paramTuning_unbal<-rbind(xgboost_paramTuning_unbal,
                                         data.frame(MAX_DEPTH=depth[j],
                                                    CHILD_WEIGHT=child_weight[k],
                                                    ETA=eta_param[l],
                                                    GAMMA=gamma_param[m],
                                                    F1=mean(weightedF1_mean)))
        if (mean(weightedF1_mean)>max_F1) {
          max_F1 <- mean(weightedF1_mean)
          final_depth<-depth[j]
          final_child_weight<-child_weight[k]
          final_eta<-eta_param[l]
          final_gamma<-gamma_param[m]
        }
      }
    }
  }
}


# Final Model for the 7 scenarios
for (m in 1:length(scenarios_xgb)) {
  
  xgboost_data<-train_data_norm[,scenarios_xgb[[m]]]
  
  xgboost_data$FailType<-as.numeric(xgboost_data$FailType)
  
  xgboost_data<-as.matrix(xgboost_data)
  train_matrix<-xgb.DMatrix(data = xgboost_data[,-ncol(xgboost_data)], label = xgboost_data[,ncol(xgboost_data)]-1)
  
  
  xgboost_test<-test_data_norm[,scenarios_xgb[[m]]]
  
  xgboost_test$FailType<-as.numeric(xgboost_test$FailType)
  
  xgboost_test<-as.matrix(xgboost_test)
  test_matrix<-xgb.DMatrix(data = xgboost_test[,-ncol(xgboost_data)], label = xgboost_test[,ncol(xgboost_data)]-1)
  
  numberOfClasses<-length(levels(train_data_norm$FailType))
  
  params = list(
    booster="gbtree",
    eta=0.05,
    max_depth=4,
    gamma=0,
    min_child_weight=1,
    objective="multi:softprob",
    eval_metric="mlogloss",
    num_class=numberOfClasses
  )
  
  set.seed(20)
  train
  xgb.fit=xgb.train(
    params=params,
    data=train_matrix,
    nrounds=7000,
    watchlist=list(val1=train_matrix,val2=test_matrix),
    verbose=1
  )
  
  xgb.pred = predict(xgb.fit,test_matrix,reshape=T)
  xgb.pred = as.data.frame(xgb.pred)
  colnames(xgb.pred) = levels(train_data_norm$FailType)
  
  xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
  xgb.pred$label = levels(train_data$FailType)[xgboost_test[,ncol(xgboost_test)]]
  
  
  predictions_xgb1<-table(OBSERVED=xgb.pred$label, PREDICTED=xgb.pred$prediction)
  
  
  confusion_xgb1 <- confusionMatrix(factor(as.factor(xgb.pred$prediction),levels = c("Heat Dissipation Failure",
                                                                                     "Overstrain Failure",
                                                                                     "Power Failure",
                                                                                     "Random Failures",
                                                                                     "Tool Wear Failure")),
                                    factor(as.factor(test_data_norm[,"FailType"]),
                                           levels = c("Heat Dissipation Failure",
                                                      "Overstrain Failure",
                                                      "Power Failure",
                                                      "Random Failures",
                                                      "Tool Wear Failure")))$byClass
  
  confusion_xgb1[is.na(confusion_xgb1[,"F1"])] = 0
  confusion_xgb1[is.na(confusion_xgb1[,"Recall"])] = 0
  
  weightedF1_xgb1<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_xgb1["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_xgb1["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_xgb1["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_xgb1["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_xgb1["Class: Tool Wear Failure","F1"]
  
  accuracy_xgb1<-sum(test_data_norm$FailType==xgb.pred$prediction)/nrow(test_data_norm)
  recall_xgb1<-mean(confusion_xgb1[,"Recall"])
  precision_xgb1<-mean(confusion_xgb1[,"Precision"])
  
  auc_xgb1 <- multiclass.roc(as.numeric(test_data_norm$FailType), as.numeric(as.factor(xgb.pred$prediction)))
  
  results_XGB<-rbind(results_XGB, data.frame(MODEL="XGBoost", DATA="Unbalanced", SCENARIO=m, ACCURACY=accuracy_xgb1, 
                                             AUC= auc_xgb1$auc[1], MEAN_RECALL=recall_xgb1, MEAN_PRECISION=precision_xgb1,
                                             WEIGHTED_F1= weightedF1_xgb1, DEPTH = 4, CHILD_WEIGHT = 1, ETA = 0.05, GAMMA = 0))
}


# Balanced data

# Parameter tuning for scenario 1
depth<-c(4,8)
child_weight<-c(1,5)
eta_param<-c(0.001,0.05)
gamma_param<-c(0,5)
xgboost_paramTuning<-data.frame()

max_F1 <-0
final_depth<-0
final_child_weight<-0
final_eta<-0
final_gamma<-0

for (j in 1:length(depth)) {
  for (k in 1:length(child_weight)) {
    for (l in 1:length(eta_param)) {
      for (m in 1:length(gamma_param)) {
        
        weightedF1_mean <- vector()
        accuracy_mean <- vector()
        recall_mean <- vector()
        precision_mean <- vector()
        
        for (x in 1:num_folds) {
          xgboost_data<-train_fold[train_fold_norm$fold==x,1:(ncol(train_fold)-1)]
          
          xgboost_data$FailType<-as.numeric(xgboost_data$FailType)
          
          xgboost_data<-as.matrix(xgboost_data)
          train_matrix<-xgb.DMatrix(data = xgboost_data[,-ncol(xgboost_data)], label = xgboost_data[,ncol(xgboost_data)]-1)
          
          
          xgboost_test<-train_validation_norm[train_validation_norm$fold==x,1:(ncol(train_validation_norm)-1)]
          
          xgboost_test$FailType<-as.numeric(xgboost_test$FailType)
          
          xgboost_test<-as.matrix(xgboost_test)
          test_matrix<-xgb.DMatrix(data = xgboost_test[,-ncol(xgboost_data)], label = xgboost_test[,ncol(xgboost_data)]-1)
          
          numberOfClasses<-length(levels(train_fold$FailType))
          
          params = list(
            booster="gbtree",
            eta=eta_param[l],
            max_depth=depth[j],
            gamma=gamma_param[m],
            min_child_weight=child_weight[k],
            objective="multi:softprob",
            eval_metric="mlogloss",
            num_class=numberOfClasses,
            silent=1
          )
          
          set.seed(20)
          xgb.fit=xgb.train(
            params=params,
            data=train_matrix,
            nrounds=7000,
            watchlist=list(val1=train_matrix,val2=test_matrix),
            verbose=1
          )
          
          xgb.pred = predict(xgb.fit,test_matrix,reshape=T)
          xgb.pred = as.data.frame(xgb.pred)
          colnames(xgb.pred) = levels(train_fold_norm$FailType)
          
          xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
          xgb.pred$label = levels(train_fold$FailType)[xgboost_test[,ncol(xgboost_test)]]
          
          
          predictions_xgb<-table(OBSERVED=xgb.pred$label, PREDICTED=xgb.pred$prediction)
          confusion <- confusionMatrix(factor(as.factor(xgb.pred$prediction),levels = c("Heat Dissipation Failure",
                                                                                        "Overstrain Failure",
                                                                                        "Power Failure",
                                                                                        "Random Failures",
                                                                                        "Tool Wear Failure")),
                                       factor(as.factor(train_validation_norm[train_validation_norm$fold==x,"FailType"]),
                                              levels = c("Heat Dissipation Failure",
                                                         "Overstrain Failure",
                                                         "Power Failure",
                                                         "Random Failures",
                                                         "Tool Wear Failure")))$byClass
          
          confusion[is.na(confusion[,"F1"])] = 0
          confusion[is.na(confusion[,"Recall"])] = 0
          
          weightedF1_xgb<-F1_weights("Heat Dissipation Failure",train_validation_norm[train_validation_norm$fold==x,1:(ncol(train_validation)-1)])*confusion["Class: Heat Dissipation Failure","F1"]+
            F1_weights("Overstrain Failure",train_validation_norm[train_validation_norm$fold==x,1:(ncol(train_validation_norm)-1)])*confusion["Class: Overstrain Failure","F1"]+
            F1_weights("Power Failure",train_validation_norm[train_validation_norm$fold==x,1:(ncol(train_validation_norm)-1)])*confusion["Class: Power Failure","F1"]+
            F1_weights("Random Failures",train_validation_norm[train_validation_norm$fold==x,1:(ncol(train_validation_norm)-1)])*confusion["Class: Random Failures","F1"]+
            F1_weights("Tool Wear Failure",train_validation_norm[train_validation_norm$fold==x,1:(ncol(train_validation_norm)-1)])*confusion["Class: Tool Wear Failure","F1"]
          
          accuracy_xgb<-sum(train_validation_norm[train_validation_norm$fold==x,]$FailType==prediction)/nrow(train_validation_norm[train_validation_norm$fold==x,])
          recall_xgb<-mean(confusion[,"Recall"])
          precision_xgb<-mean(confusion[,"Precision"])
          
          weightedF1_mean[x] <- weightedF1_xgb
          accuracy_mean[x] <- accuracy_xgb
          recall_mean[x] <- recall_xgb
          precision_mean[x] <- precision_xgb
        }
        xgboost_paramTuning<-rbind(xgboost_paramTuning,
                                   data.frame(MAX_DEPTH=depth[j],
                                              CHILD_WEIGHT=child_weight[k],
                                              ETA=eta_param[l],
                                              GAMMA=gamma_param[m],
                                              F1=mean(weightedF1_mean)))
        if (mean(weightedF1_mean)>max_F1) {
          max_F1 <- mean(weightedF1_mean)
          final_depth<-depth[j]
          final_child_weight<-child_weight[k]
          final_eta<-eta_param[l]
          final_gamma<-gamma_param[m]
        }
        
        
      }
    }
  }
}


# Final Model for the 7 scenarios
for (m in 1:length(scenarios_xgb)) {
  
  xgboost_data<-train_fold_norm[,scenarios_xgb[[m]]]
  
  xgboost_data$FailType<-as.numeric(xgboost_data$FailType)
  
  xgboost_data<-as.matrix(xgboost_data)
  train_matrix<-xgb.DMatrix(data = xgboost_data[,-ncol(xgboost_data)], label = xgboost_data[,ncol(xgboost_data)]-1)
  
  
  xgboost_test<-test_data_norm[,scenarios_xgb[[m]]]
  
  xgboost_test$FailType<-as.numeric(xgboost_test$FailType)
  
  xgboost_test<-as.matrix(xgboost_test)
  test_matrix<-xgb.DMatrix(data = xgboost_test[,-ncol(xgboost_data)], label = xgboost_test[,ncol(xgboost_data)]-1)
  
  numberOfClasses<-length(levels(train_fold$FailType))
  
  params = list(
    booster="gbtree",
    eta=0.001,
    max_depth=8,
    gamma=5,
    min_child_weight=1,
    objective="multi:softprob",
    eval_metric="mlogloss",
    num_class=numberOfClasses
  )
  
  set.seed(20)
  train
  xgb.fit=xgb.train(
    params=params,
    data=train_matrix,
    nrounds=7000,
    watchlist=list(val1=train_matrix,val2=test_matrix),
    verbose=1
  )
  
  xgb.pred = predict(xgb.fit,test_matrix,reshape=T)
  xgb.pred = as.data.frame(xgb.pred)
  colnames(xgb.pred) = levels(train_fold_norm$FailType)
  
  xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
  xgb.pred$label = levels(train_fold_norm$FailType)[xgboost_test[,ncol(xgboost_test)]]
  
  
  predictions_xgb2<-table(OBSERVED=xgb.pred$label, PREDICTED=xgb.pred$prediction)
  
  
  confusion <- confusionMatrix(factor(as.factor(xgb.pred$prediction),levels = c("Heat Dissipation Failure",
                                                                                "Overstrain Failure",
                                                                                "Power Failure",
                                                                                "Random Failures",
                                                                                "Tool Wear Failure")),
                               factor(as.factor(test_data_norm[,"FailType"]),
                                      levels = c("Heat Dissipation Failure",
                                                 "Overstrain Failure",
                                                 "Power Failure",
                                                 "Random Failures",
                                                 "Tool Wear Failure")))$byClass
  
  confusion[is.na(confusion[,"F1"])] = 0
  confusion[is.na(confusion[,"Recall"])] = 0
  
  weightedF1_xgb2<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion["Class: Tool Wear Failure","F1"]
  
  accuracy_xgb2<-sum(test_data_norm$FailType==xgb.pred$prediction)/nrow(test_data_norm)
  recall_xgb2<-mean(confusion[,"Recall"])
  precision_xgb2<-mean(confusion[,"Precision"])
  
  auc_xgb2 <- multiclass.roc(as.numeric(test_data$FailType), as.numeric(as.factor(xgb.pred$prediction)))
  
  results_XGB<-rbind(results_XGB, data.frame(MODEL="XGBoost", DATA="Balanced", SCENARIO=m, ACCURACY=accuracy_xgb2,
                                             AUC= auc_xgb2$auc[1], MEAN_RECALL=recall_xgb2, MEAN_PRECISION=precision_xgb2,
                                             WEIGHTED_F1= weightedF1_xgb2, DEPTH = params$max_depth, CHILD_WEIGHT = params$min_child_weight, ETA = params$eta, GAMMA = params$gamma))
}



#write_xlsx(results_XGB, "C:\\Users\\Rodrigo\\Desktop\\4A2S\\AE\\Assignment\\Maintenance\\Results\\ResultsXGBoost.xlsx")


######### NEURAL NETWORKS ############
results_NN <- data.frame()

scenario_f1<-0

scenarios_nn<-list(scenarios[[1]])


# Parameter tuning
layer_values <- list(c(1,1),c(3,3))


# Unbalanced data
max_F1 <- 0
final_layers <- 0

for (i in 1:length(layer_values)) {
  
  set.seed(20)
  model_neuralnet <- neuralnet(FailType~., data = train_data_norm[-folds[[1]],], 
                               hidden = layer_values[[i]] ,learningrate = 0.1, stepmax=1e7 ) 
  
  plot(model_neuralnet, rep= TRUE)
  
  prediction <- predict(model_neuralnet, train_data_norm[folds[[1]],])
  
  predictions_neuralnet <- table(OBSERVED=train_data_norm[folds[[1]],]$FailType, 
                                 PREDICTED = factor(apply(prediction, 1, which.max), levels = c(1:6)))
  
  colnames(predictions_neuralnet) <- c("Heat Dissipation Failure","No Failure","Overstrain Failure",
                                       "Power Failure","Random Failures","Tool Wear Failure")
  
  confusion_neuralnet<-confusionMatrix(predictions_neuralnet)$byClass
  confusion_neuralnet[is.na(confusion_neuralnet)] <- 0
  
  weightedF1_neuralnet<-F1_weights("Heat Dissipation Failure",train_data_norm[folds[[1]],])*confusion_neuralnet["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",train_data_norm[folds[[1]],])*confusion_neuralnet["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",train_data_norm[folds[[1]],])*confusion_neuralnet["Class: Power Failure","F1"]+
    F1_weights("Random Failures",train_data_norm[folds[[1]],])*confusion_neuralnet["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",train_data_norm[folds[[1]],])*confusion_neuralnet["Class: Tool Wear Failure","F1"]
  
  accuracy_neuralnet<-sum(as.numeric(train_data_norm[folds[[1]],]$FailType)==apply(prediction, 1, which.max))/nrow(train_data_norm[folds[[1]],])
  mean_recall_neuralnet<-mean(confusion_neuralnet[,"Recall"])
  mean_precision_neuralnet<-mean(confusion_neuralnet[,"Precision"])
  
  auc_neuralnet <- multiclass.roc(as.numeric(train_data_norm[folds[[1]],]$FailType), as.numeric(apply(prediction, 1, which.max)))
  
  if (mean(weightedF1_neuralnet)>max_F1) {
    max_F1 <- mean(weightedF1_neuralnet)
    final_layers <- layer_values[[i]]
  }
}


# Final model

for (m in 1:length(scenarios_nn)) {
  
  set.seed(20)
  model_nn1 <- neuralnet(FailType~., data = train_data_norm[,scenarios_nn[[m]]],
                         hidden = final_layers , learningrate = 0.1, stepmax=1e7) 
  
  plot(model_nn1, rep= TRUE)
  
  prediction <- predict(model_nn1, test_data_norm[,scenarios_nn[[m]]])
  
  predictions_nn1 <- table(OBSERVED=test_data_norm$FailType, 
                           PREDICTED = factor(apply(prediction, 1, which.max), levels = c(1:6)))
  
  colnames(predictions_nn1) <- c("Heat Dissipation Failure","No Failure","Overstrain Failure",
                                 "Power Failure","Random Failures","Tool Wear Failure")
  
  confusion_nn1<-confusionMatrix(predictions_nn1)$byClass
  confusion_nn1[is.na(confusion_nn1)] <- 0
  
  weightedF1_nn1<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_nn1["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_nn1["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_nn1["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_nn1["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_nn1["Class: Tool Wear Failure","F1"]
  
  accuracy_nn1<-sum(as.numeric(test_data_norm$FailType)==apply(prediction, 1, which.max))/nrow(test_data_norm)
  mean_recall_nn1<-mean(confusion_nn1[,"Recall"])
  mean_precision_nn1<-mean(confusion_nn1[,"Precision"])
  
  auc_nn1 <- multiclass.roc(as.numeric(test_data_norm$FailType), as.numeric(apply(prediction, 1, which.max)))
  
  if (weightedF1_nn1>scenario_f1) {
    scenario_f1<-weightedF1_nn1
    prediction_NN1 <- factor(apply(prediction, 1, which.max), levels = c(1:6))
  }
  
  # Summarizing the results of all scenarios
  results_NN<-rbind(results_NN, data.frame(MODEL = "Neural Networks", DATA = "Unbalanced", SCENARIO = m, LAYERS = final_layers,
                                           ACCURACY = accuracy_nn1, AUC=auc_nn1$auc[1], MEAN_RECALL = mean_recall_nn1, 
                                           MEAN_PRECISION = mean_precision_nn1, WEIGHTED_F1 = weightedF1_nn1))
}

levels(prediction_NN1) <- c("Heat Dissipation Failure", "No Failure", "Overstrain Failure", "Power Failure", "Random Failures", "Tool Wear Failure")



# Balanced data

scenario_f1<-0


# Parameter tuning
layer_values <- list(c(3,3),c(6,6))


max_F1 <- 0
final_layers <- 0

for (i in 1:length(layer_values)) {
  
  set.seed(20)
  model_neuralnet1 <- neuralnet(FailType~.-fold, data = train_fold_norm[train_fold_norm$fold==1,], 
                                hidden = layer_values[[i]] ,learningrate = 0.1, stepmax=1e7 ) 
  
  plot(model_neuralnet1, rep= TRUE)
  
  prediction <- predict(model_neuralnet1, train_validation_norm[train_validation_norm$fold==1,])
  
  predictions_neuralnet1 <- table(OBSERVED=train_validation_norm[train_validation_norm$fold==1,]$FailType, 
                                  PREDICTED = factor(apply(prediction, 1, which.max), levels = c(1:6)))
  
  colnames(predictions_neuralnet1) <- c("Heat Dissipation Failure","No Failure","Overstrain Failure",
                                        "Power Failure","Random Failures","Tool Wear Failure")
  
  confusion_neuralnet1<-confusionMatrix(predictions_neuralnet1)$byClass
  confusion_neuralnet1[is.na(confusion_neuralnet1)] <- 0
  
  weightedF1_neuralnet1<-F1_weights("Heat Dissipation Failure",train_validation_norm[train_validation_norm$fold==1,])*confusion_neuralnet1["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",train_validation_norm[train_validation_norm$fold==1,])*confusion_neuralnet1["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",train_validation_norm[train_validation_norm$fold==1,])*confusion_neuralnet1["Class: Power Failure","F1"]+
    F1_weights("Random Failures",train_validation_norm[train_validation_norm$fold==1,])*confusion_neuralnet1["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",train_validation_norm[train_validation_norm$fold==1,])*confusion_neuralnet1["Class: Tool Wear Failure","F1"]
  
  accuracy_neuralnet1<-sum(as.numeric(train_validation_norm[train_validation_norm$fold==1,]$FailType)==apply(prediction, 1, which.max))/nrow(train_validation_norm[train_validation_norm$fold==1,])
  mean_recall_neuralnet1<-mean(confusion_neuralnet1[,"Recall"])
  mean_precision_neuralnet1<-mean(confusion_neuralnet1[,"Precision"])
  
  auc_neuralnet1 <- multiclass.roc(as.numeric(train_validation_norm[train_validation_norm$fold==1,]$FailType), 
                                   as.numeric(apply(prediction, 1, which.max)))
  
  if (mean(weightedF1_neuralnet)>max_F1) {
    max_F1 <- mean(weightedF1_neuralnet)
    final_layers <- layer_values[[i]]
  }
}


# Final model
for (m in 1:length(scenarios_nn)) {
  
  set.seed(20)
  model_nn2 <- neuralnet(FailType~., data = train_fold_norm[,scenarios_nn[[m]]],
                         hidden = final_layers , learningrate = 0.1, stepmax=1e7) 
  
  plot(model_nn2, rep= TRUE)
  
  prediction <- predict(model_nn2, test_data_norm[,scenarios_nn[[m]]])
  
  predictions_nn2 <- table(OBSERVED=test_data_norm$FailType, 
                           PREDICTED = factor(apply(prediction, 1, which.max), levels = c(1:6)))
  
  colnames(predictions_nn2) <- c("Heat Dissipation Failure","No Failure","Overstrain Failure",
                                 "Power Failure","Random Failures","Tool Wear Failure")
  
  confusion_nn2<-confusionMatrix(predictions_nn2)$byClass
  confusion_nn2[is.na(confusion_nn2)] <- 0
  
  weightedF1_nn2<-F1_weights("Heat Dissipation Failure",test_data_norm)*confusion_nn2["Class: Heat Dissipation Failure","F1"]+
    F1_weights("Overstrain Failure",test_data_norm)*confusion_nn2["Class: Overstrain Failure","F1"]+
    F1_weights("Power Failure",test_data_norm)*confusion_nn2["Class: Power Failure","F1"]+
    F1_weights("Random Failures",test_data_norm)*confusion_nn2["Class: Random Failures","F1"]+
    F1_weights("Tool Wear Failure",test_data_norm)*confusion_nn2["Class: Tool Wear Failure","F1"]
  
  accuracy_nn2<-sum(as.numeric(test_data_norm$FailType)==apply(prediction, 1, which.max))/nrow(test_data_norm)
  mean_recall_nn2<-mean(confusion_nn2[,"Recall"])
  mean_precision_nn2<-mean(confusion_nn2[,"Precision"])
  
  auc_nn2 <- multiclass.roc(as.numeric(test_data_norm$FailType), as.numeric(apply(prediction, 1, which.max)))
  
  if (weightedF1_nn2>scenario_f1) {
    scenario_f1<-weightedF1_nn2
    prediction_NN2 <- factor(apply(prediction, 1, which.max), levels = c(1:6))
  }
  
  # Summarizing the results of all scenarios
  results_NN<-rbind(results_NN, data.frame(MODEL = "Neural Networks", DATA = "Balanced", SCENARIO = m, LAYERS = final_layers,
                                           ACCURACY = accuracy_nn2, AUC=auc_nn2$auc[1], MEAN_RECALL = mean_recall_nn2, 
                                           MEAN_PRECISION = mean_precision_nn2, WEIGHTED_F1 = weightedF1_nn2))
}

levels(prediction_NN2) <- c("Heat Dissipation Failure", "No Failure", "Overstrain Failure", "Power Failure", "Random Failures", "Tool Wear Failure")
















#######Detecting Outliers#######
##Z Score Method

## Calculate z-scores for each numeric column in the dataframe
z_scores = abs(scale(data[, sapply(data, is.numeric)]))


## Identify outliers based on z-score threshold
outliers_z = which(z_scores > 3, arr.ind = TRUE)
outliers_z = data.frame(outliers_z)
outliers_rot_z = outliers_z[outliers_z$col == 3,]
outliers_tor_z = outliers_z[outliers_z$col == 4,]

##Analyzing Rotational Speed Outliers
outliers_rot_z_fail = c()
for (row_index in outliers_rot_z$row) {
  # Check if the corresponding value in data[, 7] is equal to 1
  if (data[row_index, 7] == 1) {
    # Append the row index to outliers_rot_z_fail
    outliers_rot_z_fail = c(outliers_rot_z_fail, row_index)
  }
}

outliers_rot_z_no_fail = setdiff(outliers_rot_z$row, outliers_rot_z_fail)

## Scatter plot
### Create a scatter plot with improved appearance
plot(data$RotSpeed, col="black", xlab="Index", ylab="RotSpeed", main="Scatter Plot of RotSpeed", pch=20, cex=0.7)
grid()  # Add grid lines

### Add points for outliers with failure in red
points(outliers_rot_z_fail, data$RotSpeed[outliers_rot_z_fail], col="red", pch=20, cex=0.7)

### Add points for outliers without failure in blue
points(outliers_rot_z_no_fail, data$RotSpeed[outliers_rot_z_no_fail], col="blue", pch=20, cex=0.7)

yellow_points = which(data$Target == 1 & !(1:length(data$RotSpeed) %in% outliers_rot_z_fail))
points(yellow_points, data$RotSpeed[yellow_points], col="yellow", pch=20, cex=0.7)

### Add legend with larger text
legend("topright", legend=c("Normal", "Outliers with Failure", "Outliers without Failure", "Target = 1"),
       col=c("black", "red", "blue", "yellow"), pch=20, cex=0.8, bty="n")


##Analyzing Torque Outliers
outliers_tor_z_fail = c()
for (row_index in outliers_tor_z$row) {
  # Check if the corresponding value in data[, 7] is equal to 1
  if (data[row_index, 7] == 1) {
    # Append the row index to outliers_rot_z_fail
    outliers_tor_z_fail = c(outliers_tor_z_fail, row_index)
  }
}

outliers_tor_z_no_fail = setdiff(outliers_tor_z$row, outliers_tor_z_fail)

### Scatter plot
#### Create a scatter plot with improved appearance
plot(data$Torque, col="black", xlab="Index", ylab="Torque", main="Scatter Plot of Torque", pch=20, cex=0.7)
grid()  # Add grid lines

#### Add points for outliers with failure in red
points(outliers_tor_z_fail, data$Torque[outliers_tor_z_fail], col="red", pch=20, cex=0.7)

#### Add points for outliers without failure in blue
points(outliers_tor_z_no_fail, data$Torque[outliers_tor_z_no_fail], col="blue", pch=20, cex=0.7)

yellow_points = which(data$Target == 1 & !(1:length(data$Torque) %in% outliers_tor_z_fail))
points(yellow_points, data$Torque[yellow_points], col="yellow", pch=20, cex=0.7)

#### Add legend with larger text
legend("topright", legend=c("Normal", "Outliers with Failure", "Outliers without Failure", "Target = 1"),
       col=c("black", "red", "blue", "yellow"), pch=20, cex=0.8, bty="n")


## IQR Method
#IQR Torque 
quartiles = quantile(data$Torque, probs=c(.25, .75), na.rm = FALSE)
IQR_Torque = IQR(data$Torque)
Lower = quartiles[1] - 1.5*IQR_Torque
Upper = quartiles[2] + 1.5*IQR_Torque
outliers_IQR_Torque =which(data$Torque< Lower | data$Torque> Upper)
outliers_iqr_torque_df = data.frame(row = outliers_IQR_Torque, Torque = data$Torque[outliers_IQR_Torque])

#IQR Rotational Seep
quartiles = quantile(data$RotSpeed, probs=c(.25, .75), na.rm = FALSE)
IQR_RotSeep = IQR(data$RotSpeed)
Lower1 = quartiles[1] - 1.5*IQR_RotSeep
Upper1 = quartiles[2] + 1.5*IQR_RotSeep
outliers_IQR_RotSeep = which(data$RotSpeed< Lower1 | data$RotSpeed> Upper1)
outliers_iqr_RotSeep_df = data.frame(row = outliers_IQR_RotSeep, RotSeep = data$RotSpeed[outliers_IQR_RotSeep])


##Euclidean distance

# Calcular o centróide dos dados para Torque
centroid_Torque = mean(data$Torque, na.rm = TRUE)
distances_Torque = sqrt((data$Torque - centroid_Torque)^2)
threshold_Torque =2.5*sd(data$Torque) # Ajuste conforme necessário
outliers_euclidean_Torque = which(distances_Torque > threshold_Torque)

# Criar dataframe com os outliers para Torque
outliers_euclidean_Torque_df = data.frame(row = outliers_euclidean_Torque,
                                          Torque = data$Torque[outliers_euclidean_Torque])


# Calcular o centróide dos dados para RotSpeed
centroid_RotSpeed = mean(data$RotSpeed, na.rm = TRUE)
distances_RotSpeed = sqrt((data$RotSpeed - centroid_RotSpeed)^2)
threshold_RotSpeed = 2.5 * sd(data$RotSpeed) # Ajuste conforme necessário
outliers_euclidean_RotSpeed = which(distances_RotSpeed > threshold_RotSpeed)

# Criar dataframe com os outliers para RotSpeed
outliers_euclidean_RotSpeed_df = data.frame(row = outliers_euclidean_RotSpeed,
                                            RotSpeed = data$RotSpeed[outliers_euclidean_RotSpeed])



## LOF Method

# Calculate LOF for each numeric column in the dataframe
lof_scores <- LOF(data[, sapply(data, is.numeric)], k=2)

# Set a threshold for identifying outliers based on LOF scores
lof_threshold <- 2

# Identify outliers based on LOF scores exceeding the threshold
outliers_lof <- which(lof_scores > lof_threshold)

# Filter outliers
outliers_data_lof <- data[outliers_lof, ]

# Separate outliers based on FailType
outliers_fail <- outliers_data_lof[outliers_data_lof$FailType == 1, ]
outliers_no_fail <- outliers_data_lof[outliers_data_lof$FailType != 1, ]


# Dados fictícios para os cinco modelos (substitua pelos seus próprios dados)
modelos <- c("Decision Trees", "Random Forest" , "Support Vector Machines", "Neural Networks" , "XGBoost")
accuracy <- c(0.9846564376, 0.9889926618, 0.9866577718, 0.9839893262, 0.8925950634)
recall <- c(0.6539485767, 0.6490260604, 0.6311467784, 0.5972159924, 0.9670967742)
f1_score <- c(0.7371882086, 0.7354497354, 0.7099080945, 0.6635796918, 0.9588339083)
precision <- c(0.6306945062, 0.5959019856, 0.5616382456, 0.5380492184, 0.8735483871)

# Crie um dataframe com os dados
dados <- data.frame(Modelo = modelos, Accuracy = accuracy, Recall = recall, F1_score = f1_score, Precision = precision)

# Instale e carregue a biblioteca ggplot2 (se ainda não estiver instalada)
# install.packages("ggplot2")
library(ggplot2)

# Crie o gráfico de radar
ggplot(dados, aes(x = Modelo)) +
  geom_line(aes(y = Accuracy, group = 1, color = "Accuracy"), size = 1) +
  geom_line(aes(y = Recall, group = 1, color = "Recall"), size = 1) +
  geom_line(aes(y = F1_score, group = 1, color = "F1 Score"), size = 1) +
  geom_line(aes(y = Precision, group = 1, color = "Precision"), size = 1) +
  geom_point(aes(y = Accuracy, color = "Accuracy"), size = 3) +
  geom_point(aes(y = Recall, color = "Recall"), size = 3) +
  geom_point(aes(y = F1_score, color = "F1 Score"), size = 3) +
  geom_point(aes(y = Precision, color = "Precision"), size = 3) +
  labs(title = "Comparação entre 5 modelos",
       x = NULL, y = NULL) +
  theme_minimal() +
  scale_color_manual(values = c("Accuracy" = "blue", "Recall" = "green", "F1 Score" = "red", "Precision" = "violet")) +
  theme(legend.position = "top")


#Graph Plotting
###Analysis of Temperature Variation
# Create a sequence of observation numbers
observation_numbers = seq_along(data$AirT)

# Plot with scatter plots for AirT and ProcessT
ggplot(data, aes(x = observation_numbers)) +
  geom_line(aes(y = data$AirT, color = "Air Temperature"), show.legend = TRUE) +  # Add color and label for Air Temperature
  geom_line(aes(y = data$ProcessT, color = "Process Temperature"), show.legend = TRUE) +  # Add color and label for Process Temperature
  scale_y_continuous("Temperature", sec.axis = sec_axis(~., name = "Process Temperature")) +
  scale_color_manual(values = c("black", "tomato"), labels = c("Air Temperature", "Process Temperature")) +  # Define colors and labels
  theme_minimal()


ggplot(data, aes(x = observation_numbers)) +
  geom_line(aes(y = temp_dif), color = "tomato") +  # Use geom_line instead of geom_point
  scale_y_continuous("Temperature Difference") +
  theme_minimal()

# Create a scatter plot with improved appearance
plot(temp_dif, col="black", xlab="Index", ylab="Temperature Difference", main="Scatter Plot of Temp. Dif.", pch=20, cex=0.7)
grid()  # Add grid lines

# Add points for outliers with failure in red
points(which(data$Target == 1), temp_dif[which(data$Target == 1)], col="tomato", pch=20, cex=0.7)

# Add legend with larger text
legend("topright", legend=c("No Failure", "Failure"),
       col=c("black", "tomato"), pch=20, cex=0.8, bty="n")


##Check for failure type
dif_low_9 = which(temp_dif <= 9 & data$Target == 1)
fail_low_9 = data.frame(index = dif_low_9, difference = temp_dif[dif_low_9], type = data[dif_low_9, 10])
sum(fail_low_9$type == "Heat Dissipation Failure") / nrow(fail_low_9)

#Plot the chart
ggplot(fail_low_9, aes(x = index, y = difference, color = type == "Heat Dissipation Failure")) +
  geom_point(size = 3) +  # Increase point size
  scale_color_manual(values = c("black", "tomato"), labels = c("Other Types", "Heat Dissipation Failure")) +
  labs(x = "Observation Index", y = "Temperature Difference", color = "Failure Type") +  # Add axis labels and legend title
  theme_minimal() +  # Apply minimal theme
  theme(legend.position = "bottom", legend.direction = "horizontal")  # Adjust legend position and direction

#sum(fail_low_9$type == "Heat Dissipation Failure") / sum(data$FailType == "Heat Dissipation Failure")


###Graphs for basic analysis

##BoxPloting
###Process Tempreature by Failure Type
ggplot(data, aes(x = data$FailType, y = data$ProcessT)) +
  geom_boxplot() +
  labs(x = "Failure Type", y = "Process Temperature [K]", title = "Comparison of Process Temperature between Failure Types")

#Air Temperature by Failure Type
ggplot(data, aes(x = data$FailType, y = data$AirT)) +
  geom_boxplot() +
  labs(x = "Failure Type", y = "Air Temperature [K]", title = "Comparison of Air Temperature between Failure Types")

#Process Temperature by Target
ggplot(data, aes(x = data$Target, y = data$ProcessT)) +
  geom_boxplot() +
  labs(x = "Failure", y = "Process Temperature [K]", title = "Comparison of Process Temperature if failure")

#Air Tempreature by Target
ggplot(data, aes(x = data$Target, y = data$AirT)) +
  geom_boxplot() +
  labs(x = "Failure", y = "Air Temperature [K]", title = "Comparison of Air Temperature if failure")

#Rotational Speed by Failure Type
ggplot(data, aes(x = data$FailType, y = data$RotSpeed)) +
  geom_boxplot() +
  labs(x = "Failure Type", y = "Rotational Speed [rpm]", title = "Comparison of Rotational Speed between Failure Types")

#Torque by Failure Type
ggplot(data, aes(x = data$FailType, y = data$Torque)) +
  geom_boxplot() +
  labs(x = "Failure Type", y = "Torque[Nm]]", title = "Comparison of Torque between Failure Types")

#Rotational Speed by Target
ggplot(data, aes(x = data$Target, y = data$RotSpeed)) +
  geom_boxplot() +
  labs(x = "Failure", y = "Rotational Speed [rpm]", title = "Comparison of Rotational Speed if failure")

#Torque by Target
ggplot(data, aes(x = data$Target, y = data$Torque)) +
  geom_boxplot() +
  labs(x = "Failure", y = "Torque [Nm]", title = "Comparison of Torque if failure")

##Checking for correlation
# Subset numerical variables from the dataset
numeric_data = data[, sapply(data, is.numeric)]

# Calculate correlation matrix
corr_mat = round(cor(numeric_data[,-1]),2)  
head(corr_mat)

# Create correlation heatmap
# reduce the size of correlation matrix
melted_corr_mat = melt(corr_mat)
# head(melted_corr_mat)

# plotting the correlation heatmap
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, 
                                     fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), 
            color = "black", size = 4)+
  scale_fill_gradient2(low = "tomato", mid = "white", high = "tomato", limits = c(-1,1))  # Reverse the order of colors

##Histograms
#AirT
ggplot(data = data, aes(x = data$AirT)) +
  geom_histogram(color = "black", fill = "tomato", bins = 50) +
  labs(x = "Air Temperature [K]", y = "Frequency", title = "Histogram of Air Temperature") +
  theme_minimal()+
  coord_fixed(ratio = 0.025)

#ProcessT
ggplot(data = data, aes(x = data$ProcessT)) +
  geom_histogram(color = "black", fill = "tomato", bins = 50) +
  labs(x = "Process Temperature [K]", y = "Frequency", title = "Histogram of Process Temperature") +
  theme_minimal()+
  coord_fixed(ratio = 0.025)

#Rotation Speed
ggplot(data = data, aes(x = data$RotSpeed)) +
  geom_histogram(color = "black", fill = "tomato", bins = 50) +
  labs(x = "Rotation Speed [rpm]", y = "Frequency", title = "Histogram of Rotation Speed") +
  theme_minimal()+
  coord_fixed(ratio = 2)

#Torque
ggplot(data = data, aes(x = data$Torque)) +
  geom_histogram(color = "black", fill = "tomato", bins = 50) +
  labs(x = "Torque[Nm]", y = "Frequency", title = "Histogram of Torque") +
  theme_minimal()+
  coord_fixed(ratio = 0.2)

#ToolWear
ggplot(data = data, aes(x = data$ToolWear)) +
  geom_histogram(color = "black", fill = "tomato", bins = 10000) +
  labs(x = "ToolWear", y = "Frequency", title = "Histogram of ToolWear") +
  theme_minimal()+
  coord_fixed(ratio = 1)

#KS Tests

# Function to perform KS test for a single variable
perform_ks_test = function(data, variable, dist_name) {
  result = ks.test(data[[variable]],"pnorm", mean = mean(data[[variable]]), sd = sd(data[[variable]]))
  return(list(variable = variable, TS = format(result$statistic, digits= 5), Pval = format(as.numeric(result$p.value), digits = 5)))
}

# Function to perform KS test for all variables in a dataframe
perform_ks_tests = function(data, dist_name) {
  results = lapply(names(data), function(variable) perform_ks_test(data, variable, dist_name))
  return(bind_rows(results))
}

ks_results = perform_ks_tests(numeric_data[,-1], "pnorm")
#write.xlsx(ks_results, file = "KSTests.xlsx", rowNames = FALSE)


##Pie Charts
#Types
values_pie_type = c(sum(data$Type =="H"), sum(data$Type =="M"), sum(data$Type =="L"))
labels_pie_type = c("H", "M", "L")
piepercent_type= round(100 * values_pie_type / sum(values_pie_type), 4)

# Plot the chart.
pie3D(values_pie_type, labels = piepercent_type,
      main = "Type pie chart", col = c("tomato","grey","yellow2"))
legend("topright", c("H", "M", "L"),
       cex = 0.8, fill = c("tomato","grey","yellow2"))

#Targets
values_pie_ftype = c(sum(data$Target ==0), sum(data$Target ==1))
labels_pie_ftype = c("No Failure", "Failure")
piepercent_ftype= round(100 * values_pie_ftype / sum(values_pie_ftype), 4)

# Plot the chart.
pie3D(values_pie_ftype, labels = piepercent_ftype,
      main = "Failure pie chart", col = c("tomato","grey"))
legend("topright", labels_pie_ftype,
       cex = 0.8, fill = c("tomato","grey"))

#Targets for H
values_pie_ftypeH = c(sum(data$Target ==0 & data$Type =="H"), sum(data$Target ==1 & data$Type =="H"))
labels_pie_ftypeH = c("No Failure", "Failure")
piepercent_ftypeH= round(100 * values_pie_ftypeH / sum(data$Type == "H"), 4)

# Plot the chart.
pie3D(values_pie_ftypeH, labels = piepercent_ftypeH,
      main = "Failure pie chart for Type H", col = c("tomato","grey"))
legend("topright", labels_pie_ftypeH,
       cex = 0.8, fill = c("tomato","grey"))

#Targets for M
values_pie_ftypeM = c(sum(data$Target ==0 & data$Type =="M"), sum(data$Target ==1 & data$Type =="M"))
labels_pie_ftypeM = c("No Failure", "Failure")
piepercent_ftypeM= round(100 * values_pie_ftypeM / sum(data$Type == "M"), 4)

# Plot the chart.
pie3D(values_pie_ftypeM, labels = piepercent_ftypeM,
      main = "Failure pie chart for Type M", col = c("tomato","grey"))
legend("topright", labels_pie_ftypeM,
       cex = 0.8, fill = c("tomato","grey"))

#Targets for L
values_pie_ftypeL = c(sum(data$Target ==0 & data$Type =="L"), sum(data$Target ==1 & data$Type =="L"))
labels_pie_ftypeL = c("No Failure", "Failure")
piepercent_ftypeL= round(100 * values_pie_ftypeL / sum(data$Type == "L"), 4)

# Plot the chart.
pie3D(values_pie_ftypeL, labels = piepercent_ftypeL,
      main = "Failure pie chart for Type L", col = c("tomato","grey"))
legend("topright", labels_pie_ftypeL,
       cex = 0.8, fill = c("tomato","grey"))