
######REGRESSION ANALYSIS Using CART and NNW##########

#######Neural Network#######

rm(list=ls()) #clear working environment
setwd("C:/Users/daisy/OneDrive/Desktop/6356 data")

library('caret')
library('dplyr')
library('neuralnet')
car<-read.csv('ToyotaCorolla.csv')

#selecting relevant columns
car<-select(car,c('Age_08_04', 'KM','Fuel_Type','HP','Automatic','Doors','Quarterly_Tax','Mfr_Guarantee','Guarantee_Period','Airco','Automatic_airco'
                  ,'CD_Player','Powered_Windows','Sport_Model','Tow_Bar','Price'))

#selecting numerical columns for normalization
car_num<-select(car,c('Age_08_04','KM','HP','Quarterly_Tax','Doors','Guarantee_Period','Price'))
car_dummy<-select(car,c('Fuel_Type'))
car_cat<-select(car,c('Automatic','Mfr_Guarantee','Airco','Automatic_airco','CD_Player','Powered_Windows','Sport_Model','Tow_Bar'))

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#normalizing the numerical columns
car_num <- as.data.frame(lapply(car_num, min_max_norm))
head(car_num)

#creating dummy variables for the fuel type column
library('fastDummies')

car_dummy<-dummy_cols(car_dummy)
head(car_dummy)

car_dummy<-select(car_dummy,-'Fuel_Type')
head(car_dummy)

car_new<-data.frame(car_num,car_dummy,car_cat)
head(car_new)

#partitioning data into training and validation data
set.seed(1)
rows_train_car <- sample(rownames(car_new), dim(car_new)[1]*0.6)
rows_valid_car <- sample(setdiff(rownames(car_new), rows_train_car),
                         dim(car_new)[1]*0.4)
train_df_car<-car_new[rows_train_car,];train_df_car

valid_df_car<-car_new[rows_valid_car,];valid_df_car

head(train_df_car)

head(valid_df_car)

#building nn model

#single layer 5 nodes
car_nn <- neuralnet(Price ~ .,data =train_df_car, hidden = 5, linear.output = T,lifesign = 'full',threshold = 0.3)
plot(car_nn, rep='best')

#predicting data using model on validation data
pred_price = compute(car_nn,valid_df_car,rep = 1)
pred_price$net.result

#scaling back data
multiplier <-max(car$Price) - min(car$Price);multiplier

predicted_price_scaled <- pred_price$net.result * multiplier + min(car$Price)
predicted_price_scaled

valid_car_price <- as.data.frame((valid_df_car$Price)*multiplier + min(car$Price));valid_car_price

head(valid_df_car$Price)

df_val_met1<-data.frame(predicted_price_scaled,valid_car_price)
head(df_val_met1)
err1<-df_val_met1$X.valid_df_car.Price....multiplier...min.car.Price.-df_val_met1$predicted_price_scaled
df_val_met1<-data.frame(err1,df_val_met1)
head(df_val_met1)
squared_error1<-(df_val_met1$err)^2;squared_error1

df_val_met1<-data.frame(squared_error1,df_val_met1)
head(df_val_met1)
mean(df_val_met1$squared_error1)
RMSE1_valid_1 = sqrt(mean(df_val_met1$squared_error1));RMSE1_valid_1 

#predicting data using model on training data
pred_price_train = compute(car_nn,train_df_car)
pred_price_train$net.result
pred_price_train_scaled = pred_price_train$net.result*multiplier + min(car$Price)

train_car_price <- as.data.frame((train_df_car$Price)*multiplier + min(car$Price));valid_car_price

data.frame(pred_price_train_scaled,train_car_price)
df_train_met1<-data.frame(pred_price_train_scaled,train_car_price)
head(df_train_met1)
err2<-df_train_met1$X.train_df_car.Price....multiplier...min.car.Price.-df_train_met1$pred_price_train_scaled
df_train_met1<-data.frame(err2,df_train_met1)
head(df_train_met1)
squared_error2<-(df_train_met1$err2)^2;squared_error2
df_train_met1<-data.frame(squared_error2,df_train_met1);df_train_met1
mean(df_train_met1$squared_error2)
RMSE1_train_1 = sqrt(mean(df_train_met1$squared_error2));RMSE1_train_1 


#double layer 5 nodes
#validation data
car_nn2 <- neuralnet(Price ~ .,data =train_df_car, hidden = c(5,5), linear.output = T,lifesign = 'full',threshold = 0.3)
plot(car_nn2)

#predicting data using model on validation data
pred_price_2 = compute(car_nn2,valid_df_car,rep = 1)
pred_price_2$net.result

predicted_price_scaled_2 <- pred_price_2$net.result * multiplier + min(car$Price)

data.frame(predicted_price_scaled_2,valid_car_price)
df_val_met2<-data.frame(predicted_price_scaled_2,valid_car_price)
head(df_val_met2)
err3<-df_val_met2$X.valid_df_car.Price....multiplier...min.car.Price.-df_val_met2$predicted_price_scaled_2
df_val_met2<-data.frame(err3,df_val_met2)
head(df_val_met2)
squared_error3<-(df_val_met2$err3)^2;squared_error3
df_val_met2<-data.frame(squared_error3,df_val_met2);df_val_met2
mean(df_val_met2$squared_error3)
RMSE1_val_2 = sqrt(mean(df_val_met2$squared_error3));RMSE1_val_2 



#predicting data using model on training data
pred_price_2_train = compute(car_nn2,train_df_car,rep = 1)
pred_price_2_train$net.result

predicted_price_2_train_scaled <- pred_price_2_train$net.result * multiplier + min(car$Price);predicted_price_2_train_scaled
data.frame(predicted_price_2_train_scaled,train_car_price)

data.frame(predicted_price_2_train_scaled,train_car_price)
df_train_met2<-data.frame(predicted_price_2_train_scaled,train_car_price)
head(df_train_met2)
err4<-df_train_met2$X.train_df_car.Price....multiplier...min.car.Price.- df_train_met2$predicted_price_2_train_scaled
df_train_met2<-data.frame(err4,df_train_met2)
head(df_train_met2)
squared_error4<-(df_train_met2$err4)^2;squared_error4
df_train_met2<-data.frame(squared_error4,df_train_met2);df_train_met2
mean(df_train_met2$squared_error4)
RMSE1_train_2 = sqrt(mean(df_train_met2$squared_error4));RMSE1_train_2 

#RMSE values

#single layer
RMSE1_valid_1 
RMSE1_train_1

#double layer
RMSE1_val_2 
RMSE1_train_2 

#########################Classification And Regression Tree (CART), ###########################

rm(list=ls()) #clear working environment

library(rpart); library(rpart.plot)
install.packages(c("forecast", "caret", "e1071"))

library(forecast); library(caret); library(e1071)

car.df <- read.csv("ToyotaCorolla.csv")
#select the variables

selected.var<-c(3,4,7,8,9,12,14,17,19,21,25,26,28,30,34,39)

#split the data
set.seed(1)
trainr.index <- sample(c(1:dim(car.df)[1]), dim(car.df)[1]*0.6)  
trainr.df <- car.df[trainr.index,selected.var ]
validr.df <- car.df[-trainr.index, selected.var]

# build Regression Tree using training data
library(rpart)
car.rt <- rpart(Price ~ ., data = trainr.df, cp=0.01, minsplit=1, maxdepth=30)

# plotting regression tree from training data
prp(car.rt)

# computing prediction accuracy

# errors
library(forecast)
library(ggplot2)

accuracy(predict(car.rt, trainr.df), trainr.df$Price)
accuracy(predict(car.rt, validr.df), validr.df$Price)

train.err <- predict(car.rt, trainr.df) - trainr.df$Price
valid.err <- predict(car.rt, validr.df) - validr.df$Price
err <- data.frame(Error = c(train.err, valid.err), 
                  Set = c(rep("Training", length(train.err)),
                          rep("Validation", length(valid.err))))
ggplot(err, aes(x = Set, y = Error)) + geom_boxplot()

