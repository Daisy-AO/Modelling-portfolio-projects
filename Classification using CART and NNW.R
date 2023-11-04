####CLASSIFICATION ANALYSIS using Classification And Regression Tree (CART),############

rm(list=ls()) #clear working environment
setwd("C:/Users/daisy/OneDrive/Desktop/6356 data")
ebay.df<-read.csv("eBayAuctions.csv") #load data

##converting variable duration into a categorical variable
ebay.df$Duration <- as.factor(ebay.df$Duration)
str(ebay.df)
levels(ebay.df$Duration)
# set.seed command used to generate same partitions every time data is sampled.

set.seed(1)

# --- Partition the data 60-40 -----------

train.index <- sample(c(1:dim(ebay.df)[1]), dim(ebay.df)[1]*0.6)  
train.df <- ebay.df[train.index, ]
valid.df <- ebay.df[-train.index, ]

set.seed(1)
library(rpart)
full_tree <- rpart(Competitive. ~ ., data = train.df, minbucket = 50, minsplit = 2, cp=0, method = "class" )
# count number of leaves
length(full_tree$frame$var[full_tree$frame$var == "<leaf>"])
# plot tree
library("rpart.plot")
rpart.plot(full_tree)
prp(full_tree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(full_tree$frame$var == "<leaf>", 'gray', 'white')) 
printcp(full_tree)

pruned.ct <- prune(full_tree, 
                   cp = full_tree$cptable[which.min(full_tree$cptable[,"xerror"]),"CP"])

length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

rpart.plot(pruned.ct)
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)

### predicting 
full_tree.point.pred.train <- predict(full_tree,train.df,type = "class")

# generate confusion matrix for training data

confusionMatrix(full_tree.point.pred.train, as.factor(train.df$Competitive.))

### Now, repeat the code for the validation set

full_tree.point.pred.valid <- predict(full_tree,valid.df,type = "class")
# generate confusion matrix for validation data
confusionMatrix(full_point.pred.valid, as.factor(valid.df$Competitive.))

#################################Neural Network classification######################
rm (list = ls())
install.packages("neuralnet")
library(neuralnet)
# require(neuralnet)

install.packages("caret")
# read Data
ebayn.df <- read.csv("ebayAuctions.csv")
View(ebayn.df)
str(ebayn.df)

##### Selecting the predictors for the model(Seller Rating, Duration, Closeprice & Openprice)
ebay_nn<-select(ebayn.df,c('sellerRating', 'Duration','ClosePrice', 'OpenPrice', 'Competitive.'))
head(ebay_nn)
#####Data Transformation for neural nets
########scaling the data
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#min-max normalization
ebay_nn[1:4] <- as.data.frame(lapply(ebay_nn[1:4], min_max_norm))
head(ebay_nn)

####Convert  output variable as a factor 
ebay_nn$Competitive.<-as.factor(ebay_nn$Competitive.)
str(ebay_nn)

#parition the dataset
set.seed(1234)
train.index <- sample(c(1:dim(ebay_nn)[1]), dim(ebay_nn)[1]*0.6)  
train.df <- ebay_nn[train.index, ]
valid.df <- ebay_nn[-train.index, ]

#######Build Neural Network 
ebay_nnw <- neuralnet(Competitive.~., data=train.df, act.fct = "logistic", linear.output = FALSE, hidden = 3, threshold = 0.1)

plot(ebay_nnw,rep='best')

# display weights
ebay_nnw$weights

# the error of the neural network model, along with the 
# weights between the inputs, hidden layers, and outputs
ebay_nnw$result.matrix
########prediction

prediction(ebay_nnw)
training.prediction<- neuralnet::compute(ebay_nnw,train.df[-5]) # remove the outcome vraible
valid.prediction <- neuralnet::compute(ebay_nnw,valid.df[-5]) 

outnnmod$net.result[,2]
 p <- outnnmod$net.result[,2]
pred <- ifelse(p>0.5,1,0)
tabl <- table(pred, valid_df_ebay_nn$Competitive.)
tabl
                
                
