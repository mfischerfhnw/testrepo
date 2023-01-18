#MarkFor Group Work-------------------------------------------------------------
#David Wolf, Manuel Fischer
#-------------------------------------------------------------------------------
#READ DATA----------------------------------------------------------------------
#install package to read excel files
install.packages("xlsx")
library("xlsx")
#read data
REV_data <- read.xlsx("Real estate valuation data set.xlsx", 1, header=TRUE)
#INVESTIGATE DATA---------------------------------------------------------------
#view dataset as table
View(REV_data)
#view structure of data
str(REV_data)
#view summary of data
summary(REV_data)
#check if dataframe contains NaN-Values
sum(is.na(REV_data))
#TRANSFORMATIONS----------------------------------------------------------------
#remvoe column "No" from dataset
REV_data$No <- NULL
REV_train <- REV_data
#MODEL1-------------------------------------------------------------------------
#create multiple linear regression model with all predictors
lm_REV <- lm(REV_train$Y ~ ., data = REV_train)
summary(lm_REV)
summary(lm_REV)$coefficient
#MODEL2-------------------------------------------------------------------------
#create multiple linear regression model without X5.longitude
REV_train2 <- REV_train
REV_train2$X6.longitude <- NULL
summary(REV_train2)
#create 2nd model
lm_REV2 <- lm(REV_train2$Y ~ ., data = REV_train2)
summary(lm_REV2)
#MODEL3-------------------------------------------------------------------------
#create multiple linear regression model without X1.transaction.date
REV_train3 <- REV_train2
REV_train3$X1.transaction.date <- NULL
summary(REV_train3)
#create 2nd model
lm_REV3 <- lm(REV_train3$Y ~ ., data = REV_train3)
summary(lm_REV3)
#EVALUATE MODEL 3---------------------------------------------------------------
#get confidence Intervals of coefficients
confint(lm_REV3)
#calculate Percentage Error
(sigma(lm_REV3)/mean(REV_train3$Y)) * 100
#TEST MODEL ASSUMPTIONS---------------------------------------------------------
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(lm_REV3)  # Plot the model information

