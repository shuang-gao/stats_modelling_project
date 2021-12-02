# Car data set from kaggle 


## read in file 
library(readr)
Carv3 = read_csv("/Users/seangao/Desktop/courses/MDA_postgraduate_UWO/Statistical_modelling/Project/Car_details_v3.csv")

## EDA

#check dimensions of the data set 
library(tidyverse)
dim(Carv3)
head(Carv3)


############################data cleaning 
#checking for null value 
table(is.na(Carv3))

#remove null values 
carsv3 = na.omit(Carv3)

#check for dim of carsv3
dim(carsv3)

#lets check which columns are of interests for our study 
#we only want to find the physical characteristics of cars that determines the selling price 
#hence we do not remove things such as brand and car models that has a economical factors to it 

head(carsv3)
carsv3 = carsv3[,-1]
dim(carsv3)
head(carsv3)

#take the first number set of torque, engine, mileage max power
for (i in 1:length(carsv3$torque)) {
  carsv3$torque[i] = str_extract_all(carsv3$torque, "\\d+\\.*+\\d*+")[[i]][1]
  carsv3$mileage[i] = str_extract_all(carsv3$mileage, "\\d+\\.*+\\d*+")[[i]]
  carsv3$engine[i] = str_extract_all(carsv3$engine, "\\d+\\.*+\\d*+")[[i]]
  carsv3$max_power[i] = str_extract_all(carsv3$max_power, "\\d+\\.*+\\d*+")[[i]]
}

dim(carsv3)

head(carsv3)

#Lets convert the selling price from rupees to CAD 
carsv3$selling_price = carsv3$selling_price * 0.017

#data save point 
cc = carsv3
#fail save
carsv3 = cc
#convert all numbers variable into numeric values 
carsv3[,8:12] = sapply(carsv3[,8:12],as.numeric)
head(carsv3)


#convert some variables into factors 
#carsv3$year = as.character(carsv3$year)
carsv3[sapply(carsv3, is.character)] <- lapply(carsv3[sapply(carsv3, is.character)],as.factor)
#carsv3[,4:7] = sapply(carsv3[,4:7],factor)
levels(as.factor(carsv3$owner))
str(carsv3)
sapply(carsv3[,c(4:7)], levels)

#Check all levels
#s= sapply(Carv3, is.character)
#d= sapply(Carv3, is.numeric)
#Carv3_chr = Carv3[,s]
#Carv3_nonchr = Carv3[,d]
#Carv3_nonchr %>% str()
#Carv3_chr
#sapply(Carv3_chr,unique)

#The pairwise panel graph of numerical features/predictors 
library(psych)
pairs.panels(carsv3[,-4:-7], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


pairs(carsv3$selling_price ~., carsv3 , main="Scatterplot matrix")

# lets split the data in to training and testing 
set.seed(10)
n = nrow( carsv3 )
idx = sample(n, n*0.6, replace = F) 
car.tr = carsv3[idx,]
car.ts = carsv3[-idx,]

dim(carsv3);dim(car.tr);dim(car.ts)

# lets fit a linear regression 
lm1 = lm(selling_price~. ,data = car.tr)
summary(lm1)

#lets check model diagnostics 
par(mfrow=c(2,2))
plot(lm1)

lm1$residuals
library(lmtest)

bptest(lm1);
shapiro.test(lm1$residuals)
#test confirms that EV is violated and Normality is violated


#lets try removing the outliers  points 
# The obs that are influential point
influ=which(cooks.distance(lm1) > 4/length(cooks.distance(lm1)))
influ
outliers= which((abs(rstandard(lm1)) > 2) & (cooks.distance(lm1) > 4 / length(cooks.distance(lm1))))
outliers
#remove the outliers 
car.nout = carsv3[-outliers,]
n = nrow( car.nout )
idx = sample(n, n*0.6, replace = F) 
car.trO = car.nout[idx,]
car.tsO = car.nout[-idx,]
dim(car.nout);dim(car.trO);dim(car.tsO)
#refiit model 1 
# lets fit a linear regression 
lm2 = lm(selling_price~. ,data = car.tr)
summary(lm2)

#lets check model diagnostics 
par(mfrow=c(2,2))
plot(lm2)
lm1$residuals
library(lmtest)

bptest(lm2);
shapiro.test(lm2$residuals)
#test confirms that EV is violated and Normality is violated


#lets do some boxcox transfomation to see if the model assumption is corrected 
library(MASS)
boxcox(lm1,lambda = seq(-0.05, 0.01, by = 0.001))


lambda = -0.005
lm3 <- lm(((car.tr$selling_price^(lambda)-1)/(lambda)) ~ ., data = car.tr)
summary(lm3)
par(mfrow=c(2,2))
plot(lm3)


bptest(lm3);
shapiro.test(lm3$residuals)
#test confirms that EV is violated and Normality is violated



# More tests: quadratic & quartic:
fit_2 = lm(selling_price ~ . + I(selling_price^2) + 
                I(km_driven^2) + I(mileage^2) +I(engine^2) +.^2  , data = car.tr) # quadratic 

fit_4 = lm(selling_price ~ . + I(selling_price^2) + 
                I(km_driven^2) + I(mileage^2) +I(engine^2)+.^2+ I(selling_price^3) + 
                I(km_driven^3) + I(mileage^3) +I(engine^3)+.^3+I(selling_price^4) + 
                I(km_driven^4) + I(mileage^4) +I(engine^4)+.^4 , data = car.tr)  # quartic


par(mfrow=c(2,2))
plot(fit_2)
fit_2$residuals
library(lmtest)
bptest(fit_2)
shapiro.test(fit_2$residuals)


par(mfrow=c(2,2))
plot(fit_4)
fit_2$residuals
bptest(fit_4);
shapiro.test(fit_4$residuals)



# Model comparison (linear/quadratic/quartic)
library(tidyverse)
library(caret)
train.control <- trainControl(method = "cv", number = 10) # Prepares for cv scoring

fit_0 = lm(selling_price ~ 1, data = car.tr) # intercept
fit_1 = train(selling_price ~ ., data = car.tr,method = "lm",trControl = train.control) # linear
fit_2 = train(selling_price ~ . + I(selling_price^2) + 
                I(km_driven^2) + I(mileage^2) +I(engine^2)+ .^2 , data = car.tr,method = "lm",
              trControl = train.control) # quadratic 

fit_4 = train(selling_price ~ . + I(selling_price^2) + 
                I(km_driven^2) + I(mileage^2) +I(engine^2)+ I(selling_price^3) + 
                I(km_driven^3) + I(mileage^3) +I(engine^3) + I(selling_price^4) + 
                I(km_driven^4) + I(mileage^4) +I(engine^4) , data = car.tr,
              method = "lm",trControl = train.control)  # quartic


print(fit_0)
print(fit_1)
print(fit_2)
print(fit_4)


# The quadratic model has the smallest RMSE and highest Rsquared.
# We apply the quadratic model on the test data:
ypred = predict(fit_2, car.ts)

ytest = car.ts$selling_price
mse = mean((ypred-ytest)^2)
rss = sum((ypred-ytest)^2)
tss = sum((ytest-mean(ytest))^2)

rsq =1-rss/tss
rsq

# Applying on the test data R^2  = 0.9720924;

#RIDGE, LASSO, ELASTIC NET MODELS
install.packages("glmnet", dependencies=TRUE)
library(glmnet)

install.packages("caret", dependencies = TRUE)
library(caret)

#creates ridge model
ridge = train(
  selling_price ~ . + I(selling_price^2) + 
    I(km_driven^2) + I(mileage^2) +I(engine^2)+ .^2, car.tr, method = "glmnet",
  trControl = trainControl("cv", number = 20),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)

#coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)

#predictions
pred = ridge %>% predict(car.ts)

#model performance metrics
data.frame(
  RMSE = RMSE(pred, car.ts$selling_price),
  Rsquare = R2(pred, car.ts$selling_price)
)

#create lasso model
lasso = train(
  selling_price ~ . + I(selling_price^2) + 
    I(km_driven^2) + I(mileage^2) +I(engine^2)+ .^2 , car.tr, method = "glmnet",
  trControl = trainControl("cv", number = 20),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

#coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)

#predictions
pred = lasso %>% predict(car.ts)

#model performance metrics
data.frame(
  RMSE = RMSE(pred, car.ts$selling_price),
  Rsquare = R2(pred, car.ts$selling_price)
)

#create elastic net model
elastic = train(
  selling_price ~ . + I(selling_price^2) + 
    I(km_driven^2) + I(mileage^2) +I(engine^2)+ .^2,car.tr, method = "glmnet",
  trControl = trainControl("cv", number = 20),
  tuneLength = 20
)

#coefficients
coef(elastic$finalModel, elastic$bestTune$lambda)

#predictions
pred = elastic %>% predict(car.ts)

#model performance metrics
data.frame(
  RMSE = RMSE(pred, car.ts$selling_price),
  Rsquare = R2(pred, car.ts$selling_price)
)

#compare the models 
#look at median values
models = list(ridge = ridge, lasso = lasso, elastic = elastic)
resamples(models) %>% summary( metric = "RMSE")


#STEP
# backward selection with AIC
# Note that this function uses either AIC or BIC (default: AIC where k=2) 
fit_2 = lm(selling_price ~ . + I(selling_price^2) + 
             I(km_driven^2) + I(mileage^2) +I(engine^2) +.^2  , data = car.tr) # quadratic 

fit_backwardaic = step(fit_2,direction="backward")
length(fit_backwardaic$coefficients) # = #(beta)
summary(fit_backwardaic)

# Result based on BIC
# Change k (coefficient of the penalty term) to log(n) (default was k=2 (AIC))
fit_back_bic = step(fit_2, direction = "backward", k=log(n))
fit_back_bic
summary(fit_back_bic)
length(fit_back_bic$coefficients) # = #(beta)

#prediction for AIC backward
pred_backward = predict(fit_backward, newdata=carsv3[-idx,])
mse_backward = mean((pred_backward-ytest)^2) # mse for the test data
mse_backward 

#prediction for BIC backward
pred_backwardbic = predict(fit_back_bic, newdata=carsv3[-idx,])
mse_backwardbic = mean((pred_backwardbic-ytest)^2) # mse for the test data
mse_backwardbic

#anova tests
anova(fit_backwardaic,fit_back_bic)
anova(fit_back_bic)
anova(fit_backwardaic)


