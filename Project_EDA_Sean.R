# Car data set from kaggle 


## read in file 

Carv3 <- read_csv("/Users/seangao/Desktop/courses/MDA_postgraduate_UWO/Statistical_modelling/Project/Car_details_v3.csv")

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






# Model comparison (linear/quadratic/quartic)
library(tidyverse)
library(caret)
train.control <- trainControl(method = "cv", number = 10) # Prepares for cv scoring

fit_0 = lm(selling_price ~ 1, data = car.tr) # intercept
fit_1 = lm(selling_price ~ ., data = car.tr) # linear
fit_2 = lm(selling_price ~ . + .^2 , data = car.tr) # quadratic 
fit_4 = lm(selling_price ~ . +.^2 +.^3 +.^4, data = car.tr)  # quartic


#summary(fit_0)[c(1,2)]
library(broom)
print(glance(fit_0)[c(1,2)])
rmse0=sqrt(mean((car.tr$selling_price-predict(fit_0, car.tr))^2))
rmse0

print(glance(fit_1)[c(1,2)])
rmse1=sqrt(mean((car.tr$selling_price-predict(fit_1, car.tr))^2))
rmse1

print(glance(fit_2)[c(1,2)])
rmse2=sqrt(mean((car.tr$selling_price-predict(fit_2, car.tr))^2))
rmse2

print(glance(fit_4)[c(1,2)])
rmse4=sqrt(mean((car.tr$selling_price-predict(fit_4, car.tr))^2))
rmse4

# The quartic model is rank-deficient, so the prediction is not reliable:
# The quadratic model has the smallest RMSE and highest Rsquared.
# We apply the quadratic model on the test data:
ypred = predict(fit_2, car.ts)

ytest = car.ts$selling_price
mse = mean((ypred-ytest)^2)
rss = sum((ypred-ytest)^2)
tss = sum((ytest-mean(ytest))^2)

rsq =1-rss/tss
rsq
# Applying on the test data R^2  = 0.898981;





