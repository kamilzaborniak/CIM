# PROJECT 3

##### QUESTION 2 ##### 

library(ggplot2)
library(datasets)
library(boot)
library(bootstrap)
library(dplyr)
library(data.table)
library(magrittr)
library(Rdpack)
library(caret)

library(Ecdat)
data("Computers") 
names(Computers) 

head(Computers)


# task 1
model.fit<- lm(price ~ hd, data=Computers)
summary(model.fit)

# task 2
price_predicted <- predict(model.fit)
MSE <- mean((price_predicted-Computers$price)^2)
RMSE <- sqrt(MSE)

# task3
n<- dim(Computers)[1]
folds <- createFolds(Computers$price, k=10)
cv.price_predicted <- numeric(n)
for (I in folds) {
  m<- lm(price~hd, data=Computers[-I,])
  cv.price_predicted[I]<- predict(m, Computers[I, ])
}
MSE.cv<- mean((cv.price_predicted-Computers$price)^2)
RMSE.cv <- sqrt(MSE.cv)

??createFolds

# task 4
i<-2
sapply(1:n, function(i){
  m<- lm(price~hd, Computers[-i,])
  coef(m)[2]
} ) -> beta_1.loo_cv
hist(beta_1.loo_cv)
boxplot(beta_1.loo_cv)

# task 5
N<- 1000
sapply(1:N, function(i){
  id<- sample(1:n, n, replace=TRUE)
  m<- lm(price~hd, data = Computers[id,])
  predict(m, newdata = Computers)
}, simplify = T) -> price.boot

sapply(1:n, function(i){
  quantile(price.boot[i,], probs = c(.025, .975))
}, simplify = T) %>% t() %>% cbind( Computers[, c("price", "hd")]) -> boot_result

ggplot(boot_result, aes(x=hd, y=price))+
  geom_point(aes(y=price), shape=1, col="dodgerblue", alpha=0.5)+
  geom_path((aes(y=`2.5%`)), col="blue")+
  geom_path(aes(y=`97.5%`), col="blue")+
  geom_path(aes(y=price_predicted), col="red")


#### Q3

#task 1
sapply(1:N, function(i){
  id<- sample(1:n, n, replace=TRUE)
  m<- lm(price~hd, data = Computers[id,])
  summary(m)->s
  s$coefficients[,2]
  
}, simplify = T) 
quantile()



