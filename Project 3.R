library(ggplot2)
library(gridExtra)
library(cowplot)
library(datasets)
library(boot)
library(bootstrap)
library(dplyr)
library(tidyr)
library(data.table)
library(magrittr)
library(latex2exp)
library(Ecdat)
library(caret)
library(RColorBrewer)
library(DAAG)

################## Question 1 ################## 

# task1
data("chickwts")
head(chickwts)
chickwts %<>% data.table()
summary(chickwts)

# ggplot(chickwts, aes(x=feed, y=weight))+
#   geom_boxplot()
# ggplot(chickwts, aes(x= weight, y=weight, color=feed))+
#   geom_point()

chickwts[, list(Mean=mean(weight),
                StdDev=sd(weight),
                Min=min(weight),
                Max=max(weight),
                .N), by=feed]


# Is there a difference between the chicks weights 
# across the diet groups?

# 1 -- ANOVA
# H_0: There is no difference in the mean weights
# of chicks across the feed groups.
# H_A: At least one feed group has a different
# mean weight.
model.fit <- lm(weight ~ feed, data = chickwts)
summary(model.fit)
anova.fit <- anova(model.fit)
F_stat_observed<- anova.fit$`F value`[1]
p_val_observed <- anova.fit$`Pr(>F)`[1]

X <- model.matrix(model.fit)
beta <- coef <- model.fit$coefficients
res <- model.fit$residuals

# task2
SemiParametricBootstrap<- function(N=1000) {
  
  lm.0 <- lm(weight ~ 1, data = chickwts)
  e <- lm.0$residuals
  B <- lm.0$coefficients
  X <- lm.0 %>% model.matrix()
  
  replicate(N, {
    e.boot <- sample(e, replace=T);
    y.boot <- c(B %*% t(X) + e.boot);
    anova.boot<- lm(y.boot ~ chickwts$feed) %>% anova();
    anova.boot$`F value`[1]
  })  -> F_stat.boot
  
  (sum(F_stat.boot>= F_stat_observed)+1)/(N+1) -> boot_test.result
  return(boot_test.result)
}

bootstrap_result <- SemiParametricBootstrap(1000)
bootstrap_result
# distribution of p-value
# hist(bootstrap_result$`p-value`)
# p_values - nearest 0, H_A is true, I guess
#(1+sum(bootstrap_result$`F value` >= F_stat_observed))/ (1000+1)

# task 3 -- PERMUTATION TEST
inner_f3<- function(data){
  permuted_data<- data
  permuted_data$feed <- sample(permuted_data$feed, replace = FALSE)  # Shuffle feed labels
  anova.fit <- aov(weight ~ feed, data = permuted_data)
  permuted_F <- summary(anova.fit)[[1]]$`F value`[1]
  permuted_p<- summary(anova.fit)[[1]]$`Pr(>F)`[1]
  return(c(`F value`=permuted_F,
           `p-value`=permuted_p))
}

permutation_test <- function(data,
                             N = 1000) {
  replicate(N, inner_f3(data), simplify = T) %>% t() %>% as.data.frame() -> df
  p<- (1+sum(df[1]>F_stat_observed))/(N+1)
  return(list(df=df,
              `p-value`=p))
  #p-value 
  # return(mean(permuted_F >= observed_F))
}

set.seed(42)
test <- permutation_test(chickwts)
p_value_permuationt <- test$`p-value`
p_value_permuationt

# task 4 --- CI with parametric bootstrap 
# mle of mu and sigma

K<- 1000
set.seed(42)

mu_sun<- chickwts[feed=="sunflower", .(mean(weight))]%>% unlist()
sd_sun<- chickwts[feed=="sunflower", .(sd(weight))]%>% unlist()
n_sun<- chickwts[feed=="sunflower", .N]%>% unlist()

mu_soy<- chickwts[feed=="soybean", .(mean(weight))]%>% unlist()
sd_soy<- chickwts[feed=="soybean", .(sd(weight))]%>% unlist()
n_soy<- chickwts[feed=="soybean", .N]%>% unlist()

theta_observed <- mu_sun - mu_soy

mu_sun_boot <- replicate(K, rnorm(n_sun,
                                  mu_sun,
                                  sd_sun)) %>% colMeans()
mu_soy_boot <- replicate(K, rnorm(n_soy,
                                  mu_soy,
                                  sd_soy)) %>% colMeans()

theta.boot_sample <- mu_sun_boot - mu_soy_boot
theta.boot_sample%>% hist()
theta.boot_sample %>% mean() -> theta_hat
theta_hat - qt(0.95, 24) * sqrt( sd_sun^2/n_sun + sd_soy^2/n_soy )
theta_hat + qt(0.95, 24) * sqrt( sd_sun^2/n_sun + sd_soy^2/n_soy )
# 90% C.I
beta_hat <- mean(theta.boot_sample-theta_hat)
nu_hat <- var(theta.boot_sample)

theta_hat - beta_hat - qnorm(0.95) * sqrt(nu_hat)
theta_hat - beta_hat + qnorm(0.95) * sqrt(nu_hat)

theta.boot_sample %>% data.frame(V1=.) %>% 
  ggplot(aes(x=V1))+
  geom_histogram(aes(y=..density..), color="black", fill="green")+
  xlab(TeX("\\hat{\\theta}"))+
  theme_bw()+
  theme(aspect.ratio = 1)

















################## Question 2 ################## 

data("Computers") 
names(Computers) 
head(Computers)

# task 1
model.fit<- lm(price ~ hd, data=Computers)
summary(model.fit)
coef(model.fit)

# task 2
price_predicted <- predict(model.fit)
MSE <- mean((price_predicted-Computers$price)^2)
RMSE <- sqrt(MSE)

# task3
n<- dim(Computers)[1]
set.seed(42)
folds <- createFolds(Computers$price, k=10)
cv.price_predicted <- numeric(n)
for (I in folds) {
  m<- lm(price~hd, data=Computers[-I,])
  cv.price_predicted[I]<- predict(m, Computers[I, ])
}
MSE.cv<- mean((cv.price_predicted-Computers$price)^2)
RMSE.cv <- sqrt(MSE.cv)

# task 4
sapply(1:n, function(i){
  m<- lm(price~hd, Computers[-i,])
  coef(m)[2]
} ) -> beta_1.loo_cv

beta_1.loo_cv %<>% data.frame(b=., x=Computers$price)

ggplot(beta_1.loo_cv, aes(y=b, x=x))+
  geom_point(shape=1, col="blue", alpha=0.5)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  geom_hline(yintercept=coef(model.fit)[2])+
  ylab(TeX("$\\hat{\\beta}_1$"))+
  xlab("price")

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
  geom_point(aes(y=price), shape=1, col="green", alpha=0.3)+
  geom_path((aes(y=`2.5%`)), color="blue")+
  geom_path(aes(y=`97.5%`), color="blue")+
  geom_path(aes(y=price_predicted), color="red")+
  theme_bw()


################## Question 3 ################## 

# task 1
set.seed(42)
sapply(1:N, function(i){
  id<- sample(1:n, n, replace=TRUE)
  m<- lm(price~hd, data = Computers[id,])
  summary(m)->s
  s$coefficients[,2]
}, simplify = T) %>% t() %>% as.data.frame() -> beta_se.boot

quantile(beta_se.boot$`(Intercept)`, probs=c(0.025,0.975)) 
quantile(beta_se.boot$hd, probs=c(0.025,0.975))


# task 2

comp_hd_leq2000 <- Computers[Computers$hd<2000, ]
m<- lm(price~hd, data = comp_hd_leq2000)
summary(m) -> s
s$coefficients

n_new <- dim(comp_hd_leq2000)[1]
set.seed(42)
sapply(1:N, function(i){
  id<- sample(1:n_new, n_new, replace=TRUE)
  m<- lm(price~hd, data = comp_hd_leq2000[id,])
  summary(m)->s
  c(s$coefficients[,1],  s$coefficients[,2])
}, simplify = T) %>% t() %>% as.data.frame() -> coef_new

coef_new[,1:2] %>% colMeans()

coef_new[,3:4]-> beta_se_new.boot

quantile(beta_se_new.boot$`(Intercept)`, probs=c(0.025,0.975)) 
quantile(beta_se_new.boot$hd, probs=c(0.025,0.975))


cbind(beta_se.boot, task=rep("Q3.1"))->a
cbind(beta_se_new.boot, task=rep("Q3.2"))->b
df<- rbind(a,b)

leg <- get_legend(ggplot(df, aes(x=`(Intercept)`, fill = task))+
                    geom_histogram( alpha=0.4, position = "identity", col="black")+
                    theme_bw()+
                    xlab(TeX("SE(\\hat{\\beta}\\_0)"))+
                    scale_x_continuous(limits = c(12,13.2),
                                       breaks = seq(12,13.2, by=0.25))+
                    theme(legend.position = "bottom"))

ggplot(df, aes(x=`(Intercept)`, fill = task))+
  geom_histogram( alpha=0.4, position = "identity", col="black")+
  theme_bw()+
  xlab(TeX("SE(\\hat{\\beta}\\_0)"))+
  scale_x_continuous(limits = c(12,13.2),
                     breaks = seq(12,13.2, by=0.25))+
  theme(legend.position = "none")->p1

ggplot(df, aes(x=`(Intercept)`, fill = task))+
  geom_boxplot(alpha=0.4)+
  theme_bw()+
  xlab(TeX("SE(\\hat{\\beta}\\_0)"))+
  scale_x_continuous(limits = c(12,13.2),
                     breaks = seq(12,13.2, by=0.25))+
  scale_y_continuous(breaks=NULL)+
  theme(legend.position = "none")-> p2

ggplot(df, aes(x=hd, fill = task))+
  geom_histogram( alpha=0.4, position = "identity", col="black")+
  theme_bw()+
  xlab(TeX("SE(\\hat{\\beta}\\_1)"))+
  scale_x_continuous(limits = c(0.0235,0.0282),
                     breaks = seq(0.025,0.027, by=0.001))+
  theme(legend.position = "none") ->p3

ggplot(df, aes(x=hd, fill = task))+
  geom_boxplot(alpha=0.4)+
  theme_bw()+
  xlab(TeX("SE(\\hat{\\beta}\\_1)"))+
  scale_x_continuous(limits = c(0.0235,0.0282),
                     breaks = seq(0.025,0.027, by=0.001))+
  scale_y_continuous(breaks=NULL)+
  theme(legend.position = "none") -> p4

plot_grid(p1,p2,ncol=1, align="v",
          rel_heights = c(3,2,0.5)) %>% 
  plot_grid(., plot_grid(p3, p4,ncol=1, align="v", rel_heights = c(3,2,0.5)) + theme(legend.position = "bottom"), ncol=2)































################## Question 4 ################## 

B<- 1000
x<-c(0.68446806,-0.02596037,-0.90015774,0.72892605,-0.45612255, 0.19311847,
     -0.13297109, -0.99845382, 0.37278006, -0.20371894, -0.15468803, 0.19298230
     , -0.42755534, -0.04704525, 0.15273726, 0.03655799, 0.01315016, -0.59121428,
     4.50955771, 2.87272653)
# task 1
vec_mean = mean(x)
vec_median = median(x)

# task 2

n <- length(x)

boot.means <- replicate(B, mean(sample(x, n, replace = TRUE)))
boot.medians <- replicate(B, median(sample(x, n, replace = TRUE)))

boot.means.df <- data.frame(x = boot.means)
boot.medians.df <- data.frame(x = boot.medians)

mean_plot <- ggplot(boot.means.df, aes(x = x)) +  
  geom_histogram(binwidth = 0.05, fill = 'dodgerblue', alpha = 0.7) +
  labs(title = 'Bootstrap mean',
       x = 'value') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

median_plot <- ggplot(boot.medians.df, aes(x = x)) +  
  geom_histogram(binwidth = 0.06, fill = 'darkgreen', alpha = 0.7) +
  labs(title = 'Bootstrap median',
       x = 'value') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(mean_plot, median_plot, ncol = 2)

# task 3

mean.boot <- rep(0, B)
median.boot <- rep(0, B)

residual <- x - vec_mean

for (i in 1:B){
  residual.boot <- sample(residual, size = n, replace = TRUE)
  x.boot <- vec_mean + residual.boot
  
  mean.boot[i] <- mean(x.boot)
  median.boot[i] <- median(x.boot)
}

se_mean <- sd(mean.boot); se_median <- sd(median.boot)

ci_mean <- quantile(mean.boot, c(0.025, 0.975))
ci_median <- quantile(median.boot, c(0.025, 0.975))


# task 4

mean.jackknife <- rep(0, B)
median.jackknife <- rep(0, B)

for (i in 1:n){
  x.jack <- x[- c(i)]
  mean.jackknife[i] <- mean(x.jack)
  median.jackknife[i] <- median(x.jack)
}

mean.jackknife.est <- mean(mean.jackknife)
median.jackknife.est <- mean(median.jackknife)

mean.bias <- (n - 1) * (mean.jackknife.est - vec_mean)
median.bias <- (n - 1) * (median.jackknife.est - vec_mean)

mean.var <- (n - 1) / n * sum((mean.jackknife - mean.jackknife.est)^2)
median.var <- (n - 1) / n * sum((median.jackknife - median.jackknife.est)^2)

mean.mse <- mean.var + mean.bias^2
median.mse <- median.var + median.bias^2


# task 5

bootstrap_medians <- rep(0, B)
bootstrap_M0 = rep(0, B)

for (j in 1:B){
  for (i in 1:B) {
    bootstrap_medians[i] <- mean(median(sample(x, size = n, replace = TRUE)) < 0)
  }
  bootstrap_M0[j] <- mean(bootstrap_medians)
}

pi_hat <- mean(bootstrap_M0)

ci <- quantile(bootstrap_M0, probs = c(0.025, 0.975))

pi_df <- data.frame(x = bootstrap_M0)

ggplot(pi_df, aes(x = x)) +  
  geom_histogram(binwidth = 0.002, fill = 'dodgerblue', alpha = 0.7) +
  geom_vline(xintercept = ci[[1]], color = 'red') +
  geom_vline(xintercept = ci[[2]], color = 'red') + 
  geom_vline(xintercept = pi_hat, color = 'darkgreen') + 
  labs(title = 'M > 0 bootstrap',
       x = 'value') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
