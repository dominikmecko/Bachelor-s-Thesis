require(ggplot2)
require(GGally)
require(reshape2)
require(lme4)
require(compiler)
require(parallel)
require(boot)
require(lattice)


data <- read.csv("/Users/dominikmecko/Work/Bachelor's Thesis/Data/final_cleaned.csv")

library(survival)


#####FINAL MODEL########
model_clogit <- clogit(value ~ Real.Price+ All.RES + wind + solar + hydro + biomass + Slovak + Can.Change + No.change + strata(ResponseId) + strata(SetID_1), data = data)
summary(model_clogit)
model_clogit$coefficients
########################

install.packages('glmnet')

#####TEST MODELS DO NOT RUN#####
diff_model_clogit <- clogit(value.diff ~ Real.Price.diff+ All.RES.diff + wind.diff + solar.diff + hydro.diff + biomass.diff + Slovak.diff + Can.Change.diff + No.change.diff + strata(ResponseId)+ strata(SetID_1), data = new_data1)
summary(diff_model_clogit)
diff_model_clogit$coefficients

diff_model_logit <- glm(value.diff ~ Real.Price.diff+ All.RES.diff + wind.diff + solar.diff + hydro.diff + biomass.diff + Slovak.diff + Can.Change.diff + No.change.diff, data = new_data1, family = "binomial")
diff_model_logit$coefficients
summary(diff_model_logit)

logit_new <- glm(value ~ Price.Cont+ All.RES + wind + solar + hydro + biomass + Slovak + Can.Change + No.change, data = data, family="binomial")
logit_new$coefficients
#####################

#####LASSO
library(tidyverse)
library(caret)
library(glmnet)

new_data <- read.csv("/Users/dominikmecko/Work/Bachelor's Thesis/Data/final_interacted_2.csv")
col_x <- colnames(new_data)[5:ncol(new_data)]
col_y <- colnames(new_data)[4:4]

x1 <- subset(new_data, select=col_x)
y1 <- subset(new_data, select=col_y)

is.matrix(y1)

x1 <- data.matrix(x1)
y1 <- data.matrix(y1)

# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(x1, y1, alpha = 1, family = "binomial")
# Fit the final model on the training data
model <- glmnet(x1, y1, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model)
summary(model)


###########



elasticity <- read.csv(file="/Users/dominikmecko/Work/Bachelor's Thesis/Data/elasticity1.csv",header = T)
elasticity1 <- t(elasticity)

elasticity1 <- data.frame(elasticity1)

elastic_model <- lm(log(Quantity) ~ log(Price), data=elasticity1)
summary(elastic_model)
confint(elastic_model)


scatter.smooth(elasticity1$Price,elasticity1$Quantity)
summary(elasticity1)
sd(elasticity1$Quantity)
