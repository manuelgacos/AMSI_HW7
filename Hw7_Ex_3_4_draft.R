library(BioStatR)
library(olsrr)

# Set Working directory
setwd('/home/noble_mannu/Documents/PhD/First/STAT_2131_Applied_Statistical_Methods_I/HW7')

# Read data
Data <- data.frame(read.table(file = "Boston.txt", header = T, sep = "\t", stringsAsFactors = F))
Data <- Data[,!(colnames(Data)%in%c("LSTAT","b"))]

# Function for creating the scatterplot matrix
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

# Plotting Scatterplot matrix
pairs(~mvalue + indus + chas + nox + rooms + age + distance + radial + tax + pt, data=Data,
      main="Scatterplot Matrix")
pairs(~mvalue + crim + nox + rooms + distance + tax + pt, data=Data,
      main="Scatterplot Matrix", diag.panel = panel.hist)


### Exercise 3 Homework 7 ###

# Make the multilinear regression model
linearMod <- lm(mvalue ~ ., data=Data)

# Display summary of our model
summary(linearMod)

# PLot the response against X1
plot(Data$crim, Data$mvalue, xlab = "Per capita crime rate by town", ylab = "mvalue", 
     main = 'Scatterplot against crim')
# PLot the response against X2
plot(Data$zn, Data$mvalue, xlab = "proportion of residential land zoned for lots over 25,000 sq.ft", ylab = "mvalue", 
     main = 'Scatterplot against zn')
# PLot the response against X3
plot(Data$indus, Data$mvalue, xlab = "proportion of non-retail business acres per town", ylab = "mvalue", 
     main = 'Scatterplot against indus')
# PLot the response against X4
plot(Data$chas, Data$mvalue, xlab = "Charles River factor variable", ylab = "mvalue", 
     main = 'Scatterplot against chas')
# PLot the response against X5
plot(Data$nox, Data$mvalue, xlab = "nitric oxide concentration", ylab = "mvalue", 
     main = 'Scatterplot against nox')
# PLot the response against X6
plot(Data$rooms, Data$mvalue, xlab = "average number of rooms per dwelling", ylab = "mvalue", 
     main = 'Scatterplot against rooms')
# PLot the response against X7
plot(Data$age, Data$mvalue, xlab = "proportion of owner-occupied units built prior to 1940", ylab = "mvalue", 
     main = 'Scatterplot against age')
# PLot the response against X8
plot(Data$distance, Data$mvalue, xlab = "weighted distances to five Boston employment centers", ylab = "mvalue", 
     main = 'Scatterplot against distance')
# PLot the response against X9
plot(Data$radial, Data$mvalue, xlab = "index of accessibility to highways", ylab = "mvalue", 
     main = 'Scatterplot against radial')
# PLot the response against X10
plot(Data$tax, Data$mvalue, xlab = "full-value property-tax rate per $10,000", ylab = "mvalue", 
     main = 'Scatterplot against tax')
# PLot the response against X11
plot(Data$pt, Data$mvalue, xlab = "pupil-teacher ratio by town", ylab = "mvalue", 
     main = 'Scatterplot against pt')

# PLot the residuals against X1
plot(Data$crim, resid(linearMod), xlab = "Per capita crime rate by town", ylab = "Residuals", 
     main = 'Residual plot against crim', col = 'blue')
abline(a=0, b=0)
# PLot the residuals against X2
plot(Data$zn, resid(linearMod), xlab = "proportion of residential land zoned for lots over 25,000 sq.ft", ylab = "Residuals", 
     main = 'Residual plot against zn', col = 'blue')
abline(a=0, b=0)
# PLot the residuals against X3
plot(Data$indus, resid(linearMod), xlab = "proportion of non-retail business acres per town", ylab = "Residuals", 
     main = 'Residual plot against indus', col = 'blue')
abline(a=0, b=0)
# PLot the residuals against X4
plot(Data$chas, resid(linearMod), xlab = "Charles River factor variable", ylab = "Residuals", 
     main = 'Residual plot against chas', col = 'blue')
abline(a=0, b=0)
# PLot the residuals against X5
plot(Data$nox, resid(linearMod), xlab = "nitric oxide concentration", ylab = "Residuals", 
     main = 'Residual plot against nox', col = 'blue')
abline(a=0, b=0)
# PLot the residuals against X6
plot(Data$rooms, resid(linearMod), xlab = "average number of rooms per dwelling", ylab = "Residuals", 
     main = 'Residual plot against rooms', col = 'blue')
abline(a=0, b=0)
# PLot the residuals against X7
plot(Data$age, resid(linearMod), xlab = "proportion of owner-occupied units built prior to 1940", ylab = "Residuals", 
     main = 'Residual plot against age', col = 'blue')
abline(a=0, b=0)
# PLot the residuals against X8
plot(Data$distance, resid(linearMod), xlab = "weighted distances to five Boston employment centers", ylab = "Residuals", 
     main = 'Residual plot against distance', col = 'blue')
abline(a=0, b=0)
# PLot the residuals against X9
plot(Data$radial, resid(linearMod), xlab = "index of accessibility to highways", ylab = "Residuals", 
     main = 'Residual plot against radial', col = 'blue')
abline(a=0, b=0)
# PLot the residuals against X10
plot(Data$tax, resid(linearMod), xlab = "full-value property-tax rate per $10,000", ylab = "Residuals", 
     main = 'Residual plot against tax', col = 'blue')
abline(a=0, b=0)
# PLot the residuals against X11
plot(Data$pt, resid(linearMod), xlab = "pupil-teacher ratio by town", ylab = "Residuals", 
     main = 'Residual plot against pt', col = 'blue')
abline(a=0, b=0)

plot(predict(linearMod), residuals(linearMod))
qqnorm(residuals(linearMod)); qqline(residuals(linearMod), col="red")

## Fitting the full model and a proposed model ##
# Make the full model seen in class
linearMod <- lm(mvalue ~ ., data=Data)
# Make the improved model seen in class
linearMod1 <- lm(mvalue ~ crim+zn+chas+nox+rooms+age+distance+radial+tax+pt, data=Data)
# Make the proposed model (Based on residual plots and scatterplots)
test <- lm(mvalue ~ .+I(indus^2)+I(crim^2)+I(distance^2), data=Data)
# Later we obtain a submodel of the proposed model

## Running best subset selection for both models ##
# Run best subset selection for the original full model. This may take a minute or two...
Best.subset <- olsrr::ols_step_best_subset(linearMod)
# Run best subset selection for the proposed model. This took my computer about 20-30 mins...
Best.subset.test <- olsrr::ols_step_best_subset(test)

## Comparing models Using R2 and adjusted R2 ##

# Choosing the model based on R2 (Original model)
which.max(Best.subset$rsquare)
# Returns row 11, this corresponds to the model with all predictors
# Prints the names of the predictors used in the best model with R2 criteria
Best.subset$predictors[which.max(Best.subset$rsquare)]
# Choosing the model based on adjusted R2 (Original model)
which.max(Best.subset$adjr)
# Returns row 10, this corresponds to the model with all predictors except indus
# Prints the names of the predictors used in the best model with adjusted R2 criteria
Best.subset$predictors[which.max(Best.subset$adjr)]

### Decide about this code ###
# Printing which predictors where left out while using adjusted R2  (Original model)
predictors.include <- strsplit(Best.subset$predictors[which.max(Best.subset$adjr)], "[ ]+", perl=T)[[1]]
colnames(Data)[!(colnames(Data)%in%predictors.include)] # Doesn't include indus
# We get a model that includes all predictors on the Original full model except for indus
### Decide about this code ###

# Choosing the model based on R2 (Proposed model)
which.max(Best.subset.test$rsquare)
# Returns row 14, this corresponds to the model with all predictors
# Prints the names of the predictors used in the best model (Proposed model) with R2 criteria
Best.subset.test$predictors[which.max(Best.subset.test$rsquare)]
# Choosing the model based on adjusted R2 (Original model)
which.max(Best.subset.test$adjr)
# Returns row 13, this corresponds to the model with all predictors except zn
# Prints the names of the predictors used in the best model (Proposed model) with adjusted R2 criteria
Best.subset.test$predictors[which.max(Best.subset.test$adjr)]

# Choosing the model based on adjusted R2 (Proposed model)
which.max(Best.subset.test$rsquare)
which.max(Best.subset.test$adjr)
# Printing which predictors where left out (Proposed model)
predictors.include.test <- strsplit(Best.subset.test$predictors[which.max(Best.subset.test$adjr)], "[ ]+", perl=T)[[1]]
colnames(Data)[!(colnames(Data)%in%predictors.include.test)] # Doesn't include zn
# We get a model that includes all predictors of the Proposed model except for zn

# Creating a 3rd model (submodel of the Proposed model, here we drop zn)
test2 <- lm(mvalue ~ crim+chas+nox+rooms+age+distance+radial+tax+pt+I(indus^2)+I(crim^2)+I(distance^2), data=Data)

## Computing AIC, BIC for our models ##
# AIC for Original full model
aic.original <- AIC(linearMod)
# AIC for Original full model dropping indus
aic.original1 <- AIC(linearMod1)
# AIC for Proposed  model
aic.proposed <- AIC(test)
# AIC for Proposed  model dropping zn
aic.proposed1 <- AIC(test2)

# BIC for Original full model
bic.original <- BIC(linearMod)
# BIC for Original full model dropping indus
bic.original1 <- BIC(linearMod1)
# BIC for Proposed  model
bic.proposed <- BIC(test)
# BIC for Proposed  model dropping zn
bic.proposed1 <- BIC(test2)

## Comparing the AIC between models ##
# Original vs Original dropping indus
aic.original > aic.original1
# Original dropping indus vs Proposed model
aic.original1 > aic.proposed
# Original dropping indus vs Proposed model dropping zn
aic.original > aic.proposed1

## Comparing the BIC between models ##
# Original vs Original dropping indus
bic.original > bic.original1
# Original dropping indus vs Proposed model
bic.original1 > bic.proposed
# Original dropping indus vs Proposed model dropping zn
bic.original1 > bic.proposed1

### This is using the code from courseweb ###
# AIC (Original model)
which.min(Best.subset$aic)
# AIC (Proposed model)
which.min(Best.subset.test$aic)
# Compute AIC and BIC (Original model)
# $aic gives us a sequence of ordered aic values where the smallest value is at the end
AIC <- Best.subset$aic
our.BIC <- AIC - 2*(1:11) + log(nrow(Data))*(1:11)
# Compute AIC and BIC for (Proposed model)
AIC.test <- Best.subset.test$aic
our.BIC.test <- AIC - 2*(1:11) + log(nrow(Data))*(1:11)
### Here ends the code from courseweb ###

### Exercise 4 Homework 7 ###

# Read the data
steam_data <- read.table('/home/noble_mannu/Documents/PhD/First/STAT_2131_Applied_Statistical_Methods_I/HW7/steam_text.txt', header = TRUE)

# Making the regression model
m1 <- lm(steam ~ fat+glycerine+wind+frezday+temp, data = steam_data)

## Performing forward and backward model selection ##
# Performing forward selection
alpha.1 <- 0.1
forward.1 <- ols_step_forward_p(m1, penter = alpha.1)
forward.1$predictors   # Model only contains temp and fat
# Performing backward selection
alpha.2 <- 0.2
backward.1 <- ols_step_backward_p(m1, penter = alpha.2)
backward.1$removed    # Model only contains temp and fat
# Performing forward step-wise selection
step.wise <- ols_step_both_p(m1, pent = alpha.1, prem = alpha.2)
step.wise$predictors  # Model only contains temp and fat

## Running best subset selection  ##
Best.subset.steam <- olsrr::ols_step_best_subset(m1)

## Comparing models Using R2 ##
# Choosing the model based on R2 or adjusted R2 (Original model)
which.max(Best.subset.steam$rsquare)
which.max(Best.subset.steam$adjr)
# Printing which predictors where left out (Original model)
predictors.include.steam <- strsplit(Best.subset.steam$predictors[which.max(Best.subset.steam$adjr)], "[ ]+", perl=T)[[1]]
colnames(steam_data)[!(colnames(steam_data)%in%predictors.include.steam)]
# Our model only includes temp and fat

# How can we decide whether to use AIC or BIC to rank models?