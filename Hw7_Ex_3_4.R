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

# Choosing the model based on R2 (Proposed model)
which.max(Best.subset.test$rsquare)
# Returns row 14, this corresponds to the model with all predictors
# Prints the names of the predictors used in the best model (Proposed model) with R2 criteria
Best.subset.test$predictors[which.max(Best.subset.test$rsquare)]

# Choosing the model based on adjusted R2 (Proposed model)
which.max(Best.subset.test$adjr)
# Returns row 13, this corresponds to the model with all predictors except zn
# Prints the names of the predictors used in the best model (Proposed model) with adjusted R2 criteria
Best.subset.test$predictors[which.max(Best.subset.test$adjr)]

# Comparing best subset of Original model vs best subset of Proposed model with R^2 criteria
r_origin <- Best.subset$rsquare[which.max(Best.subset$rsquare)]
r_prop <- Best.subset.test$rsquare[which.max(Best.subset.test$rsquare)]
r_prop > r_origin
# This indicates the Proposed model is an improvement over the Original model using R^2 criteria

# Comparing best subset of Original model vs best subset of Proposed model with adjusted R^2 criteria
r_adj_origin <- Best.subset$adjr[which.max(Best.subset$adjr)]
r_adj_prop <- Best.subset.test$adjr[which.max(Best.subset.test$adjr)]
r_adj_prop > r_adj_origin
# This indicates the Proposed model is an improvement over the Original model using adjusted R^2 criteria

## Comparing models Using AIC and BIC ##

# Choosing the model based on AIC (Original model)
which.min(Best.subset$aic)
# Returns row 10, this corresponds to the model with all predictors exceptc indus
# Prints the names of the predictors used in the best model with AIC criteria
Best.subset$predictors[which.min(Best.subset$aic)]

# Choosing the model based on AIC (Proposed model)
which.min(Best.subset.test$aic)
# Returns row 13, this corresponds to the model with all predictors exceptc zn
# Prints the names of the predictors used in the best model with AIC criteria
Best.subset.test$predictors[which.min(Best.subset.test$aic)]

# Comparing best subset of Original model vs best subset of Proposed model with IAC criteria
aic_origin <- Best.subset$aic[which.min(Best.subset$aic)]
aic_prop <- Best.subset.test$aic[which.min(Best.subset.test$aic)]
aic_origin > aic_prop
# This indicates the Proposed model is an improvement over the Original model using AIC criteria

# Choosing the model based on BIC (Original model)
which.min(Best.subset$sbc)
# Returns row 10, this corresponds to the model with all predictors exceptc indus
# Prints the names of the predictors used in the best model with BIC criteria
Best.subset$predictors[which.min(Best.subset$sbc)]

# Choosing the model based on BIC (Proposed model)
which.min(Best.subset.test$sbc)
# Returns row 11, this corresponds to the model with the model that omits zn, indus and indus^2
# Prints the names of the predictors used in the best model with BIC criteria
Best.subset.test$predictors[which.min(Best.subset.test$sbc)]

# Comparing best subset of Original model vs best subset of Proposed model with BIC criteria
bic_origin <- Best.subset$sbc[which.min(Best.subset$sbc)]
bic_prop <- Best.subset.test$sbc[which.min(Best.subset.test$sbc)]
bic_origin > bic_prop
# This indicates the Proposed model is an improvement over the Original model using BIC criteria

### Exercise 4 Homework 7 ###

# Read the data
steam_data <- read.table('steam_text.txt', header = TRUE)

# Making the regression model
m1 <- lm(steam ~ fat+glycerine+wind+frezday+temp, data = steam_data)

## Performing forward and backward model selection ##

# Performing forward selection
alpha.1 <- 0.1
forward.1 <- ols_step_forward_p(m1, penter = alpha.1)
forward.1$predictors   # Model only contains temp and fat
# Our final model only has temp and fat as predictors

# Performing backward selection
alpha.2 <- 0.2
backward.1 <- ols_step_backward_p(m1, penter = alpha.2)
backward.1$removed    # Model only contains temp and fat
# Our final model only has temp and fat as predictors

# Although we weren't required to do so, I performed forward step-wise selection just to see how it went
step.wise <- ols_step_both_p(m1, pent = alpha.1, prem = alpha.2)
step.wise$predictors  # Model only contains temp and fat
# Once again our final model only has temp and fat as predictors

# Now we'll run best subset selection for this model

## Running best subset selection  ##
Best.subset.steam <- olsrr::ols_step_best_subset(m1)

# Choosing the model based on AIC
which.min(Best.subset.steam$aic)
# Returns row 2, this corresponds to the model with only fat and temp as predictors
# Prints the names of the predictors used in the best model with AIC criteria
Best.subset.steam$predictors[which.min(Best.subset.steam$aic)]
# This shows that the best model using AIC criteria is the one that has fat and temp as predictors

# Choosing the model based on BIC
which.min(Best.subset.steam$sbc)
# Returns row 2, this corresponds to the model with only fat and temp as predictors
# Prints the names of the predictors used in the best model with BIC criteria
Best.subset.steam$predictors[which.min(Best.subset.steam$sbc)]
# This shows that the best model using BIC criteria is the one that has fat and temp as predictors

# We condlude that backward, forward, step-wise forward and AIC and BIC methods all returned back the model with predictors 'fat' and 'temp'

# Note: With adjusted R2 criteria we get the same model as above
which.max(Best.subset.steam$adjr)
# Returns row 2, this corresponds to the model with only fat and temp as predictors
# Prints the names of the predictors used in the best model with R2 criteria
Best.subset.steam$predictors[which.max(Best.subset.steam$adjr)]

# Note: With R2 criteria we get the full model
which.max(Best.subset.steam$rsquare)
# Returns row 5, this corresponds to the full model
# Prints the names of the predictors used in the best model with AIC criteria
Best.subset.steam$predictors[which.max(Best.subset.steam$rsquare)]