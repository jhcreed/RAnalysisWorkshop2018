library(datasets)
library(MASS)

data(airquality)
names(airquality)
summary(airquality)

# Split the sample data into training and evaluation set 
# Here I choose 8:2
set.seed(33612)
row.number <- sample(1:nrow(airquality), 0.8*nrow(airquality))
train = airquality[row.number,]
test = airquality[-row.number,]
dim(train) # dimension of train set
dim(test) # dimension of test set
# Note that both response and prediction variables of our interest have missing values 
# R will toss missing values in modeling, but let's eliminate missing values upfront
nmiss.airquality <- na.omit(airquality[,1:2])
row.number <- sample(1:nrow(nmiss.airquality), 0.8*nrow(nmiss.airquality))
train = nmiss.airquality[row.number,]
test = nmiss.airquality[-row.number,]
dim(train) # dimension of train set
dim(test) # dimension of test set


# Graphical Analysis
# Plot one variable at a time
## Scatter Plot 
par(mfrow=c(1,2))
plot(train$Ozone)
plot(train$Solar.R)

## Histogram
par(mfrow=c(1,2))
hist(train$Ozone)
hist(train$Solar.R)

# Plot two variables together 
plot(Ozone ~ Solar.R, data=train)
plot(Solar.R, Ozone, data=train) # the same plot
abline(h=mean(train$Ozone,na.rm=T))

# Transformation
## Log Transformation
train$logOzone <- log(train$Ozone)
train$logSolar <- log(train$Solar.R)
# Plot to see the difference before and after transformation 
par(mfrow=c(1,2))
plot(Ozone ~ Solar.R, data=train)
plot(logOzone ~ Solar.R, data=train)

# Let's fit a simple linear regression model
## on original data
lin.reg <- lm(Ozone~Solar.R, data=train)
# summary( ) is the most popular way to display lm results in R
summary(lin.reg)
# Anova table
anova(lin.reg) 
## on log-transformed data
log.reg <- lm(logOzone~Solar.R, data=train)
summary(log.reg)
anova(log.reg)
## ANOVA to compare two models 
anova(log.reg, lin.reg, test="Chisq")

x <- train$Solar.R
y <- train$logOzone
yhat <- log.reg$fitted.values
residuals <- log.reg$residuals
par(mfrow=c(1,3))
plot(x, yhat, type="l", main="scatter plot and fitted line",ylab="logOzone and logOzone_hat",xlab="Solar.R")
points(x,y,pch=19)
plot(x,residuals,main="residuals vs. logOzone",xlab="Solar.R",pch=19)
qqnorm(residuals)
qqline(residuals)

# Or you can do this
par(mfrow=c(2,2))
plot(log.reg, which=1:4)

# Shapiro-Wilk normality test
shapiro.test(residuals)

# Keep exploring lm 
summary.reg <- summary(log.reg)
anova.table <- anova(log.reg) # Anova table

# the output of lm() is an "object" contains several variables
log.reg$coefficients  # the coefficients $\hat{\beta}$
log.reg$df.residual  # df_e, degree of freedom of error (and the model)
log.reg$fitted.values  # the fitted values $\hat{y}$
log.reg$residuals	  # the residuales $y-\hat{y}$

# the variable names can be simplified as long as they unique.
log.reg$coef
log.reg$fit
log.reg$res

SSR=anova.table[1,2]	
SSE=anova.table[2,2]
SSTO=SSR+SSE	

Rsq=SSR/SSTO
Rsq

hist(log.reg$res)
qqnorm(log.reg$res)
qqline(log.reg$res)

# confint( ) computes the CI for $\beta$.
confint(log.reg, level=0.95)  # by default, show 95% CI for all parameters 
confint(log.reg, "Solar.R", level=0.90)

# AIC and BIC
AIC(log.reg)
BIC(log.reg)

# Plot regression line with data
par(mfrow=c(1,1))
plot(logOzone ~ Solar.R, data=train,
     xlab="Solar Radiation", ylab="LogOzone",
     main="Daily Air Quality Measurements in NY", pch=16)
abline(log.reg$coef, lwd=2, col=2)

# Plot simple linear regression with 95% CI and PI
temp<-data.frame(Solar.R=sort(train$Solar.R)) # get the increasing sequence of x
ci<-predict(log.reg, newdata=temp, interval = "confidence")
pi<-predict(log.reg, newdata=temp, interval = "prediction")
is.data.frame(ci)  # note that predict( ) put the results as a matrix
is.matrix(ci)
lines(temp$Solar.R, ci[,2], lwd=2, lty=2, col=3)
lines(temp$Solar.R, ci[,3], lwd=2, lty=2, col=3)
lines(temp$Solar.R, pi[,2], lwd=2, lty=3, col=4)
lines(temp$Solar.R, pi[,3], lwd=2, lty=3, col=4)
legend(x=175, y=1, lty=c(1,2,3), col=c(2,3,4),
       legend=c("Fitted regression line", "95% CI (mean)", "95% PI (Individual)"))


# Test H0 $\beta$ = a non-0 value need a little bit work
# Eg. H0: $\beta_1$ = 0.0035, vs. Ha: $\beta_1$ > 0.0035
vcov(log.reg)			      # The variance/covariance matrix of the parameter estimation.
se.b1<-sqrt(vcov(log.reg)[2,2])  	# now we have std.err of $\beta_1$.
tobs<-(log.reg$coef[2]-0.0035)/se.b1    # compute test statistic
pvalue<-1-pt(-tobs, df=log.reg$df)   # In this test, p-value=P(T(df) > tobs)
c(tobs, pvalue)

# The predict function gives fitted values, confidence limits, standard errors, degrees of freedom.
# se.fit=TRUE gives standard errors along with the confidence bounds.
# interval = "confidence" finds a confidence interval (as opposed to a prediction interval).
test.clim <- predict(log.reg, test, se.fit=TRUE,interval="confidence")
test.clim$fit 
test.clim$se.fit
test.clim$df

# Use test set to validate lm
# Do not forget to log-transform y-variable in test set
test$logOzone <- log(test$Ozone)
# use function predict() or predict.lm()
test.plim <- predict(log.reg, test, se.fit=TRUE,interval="prediction")
preds <- data.frame(cbind(observed=test$logOzone, predicted=test.plim$fit[,1])) 
# Prediction accuracy
correlation_accuracy <- cor(preds) # 59.2%
# Min Max accuracy
min_max_accuracy <- mean(apply(preds, 1, min) / apply(preds, 1, max)) 
# Mean Absolute Percentage Error (MAPE)
mape <- mean(abs((preds$predicted - preds$observed))/preds$observed)


