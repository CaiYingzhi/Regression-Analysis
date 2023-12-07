library(dplyr)
library(ggplot2)
library(lmtest)

aadt_raw = read.table('aadt.txt', header = FALSE)
aadt = data.frame(y = aadt_raw$V1, x1 = aadt_raw$V2, x2 = aadt_raw$V3, x3 = aadt_raw$V4, x4 = aadt_raw$V5)
plot(aadt)

# Fit MLR model
mlr = lm(y ~ x1 + x2 + x3 + x4, data = aadt)
summary(mlr)
# Checking time order
names(mlr)
# Normality check
qqnorm(residuals(mlr), ylab = "mlr")
qqline(residuals(mlr))
# Residual plot
par(mfrow = c(1,5))
plot(residuals(mlr), fitted(mlr), ylab = "Residuals", xlab = "Fitted values")
plot(residuals(mlr), aadt$x1, ylab = "Residuals", xlab = "X1")
plot(residuals(mlr), aadt$x2, ylab = "Residuals", xlab = "X2")
plot(residuals(mlr), aadt$x3, ylab = "Residuals", xlab = "X3")
plot(residuals(mlr), aadt$x4, ylab = "Residuals", xlab = "X4")
par(mfrow = c(1,1))
# Checking for Sequential Dependence/ Durbin-Watson Test
dwtest(y ~ x1 + x2 + x3 + x4, data = aadt)

# Some F-tests
# Test for B_3 coefficient is zero
mlr_eval = lm(y ~ x3, data = aadt)
anova(mlr_eval, mlr)

# Test for B_ coefficient is constant
mlr_eval2 = lm(y ~ x1 + x2 + offset(1e+02*x3) + x4, data = aadt)
anova(mlr_eval2, mlr)

# Second model
mlr1 = lm(y ~ x1 + x2 + x4, data = aadt)
anova(mlr1, mlr)

# Third model
mlr2 = lm(I(y^(1/3)) ~ x1 + x2 + x4, data = aadt)
summary(mlr2)
# Normality check
par(mfrow = c(1,1))
qqnorm(residuals(mlr2), ylab = "mlr2")
qqline(residuals(mlr2))
# Residual plot
par(mfrow = c(1,4))
plot(residuals(mlr2), fitted(mlr2), ylab = "Residuals", xlab = "Fitted values")
plot(residuals(mlr2), aadt$x1, ylab = "Residuals", xlab = "X1")
plot(residuals(mlr2), aadt$x2, ylab = "Residuals", xlab = "X2")
plot(residuals(mlr2), aadt$x4, ylab = "Residuals", xlab = "X4")

#Observing transformed y^1/3
aadtv2 = aadt
aadtv2$y = aadtv2$y^(1/3)
plot(aadtv2)

# Fourth model
mlr3 = lm(y^(1/3) ~ log(x1) + x2 + x4, data = aadt)
summary(mlr3)

# Normality Check
par(mfrow = c(1,1))
qqnorm(residuals(mlr3), ylab = "mlr3")
qqline(residuals(mlr3))
# Residual Plot
par(mfrow = c(1,4))
plot(residuals(mlr3), fitted(mlr3), ylab = "Residuals", xlab = "Fitted values")
plot(residuals(mlr3), log(aadt$x1), ylab = "Residuals", xlab = "X1")
plot(residuals(mlr3), aadt$x2, ylab = "Residuals", xlab = "X2")
plot(residuals(mlr3), aadt$x4, ylab = "Residuals", xlab = "X4")
# Checking for Sequential Dependence/ Durbin-Watson Test
dwtest(y^(1/3) ~ log(x1) + x2 + x4, data = aadt)

# Predictions
mlrs = summary(mlr3)
con <- c(1,log(50000),3,2)
lhat <- sum(con*coef(mlr3))
# Calculate the critical value with degree of freedom of 116
t = qt(0.975,117)
c3 = 1
bm = t*mlrs$sigma*sqrt(con%*%mlrs$cov.unscaled%*%con + c3)
# Prediction interval of new response
c(lhat - bm, lhat + bm)^3
# Alternative method to get prediction interval
con = data.frame(x1 = 50000, x2 = 3, x3 = 60, x4 = 2)
predict(mlr3, con, interval = 'prediction', level = 0.95)^3
predict(mlr3, con, interval = 'confidence', level = 0.95)^3
