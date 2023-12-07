# (a) Fit a simple regression to the data
# Create vector X in concentration (mg/mL) and Y for colorimeter reading

X = c(40,50,60,70,80,90,40,60,80,50)
Y = c(69,175,272,335,490,415,72,265,492,180)
mean(X)
#[1] 62
mean(Y)
#[1] 276.5
sum((X-mean(X))^2)
#[1] 2760
sum((X-mean(X))*(Y-mean(Y)))
#[1] 23540
S_xx = sum((X-mean(X))^2)
S_xy = sum((X-mean(X))*(Y-mean(Y)))
beta_1 = S_xy/S_xx
beta_1
#[1] 8.528986
x_bar = mean(X)
y_bar = mean(Y)
beta_0 = y_bar - beta_1 * x_bar
beta_0
#[1] -252.2971
lsfit(X, Y)$coef
#  Intercept           X 
#-252.297101    8.528986 

# Calculate SSTO
SSTO = sum((Y - mean(Y))^2)
SSTO
#[1] 219270.5
SSR = (beta_1^2) * S_xx
SSR
#[1] 200772.3
SSE = SSTO - SSR
SSE
#[1] 18498.18

#Fit simple linear regression model
slr = lm(Y ~ X)

#Create scatter plot
plot(X, Y, main="Concentration in aqueous solution vs Turbidity", xlab="Concentration (mg/mL)", ylab="Colorimeter Reading of Turbidity")
abline(slr, col="red")

#(b) Obtain the residuals and examine them
residual = slr$res
plot(X, residual, main="Residual vs. Concentration (mg/mL)", xlab="Concentration (mg/mL)", ylab="Residual")
abline(h=0, col="red")

stan_res <- residuals(slr, type = "pearson")  # Calculate standardized residuals

# Create scatter plot of x against standardized residuals
plot(X, stan_res, main="Concentration in aqueous solution vs Standardized Residuals", xlab="Concentration (mg/mL)", ylab="Standardized Residuals")
abline(h = 0, col = "red")


#Check whether normally distributed
qqnorm(residual)
qqline(residual)

#(c) Comment on adequacy of the model
anova(slr)
# Note that in the anova table, "X" represents the source of variation due to the model or the fitted values

Rsquare = SSR/SSTO
Rsquare
#[1] 0.9156376
