
# PRELIMINARY ANALYSIS 
## Descriptive Statistics 
numSummary(low_birth)

## Graphical Summaries
#Is the relationship between headcircumference and gestage  gender dependent?
xyplot(headcirc ~ gestage, group=sex, data = low_birth,auto.key = list(column = 2),
       type = c("g","r","p"), par.settings = simpleTheme(col = c("red", "darkblue"),pch = 20, col.line = c("red", "darkblue") ))

#Is the relationship between headcirc and birthweight  gender dependent?
xyplot(headcirc ~ birthwt, group=sex, data = low_birth,auto.key = list(column = 2),
type = c("g","r","p"), par.settings = simpleTheme(col = c("red", "darkblue"), pch = 20, col.line = c("red", "darkblue") ))

## Checking the Distributions of the data
plotNormalHistogram(gestage, main = "Histogram of Gestational Age")

boxplot(headcirc , main = "Boxplot of headcircumference")

boxplot(birthwt , main = "Boxplot of birth weight")

plotNormalHistogram(momage, main = "Histogram of momage")

ggplot(low_birth, aes(x = sex, fill = sex)) + 
  geom_bar() +
  xlab("sex") +
  ylab("Count") +
  ggtitle("Gender Distribution")

## COrrelation Matrix
# Calculate correlation matrix
cor_matrix <- cor(low_birth[,-8])
rcorr(cor_matrix,type="pearson")

corr <- cor(low_birth[,-8])
# Create the corrplot
corrplot(corr, method = "number", type = "upper", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", col = colorRampPalette(c("#F7FBFF", "#2171B5"))(50),
         number.cex = 0.7, tl.cex = 0.8, width = 12)

## Scatterplot Matrix
scatterplotMatrix(~headcirc +birthwt + gestage + momage + length + sex , col = "black")



# REGRESSION MODELS
## Full regression model 

### Full regression model 
low.lm1<-lm (headcirc~.,data=low_birth)
summary(low.lm1)

## Reduced regression model 
## regression model to with interactions 
low.lm2 <- lm(headcirc ~ gestage + birthwt , data = low_birth)
summary(low.lm2)

## Reduced regression with Interactions
low.lm3<- lm(headcirc ~ gestage + birthwt + gestage*(as.factor(sex)) + birthwt*(as.factor(sex)), data = low_birth)
summary(low.lm3)

## AIC Approach
low.lm4=step(lm(headcirc ~ gestage + birthwt + gestage*(as.factor(sex)) + birthwt*(as.factor(sex)) + momage + length + sbp, data = low_birth),direction='both')
summary(low.lm4)

## LASSO Model
x=data.matrix(low_birth)
x<- subset(x, select = -c(`headcirc`))
y=data.matrix(low_birth, rownames.force = NA)[,5]
set.seed(2000)
fit1=glmnet::cv.glmnet(x,y,type.measure = "mae", nfolds=3)
coef(fit1,s = "lambda.1se")

## Model Selection by CP 
library(leaps)
outs=leaps(x, y, method="Cp")
plot(outs$size, outs$Cp, xlab = "p", ylab = expression(C[p]), ylim=c(-5, 200))
lines(outs$size, outs$size)
ifoo <- outs$Cp == min(outs$Cp)
ifoo <- outs$which[ifoo, ]
foo <- x[, ifoo]
low.lm5 <- lm(y ~ foo)
summary(low.lm5)

## Model Selection by R-Squared
library(leaps)
outadjr2=leaps(x, y, method="adjr2")
plot(outadjr2$size, outadjr2$adjr2, xlab = "p", ylab = expression(R^2), ylim=c(0, 1))

#  MODEL SELECTION 

# COMPARING REGRESSION MODELS PERFORMANCE 
## Metrics for model 1 (model with all variables)
glance(low.lm1) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

## Metrics for model2 (model with all significant variables)
glance(low.lm2) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

## Metrics for model 3(model with interactions)
glance(low.lm3) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

## Metrics for model 4(model with interactions)
glance(low.lm4) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

## Metrics for model 5(model with interactions)
glance(low.lm5) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)

## Model interpretation
summary(low.lm3)





# MODEL DIAGNOSTICS

## External Residual

nobs=nrow(low_birth)
jack=rstudent(low.lm3)

# Critical Value for Outlier t Test
outqt=qt(.05/(nobs*2),(nobs-2-2))
# alpha=0.05, no. of obs=34, no. of regressors is 2 34-2-2=30
plot(jack,ylab="Jacknife Residuals", main="Jacknife Residuals",
     ylim=c(-4, 4),col='blue')
abline(h=outqt, col='blue')
abline(h=abs(outqt), col='blue')

jack[abs(jack)==max(abs(jack))] # Output the largest residual


## Leverage 
x<-model.matrix(low.lm2)
lev<-hat(x)
plot(lev,ylab="Leverages",main="Index Plot of Leverages", col='blue')
abline(h=2*3/nobs, col='blue') # rule of thumb 2*(p+1)/n
identify(1:nobs, lev)

## Influential 
cutoff <- 4/((nrow(low_birth)-length(low.lm3$coefficients)-2))
plot(low.lm3, which = 4, cook.levels = cutoff)

#4 diagnostic plots to identify influential points
infIndexPlot(low.lm3)
#Influence Measures
summary(influence.measures(low.lm3));

#Bonferroni p-values for testing outliner
outlierTest(low.lm3)

## Homoscedasticity (Equal Variance Assumption)
#residual vs. fitted value plot for Homoscedasticity
plot(low.lm3$resid ~ low.lm3$fitted.values)
#add horizental line from 0
abline(h = 0, lty = 2)
bptest(low.lm3)

## Linearity
#residual vs. fitted value (linearity) and all predictors plus test for curvature for linearity
residualPlots(low.lm3)

## Normality Test 
#Normal Quantile to Quantile plot
qqPlot(low.lm3, main="QQ Plot")

# distribution of studentized residuals
sresid <- studres(low.lm3)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
## numerical test
shapiro.test(residuals(low.lm3))


## variance inflation factor (Multicolinearity)

car::vif(low.lm3)  
sqrt(vif(low.lm3)) > 2 # problem?

## residual plot vs. PIQ (independence)
durbinWatsonTest(low.lm3)
plot(low.lm2$res, ylab="Residuals", main="Index Plot of Residuals")



