rm(list=ls())

############################
#### QUESTGION 1 Part a ####
############################
set.seed(5072)

############################
#### QUESTGION 1 Part b ####
############################
x <- rnorm(n=100, mean=0, sd = 1)

############################
#### QUESTGION 1 Part c ####
############################
eps <- rnorm(n = 100, mean=0, sd = 0.25)

############################
#### QUESTGION 1 Part d ####
############################
y <- -1 +0.5*x +eps
var(eps)

############################
#### QUESTGION 1 Part e ####
############################
length(y)

############################
#### QUESTGION 1 Part f ####
############################
#The population parameter values of β0 = 1; β 1=0.5.

############################
#### QUESTGION 1 Part g ####
############################
plot(y~x, xlab = 'x', ylim = c(-2.5,1))


############################
#### QUESTGION 1 Part h ####
############################
#There is a linear relationship between X and Y.

############################
#### QUESTGION 1 Part i ####
############################
lm.fitsimple <- lm(y~x)

############################
#### QUESTGION 1 Part j ####
############################
coef(lm.fitsimple)
#a. βhat0 = -1.0014; βhat1 = 0.4676;
#b. βhat0 and βhat1 are pretty closed to β0 and β1 which means the model fits the data quite well.

############################
#### QUESTGION 1 Part k ####
############################
abline(lm.fitsimple, col="black")

############################
#### QUESTGION 1 Part l ####
############################
yp <- -1 + 0.5*x
lm.fitsimplepop <- lm(yp ~ x) #population regression
abline(lm.fitsimplepop, col="red")

############################
#### QUESTGION 1 Part m ####
############################
legend( "bottomright", c("Least squares","Population"), lty = 1, col = c("black", "red"), bty="o")
title(main = "Moderate Error in the Population")

############################
#### QUESTGION 1 Part n ####
############################
lm.fitpoly <- lm(y ~ poly(x, 2))

############################
#### QUESTGION 1 Part o ####
############################
anova(lm.fitsimple, lm.fitpoly)
#The RSS of the polynomial regression has not significant improved;So, the quadratic term didn't improved the model.

############################
#### QUESTGION 1 Part p ####
############################
eps2 <- rnorm(n = 100, mean=0, sd = 0.1) # less noise
y2 <- -1 + 0.5*x + eps2
lm.fit2 <- lm(y2 ~ x)

# Least Squares
plot(y2~x, xlab = 'x', ylim = c(-2.0,0.5), ylab = 'y.less')
abline(lm.fit2, col="black")

# y2 population 
yp <- -1 + 0.5*x
lm.fitsimplepop <- lm(yp ~ x) #population regression
abline(lm.fitsimplepop, col="red")

legend( "bottomright", c("Least squares","Population"), lty = 1, col = c("black", "red"), bty="o")
title(main = "Less Error in the Population")

############################
#### QUESTGION 1 Part q ####
############################
eps3 <- rnorm(n = 100, mean=0, sd = 0.5) # more noice
y3 <- -1 + 0.5*x + eps3 
lm.fit3 <- lm(y3 ~ x)

# Least Squares
plot(y3~x, xlab = 'x', ylim = c(-3.0,0.5), ylab = 'y.more')
abline(lm.fit3, col="black")

# y3 population 
yp <- -1 + 0.5*x
lm.fitsimplepop <- lm(yp ~ x) #population regression
abline(lm.fitsimplepop, col="red")

legend( "bottomright", c("Least squares","Population"), lty = 1, col = c("black", "red"), bty="o")
title(main = "Higher Error in the Population")

############################
#### QUESTGION 1 Part r ####
############################
summary(lm.fitsimple)
summary(lm.fit2)
summary(lm.fit3)
# compared with R-squared between three models, lm.fit2, the model with less noice preformed a better fit for data. 

############################
#### QUESTGION 1 Part s ####
############################
lm.fitsimple <- lm(y~x) #moderate noise
lm.fit2 <- lm(y2 ~ x)   #less noise
lm.fit3 <- lm(y3 ~ x)   # more noise
confint(lm.fitsimple, level = 0.95)
confint(lm.fit2, level = 0.95)
confint(lm.fit3, level = 0.95)  #level required 95%

############################
#### QUESTGION 1 Part t ####
############################
# The widths of the confidence intervals are different 
# is because the noice of data set has changed so that 
# the standard diviation changed. The width of confidence interval increase as the sd increase.
# less noice means less sd and a small width; high noice with a higher sd has a larger width.

############################
#### QUESTGION 2 Part a ####
############################
set.seed(5072)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

############################
#### QUESTGION 2 Part b ####
############################
print("In the linear model, the population parameter values β0=2, β1 =2, β2= 0.3")

############################
#### QUESTGION 2 Part c ####
############################
col <- data.frame(y, x1, x2) #create a data frame and put variables in
cor(col, use = "everything", method = "pearson")

############################
#### QUESTGION 2 Part d ####
############################
pairs(col)

############################
#### QUESTGION 2 Part e ####
############################
print('Since Peason correlation is a number between -1 and 1,
      The corelation between x1 and x2 is 0.84, indicating that 
      there is a strong linear corelation between x1 and x2; 
      But the correlation number between x1 and y,and x2 and y are 0.51 and 0.40 respectively
      indicating the linear relationship between variable x1 and y and x2 and y are relatively ambiguous')

############################
#### QUESTGION 2 Part f ####
############################
lm.fit.both <- lm(y~x1+x2) 

############################
#### QUESTGION 2 Part g ####
############################
coef(lm.fit.both)

############################
#### QUESTGION 2 Part h ####
############################
summary(lm.fit.both)
print("The βhat0 = 2.04 and βhat1 = 2.3 haven't changed a lot, since β0 and β1 both equals to 2; 
β2 changed dramatically from 0.3 to -0.49,which means the model doesn't fit as well as what we expected; 
the p-value of x2 is 0.64 > 0.05, so I would say x2 has no statistical siginificance on y;
The p-value of x1 is 0.0006 < 0.05, so x1 has statistical significance on y")

############################
#### QUESTGION 2 Part i ####
############################
summary(lm.fit.both)
print("According to the summary result of lm.fit.both, since the p-value of β1 = 0.0006 < 0.05, 
      which means it reject the null hypothesis that β1 = 0; 
the p-value of β2 = 0.64 > 0.05, which means it failed to reject the null hypothesis that β2 = 0")

############################
#### QUESTGION 2 Part j ####
############################
lm.fit.justx1 <- lm(y~x1)
lm.fit.justx1

############################
#### QUESTGION 2 Part k ####
############################
summary(lm.fit.justx1)
print("The result shows that x1 can 'perfectly' predict y, the model is good. 
      Yes, we can reject the null hypothesis, 
      because the p-value of β1 is less than 0.05.")

############################
#### QUESTGION 2 Part l ####
############################
lm.fit.justx2 <- lm(y~x2)
lm.fit.justx2

############################
#### QUESTGION 2 Part m ####
############################
summary(lm.fit.justx2)
print("The result shows that x2 can predict y, the model is still good. 
      Yes, we can reject the null hypothesis, 
      because the p-value of β2 is less than 0.05.")

############################
#### QUESTGION 2 Part n ####
######"######################
print("The results from j-m and f-i are not contradict because this is a collinearity situation,
      where x1 and x2 as variables have a strong linear relationship.")

############################
#### QUESTGION 2 Part o ####
############################
#print(x1)
x1 = c(x1,0.1) # add a number 0.1 to vector x1
#print(x1)  
x2 = c(x2, 0.8)
y = c(y,6)

################################
#### QUESTGION 2 Part p & q ####
################################
lm.fit.bothedit <- lm(y~x1+x2) # fit new edited data again
lm.fit.bothedit$coefficients # display the β0, β1, β2
summary(lm.fit.bothedit)
print("After the new oberservations was introduced into the model,
The βhat0 doesn't change a lot from 2 to 2.17; β1 and β2 changed dramatically, which means the model doesn't fit; the p-value of x2 is 0.09 > 0.05, 
      so x2 has no statistical siginificance on y")

print("According to the summary result of lm.fit.bothedit, since the p-value of β1 = 0.04 < 0.05, which means it reject the null hypothesis that β1 = 0;
the p-value of β2 = 0.095 > 0.05, which means it failed to reject the null hypothesis that β2 = 0")

# only x1
lm.fit.justx1edit <- lm(y~x1)
summary(lm.fit.justx1edit)
print("The result shows that x1 can 'perfectly' predict y, since β1=1.9, the model is good. 
      Yes, we can reject the null hypothesis, 
      because the p-value of β1 is less than 0.05.")

#only x2
lm.fit.justx2edit <- lm(y~x2)
summary(lm.fit.justx2edit)
print("The result shows that x2 can predict y. 
      Yes, we can reject the null hypothesis, 
      because the p-value of β2 is way less than 0.05.")

coledit <- data.frame(y, x1, x2) #use pearson to check the new linear relationship
cor(coledit, use = "everything", method = "pearson")
print("The new observation has a slight effect on to the model in general.
      The β2 changed a little bit. I recall the pearson correlation and found out x1 and x2 still have a 
      strong linear relationship, but weaker than our oringinal data set. I believe the relationship between x1 and x2 doesn't 
      change dramatically is due to the small dataset. If we add addition 100 new observations into these variables, the collinearity would different a lot.")

##############################
#### QUESTGION 2 Part r ####
##############################
par(mfrow=c(2,2))
plot(lm.fit.bothedit) # both
print("When predicting y using x1 and x2, the new observation 101 is an outlier and a high-leverage point.
      Through the Residuals vs Fitted plot, the observation 101's residual 'stands out' from the basic random pattern of residuals,
      this suggests that observation 101 is an outliers; 
      Through the Residuals vs Leverage plot, the observatgion 101 lies outside the dashed red curves with a hight leverage. 
      This suggests that observation 101 is a hight-leverage point")


plot(lm.fit.justx1edit) # both
print("When predicting y using only x1, the new observation 101 is an outlier and a high-leverage point.
      Through the Residuals vs Fitted plot, the observation 101's residual 'stands out' from the basic random pattern of residuals,
      this suggests that observation 101 is an outliers; In the Normal Q-Q plot, the observation 101 stands out from the theoretical quantiles, 
which proofs that it is an outlier point. 
      Through the Residuals vs Leverage plot, the observatgion 101 lies in the upper right hand side with a high leverage value. 
      This suggests that observation 101 is a hight-leverage point")

plot(lm.fit.justx2edit) # high-leverage
print("When predicting y using only x2, the new observation 101 is a high-leverage point.
There is no observation 101 standing out in the Residuals vs Fitted, Normal Q-Q and Scale-location plots;
      Through the Residuals vs Leverage plot, the observatgion 101 lies in the upper right hand side with a high leverage value. 
      This suggests that observation 101 is a hight-leverage point")

par(mfrow=c(1,1))

############################
#### QUESTGION 3 Part a ####
############################

# load required pacakges & use MASS
rm(list=ls())
needed  <-  c("MASS")      
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    library(thispackage, character.only = T)
  }
}
installIfAbsentAndLoad(needed)

#check variables
str(Boston) 

#set seed & create lists
set.seed(5072)

listFstatistic <- matrix(nrow=13, ncol=3, byrow = T)
colnames(listFstatistic)<- c("value","numdf","dendf")
rownames(listFstatistic)<-c("medv","dis","nox","indus","age","tax","ptratio","rm","zn","rad","lstat","black","chas")
#listFstatistic
listpvalue <-  matrix(nrow=13, ncol=2, byrow = T)
colnames(listpvalue)<- c("pvalue", "NA")
rownames(listpvalue)<- c("medv","dis","nox","indus","age","tax","ptratio","rm","zn","rad","lstat","black","chas")

listcoef <-  matrix(nrow=13, ncol=2, byrow = T)
colnames(listcoef)<- c("Intercept","medv")
rownames(listcoef)<-c("medv","dis","nox","indus","age","tax","ptratio","rm","zn","rad","lstat","black","chas")
#Not sure if we need chas

# predictors
#1 medv
lm.medv=lm(crim~medv,data=Boston)
listFstatistic[1,] <-summary(lm.medv)$fstatistic
#listFstatistic
#summary(lm.medv)
summary(lm.medv)$fstatistic
#anova(lm.medv)
anova(lm.medv)$`Pr(>F)`  # why anova p-value different from call anova p-value?
listpvalue[1,] <- anova(lm.medv)$`Pr(>F)`
listcoef[1,] <- coef(lm.medv)

#2 dis
lm.dis=lm(crim~dis,data=Boston)
#summary(lm.dis)
listFstatistic[2,] <- summary(lm.dis)$fstatistic
#anova(lm.dis)
listpvalue[2,] <- anova(lm.dis)$`Pr(>F)`
listcoef[2,] <- coef(lm.dis)

#3 nox
lm.nox=lm(crim~nox,data=Boston)
#summary(lm.nox)
listFstatistic[3,] <- summary(lm.nox)$fstatistic
#anova(lm.nox)
listpvalue[3,] <- anova(lm.nox)$`Pr(>F)`
listcoef[3,] <- coef(lm.nox)

#4 indus
lm.indus=lm(crim~indus,data=Boston)
#summary(lm.indus)
listFstatistic[4,] <- summary(lm.indus)$fstatistic
#anova(lm.indus)
listpvalue[4,] <- anova(lm.indus)$`Pr(>F)`
listcoef[4,] <- coef(lm.indus)

#5 age
lm.age=lm(crim~age,data=Boston)
#summary(lm.age)
listFstatistic[5,] <- summary(lm.age)$fstatistic
#anova(lm.age)
listpvalue[5,] <- anova(lm.age)$`Pr(>F)`
listcoef[5,] <- coef(lm.age)

#6 tax
lm.tax=lm(crim~tax,data=Boston)
#summary(lm.tax)
listFstatistic[6,] <- summary(lm.tax)$fstatistic
#anova(lm.tax)
listpvalue[6,] <- anova(lm.tax)$`Pr(>F)`
listcoef[6,] <- coef(lm.tax)

#7 ptratio
lm.ptratio=lm(crim~ptratio,data=Boston)
#summary(lm.ptratio)
listFstatistic[7,] <- summary(lm.ptratio)$fstatistic
#anova(lm.ptratio)
listpvalue[7,] <- anova(lm.ptratio)$`Pr(>F)`
listcoef[7,] <- coef(lm.ptratio)

#8 rm
lm.rm=lm(crim~rm,data=Boston)
#summary(lm.rm)
listFstatistic[8,] <- summary(lm.rm)$fstatistic
#anova(lm.rm)
listpvalue[8,] <- anova(lm.rm)$`Pr(>F)`
listcoef[8,] <- coef(lm.rm)

#9 zn
lm.zn=lm(crim~zn,data=Boston)
#summary(lm.zn)
listFstatistic[9,] <- summary(lm.zn)$fstatistic
#anova(lm.zn)
listpvalue[9,] <- anova(lm.zn)$`Pr(>F)`
listcoef[9,] <- coef(lm.zn)

#10 rad
lm.rad=lm(crim~rad,data=Boston)
#summary(lm.rad)
listFstatistic[10,] <- summary(lm.rad)$fstatistic
#anova(lm.rad)
listpvalue[10,] <- anova(lm.rad)$`Pr(>F)`
listcoef[10,] <- coef(lm.rad)

#11 lstat
lm.lstat=lm(crim~lstat,data=Boston)
#summary(lm.lstat)
listFstatistic[11,] <- summary(lm.lstat)$fstatistic
#anova(lm.lstat)
listpvalue[11,] <- anova(lm.lstat)$`Pr(>F)`
listcoef[11,] <- coef(lm.lstat)

#12 black
lm.black=lm(crim~black,data=Boston)
#summary(lm.black)
listFstatistic[12,] <- summary(lm.black)$fstatistic
#anova(lm.black)
listpvalue[12,] <- anova(lm.black)$`Pr(>F)`
listcoef[12,] <- coef(lm.black)

#13 
lm.chas=lm(crim~chas,data=Boston)
#summary(lm.chas)
listFstatistic[13,] <- summary(lm.chas)$fstatistic
#anova(lm.chas)
listpvalue[13,] <- anova(lm.chas)$`Pr(>F)`
listcoef[13,] <- coef(lm.chas)

# show table result
#listFstatistic 
#listpvalue 
#listcoef 
cbind(listFstatistic,listpvalue,listcoef)
table <- cbind(listFstatistic[,1],listpvalue[,1],listcoef)
colnames(table) <- c("F-statistic", "p-value", "Intercept", "medv")
table

############################
#### QUESTGION 3 Part b ####
############################
print("To find which predictors are significant, we have to test H0 that β0=0. 
      All predictors have a p-value less than 0.05, reject the H0 Hypothesis,
      so we may conclude that there is a statistically significant association 
      between each predictor and the response")

############################
#### QUESTGION 3 Part c ####
############################
par(mfrow=c(4,3))
par(ask=F)
plot(crim ~ zn, data = Boston, main= "zn", xlab = "x", ylab= "Boston$crim")
abline(lm.zn, col ="red",lwd = 3)
plot(crim ~ indus, data = Boston, main= "indus", xlab = "x", ylab= "Boston$crim")
abline(lm.indus, col ="red",lwd = 3)
plot(crim ~ nox, data = Boston, main= "nox", xlab = "x", ylab= "Boston$crim")
abline(lm.nox, col ="red",lwd = 3)
plot(crim ~ rm, data = Boston, main= "rm", xlab = "x", ylab= "Boston$crim")
abline(lm.rm, col ="red",lwd = 3)
plot(crim ~ age, data = Boston, main= "age", xlab = "x", ylab= "Boston$crim")
abline(lm.age, col ="red",lwd = 3)
plot(crim ~ dis, data = Boston, main= "dis", xlab = "x", ylab= "Boston$crim")
abline(lm.dis, col ="red",lwd = 3)
plot(crim ~ rad, data = Boston, main= "rad", xlab = "x", ylab= "Boston$crim")
abline(lm.rad, col ="red",lwd = 3)
plot(crim ~ tax, data = Boston, main= "tax", xlab = "x", ylab= "Boston$crim")
abline(lm.tax, col ="red",lwd = 3)
plot(crim ~ ptratio, data = Boston, main= "patratio", xlab = "x", ylab= "Boston$crim")
abline(lm.ptratio, col ="red",lwd = 3)
plot(crim ~ black, data = Boston, main= "black", xlab = "x", ylab= "Boston$crim")
abline(lm.black, col ="red",lwd = 3)
plot(crim ~ lstat, data = Boston, main= "lstat", xlab = "x", ylab= "Boston$crim")
abline(lm.lstat, col ="red",lwd = 3)
plot(crim ~ medv, data = Boston, main= "medv", xlab = "x", ylab= "Boston$crim")
abline(lm.medv, col ="red",lwd = 3)
par(mfrow=c(1,1))

############################
#### QUESTGION 3 Part d ####
############################
#Fit all variables
fit.all <- lm(crim ~ ., data = Boston)

############################
#### QUESTGION 3 Part e ####
############################
summary(fit.all)
#summary(fit.all)$coef[,4] <= .05
pvalue005 <- summary(fit.all)$coef[summary(fit.all)$coef[,4] <= .05,4]
pvalue005

print("according to the  Pr(>|t|) value from summary,
      variables zn, dis, rad, black, and medv are significant at a level of a=0.05")

############################
#### QUESTGION 3 Part f ####
############################
x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
coefficients(fit.all)

y = c(coefficients(fit.all)[2:14])
y # make sure x,y are accord & drop intercept
plot(x, y, ylab = "Multiple", xlab = "Simple")

print("One uni coefficient points changed a lot. 
      The 'nox' coefficients  was over 30 from 
      univariable situation to -10 in Multiplevaribles case. 
      The other coefficient points look fine")
print("The multiple variable approach produced the most accurate reflection of
      the population parameters. Because the simple variable only generate the direct relationship
      between the single variable and the result. The Multiple variable approach contains all 
      the variable, which includes all variable affects on our result y. The multiple variable
      approach comprehense all variables and their effects on y
      so that it can better produce a accurate reflection of population parameters.")

############################
#### QUESTGION 3 Part g ####
############################

str(Boston) # why no chas?
# drop chas variable since it can't fit into a polynomial term.

fstat <- c()
pvalueofFstat <- c()
colnames(listFstatistic)<- c("Predictor","fstat","pvalueofFstat")

#1 zn
lm.zn2 <- lm(crim~poly(zn,3),data=Boston)
anova(lm.zn,lm.zn2)
fstat[1] <- anova(lm.zn,lm.zn2)[2,5]
pvalueofFstat[1] <- anova(lm.zn,lm.zn2)[2,6]

#2 indus
lm.indus2 <- lm(crim ~ poly(indus, 3),data=Boston)
anova(lm.indus,lm.indus2)
fstat[2] <- anova(lm.indus,lm.indus2)[2,5]
pvalueofFstat[2] <- anova(lm.indus,lm.indus2)[2,6]

#3 nox
lm.nox2 <- lm(crim ~ poly(nox, 3),data=Boston)
anova(lm.nox,lm.nox2)
fstat[3] <- anova(lm.nox,lm.nox2)[2,5]
pvalueofFstat[3] <- anova(lm.nox,lm.nox2)[2,6]

# 4 rm
lm.rm2 <- lm(crim ~ poly(rm, 3),data=Boston)
anova(lm.rm,lm.rm2)
fstat[4] <- anova(lm.rm,lm.rm2)[2,5]
pvalueofFstat[4] <- anova(lm.rm,lm.rm2)[2,6]

#5 age
lm.age2 <- lm(crim ~ poly(age, 3),data=Boston)
anova(lm.age,lm.age2)
fstat[5] <- anova(lm.age,lm.age2)[2,5]
pvalueofFstat[5] <- anova(lm.age,lm.age2)[2,6]

#6 dis
lm.dis2 <- lm(crim ~ poly(dis, 3),data=Boston)
anova(lm.dis,lm.dis2)
fstat[6] <- anova(lm.dis,lm.dis2)[2,5]
pvalueofFstat[6] <- anova(lm.dis,lm.dis2)[2,6]

#7 rad
lm.rad2 <- lm(crim ~ poly(rad, 3),data=Boston)
anova(lm.rad,lm.rad2)
fstat[7] <- anova(lm.rad,lm.rad2)[2,5]
pvalueofFstat[7] <- anova(lm.rad,lm.rad2)[2,6]

#8 tax
lm.tax2 <- lm(crim ~ poly(tax, 3),data=Boston)
anova(lm.tax,lm.tax2)
fstat[8] <- anova(lm.tax,lm.tax2)[2,5]
pvalueofFstat[8] <- anova(lm.tax,lm.tax2)[2,6]

#9 ptratio
lm.ptratio2 <- lm(crim ~ poly(ptratio, 3),data=Boston)
anova(lm.ptratio,lm.ptratio2)
fstat[9] <- anova(lm.ptratio,lm.ptratio2)[2,5]
pvalueofFstat[9] <- anova(lm.ptratio,lm.ptratio2)[2,6]

#10 black
lm.black2 <- lm(crim ~ poly(black, 3),data=Boston)
anova(lm.black,lm.black2)
fstat[10] <- anova(lm.black,lm.black2)[2,5]
pvalueofFstat[10] <- anova(lm.black,lm.black2)[2,6]

#11 lstat
lm.lstat2 <- lm(crim ~ poly(lstat, 3),data=Boston)
anova(lm.lstat,lm.lstat2)
fstat[11] <- anova(lm.lstat,lm.lstat2)[2,5]
pvalueofFstat[11] <- anova(lm.lstat,lm.lstat2)[2,6]

#12 medv
lm.medv2 <- lm(crim ~ poly(medv, 3),data=Boston)
anova(lm.medv,lm.medv2)
fstat[12] <- anova(lm.medv,lm.medv2)[2,5]
pvalueofFstat[12] <- anova(lm.medv,lm.medv2)[2,6]

# show table in ascending order of p-value
fstat
pvalueofFstat
predictor <- c("zn", "indus", "nox", "rm", "age", "dis", "rad","tax","ptratio","black","lstat","medv")
tableploy <- cbind(predictor,fstat,pvalueofFstat)
talbeploysort <- tableploy[order(pvalueofFstat),]
talbeploysort

#From the acending P-value listed in the table. We can easitly tell that "black" as predictors, their coefficient is not
#statistically significant; Their p-values are larger than 0.05, which fialed to rejct the null hypothesis. 