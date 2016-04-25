#Read data from homepage
data_1 <- read.table("http://www.maths.lth.se/matstat/kurser/masm22/exercise/dataCDI.txt",header=FALSE,sep="")
colnames(data_1) <- c("id","county","state","area","pop","pop1834","pop65","phys","beds","crimes","higrads","bachelors","poors","unemployed","pci","totinc","region")
attach(data_1)

# 3.1.1
# Unemployment is the response variable
n = length(unemployed)
idx <- which(unemployed >= 8)
hiunemployed <- mat.or.vec(n,1)
hiunemployed[idx] <- 1
hiunemployed <- factor(hiunemployed,labels=c("low","high"))

# Defining regions
regions <- factor(region,labels = c("NE","MW","S","W"))
regions <- relevel(regions,"W")

# Fitting model
model1 <- glm(hiunemployed~regions,family = "binomial")
summary(model1)

# exp(beta)
beta1 <- exp(model1$coefficients)

# confidence interval
ci1 <- confint(model1)
exp(ci1) # Odds Ratio (OR) compared to baseline parameter = West
# The intervals witch includes 1 are not significant. Can also be seen in the summary.

#3.1.2
table1 <- table(regions,hiunemployed)
totW <- 59+18
totNE <- 72 + 31
totMW <- 87 + 21
totS <- 133 + 19
totLow <- 59+72+87+133
totHigh <- 18+31+21+19

# Proportions of high unemployment compared to region
probW = 18/totW
probNE = 31/totNE
probMW = 21/totMW
probS = 19/totS

# ln Odds 
lnOddsW <- log(probW/(1-probW))
lnOddsNE <- log(probNE/(1-probNE))
lnOddsMW <- log(probMW/(1-probMW))
lnoddsS <- log(probS/(1-probS))

# ln Odds Ratio
lnORNE <- lnOddsNE - lnOddsW
lnORMW <- lnOddsMW - lnOddsW
lnORS <- lnoddsS - lnOddsW

# Odds Ratio
ORNE <- exp(lnORNE)
ORMW <- exp(lnORMW)
ORS <- exp(lnORS)

#3.2 Multivariate logistic regression

#3.2.1. Let’s create the variable CRM1000 = 1000  (crimes/popul) 
#that is number of crimes per 1,000 people. Check if CRM1000 alone 
#is relevant to explain the probability of high unemployment by fitting a 
#simple logistic regression model. Now consider the multivariate logistic model 
#having region and crm1000 as covariates, what are your considerations? Discuss in detail.
CRM1000 <- (crimes/pop)/1000
modelCRM <- glm(hiunemployed~CRM1000,family = "binomial")
summary(modelCRM)
#Only intercept has 3 stars significance while CRM1000
#have no star significance, this means that a 
#flat line describes more of the data than the
#covariate CRM1000.

modelCRMextend <- glm(hiunemployed~CRM1000+regions,family = "binomial")
summary(modelCRMextend)

modelCRMextendswitch <- glm(hiunemployed~regions+CRM1000,family = "binomial")
summary(modelCRMextendswitch)

#3.2.2
# Get the covariance matrix
cov <- summary(modelCRMextend)$cov.unscaled
stdint <- sqrt(cov[1,1])
stdb1 <- sqrt(cov[2,2])
stdb2 <- sqrt(cov[3,3])
stdb3 <- sqrt(cov[4,4])
stdb4 <- sqrt(cov[5,5])


#xbeta2 <- predict(modelCRMextend,se.fit=T)
#selogodds<-xbeta2$se.fit # SE for logodds. Want to try "by hand"? get summary(model)$cov.unscaled
#lo<-xbeta2$fit-1.96*selogodds # lower bound
#hi<-xbeta2$fit+1.96*selogodds # upper bound
#### Plot of 95% Conf.int. for log-odds #####################################:
# here we plot the TRUE log(odds) in black, typically unknown in real experiments
#plot(CRM1000)
# here we add the logodds estimated from data (blue)
#lines(CRM1000,xbeta2$fit,col="blue")
#lines(CRM1000,lo,col="blue",lty=2) # lower 95% band
#lines(CRM1000,hi,col="blue",lty=2) # upper 95% band


#3.2.3
#Conduct a likelihood ratio test at 5% significance level for the comparison of the model containing only 
#region as predictor vs the multivariate model. Motivate your conclusions.


-2*(logLik(model1)[1]-logLik(modelCRMextend)[1]) #8.44
# notice I did NOT use "anova" as this tests two models following the same distributions
# but this is not our case. Hence I compute everything by hand

qchisq(1-0.05,1) # 3.84 < 8.44  --> We reject the Null model at 0.05 significance


#3.2.4. Now create suitable subgroups for the crm1000 covariate then, for each subgroup and each region, 
#estimate the observed proportions of “success” (high unemployment), then produce plots comparing such 
#observed proportions with the estimated proportions of success as from the multivariate logistic regression 
#model suggested above.



#Plottning - Byt ut regions mot de nya regions.
#pred1 <- predict.glm(modelCRMextend, data_1,  type = "response")  #Testad med data_1 och det gav samma resultat som nednför.
predhi1 <- predict.glm(modelCRMextend, data=datfr[CRM1000, regions], type = "response")

plot(regions, hiunemployed)
plot(regions, predhi1)


#3.2.5. Some graphical comparisons as from the plots above might not look satisfactory for some regions (also depending on how 
#you created groups for crm1000). Use automatic selection tools to select a new model and produce again the graphical comparisons 
#as in the previous question (do create subgroups for the crm1000 covariate not for other continuous variables that might have entered 
#in the new model).
