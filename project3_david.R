#Read data from homepage
data_1 <- read.table("http://www.maths.lth.se/matstat/kurser/masm22/exercise/dataCDI.txt",header=FALSE,sep="")
colnames(data_1) <- c("id","county","state","area","pop","pop1834","pop65","phys","beds","crimes","higrads","bachelors","poors","unemployed","pci","totinc","region")
attach(data_1)

## Requires once:
#install.packages("quantreg")
#install.packages("ggplot2")
#install.packages("quantreg")
#and in every session:
library(quantreg)
require(foreign)
require(ggplot2)
require(MASS)
library(quantreg)

crm = (crimes/pop)*1000

###We try to do the same as described below with PCI data below instead of CRM.

#An example (for inspiration): 1 Suppose you are interested in modelling the CRM 1000 variable (number of crimes 
#per 1000 people, defined as (crimes/popul) * 1000) considered in the previous two projects. Since Poisson- and
#NB-distributed random variables take values on the set of non-negative integers, we are required to first approximate
#CRM 1000 to take integer values. For example, create the variable CRM1000* = ceiling(CRM 1000) and, in practice, 
#use CRM1000* as response variable for your analyses.

#***  Consider that unlike in reports, in a presentation you do not have to answer point-by-point to the suggested ideas. 
#***  Explore and try to take initiative.



#1. Do some initial descriptives, for example plot the empirical distribution of CRM1000* and check whether some
#characteristics for the distribution of CRM1000* vary conditionally on the region (i.e. depending on the region).
regions <- factor(region,labels = c("NE","MW","S","W"))
regions <- relevel(regions,"W")

#pci2 = ceiling(pci)
#hist(pci[regions=="W"])
#hist(pci[regions=="MW"])
#hist(pci[regions=="NE"])
#hist(pci[regions=="S"])

#ANVÄNDNING AV GGPLOT2 FÖR SNYGGA GRAFER
dat <-within(data_1, {
  regions <- factor(region,labels = c("NE","MW","S","W"))
  regions <- relevel(regions,"W")
  id <- factor(id)
})
summary(dat)

ggplot(dat, aes(pci, fill = regions)) +
  geom_histogram(binwidth=1000) +
  facet_grid(regions ~. , margins=TRUE, scales="free")


#2. Do you see potential for Poisson regression to be of help in this case or do you foresee problems? Discuss.

#It looks like the data for all the regions might be a good fit for an Poisson distribution.


#3. Regardless of the considerations above, fit a Poisson regression model for the dependence between CRM1000*
#and the U.S. regions. Comment on results. Among several things that could be done, initially you might want to
#compare estimated mean and variance for your responses against the empirical distributions created in point 1.

POI <- glm(pci~regions, family = "poisson")
summary(POI)

#POI2 <- glm(pci~regions+bachelors, family = "poisson")
#summary(POI2)


#Secondarily, separately for each region, superimpose the estimated/fitted Poisson 
#distributions of CRM1000* over the corresponding empirical histograms. [Note that for such a comparison you might want to use 
#hist(...,freq=FALSE) so your histogram shows relative frequencies. This allows a more meaningful comparison with estimated/fitted 
#Poisson distributions.)]



#4. Now fit the same model using NB regression. As compared to Poisson regression, do you think this model is
#more appropriate for our scenario? Why? Plot and compare distributions as in point 3 using the new model.

NB <- glm.nb(pci~regions, data_1)
summary(NB)
summary(POI)
#Plot the same way as in 3.

NB2 <- glm.nb(pci~regions+bachelors, data_1)
summary(NB2)
BIC(NB2)


NB3 <- glm.nb(pci~regions+bachelors, data_1)
summary(NB3)
BIC(NB3)

#5. Use an appropriate test to conclude whether the NB model with region as predictor is more appropriate than
#the corresponding Poisson model.
BIC(POI) #POISSON BIC  = 34 732
BIC(NB)  #NB BIC       =  8 494
BIC(NB2) #NB2 BIC      =  8 177

#NB2 <- update(NB, . ~ . - regions)
#anova(NB, NB2)

-2*(logLik(NB)[1]-logLik(POI)[1])     #-332244.5
qchisq(1-0.05,1)                      # 3.84 with alpha=5%
#Since -332245<3.84 We reject the Poisson model with the new NB model instead, with alpha=5%


#6. (partially shown at lecture when discussing quantiles for discrete distributions) Compute the probability that, for a
#generic county in the Western region, CRM1000* is between, say, 60 and 80 using the estimated Poisson and NB models.
#Do you find any striking difference?

y <- seq(0,40000,1000)
POIcoeff <- POI$coefficients
mu_hat_POI = exp(POIcoeff[1])

#Likelyhood that a West region has 18k-10k PCI:
pPOI <- (ppois(18000,mu_hat_POI)-ppois(10000,mu_hat_POI))*100 

#For plotting the probability density function 
Py<-dpois(y,mu_hat_POI, log = FALSE)
#Py<-dpois(y,mu_hat_POI, log = FALSE)
plot(y, Py, type="h",ylim=c(0,0.0035), xlim=c(0,40000),xlab="y",ylab="P(Y=y)", main="Poisson distributions")
sum(Py)
sum(Py[0:40000])

#Py<-(dpois(pci[regions=="W"],mu_hat_POI))
#plot(pci[regions=="W"], Py, type="h",ylim=c(0,0.0015), xlim=c(0,40000),xlab="y",ylab="P(Y=y)", main="Poisson distributions")
#sum(Py)
#sum(Py[10000:19000])
#Poisson distribution for the Negative Binomial model
NBcoeff <- NB$coefficients
mu_hat_NB = exp(NBcoeff[1])

PyNB <- dnbinom(y,25.28, mu_hat_NB)
plot(y, PyNB, type="h",ylim=c(0,0.0015), xlim=c(0,40000),xlab="y",ylab="P(Y=y)", main="Poisson distributions")


#7. (not explicitly shown at lecture) Compute the probability as in point 7 for when a linear regression model is used,
#with response variable logCRM1000* and region as covariate. Compare with the results obtained in 7.
L <- lm(pci~regions)
summary(L)
pL = pnorm(18000, 18322.58, 444.02)-pnorm(10000, 18322.58, 444.02)

#8. Use the “distribution-free” quantile regression method to compute median criminality conditionally on the several regions
#and check if this is any similar to the mean criminality as estimated with linear regression and NB
#regression, for the several regions. In addition check that the 2.5-97.5th quantiles are similar (or not) to the 95%
#confidence bounds returned by linear and/or NB regression.

## Quantile regression:
RQ <- rq(pci ~ regions, tau=c(.1,.5,.9))
## See help(predict.rq) for more info:
yRQ <- predict(RQ)
plot(regions, yRQ)
lines(pci,yRQ[,1],col="red")
lines(pci,yRQ[,2],col="red")
lines(pci,yRQ[,3],col="red")

sum <- summary(RQ)
sum
## Extract coefficients:
beta <- coef(RQ)
beta
## Extract residuals:
res <- resid(RQ)
res




#9. Expand on the suggestions above by considering multivariate models to compute similar predictions.



#10. ...more ideas...
