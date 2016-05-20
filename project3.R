# Project 3 - CRM is done first then PCI


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


## Read data
data_1 <- read.table("http://www.maths.lth.se/matstat/kurser/masm22/exercise/dataCDI.txt",header=FALSE,sep="")
colnames(data_1) <- c("id","county","state","area","pop","pop1834","pop65","phys","beds","crimes","higrads","bachelors","poors","unemployed","pci","totinc","region")
attach(data_1)

## Creating variables
n = 440
crm <- (crimes/pop)*1000
crm <- ceiling(crm)

regions <- factor(region,labels = c("NE","MW","S","W"))
regions <- relevel(regions,"W")

x_w = mat.or.vec(n,1)
x_w[regions=="W"] = 1
x_ne = mat.or.vec(n,1)
x_ne[regions=="NE"] = 1
x_mw = mat.or.vec(n,1)
x_mw[regions=="MW"] = 1
x_s = mat.or.vec(n,1)
x_s[regions=="S"] = 1

regs <- data.frame(crm=crm, West=x_w, NorthEast=x_ne, MidWest=x_mw, South=x_s )
## Plots
#ANVÄNDNING AV GGPLOT2 FÖR SNYGGA GRAFER
dat <-within(data_1, {
  regions <- factor(region,labels = c("NE","MW","S","W"))
  regions <- relevel(regions,"W")
  id <- factor(id)
  crm <- (crimes/pop)*1000
})

ggplot(dat, aes(crm, fill = regions)) +
  geom_histogram(binwidth=5) +
  facet_grid(regions ~. , margins=TRUE, scales="free")


hist(crm[regions=="W"])
hist(crm[regions=="NE"])
hist(crm[regions=="MW"])
hist(crm[regions=="S"])

## Fit model calculate mean from each regon by hand using formula. compute probability with dpois.
modelPo <- glm(crm~regions,family = "poisson",data = regs)
summary(modelPo)

## Calculate estimate of the linear part mu_i = X_i*b
xb_hat <- predict(modelPo,se.fit=T)
mu_hat <- predict(modelPo,type = "response")

## Confints at 95% for x_i*b
cixb_lo<-xb_hat$fit-1.96*xb_hat$se.fit
cixb_hi<-xb_hat$fit+1.96*xb_hat$se.fit

## transform to mu_i
cimu_lo<-exp(cixb_lo)
cimu_hi<-exp(cixb_hi)

## Calculate estimated mu
beta <- modelPo$coefficients
b <- exp(beta)
mu_hatW <- exp(beta[1])
mu_hatNE <- exp(beta[1]+beta[2])
mu_hatMW <- exp(beta[1]+beta[3])
mu_hatS <- exp(beta[1]+beta[4])

## Compute estimated probabilities
x <- seq(0,150,1)
y_hatW <- dpois(x,mu_hatW)
y_hatNE <- dpois(x,mu_hatNE)
y_hatMW <- dpois(x,mu_hatMW)
y_hatS <- dpois(x,mu_hatS)

## Plot empirical distribution and predicted distribution for Poission
hist(crm[regions=="W"],col="blue",ylim=c(0,.05),freq=FALSE,breaks=c(5*6:20))
points(y_hatW,col="red")

hist(crm[regions=="NE"],col="blue",freq=FALSE,breaks=c(25*0:12))
points(y_hatNE,col="red")

hist(crm[regions=="MW"],col="blue",freq=FALSE,breaks=c(12*0:15))
points(y_hatMW,col="red")

hist(crm[regions=="S"],col="blue",freq=FALSE,breaks = c(5*5:30))
points(y_hatS,col="red")


## 4. NB regression
## Fit model
modelNB <- glm.nb(crm~regions)
summodelNB = summary(modelNB)

## Calculate estimated mu
betaNB <- modelNB$coefficients
bNB <- exp(betaNB)
mu_NBW <- exp(betaNB[1])
mu_NBNE <- exp(betaNB[1]+betaNB[2])
mu_NBMW <- exp(betaNB[1]+betaNB[3])
mu_NBS <- exp(betaNB[1]+betaNB[4])

## Compute estimated probabilities

y_NBW <- dnbinom(x,mu=mu_NBW,size = summodelNB$theta)
y_NBNE <- dnbinom(x,mu=mu_NBNE,size = summodelNB$theta)
y_NBMW <- dnbinom(x,mu=mu_NBMW,size = summodelNB$theta)
y_NBS <- dnbinom(x,mu=mu_NBS,size = summodelNB$theta)


## Plot empirical distribution and predicted distribution for NB
hist(crm[regions=="W"],col="blue",ylim=c(0,.05),freq=FALSE,breaks=c(5*6:20))
points(y_hatW,col="red")
points(y_NBW,col="green")

hist(crm[regions=="NE"],col="blue",freq=FALSE,breaks=c(25*0:12))
points(y_hatNE,col="red")
points(y_NBNE,col="green")
lines(y_NBNE,col="green")

hist(crm[regions=="MW"],col="blue",freq=FALSE,breaks=c(12*0:15))
points(y_hatMW,col="red")
points(y_NBMW,col="green")
lines(y_NBMW,col="green")

hist(crm[regions=="S"],col="blue",freq=FALSE,breaks = c(5*5:30))
points(y_hatS,col="red")
points(y_NBS,col="green")
lines(y_NBS,col="green")

## 5. Compare models
-2*(logLik(modelPo)[1]-logLik(modelNB)[1]) # 2750
qchisq(1-0.05,1) # 3.84 < 2750 reject Poisson model at 0.05 significance 

## 6. probability of crm for generic western region between 60-80
probPO <- sum(y_hatW[60:80])
probNB <- sum(y_NBW[60:80])

probPo <- ppois(80,mu_hatW)-ppois(60,mu_hatW)
probNB <- pnbinom(q=80,size=6.627,mu=mu_hatW)-pnbinom(q=60,size=6.627,mu=mu_hatW)

# The Poisson model overestimates alot compared with the NB model, factor of 2.

## 7. Linear regression
modelL <- lm(crm~regions) 
summodelL = summary(modelL)

mu_LW <- modelL$coefficients[1]
mu_LNE <- modelL$coefficients[1]+modelL$coefficients[2]
mu_LMW <- modelL$coefficients[1]+modelL$coefficients[3]
mu_LS <- modelL$coefficients[1]+modelL$coefficients[4]

y_LW <- dnorm(x,mu_LW,summodelL$coefficients[5])

## Plot of linear regression for West
hist(crm[regions=="W"],col="blue",ylim=c(0,.05),freq=FALSE,breaks=c(5*6:20))
points(y_hatW,col="red")
points(y_NBW,col="green")
points(y_LW, col="orange")

## 8. quantile regression
modelQ <- rq(crm~regions, tau=c(.025,.5,.975))
y_rq <- predict(modelQ)
summary(modelQ)

## Extract coefficients
betaQ <- coef(modelQ)
## Extract residuals 
resQ <- resid(modelQ)

## Compare median fron QR with mean in NB and Linear
ciNBW_lo<-exp(log(mu_hatW)-1.96*summodelL$coefficients[5]) # 67.2
ciNBW_hi<-exp(log(mu_hatW)+1.96*summodelL$coefficients[5]) # 56.0













#PCI sammanställning

## Creating variables
n = 440

regions <- factor(region,labels = c("NE","MW","S","W"))
regions <- relevel(regions,"W")

x_w = mat.or.vec(n,1)
x_w[regions=="W"] = 1
x_ne = mat.or.vec(n,1)
x_ne[regions=="NE"] = 1
x_mw = mat.or.vec(n,1)
x_mw[regions=="MW"] = 1
x_s = mat.or.vec(n,1)
x_s[regions=="S"] = 1

#REGULAR R-plots
#regs <- data.frame(pci=pci, West=x_w, NorthEast=x_ne, MidWest=x_mw, South=x_s )
## Plots
#hist(crm[regions=="W"])
#hist(crm[regions=="NE"])
#hist(crm[regions=="MW"])
#hist(crm[regions=="S"])

#USING GGPLOT2 TO GET GOOD LOOKING GRAPHS
dat <-within(data_1, {
  regions <- factor(region,labels = c("NE","MW","S","W"))
  regions <- relevel(regions,"W")
  id <- factor(id)
})
summary(dat)

ggplot(dat, aes(pci, fill = regions)) +
  geom_histogram(binwidth=1000) +
  facet_grid(regions ~. , margins=TRUE, scales="free")

## Fit model calculate mean from each regon by hand using formula. compute probability with dpois.
POI <- glm(pci~regions,family = "poisson",data = regs)
summary(POI)

## Calculate estimate of the linear part mu_i = X_i*b
xb_hat <- predict(POI,se.fit=T)
mu_hat <- predict(POI,type = "response")

## Confints at 95% for x_i*b
cixb_lo<-xb_hat$fit-1.96*xb_hat$se.fit
cixb_hi<-xb_hat$fit+1.96*xb_hat$se.fit

## transform to mu_i
cimu_lo<-exp(cixb_lo)
cimu_hi<-exp(cixb_hi)

## Calculate estimated mu
beta <- POI$coefficients
b <- exp(beta)
mu_hatW <- exp(beta[1])
mu_hatNE <- exp(beta[1]+beta[2])
mu_hatMW <- exp(beta[1]+beta[3])
mu_hatS <- exp(beta[1]+beta[4])

## Compute estimated probabilities
x <- seq(10000,40000,500)
y_hatW <- dpois(x,mu_hatW)
y_hatNE <- dpois(x,mu_hatNE)
y_hatMW <- dpois(x,mu_hatMW)
y_hatS <- dpois(x,mu_hatS)

## Plot empirical distribution and predicted distribution for Poission
hist(pci[regions=="W"],col="blue",freq=FALSE,ylim=c(0,.0001))
points(x, y_hatW,pch = 21, col="red")

hist(pci[regions=="NE"],col="blue",freq=FALSE)
points(x, y_hatNE,col="red")

hist(pci[regions=="MW"],col="blue",freq=FALSE)
points(x, y_hatMW,col="red")

hist(pci[regions=="S"],col="blue",freq=FALSE)
points(y_hatS,col="red")


## 4. NB regression
## Fit model
NB <- glm.nb(pci~regions)
sumNB = summary(NB)

NB2 <- glm.nb(pci~regions+bachelors, data_1)
summary(NB2)
BIC(NB2)

NB3 <- glm.nb(pci~regions+bachelors, data_1)
summary(NB3)
BIC(NB3)

## Calculate estimated mu
betaNB <- NB$coefficients
bNB <- exp(betaNB)
mu_NBW <- exp(betaNB[1])
mu_NBNE <- exp(betaNB[1]+betaNB[2])
mu_NBMW <- exp(betaNB[1]+betaNB[3])
mu_NBS <- exp(betaNB[1]+betaNB[4])

## Compute estimated probabilities

y_NBW <- dnbinom(x,mu=mu_NBW,size = sumNB$theta)
y_NBNE <- dnbinom(x,mu=mu_NBNE,size = sumNB$theta)
y_NBMW <- dnbinom(x,mu=mu_NBMW,size = sumNB$theta)
y_NBS <- dnbinom(x,mu=mu_NBS,size = sumNB$theta)


## Plot empirical distribution and predicted distribution for NB
hist(pci[regions=="W"],col="blue",ylim=c(0,.0001),freq=FALSE)
points(x,y_hatW,col="red")
points(x,y_NBW,col="green")

hist(pci[regions=="NE"],col="blue",freq=FALSE)
points(x,y_hatNE,col="red")
points(x,y_NBNE,col="green")
lines(x,y_NBNE,col="green")

hist(pci[regions=="MW"],col="blue",freq=FALSE)
points(y_hatMW,col="red")
points(y_NBMW,col="green")
lines(y_NBMW,col="green")

hist(pci[regions=="S"],col="blue",freq=FALSE)
points(x,y_hatS,col="red")
points(x,y_NBS,col="green")
lines(x,y_NBS,col="green")

## 5. Compare models

-2*(logLik(POI)[1]-logLik(NB)[1])     #332244.5
qchisq(1-0.05,1)                      # 3.84 with alpha=5%
#Since 3.84<332245 We reject the Poisson model with the new NB model instead, with alpha=5%

#Maybe ANOVA can be used but not necassary
#NB2 <- update(NB, . ~ . - regions)
#anova(NB, NB2)


## 6. probability of pci for generic western region between 18000 $ and 10000 $
probPOI <- ppois(18000,mu_hatW)-ppois(10000,mu_hatW)
probNB <- pnbinom(q=18000,size=sumNB$theta,mu=mu_hatW)-pnbinom(q=10000,size=sumNB$theta,mu=mu_hatW)

# The Poisson model overestimates alot compared with the NB model, factor of 2.

## 7. Linear regression
L <- lm(pci~regions) 
sumL = summary(L)

mu_LW <- L$coefficients[1]
mu_LNE <- L$coefficients[1]+L$coefficients[2]
mu_LMW <- L$coefficients[1]+L$coefficients[3]
mu_LS <- L$coefficients[1]+L$coefficients[4]

y_LW <- dnorm(x,mu_LW,sumL$coefficients[5])

## Plot of linear regression for West
hist(pci[regions=="W"],col="blue",ylim=c(0,.0005),freq=FALSE)
points(y_hatW,col="red")
points(y_NBW,col="green")
points(y_LW, col="orange")

## 8. quantile regression
modelQ <- rq(pci~regions, tau=c(.025,.5,.975))
y_rq <- predict(modelQ)
summary(modelQ)

## Extract coefficients
betaQ <- coef(modelQ)
## Extract residuals 
resQ <- resid(modelQ)

## Compare median fron QR with mean in NB and Linear
ciNBW_lo<-exp(log(mu_hatW)-1.96*sumL$coefficients[5]) # 67.2
ciNBW_hi<-exp(log(mu_hatW)+1.96*sumL$coefficients[5]) # 56.0
