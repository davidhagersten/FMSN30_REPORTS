#Read data from homepage
data_1 <- read.table("http://www.maths.lth.se/matstat/kurser/masm22/exercise/dataCDI.txt",header=FALSE,sep="")
colnames(data_1) <- c("id","county","state","area","pop","pop1834","pop65","phys","beds","crimes","higrads","bachelors","poors","unemployed","pci","totinc","region")
attach(data_1)
#3.1 Plot crimes
CRM1000 <- (crimes/pop)*1000
boxplot(CRM1000~state, data=data_1, notch=FALSE)
#plot(id,CRM1000)

#
pairs(~county+state+area+pop+pop1834)


#3.2.1 Active physicians
pairs(~phys+pop+beds+totinc)
#Alla variablerna verkar korrelera med varandra på ett linjärt sätt.

#3.2.2 Tydligt enkelt linjärt samband

#3.2.3 Enkel linjär modell
physmodel <- lm(phys~totinc)
summary(physmodel)
plot(totinc, phys)
abline(physmodel)

#Enkel log modell
physmodellog <- lm(log(phys)~log(totinc))
summary(physmodellog)
plot(log(totinc), log(phys))
abline(physmodellog)


#3.2.4.1 T-Test av covarianser p-värdet


#3.2.4.2 Randomness of errors
#Test of randomness of errors - linjär model
e = phys - physmodel$fit
plot(e)
abline(h=0)

#Test of randomness of errors - log model
elog = physmodellog$res
plot(elog)
abline(h=0)

#Auto-correlation av residualer - linjär model
acf(e)

#Auto-correlation av residualer - log model
acf(elog)

#Kolla om fel är normalfördelade qqnorm & histogram -  Linjär model
qqnorm(e)
qqline(e)
hist(e)

#Kolla om fel är normalfördelade qqnorm & histogram - log model
qqnorm(elog)
qqline(elog)
hist(elog)


#3.2.5
x_30 = c(1,30000) #Millions of dollares
x_30log = c(1,log(30000))
betahat = physmodel$coefficients
betahatlog = physmodellog$coefficients
y_30 = betahat%*%x_30
y_30 = y_30[1,] 
y_30_log = betahatlog%*%x_30log
y_30_log = y_30_log[1,]

#Confidence intervals
n = length(pop)
s = sqrt(sum(e^2)/(n-2))
slog = sqrt(sum(elog^2)/(n-2))
t_quant = qt(.05,n-2)
ci_y_30 = y_30 + c(1,-1)*t_quant*s*sqrt((1/n)+(30000-mean(totinc)^2)/(sum((totinc-mean(totinc))^2)))
ci_y_30log = y_30_log + c(1,-1)*t_quant*slog*sqrt((1/n)+(30000-mean(totinc)^2)/(sum((totinc-mean(totinc))^2)))
ci_y_30log = exp(ci_y_30log)
#Båda värdena är rimliga och våra modeller bygger på observationer som finns i området runt 30 000 vilket även det gör att vi kan känna oss trygga med resultatet.

# Multiple linear regression
#3.3.1
NE <- which(region==1)
MW <- which(region==2)
S <- which(region==3)
NEregion <- mat.or.vec(n,1)
NEregion[NE] = 1
MWregion <- mat.or.vec(n,1)
MWregion[MW] = 1
Sregion <- mat.or.vec(n,1)
Sregion[S] = 1

#3.3.2
myModel <- lm(phys~pop+totinc+NEregion+MWregion+Sregion)
myModelLog <- lm(log(phys)~log(totinc)+pop+NEregion+MWregion+Sregion)
summary(myModel)
summary(myModelLog)
pairs(~phys+pop+totinc+NEregion+MWregion+Sregion)
# Only the south region seems to affect the number of physicans in a significant way.

#3.3.3 Smaller Model
mySmallModel <- lm(phys~pop+totinc)
summary(mySmallModel)
slm <- step(physmodel,scope = list(lower=physmodel,upper=myModel),direction = "both")
slmLog <- step(physmodellog,scope = list(lower=physmodellog,upper=myModelLog),direction = "both")

#The model using only the parameter with total income discribes almost as much of the data as the 5 parameters together. 
#We should therefore reduce the model to the one with only one parameter, namle totinc.
#The logarithmic model has a way lower AIC than the regular. But we are uncirtain if we can relate the numbers since AIC for log is negative


#3.3.4
eSlm <- slm$res
eSlmLog <- slmLog$res
eMulti = myModel$res
plot(eMulti)
plot(eSlm)
plot(eSlmLog)
abline(h=0)
qqnorm(eSlmLog)
qqnorm(eMulti)
qqline(eMulti)
# Too little randomness in the residuals for the regular. The log looks good

#3.3.5
pairs(~phys+pop65+higrads+pci+bachelors)
pairs(~phys+pop1834+unemployed+poors)
#There seems to be no strong relationship between physicians and the other parameters. We didn't do a proper test just used the 
#pairs() funktion to see if there were a relationship.
