#Read data from homepage
data_1 <- read.table("http://www.maths.lth.se/matstat/kurser/masm22/exercise/dataCDI.txt",header=FALSE,sep="")
colnames(data_1) <- c("id","county","state","area","pop","pop1834","pop65","phys","beds","crimes","higrads","bachelors","poors","unemployed","pci","totinc","region")
attach(data_1)
#3.1 Plot crimes
CRM1000 <- (crimes/pop)*1000
plot(id,CRM1000)

#
pairs(~county+state+area+pop+pop1834)


#3.2.1 Active physicians
pairs(~phys+pop+beds+totinc)
#Alla variablerna verkar korrelera med varandra på ett linjärt sätt.

#3.2.2 Tydligt enkelt linjärt samband

#3.2.3Enkel linjär modell = Tar sämre hänsyn till lägre värden men tar bättre hänsyn till de 2 höga outliers
physmodel <- lm(phys~totinc)
summary(physmodel)
plot(totinc, phys)
abline(physmodel)

#Enkel log modell = Tar sämre hänsyn till outliers då Residual standard error är högre. Men tar bättre hänsyn till de flesta punkterna då Multiple R-squared är lägre (0.52<0.9) 
#Högt värde på intercept => passar ej för låg inc. countys samt hög inc. countys
physmodellog <- lm(phys~log(totinc))
summary(physmodellog)
plot(log(totinc), phys)
abline(physmodellog)


#3.2.4.1 T-Test av covarianser p-värdet


#3.2.4.2 Randomness of errors
#Test of randomness of errors - linjär model
e = phys - physmodel$fit
plot(e)
abline(h=0)

#Test of randomness of errors - log model
elog = phys - physmodellog$fit
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
x_30 <- data.frame(30000) #Millions of dollares
y_30 <- predict(physmodel,x_30)
y_30_log <- predict(physmodellog, x_30)
