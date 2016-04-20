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
