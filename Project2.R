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
dataTrash <- data.frame()
agg <- aggregate(region,by=list(Region=regions),FUN=sum) # We want to aggregate to have it as we do in the table1!
agg
