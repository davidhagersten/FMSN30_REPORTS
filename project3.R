#Read data from homepage
data_1 <- read.table("http://www.maths.lth.se/matstat/kurser/masm22/exercise/dataCDI.txt",header=FALSE,sep="")
colnames(data_1) <- c("id","county","state","area","pop","pop1834","pop65","phys","beds","crimes","higrads","bachelors","poors","unemployed","pci","totinc","region")
attach(data_1)

#An example (for inspiration): 1 Suppose you are interested in modelling the CRM 1000 variable (number of crimes 
#per 1000 people, defined as (crimes/popul) * 1000) considered in the previous two projects. Since Poisson- and
#NB-distributed random variables take values on the set of non-negative integers, we are required to first approximate
#CRM 1000 to take integer values. For example, create the variable CRM1000* = ceiling(CRM 1000) and, in practice, 
#use CRM1000* as response variable for your analyses.

#***  Consider that unlike in reports, in a presentation you do not have to answer point-by-point to the suggested ideas. 
#***  Explore and try to take initiative.


#1. Do some initial descriptives, for example plot the empirical distribution of CRM1000* and check whether some
#characteristics for the distribution of CRM1000* vary conditionally on the region (i.e. depending on the region).



#2. Do you see potential for Poisson regression to be of help in this case or do you foresee problems? Discuss.



#3. Regardless of the considerations above, fit a Poisson regression model for the dependence between CRM1000*
#and the U.S. regions. Comment on results. Among several things that could be done, initially you might want to
#compare estimated mean and variance for your responses against the empirical distributions created in point 1.

#Secondarily, separately for each region, superimpose the estimated/fitted Poisson 
#distributions of CRM1000* over the corresponding empirical histograms. [Note that for such a comparison you might want to use 
#hist(...,freq=FALSE) so your histogram shows relative frequencies. This allows a more meaningful comparison with estimated/fitted 
#Poisson distributions.)]



#4. Now fit the same model using NB regression. As compared to Poisson regression, do you think this model is
#more appropriate for our scenario? Why? Plot and compare distributions as in point 3 using the new model.




#5. Use an appropriate test to conclude whether the NB model with region as predictor is more appropriate than
#the corresponding Poisson model.



#6. (partially shown at lecture when discussing quantiles for discrete distributions) Compute the probability that, for a
#generic county in the Western region, CRM ∗ 1000 is between, say, 60 and 80 using the estimated Poisson and NB
#models. Do you find any striking difference?



#7. (not explicitly shown at lecture) Compute the probability as in point 7 for when a linear regression model is used,
#with response variable logCRM1000* and region as covariate. Compare with the results obtained in 7.



#8. Use the “distribution-free” quantile regression method to compute median criminality conditionally on the several regions
#and check if this is any similar to the mean criminality as estimated with linear regression and NB
#regression, for the several regions. In addition check that the 2.5-97.5th quantiles are similar (or not) to the 95%
#confidence bounds returned by linear and/or NB regression.



#9. Expand on the suggestions above by considering multivariate models to compute similar predictions.



#10. ...more ideas...
