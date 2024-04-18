library(causaldata)
library(COUNT)
library(ordinal)
library(brant)
library(MASS)
library(rio)
library(glmtoolbox)
library(nnet)


# *************************LOGISTIC REGRESSION ************************
# **Use thornton_hiv data (Can load here or copy on Canvas)  
data("thornton_hiv")
#https://rdrr.io/cran/causaldata/man/thornton_hiv.html

View(thornton_hiv)  

#comes from an experiment in Malawi looking at whether 
##cash incentives could encourage people to learn the results of their HIV tests.

# got (DV) = whether person got their HIV results 
# tinc = incentive in local currency 
# distvct = distance in km away from reporting center
# age = age 

#no checking for normality here..Doesn't apply with binary variable
#Move from usin lm for linear model to glm (generalized linear model) to estimate logistic
model1 <- glm(got ~ tinc + distvct + age, data= thornton_hiv, family = binomial(link='logit'))
#Notice, function of regression input looks same, but add a "link" or new family to the formula
#This specifies the new regression type (logistic or logit) 

##BUT...can still do robust standard errors!!
coeftest(model1, vcov=vcovHC(model1, type="HC1"))
#All three IVs are significant

#Let's work on interpretation 
#Change to odds ratios
model1.or <- exp(cbind(OR = coef(model1), confint(model1)))
round(model1.or, digits=4)
##Compare to the logit coefficients! Negatives = OR <1; Positives = OR>1

#Start tinc - incentive in local currency 
#As total incentive increases by 1 unit, odds of getting HIV test results
###increase by a factor of 2.83 (%change = 183% [100*(2.83-1)])

#Distvct - distance in km away from reporting center
##As distance to reporting center increase 1 km, odds of getting HIV test,
####decrease by a factor of 0.87 (%change=-13% [100*(0.87-1)]=-13)

##Age - As age increases 1 year, odds of getting HIV test increase by a factor
####of 1.01 or 1% (percentage change)

#Check model fit
PseudoR2(model1, which=NULL)
#So fit not super great - as gets closer to 1, IVs better predictors of DV

#PRE - package too old for use in this version of R...working to find update
# library(DAMisc)
# pre(model1)

#Graph some predicted probabilities
##Get for ALL IVs!!
plot(allEffects(model1)) 
  
 ##SEE REST OF TUCKER SCRIPT FOR OTHER LOGIT AND OTHER CAT REGS!!
