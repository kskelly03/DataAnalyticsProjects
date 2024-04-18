library(causaldata)
library(COUNT)
library(ordinal)
library(brant)
library(MASS)
library(rio)
library(glmtoolbox)
library(nnet)

library(foreign)
library(reshape2)

install.packages("foreign")
install.packages("reshape2")

#*************MULTNOMIAL LOGIT REGRESSION  *******************
#FYI to take off scientific notation to get exact p values can....
options(scipen = 999)

ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
# The data set contains variables on 200 students. 
# The outcome variable is prog, program type. NOTE THIS IS TEXT!! Okay here!
# The predictor variables are social economic status, ses, a three-level categorical variable 
# writing score, write, a continuous variable. 

#How to set the reference outcome category
ml$prog2 <- relevel(ml$prog, ref = "academic")
#if this were a numeric variable, use same structure but just put in number

multi1 <- multinom(prog2 ~ ses + write, data = ml)
##Robust standard errors not super easy to do with multnomial data right now
##I will continue to look for a solution and keep you updated
summary(multi1)
#understanding the output
##Using category "academic" as the reference outcome category
##Each numbered row represents 1 logit regression of included category to reference
#So here, first row says "general" so logit comparing outcome general to outcome academic
#"vocation" is comparing outcome vocation to outcome academic
##Giving you the coefficients, NOT the odds ratios and NOT robust standard errors
#Notice it is also not giving you significance levels of variables (just coefficiens and SEs)

#Also, SES is ordinal variable with 3 categories - low, middle, high
##R smart enough to leave out seslow as a reference group to avoid perfect multicollinearity!
##Does this (I think) when recorded as text, not numbers as in this data set

#To get p-values have to standardize
multi1_zs <- summary(multi1)$coefficients/summary(multi1)$standard.errors
#Do a two tailed test since this is just synthetic data 
multi1_ps <- (1-pnorm(abs(multi1_zs), 0, 1))*2 
print(multi1_ps)
#What is significant? 
#General: sesHigh, write
#Vocation: write

#Convert to odds ratios
multi1.or <- exp(cbind(OR = coef(multi1), confint(multi1)))
round(multi1.or, digits=4)

#Interpretation
#General: The odds of someone in a high ses class being in a general program vs. academic
##are 0.31 times lower as compared to someone in a low ses class (or 69%)
###IN OTHER WORDS: high ses more likely to be academic track

#Write (for both)
##Odds ratio less than 1: For each one unit increase in 
####writing score, adds of being in general or vocation progam vs. acacdemic
###decrease by a factor of 0.94 or 0.89 (respectively)
##% change = 6% and 11% (decreases)
 
##Calculate predicted probabilities for each program for each person
head(pp <- fitted(multi1))

#A better way to see this is by using the predictd probs for different vales of write across ses
dwritem1 <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), write = rep(c(30:70),3))
pp.write <- cbind(dwritem1, predict(multi1, newdata = dwritem1, type = "probs", se = TRUE))
lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
ggplot(lpp, aes(x = write, y = probability, colour = ses)) + geom_line() + facet_grid(variable ~
                                                                                        ., scales = "free") 
#INTERPRET!!! 
