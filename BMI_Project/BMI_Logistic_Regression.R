# Assignment 4 Data Analytics

# Descriptive Statistics
mean(nhis_2009$female) # 0.55

summary(nhis_2009$age) # mean 47.25, median 46
sd(nhis_2009$age) # 17.82

summary(nhis_2009$exfreqwR) # mean 5.30, median 3.00
sd(nhis_2009$exfreqwR) # 7.03
max(nhis_2009$exfreqwR) # 84

plot(hist(nhis_2009$bmiC))
ggqqplot(nhis_2009$bmiC)

# Regression Analyses
logit1 <- glm(bmi2 ~ female + age + exfreqwR, data = nhis_2009, 
                   family = binomial(link = logit))
summary(logit1)
coeftest(logit1, vcov = vcovHC(logit1, type = "HC1"))
# In summary, the model suggests that age and exercise frequency have a 
# statistically significant effect on obesity, while the effect of gender 
# (female) is not statistically significant.
logit1.or <- exp(cbind(OR = coef(logit1), confint(logit1)))
round(logit1.or, digits=4)
# female
# As female increases by 1 unit, odds of being obese increase by a factor of 1.0 
# (%change = 0% [100*(1.00-1)])

# age 
# As age increases 1 year, odds of being obese stay constant
# (%change=- 0% [100*(1.00-1)]=)

# exfreqwR
# As excersize frequency per week increases by 1 workout, odds of being obese
# decrease by a factor of 0.96 (%change = -4% [100*(0.96-1)])

PseudoR2(logit1, which=NULL)
# Bad fit

plot(Effect("exfreqwR", logit1))
# Include this plot in your report and incorporate it into the interpretation
     
#2b
logit2 <- glm(bmi2 ~ female + exfreqwR + age + I(age^2), data = nhis_2009, 
              family = binomial(link = logit))
coeftest(logit2, vcov = vcovHC(logit2, type = "HC1"))
PseudoR2(logit2)

# Test fit of models
waldtest(logit1, logit2, test = "F")

# p-value was 2.2e-16, indicating OVB and declaring model 2 to be more robust

logit2.or <- exp(cbind(OR = coef(logit2), confint(logit2)))
round(logit2.or, digits=4)
# female
# As female increases by 1 unit, odds of being obese increase by a factor of 1.02
# (%change = 2% [100*(1.02-1)])

# exfreqwR
# As excersize frequency per week increases by 1 workout, odds of being obese
# decrease by a factor of 0.96 (%change = -4% [100*(0.96-1)])

# age 
# As age increases by 1 year, odds of being obese increase by a factor of 1.10
# (%change=- 10% [100*(1.10-1)])

#age^2
# As age^2 increases by 1 year, odds of being obese stay constant
# (%change=- 0% [100*(1.00-1)])

# The effects of female did change slightly in this model

plot(Effect("age", logit2))

#2C
# ordinal logit
nhis_2009$bmiC = as.factor(nhis_2009$bmiC)
is.factor(nhis_2009$bmiC)
ordinallogit <- polr(bmiC ~ female + exfreqwR + age + I(age^2), data = nhis_2009
                     , Hess = TRUE)
summary(ordinallogit)
coeftest(ordinallogit)
exp(coef(ordinallogit))
brant(ordinallogit)
# omnibus rejects the null hypothesis which is not good

#multinomial logit
nhis_2009$bmiC2 <- relevel(nhis_2009$bmiC, ref = 4)
multinomiallogit <- multinom(bmiC2 ~ female + exfreqwR + age + I(age^2), data = nhis_2009)
summary(multinomiallogit)
multinomiallogit_zs <-summary(multinomiallogit)$coefficients/summary(multinomiallogit)$standard.errors
multinomiallogit_ps <- (1-pnorm(abs(multinomiallogit_zs), 0, 1))*2 
print(multinomiallogit_ps)

multinomiallogit.or <- exp(cbind(OR = coef(multinomiallogit), confint(multinomiallogit)))
round(multinomiallogit.or, digits=4)

plot(Effect("exfreqwR", multinomiallogit))
