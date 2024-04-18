# Assignment 2 - Kevin Skelly

# 1. Do 1980 and 2010  adults have different familial and childhood backgrounds?
# Create subsets for 1980 adults and 2010 adults
subset_1980 <- hw1data[hw1data$group == 0,]
subset_2010 <- hw1data[hw1data$group == 1,] 

# Find summary statistics for FAMILY 16 given the two different groups
Mode(subset_1980$FAMILY16) # Mode 1, freq 158
median(subset_1980$FAMILY16) # Median 1

Mode(subset_2010$FAMILY16) # Mode 1, freq 103
median(subset_2010$FAMILY16) # Median 1

# Find summary statistics for  PLACE16 given the two different groups
Mode(subset_1980$PLACE16) # Mode 2, freq 74
median(subset_1980$PLACE16) # Median 2

Mode(subset_2010$PLACE16) # Mode 2, freq 63
median(subset_2010$PLACE16) # Median 3

# Find summary statistics for siblings given the two different subsets
mean(subset_1980$siblings) # 3.68
median(subset_1980$siblings) # median 3
Mode(subset_1980$siblings) # 2, frequency 47
sd(subset_1980$siblings) # 2.50
var(subset_1980$siblings) # 6.25

mean(subset_2010$siblings) # 2.93
median(subset_2010$siblings) # 2
Mode(subset_2010$siblings) # 1, freq 52
sd(subset_2010$siblings) # 2.30
var(subset_2010$siblings) # 5.28

# Find summary statistics for mapaeduc given the two different subsets
mean(subset_1980$mapaeduc, na.rm = TRUE) # 11.68
median(subset_1980$mapaeduc, na.rm = TRUE) # 12
Mode(subset_1980$mapaeduc, na.rm = TRUE) # 12, freq 59
sd(subset_1980$mapaeduc, na.rm = TRUE) # 2.88
var(subset_1980$mapaeduc, na.rm = TRUE) # 8.28

mean(subset_2010$mapaeduc, na.rm = TRUE) # 12.94
median(subset_2010$mapaeduc, na.rm = TRUE) # 12.5
Mode(subset_2010$mapaeduc, na.rm = TRUE) # 12, freq 52
sd(subset_2010$mapaeduc, na.rm = TRUE) # 3.09
var(subset_2010$mapaeduc, na.rm = TRUE) # 9.54

# Examine association between FAMILY16 for cohort
# CramerV since both are categorical
cramerV(hw1data$group, hw1data$FAMILY16) # 0.24
# Same process with PLACE16
cramerV(hw1data$group, hw1data$PLACE16) # 0.11

# Now examine association between siblings and mapaeduc GIVEN group
# Use subsets for this one, also use  Pearson Coefficient since we have ratio
cor(subset_1980$siblings, subset_1980$mapaeduc, use = "complete.obs") # -0.19
cor(subset_2010$siblings, subset_2010$mapaeduc, use = "complete.obs") # -0.20


# 2. Do 1980 and 2010 young adults have different demographic characteristics?
# Find summary statistics for class given the two different groups
Mode(subset_1980$class, na.rm = TRUE) # Mode 2, freq 121
median(subset_1980$class, na.rm = TRUE) # Median 2

Mode(subset_2010$class) # Mode 2, freq 116
median(subset_2010$class) # Median 2

# Find summary statistics for INCOME86 given the two different subsets
mean(subset_1980$INCOME86, na.rm = TRUE) # 14955.38
median(subset_1980$INCOME86, na.rm = TRUE) # 13595
Mode(subset_1980$INCOME86, na.rm = TRUE) # 16994, freq 32
sd(subset_1980$INCOME86, na.rm = TRUE) # 8781.827
var(subset_1980$INCOME86, na.rm = TRUE) # 77120494

mean(subset_2010$INCOME86, na.rm = TRUE) # 12322.86
median(subset_2010$INCOME86, na.rm = TRUE) # 11007.5
Mode(subset_2010$INCOME86, na.rm = TRUE) # 114245 freq 15
sd(subset_2010$INCOME86, na.rm = TRUE) # 10374.8
var(subset_2010$INCOME86, na.rm = TRUE) # 107636460

# Find summary statistics for educ given the two different groups
Mode(subset_1980$educ) # 12, freq 102
median(subset_1980$educ) # 12

Mode(subset_2010$educ) # 12, freq 47
median(subset_2010$educ) # 14

# Find summary statistics for marital given the two different groups
Mode(subset_1980$marital) # 1, freq 98
median(subset_1980$marital) # 3

Mode(subset_2010$marital) # 5, freq 137
median(subset_2010$marital) # 5

# Find summary statistics for children given the two different subsets
mean(subset_1980$children) # 0.55
median(subset_1980$children) # 0
Mode(subset_1980$children) # 0, freq 135
sd(subset_1980$children) # 0.84
var(subset_1980$children) # 0.72

mean(subset_2010$children) # 0.63
median(subset_2010$children) # 0
Mode(subset_2010$children) # 0, freq 122
sd(subset_2010$children) # 0.99
var(subset_2010$children) # 0.97

# Measure of association to see if there is a relationship between cohort
# and class
cramerV(hw1data$group, hw1data$class) # 0.12
# and marital
cramerV(hw1data$group, hw1data$marital) # 0.28

# Now measure correlation for different variables given cohort
# Income(ratio) and Education(ratio)
cor(subset_1980$INCOME86, subset_1980$educ, use = "complete.obs") # 0.25
cor(subset_2010$INCOME86, subset_2010$educ, use = "complete.obs") # 0.22
# Income(ratio) and Children(ratio)
cor(subset_1980$INCOME86, subset_1980$children, use = "complete.obs") # -0.18
cor(subset_2010$INCOME86, subset_2010$children, use = "complete.obs") # -0.19
# Education(ratio) and Children(ratio)
cor(subset_1980$educ, subset_1980$children, use = "complete.obs") # -0.25
cor(subset_2010$educ, subset_2010$children, use = "complete.obs") # -0.41

# Hypothesis Testing
# Null Hypothesis (H0): There is no significant difference in the distribution 
# of political views between 1980 and 2010 young adults
polviews_contingency_table <- table(hw1data$group, hw1data$polviews)
chisq.test(polviews_contingency_table) # p-val: 0.8, accept the null hyp

# Null Hypothesis (H0): There is no significant difference in the distribution 
#of tv hours between 1980 and 2010 young adults
t.test(tvhours ~ group, alternative = "two.sided", var.equal = TRUE, data = hw1data)
# t = 0.98, p-val = 0.32, reject H0
# 95 percent conf int, -0.2132468  0.6399866

# Null Hypothesis (H0): There is no significant difference in the distribution 
#of postlife belief between 1980 and 2010 young adults
postlife_contingency_table <- table(hw1data$group, hw1data$postlife)
chisq.test(postlife_contingency_table) # p-val: 0.26, accept null hyp

# Null Hypothesis (H0): There is no significant difference in the distribution 
#of overall happiness between 1980 and 2010 young adults
happy_contingency_table <- table(hw1data$group, hw1data$happy)
chisq.test(happy_contingency_table) # p-val: 0.76, accept null hyp


# Null Hypothesis (H0): There is no significant difference in the distribution 
#of work hours between 1980 and 2010 young adults
t.test(hours ~ group, alternative = "two.sided", var.equal = TRUE, data = hw1data)
# t = 0.59904, df = 396, p-value = 0.5495


