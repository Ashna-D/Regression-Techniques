path1 <- "Data_RAssignment1.csv"
data <- read.csv(path1)
install.packages("tidyverse")
library(tidyverse)
attach(data)
data
tail(data)

#1. First Method- Randomised Control Trial 
install.packages("dplyr")                
library("dplyr")  


#are there enough subjects in both groups?
data %>%
  count(Mother_Smoking) %>%
  mutate(prop = n / sum(n))

#Is dataset balanced?
data %>%
  group_by(Mother_Smoking) %>%
  summarize(avg_m_edu = mean(Mother_edu),	
            avg_f_edu = mean(Father_edu),
            avg_f_smoke = mean(Father_Smoking),
            avg_percapY =mean(Monthly_income),
            avg_m_health = mean(Mother_health))

#the dataset is not balanced

#Average treatment effect
data %>%
  group_by(Mother_Smoking) %>%
  summarize(avg_childwt = mean(Child_weight))


#Second Method- Regression
reg0 <- lm(Child_weight~Mother_Smoking)
summary(reg0)

reg2 <- lm(Child_weight~ Mother_Smoking+Mother_edu+Mother_health+Father_edu+Father_Smoking+Monthly_income)
summary(reg2)

reg1 <- lm(Child_weight~Mother_Smoking + Mother_edu + Monthly_income + Mother_health)
summary(reg1)

#Testing for heteroskedasticity 
install.packages('lmtest')
library(lmtest)
bptest(reg1)

#Testing for multicollinearity
install.packages("car")
library(car)
vif(reg1)

#model explained slightly better 

#Third Method- Propensity Score Matching

#Regression- try to control variables affecting outcome. In matching, we control
#variables affecting treatment itself. 
install.packages("MatchIt")
library(MatchIt)

pscores.model = glm(Mother_Smoking~Mother_edu+Mother_health+Father_edu+Father_Smoking+Monthly_income) 
summary(pscores.model)

pscores.model1 = glm(Mother_Smoking~Mother_edu+Mother_health+Father_edu+Monthly_income) 
summary(pscores.model1)

pscores.model2 = glm(Mother_Smoking~Mother_health+Father_edu+Father_Smoking+Monthly_income)
summary(pscores.model2)

psm.score = data.frame(pr_score = predict(pscores.model, type = "response"), training = pscores.model$model$Mother_Smoking)
View(psm.score)

match = matchit(Mother_Smoking~Mother_edu+Mother_health+Father_edu+Father_Smoking+Monthly_income, method = "nearest", data = data) 
summary(match)
treat_matrix = match$match.matrix
treat_matrix
plot(match)
dta.match = match.data(match)
dim(dta.match)
head(dta.match)

#Estimate TE- difference in means 
intro_smoke = subset(dta.match, Mother_Smoking ==1)
control = subset(dta.match, Mother_Smoking ==0)
mean(intro_smoke$Child_weight)- mean(control$Child_weight)

#Regression to see significance
bivar_reg = lm(Child_weight~Mother_Smoking, data = dta.match)
summary(bivar_reg)
#statistically significant difference in average birth weight of child. If in treatment group, birth weight 62 units less

#Selection bias- difference between raw data and matched data through regression 
bivar_reg2 = lm(Child_weight~Mother_Smoking, data = data)
bivar_reg2$coefficients[2] - bivar_reg$coefficients[2]
#slight overestimation 