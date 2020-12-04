library(tidyverse)
library(psych)
library(gridExtra)
library(lm.beta)
library(ggplot2)
library(car)
library(lmtest)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)
library(sandwich)
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

view(data_sample_1)

#checking the description of the data for irregulatities
describe(data_sample_1)
summary(data_sample_1)

#There is a age that is way to high 
#There is a negative household income 
#there is a STAI-trait that is under 20, but 20 is the lowest possible number

#checking histograms for more outliers
hist(data_sample_1$pain)
hist(data_sample_1$age)
hist(data_sample_1$STAI_trait)
hist(data_sample_1$pain_cat)
hist(data_sample_1$cortisol_serum)
hist(data_sample_1$cortisol_saliva)
hist(data_sample_1$mindfulness)
hist(data_sample_1$weight)
hist(data_sample_1$IQ)
hist(data_sample_1$household_income)

data_sample_1 %>% 
ggplot( aes(x = sex)) +
  geom_bar()

# the age and the STAI-trait and look strange, just as expected. 
#The outliers need to be removed, the next step takes out the outliers in age, STAI_trait and household_income

data_sample_1.1 = data_sample_1 %>% 
  mutate( household_income = replace(household_income, household_income == "-3732", NA) ,STAI_trait = replace(STAI_trait, STAI_trait == "3.9", NA), age = replace(age, age == "444", NA))

data_sample_1.1 = na.omit(data_sample_1.1) 
  
#now we check the mutated variables

data_sample_1.1 %>% 
  select(age, STAI_trait,household_income) %>% 
  describe()

hist(data_sample_1.1$age)
hist(data_sample_1.1$STAI_trait)
hist(data_sample_1.1$household_income)

#Everthing looks good now, the n has decreased by 3, because three cases have been excluded from the data set. The data can be used for further analysis.
#Next stept is to look at the realtionship of pain and the variables

data_sample_1.1 %>%
  ggplot() +aes(x = STAI_trait, y = pain) 
+geom_point()
   
mod1 = lm( pain ~ sex + age, data = data_sample_1.1)
mod1

mod2 = lm( pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum  + cortisol_saliva + mindfulness ,data = data_sample_1.1 )
mod2

#checking the models for possible outliers once more using Cook's distance with the  the traditional 4/n criterion
4/ nrow(data_sample_1.1)
#model 1
mod1 %>%plot(which = 4)  
data_sample_1.1 %>% slice( c(99, 126, 139))
#model 2
mod2 %>%plot(which = 4)
data_sample_1.1 %>% slice( c(68, 99, 112))

# checking the assumptions of the models
#normality of residuals 
mod1 %>% plot(which = 2)
mod2 %>% plot(which = 2)

describe(residuals(mod1))
describe(residuals(mod2))
#normality for both models can be assumed

#linearity of the models
#model1
mod1 %>% residualPlots() 
#model2 
mod2 %>% residualPlots() 
# results of the models
#linearity for both models can be assumed 

#homoscedacity 
#model 1
mod1 %>% plot(which = 3)
mod1 %>% ncvTest()
mod1 %>% bptest()
#model 2
mod2 %>% plot(which = 3)
mod2 %>% ncvTest()
mod2 %>% bptest()
#homoscedacity for both models can be assumed
# No multicollinearity
#model 1
mod1 %>% vif()
#model 2
mod2 %>% vif()
#cortisl_serum and cortisol_saliva have values that are >3
#the solution to this to remove the cortisol_saliva, as it is not as important as an predictor. This could reduce the multicollinearity

#building model 2.1 and checkingt the assumtions again. 

mod2.1 = lm( pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness ,data = data_sample_1.1 )
mod2.1
# outliers
mod2.1 %>%plot(which = 4)
data_sample_1.1 %>% slice( c(96, 100, 114))
#normality of residuals 
mod2.1 %>% plot(which = 2)
describe(residuals(mod2))
#linearity
mod2.1 %>% residualPlots() 
#homoscedacity 
mod2.1 %>% plot(which = 3)
mod2.1 %>% ncvTest()
mod2.1 %>% bptest()
# No multicollinearity
mod2.1 %>% vif()
#all the results for the dajusted model are acceptable 


# model 1 
summary(mod1)
confint(mod1)
lm.beta(mod1)
AIC(mod1)
#model 2.1
summary(mod2.1)
confint(mod2.1)
lm.beta(mod2.1)
AIC(mod2.1)
#compare AIC
AIC(mod1) - AIC(mod2.1) 

#because the model1 is nested in model2 a comoarison using anova can be computed to compare them
anova(mod1, mod2.1)

tab_model(mod1, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE) 
tab_model(mod2.1,show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE) 

#Assigment 2 
mod_basic = lm( pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income ,data = data_sample_1.1 )
summary(mod_basic)

mod_basic %>%plot(which = 4)
data_sample_1.1 %>% slice( c(3, 103, 114))
#normality of residuals 
mod_basic %>% plot(which = 2)
describe(residuals(mod_basic))
#linearity
mod_basic %>% residualPlots() 
#homoscedacity 
mod_basic %>% plot(which = 3)
mod_basic %>% ncvTest()
mod_basic %>% bptest()
# No multicollinearity
mod_basic %>% vif()
#All results look good, the modelis accepteable

mod_back =step(mod_basic, direction = "backward")
summary(mod_back)

AIC(mod_basic)
AIC(mod_back)
AIC(mod2.1)
#compare AIC
AIC(mod2.1) - AIC(mod_back)

#No ANOVA because not testes

tab_model(mod2.1, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
tab_model(mod_back, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE) 

data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

#check the data
describe(data_sample_2)
#mindufness dude is waaaay off, need to check that

pred_test <-predict(mod2.1, data_sample_2)
pred_test_back <-predict(mod_back, data_sample_2)
# now we calculate the sum of squared residuals
RSS_test =sum((data_sample_2[, "pain"] - pred_test)^2)
RSS_test_back =sum((data_sample_2[, "pain"] - pred_test_back)^2)
RSS_test
RSS_test_back
RSS_test_back - RSS_test

#It reveals that the backward regression model has more error than the original model 


#Assignment 3

data_sample_3 = read.csv("https://tinyurl.com/ha-dataset3")
data_sample_4 = read.csv("https://tinyurl.com/ha-dataset4")

#function to extract standardized beta coefficients from linear mixed models
stdCoef.merMod <-function(object) {sdy <-sd(getME(object,"y"))sdx <-apply(getME(object,"X"), 2, sd)sc <-fixef(object)*sdx/sdyse.fixef <-coef(summary(object))[,"Std. Error"]se <- se.fixef*sdx/sdyreturn(data.frame(stdcoef=sc, stdse=se))}

view(data_sample_3)
view(data_sample_4)

summary(data_sample_3)
describe(data_sample_3)

summary(data_sample_4)
describe(data_sample_4)

# take neagtive income out again and the person that is to mindfull

data_sample_3 = data_sample_3 %>% 
  mutate( household_income = replace(household_income, household_income == "-6994", NA))

data_sample_3 = data_sample_3 %>% 
  mutate( sex = replace( sex, sex == "femlae" , "female"))

data_sample_4 = data_sample_4 %>% 
  mutate( household_income = replace(household_income, household_income == "-23482", NA)) 
          
data_sample_4 = data_sample_4 %>% 
  mutate( household_income = replace(household_income, household_income == "-3409", NA))

data_sample_4 = data_sample_4 %>% 
  slice(-80)

data_sample_3 = na.omit(data_sample_3) 
data_sample_4 = na.omit(data_sample_4) 

#final overlook on the data
summary(data_sample_3)
describe(data_sample_3)

summary(data_sample_4)
describe(data_sample_4)

hist(data_sample_3$household_income)
hist(data_sample_4$household_income)

data_sample_3%>%
  mutate(hospital =factor(hospital))
str(data_sample_3)

data_sample_4%>%
  mutate(hospital =factor(hospital))

data_sample_3%>%
  ggplot()+aes(y = pain, x = pain_cat)+
geom_point(aes(color = hospital), size = 4)+
geom_smooth(method = "lm", se = F)

int_plot = data_sample_3%>%
  ggplot() +aes(y = pain, x = pain_cat, color = hospital)+
  geom_point(size = 4)+
  geom_smooth(method = "lm", se = F, fullrange=TRUE)
int_plot

mod_rnd_int =lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness+(1|hospital), data = data_sample_3)
summary(mod_rnd_int)

AIC(mod2.1)
cAIC(mod_rnd_int)$caic
anova(mod2.1, mod_rnd_int)

r2beta(mod_rnd_int, method = "nsj", data = data_sample_3)

r.squaredGLMM(mod_rnd_int)

#to report values
summary(mod_rnd_int)
confint(mod_rnd_int)
stdCoef.merMod(mod_rnd_int)

#or this
tab_model(mod_rnd_int, mod2.1, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE) 


#predict pain in the other sample
pred_test2 <-predict(mod_rnd_int, data_sample_4, allow.new.levels = TRUE)
# now we calculate the sum of squared residuals
RSS_test2 =sum((data_sample_4[, "pain"] - pred_test2)^2)
RSS_test2

mod_mean = lm(pain ~1, data = data_sample_4)

TSS =sum((data_sample_4$pain- predict(mod_mean))^2)
TSS

RSS_test_back
R2 = 1-(RSS_test2/TSS)
R2

# building a new model with random slope and intercept 

mod_rnd_slope =lmer(pain~ cortisol_serum +(cortisol_serum|hospital), data = data_sample_3)

mod_rnd_slope_opt =lmer(pain~ cortisol_serum +(cortisol_serum|hospital), control =lmerControl(optimizer = "Nelder_Mead"), data = data_sample_3)

summary(mod_rnd_slope_opt)

data_sample_3_slope = data_sample_3 %>%
  mutate(pred_slope =predict(mod_rnd_slope_opt))

tab_model(mod_rnd_slope_opt, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE) 


data_sample_3_slope%>%
  ggplot()+aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 4)+
  geom_line(color='black',aes(y=pred_slope, x=cortisol_serum))+
  facet_wrap(~hospital, ncol = 2)                           
                           
                           