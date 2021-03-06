library(tidyverse)
library(lme4)

dat<-read_csv("~/Kinesiology_Teaching/Data/Manuscript_Data.csv")

#Let's make some fake variables to make this a bit more interesting.

dat$Sex<-as.character(dat$Sex)

#now change the vector
dat$Sex<-dplyr::recode(dat$Sex, "1" = "Male", "2" = "Female", .default = NA_character_)

#ok, lets also rename some of the quantitative variables in our dataframe
#so that they are easier to deal with

names(dat)[52]<-"Variable_1"
names(dat)[65]<-"Variable_2"
names(dat)[75]<-"Variable_3"
names(dat)[100]<-"Variable_4"

#dat<-dat%>%select(Subject, Sex, Variable_1,Variable_2,Variable_3,Variable_4)
dat<-dat[,c(1,2,52,65,75,100)]
head(dat)

dat<-arrange(dat, Variable_1)

#Now let's add a treatment category

dat$Treatment<-c(rep("A",12),rep("B",12),rep("C",12))

#Ok, now we should have a treatment variable that may 
#be reflective of increasing Variable1

#Let's compare two things using a t-test

#Lets compare treatment A to treatment c
x<-dat%>%filter(Treatment=="A")
y<-dat%>%filter(Treatment=="C")

#Now that we have two different dataframes, let's compare them
t.test(x$Variable_1, y$Variable_1)

#we see here that our p value is very low 5.724e-10
#So they are significantly different.

#Now, what if we wanted to compare all three treatments (A,B,C)

#It's always a good isea to plot your data before analyzing it

ggplot(dat, aes(x=Treatment, y=Variable_1, color=Sex))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic()

#It looks like treatment makes a big difference here. 
#Let's run an ANOVA to compare 3+ groups

fit <- aov(Variable_1 ~ Treatment, data=dat)
summary(fit)

#Cool...The group's aren't alll the same...We probably already knew that
#There's a phrase that I like that says 
#If there's a real difference, you don't need statistics..this is a good example
#What we can do though is use a linear model to tell us exactly what
#the difference is.


#linear model
fit <- lm(Variable_1 ~ Treatment, data=dat)
summary(fit)
plot(fit)
hist(residuals(fit)) #this should be relatively normal
#lm with both continuous
#why are there only two treatments displayed?

#TreatmentA is the intercept!

fit<-lm(Variable_1~Treatment, data=dat)
summary(fit)


#summary gives us some parameter estimates. What do these mean?

#Let's check out the means to get a better idea
mean(dat[dat$Treatment=="A",]$Variable_1)
mean(dat[dat$Treatment=="B",]$Variable_1)
mean(dat[dat$Treatment=="C",]$Variable_1)

#the parameter estimate (for a standard linear model)
#tells us how much the dependant variable increases as a result of 
#the independant variables

#compare the means from above to the parameter estimates from the model

#What about continuous by continuous
ggplot(dat, aes(x=Variable_4, y=Variable_1))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

#Let's model what we just saw

fit <- lm(Variable_1 ~ Variable_4, data=dat)
summary(fit)
hist(residuals(fit))


#We can look at multivariable regressions too

#Let's look at all of our continuous variables as a predictr for var 1

fit<-lm(Variable_1~Variable_4+Variable_3+Variable_2+Treatment, data=dat)
summary(fit)

#Note that variable 2 has the highest p value
# remove that first
fit<-lm(Variable_1~Variable_4+Variable_3+Treatment, data=dat)
summary(fit)

#Now note that the p values for the other variables has changed
#How have they changed?
#They got lower! That's why it's important to remove the variables one at a time
fit<-lm(Variable_1~Variable_4+Treatment, data=dat)
summary(fit)

#We see now that none of our continuous variables are important

#Treatment is though.



#now control for sex
#You can make a 3d plot or a 
#density plot to plot 2 variable regressions

#We're not going to show these
fit<-lm(Variable_1~Sex+Treatment, data=dat)
summary(fit)

#a better way to do this is to give each sex 
#its own intercept, slope, or both

#let's plot both

ggplot(dat, aes(x=Variable_4, y=Variable_1, color=Sex))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

#we need a new package to fit this model 
fit <- lmer(Variable_1 ~ Variable_4+(Variable_4|Sex), data=dat)
summary(fit)


#If we wanted each sex to just have its own intercept
fit <- lmer(Variable_1 ~ Variable_4+(1|Sex), data=dat)
summary(fit)
plot(fit)
fit<-lm(Variable_1~Variable_4,data=dat)

#show this
#http://mfviz.com/hierarchical-models/

#Make some predictions
#this is a really powerful way to predict things, but also
#to gutcheck yourself!
new <- data.frame(Variable_4 = 25, Treatment="B", Sex="Male")
predict(fit, new)





  