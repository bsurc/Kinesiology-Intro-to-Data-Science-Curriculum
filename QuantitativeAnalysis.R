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

ggplot(dat, aes(x=Treatment, y=Variable_1, color=Sex))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic()


fit <- aov(Variable_1 ~ Treatment, data=dat)
summary(fit)


#linear model
fit <- lm(Variable_1 ~ Treatment, data=dat)
summary(fit)
plot(fit)

#lm with both continuous
ggplot(dat, aes(x=Variable_4, y=Variable_1))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()


fit <- lm(Variable_1 ~ Variable_4, data=dat)
summary(fit)

#now control for sex
ggplot(dat, aes(x=Variable_4, y=Variable_1, color=Sex))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

fit <- lmer(Variable_1 ~ Variable_4+(Variable_4|Sex), data=dat)
summary(fit)

#show this
#http://mfviz.com/hierarchical-models/



  