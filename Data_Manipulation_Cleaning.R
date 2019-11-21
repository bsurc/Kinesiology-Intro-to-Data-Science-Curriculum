library(tidyverse)
dat<-read_csv("~/Kinesiology_Teaching/Data/Manuscript_Data.csv")
dat$Treatment<-c(rep("A",9),rep("B",9),rep("C",9),rep("D",9))
#Let's check what kind of variable our "Sex" data is being read as
#How do we want this to be read?

typeof(dat$Sex)
#check what the values are
unique(dat$Sex)
#there are MANY ways to do this.
#I will show you a particularly easy one from the dplyr package
dat$Sex<-as.character(dat$Sex)

#check that it's doing what we want it to do
dplyr::recode(dat$Sex, "1" = "Male", "2" = "Female", .default = NA_character_)

#now change the vector
dat$Sex<-dplyr::recode(dat$Sex, "1" = "Male", "2" = "Female", .default = NA_character_)

#ok, lets also rename some of the quantitative variables in our dataframe
#so that they are easier to deal with

names(dat)[3]<-"Variable_1"
names(dat)[4]<-"Variable_2"

#Lets make it a bit more simple and only include the columns we want. 
#We will do this using dplyr

dat_simple<-select(dat, Subject,Sex,Variable_1,Variable_2,Treatment)

#you can imagine that if we had more than 1 function, nesting them could
#be quite cumbersome. To get around this, we can use pipes

dat_simple<-dat%>%
  select(Subject,Sex,Variable_1,Variable_2,Treatment)

#Imagine we wanted to do the same subsetting above, but also filter by a variable
dat_simple_female<-dat%>%
  select(Subject,Sex,Variable_1,Variable_2,Treatment)%>%
  filter(Sex == "Female")

#CHALLENGE 1
#Write a single command (which can span multiple lines and includes pipes) 
#that will produce a dataframe that has the male values 
#for only Treatment & Subject
dat_Challenge_1<-dat%>%
  filter(Sex == "Male")%>%
  select(Subject,Treatment)
  
#note that the order must be switched from before ^^^

#CHALLENGE 2
#Write a single command (which can span multiple lines and includes pipes) 
#that will produce a dataframe that has the values from treatments C & D
#Where Variable_2 is above 45 and contains just the treatment and variable2 columns  
#note the OR | operator and the %in% operator

dat_Challenge_2<-dat%>%
  filter(Treatment %in% c("C","D"))%>%
  filter(Variable_2 > 45)%>%
  select(Variable_2,Treatment)

dat_Challenge_2<-dat%>%
  filter(Treatment == "C" | Treatment == "D")%>%
  filter(Variable_2 > 30)%>%
  select(Variable_2,Treatment)

#Let's look at grouping 
mean(dat_simple$Variable_2)

#what if we wanted to do this by treatment?
dat_simple%>%
  group_by(Treatment)%>%
  summarize(mean_Var_2 = mean(Variable_2))

#CHALLENGE 3
# do the same thing, but get the median of Variable 1 for males and females
dat_simple%>%
  group_by(Sex)%>%
  summarize(med_Var_1 = median(Variable_1))

#this is cool, but we can do tons of stuff all at the same time
dat_simple%>%
  group_by(Treatment)%>%
  summarize(mean_Var1 = mean(Variable_1),
            sd_Var1 = sd(Variable_1),
            mean_Var2 = mean(Variable_2),
            sd_Var2 = sd(Variable_2))

#A common thing you might want to do with your data is count things. 
#We can do this with dplyr
dat_simple %>%
  filter(Variable_2 >= 45) %>%
  count(Treatment, sort = TRUE)

#notice that the count produces an "n" for us?
#this is useful, it can be used even without calling "count"
#one reason we might want to use the number of observations 
#is in the standard error

dat_simple %>%
  group_by(Treatment) %>%
  summarize(se_Var2 = sd(Variable_2)/sqrt(n()))


####Now let's look at a super common....and super annoying problem
####Long data......

#first, our data is already in a beautiful wide format..
#Let's mess it up


library(tidyr)
data_long <- gather(dat_simple, condition, measurement, Sex:Treatment, factor_key=TRUE)

















