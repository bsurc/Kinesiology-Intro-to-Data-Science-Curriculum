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

# You could imagine that processes like this could get repetitive 
#if you wanted to do this to a ton of datasets or a ton
#of variables within a dataset. One common way to make repetitive tasks faster 
#is to create functions. Let's look at a simple example first


fahr_to_kelvin <- function(temp) {
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}

#CHALLENGE 4
#Write a function called kelvin_to_celsius() that takes a 
#temperature in Kelvin and returns that temperature in Celsius.
#Hint: To convert from Kelvin to Celsius you subtract 273.15


kelvin_to_celsius <- function(temp) {
  celsius <- temp - 273.15
  return(celsius)
}


#Challenge 4
#Now go straight from far to celc
#combine the previous two
fahr_to_celsius <- function(temp) {
  temp_k <- fahr_to_kelvin(temp)
  result <- kelvin_to_celsius(temp_k)
  return(result)
}


#ok, now that we know how to make a function, let's do it with
#our data.

#lets make a function which will calculate and display the standard error
#of any given variable
zzz<-function(dat, Variable_2 , Treatment){
  require("dplyr")
  x<-dat %>%
    dplyr::group_by(dat$Treatment)%>%
    dplyr::summarize(se_Var2 = sd(dat$Variable_2)/sqrt(n()))
  return(x)
}


zzz(dat=dat_simple,Variable_2,Treatment)






####Now let's look at a super common....and super annoying problem
####Long data......

#first, our data is already in a beautiful wide format..
#Let's mess it up


library(tidyr)
data_long <- gather(dat_simple, Measurment, Value, Sex:Treatment, factor_key=TRUE)

#ok, so this is a common way that people output data and 
#it's the worst thing ever for analysis
#why is this not good for analysis?


#this might seem really simple, but knowing that these functions exist
#will save you days of pulling our your hair
data_wide <- spread(data_long, Measurment, Value)


