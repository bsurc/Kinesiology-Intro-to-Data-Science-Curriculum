library(tidyverse)
dat<-read_csv("~/KineTeaching/Data/Manuscript_Data.csv")
head(dat)
View(dat)
library(ggthemes)
#distribution of data

#make a very simple plot
ggplot(dat, aes( x=`20kg_PSvGRF_SS`))+
  geom_histogram()+theme_few()

#try to change the number of bins
ggplot(dat, aes( x=`20kg_PSvGRF_SS`))+
  geom_histogram(bins=10)+theme_few()

#lets try 100
ggplot(dat, aes( x=`20kg_PSvGRF_SS`))+
  geom_histogram(bins=100)+theme_few()

#try to make it red..or whatever color you want
ggplot(dat, aes( x=`20kg_PSvGRF_SS`))+
  geom_histogram(color=red)+theme_few()

#red needs to be in quotes
ggplot(dat, aes( x=`20kg_PSvGRF_SS`))+
  geom_histogram(color='red')+theme_few()

#fill is what we are looking for. Also change to a better bin number
ggplot(dat, aes( x=`20kg_PSvGRF_SS`))+
  geom_histogram(fill='red', bins=10)+theme_few()

#Add axis labels
ggplot(dat, aes( x=`20kg_PSvGRF_SS`))+
  geom_histogram(fill='red', bins=10)+theme_few()+
xlab("Something I Don't Understand")+
  ylab("Number of Observations")

#add a title
ggplot(dat, aes( x=`20kg_PSvGRF_SS`))+
  geom_histogram(fill='red', bins=10)+theme_few()+
  xlab("Something I Don't Understand")+
  ylab("Number of Observations")+
  ggtitle("Good Title")

#fix all of the text 
ggplot(dat, aes( x=`20kg_PSvGRF_SS`))+
  geom_histogram(fill='red', bins=10)+theme_few()+
  xlab("Something I Don't Understand")+
  ylab("Number of Observations")+
  ggtitle("Good Title")+
theme(text=element_text(size=20),
      axis.text = element_text(size=20, color="black"),
      plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))

#The y labels doesn't make sense for count data, fix that
ggplot(dat, aes( x=`20kg_PSvGRF_SS`))+
  geom_histogram(fill='red', bins=10)+theme_few()+
  xlab("Something I Don't Understand")+
  ylab("Number of Observations")+
  ggtitle("Good Title")+
  theme(text=element_text(size=20),
        axis.text = element_text(size=20, color="black"),
        plot.title = element_text(lineheight=.8, face="bold",hjust=0.5))+
  scale_y_continuous(breaks=c(1,3, 5, 7,9))



#lets do a more complicated plot...We will see that it's not actually much more complicated
#catagorical by continuous

ggplot(dat, aes(x=as.character(Sex), y=`20kg_PSvGRF_SS`))+
  geom_boxplot()+theme_few()

#Make it the id instead. Let's use point since there's
#no distribution. Boxplots wouldnt make sense
ggplot(dat, aes(x=as.character(Subject), y=`20kg_PSvGRF_SS`))+
  geom_point()+theme_few()

#change the shape
ggplot(dat, aes(x=as.character(Subject), y=`20kg_PSvGRF_SS`))+
  geom_point(shape=4)+theme_few()

#densities by gender
#also using p to layout plotting environment is standard
p <- ggplot(dat, aes(`20kg_PSvGRF_SS`))
p + geom_density(aes(fill=as.character(Sex)), alpha=1/4)+
  theme_bw()+ #show color brewer
  scale_fill_manual(values=c("#1f78b4","#b2df8a"),labels=c("Male","Female"),
                     name=("Gender")) 

#continuous by continuous

ggplot(dat, aes(x= `20kg_LR_SS`, y=`20kg_PSvGRF_SS`))+
  geom_point()

#this is ok, lets add a trend line
ggplot(dat, aes(x= `20kg_LR_SS`, y=`20kg_PSvGRF_SS`))+
  geom_point()+
  geom_smooth(method= "lm")+
  theme_classic()

#try to make the dots bigger
ggplot(dat, aes(x= `20kg_LR_SS`, y=`20kg_PSvGRF_SS`))+
  geom_point(size=5)+
  geom_smooth(method= "lm")+
  theme_classic()

#make the points by sex
ggplot(dat, aes(x= `20kg_LR_SS`, y=`20kg_PSvGRF_SS`))+
  geom_point(aes(color=Sex))+
  geom_smooth(method= "lm")+
  theme_classic()

#well that didnt work, it's got to be catgorical!
ggplot(dat, aes(x= `20kg_LR_SS`, y=`20kg_PSvGRF_SS`))+
  geom_point(aes(color=as.character(Sex)))+
  geom_smooth(method= "lm")+
  theme_classic()

#now we need to look at the trend lines by gender too
ggplot(dat, aes(x= `20kg_LR_SS`, y=`20kg_PSvGRF_SS`))+
  geom_point(aes(color=as.character(Sex)))+
  geom_smooth(aes(color=as.character(Sex)),method= "lm")+
  theme_classic()

#lets make the legend better
ggplot(dat, aes(x= `20kg_LR_SS`, y=`20kg_PSvGRF_SS`))+
  geom_point(aes(color=as.character(Sex)))+
  geom_smooth(aes(color=as.character(Sex)),method= "lm")+
  theme_classic()+ #show color brewer
scale_color_manual(values=c("#1f78b4","#b2df8a"),labels=c("Male","Female"),
                              name=("Gender")) 
  

#faceting
 dat$Treatment<-c(rep("A",9), rep("B",9),rep("C",9),rep("D",9))       
names(dat)

ggplot(dat, aes(x= `20kg_LR_SS`, y=`20kg_PSvGRF_SS`))+
  geom_point(aes(color=as.character(Sex)))+
  geom_smooth(aes(color=as.character(Sex)),method= "lm")+
  facet_wrap( ~ Treatment, ncol=2)+
  theme_bw()+ #show color brewer
  scale_color_manual(values=c("#1f78b4","#b2df8a"),labels=c("Male","Female"),
                     name=("Gender"))


#same thing with density plots 
#change x variable so it looks better
p <- ggplot(dat, aes(`25kg_AnkleStiff_SS`))
p + geom_density(aes(fill=as.character(Sex)), alpha=1/4)+
  facet_wrap( ~ Treatment, ncol=2)+
  theme_bw()+ #show color brewer
  scale_fill_manual(values=c("#1f78b4","#b2df8a"),labels=c("Male","Female"),
                    name=("Gender")) 


#activity: Make any plot, show your partner, have them write the code that think was used to make it