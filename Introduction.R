vector1<-vector(mode='character',length=4)
vector2<-c(2,6,FALSE)
vector2<-c("0",4,7,9)
vector3<-as.numeric(vector2)
vector3

vector4<-as.logical(vector3)

ab_vector<-c('a','b')
abc_vector<-c(ab_vector,"c")

x<-seq(1,10,by=0.1)


typeof(x)
class(x)
cats[,1]

cats$coat<-as.character(cats$coat)
typeof(cats$coat)
install.packages("gapminder")
library(gapminder)

View(gapminder)
str(gapminder)
length(gapminder)
gapminder$cont

library(ggplot2)

ggplot(data=gapminder, aes(x=gdpPercap,y=lifeExp))+
  geom_point()


