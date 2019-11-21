#Primer on statistics
males<-dat_simple%>%
  filter(Sex=="Male")

females<-dat_simple%>%
  filter(Sex=="Female")

#simple t.test
t.test(males$Variable_1,females$Variable_1)

#try using the different variables just for fun
t.test(males$Variable_1,females$Variable_2)


#Now try a linear model
fit<-lm(Variable_2~Sex, data=dat_simple)
summary(fit)


fit<-lm(Variable_2~Treatment, data=dat_simple)
summary(fit)














