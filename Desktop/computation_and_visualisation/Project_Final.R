library(dplyr)
library(ggplot2)
library(gganimate)
 
#converting step into days according to their day value
category1 <- filter(credit_data,step>=1,step<=24) 
category2 <- filter(credit_data,step>=169,step<=193)
category3 <- filter(credit_data,step>=338,step<=362)
category4 <- filter(credit_data,step>=507,step<=531)
merge1 <- merge(category2,category3,all=TRUE)
merge2 <- merge(category3,category4,all=TRUE)
Mon <- merge(merge1,merge2,all=TRUE)
Mon <- merge(category1,Mon,all=TRUE)

category10 <- filter(credit_data,step>=25,step<=48)
category20 <- filter(credit_data,step>=194,step<=218)
category30 <- filter(credit_data,step>=363,step<=387)
category40 <- filter(credit_data,step>=532,step<=556)
merge3 <- merge(category20,category30,all=TRUE)
merge4 <- merge(category30,category40,all=TRUE)
Tues <- merge(merge3,merge4,all=TRUE)
Tues <- merge(category10,Tues,all=TRUE)

category11 <- filter(credit_data,step>=49,step<=72)
category21 <- filter(credit_data,step>=219,step<=243)
category31 <- filter(credit_data,step>=388,step<=412)
category41 <- filter(credit_data,step>=557,step<=581)
merge5 <- merge(category21,category31,all=TRUE)
merge6 <- merge(category31,category41,all=TRUE)
Wed <- merge(merge5,merge6,all=TRUE)
Wed <- merge(category11,Wed,all=TRUE)

category12 <- filter(credit_data,step>=73,step<=96)
category22 <- filter(credit_data,step>=244,step<=268)
category32 <- filter(credit_data,step>=413,step<=437)
category42 <- filter(credit_data,step>=582,step<=606)
merge7 <- merge(category22,category32,all=TRUE)
merge8 <- merge(category32,category42,all=TRUE)
Thurs <- merge(merge7,merge8,all=TRUE)
Thurs <- merge(category12,Thurs,all=TRUE)

category13 <- filter(credit_data,step>=97,step<=120)
category23 <- filter(credit_data,step>=269,step<=293)
category33 <- filter(credit_data,step>=438,step<=462)
category43 <- filter(credit_data,step>=607,step<=631)
merge9 <- merge(category23,category33,all=TRUE)
merge10 <- merge(category33,category43,all=TRUE)
Fri <- merge(merge9,merge10,all=TRUE)
Fri <- merge(category13,Fri,all=TRUE)


category14 <- filter(credit_data,step>=121,step<=145)
category24 <- filter(credit_data,step>=294,step<=318)
category34 <- filter(credit_data,step>=463,step<=487)
category44 <- filter(credit_data,step>=632,step<=656)
merge11 <- merge(category24,category34,all=TRUE)
merge12 <- merge(category34,category44,all=TRUE)
Sat <- merge(merge11,merge12,all=TRUE)
Sat <- merge(category14,Sat,all=TRUE)


category15 <- filter(credit_data,step>=146,step<=169)
category25 <- filter(credit_data,step>=319,step<=343)
category35 <- filter(credit_data,step>=488,step<=511)
category45 <- filter(credit_data,step>=657,step<=680)
merge13 <- merge(category25,category35,all=TRUE)
merge14 <- merge(category35,category45,all=TRUE)
Sun <- merge(merge13,merge14,all=TRUE)
Sun <- merge(category15,Sun,all=TRUE)

Fri$Day <- "Friday"
Sat$Day <- "Saturday"
Mon$Day <- "Monday"
Tues$Day <- "Tuesday"
Wed$Day <- "Wednesday"
Sun$Day <- "Sunday"
Thurs$Day <- "Thursday"
one <- merge(Sun,Sat,all=TRUE)
two <- merge(Thurs,Wed,all=TRUE)
three<- merge(Mon,Tues,all=TRUE)
four <- merge(one,two,all=TRUE)
five <- merge(Fri,three,all=TRUE)
daysplus <- merge(four,five,all=TRUE)

#Separating into weeks
t1 <-filter(credit_data,step<=168) 
t1[,'Week'] <- 1
t2 <- filter(credit_data,step>=169,step<=336)
t2[,'Week'] <- 2
t3 <- filter(daysplus,step>=337,step<505)
t3[,'Week'] <- 3
t4 <- filter(daysplus,step>=506,step<673)
t4[,'Week'] <- 4

mergefin1 <- merge(t1,t2,all=TRUE)
mergefin2 <-merge(t3,t4,all=TRUE)
mergefinal <- merge(mergefin1,mergefin2,all=TRUE)

#SUMMARISE AND GROUP BY FRAUD AND TYPE
summarise(group_by(mergefinal,type,isFraud),amountsum=sum(amount))

#DAYS AND THEIR AMOUNT EACH DAY
ggplot(daysplus,aes(x=Day,y=amount,fill=Day))+geom_point()

#PLOT FOR HOUR VS AMOUNT
ggplot(mergefinal,aes(step,amount, group = Week, color = factor(Week))) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Hour", y = "Amount") +
  theme(legend.position = "top")

#PLOT FOR FRAUDULENT TRANSACTION ENCIRCLE
fraud <- filter(a,isFraud==1)
ggplot(daysplus, aes(step,amount,color=isFraud))+
  geom_point()+
  geom_encircle(aes(step,amount),data=fraud,color='red',size=0.5)+
  labs(subtitle = 'Circling the Fraudulent transactions',y='Amount',x='Step')


#PLOT FOR OLD BALANCE VS NEW BALANCE
ggplot(daysplus,aes(oldbalanceOrig,newbalanceOrig) + geom_point())
       

