#Name : Ananya Naresh
#Section : Friday 6pm-9.30pm
#Course ID : IE6600 
#CRN : 18587

library(dplyr)
library(data.table)
library(stringr)
library(reshape2)
library(tidyr)

#QUESTION 1
str_to_upper("taxi")
df=filter(nydata, VEHICLE.TYPE.CODE.1=="TAXI"| VEHICLE.TYPE.CODE.2=="TAXI"|VEHICLE.TYPE.CODE.3=="TAXI"|VEHICLE.TYPE.CODE.4=="TAXI"|VEHICLE.TYPE.CODE.5=="TAXI")
#selects only the rows that has taxi in it
r<-select(df, BOROUGH, NUMBER.OF.PERSONS.INJURED)
#selects the BOROUGH and NUMBER OF PERSONS INJURED from the previous data frame containing taxi in one of the columns
ind <- which(!is.na(r$BOROUGH) & !is.na(r$NUMBER.OF.PERSONS.INJURED)) 
#removes all the rows that has NA in r
temp = r[ind,]
f=data.table(temp)
f[,sum(NUMBER.OF.PERSONS.INJURED), by=BOROUGH]
#finds the sum of the persons injured based on the type of the borough


#QUESTION 2

df2<-select(nydata, BOROUGH, NUMBER.OF.PERSONS.KILLED)
ind1 <- which(!is.na(df2$BOROUGH) & !is.na(df2$NUMBER.OF.PERSONS.KILLED))
temp1 = df2[ind1,]
f1=data.table(temp1)
f1[,sum(NUMBER.OF.PERSONS.KILLED), by=BOROUGH]
 
 #QUESTION 3
 
df2<-select(nydata, BOROUGH,LOCATION, ON.STREET.NAME, NUMBER.OF.CYCLIST.INJURED)
l=filter(df2,BOROUGH=="MANHATTAN") 
ind2 <- which(!is.na(l$ON.STREET.NAME) & !is.na(l$LOCATION) & !is.na(l$NUMBER.OF.CYCLIST.INJURED))
temp2 = l[ind2,] 
f2=data.table(temp2)
s=f2[, sum(NUMBER.OF.CYCLIST.INJURED),  by=ON.STREET.NAME]
s2<-s[order(-V1),] 
#the - is used to arrange it in descending order
#V1 is the name of the new column created after summing
head(s2,10) 
#filters and shows the top 10 number of cyclists injured 

#QUESTION 4 

Address<-str_c(nydata$BOROUGH,nydata$ZIP.CODE,nydata$ON.STREET.NAME, sep=",")
#creates values that contain the required fields seperated by ,
x<-as.data.frame(Address)
s3 <- na.omit(x)
#removing the NAs

#QUESTION 5

df3<-select(nydata, NUMBER.OF.PERSONS.INJURED,CONTRIBUTING.FACTOR.VEHICLE.1,CONTRIBUTING.FACTOR.VEHICLE.2,CONTRIBUTING.FACTOR.VEHICLE.3,CONTRIBUTING.FACTOR.VEHICLE.4)
l2=filter(df3,CONTRIBUTING.FACTOR.VEHICLE.1=="Fatigued/Drowsy"| CONTRIBUTING.FACTOR.VEHICLE.2=="Fatigued/Drowsy"| CONTRIBUTING.FACTOR.VEHICLE.3=="Fatigued/Drowsy"| CONTRIBUTING.FACTOR.VEHICLE.4=="Fatigued/Drowsy")
p2=sum(l2$NUMBER.OF.PERSONS.INJURED) 
#sums the number of persons injured from the filtered df


#QUESTION 6

df4<-select(nydata,DATE,NUMBER.OF.PERSONS.INJURED,NUMBER.OF.PERSONS.KILLED)
df4$DATE=format(as.Date(df4$DATE, format = "%m/%d/%Y"),"%Y")
#changing the m/d/Y to Y alone (year)
ind3 <- which(!is.na(df4$NUMBER.OF.PERSONS.INJURED))
temp3 = df4[ind3,] 
f3=data.table(temp3)
lm =f3[,sum(NUMBER.OF.PERSONS.INJURED), by=DATE]
#gives sum of number of persons injured segregated by the date
ind4 <- which(!is.na(df4$NUMBER.OF.PERSONS.KILLED))
temp4 = df4[ind4,] 
f4=data.table(temp4)
bs=f4[,sum(NUMBER.OF.PERSONS.KILLED), by=DATE]
names(bs)[2]<-"V2"
#V1 here represents Number of persons injured and V2 represents Number of persons killed
#Renaming to V2 in order to avoid confusion between two V1s
merge(lm,bs)
#final table showing the date, and number of people killed and number of people injured

#QUESTION 7
library(magrittr)
df5<-select(nydata,BOROUGH,CONTRIBUTING.FACTOR.VEHICLE.1,NUMBER.OF.PERSONS.INJURED)
p3=as.matrix(df5)
#converting the df into a matrix
removed_na<-na.omit(df5)
uniquedata<-unique(removed_na)
#presents uniquedata alone
factor<-uniquedata %>%
group_by(BOROUGH,CONTRIBUTING.FACTOR.VEHICLE.1)  %>%
  summarize(no.of.injured.factors=sum(NUMBER.OF.PERSONS.INJURED,na.rm=TRUE))  %>% #sums the people who got injured against BOROUGH and CONTRIBUTING FACTORS
  spread(BOROUGH, no.of.injured.factors)  
  #Takes key and value and spreads it across into the columns
  