#Capstone Project 
#Fall 2019 
#Sthefano Alvarado
setwd("C:/Users/Teno/Desktop/UCLA DATA SCIENCE/Final")

library(tidycensus)
library(tidyverse)
library(viridis)
library(corrplot)
library(ggplot2)
library(mapview)
library(readxl)
#options(tigris_use_cache = TRUE)

wfdata = read_excel("wfdata.xlsx")
str(wfdata)
wfdata=wfdata[,c(3,7)]
names(wfdata)= c("StoreName","Zip")
wfdata$count = as.numeric(ave(wfdata$Zip,wfdata$Zip, FUN = length)) 
wfdata=wfdata[,c(2,3)]
wfdata= distinct(wfdata)
wfdata$Zip = sprintf("%05d", as.numeric(wfdata$Zip))

median_age= read.csv("ACS_17_5YR_B01002_with_ann.csv")
str(median_age)
median_age= median_age[-c(1),]
median_age= median_age[,c(2,4,6,8)]
names(median_age)= c("Zip","MedianAge","MedianAgeMale","MedianAgeFemale")
median_age$MedianAge= gsub("-",replacement = "NA",median_age$MedianAge)
median_age$MedianAgeFemale= gsub("-",replacement = "NA",median_age$MedianAgeFemale)
median_age$MedianAgeMale= gsub("-",replacement = "NA",median_age$MedianAgeMale)
sum(median_age == "NA")
sum(median_age$MedianAge == "NA")
sum(median_age$MedianAgeMale == "NA")
sum(median_age$MedianAgeFemale == "NA")

median_rent = read.csv("ACS_17_5YR_B25031_with_ann.csv")
median_rent= median_rent[-c(1),]
median_rent= median_rent[,c(2,4)]
names(median_rent)= c("Zip","MedianRent")
median_rent$MedianRent= gsub("-",replacement = "NA",median_rent$MedianRent)
sum(median_rent$MedianRent == "NA")
median_rent$MedianRent= as.numeric(median_rent$MedianRent)
str(median_rent)

median_income = read.csv("ACS_17_5YR_B19013_with_ann.csv")
median_income= median_income[-c(1),]
median_income= median_income[,c(2,4)]
names(median_income) = c("Zip","MedianHouseholdIncome")
str(median_income)
median_income$MedianHouseholdIncome= as.numeric(median_income$MedianHouseholdIncome)
median_income$Zip = sprintf("%05d", as.numeric(median_income$Zip))


popAge= read.csv("ACS_17_5YR_S0101_with_ann.csv")
popAge$Id2 = sprintf("%05d", as.numeric(popAge$Id2))

totalpop= popAge[,c(2:5)]
names(totalpop)= c("Zip","TotalPopulation","MalePopulation","FemalePopulation")
str(totalpop)
c=totalpop[2:4]
corrplot(cor(c), method = "number", type = "upper") 
#high correlation between all of them... Omit Male/Female since pointless
totalpop= totalpop[,-c(3:4)]
#NOTE there are a bunch with zero pop.... maybe remove? Filter them out at end.Only Pop>4K

youthpop= popAge[,c(2,13,15,17,19)]  #15-34yr olds
colnames(youthpop)
#Only keep percents
names(youthpop)=c("Zip","15t19yrs", "20t24yrs","25t29yrs","30t34yrs")   
youthpop$`15t19yrs`= as.numeric(as.character(youthpop$`15t19yrs`))
youthpop$`20t24yrs`= as.numeric(as.character(youthpop$`20t24yrs`))                     
youthpop$`25t29yrs`= as.numeric(as.character(youthpop$`25t29yrs`))                     
youthpop$`30t34yrs`= as.numeric(as.character(youthpop$`30t34yrs`))                     
str(youthpop)
youthpop$peryouthpop= (youthpop$`15t19yrs`+youthpop$`20t24yrs`+youthpop$`25t29yrs`+youthpop$`30t34yrs`)
youthpop= youthpop[,c(1,6)]

middlepop=popAge[,c(2,21,23,25,27,29)]  #35-59
colnames(middlepop)
names(middlepop)=c("Zip","35t39yrs","40t44yrs","45t49yrs" ,"50t54yrs","55t59yrs")
middlepop$`35t39yrs`=as.numeric(as.character(middlepop$`35t39yrs`))
middlepop$`40t44yrs`=as.numeric(as.character(middlepop$`40t44yrs`))
middlepop$`45t49yrs`=as.numeric(as.character(middlepop$`45t49yrs`))
middlepop$`50t54yrs`=as.numeric(as.character(middlepop$`50t54yrs`))
middlepop$`55t59yrs`=as.numeric(as.character(middlepop$`55t59yrs`))
middlepop$permiddlepop=middlepop$`35t39yrs`+middlepop$`40t44yrs`+middlepop$`45t49yrs`+middlepop$`50t54yrs`+middlepop$`55t59yrs`
middlepop= middlepop[,c(1,7)]

oldpop=popAge[,c(2,31,33,35,37,39,41)] #60+
colnames(oldpop)
names(oldpop)=c("Zip","60t64yrs","65t69yrs","70t74yrs","75t79yrs","80t84yrs","85+yrs")
oldpop$`60t64yrs`=as.numeric(as.character(oldpop$`60t64yrs`))
oldpop$`65t69yrs`=as.numeric(as.character(oldpop$`65t69yrs`))
oldpop$`70t74yrs`=as.numeric(as.character(oldpop$`70t74yrs`))
oldpop$`75t79yrs`=as.numeric(as.character(oldpop$`75t79yrs`))
oldpop$`80t84yrs`=as.numeric(as.character(oldpop$`80t84yrs`))
oldpop$`85+yrs`=as.numeric(as.character(oldpop$`85+yrs`))
oldpop$peroldpop=oldpop$`60t64yrs`+oldpop$`65t69yrs`+oldpop$`70t74yrs`+oldpop$`75t79yrs`+oldpop$`80t84yrs`+oldpop$`85+yrs`
oldpop=oldpop[,c(1,8)]

race= read.csv("ACS_17_5YR_B02001_with_ann.csv")
colnames(race)
race= race[,c(2,4,6,8,10,12,14,16,18,20,22)]
names(race)= c("Zip","TP","W","B.A","AI.AL","A","NH.PI","Other","Two","Three","Four")
race$perW= 100*race$W/race$TP
race$perB.A= 100*race$B.A/race$TP
race$perAI.AL= 100*race$AI.AL/race$TP
race$perA= 100*race$A/race$TP
race$perNH.PI= 100*race$NH.PI/race$TP
race$perOther= 100*race$Other/race$TP
race$perThree= 100*race$Three/race$TP
race$perTwo= 100*race$Two/race$TP
race$perFour= 100*race$Four/race$TP
str(race)
racefinal=race[,c(1,12:20)]
racefinal$perMixed.Other= racefinal$perOther+racefinal$perTwo+racefinal$perThree+racefinal$perFour
racefinal=racefinal[,-c(7:10)]
colnames(racefinal)
racefinal$Zip = sprintf("%05d", as.numeric(racefinal$Zip))

mergeA= merge(median_age,median_rent, by = "Zip")
merge.age1= merge(youthpop,middlepop, by="Zip" )
merge.age2= merge(merge.age1,oldpop, by = "Zip")
mergeB= merge(mergeA,merge.age2, by = "Zip") #med.age,med.agemale/female,rent,age breakdown
str(mergeA)
mergeB= mergeB[,-c(3,4)]

mergeC=merge(mergeB,racefinal, by= "Zip", all.x = TRUE) 
mergeD=merge(mergeC,totalpop, by="Zip")
mergeE=merge(mergeD,median_income, by="Zip", all.x = TRUE)


final= merge(mergeE,wfdata, all.x = TRUE)
sum(is.na(final$count)) #32552
32989-32552  #getting 437 here....#SHOULD BE 438
#NOTE: There are 35+ WFstores without RACE data
final$count[is.na(final$count)]= 0
str(final)
#final$Zip = as.numeric(as.character(final$Zip))   #NOTE:your changing the zip here, omit this is you need to backtrack
#final$Zip = sprintf("%05d", as.numeric(final$Zip))
final$MedianAge= as.numeric(final$MedianAge)
final$TotalPopulation=as.numeric(final$TotalPopulation)

finaldata=subset(final,TotalPopulation>4000 & MedianHouseholdIncome>2500)
str(finaldata)
fit1=lm(count~.-Zip, data=finaldata)
summary(fit1) #adj.r = .05536

#Benchmark PerW
fit1a=lm(count~.-Zip-perW, data=finaldata)
summary(fit1a) #adj.r = .05531

#Save data for regression
finaldata1= finaldata[,c(1:15)]

#Prepare for Log. Regression
finaldata$count= as.factor(finaldata$count)
finaldata$count[(finaldata$count==2)]= 1
logit=glm(count~.-Zip, data= finaldata, family = "binomial")
summary(logit) #AIC:2310.5

logit1=glm(count~.-Zip-MedianAge, data= finaldata, family = "binomial", )
summary(logit1) #AIC:2345.7  #2515 (excluding yout/middle/old)

final = finaldata[,c(1:15)]
final$Zip= as.numeric(as.character(final$Zip))
final$Zip = sprintf("%05d", as.numeric(final$Zip))
str(final)
CAfinal= subset(final, Zip>90000 & Zip<97636)
str(CAfinal)

CAfinal$count= as.numeric(as.character(CAfinal$count))
fit2=lm(count~.-Zip, data=CAfinal)
summary(fit2) #.06922

fit2a=lm(count~.-Zip-perW, data=CAfinal)
summary(fit2a) #.06894

#sum(is.na(finaldata1))
#finaldata1= finaldata[,c(1:15)]

CAfinal$count[(CAfinal$count==2)]= 1
CAfinal$count=as.factor(as.character(CAfinal$count))
str(CAfinal)
CAlogit=glm(count~.-Zip, data=CAfinal, family = "binomial")
summary(CAlogit) #380

CAlogit1=glm(count~.-Zip-MedianAge, data=CAfinal, family = "binomial")
summary(CAlogit1) #383.33

CAlogit2=glm(count~.-Zip-peryouthpop, data=CAfinal, family = "binomial")
summary(CAlogit2) #408.44

#Exploring Regressions
str(finaldata1)
finaldata1$count = as.numeric(as.character(finaldata1$count))
fit01=lm(count~.-Zip-MedianHouseholdIncome, data = finaldata1)
summary(fit01) #adj.R:.05476 

fit01a=lm(count~.-Zip-MedianAge, data = finaldata1)
summary(fit01a) #adj.R:.05149 #All sig except NH.PI

fit01b=lm(count~.-Zip-MedianAge-peryouthpop-perW, data = finaldata1)
summary(fit01b) #adj.R:.03643 #All sig except middle/old/ai.al/

fit01c=lm(count~.-Zip-MedianAge-peryouthpop-permiddlepop-peroldpop, data = finaldata1)
summary(fit01c) #adj.R:.03533 #All sig except NHPI

fit01d=lm(count~.-Zip-MedianAge-perW-perB.A-perAI.AL-perA-perMixed.Other-perNH.PI, data = finaldata1)
summary(fit01d) #adj.R:.05061

fit01e=lm(count~.-Zip-MedianAge-peryouthpop-permiddlepop-peroldpop-perW-perB.A-perAI.AL-perA-perMixed.Other-perNH.PI, data = finaldata1)
summary(fit01e) #adj.R:.03158

fit01f=lm(count~.-Zip-peryouthpop-permiddlepop-peroldpop-perW-perB.A-perAI.AL-perA-perMixed.Other-perNH.PI, data = finaldata1)
summary(fit01f) #adj.R:.03151

finallog= finaldata1[,c(1:15)]
str(finallog)
finallog$count[(finallog$count==2)]= 1
sum(finallog$count==1) #436 
str(finallog)
finallog$count=as.factor(finallog$count)
fitl1= glm(count~.-Zip,data=finallog, family = "binomial")
summary(fitl1) #AIC:2310.5 Sig***MedAge,Rent,Totalpop,youth/middle/pop,W/B.A/A/Mixed
fitl1a= glm(count~.-Zip-MedianAge,data=finallog, family = "binomial")
summary(fitl1a) #AIC:2345.7 Sig***MedAge,Rent,Totalpop,youth/middle/pop,W/B.A/A/Mixed
fitl1b= glm(count~.-Zip-peryouthpop-permiddlepop-peroldpop,data=finallog, family = "binomial")
summary(fitl1b) #AIC:2514.7

fitl2= glm(count~+MedianRent+peryouthpop+permiddlepop+peroldpop+TotalPopulation+MedianHouseholdIncome ,data=finallog, family = "binomial")
summary(fitl2) #AIC:2341.6  Sig***:Rent,youth,old,middle,Total (all except income)
str(finallog)
fitl3= glm(count~+MedianRent+TotalPopulation+MedianHouseholdIncome ,data=finallog, family = "binomial")
summary(fitl3) #AIC:2540.4 Sig***Rent,Pop,,Income**
fitl4= glm(count~+MedianAge+MedianRent+TotalPopulation+MedianHouseholdIncome ,data=finallog, family = "binomial")
summary(fitl4) #AIC:2541 Sig***Rent,Pop,,Income** (all except median Age)
fitl5= glm(count~+MedianAge+MedianRent+TotalPopulation+MedianHouseholdIncome ,data=finallog, family = "binomial")
summary(fitl5) #AIC:2541 Sig***Rent,Pop,,Income** (all except median Age)
fitl6= glm(count~.-Zip-MedianAge,data=finallog, family = "binomial")
summary(fitl6) #AIC:2345.7 Sig***Rent,Totalpop,youth/middle/pop,W/B.A/A/Mixed
fitl7= glm(count~.-Zip-MedianAge-MedianRent-TotalPopulation-MedianHouseholdIncome,data=finallog, family = "binomial")
summary(fitl7) #AIC:2610 All Sig except AI.AL
fitl8=glm(count~+peryouthpop+permiddlepop+peroldpop, data=finallog, family = "binomial")
summary(fitl8) #AIC:2668.9


#finallog2=finallog[,c(1:15)]
#finallog2$Zip=as.numeric(as.character(finallog2$Zip))
#str(finallog2)
#finallog2a=subset(finallog2,finallog2$MedianHouseholdIncome>2500)
#str(finallog2a)
#sum(finallog2a$count==1)
#plot_histogram(finallog2a$MedianHouseholdIncome)

#logit=glm(count~.-Zip,data=finallog2a,family="binomial")
#summary(logit) #AIC:2310.5

#logit2=glm(count~.-Zip-MedianAge,data=finallog2a,family="binomial")
#summary(logit2) #AIC:2345.7

#plot_correlation(finallog2a)
#finalStorecount = filter(finallog2a, count > 0)

#finallog2a$count=as.numeric(finallog2a$count)

#finallog2$Zip = sprintf("%05d", as.numeric(finallog2$Zip))


library(ggplot2)
ggplot(final.1, aes(x=Zip, y=count))+
  geom_point(size = 3, color ='blue')+
  geom_line(aes(y = predict(fit03)), color="red")

library(cowplot)


#hist(finaldata$count)
#ggplot(final.1, aes(x = count)) + geom_histogram(stat = "bin", binwidth=)
#names(final.1)
finaldata$count= as.numeric(as.character(finaldata$count))
Storecount = filter(finaldata, count > 0) 
str(Storecount) 
ggplot(Storecount,aes(y = TotalPopulation, x = MedianHouseholdIncome)) + geom_point() #No Real Correlation
ggplot(Storecount,aes(y = TotalPopulation, x = log(MedianHouseholdIncome))) + geom_point() 
#summary(log(Storecount$MedianHouseholdIncome))

ggplot(Storecount,aes(y = TotalPopulation, x = MedianRent)) + geom_point()   #No Real corr
ggplot(Storecount,aes(y = TotalPopulation, x = MedianAge)) + geom_point()    #cluster 30:45yrs
ggplot(Storecount,aes(y = MedianAge, x = MedianRent)) + geom_point()   #No Real corr
ggplot(Storecount,aes(y = MedianHouseholdIncome, x = TotalPopulation)) + geom_point() #No corr.
ggplot(Storecount,aes(y = MedianHouseholdIncome, x = MedianRent)) + geom_point() 
#slight corr from 0-2K #Also:1-2.5K seems to have a pattern worth investigating

ggplot(Storecount,aes(y = peryouthpop, x = MedianAge)) + geom_point() #highlit neg. corr
ggplot(Storecount,aes(y = permiddlepop, x = MedianAge)) + geom_point() #Positive corr 20-45, many  outliers though
ggplot(Storecount,aes(y = peroldpop, x = MedianAge)) + geom_point() #highly pos. corr

ggplot(Storecount,aes(y = peryouthpop, x = MedianHouseholdIncome)) + geom_point() #No corr.
ggplot(Storecount,aes(y = permiddlepop, x = MedianHouseholdIncome)) + geom_point() #No corr.
ggplot(Storecount,aes(y = peroldpop, x = MedianHouseholdIncome)) + geom_point() #No corr.

Storecount2= Storecount[,c(3,13:15)]
plot(Storecount2)  #No real correlations between: TotalPop,Rent,Income

library(viridis)
library(cowplot)
plot_grid(ggplot(Storecount, aes(x=peryouthpop, y=MedianRent, fill=peryouthpop, group=peryouthpop)) + geom_violin()
          +geom_boxplot(width=0.1, fill="white") 
          + labs(title="Rent v YouthPop Frequency Store Count") 
          +guides(fill=FALSE))

#VIOLIN 

ggplot(final, aes(x=count, y=MedianRent)) + 
  geom_violin()
ggplot(Storecount, aes(x=TotalPopulation, y=MedianRent)) + 
  geom_violin() 

ggplot(Storecount, aes(x = MedianHouseholdIncome)) + geom_histogram() #no real correlation,from 0-2.5K there's a spike
ggplot(Storecount, aes(x = MedianRent)) + geom_histogram()  #more signigicant
ggplot(sflogit, aes(x = MedianRent)) + geom_histogram()  #more signigicant

summary(Storecount$MedianRent)
ggplot(Storecount, aes(x = TotalPopulation)) + geom_histogram() 
summary(Storecount$TotalPopulation)


#Age categories
names(Storecount)
ggplot(Storecount, aes(x =peryouthpop )) + geom_histogram() #Between 25-35% is highest
ggplot(Storecount, aes(x =permiddlepop )) + geom_histogram() #Between 35% is highest
ggplot(Storecount, aes(x =peroldpop )) + geom_histogram() #Between 18-22% is highest 
ggplot(Storecount, aes(x =MedianAge )) + geom_histogram() #around 38

#Race categories
ggplot(Storecount, aes(x =perW )) + geom_histogram() #Zips with 75%<  
ggplot(Storecount, aes(x =perB.A )) + geom_histogram() #Zips 15%>  FIND HOW TO SET RANGE
ggplot(Storecount, aes(x =perA )) + geom_histogram() #Zips 15%> noticebly higher count
ggplot(Storecount, aes(x =perAI.AL )) + geom_histogram() #Zips 2%> 
ggplot(Storecount, aes(x =perNH.PI )) + geom_histogram() #Zips 1%>
ggplot(Storecount, aes(x =perMixed.Other )) + geom_histogram() #Zips>20%

plot(final.1$peryouthpop,final.1$count)
plot(Storecount$peryouthpop,Storecount$MedianRent)

plot(Merge2$Per, Merge2$log, col=c, lwd=1, xlab="College Degree %", ylab="Log Median Household Income")
abline(lm(Merge2$log~Merge2$Per,data=Merge2), col="red")         # Add a regression line
abline(fit01a, col="red", lwd=3)

names(racefinal)
names(Storecount)
racesubset= Storecount[,c(7:12,15)]

#install.packages('DataExplorer')
library(DataExplorer)
plot_correlation(Storecount, type="continuous")
plot_correlation(final)
?plot_correlation
plot_histogram(Storecount)

plot_histogram(Storecount$peryouthpop)
plot_histogram(Storecount1$permiddlepop)
plot_histogram(Storecount$peroldpop)

plot_histogram(Storecount$perA)
plot_histogram(Storecount$perMixed.Other)
names(Storecount)
plot_histogram(Storecount$TotalPopulation)
plot_histogram(Storecount$MedianRent)
plot_histogram(Storecount$MedianHouseholdIncome)


plot_density(Storecount)
#plot_bar(Storecount)
#plot_bar(Storecount$Zip)

