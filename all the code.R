library(tidyverse)
library(rlang)
library(reshape2)
library(agricolae)
library(FinCal)
library(FinancialMath)
library(rgl)
library(plot3D)
library(barplot3d)
library(htmlwidgets)
library(qcc)
library(leaflet)
library(treemapify)
library(d3Tree)
library(treemap)
library(plotrix)
library(plotly)
library(GGally)
library(readr)


##############################################
##############################################
##
## כל הקוד בוצע על ידי עמית בוחבוט וניר מעיין
## יצירת הקוד המשותף עבד בצורה שאחד יצר מבנה ראשוני וכך המשכנו להעביר בינינו לשיפורים וליטושים שהתבצעו על ידי שתינו 
##
##############################################
##############################################
##
######################Primary data tables######################
###############################################################

#####Pareto table
pareto<-read_csv("C:\\Users\\nmaayan\\OneDrive - Intel Corporation\\Desktop\\datascience\\project\\סופי גמור\\csv\\1.csv",locale = locale(date_names = "he", encoding = "UTF-8"))
pareto<-rename(pareto,settlement='יישוב',year='שנה',motorcycle='אופנוע',taxi='מונית',bus='אוטובוס',truck='משאית סך הכל',car='רכב פרטי')
pareto$car<-replace(pareto$car,pareto$car=="***",0)
pareto$motorcycle<-replace(pareto$motorcycle,pareto$motorcycle=="***",0)
pareto$taxi<-replace(pareto$taxi,pareto$taxi=="***",0)
pareto$bus<-replace(pareto$bus,pareto$bus=="***",0)
pareto$truck<-replace(pareto$truck,pareto$truck=="***",0)
pareto2021<-pareto[pareto$year==2021,]
pareto2021$sum<-as.numeric(pareto2021$car)+as.numeric(pareto2021$motorcycle)
sum2021<-sum(pareto2021$sum)
pareto2021$percent<-(pareto2021$sum/sum2021)*100
pareto2021<-pareto2021[order(pareto2021$percent,decreasing=TRUE),]
pareto2021$cumulative<-as.numeric(0)
final_pareto2021<-pareto2021[0,]
#r<-as.numeric()
k<-0;i<-1
while(k<=80)
{
  k<-k+(pareto2021[i,'percent'])
  # r<-c(r,k)
  pareto2021[i,'cumulative']<-k
  final_pareto2021[i,]<-pareto2021[i,]
  i<-i+1 
}
Table_7<-read_csv("C:\\Users\\nmaayan\\OneDrive - Intel Corporation\\Desktop\\datascience\\project\\סופי גמור\\csv\\7.csv",locale = locale(date_names = "he", encoding = "UTF-8"))
Table_7<-rename(Table_7,settlement='יישוב',district='מחוז')
final_pareto2021<-merge(x=final_pareto2021,y=Table_7,by.x='settlement',by.y ='settlement' )
final_pareto2021<-final_pareto2021[c(1,11)]
#####Table_1
Table_1<-pareto
Table_1<-Table_1[Table_1$year>2011,]
Table_1<-merge(x=Table_1,y=final_pareto2021,by.x='settlement',by.y ='settlement' )
#####Table_2
Table_2<-read_csv("C:\\Users\\nmaayan\\OneDrive - Intel Corporation\\Desktop\\datascience\\project\\סופי גמור\\csv\\2.csv",locale = locale(date_names = "he", encoding = "UTF-8"))
Table_2<-rename(Table_2,settlement='יישוב',year='שנה',easy='קל',hard='קשה',fatal='קטלנית')
Table_2$easy<-replace(Table_2$easy,Table_2$easy=="***",0)
Table_2$hard<-replace(Table_2$hard,Table_2$hard=="***",0)
Table_2$fatal<-replace(Table_2$fatal,Table_2$fatal=="***",0)
Table_2<-Table_2[Table_2$year>2011,]
Table_2<-merge(x=Table_2,y=final_pareto2021,by.x='settlement',by.y ='settlement' )
#####Table_3
Table_3<-read_csv("C:\\Users\\nmaayan\\OneDrive - Intel Corporation\\Desktop\\datascience\\project\\סופי גמור\\csv\\3.csv",locale = locale(date_names = "he", encoding = "UTF-8"))
p<-Table_3[-nrow(Table_3),]
#####Table_4
Table_4<-read_csv("C:\\Users\\nmaayan\\OneDrive - Intel Corporation\\Desktop\\datascience\\project\\סופי גמור\\csv\\4.csv",locale = locale(date_names = "he", encoding = "UTF-8"))
Table_4<-rename(Table_4,settlement='יישוב',year='שנה',motorcycle='אופנוע',taxi='מונית',bus='אוטובוס',truck='משאית סך הכל',car='רכב פרטי')
Table_4$car<-replace(Table_4$car,Table_4$car=="***",0)
Table_4$motorcycle<-replace(Table_4$motorcycle,Table_4$motorcycle=="***",0)
Table_4$taxi<-replace(Table_4$taxi,Table_4$taxi=="***",0)
Table_4$bus<-replace(Table_4$bus,Table_4$bus=="***",0)
Table_4$truck<-replace(Table_4$truck,Table_4$truck=="***",0)
Table_4<-Table_4[Table_4$year>2011,]
Table_4<-merge(x=Table_4,y=final_pareto2021,by.x='settlement',by.y ='settlement' )
Table_4<-full_join(Table_4, Table_1, by = c("settlement", "year"))
Table_4[is.na(Table_4)]=0
Table_4$district.x<-replace(Table_4$district.x,Table_4$district.x==0 & Table_4$settlement=="ניר צבי","מחוז מרכז")
Table_4$district.x<-replace(Table_4$district.x,Table_4$district.x==0 & Table_4$settlement=="חריש","מחוז חיפה")
Table_4<-rename(Table_4,accident_motorcycle='motorcycle.x',accident_taxi='taxi.x',accident_bus='bus.x',accident_truck='truck.x',accident_car='car.x',count_motorcycle='motorcycle.y',count_taxi='taxi.y',count_bus='bus.y',count_truck='truck.y',count_car='car.y',district='district.x')
Table_4<-Table_4[-14]
Table_4$avg_motorcycle<-(as.numeric(Table_4$accident_motorcycle)/as.numeric(Table_4$count_motorcycle))*100
Table_4$avg_car<-(as.numeric(Table_4$accident_car)/as.numeric(Table_4$count_car))*100
Table_4$avg_bus<-(as.numeric(Table_4$accident_bus)/as.numeric(Table_4$count_bus))*100
Table_4$avg_taxi<-(as.numeric(Table_4$accident_taxi)/as.numeric(Table_4$count_taxi))*100
Table_4$avg_truck<-(as.numeric(Table_4$accident_truck)/as.numeric(Table_4$count_truck))*100
Table_4[is.na(Table_4)]=0
for (i in 14:18) {
  Table_4[,i]<-replace(Table_4[,i],is.infinite(Table_4[,i]),0)
}
Table_4$sum<-as.numeric(Table_4$accident_motorcycle)+as.numeric(Table_4$accident_taxi)+as.numeric(Table_4$accident_bus)+as.numeric(Table_4$accident_truck)+as.numeric(Table_4$accident_car)
#####Table_5
Table_5<-read_csv("C:\\Users\\nmaayan\\OneDrive - Intel Corporation\\Desktop\\datascience\\project\\סופי גמור\\csv\\5.csv",locale = locale(date_names = "he", encoding = "UTF-8"))
Table_5$`17 - 24`<-replace(Table_5$`17 - 24`,Table_5$`17 - 24`=="***",0)
Table_5$`>=75`<-replace(Table_5$`>=75`,Table_5$`>=75`=="***",0)
Table_5$`25 - 44`<-replace(Table_5$`25 - 44`,Table_5$`25 - 44`=="***",0)
Table_5$`45 - 64`<-replace(Table_5$`45 - 64`,Table_5$`45 - 64`=="***",0)
Table_5$`65 - 74`<-replace(Table_5$`65 - 74`,Table_5$`65 - 74`=="***",0)
Table_5<-rename(Table_5,settlement='יישוב',year='שנה')
Table_5<-Table_5[Table_5$year>2011,]
Table_5<-merge(x=Table_5,y=final_pareto2021,by.x='settlement',by.y ='settlement' )
Table_5$sum_age<-as.numeric(Table_5$`17 - 24`)+as.numeric(Table_5$`65 - 74`)+as.numeric(Table_5$`45 - 64`)+as.numeric(Table_5$`25 - 44`)+as.numeric(Table_5$`>=75`)
Table_5$`17 - 24`<-(as.numeric(Table_5$`17 - 24`)/Table_5$sum)*100
Table_5$`>=75`<-(as.numeric(Table_5$`>=75`)/Table_5$sum)*100
Table_5$`25 - 44`<-(as.numeric(Table_5$`25 - 44`)/Table_5$sum)*100
Table_5$`45 - 64`<-(as.numeric(Table_5$`45 - 64`)/Table_5$sum)*100
Table_5$`65 - 74`<-(as.numeric(Table_5$`65 - 74`)/Table_5$sum)*100
#####Table_6
Table_6<-read_csv("C:\\Users\\nmaayan\\OneDrive - Intel Corporation\\Desktop\\datascience\\project\\סופי גמור\\csv\\6.csv",locale = locale(date_names = "he", encoding = "UTF-8"))
Table_6<-rename(Table_6,year='שנת תאונה')
#####Year tables
Table_2012<-full_join(Table_4[Table_4$year=='2012',],Table_5[Table_5$year=='2012',],Table_2[Table_2$year=='2012',], by = c("settlement","year"))
Table_2012<-Table_2012[-2]
Table_2012[is.na(Table_2012)]=0
Table_2013<-full_join(Table_4[Table_4$year=='2013',],Table_5[Table_5$year=='2013',],Table_2[Table_2$year=='2013',], by = c("settlement","year"))
Table_2013<-Table_2013[-2]
Table_2013[is.na(Table_2013)]=0
Table_2014<-full_join(Table_4[Table_4$year=='2014',],Table_5[Table_5$year=='2014',],Table_2[Table_2$year=='2014',], by = c("settlement","year"))
Table_2014<-Table_2014[-2]
Table_2014[is.na(Table_2014)]=0
Table_2015<-full_join(Table_4[Table_4$year=='2015',],Table_5[Table_5$year=='2015',],Table_2[Table_2$year=='2015',], by = c("settlement","year"))
Table_2015<-Table_2015[-2]
Table_2015[is.na(Table_2015)]=0
Table_2016<-full_join(Table_4[Table_4$year=='2016',],Table_5[Table_5$year=='2016',],Table_2[Table_2$year=='2016',], by = c("settlement","year"))
Table_2016<-Table_2016[-2]
Table_2016[is.na(Table_2016)]=0
Table_2017<-full_join(Table_4[Table_4$year=='2017',],Table_5[Table_5$year=='2017',],Table_2[Table_2$year=='2017',], by = c("settlement","year"))
Table_2017<-Table_2017[-2]
Table_2017[is.na(Table_2017)]=0
Table_2018<-full_join(Table_4[Table_4$year=='2018',],Table_5[Table_5$year=='2018',],Table_2[Table_2$year=='2018',], by = c("settlement","year"))
Table_2018<-Table_2018[-2]
Table_2018[is.na(Table_2018)]=0
Table_2019<-full_join(Table_4[Table_4$year=='2019',],Table_5[Table_5$year=='2019',],Table_2[Table_2$year=='2019',], by = c("settlement","year"))
Table_2019<-Table_2019[-2]
Table_2019[is.na(Table_2019)]=0
Table_2020<-full_join(Table_4[Table_4$year=='2020',],Table_5[Table_5$year=='2020',],Table_2[Table_2$year=='2020',], by = c("settlement","year"))
Table_2020<-Table_2020[-2]
Table_2020[is.na(Table_2020)]=0
Table_2021<-full_join(Table_4[Table_4$year=='2021',],Table_5[Table_5$year=='2021',],Table_2[Table_2$year=='2021',], by = c("settlement","year"))
Table_2021<-Table_2021[-2]
Table_2021[is.na(Table_2021)]=0




######################statistical analysis######################
###############################################################

###############Question_1###############
Question_1<-data.frame(unique(Table_7$district))
Question_1<-rename(Question_1,district='unique.Table_7.district.')
Question_1$motorcycle<-as.numeric(0)
Question_1$car<-as.numeric(0)
Question_1$taxi<-as.numeric(0)
Question_1$bus<-as.numeric(0)
Question_1$truck<-as.numeric(0)
Question_1$sum<-as.numeric(0) 
type<-list("","avg_motorcycle","avg_car","avg_taxi","avg_bus","avg_truck")
for(i in 2:6){
  for (j in 1:7){
    x<-Table_4[Table_4$district==Question_1[j,1],]
    Question_1[j,i]<-sum(as.numeric(x[,type[[i]]]))/length(x[,type[[i]]])  #length(x$district)
    x<-as.numeric(0)
  }
}
for (i in 1:7){
  Question_1[i,7]<-sum(as.numeric(Table_4$district==Question_1[i,1],20))
}
#####normality test
lim<-lm(data=Question_1,sum~motorcycle+car+taxi+bus+truck)
summary(lim)
qqnorm(lim$residuals,main = "Normal QQPlot",xlab = "z", ylab = "e")
qqline(lim$residuals,distribution = qnorm,col="blue")
hist(lim$residuals,main ="residuals",prob=TRUE)
shapiro.test<-shapiro.test(lim$residuals)
shapiro.test
if (shapiro.test[["p.value"]]<0.05)
{
  print ("Reject H0,  The values are not normally distributed")
}else
{
  print ("NOT Reject H0, The values are normally distributed")
}
#####Correlation test
chisq.test <- chisq.test(table(Question_1$motorcycle,Question_1$car))
if (chisq.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is a dependency between motorcycle and car ")
}else
{
  print ("NOT Reject H0,There is no dependence between motorcycle and car")
}
chisq.test <- chisq.test(table(Question_1$motorcycle,Question_1$taxi))
if (chisq.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is a dependence between motorcycle and taxi")
}else
{
  print ("NOT Reject H0,There is no dependence between motorcycle and taxi")
}
chisq.test <- chisq.test(table(Question_1$motorcycle,Question_1$bus))

if (chisq.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is a dependence between motorcycle and bus")
}else
{
  print ("NOT Reject H0,There is no dependence between motorcycle and bus")
}
chisq.test <- chisq.test(table(Question_1$motorcycle,Question_1$truck))
if (chisq.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is a dependence between motorcycle and truck")
}else
{
  print ("NOT Reject H0,There is no dependence between motorcycle and truck")
}
chisq.test <- chisq.test(table(Question_1$car,Question_1$taxi))
if (chisq.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is a dependence between car and taxi")
}else
{
  print ("NOT Reject H0,There is no dependence between car and taxi")
}
chisq.test <- chisq.test(table(Question_1$car,Question_1$bus))
if (chisq.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is a dependence between car and bus")
}else
{
  print ("NOT Reject H0,There is no dependence between car and bus")
}
chisq.test <- chisq.test(table(Question_1$car,Question_1$truck))
if (chisq.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is a dependence between car and truck")
}else
{
  print ("NOT Reject H0,There is no dependence between car and truck")
}
chisq.test <- chisq.test(table(Question_1$taxi,Question_1$bus))
if (chisq.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is a dependence between taxi and bus")
}else
{
  print ("NOT Reject H0,There is no dependence between taxi and bus")
}
chisq.test <- chisq.test(table(Question_1$taxi,Question_1$truck))
if (chisq.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is a dependence between taxi and truck")
}else
{
  print ("NOT Reject H0,There is no dependence between taxi and truck")
}
chisq.test <- chisq.test(table(Question_1$bus,Question_1$truck))
if (chisq.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is a dependence between bus and truck")
}else
{
  print ("NOT Reject H0,There is no dependence between bus and truck")
}
#####regression
lim<-summary(lim)
critic_val<- qf(0.95,df1=lim[["fstatistic"]][["numdf"]],df2=lim[["fstatistic"]][["dendf"]])
critic_val
f_val<- lim[["fstatistic"]][["value"]]
f_val
if (f_val>critic_val)
{
  print ("Reject H0,There is a linear relationship Between accident averages in the different types of vehicles and the number of accidents ")
}else
{
  print ("NOT Reject H0,There is no linear relationship Between accident averages in the different types of vehicles and the number of accidents")
}


###############Question_2###############
Question_2<-data.frame(unique(Table_7$district))
Question_2<-rename(Question_2,district='unique.Table_7.district.')
Question_2$motorcycle<-as.numeric(0)
Question_2$taxi<-as.numeric(0)
Question_2$bus<-as.numeric(0)
Question_2$truck<-as.numeric(0)
Question_2$car<-as.numeric(0)
j<-1
for (i in Question_2$district) {
  sum1<-sum(as.numeric(Table_4[Table_4$district==i,'accident_motorcycle' ]))
  Question_2[j,'motorcycle']<-sum1/length(Table_4[Table_4$district==i,])
  sum1<-sum(as.numeric(Table_4[Table_4$district==i,'accident_taxi' ]))
  Question_2[j,'taxi']<-sum1/length(Table_4[Table_4$district==i,])
  sum1<-sum(as.numeric(Table_4[Table_4$district==i,'accident_bus' ]))
  Question_2[j,'bus']<-sum1/length(Table_4[Table_4$district==i,])
  sum1<-sum(as.numeric(Table_4[Table_4$district==i,'accident_truck' ]))
  Question_2[j,'truck']<-sum1/length(Table_4[Table_4$district==i,])
  sum1<-sum(as.numeric(Table_4[Table_4$district==i,'accident_car' ]))
  Question_2[j,'car']<-sum1/length(Table_4[Table_4$district==i,])
  j<-j+1
}
#####Summary
Q1<-data.frame(Table_4$district,Table_4$accident_motorcycle,Table_4$accident_taxi,Table_4$accident_bus,Table_4$accident_truck,Table_4$accident_car)
Q1<-rename(Q1,district='Table_4.district',motorcycle='Table_4.accident_motorcycle',taxi='Table_4.accident_taxi',bus='Table_4.accident_bus',truck='Table_4.accident_truck',car='Table_4.accident_car')
Q1<-melt(Q1,id.vers=c("district"),measure.vars=c("motorcycle", "taxi", "bus","truck","car"),
         variable.name="vehicle type",
         value.name="accident")
Q1$accident<-as.numeric(Q1$accident)
summary(Q1$accident)
Q1<-as_tibble(Q1)
group1<-Q1 %>% group_by(district) %>% summarize(min= min(accident),
                                                median = median(accident),
                                                mean = mean(accident),
                                                max = max(accident),
                                                var= var(accident))
as.data.frame(group1)
group2<-Q1 %>% group_by(`vehicle type`) %>% summarize(min= min(accident),
                                                      median = median(accident),
                                                      mean = mean(accident),
                                                      max = max(accident),
                                                      var= var(accident))
as.data.frame(group2)
#####normality test
hist(Q1$accident,prob=T,col="orange")
lines(density(Q1$accident),col="blue")
shapiro.test<-shapiro.test(Q1$accident)
qqnorm(Q1$accident, pch = 1, frame = FALSE)
qqline(Q1$accident, col = "steelblue", lwd = 2)
if (shapiro.test[["p.value"]]<0.05)
{
  print ("Reject H0,  The number of accidents are not normally distributed")
}else
{
  print ("NOT Reject H0, The number of accidents are normally distributed")
}
#####Equality of differences test
bartlett.test<-bartlett.test(accident~district, Q1)
if (bartlett.test[["p.value"]]<0.05)
{
  print ("Reject H0, There is no equality of differences in the block")
}else
{
  print ("NOT Reject H0, there is equality of differences in the block")
}
bartlett.test<-bartlett.test(accident~`vehicle type`, Q1)
if (bartlett.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is no equality of differences in the factor")
}else
{
  print ("Reject H0,There is  equality of differences in the factor")
}
#####regression
analysis<-lm(accident~district + `vehicle type`,Q1)
a1<-anova(analysis)
a1
f1_val<- anova(analysis)[1,4]
f1_val
critic_val<- qf(0.95,df1=a1[1,1],df2 = ( a1[1,1]*a1[2,1]))
critic_val
if (f1_val>critic_val)
{
  print ("Reject H0, in conclusion the block is significant")
}else
{
  print ("NOT Reject H0, in conclusion the block is not significant")
}
f2_val<- anova(analysis)[2,4]
f2_val
critic_val<- qf(0.95,df1=a1[2,1],df2 =( a1[1,1]*a1[2,1]))
critic_val
if (f2_val>critic_val)
{
  print ("Reject H0, in conclusion the treatment is significant")
}else
{
  print ("NOT Reject H0, in conclusion the treatment is not significant")
}
#####homogeneity test
duncan<-duncan.test(Q1$accident,Q1$`vehicle type`,a1[3,1],a1[3,3],0.05,console = TRUE)
plot(duncan)
#g<-data.frame(a=c(as.numeric(duncan[["groups"]][["Q1$accident"]])),b=c(duncan[["groups"]][["groups"]]),$$$$$$$$$$))
#g[min(g$a),'b']
#g[g$b==g[min(g$a),'b'],]


###############Question_3###############
Question_3<-data.frame(2012:2021)
Question_3<-rename(Question_3,year='X2012.2021')
Question_3$`17 - 24`<-as.numeric(0)
Question_3$`25 - 44`<-as.numeric(0)
Question_3$`45 - 64`<-as.numeric(0)
Question_3$`65 - 74`<-as.numeric(0)
Question_3$`>=75`<-as.numeric(0)
tableArray<- list(Table_2012,Table_2013, Table_2014,Table_2015,Table_2016,Table_2017,Table_2018,Table_2019,Table_2020,Table_2021)
ageList<-list("17 - 24","25 - 44","45 - 64","65 - 74",">=75")
for (t in 1:10) {
  for (i in 1:5){
    sum1<-sum(as.numeric(tableArray[[t]][[(ageList[[i]])]]))
    Question_3[t,ageList[[i]]]<-sum1/length(tableArray[[t]][[ageList[[i]]]])
  }
}
#####Correlation test
cor<-cor.test(Question_3$`17 - 24`,Question_3$`25 - 44`, method = "pearson")
cor<-cor[["estimate"]]
if ((cor>=(0.8) & cor<=(1)) || (cor<=(-0.8) & cor>=(-1)))
{
  print ("NOT Reject H0,There is a definite connection between ages 17-24 and ages 25 - 44 ")
}else
{
  print ("Reject H0,There is no a definite connection between ages 17-24 and ages 25 - 44")
}
cor.test(Question_3$`17 - 24`,Question_3$`45 - 64`, method = "pearson")
if ((cor>=(0.8) & cor<=(1)) || (cor<=(-0.8) & cor>=(-1)))
{
  print ("NOT Reject H0,There is a definite connection between ages 17-24 and ages 45 - 64 ")
}else
{
  print (" Reject H0,There is no a definite connection between ages 17-24 and ages 45 - 64")
}
cor.test(Question_3$`17 - 24`,Question_3$`65 - 74`, method = "pearson")
if ((cor>=(0.8) & cor<=(1)) || (cor<=(-0.8) & cor>=(-1)))
{
  print ("NOT Reject H0,There is a definite connection between ages 17-24 and ages 65 - 74 ")
}else
{
  print (" Reject H0,There is no a definite connection between ages 17-24 and ages 65 - 74")
}
cor.test(Question_3$`17 - 24`,Question_3$`>=75`, method = "pearson")
if ((cor>=(0.8) & cor<=(1)) || (cor<=(-0.8) & cor>=(-1)))
{
  print ("NOT Reject H0,There is a definite connection between ages 17-24 and ages >=75 ")
}else
{
  print (" Reject H0,There is no a definite connection between ages 17-24 and ages >=75")
}
cor.test(Question_3$`25 - 44`,Question_3$`>=75`, method = "pearson")
if ((cor>=(0.8) & cor<=(1)) || (cor<=(-0.8) & cor>=(-1)))
{
  print ("NOT Reject H0,There is a definite connection between ages 25 - 44 and ages >=75 ")
}else
{
  print (" Reject H0,There is no a definite connection between ages 25 - 44 and ages >=75")
}
cor.test(Question_3$`25 - 44`,Question_3$`65 - 74`, method = "pearson")
if ((cor>=(0.8) & cor<=(1)) || (cor<=(-0.8) & cor>=(-1)))
{
  print ("NOT Reject H0,There is a definite connection between ages 25 - 44 and ages 65 - 74 ")
}else
{
  print (" Reject H0,There is no a definite connection between ages 25 - 44 and ages 65 - 74")
}
cor.test(Question_3$`25 - 44`,Question_3$`45 - 64`, method = "pearson")
if ((cor>=(0.8) & cor<=(1)) || (cor<=(-0.8) & cor>=(-1)))
{
  print ("NOT Reject H0,There is a definite connection between ages 25 - 44 and ages 45 - 64 ")
}else
{
  print (" Reject H0,There is no a definite connection between ages 25 - 44 and ages 45 - 64")
}
cor.test(Question_3$`45 - 64`,Question_3$`65 - 74`, method = "pearson")
if ((cor>=(0.8) & cor<=(1)) || (cor<=(-0.8) & cor>=(-1)))
{
  print ("NOT Reject H0,There is a definite connection between ages 45 - 64 and ages 65 - 74 ")
}else
{
  print (" Reject H0,There is no a definite connection between ages 45 - 64 and ages 65 - 74")
}
cor.test(Question_3$`45 - 64`,Question_3$`>=75`, method = "pearson")
if ((cor>=(0.8) & cor<=(1)) || (cor<=(-0.8) & cor>=(-1)))
{
  print ("NOT Reject H0,There is a definite connection between ages 45 - 64 and ages >=75 ")
}else
{
  print (" Reject H0,There is no a definite connection between ages 45 - 64 and ages >=75")
}


###############Question_3.1###############
Question_3.a<-data.frame(2012:2021)
Question_3.a<-rename(Question_3.a,year='X2012.2021')
Question_3.a$`17 - 24`<-Question_3$`17 - 24`
Question_3.a$`25 - 44`<-Question_3$`25 - 44`
#####Correlation test
chisq.test <- chisq.test(table(Question_3.a$`17 - 24`,Question_3.a$`25 - 44`))
if (chisq.test[["p.value"]]<0.05)
{
  print ("Reject H0,There is a dependence between bus and truck")
}else
{
  print ("NOT Reject H0,There is no dependence between bus and truck")
}
#####T-test
t.test<-t.test(Question_3$`17 - 24`,Question_3$`25 - 44`, paired = TRUE, alternative = "greater")
t_crit<-qt(0.95,t.test[["parameter"]][["df"]])
if(t.test[["statistic"]]>t_crit)
{
  print ("Reject H0,there are more accidents in ages 17 - 24  than ages 25 - 44")
}else
{
  print ("NOT Reject H0,there are less or equall accidents in ages 17 - 24  than ages 25 - 44")
}


###############Question_4###############
Question_4<-as.tibble(Table_6)
pairs(Question_4)


###############Question_5###############
Q5<-read_csv("C:\\Users\\nmaayan\\OneDrive - Intel Corporation\\Desktop\\datascience\\project\\סופי גמור\\csv\\9.csv",locale = locale(date_names = "he", encoding = "UTF-8"))
Q5<-Q5[c(-2:-6,-9:-12,-14:-28)]
Q5<-rename(Q5,lightly_moderately_injured="קל בינוני",Severly_injured="פצועים קשה",dead="הרוגים",year="שנה")
Q5<-filter(Q5,year<=2019)
befor_table<-filter(Q5,year<="2013")
after_table<-filter(Q5,year>"2013")
#long
g1<-melt(befor_table,id.vers=c("year"),measure.vars=c("dead", "Severly_injured","lightly_moderately_injured"),variable.name="type",value.name="count")
g2<-melt(after_table,id.vers=c("year"),measure.vars=c("dead", "Severly_injured","lightly_moderately_injured"),variable.name="type",value.name="count")
#####Summary
befor_table_year<-g1%>%group_by(year,type)%>%summarise(sum_befor=sum(count))
after_table_year<-g2%>%group_by(year,type)%>%summarise(sum_after=sum(count))
Q5$time<-char("")
Q5$time<-replace(Q5$time,Q5$year>"2013","after")
Q5$time<-replace(Q5$time,Q5$year<="2013","befor")
all_long<-melt(Q5,id.vers=c("year","time"),measure.vars=c("dead","Severly_injured","lightly_moderately_injured"),variable.name="type",value.name="count")
all_year<-all_long%>%group_by(time,type)%>%summarise(sum1=sum(count))
all_wide<-dcast(all_year,time~type,value.var="sum1")
#####wilcox.test-dead
#dead
x<-as.numeric(unlist(select(filter(all_wide,time=="befor"),dead)))
y<-as.numeric(unlist(select(filter(all_wide,time=="after"),dead)))
wilcox.test<-wilcox.test(x,y,alternative = "greater")
if(wilcox.test[["p.value"]]<0.05)
{
  print ("Reject H0,The number of deaths decreased after the project")
}else
{
  print ("NOT Reject H0,The number of deaths equal or lower than after the project")
}
#Severly_injured
x<-as.numeric(unlist(select(filter(all_wide,time=="befor"),Severly_injured)))
y<-as.numeric(unlist(select(filter(all_wide,time=="after"),Severly_injured)))
wilcox.test<-wilcox.test(x,y,alternative = "greater")
if(wilcox.test[["p.value"]]<0.05)
{
  print ("Reject H0,The number of Severly_injured decreased after the project")
}else
{
  print ("NOT Reject H0,The number of Severly_injured equal or lower than after the project")
}
#lightly_moderately_injured
x<-as.numeric(unlist(select(filter(all_wide,time=="befor"),lightly_moderately_injured)))
y<-as.numeric(unlist(select(filter(all_wide,time=="after"),lightly_moderately_injured)))
wilcox.test<-wilcox.test(x,y,alternative = "greater")
if(wilcox.test[["p.value"]]<0.05)
{
  print ("Reject H0,The number of Severly_injured decreased after the project")
}else
{
  print ("NOT Reject H0,The number of Severly_injured equal or lower than after the project")
}
#mean befor
b.w<-dcast(befor_table_year,year~type,value.var="sum_befor")
m_befor<-c(mean(as.numeric(unlist(select(b.w,dead)))),mean(as.numeric(unlist(select(b.w,Severly_injured)))),
           mean(as.numeric(unlist(select(b.w,lightly_moderately_injured)))))
m_befor
#####NPV after
after_NPV<-dcast(after_table_year,year~type,value.var="sum_after")
after_NPV$dead<-m_befor[1]-after_NPV$dead
after_NPV$Severly_injured<-m_befor[2]-after_NPV$Severly_injured
after_NPV$lightly_moderately_injured<-m_befor[3]-after_NPV$lightly_moderately_injured
after_NPV<-mutate(after_NPV,cost_d=as.numeric(dead*6.1))
after_NPV<-mutate(after_NPV,cost_s=as.numeric(Severly_injured*4.03))
after_NPV<-mutate(after_NPV,cost_l=as.numeric(lightly_moderately_injured*0.58))
after_NPV<-mutate(after_NPV,inflation=c(-0.002,0.01,0.002,0.004,0.008,0.006))
after_NPV<-mutate(after_NPV,Real_value=as.numeric((cost_d+cost_s+cost_l)))
for(j in 1:6){k<-1
for(i in 1:j)
{
  k<-k*(1+as.numeric(after_NPV[i,'inflation']))
}
after_NPV[j,'Real_value']<-after_NPV[j,'Real_value']/k
}
x<-as.numeric(unlist(select(after_NPV,Real_value)))
x<-x/(1.07)^(1:6)
after_NPV<-mutate(after_NPV,npv=x)
NPV(cf0=-1000, cf=after_NPV$Real_value,times=c(1:6),i=.07)
after_NPV$npv_cl<-numeric(6)
for(j in 1:6){k<-0
for(i in 1:j)
{
  k<-k+(after_NPV[i,'npv'])
}
after_NPV[j,'npv_cl']<-k
}
NPV(cf0=-1000, cf=after_NPV$Real_value,times=c(1:6),i=.07)


######################graphs######################
##################################################

#####3D PLOT-3D histogram:X-Years,Y-Districts,Z-Number of accidents
p<-select(Table_4,year,accident_motorcycle,accident_taxi,accident_bus,accident_truck,accident_car,district)
p$sum_of_accidents=as.numeric(p$accident_motorcycle)+as.numeric(p$accident_taxi)+as.numeric(p$accident_bus)+as.numeric(p$accident_truck)+as.numeric(p$accident_car)
p<-select(p,year,sum_of_accidents,district)
p$district<-replace(p$district,p$district=="מחוז ירושלים",1)
p$district<-replace(p$district,p$district=="מחוז צפון",2)
p$district<-replace(p$district,p$district=="מחוז תל אביב",3)
p$district<-replace(p$district,p$district=="מחוז חיפה",4)
p$district<-replace(p$district,p$district=="מחוז יהודה ושומרון",5)
p$district<-replace(p$district,p$district=="מחוז דרום",6)
p$district<-replace(p$district,p$district=="מחוז מרכז",7)
p$district<-as.factor(p$district)
t<-group_by(p,district,year)
t<-summarise(t,val=sum(sum_of_accidents))
p<-plot3d( 
  x=t$year, y=t$district, z=t$val, 
  col = t$district, lwd = 6,
  type = 'h', 
  xlab="Year", ylab="District", zlab="Total accidents")+
  legend3d(x=0, y=.65, legend=c("1-מחוז ירושלים","2-מחוז צפון","3-מחוז תל אביב","4-מחוז חיפה","5-מחוז יהודה ושומרון","6-מחוז דרום","7-מחוז מרכז"), pch=10, col=t$district, title='מספרי מחוזות', horiz=FALSE,cex=0.65)+
  text3d(x=t$year,y=t$district, z=t$val,
         texts =t$val,cex=0.8) +
  title3d(main="Evaluation - Number of casualties",line=6)
if (interactive()) p
htmlwidgets::saveWidget(rglwidget(), "3d.html",selfcontained = FALSE)


#####Stacked bar plot:X-hours of the day,Y-amount of accidents per hour 
p<-mutate(Table_6,'00:00-01:59'=Table_6$`00:00-00:59`+Table_6$`01:00-01:59`,'02:00-03:59'=Table_6$`02:00-02:59`+Table_6$`03:00-03:59`,'04:00-05:59'=Table_6$`04:00-04:59`+Table_6$`05:00-05:59`,'06:00-07:59'=Table_6$`06:00-06:59`+Table_6$`07:00-07:59`,'08:00-09:59'=Table_6$`08:00-08:59`+Table_6$`09:00-09:59`,'10:00-11:59'=Table_6$`10:00-10:59`+Table_6$`11:00-11:59`,'12:00-13:59'=Table_6$`12:00-12:59`+Table_6$`13:00-13:59`,'14:00-15:59'=Table_6$`14:00-14:59`+Table_6$`15:00-15:59`,'16:00-17:59'=Table_6$`16:00-16:59`+Table_6$`17:00-17:59`,'18:00-19:59'=Table_6$`18:00-18:59`+Table_6$`19:00-19:59`,'20:00-21:59'=Table_6$`20:00-20:59`+Table_6$`21:00-21:59`,'22:00-23:59'=Table_6$`22:00-22:59`+Table_6$`23:00-23:59`)[c(-2:-25)]
x<-melt(p,id.vars = "year")
ggplot(x, aes(x = year, y = value, fill = variable, label = value)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Set3")+
  labs(x="שנים",y="כמות תאונות",fill="טווחי שעות")+
  ggtitle("כמות התאונות בשעות היום השונות, לפי שנים 2003-2022","בפילוח לפי שעות היום")+ 
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(plot.subtitle = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(2003, 2022, by=1), limits=c(2002,2023))


#####Grouped barplot:X-Districts,X(internal)-Range of detectors,Y-Amount of accidents 
p<-Table_2012
p<-mutate(Table_2012[c(-1:-6,-8:-17)],year=2013)
p<-rbind(p,mutate(Table_2013[c(-1:-6,-8:-17)],year=2013))
p<-rbind(p,mutate(Table_2014[c(-1:-6,-8:-17)],year=2014))
p<-rbind(p,mutate(Table_2015[c(-1:-6,-8:-17)],year=2015))
p<-rbind(p,mutate(Table_2016[c(-1:-6,-8:-17)],year=2016))
p<-rbind(p,mutate(Table_2017[c(-1:-6,-8:-17)],year=2017))
p<-rbind(p,mutate(Table_2018[c(-1:-6,-8:-17)],year=2018))
p<-rbind(p,mutate(Table_2019[c(-1:-6,-8:-17)],year=2019))
p<-rbind(p,mutate(Table_2020[c(-1:-6,-8:-17)],year=2020))
p<-rbind(p,mutate(Table_2021[c(-1:-6,-8:-17)],year=2021))
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age)
p<-mutate(p,district=district.x,district.x=NULL)
p$`17 - 24`<-(as.numeric(p$`17 - 24`))
p$`65 - 74`<-(as.numeric(p$`65 - 74`))
p$`45 - 64`<-(as.numeric(p$`45 - 64`))
p$`>=75`<-(as.numeric(p$`>=75`))
p<-group_by(p,district)
x<-summarise(p,'17-24'=sum(`17 - 24`,na.rm = TRUE),'25-44'=sum(`25 - 44`,na.rm = TRUE),'>=75'=sum(`>=75`,na.rm = TRUE),'45-64'=sum(`45 - 64`,na.rm = TRUE),'65-74'=sum(`65 - 74`,na.rm = TRUE))

x<-melt(x,variable.name = "ages",value.name="Number_of_accidents")
ggplot(x, aes(x=district, y=Number_of_accidents, fill=sort(ages))) + 
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("כמות תאונות בכל מחוז","בחלוקה לטווחי גיל שונים")+ 
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(plot.subtitle = element_text(hjust = 0.5))+
  labs(x="מחוז",y="כמות תאונות",fill="טווחי גילאים")+
  geom_text(aes(label =Number_of_accidents ), 
            position = position_dodge(0.9),vjust=-1)


#####Pareto chart:For all settlements
p<-pareto2021
data1<-p[c(-2:-7,-9)]
data1<-filter(data1,cumulative>0)
x<-sum(filter(p,cumulative==0)$sum)
data1$sum<-as.numeric(data1$sum)
data1$tot<-as.numeric(data1$sum)
data1<-rbind(data1,c("יתר 1094 יישובים",0,100,x))
data1$tot<-as.numeric(data1$tot)
for (i in 2:nrow(data1)) {
  data1$tot[i]  <-data1$tot[i-1]+data1$tot[i]
}
data1$sum<-as.numeric(data1$sum)
t<-max(data1$tot)
data1$cumulative<-as.numeric(data1$cumulative)
t2<-round(data1$cumulative,2)
data1$sum<-as.numeric(data1$sum)
ggplot(data1, aes(x = reorder(settlement,sum), y = sum,label=t2))+
  geom_bar(stat="identity", color='skyblue',fill='steelblue')+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  geom_point(aes(y=tot),col="red")+
  scale_y_continuous(breaks=seq(0, t, t/10), limits=c(0,t))+ 
  annotate("text", x = 7, y = seq(0, t, t/10), label =  c("  0%", " 10%", " 20%", " 30%", " 40%", " 50%", " 60%", " 70%", " 80%", " 90%", "100%"), size = 5)+
  geom_text(aes(x=data1$settlement,y=data1$tot),vjust=-1.5,size=1.5,col="red")+
  geom_hline(yintercept = t*0.8,size=.1)+
  ylab("כמות כלי רכב (מצרפי)")+xlab("יישובים")+
  labs(title =" טבלת פארטו לפי מצבת כלי הרכב שנת 2021")+
  theme(plot.title = element_text(hjust = 0.5))


#####Pareto pie:By regions-years 2017 to 2021
p<-rbind(Table_2021[c(-1:-6,-13:-23)],Table_2020[c(-1:-6,-13:-23)],Table_2019[c(-1:-6,-13:-23)],Table_2018[c(-1:-6,-13:-23)],Table_2017[c(-1:-6,-13:-23)])
p$count_motorcycle<-as.numeric(p$count_motorcycle)
p$count_taxi<-as.numeric(p$count_taxi)
p$count_bus<-as.numeric(p$count_bus)
p$count_truck<-as.numeric(p$count_truck)
p$count_car<-as.numeric(p$count_car)
p$tot<-p$count_motorcycle+p$count_taxi+p$count_bus+p$count_truck+p$count_car
p<-p[c(-2:-6)]
p<-group_by(p,district.x)
p<-summarise(p,sum(tot,na.rm = T))

ggplot(p, aes(x="", y=p$`sum(tot, na.rm = T)`, fill=p$district.x)) +
  geom_bar(stat="identity", width=5,color="white") +
  coord_polar("y", start=0)+labs(x="",y="",fill="מחוזות")+
  scale_fill_brewer(palette="Set2")+theme_void()+
  ggtitle("כמות הרכבים בכל מחוז","שנים 2017-2021")+ 
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(plot.subtitle = element_text(hjust = 0.5))+
  geom_label(aes(label = p$`sum(tot, na.rm = T)`),
             color = "black",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) 


#####Boxplot:X-Hours interval,Y-The number of accidents
p<-mutate(Table_6,'00:00-01:59'=Table_6$`00:00-00:59`+Table_6$`01:00-01:59`,'02:00-03:59'=Table_6$`02:00-02:59`+Table_6$`03:00-03:59`,'04:00-05:59'=Table_6$`04:00-04:59`+Table_6$`05:00-05:59`,'06:00-07:59'=Table_6$`06:00-06:59`+Table_6$`07:00-07:59`,'08:00-09:59'=Table_6$`08:00-08:59`+Table_6$`09:00-09:59`,'10:00-11:59'=Table_6$`10:00-10:59`+Table_6$`11:00-11:59`,'12:00-13:59'=Table_6$`12:00-12:59`+Table_6$`13:00-13:59`,'14:00-15:59'=Table_6$`14:00-14:59`+Table_6$`15:00-15:59`,'16:00-17:59'=Table_6$`16:00-16:59`+Table_6$`17:00-17:59`,'18:00-19:59'=Table_6$`18:00-18:59`+Table_6$`19:00-19:59`,'20:00-21:59'=Table_6$`20:00-20:59`+Table_6$`21:00-21:59`,'22:00-23:59'=Table_6$`22:00-22:59`+Table_6$`23:00-23:59`)[c(-2:-25)]
p<-filter(p,between(year,2017,2021))[-1]
p$year<-as.factor(p$year)
t<-melt(p)
x1<-group_by(t,variable)
x1<-summarise(x1,means=mean(value))
#x1<-as.vector(x1[-1])
ggplot(t,aes(x=variable,y=value,color=variable))+geom_boxplot()+
  geom_text(data = as.data.frame(x1), aes(label = means, y = means,x=variable ),vjust=-3)+
  labs(subtitle="הצגת ממוצע התאונות לכל טווח",title ="התפלגות כמות התאונות, בכל טווחי השעות ביום",x="שעות היום",y="כמות התאונות",colour="טווחי שעות")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5))


#####Boxplot:X-Types of accidents,Y-Amount of accidents 
p<-Table_3[-nrow(Table_3),-1]
t<-melt(p)
t$variable<-as.factor(t$variable)
x1<-group_by(t,variable)
x1<-(summarise(x1,means=mean(value)))
x1$means<-round(x1$means,2)
ggplot(t,aes(x=variable,y=value,color=variable))+geom_boxplot()+
  geom_text(data = as.data.frame(x1), aes(label = means, y = means,x=variable ),vjust=-4)+
  labs(subtitle="הצגת ממוצע התאונות לכל סוג תאונה",title ="הפתלגות כמות התאונות, בכל סוג תאונה",x="סוגי תאונות",y="כמות התאונות",colour="סוגי תאונות")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous( limits=c(0, 20000),breaks =seq(0,20000,by=1000) )


#####Grouped barplot with ggplot2:Y-Average amount of accidents,X1-Vehicle type,X-Districts 
p<-rbind(Table_2012[c(-1,-8:-ncol(Table_2012))],Table_2013[c(-1,-8:-ncol(Table_2012))],Table_2014[c(-1,-8:-ncol(Table_2012))],Table_2015[c(-1,-8:-ncol(Table_2012))],Table_2016[c(-1,-8:-ncol(Table_2012))],Table_2017[c(-1,-8:-ncol(Table_2012))],Table_2018[c(-1,-8:-ncol(Table_2012))],Table_2019[c(-1,-8:-ncol(Table_2012))],Table_2020[c(-1,-8:-ncol(Table_2012))],Table_2021[c(-1,-8:-ncol(Table_2012))])
p$accident_motorcycle<-as.numeric((p$accident_motorcycle))
p$accident_truck<-as.numeric((p$accident_truck))
p$accident_taxi<-as.numeric((p$accident_taxi))
p$accident_bus<-as.numeric((p$accident_bus))
p$accident_car<-as.numeric((p$accident_car))
p<-group_by(p,district.x)
p<-summarise(p,"פרטי"=mean(accident_car,na.rm=T),"אוטובוסים"=mean(accident_bus,na.rm=T),"משאית"=mean(accident_truck,na.rm=T),"מונית"=mean(accident_taxi,na.rm=T),"אופנוע"=mean(accident_motorcycle,na.rm=T))
p<-melt(p)
ggplot(p, aes(x=district.x, y=value, fill=variable)) + 
  geom_bar(stat="identity", position=position_dodge())+
  ggtitle("כמות התאונות הממוצעות לפי סוג כלי רכב","שנים 2012-2021")+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(plot.subtitle = element_text(hjust = 0.5))+
  labs(x="מחוז",y="ערך תאונות ממוצע",fill="סוג כלי רכב")+
  geom_text(aes(label =round(value,2) ), 
            position = position_dodge(0.9),vjust=-1)+
  scale_fill_manual(values=c("skyblue1","palegreen","orchid","sandybrown","khaki1"))


#####Heatmap with ggplot2:X-years,y-number of accidents by age  -לא שומש
p<-mutate(Table_2012[c(-1:-6,-8:-17)])
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age,sum_age=NULL,district.x=NULL)
t1<-melt(p,variable.name = "range1",value.name = "accidentNo")
t1<-mutate(t1,year=2012)

p<-mutate(Table_2013[c(-1:-6,-8:-17)])
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age,sum_age=NULL,district.x=NULL)
t2<-melt(p,variable.name = "range1",value.name = "accidentNo")
t2<-mutate(t2,year=2013)

p<-mutate(Table_2014[c(-1:-6,-8:-17)])
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age,sum_age=NULL,district.x=NULL)
t3<-melt(p,variable.name = "range1",value.name = "accidentNo")
t3<-mutate(t3,year=2014)

p<-mutate(Table_2015[c(-1:-6,-8:-17)])
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age,sum_age=NULL,district.x=NULL)
t4<-melt(p,variable.name = "range1",value.name = "accidentNo")
t4<-mutate(t4,year=2015)

p<-mutate(Table_2016[c(-1:-6,-8:-17)])
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age,sum_age=NULL,district.x=NULL)
t5<-melt(p,variable.name = "range1",value.name = "accidentNo")
t5<-mutate(t5,year=2016)

p<-mutate(Table_2017[c(-1:-6,-8:-17)])
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age,sum_age=NULL,district.x=NULL)
t6<-melt(p,variable.name = "range1",value.name = "accidentNo")
t6<-mutate(t6,year=2017)

p<-mutate(Table_2018[c(-1:-6,-8:-17)])
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age,sum_age=NULL,district.x=NULL)
t7<-melt(p,variable.name = "range1",value.name = "accidentNo")
t7<-mutate(t7,year=2018)

p<-mutate(Table_2019[c(-1:-6,-8:-17)])
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age,sum_age=NULL,district.x=NULL)
t8<-melt(p,variable.name = "range1",value.name = "accidentNo")
t8<-mutate(t8,year=2019)

p<-mutate(Table_2020[c(-1:-6,-8:-17)])
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age,sum_age=NULL,district.x=NULL)
t9<-melt(p,variable.name = "range1",value.name = "accidentNo")
t9<-mutate(t9,year=2020)

p<-mutate(Table_2021[c(-1:-6,-8:-17)])
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age,sum_age=NULL,district.x=NULL)
t10<-melt(p,variable.name = "range1",value.name = "accidentNo")
t10<-mutate(t10,year=2021)

p<-rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)
p$accidentNo<-as.numeric(p$accidentNo)
ggplot(p, aes(year, range1, fill= accidentNo),colours())  +
  geom_tile()+
  scale_fill_distiller(palette = "Reds",direction = 0)+
  ggtitle("מפת חום למיפוי טווחי הגילאים לפי רמות סיכון","שנים 2012-2021")+
  labs(x="שנים",y="טווחי גיל",fill="כמות תאונות")+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(plot.subtitle = element_text(hjust = 0.5))


#####Map  
p<-Table_2012
p<-mutate(Table_2012[c(-1:-6,-8:-17)],year=2013)
p<-rbind(p,mutate(Table_2013[c(-1:-6,-8:-17)],year=2013))
p<-rbind(p,mutate(Table_2014[c(-1:-6,-8:-17)],year=2014))
p<-rbind(p,mutate(Table_2015[c(-1:-6,-8:-17)],year=2015))
p<-rbind(p,mutate(Table_2016[c(-1:-6,-8:-17)],year=2016))
p<-rbind(p,mutate(Table_2017[c(-1:-6,-8:-17)],year=2017))
p<-rbind(p,mutate(Table_2018[c(-1:-6,-8:-17)],year=2018))
p<-rbind(p,mutate(Table_2019[c(-1:-6,-8:-17)],year=2019))
p<-rbind(p,mutate(Table_2020[c(-1:-6,-8:-17)],year=2020))
p<-rbind(p,mutate(Table_2021[c(-1:-6,-8:-17)],year=2021))
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age)
p<-mutate(p,district=district.x,district.x=NULL)
p$`17 - 24`<-(as.numeric(p$`17 - 24`))
p$`65 - 74`<-(as.numeric(p$`65 - 74`))
p$`45 - 64`<-(as.numeric(p$`45 - 64`))
p$`>=75`<-(as.numeric(p$`>=75`))
p<-group_by(p,district)
x<-summarise(p,'17-24'=sum(`17 - 24`,na.rm = TRUE),'25-44'=sum(`25 - 44`,na.rm = TRUE),'>=75'=sum(`>=75`,na.rm = TRUE),'45-64'=sum(`45 - 64`,na.rm = TRUE),'65-74'=sum(`65 - 74`,na.rm = TRUE))


x$sum1<-x$`17-24`+x$`25-44`+x$`45-64`+x$`65-74`+x$`>=75`
x<-x[c(-2:-6)]
#lat lng
#30.8296° N, 35.0388° E דרום  13791
#32.8972° N, 35.3027° E צפון  11643
#31.9521° N, 34.9066° E מרכז  22311
#32.4814° N, 34.9948° E חיפה  15664
#31.7648° N, 34.9948° E ירושלים  12416
#31.9032° N, 35.2035° E יהודה ושומרון  339
#32.0853° N, 34.7818° E תל אביב  29760
unique(p$district)

m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng=35.0388, lat=30.8296, popup="מחוז דרום-13791 תאונות")
m <- addMarkers(m, lng=35.3027, lat=32.8972, popup="מחוז צפון-11643 תאונות")
m <- addMarkers(m, lng=34.9066, lat=31.9521, popup="מחוז מרכז-22311 תאונות")
m <- addMarkers(m, lng=34.9948, lat=32.4814, popup="מחוז חיפה-15664 תאונות")
m <- addMarkers(m, lng=34.9948, lat=31.7648, popup="מחוז ירושלים-12416 תאונות")
m <- addMarkers(m, lng=35.2035, lat=31.9032, popup="מחוז יהודה ושומרון-339 תאונות")
m <- addMarkers(m, lng=34.7818, lat=32.0853, popup="מחוז תל אביב-29760 תאונות")
m
saveWidget(m, file="Map.html")


#####map tree - לא שומש 
p<-rbind(Table_2021[c(-1:-6,-13:-23)],Table_2020[c(-1:-6,-13:-23)],Table_2019[c(-1:-6,-13:-23)],Table_2018[c(-1:-6,-13:-23)],Table_2017[c(-1:-6,-13:-23)],Table_2016[c(-1:-6,-13:-23)],Table_2015[c(-1:-6,-13:-23)],Table_2014[c(-1:-6,-13:-23)],Table_2013[c(-1:-6,-13:-23)],Table_2012[c(-1:-6,-13:-23)])
p$count_motorcycle<-as.numeric(p$count_motorcycle)
p$count_taxi<-as.numeric(p$count_taxi)
p$count_bus<-as.numeric(p$count_bus)
p$count_truck<-as.numeric(p$count_truck)
p$count_car<-as.numeric(p$count_car)
p<-group_by(p,district.x)
p<-summarise(p,"פרטי":=sum(count_car),"אוטובוס"=sum(count_bus),"משאית"=sum(count_truck),"מונית"=sum(count_taxi),"אופנוע"=sum(count_motorcycle))
x<-melt(p,variable.name = "typeof",value.name = "qty")

x$district.x<-as.character(x$district.x)
x$variable<-as.character(x$variable)
as.tibble(x)
data <- data.frame(x)

ggplot(data, aes(area = qty, fill =district.x , label = typeof, subgroup = district.x) )+
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.1, colour =
                               "black", fontface = "italic", min.size = 0,cex=8) +
                               geom_treemap_text(colour = "black", place = "topleft", reflow = T)+
  scale_fill_brewer(palette = "Pastel1",direction = -1)+ 
  theme(legend.position = "bottom") +
  labs(caption = "דיאגרמה זו מציגה את המחוזות המשפיעים ביותר מבחינת כמות כלי הרכב במדינת ישראל, בתוך כל מחוז ניתן גם לראות את סוגי כלי הרכב המשפיעים ביותר בכל מחוז גם כן לפי כמות מאותו סוג***",cex=12,
       fill = "מחוזות")+
  ggtitle("הצגת המחוזות המשפיעים, ובכל מחוז את סוג הרכב המשפיע","שנים 2012-2021")+ 
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(plot.subtitle = element_text(hjust = 0.5))


#####3D Pareto pie:ages
p<-Table_2012
p<-mutate(Table_2012[c(-1:-6,-8:-17)],year=2013)
p<-rbind(p,mutate(Table_2013[c(-1:-6,-8:-17)],year=2013))
p<-rbind(p,mutate(Table_2014[c(-1:-6,-8:-17)],year=2014))
p<-rbind(p,mutate(Table_2015[c(-1:-6,-8:-17)],year=2015))
p<-rbind(p,mutate(Table_2016[c(-1:-6,-8:-17)],year=2016))
p<-rbind(p,mutate(Table_2017[c(-1:-6,-8:-17)],year=2017))
p<-rbind(p,mutate(Table_2018[c(-1:-6,-8:-17)],year=2018))
p<-rbind(p,mutate(Table_2019[c(-1:-6,-8:-17)],year=2019))
p<-rbind(p,mutate(Table_2020[c(-1:-6,-8:-17)],year=2020))
p<-rbind(p,mutate(Table_2021[c(-1:-6,-8:-17)],year=2021))
p<-mutate(p,`17 - 24`=((p$`17 - 24`)/100)*p$sum_age,`25 - 44`=((p$`25 - 44`)/100)*p$sum_age,`>=75`=((p$`>=75`)/100)*p$sum_age,`45 - 64`=((p$`45 - 64`)/100)*p$sum_age,`65 - 74`=((p$`65 - 74`)/100)*p$sum_age)
p<-mutate(p,district=district.x,district.x=NULL)
p$`17 - 24`<-(as.numeric(p$`17 - 24`))
p$`65 - 74`<-(as.numeric(p$`65 - 74`))
p$`45 - 64`<-(as.numeric(p$`45 - 64`))
p$`>=75`<-(as.numeric(p$`>=75`))
x<-summarise(p,'17-24'=sum(`17 - 24`,na.rm = TRUE),'25-44'=sum(`25 - 44`,na.rm = TRUE),'>=75'=sum(`>=75`,na.rm = TRUE),'45-64'=sum(`45 - 64`,na.rm = TRUE),'65-74'=sum(`65 - 74`,na.rm = TRUE))
x1<-colnames(x)
x2<-as.numeric(x[1:(length(x))])
pie3D(x2,mar = (rep(6,4)),theta = 1.2,shade = 0.65,
      col = hcl.colors(length(as.numeric(x2)), "Spectral"),labelcex = 1,radius = 1,main = "2013-2021,פילוח טווחי הגיל באוכלוסייה",
      labels = as.numeric(x2),
      explode = 0.1)+
  legend(x=-1.1,y=1.1,legend = x1, title="טווחי גילאים", fill=hcl.colors(length(as.numeric(x2)), "Spectral"), horiz=T, cex=.7, bg='lightblue1',x.intersp = 0.1,y.intersp = 0.7,text.width = 0.04)

#####Pairs-question 4
p<-mutate(Table_6,'00:00-01:59'=Table_6$`00:00-00:59`+Table_6$`01:00-01:59`,'02:00-03:59'=Table_6$`02:00-02:59`+Table_6$`03:00-03:59`,'04:00-05:59'=Table_6$`04:00-04:59`+Table_6$`05:00-05:59`,'06:00-07:59'=Table_6$`06:00-06:59`+Table_6$`07:00-07:59`,'08:00-09:59'=Table_6$`08:00-08:59`+Table_6$`09:00-09:59`,'10:00-11:59'=Table_6$`10:00-10:59`+Table_6$`11:00-11:59`,'12:00-13:59'=Table_6$`12:00-12:59`+Table_6$`13:00-13:59`,'14:00-15:59'=Table_6$`14:00-14:59`+Table_6$`15:00-15:59`,'16:00-17:59'=Table_6$`16:00-16:59`+Table_6$`17:00-17:59`,'18:00-19:59'=Table_6$`18:00-18:59`+Table_6$`19:00-19:59`,'20:00-21:59'=Table_6$`20:00-20:59`+Table_6$`21:00-21:59`,'22:00-23:59'=Table_6$`22:00-22:59`+Table_6$`23:00-23:59`)[c(-2:-25)]
ggpairs( 
  data = p[-1],aes(color="" ),
  lower = list(continuous = wrap("smooth", alpha = 0.5, color = "blue") 
  ))+
  theme_grey(base_size = 9,base_rect_size = 14)+
  ggtitle("טבלת זוגות להצגת תלות בין טווחי השעות השונים","בין השנים 2003-2022")+
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(plot.subtitle = element_text(hjust = 0.5))+
  #labs(x="כמות תאונות בטווח שני",y= "כמות תאונות בטווח ראשון")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, color = "black"))+
  theme(axis.text.y = element_text(angle = 45, hjust = 1, color = "black"))
#####Interactive tree map 
p<-rbind(Table_2021[c(-1:-6,-13:-23)],Table_2020[c(-1:-6,-13:-23)],Table_2019[c(-1:-6,-13:-23)],Table_2018[c(-1:-6,-13:-23)],Table_2017[c(-1:-6,-13:-23)],Table_2016[c(-1:-6,-13:-23)],Table_2015[c(-1:-6,-13:-23)],Table_2014[c(-1:-6,-13:-23)],Table_2013[c(-1:-6,-13:-23)],Table_2012[c(-1:-6,-13:-23)])
p$count_motorcycle<-as.numeric(p$count_motorcycle)
p$count_taxi<-as.numeric(p$count_taxi)
p$count_bus<-as.numeric(p$count_bus)
p$count_truck<-as.numeric(p$count_truck)
p$count_car<-as.numeric(p$count_car)
p<-group_by(p,district.x)
p<-summarise(p,"פרטי":=sum(count_car),"אוטובוס"=sum(count_bus),"משאית"=sum(count_truck),"מונית"=sum(count_taxi),"אופנוע"=sum(count_motorcycle))
x<-melt(p,variable.name = "typeof",value.name = "qty")


x$district.x<-as.character(x$district.x)
as.tibble(x)
data1 <- data.frame(x)
data2 <- data1$district.x[order(data1$district.x,decreasing = F)]
dt<-group_by(data1,district.x)
dt<-summarise(dt,tot=sum(qty))
dt1 <- data.frame(
  subtopic = c("כללי","מחוז דרום","מחוז חיפה","מחוז יהודה ושומרון","מחוז ירושלים","מחוז מרכז","מחוז צפון","מחוז תל אביב","רכב פרטי","רכב פרטי","רכב פרטי","רכב פרטי","רכב פרטי","רכב פרטי","רכב פרטי","אוטובוס","אוטובוס","אוטובוס","אוטובוס","אוטובוס","אוטובוס","אוטובוס","משאית","משאית","משאית","משאית","משאית","משאית","משאית","מונית","מונית","מונית","מונית","מונית","מונית","מונית","אופנוע","אופנוע","אופנוע","אופנוע","אופנוע","אופנוע","אופנוע"),
  topic = c("","כללי","כללי","כללי","כללי","כללי","כללי","כללי","מחוז דרום","מחוז חיפה","מחוז יהודה ושומרון","מחוז ירושלים","מחוז מרכז","מחוז צפון","מחוז תל אביב","מחוז דרום","מחוז חיפה","מחוז יהודה ושומרון","מחוז ירושלים","מחוז מרכז","מחוז צפון","מחוז תל אביב","מחוז דרום","מחוז חיפה","מחוז יהודה ושומרון","מחוז ירושלים","מחוז מרכז","מחוז צפון","מחוז תל אביב","מחוז דרום","מחוז חיפה","מחוז יהודה ושומרון","מחוז ירושלים","מחוז מרכז","מחוז צפון","מחוז תל אביב","מחוז דרום","מחוז חיפה","מחוז יהודה ושומרון","מחוז ירושלים","מחוז מרכז","מחוז צפון","מחוז תל אביב"),
  n= c(sum(dt$tot),dt$tot,data1$qty))

p <- plot_ly(
  dt1,size=8,height = 8,width = 5,
  type='treemap',
  labels=~subtopic,
  parents=~topic,
  values= ~n
)
p
if (interactive()) p
htmlwidgets::saveWidget( "treemap.html",selfcontained = FALSE)

##############################################

dead_color <- colorRampPalette(c("#eb3349", "#f45c43")) 
severly_injured_color <- colorRampPalette(c("#56ab2f", "#a8e063")) 
lightly_moderately_injured_color <- colorRampPalette(c("#ff7e5f", "#feb47b")) 


#####barplot-NPV
dead_color <- colorRampPalette(c("#eb3349", "#f45c43")) 
severly_injured_color <- colorRampPalette(c("#56ab2f", "#a8e063")) 
lightly_moderately_injured_color <- colorRampPalette(c("#ff7e5f", "#feb47b")) 

Time_Periods <- c("לפני הפרויקט", "אחרי הפרויקט")
dead <-c(all_year[4,3]$sum1, all_year[1,3]$sum1)
severly_injured <- c(all_year[5,3]$sum1, all_year[2,3]$sum1)
lightly_moderately_injured <- c(all_year[6,3]$sum1, all_year[3,3]$sum1)

text <- c(all_year[1,3]$sum1,  all_year[2,3]$sum1, all_year[3,3]$sum1, all_year[4,3]$sum1, all_year[5,3]$sum1, all_year[6,3]$sum1)
data <- data.frame(Time_Periods, dead, severly_injured,lightly_moderately_injured)

fig <- plot_ly(data, x = ~Time_Periods, y = ~dead, type = 'bar', name = "הרוג",text = severly_injured,
               marker = list(color =dead_color(5),
                             line = list(color = 'black',width = 1)))
fig <- fig %>% add_trace(y = ~severly_injured, name = "פצוע קשה",text = severly_injured,
                         marker = list(color =severly_injured_color(5),
                                       line = list(color = 'black',width = 1)))
fig <- fig %>% add_trace(y = ~lightly_moderately_injured, name = "פצוע קל-בינוני",text = lightly_moderately_injured,
                         marker = list(color =lightly_moderately_injured_color(5),
                                       line = list(color = 'black',width = 1)))
fig <- fig %>% layout(
  yaxis = list(title = "קורבנות", titlefont = list(family = 'Arial' ,size = 16, color = 'rgb(107, 107, 107)')),
  xaxis= list(title = "תקופות", titlefont = list(family = 'Arial', size = 16, color = 'rgb(107, 107, 107)')))

fig

#####


###########NPV GRAPH percent
gg<-after_NPV$npv_cl-1000
gg<-c(-1000,gg)
names(gg)<-2013:2019
gg1<-as.data.frame(matrix(c(names(gg),gg),length(gg),2))
gg1$perc<-(abs(as.numeric(gg1$V2)/10)-100)/100
gg1$perc<-gg1$perc*-1
for(i in 1:nrow(gg1))
{
  if(as.numeric(gg1$perc[i])>0)
  {
    gg1$stat[i]<-"חיובי"
  }
  else
  {
    gg1$stat[i]<-"שלילי"
  }
}


###באחוזים
ggplot(gg1, aes(V1, perc)) +
  geom_bar(stat = "identity", aes(fill = stat), legend = FALSE) + 
  geom_text(aes(label = paste(round(perc,4) * 100, "%"),
                vjust = ifelse(perc >= 0, 0, 1)))+
  ggtitle("החזר השקעה באחוזים ","בין השנים 2013-2019")+ 
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(plot.subtitle = element_text(hjust = 0.5))+
  labs(x="שנים",y="אחוז החזר ההשקעה",fill="סטטוס החזר השקעה")+
  scale_fill_brewer(palette= "Set2")

#### במיליונים
gg1$V2<-round(as.numeric(gg1$V2),5)
gg1$zerol<--1000
ggplot(gg1, aes(V1, V2)) +
  geom_bar(stat = "identity", aes(fill = V1), legend = FALSE) + 
  geom_text(aes(label = V2,
                vjust = -2))+
  ggtitle("החזר השקעה במיליוני שקלים ","בין השנים 2013-2019")+ 
  theme(plot.title = element_text(hjust = 0.5))+ 
  theme(plot.subtitle = element_text(hjust = 0.5))+
  labs(x="שנים",y="מיליוני שקלים",fill="שנים")+
  scale_fill_brewer(palette= "Set3",direction = -1)+
  geom_hline(yintercept = -1000,col="red",cex=1)+
  scale_y_continuous(breaks=seq(-1200, 0, 200), limits=c(-1200,0))


