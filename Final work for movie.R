#파이널 작업
setwd("c:/R/final")
r<-read.csv("re.csv")
r$only<-as.factor(r$only)
r$holiday<-as.factor(r$holiday)
count(distinct(f$movieNm))
install.packages("dplyr")
library(dplyr)
distinct(f,movieNm)
install.packages("lmtest")
library(lmtest)
install.packages("mixlm")
library(mixlm)
install.packages("ggplot2")
library(ggplot2)
library(bindrcpp)
#회귀분석
glimpse(f)
f$only<-as.factor(f$only)
f$holiday<-as.factor(f$holiday)
model<-lm(log(acd18)~log(N_people)+log(N_like)+holiday+genre+log(d2)+
            watchGradeNm+log(actor_mean)+nation+log(preview_audience),data=a)
hist(a$acd18^-0.2)
attach(a)
plot(log(N_dislike),sqrt(acd18))
plot(log(N_dislike),log(acd18))
plot(log(d2),log(acd18))

model<-lm(log(N_audience)~log(N_people)+log(N_like)+holiday+genre+sqrt(mean_screen)+sqrt(mean_rank)+log(d1)+
             watchGradeNm+log(actor_mean)+log(acd23)+nation+log(preview_audience)+count10+only,data=f)
stepWise(model, alpha.enter = 0.15, 
         alpha.remove = 0.15, full=TRUE)
fm<-lm(formula = log(f$N_audience) ~ log(acd23) + sqrt(rate), data = f)
summary(fm)
plot(fm)
f_copy<-f %>% select(N_audience,acd23,rate)
par(mfrow=c(2,2))
#다중공선성
install.packages("psych")
library(psych)
pairs.panels(f_copy)
vif(fm)
par(mfrow = c(1,1))
plot(fm)
plot(sqrt(f$mean_rank),log(f$N_audience))
plot(f$rate,log(f$N_audience))
boxplot(f$N_audience)$stats
qplot(f$N_audience)
f_copy<-f
f_copy$N_audience<-ifelse(f_copy$N_audience < 265341, NA, f_copy$N_audience)
f_copy<- f_copy %>% filter(!is.na(N_audience))
hist(log(f$N_audience))
car::powerTransform(f$N_audience)
vif(fm)
hist(f$N_audience^-0.5)
hist(log(f$N_audience))

#관객수에 따른 분석
f1<-f %>% filter(N_audience <= 1000000)
fm1<-lm(formula = N_audience^-0.5 ~ log(acd23) + sqrt(mean_rank)+sqrt(rate)+
         sqrt(mean_screen), data = f1)
plot(fm1)
hist(log(f1$N_audience))
f2<-f %>% filter(N_audience >= 1000000 & N_audience <=2000000)
fm2<-lm(formula = log(N_audience) ~ log(acd23) + sqrt(mean_rank)+sqrt(rate)+
          sqrt(mean_screen), data = f2)
plot(fm2)
hist(log(f2$N_audience))
f3<-f %>% filter(N_audience >= 2000000 & N_audience <=3000000)
fm3<-lm(formula = log(N_audience) ~ log(acd23) + sqrt(mean_rank)+sqrt(rate)+
          sqrt(mean_screen), data = f3)
plot(fm3)
hist(log(f3$N_audience))
f4<-f %>% filter(N_audience >= 3000000)
hist(log(f4$N_audience))

#rate 분석
glimpse(a)
install.packages("corrplot")
library(corrplot)
r<-read.csv("re.csv")
rc<-r
rc$rate2318<-(rc$acd23-rc$acd18)/rc$acd23
rc$rate2319<-(rc$acd23-rc$acd19)/rc$acd23
rc$rate2322<-(rc$acd23-rc$acd22)/rc$acd23
rc$realrate<-(rc$N_audience-rc$acd23)/rc$N_audience
rc$new_aud<-rc$N_audience-rc$acd23

glimpse(rc)
cor(rc[,72:75])
x<-cor(rc[,72:75])
corrplot(x, method="ellipse")
corrplot(x, method="pie")
plot(sqrt(rate),log(N_audience))
abline(model)
model<-lm(log(N_audience)~sqrt(rate1),data=rc)
summary(model)
model<-lm(log(N_audience)~sqrt(rate2),data=rc)
summary(model)
model<-lm(log(N_audience)~sqrt(rate3),data=rc)
summary(model)
model2<-lm(log(new_aud+1)~sqrt(rate1),data=rc)
summary(model2)
model2<-lm(log(new_aud+1)~sqrt(rate2),data=rc)
summary(model2)
model2<-lm(log(new_aud+1)~sqrt(rate3),data=rc)
summary(model2)
corrplot(x, 
         +          method="shade", 
         +          addshade="all", # 상관관계 방향선 제시
           +          tl.col="red", # 라벨 색 지정
         +          tl.srt=30, # 위쪽 라벨 회전 각도
         +          diag=FALSE, # 대각선 값 미제시
         +          addCoef.col="black", # 상관계수 숫자 색
         +          order="FPC")

corrplot(x,method="circle",addshade="all",col="skyblue",tl.col="black",tl.srt=0,addCoef.col="black",diag=FALSE)
attach(rc)
plot(sqrt(rate1), log(N_audience))
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(x, histogram=TRUE, pch=19)
corrplot(x, method="number")

attach(rc)
cor(rate,log(N_audience))
?hist
hist(r$N_audience, breaks=30, freq=TRUE,main="총관객수 히스토그램",xlab="총관객수",ylab="빈도",col="skyblue")
hist(log(r$N_audience), breaks=30, freq=TRUE,main="로그총관객수 히스토그램",xlab="로그총관객수",ylab="빈도",col="skyblue")
lines(density(log(r$N_audience,col="black")
hist(log(r$N_audience), breaks=30, freq=TRUE,col="skyblue")
hist(r$acd19, breaks=30, freq=TRUE)
hist(log(r$acd19), breaks=30, freq=TRUE)
