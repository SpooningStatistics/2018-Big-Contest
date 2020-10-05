movie<-read.csv("c:\\data\\movie.csv")

#genre�� ����
movie$genre[movie$repGenreNm=="�׼�"]<-"�׼�"
movie$genre[movie$repGenreNm=="����"]<-"�׼�"
movie$genre[movie$repGenreNm=="SF"]<-"�׼�"
movie$genre[movie$repGenreNm=="��Ÿ��"]<-"�׼�"
movie$genre[movie$repGenreNm=="��庥ó"]<-"�׼�"
movie$genre[movie$repGenreNm=="���"]<-"���"
movie$genre[movie$repGenreNm=="�ڹ̵�"]<-"���"
movie$genre[movie$repGenreNm=="����"]<-"���"
movie$genre[movie$repGenreNm=="������"]<-"���"
movie$genre[movie$repGenreNm=="���"]<-"���"
movie$genre[movie$repGenreNm=="����"]<-"���"
movie$genre[movie$repGenreNm=="�ִϸ��̼�"]<-"�ִϸ��̼�"
movie$genre[movie$repGenreNm=="���/�θǽ�"]<-"���/�θེ"
movie$genre[movie$repGenreNm=="����(ȣ��)"]<-"����"
movie$genre[movie$repGenreNm=="������"]<-"����"
movie$genre[movie$repGenreNm=="�̽��͸�"]<-"����"

#�ִϸ��̼� ����
install.packages("dplyr")
library(dplyr)
movie<-movie %>% filter(genre!="�ִϸ��̼�")

#holiday �� ����

movie$holiday<-ifelse(movie$movieNm %in% c("������Ž�� : ����� ���� ��","ŷ���� : ��ũ�� ������Ʈ","���󿡸� : ���� ���� ��",
                                   "��Ž�� �ڳ� : �ڳ� ������� - ��� �־��� ��Ʋ","������ ������: ���ƿ� ���ν�",
                                   "�̹����̼� ����","�������� 3D","��: ���� ����","��������","��������Ʈ",
                                   "����","Ž�� : �� ����","�˻����","�ٺ�� ���۹��: �ǵ� ��庥ó",
                                   "�ְ����� �̴�Ư����: ������ ź��","ĳ��","�ſﳪ���� �ٸ���",
                                   "������, �뵿������","�޺��ñ�","�κ� ũ���","����","�峭���� ����ִ�",
                                   "�帲 ��","�ű״��Ǽ�Ʈ 7","����","ī�� �һ��̾�Ƽ","����","�� ŷ",
                                   "�ʹ׸�ī��W: �����̷��� ��Ȱ","������ ȣ������","������Ʈ �̺�: �ĸ��� ��",
                                   "¯���� ������ ������: ��ǳ����! �޲ٴ� ���� �뵹��","���̾�","���� ĵ ����ũ",
                                   "������ �䱫��ġ: �ϴ��� ���� ������ ��������ٳ�!","ŷ����: ��� ��Ŭ",
                                   "���� ���ڰ� ����","���ѻ꼺","���� 2","��","���˵���","������Ž��: ���������� ���",
                                   "�е��� 2","��罽����","��Ž�� �ڳ�:������ ��","���� �Ҽ�","���: �۷� ������ �ٲ� ��",
                                   "50���� �׸���: �ع�","������ ���� ����: ����� ���","�������� �������"),1,0)


#nation �� ����
movie$nation<-ifelse(movie$nation=="�ѱ�","�ѱ�","�ܱ�")

c$date<-paste(substr(c$openDt,1,4),substr(c$openDt,5,6),sep = "-")
c$date<-paste(c$date,substr(c$openDt,7,8),sep = "-")
c$date<-as.Date(c$date)

#count �� 
for (i in movie$date){
  t<-0
  for (q in movie$date) {
    if ( q<=i+7 & q>=i-7) {t= t+1}
  }
  movie$count7[movie$date == i]<-t-1
}

for (i in movie$date){
  t<-0
  for (q in movie$date) {
    if ( q<=i+10 & q>=i-10) {t= t+1}
  }
  movie$count10[movie$date == i]<-t-1
}

#rate ��
movie$rate<-(movie$acd23-movie$acd18)/movie$acd23

#factor ó��
movie$only<-as.factor(movie$only)
movie$holiday<-as.factor(movie$holiday)


#ȸ�͸�
full<-lm(log(N_audience)~log(acd23)+log(d1)+log(actor_mean)+log(score)+sqrt(rate)+log(mean_rank)+log(mean_screen)+
        +genre+log(N_dislike)+log(N_like)+log(N_people)+nation+watchGradeNm+
          log(preview_screen)+log(preview_audience)+holiday+only+log(count10+1),data=movie)

install.packages("mixlm")
library(mixlm)
stepWise(full, alpha.enter = 0.15, 
         alpha.remove = 0.15, full=TRUE)

lm1<-lm(formula = log(N_audience) ~ log(acd23) + sqrt(rate) + 
          log(count10 + 1) , data = movie)

summary(lm1)
str(movie)
vif(lm1)
plot(lm1)


full2<-lm(log(acd19)~log(acd2)+log(d2)+log(actor_mean)+log(score)+log(d1_rank)+log(d2_rank)+log(d1)+
            log(d1_screen)+log(d2_screen)+genre+log(N_dislike)+log(N_like)+log(N_people)+nation+watchGradeNm+
          log(preview_screen)+log(preview_audience)+holiday+only+log(count10+1),data=movie)

stepWise(full2, alpha.enter = 0.15, 
         alpha.remove = 0.15, full=TRUE)


lm3<-lm(formula = log(acd19) ~ log(actor_mean) + log(d2_rank) + 
     holiday + log(d2) + nation + 
     log(preview_audience) + log(preview_screen) + log(N_like) + 
     log(count10 + 1) , data = movie)

plot(lm3)
summary(lm3)
vif(lm3)

#genre,watchGrade ��ó��
movie$genre5<-ifelse(movie$genre=="�׼�","1",ifelse(movie$genre=="���/�θེ","2",
                                                  ifelse(movie$genre=="���","3","4")))
movie$genre5<-as.factor(movie$genre5)
movie$grade4<-ifelse(movie$watchGradeNm=="��ü������","1",ifelse(movie$watchGradeNm=="12���̻������","2",
                                                            ifelse(movie$watchGradeNm=="15���̻������","3","4")))
movie$grade4<-as.factor(movie$grade4)

#cross validation
install.packages("plyr")
install.packages("randomForest")
library(plyr)
library(randomForest)