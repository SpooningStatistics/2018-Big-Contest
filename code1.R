movie<-read.csv("c:\\data\\movie.csv")

#genre값 정리
movie$genre[movie$repGenreNm=="액션"]<-"액션"
movie$genre[movie$repGenreNm=="범죄"]<-"액션"
movie$genre[movie$repGenreNm=="SF"]<-"액션"
movie$genre[movie$repGenreNm=="판타지"]<-"액션"
movie$genre[movie$repGenreNm=="어드벤처"]<-"액션"
movie$genre[movie$repGenreNm=="드라마"]<-"드라마"
movie$genre[movie$repGenreNm=="코미디"]<-"드라마"
movie$genre[movie$repGenreNm=="가족"]<-"드라마"
movie$genre[movie$repGenreNm=="뮤지컬"]<-"드라마"
movie$genre[movie$repGenreNm=="사극"]<-"드라마"
movie$genre[movie$repGenreNm=="전쟁"]<-"드라마"
movie$genre[movie$repGenreNm=="애니메이션"]<-"애니메이션"
movie$genre[movie$repGenreNm=="멜로/로맨스"]<-"멜로/로멘스"
movie$genre[movie$repGenreNm=="공포(호러)"]<-"공포"
movie$genre[movie$repGenreNm=="스릴러"]<-"공포"
movie$genre[movie$repGenreNm=="미스터리"]<-"공포"

#애니메이션 제거
install.packages("dplyr")
library(dplyr)
movie<-movie %>% filter(genre!="애니메이션")

#holiday 값 정리

movie$holiday<-ifelse(movie$movieNm %in% c("조선명탐정 : 사라진 놉의 딸","킹스맨 : 시크릿 에이전트","도라에몽 : 스탠 바이 미",
                                   "명탐정 코난 : 코난 실종사건 - 사상 최악의 이틀","오즈의 마법사: 돌아온 도로시",
                                   "이미테이션 게임","스폰지밥 3D","뮨: 달의 요정","서부전선","에베레스트",
                                   "인턴","탐정 : 더 비기닝","검사외전","앨빈과 슈퍼밴드: 악동 어드벤처",
                                   "최강전사 미니특공대: 영웅의 탄생","캐롤","거울나라의 앨리스",
                                   "고산자, 대동여지도","달빛궁궐","로빈슨 크루소","밀정","장난감이 살아있다",
                                   "드림 쏭","매그니피센트 7","벤허","카페 소사이어티","공조","더 킹",
                                   "터닝메카드W: 블랙미러의 부활","딥워터 호라이즌","레지던트 이블: 파멸의 날",
                                   "짱구는 못말려 극장판: 폭풍수면! 꿈꾸는 세계 대돌격","라이언","아이 캔 스피크",
                                   "극장판 요괴워치: 하늘을 나는 고래와 더블세계다냥!","킹스맨: 골든 서클",
                                   "레고 닌자고 무비","남한산성","넛잡 2","딥","범죄도시","조선명탐정: 흡혈괴마의 비밀",
                                   "패딩턴 2","골든슬럼버","명탐정 코난:감벽의 관","블랙 팬서","흥부: 글로 세상을 바꾼 자",
                                   "50가지 그림자: 해방","셰이프 오브 워터: 사랑의 모양","월요일이 사라졌다"),1,0)


#nation 값 정리
movie$nation<-ifelse(movie$nation=="한국","한국","외국")

c$date<-paste(substr(c$openDt,1,4),substr(c$openDt,5,6),sep = "-")
c$date<-paste(c$date,substr(c$openDt,7,8),sep = "-")
c$date<-as.Date(c$date)

#count 값 
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

#rate 값
movie$rate<-(movie$acd23-movie$acd18)/movie$acd23

#factor 처리
movie$only<-as.factor(movie$only)
movie$holiday<-as.factor(movie$holiday)


#회귀모델
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

#genre,watchGrade 전처리
movie$genre5<-ifelse(movie$genre=="액션","1",ifelse(movie$genre=="멜로/로멘스","2",
                                                  ifelse(movie$genre=="드라마","3","4")))
movie$genre5<-as.factor(movie$genre5)
movie$grade4<-ifelse(movie$watchGradeNm=="전체관람가","1",ifelse(movie$watchGradeNm=="12세이상관람가","2",
                                                            ifelse(movie$watchGradeNm=="15세이상관람가","3","4")))
movie$grade4<-as.factor(movie$grade4)

#cross validation
install.packages("plyr")
install.packages("randomForest")
library(plyr)
library(randomForest)
