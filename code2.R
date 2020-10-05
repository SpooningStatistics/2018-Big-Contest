#cross validation
install.packages("plyr")
install.packages("randomForest")
library(plyr)
library(randomForest)

rf1$only<-as.factor(rf1$only)
rf1$holiday<-as.factor(rf1$holiday)
rf1$genre5<-as.factor(rf1$genre5)
rf1$grade4<-as.factor(rf1$grade4)
rf1$nation1<-as.factor(rf1$nation1)
str(rf1)

rf2$only<-as.factor(rf2$only)
rf2$holiday<-as.factor(rf2$holiday)
rf2$genre5<-as.factor(rf2$genre5)
rf2$grade4<-as.factor(rf2$grade4)
rf2$nation1<-as.factor(rf2$nation1)
str(rf2)


#randomforest model1

progress.bar <-create_progress_bar("text")
progress.bar$init(k)

r2<-data.frame()
for (i in 1:10){
  k=10
  rf1$id<-sample(1:k,nrow(rf1),replace = TRUE)
  list<-1:k
  
  prediction <-data.frame()
  testsetcopy<-data.frame()
  
  for (i in 1:k){
    trainingset <-subset(rf1, id %in% list[-i])
    testset <-subset(rf1, id%in% c(i))
    model1 <- randomForest(N_audience~.,data = trainingset)
    temp <-as.data.frame(predict(model1,testset[,-1]))
    prediction <- rbind(prediction,temp)
    testsetcopy <- rbind(testsetcopy,as.data.frame(testset[,1]))

}
progress.bar$step()
result <- cbind(prediction, testsetcopy[,1])
names(result) <-c("predicted","actual")
r1<-RMSE(result$predicted,result$actual)
r2<-rbind(r2,r1)
}
plot(result$predicted,result$actual)
abline(0,1)



#randomforest model2

progress.bar <-create_progress_bar("text")
progress.bar$init(k)

r3<-data.frame()
for (i in 1:10){
  k=10
  rf2$id<-sample(1:k,nrow(rf2),replace = TRUE)
  list<-1:k
  
  prediction <-data.frame()
  testsetcopy<-data.frame()
  for (i in 1:k){
    trainingset <-subset(rf2, id %in% list[-i])
    testset <-subset(rf2, id%in% c(i))
    model1 <- randomForest(acd19~.,data = trainingset)
    temp <-as.data.frame(predict(model1,testset[,-1]))
    prediction <- rbind(prediction,temp)
    testsetcopy <- rbind(testsetcopy,as.data.frame(testset[,1]))
  }

progress.bar$step()
result <- cbind(prediction, testsetcopy[,1])
names(result) <-c("predicted","actual")
r1<-RMSE(result$predicted,result$actual)
r3<-rbind(r3,r1)
}



#È¸±ÍºÐ¼® model1

progress.bar <-create_progress_bar("text")
progress.bar$init(k)
r4<-data.frame()
for (i in 1:10){
  k=10
  movie$id<-sample(1:k,nrow(movie),replace = TRUE)
  list<-1:k
  
  prediction <-data.frame()
  testsetcopy<-data.frame()
  
  for (i in 1:k){
    trainingset <-subset(movie, id %in% list[-i])
    testset <-subset(movie, id%in% c(i))
    model1 <-lm(log(N_audience)~log(acd23)+sqrt(rate)+log(count10+1),data=trainingset)
    temp <-as.data.frame(predict(model1,testset[,-27]))
    prediction <- rbind(prediction,temp)
    testsetcopy <- rbind(testsetcopy,as.data.frame(testset[,27]))
    }

progress.bar$step()
result <- cbind(prediction, testsetcopy[,1])
names(result) <-c("predicted","actual")
result$predicted<-exp(result$predicted)
r1<-RMSE(result$predicted,result$actual)
r4<-rbind(r4,r1)
}


#È¸±ÍºÐ¼® model2

progress.bar <-create_progress_bar("text")
progress.bar$init(k)
r5<-data.frame()

for (i in 1:10) {
  
  k=10
  movie$id<-sample(1:k,nrow(movie),replace = TRUE)
  list<-1:k
  prediction <-data.frame()
  testsetcopy<-data.frame()
  
  for (i in 1:k){
    trainingset <-subset(movie, id %in% list[-i])
    testset <-subset(movie, id%in% c(i))
    model1 <-lm(formula = log(acd19) ~ log(acd2) + log(actor_mean) + log(d2_rank) + 
                  + holiday +  log(d2_screen) + nation + log(N_like) + log(preview_screen)
                + log(preview_audience) +  log(count10 + 1), data = trainingset)
    temp <-as.data.frame(predict(model1,testset[,-24]))
    prediction <- rbind(prediction,temp)
    testsetcopy <- rbind(testsetcopy,as.data.frame(testset[,24]))
    }
progress.bar$step()
result <- cbind(prediction, testsetcopy[, 1])
names(result) <- c("predicted", "actual")
result$predicted<-exp(result$predicted)
r1<-RMSE(result$predicted,result$actual)
r5<-rbind(r5,r1)
}

predict()

