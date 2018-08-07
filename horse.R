horse_test = 'C://Users//Minyeong Lee//Desktop//horse-colic_data.txt'
horse_test2 = 'C://Users//계정명//Desktop//horse-colic_data_2.txt'
horse_test3 = 'C://Users//계정명//Desktop//horse-colic_test_2.txt'
test = read.table(horse_test, header=T)
test3 = read.table(horse_test3, header=T)
head(test)
# test2 = na.omit(test)
test2 = read.table(horse_test2, header=T)
str(test2)
# install.packages("ggplot2")
library(ggplot2)
# 30% missin value
# horse colic(말 복통) data
str(test)
test[1,] #1행
rectalT = test[,4] #연속형
as.data.frame(rectalT)
extremT = test[,7] #범주형(명목형?순서형?)
as.data.frame(extremT)
drop.levels(rectalT)
rectalT <- as.numeric(as.character(rectalT))
class(rectalT)
class(extremT)

table(extremT) #Null값 56개
table(rectalT) #Null값 60개
e <- within(extremT)
boxplot(rectalT ~ extremT)
plot(rectalT ~ extremT)

ggplot(aes(as.data.frame(factor(rectalT)), as.data.frame(factor(extremT))))

################################################################################
# removed NA on only 4th $ 7th col.
rect_ext <- data.frame('rectal'=test2[,4], 'extrm'=test2[,7])
head(rect_ext)
r_rect_ext <- na.omit(rect_ext)
r_rect_ext[,2] <- as.factor(r_rect_ext[,2])
str(r_rect_ext)
plot(r_rect_ext$extrm ~ r_rect_ext$rectal)
plot(r_rect_ext$rectal ~ r_rect_ext$extrm) #이거다.

################################################################################

r_rectalT <- na.omit(test2)[,4]
r_rectalT<- as.numeric(as.character(r_rectalT))
r_extremT <- na.omit(test2)[,7]
summary(r_rectalT)
summary(r_extremT)
plot(r_rectalT ~ r_extremT)

### train set과 test set의 결측치 개수 세느라
sum(is.na(test2))
sum(is.na(test3))
1923/10304*100 #train과 test 합한 전체 데이터셋에서 결측치 비율 계산


test[,25]
test[,26]
temp <- data.frame('rectal'=test2[,4], 'extrm'=test2[,7])
str(temp) #300 obs
temp <- na.omit(temp)
str(temp) #198 obs

anly <- aov(rectal~extrm, data=temp)
summary(anly)
anly
anly2 <- lm(rectal~extrm, data=temp)
summary(anly2)

install.packages("DMwR")
library(DMwR)
a<-knnImputation(data=temp)
b <- knnImputation(test2)
# knnImputation 함수 안먹힌다 
# -> 'Not sufficient complete cases for computing neighbors.'
plot(test[,4]~test[,7])
boxplot(test2[,4]~test2[,7])
synth <- rbind(test2, test3)
str(synth)
c <- knnImputation(synth) #368개로도 부족함.

check_missing_value <- function(x){sum(is.na(x))} #/length(x)*100
check_missing_value(synth)
apply(synth, 2, check_missing_value) #apply(데이터셋, 열방향(행방향은 1), 기능)
