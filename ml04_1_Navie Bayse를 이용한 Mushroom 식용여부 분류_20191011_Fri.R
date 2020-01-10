rm(list = ls())

# Naive Bayse를 이용한 mushroom 분류
mushroom_raw <- read.csv(file = "mlwr/mushrooms.csv", stringsAsFactors = F, encoding = "UTF-8")
str(mushroom_raw)

# type 변수(특징)를 팩터로
mushroom_raw$type <- factor(mushroom_raw$type)
str(mushroom_raw$type)
table(mushroom_raw$type)
prop.table(table(mushroom_raw$type))

  
# 학습 데이터 세트(75%)와 테스트 데이터 세트(25%)로 나눔
mushroom_train <- mushroom_raw[1:6000,]
mushroom_test <- mushroom_raw[6001:8124,]

# 학습 데이터 레이블, 테스트 데이터 레이블(posinous/edible)
mushroom_train_label <- mushroom_raw[1:6000,1]
mushroom_test_label <- mushroom_raw[6001:8124,1]
table(mushroom_train_label)
table(mushroom_test_label)

  
# 나이브 베이즈 분류 알고리즘을 구현한 패키지를 설치
install.packages("e1071")
library(e1071)
search()

# 학습 데이터 세트를 가지고 분류기(classifier)를 생성
mushroom_classifier <- naiveBayes(mushroom_train, mushroom_train_label)
# 분류기를 사용해서 테스트 데이터의 분류 결과를 예측
mushroom_pred <- predict(mushroom_classifier, mushroom_test)
mushroom_pred


library(gmodels)
search()
CrossTable( x = mushroom_test_label, y = mushroom_pred, prop.chisq = F, prop.c = F, prop.r = F)

rm(list=ls())
