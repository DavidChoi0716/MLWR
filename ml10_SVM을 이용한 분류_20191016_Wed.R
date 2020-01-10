# SVM(Support Vector Machine)을 이용한 분류

# 1. 데이터 준비
setwd("C:/dev/lab-r")
letters <- read.csv("mlwr/letterdata.csv")

# 2. 데이터 확인, 전처리
str(letters)
head(letters)
table(letters$letter)
prop.table(table(letters$letter))

# 학습 데이터(80%) / 테스트 데이터(20%) 세트
letters_train <- letters[1:16000,]
letters_test <- letters[16001:20000,]

table(letters_train$letter)
table(letters_test$letter)

prop.table(table(letters_train$letter))
prop.table(table(letters_test$letter))

# 3. 모델 생성 - SVM 
# kernlab
install.packages("kernlab")
library(kernlab)
search()
detach("package:kernlab")
search()
install.packages("kernlab")
library(kernlab)
search()

# SVM 알고리즘 모델을 생성
letter_classifier <- ksvm(letter ~ . , data = letters_train, kernel = "vanilladot")
letter_classifier_2 <- ksvm(letter ~ . , data = letters_train, kernel = "polydot")


# 4. 모델 평가
letters_predict <- predict(letter_classifier, letters_test)
head(letters_predict)
table(letters_predict, letters_test$letter)

letters_predict[1]
letters_test$letter[1]
letters_predict[1] == letters_test$letter[1]

correct <- ifelse(letters_predict == letters_test$letter, 1, 0)
correct_count <- sum(correct)
correct_count  # SVM 모델이 문자들을 제대로 구분한 갯수
correct_ratio <- correct_count / 4000
correct_ratio # 약 0.84

# 5. 모델 수정  -> 재평가 -> 성능 개선
classifier3 <- ksvm(letter ~ . , data = letters_train, kernel = "rbfdot") #Gaussian이 대부분의 경우에 성능을 높혀줌 (논문)
predict3 <- predict(classifier3, letters_test)
head(predict3, n=10)
head(letters_test$letter, n = 10)

table(predict3, letters_test$letter)
correct3 <- ifelse(predict3 == letters_test$letter, 1, 0)
correct3_count <- sum(correct3)
correct3_count
correct3_ratio <- correct3_count / 4000
correct3_ratio # 0.93

correct_ratio
















    
























