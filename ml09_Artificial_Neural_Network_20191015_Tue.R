rm(list = ls())

# Artificial Neural Network (인공 신경망)
# f(x) = 2x + 1
curve(expr = 2 * x + 1, from = -5, to = 5)
# sigmoid 함수: f(x) = 1 / [1 + exp(-x)]
curve(expr = 1 / (1 + exp(-x)), from = -10, to = 10)
# hypobolic tangent: f(x) = tanh(x)
curve(expr = tanh(x), from = -5, to = 5)

# 콘크리트의 강도 예측
# 1. 데이터 준비
setwd("C:/dev/lab-r")
concrete <- read.csv(file = "mlwr/concrete.csv")

# 2. 데이터 확인, 전처리
str(concrete)
summary(concrete)

# 정규화(Normalization) : 실제값 -> 0 ~ 1
# 표준화(Standardization) : z-score 표준화 (평균, 표준편차)

normalization <- function(x) { return ((x - min(x))/(max(x)-min(x)))}
normalization

l <- lapply(concrete, normalization)
l$strength

concrete_norm <- as.data.frame(lapply(concrete,normalization))
summary(concrete_norm)

# 신경망 알고리즘 적용하기 위한 패키지: neuralnet
# 오차 역전파(backpropagation)를 사용해서 신경망을 훈련시키는 알고리즘
install.packages("neuralnet")
library(neuralnet)
search()

# 3. 모델 생성, 학습
# 학습 데이터 세트(75%) / 테스트 데이터 세트(25%)
concrete_train <- concrete_norm [1:773,]
concrete_test <- concrete_norm [774:1030,]

summary(concrete_train$strength)
summary(concrete_test$strength)
1319*0.75
1319*0.25

# 신경망 모델 생성
set.seed(12345) 
concrete_model <- neuralnet(formula = strength ~ . , data = concrete_train)

# 생성된 NN을 확인
plot(concrete_model)
  
# 4.만들어진 NN을 평가 - 테스트 데이터 세트에 적용
model_result <- compute(concrete_model, concrete_test[-9])
head(model_result) # 신경망 모델에 의해서 계산 된 strength 예측값
summary(model_result)

predict_result <- model_result$net.result
# 예측 결과와 실제 값의 상관 관계 - 상관 계수
cor(predict_result, concrete_test$strength) # 0.8064

concrete_test[255:257,9]


# 5. 모델 향상
model2 <- neuralnet(formula = strength ~ . , data = concrete_train, hidden = 2)
plot(model2)

model5 <- neuralnet(formula = strength ~ . , data = concrete_train, hidden = 5)
plot(model5)


# 각 모델(model2,model5)에서 예측 결과와 실제 strength간의 상관 계수를 계산
model2_result <- compute(model2, concrete_test[-9])
model2_result
model2_predict_result <- model2_result$net.result
cor(model2_predict_result, concrete_test$strength) # 0.9024

model5_result <- compute(model5, concrete_test[-9])
model5_result
model5_predict_result <- model5_result$net.result
cor(model5_predict_result, concrete_test$strength) # 0.9292


# 평균 절대 오차 함수 작성
# -> 각 모델의 MAE를 계산
MAE <- function(actual, predict) {return(mean(abs(actual-predict)))}
MAE(concrete_test$strength, predict_result)

MAE(concrete_test$strength, predict_result)           #0.093
MAE(concrete_test$strength, model2_predict_result)    #0.066
MAE(concrete_test$strength, model5_predict_result)    #0.055




# 역정규화(정규화 -> 실제값) 함수 작성
# -> 실제 데이터 프레임(concrete)의 값들과 비교
# normalization = (x-min) / (max-min)
# x = normalization * (max - min) + min
denormalize <- function(x) {return (x * (max(concrete$strength)-min(concrete$strength))+min(concrete$strength))}
denormalize

predict1_denorm <- denormalize(predict_result)
summary(predict1_denorm)
predict2_denorm <- denormalize(model2_predict_result)
summary(predict2_denorm)
predict5_denorm <- denormalize(model5_predict_result)
summary(predict5_denorm)

actual_predict_df <- data.frame(actual = concrete[774:1030,9], predict1 = predict1_denorm, predict2 = predict2_denorm, predict5 = predict5_denorm)
head(actual_predict_df, n =10)

cor(actual_predict_df$actual,actual_predict_df$predict1) # 0.8064
cor(actual_predict_df$actual,actual_predict_df$predict2) # 0.9024
cor(actual_predict_df$actual,actual_predict_df$predict5) # 0.9292
# 정규화된 값들의 상관 계수 = 정규화되지 않은 값들의 상관 계수


# neuralnet 함수의 파라미터 중에서
# hidden 파라미터는 은닉 노드와 은닉 계층의 갯수를 조정할 수 있고,
# act.fct (활성함수) 파라미터는 활성 함수를 바꿔줄 수 있습니다.
# 두 개의 파라미터를 활용해서 다른 신경망 모델을 만들어 보고,
# 예측 결과가 얼마나 개선 되는 지 확인해 보세요.

set.seed(12345)
softsum <- function(x) {log(1+exp(x))}
curve(expr=softsum, from = -5, to = 5)
model53 <- neuralnet(formula = strength ~ . , data = concrete_train, hidden = c(5,3,2), linear.output = T, act.fct = softsum)
plot(model53)


rm(model53)




  


