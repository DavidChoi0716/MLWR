rm(list =  ls() )

# Iris Species 데이터 준비
getwd()
setwd("C:/dev/lab-r")

# CSV 파일에서 데이터 프레임 생성
is <- read.csv( file = "data/iris.csv", stringsAsFactors = F)
is

#데이터 구조 확인
str(is)
head(is)
tail(is, 3)

# iris 품종 분류와는 상관 없는 변수(특징)인 ID를 제거
is <- is[-1]
str(is)

# Species 컬럼을 팩터로 만듦 - 레이블
is$Species <- factor(is$Species, levels = c("Iris-setosa","Iris-versicolor","Iris-virginica"), labels = c("Setosa","Versicolor","Veginica"))
str(is$Species)
table(is$Species)

# 학습 데이터 세트, 테스트 데이터 세트를 준비 
# 품종별로 구분이 되어 있는 데이터를 랜덤하게 섞은 후 데이터 세트를 나눔
v <- c(1:10)
v

# sample(벡터): 벡터의 원소들을 랜덤하게 모두 추출
sample(v)
# sample(벡터, n): 벡터의 원소들 중에서 n개의 원소를 랜덤하게 추출
sample(v,7)
# sample(n) : 1~n까지 n개의 정수를 랜덤하게 추출
sample(5)
sample(150)

# nrow(데이터프레임), ncol(데이터프레임): 데이터프레임의 행/열의 갯수
nrow(is)
is_shuffled <- is[sample(nrow(is)),]
head(is_shuffled)
tail(is_shuffled)
table(is_shuffled$Species)

#학습 데이터
train_set <- is_shuffled[1:100, 1:4]
head(train_set)

#학습 데이터 레이블
train_label <- is_shuffled[1:100,5]
head(train_label)

#테스트 데이터
test_set <- is_shuffled[101:150, 1:4]
head(test_set)

#테스트 데이터 레이블
test_label <- is_shuffled[101:150,5]
head(test_label)

prop.table(table(train_label))
prop.table(table(test_label))

# 최소-최대 정규화 함수 정의
normalize <- function(x){return((x-min(x)/(max(x)-min(x))))}

train_set <- as.data.frame(lapply(train_set, normalize))
summary(train_set)

test_set <- as.data.frame(lapply(test_set, normalize))
summary(test_set)

# Knn 함수가 있는 패키지
library(class)
# CrossTable 함수가 있는 패키지
library(gmodels)
search()


# knn을 적용했을 때 예측 값
predict <- knn (train = train_set, test = test_set
                , cl = train_label, k = 11)

CrossTable(x = test_label, y = predict, prop.chisq = F)


# 학습 데이터와 테스트 데이터를 나누기 전에 정규화를 먼저 하고
# 학습 데이터와 테스트 데이터로 나눠서 knn알고리즘 적용해 보세요


