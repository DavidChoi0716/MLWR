rm(list = ls())

# Naive Bayse를 이용한 스팸 SMS 분류
# 데이터 준비
sms_raw <- read.csv(file = "mlwr/sms_spam.csv", stringsAsFactors = F, encoding = "UTF-8")
str(sms_raw)
head(sms_raw)

# type 변수(특징)를 팩터로
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
prop.table(table(sms_raw$type))


# Text Mining: SMS 메시지에 포함된 단어들을 분석
install.packages("tm")
library(tm)
search()


# SMS 메시지들의 전집(corpus)을 생성
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# corpus 확인
print(sms_corpus)
sms_corpus[[1]]
sms_corpus[[1]]$content
sms_corpus[[1]]$meta
sms_corpus[[1072]]$content
sms_corpus[[1072]]$meta


# 모든 메시지를 소문자로 변환
# corpus 객체의 content만 소문자로 변환
# 변환 함수가 tm 패키지에 없는 함수인 경우에는
# tm_map(corpus, content_transformer(함수)) 형식으로 호출
# 변환함수가 tm 패키지에 있는 경우에는
# tm_map(corpus,함수) 형식으로 호출
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean[[1]]$content
sms_corpus_clean[[1072]]$content
sms_corpus_clean[[1]]$meta

# 숫자들을 제거 
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean[[4]]$content
  
# stopwords(a, an, the, to, ...)들을 제거
stopwords
stopwords()
stopwords("german")
stopwords("french")
stopwords("SMART")
removeWords(sms_corpus_clean[[1]]$content,stopwords())
removeWords(sms_corpus_clean[[2]]$content,stopwords())
removeWords(sms_corpus_clean[[3]]$content,stopwords())

sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean[[1]]$content
sms_corpus_clean[[1072]]$content

# punctuation(구두점, 문장부호) 제거
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean[[1072]]$content


# 단어나 구두점 등을 제거하면서 생긴 추가적인 공백들을 제거
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
sms_corpus_clean[[1072]]$content

# 형태소 분석 (어근/어미 분리해서 어근만 return)
install.packages("SnowballC")
library(SnowballC)
search()
wordStem(c("learn","learning","learned","learner","learns"))
wordStem(c("play","plays","playing","played"))

sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean[[1072]]$content

# DTM(Document-Term Matrix): 문서-단어 행렬 
# 행(row): Document - 문자 메시지, 이메일
# 열(column): 모든 문서에서 추출한 단어들

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
str(sms_dtm)

# DTM를 사용해서 학습 데이터 세트(75%)와 테스트 데이터 세트(25%)로 나눔
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4710:5559,]

# 학습 데이터 레이블, 테스트 데이터 레이블(Spam/Ham)
sms_train_label <- sms_raw[1:4169,1]
sms_test_label <- sms_raw[4170:5559,1]
table(sms_train_label)
table(sms_test_label)

# DTM은 희소행렬(sparse matrix): 행렬의 대부분의 원소들의 값이 0이고, 
# 0이 아닌 값을 갖는 원소들은 매우 희소한(적은) 행렬
# DTM에서 자주 등장하는 단어(Term)들만 선택해보자.
freq_terms <- findFreqTerms(sms_dtm_train, lowfreq = 5)

sms_dtm_freq_train <- sms_dtm_train[, freq_terms]
sms_dtm_freq_test <- sms_dtm_test[, freq_terms]


str(sms_dtm_train)
str(sms_dtm_freq_train)
str(sms_dtm_test)
str(sms_dtm_freq_test)

# 나이브 베이즈 알고리즘 함수는 명목형 변수들만 처리할 수 있음.
# DTM에서 각 원소의 값이 0보다 크면 "Y", 그렇지 않으면 "N"
# 변환 함수 
convert_content <- function(x) { 
  x <- ifelse(x>0,"Y","N") 
  return(x) 
}

x <- c(1,2,0,3)
convert_content(x)

sms_train <- apply(X = sms_dtm_freq_train, MARGIN = 2, FUN = convert_content)
sms_test <- apply(X = sms_dtm_freq_test, MARGIN = 2, FUN = convert_content)

# MARGIN = 1 : FUN을 호출할 때 X의 행(row)을 파라미터로 전달
# MARGIN = 2 : FUN을 호출할 때 X의 열(column)을 파라미터로 전달 


# 나이브 베이즈 분류 알고리즘을 구현한 패키지를 설치
install.packages("e1071")
library(e1071)
search()


# 학습 데이터 세트를 가지고 분류기(classifier)를 생성
sms_classifier <- naiveBayes(sms_train, sms_train_label)
# 분류기를 사용해서 테스트 데이터의 분류 결과를 예측
sms_pred <- predict(sms_classifier, sms_test)
sms_pred
# sms_test[841,]

library(gmodels)
CrossTable( x = sms_test_label, y = sms_pred, prop.chisq = F)




  
