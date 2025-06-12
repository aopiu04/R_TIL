#### 라이브러리 호출 ####

set.seed(1234)


library(pacman)

p_load("tidyverse","caret","randomForest","nnet","e1071","rpart",
         "rpart.plot","reshape2","rsample","smotefamily","mice",
         "glmnet","randtests","tseries","arules","MASS","adabag",
         "factoextra","dbscan","mclust","kohonen","car","ROSE","gbm",
         "klaR","class","FNN","neuralnet","fpc","pROC","xgboost","forecast","coxme","UBL","naivebayes","RColorBrewer")


#### 250428(월) #### skimr 패키지 확인 ####

# 1. 기본 데이터프레임 생성
data_raw <- data.frame(
  id = 1:1000,
  gender = sample(c("M", "F", NA, ""), 1000, replace = TRUE, prob = c(0.45, 0.45, 0.05, 0.05)),
  age = sample(c(18:70, NA), 1000, replace = TRUE),
  income = sample(c(seq(20000, 100000, by = 5000), NA), 1000, replace = TRUE),
  signup_date = sample(seq(as.Date('2018-01-01'), as.Date('2023-12-31'), by = "day"), 1000, replace = TRUE),
  last_login = sample(c(seq(as.Date('2019-01-01'), as.Date('2024-04-01'), by = "day"), NA), 1000, replace = TRUE),
  product_A = sample(c(0:10, NA), 1000, replace = TRUE),
  product_B = sample(c(0:5, ""), 1000, replace = TRUE),
  product_C = sample(c(0:3, NA), 1000, replace = TRUE),
  customer_type = sample(c("VIP", "Regular", "New", NA, ""), 1000, replace = TRUE),
  region = sample(c("North", "South", "East", "West", NA), 1000, replace = TRUE),
  marital_status = sample(c("Single", "Married", "Divorced", NA, ""), 1000, replace = TRUE),
  satisfaction_score = sample(c(1:5, NA), 1000, replace = TRUE),
  feedback = sample(c("Good", "Average", "Poor", NA, ""), 1000, replace = TRUE),
  has_coupon = sample(c("Yes", "No", "", NA), 1000, replace = TRUE),
  coupon_used = sample(c(0, 1, NA), 1000, replace = TRUE),
  subscription_type = sample(c("Monthly", "Yearly", NA, ""), 1000, replace = TRUE),
  referred_by = sample(c("Friend", "Ad", "SocialMedia", NA, ""), 1000, replace = TRUE),
  visit_frequency = sample(c(1:10, NA), 1000, replace = TRUE),
  active_status = sample(c("Active", "Inactive", "", NA), 1000, replace = TRUE),
  y = sample(c("Class1", "Class2", "Class3"), 1000, replace = TRUE)
)

#### (250503 / 토) ####

# 🔍 문제 1. 고급 결측값 처리 및 정제
# data_raw에서 다음 조건에 따라 데이터를 정제하시오.
# 
# gender, customer_type, has_coupon, active_status가 NA 또는 빈 문자열 ""인 경우 "Unknown"으로 대체하라.
# 
# 단, referred_by는 결측치로 유지할 것.
# 
# product_B가 빈 문자열인 경우 NA로 변환하되, 그 외의 변수는 공백을 건드리지 않는다.
# 
# 이후 결측치가 있는 행의 수를 구하고, 전체 행 수 대비 결측치가 있는 행의 비율(%)을 소수점 둘째자리까지 출력하라.
# 
# 이 작업 후 생성된 정제된 데이터셋은 data_clean이라고 하자.
# ➡ 다음 문제부터는 data_clean을 기준으로 출제됨
           
summary(data)

# gender / customer_type / has_coupon / active_status


summary(data_raw)

data_clean <- data_raw %>%
  mutate(gender = ifelse(is.na(gender)| gender == "", "Unknown",gender)) %>%
  mutate(customer_type = ifelse(is.na(customer_type)| customer_type == "", "Unknown",customer_type)) %>%
  mutate(has_coupon = ifelse(is.na(has_coupon)| has_coupon == "", "Unknown",has_coupon)) %>%
  mutate(active_status = ifelse(is.na(active_status)| active_status == "", "Unknown",active_status)) %>%
  mutate(product_B = ifelse(product_B == "", NA,product_B))
  

# 이후 결측치가 있는 행의 수를 구하고, 전체 행 수 대비 결측치가 있는 행의 비율(%)을 소수점 둘째자리까지 출력하라.
# 
# 이 작업 후 생성된 정제된 데이터셋은 data_clean이라고 하자.
# ➡ 다음 문제부터는 data_clean을 기준으로 출제됨

na <- as.data.frame(is.na(data_clean))


na_sum <- na$id + na$gender + na$age+na$income+na$signup_date+na$last_login +na$product_A +na$product_B +na$product_C +na$customer_type +na$region +na$marital_status +na$satisfaction_score +na$feedback +na$has_coupon +na$coupon_used+na$subscription_type+ na$referred_by+ na$visit_frequency+ na$active_status+ na$y

# 결측치가 있는 행의 비율은 91%임
round(sum(ifelse(na_sum >= 1, 1, 0)) / 1000,2)

colSums(na)
rowSums(na)

# 문제 1-1. across() & case_when() 연습

data_test <- data_clean %>%
  mutate(
    across(c(gender,customer_type,has_coupon),
           ~case_when(
             . %in% c("M","VIP","Yes") ~ "Group1",
             . %in% c("F","Regular","No") ~ "Group2",
             T ~ "Other"
           ))
  )

data_clean_2 <- data_clean
data_clean_2$gender_new <-  data_test$gender
data_clean_2$customer_type_new <-  data_test$customer_type
data_clean_2$has_coupon_new <-  data_test$has_coupon


# 문제 1-1. 최적화 코드

# .names = "{.col}_new : 새로운 컬럼명을 자동으로 만들어주는 across()의 옵션

data_clean_2 <- data_clean %>%
  mutate(
    across(c(gender, customer_type, has_coupon),
           ~ case_when(
             . %in% c("M", "VIP", "Yes") ~ "Group1",
             . %in% c("F", "Regular", "No") ~ "Group2",
             TRUE ~ "Other"
           ),
           .names = "{.col}_new")
  )

# 🧪 문제 2. 복합 조건 파생 변수 생성
# data_clean을 기반으로 다음 조건을 만족하는 파생변수 user_score를 생성하라.
# 
# 조건:
#   
# satisfaction_score가 4 이상이면 +2점
# 
# coupon_used가 1이면 +2점
# 
# customer_type이 "VIP"이면 +3점, "Regular"이면 +1점, 그 외는 0점
# 
# visit_frequency가 5 이상이면 +1점, 10이면 +2점
# 
# product_A + product_B + product_C의 합이 10 이상이면 +2점
# 
# 이 점수의 총합을 user_score라는 새 변수로 만들고, user_score가 높은 상위 5명(id)을 추출하라.


score_matrix <- data_clean %>%
  dplyr::select(c(satisfaction_score,coupon_used,customer_type,visit_frequency,product_A,product_B,product_C)) %>%
  mutate(product_B = as.numeric(product_B)) %>%
  mutate(product_sum = sum(product_A + product_B + product_C, na.rm = T)) %>%
  mutate(
    satisfaction_score = ifelse(satisfaction_score >= 4, 2,0),
    coupon_used = ifelse(coupon_used == 1, 2, 0),
    customer_type = case_when(
      customer_type == "VIP" ~ 3,
      customer_type == "Regular" ~ 1,
      T ~ 0
    ),
    visit_frequency = case_when(
      visit_frequency >= 10 ~ 2,
      visit_frequency >= 5 ~ 1,
      T ~ 0
    ),
    product_sum = ifelse(product_sum >= 10, 2, 0)
  )

score_matrix

data_clean$user_score <- rowSums(score_matrix, na.rm = T)

data_clean %>%
  arrange(user_score, desc = T) %>%
  dplyr::select(id) %>%
  head(5)

# 2-1 개선코드

score_matrix <- data_clean %>%
  dplyr::select(c(satisfaction_score,coupon_used,customer_type,visit_frequency,product_A,product_B,product_C)) %>%
  mutate(product_B = as.numeric(product_B)) %>%
  mutate(product_sum = rowSums(across(c(product_A,product_B,product_C)), na.rm = T)) %>%
  mutate(
    satisfaction_score = ifelse(satisfaction_score >= 4, 2,0),
    coupon_used = ifelse(coupon_used == 1, 2, 0),
    customer_type = case_when(
      customer_type == "VIP" ~ 3,
      customer_type == "Regular" ~ 1,
      T ~ 0
    ),
    visit_frequency = case_when(
      visit_frequency >= 10 ~ 2,
      visit_frequency >= 5 ~ 1,
      T ~ 0
    ),
    product_sum = ifelse(product_sum >= 10, 2, 0)
  ) %>%
  dplyr::select(satisfaction_score,coupon_used,customer_type,visit_frequency,product_sum)

score_matrix

data_clean$user_score <- rowSums(score_matrix, na.rm = T)

data_clean %>%
  arrange(desc(user_score)) %>%
  dplyr::select(id) %>%
  head(5)

data_clean


# 🧮 문제 3. 날짜 기반 파생 변수 및 필터링
# data_clean에서 다음을 수행하라.
# 
# signup_date와 last_login의 차이를 일 단위로 계산한 후 days_since_signup 변수로 저장하라.
# 
# 이후, days_since_signup이 음수이거나 결측치인 행은 제거하라.
# 
# 가입 후 첫 로그인까지 30일 이상 걸린 고객 수를 구하라.

data_clean <- data_clean %>%
  mutate(days_since_signup = data_clean$last_login - data_clean$signup_date) %>%
  drop_na(days_since_signup) %>%
  dplyr::filter(days_since_signup >=0)

# 610명
nrow(data_clean)

# 3-1 개선코드

# signup_date - last_login 순서가 반대
# 가입일은 과거 로그인일은 최근으로 순서를 반대로 써야됨

# mutate(... = na.omit(...))는 잘못된 사용
# na.omit()은 데이터 프레임 전체에서 NA가 있는 행을 제거하는 함수로, 벡터형식으로 쓰면 길이가 짧아져서 mutate 내에서 작동 불가능


data_clean <- data_clean %>%
  mutate(
    days_since_signup = as.numeric(difftime(last_login, signup_date, units = "days"))
  ) %>%
  filter(!is.na(days_since_signup) & days_since_signup >= 30)

# 가입 후 30일 이상 걸린 고객 수
nrow(data_clean)


# 📊 문제 4. 군집화를 위한 데이터 전처리
# data_clean에서 다음 조건에 따라 군집화용 데이터를 전처리하라.
# 
# 군집화에 사용할 변수는 다음과 같다: age, income, visit_frequency, user_score
# 
# 이들 변수 중 하나라도 결측치가 있으면 해당 행을 제거한다.
# 
# 각 변수를 0~1 범위로 정규화(min-max scaling)하라.
# 
# 정규화된 데이터를 data_cluster라는 이름으로 저장하고, 전체 평균 벡터(4개의 변수의 평균)를 출력하라.

# 정규화 함수 정의
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

data_cluster <- data_clean %>%
  na.omit(age, income, visit_frequency, user_score) %>%
  mutate(
    across(c("age","income","visit_frequency","user_score"),
           ~ normalize(.))
  )



data_cluster %>%
  summarize(
    across(c("age","income","visit_frequency","user_score"),
           ~ mean(.)
  )
)

# 4-1 개선코드
# na.omit()은 데이터 프레임 전체에 대해 NA가 있는 행을 제거하는 함수지, 특정 열만 지정해서 제거할 수는 없음

data_cluster <- data_clean %>%
  drop_na(age, income, visit_frequency, user_score) %>%
  mutate(across(c(age,income, visit_frequency, user_score), normalize))

data_cluster %>%
  summarize(across(c(age,income,visit_frequency, user_score), mean))


# 🧠 문제 5. 모델링을 위한 데이터 분할과 클래스 비율 유지
# data_cluster에 원래 데이터의 타깃 변수 y를 조인하고 다음을 수행하라.
# 
# caret::createDataPartition()을 사용해 y의 클래스 비율을 유지한 상태로 70% 훈련 데이터, 30% 테스트 데이터로 나눈다.
# 
# 훈련 데이터와 테스트 데이터의 클래스별(y) 분포를 표로 출력하라.
# 
# user_score의 평균이 훈련과 테스트 셋 간에 유의하게 차이가 있는지를 t.test()로 검정하라.

data_cluster <- data_cluster %>%
  dplyr::select(age, income, visit_frequency, user_score,y)

train_index <- createDataPartition(data_cluster$y, p = 0.7, list = F)
train_data <- data_cluster[train_index,]
test_data <- data_cluster[-train_index,]

table(train_data$y)
table(test_data$y)

# 독핍표본 t검정 실시

# 두 표본다 표본크기는 30이하로 정규성 검정 필요
shapiro.test(train_data$user_score)
shapiro.test(test_data$user_score)

# H0: 정규분포 따름
# H1: 정규분포를 따르지 않음
# train_data의 경우 정규분포를 따르지 않지만(유의수준 0.05 기준) 지문에서 t.test를 진행하라고 해서 t.test실시

# 등분산 검정
var.test(train_data$user_score, test_data$user_score)

# H0: 등분산
# H1: 이분산

t.test(train_data$user_score, test_data$user_score, var.equal = T)

# H0: 집단간 차이가 없다
# H1: 집단간 차이가 있다.

# p-value 0.1384 > 0.05 로 귀무가설 채택 집단간 차이가 없는 것으로 보아서 잘 분할 된 부분을 알 수 있다.



#### 250505/월 ####

# 메인 유저 데이터
data_complex <- data.frame(
  user_id = 1:1000,
  gender = sample(c("M", "F", "Male", "Female", NA, ""), 1000, replace = TRUE, prob = c(0.3, 0.3, 0.1, 0.1, 0.1, 0.1)),
  age = sample(c(18:75, NA), 1000, replace = TRUE),
  income = sample(c(seq(15000, 120000, by = 5000), NA), 1000, replace = TRUE),
  signup_time = sample(seq(as.POSIXct('2019-01-01 00:00:00'), as.POSIXct('2023-12-31 23:59:59'), by = "hour"), 1000, replace = TRUE),
  last_active_time = sample(c(seq(as.POSIXct('2019-06-01 00:00:00'), as.POSIXct('2024-04-01 23:59:59'), by = "hour"), NA), 1000, replace = TRUE),
  feedback_comment = sample(c("Great service!", "bad experience", "Okay", "Will return", "Bad!!", "Excellent", "", NA), 1000, replace = TRUE),
  preferred_contact = sample(c("Email", "SMS", "None", "email", "sms", NA, ""), 1000, replace = TRUE),
  purchases = sample(c(0:15, NA), 1000, replace = TRUE),
  subscription = sample(c("Monthly", "Yearly", "Trial", "Canceled", "", NA), 1000, replace = TRUE),
  referred_code = sample(c("REF123", "ad001", "FRIEND", "", NA), 1000, replace = TRUE),
  tier = sample(c("Bronze", "Silver", "Gold", "Platinum", NA, ""), 1000, replace = TRUE),
  y = sample(c("ClassA", "ClassB", "ClassC"), 1000, replace = TRUE)
)

# 조인을 위한 외부 테이블 user_region_info
user_region_info <- data.frame(
  user_id = sample(1:1200, 1100, replace = FALSE),  # 일부 사용자 누락 / 초과
  region_code = sample(c("N", "S", "E", "W", NA), 1100, replace = TRUE),
  city = sample(c("Tokyo", "Osaka", "Seoul", "Busan", "Beijing", NA), 1100, replace = TRUE)
)

# 🔧 문제 1. 결측치 판단 및 처리 전략 수립
# data_complex에서 다음 조건을 바탕으로 결측치 처리 방안을 제시하고 수행하시오.
# 
# income, age는 각각 결측률이 10% 이상이면 제거하지 않고 그룹별 평균으로 대체할 것.
# 그룹은 tier (Bronze/Silver/Gold/Platinum) 기준.
# 
# gender, preferred_contact, subscription은 결측/공백을 "Unknown"으로 통일하라.
# 
# feedback_comment, referred_code, tier는 결측치로 남겨둔다 (분석 후 판단).
# 
# 📌 이 문제는 실제 결측률 확인 + 조건 판단 후 처리해야 합니다.  

summary(data_complex)
summary(user_region_info)  

# 결측률 확인
sum(is.na(data_complex$income)) / nrow(data_complex) # 37개
sum(is.na(data_complex$age)) / nrow(data_complex) # 16개

# income, age는 각각 결측률이 10% 이하이므로 제거함

data_complex <- data_complex %>%
  drop_na(income,age)

# 결측률이 10% 이상일 경우를 가정해서 테스트 -> 실제 코드로는 활용 x
# summarize()는 group_by()된 데이터에서 요약 통계만 남기고 나머지 열을 제거 -> mutate로 사용

data_complex %>%
  group_by(tier) %>%
  mutate(
    income_mean = mean(income, na.rm = T),
    age_mean = mean(age, na.rm = T)
  ) %>%
  mutate(
    income = ifelse(is.na(income), income_mean, income),
    age = ifelse(is.na(age), age_mean, age)
  ) %>%
  ungroup()

# gender, preferred_contact, subscription은 결측/공백을 "Unknown"으로 통일하라.
data_complex <- data_complex %>%
  mutate(
    across(
      c(gender, preferred_contact, subscription),
      ~case_when(
        is.na(.) ~ "Unknown",
        . == "" ~ "Unknown",
        T ~ .
      )
    )
  )
      
# feedback_comment, referred_code, tier는 결측치로 남겨둔다 (분석 후 판단).


table(data_complex$feedback_comment) # 피드백 코멘트는 정량적인 수치가 아니고 사람 개인의 판단으로 대체하기는 어려움으로 결측치 제거
table(data_complex$referred_code) # 레퍼럴 코드의 경우도 해당 코드에 대한 도메인 지식이 없고, 별도 대체는 어렵기 때문에 제거하는 것이 바람직해 보임
table(data_complex$tier) # tier는 구매횟수, 나이등에 비례해서 측정되므로 대체처리가 가능할 것 같다.

# tier정도만 대체가 가능할 것 같고 나머지는 제거해야 될 것 같은데 선지에 일단 유지하라고 했으니까 유지한 데이터 셋으로 데이터를 작성함

# ⏰ 문제 2. 날짜 및 시간 처리 (초 단위 포함)
# signup_time, last_active_time에서 **시간 차이(초 단위)**를 계산해 seconds_active 파생 변수를 만들 것.
# 
# 단, seconds_active가 음수이거나 결측치인 경우는 제거할 것.
# 
# signup_time을 기준으로 가입 요일, 가입 시간대(오전/오후/야간) 파생 변수도 생성하라.
# 


data_complex <- data_complex %>%
  mutate(seconds_active = as.numeric(difftime(last_active_time,signup_time, units = "secs"))) %>%
  dplyr::filter(seconds_active >= 0)

# signup_time을 기준으로 가입 요일, 가입 시간대(오전/오후/야간) 파생 변수

data_complex <- data_complex %>%
  mutate(
    DAY = format(signup_time, "%a"),
    TIME = case_when(
      hour(signup_time) >= 5 & hour(signup_time) < 12 ~ "morning",
      hour(signup_time) >=12 & hour(signup_time) < 18 ~ "afternoon",
      T ~ "night"
    )
  )

# 🔗 문제 3. Join 후 결합 판단 및 누락 확인
# data_complex와 user_region_info를 활용하여 다음을 수행하시오.
# 
# 🎯 1. left_join 수행
# 두 데이터프레임을 user_id 기준으로 left_join하시오.
# 
# 🎯 2. 누락된 지역 코드 수 확인
# 조인 후 생성된 데이터에서 region_code가 NA인 행은 총 몇 개인가?
#   
#   🎯 3. 도시별 분포 및 필터링
# 조인된 데이터에서 city별 고객 수를 빈도 기준으로 집계하라.
# 
# 이 중 상위 3개 도시만 필터링하여 새로운 데이터셋 data_top_city를 생성하라.

# left_join 수행
join_data <- left_join(data_complex, user_region_info, by = "user_id")

# 누락된 지역 코드 수: 140개
sum(is.na(join_data$region_code))

join_data %>%
  group_by(city) %>%
  count()

# Q. data_top_city에 city를 베이징, 부산, 오사카만 필터로 골랐는데 왜 NA값이 같이 들어오는거야?
# A. 이유 설명: %in%은 NA를 자동으로 제외하지 않습니다
data_top_city <- join_data %>%
  dplyr::filter(city %in% c("Beijing","Busan","Osaka"))

# 개선 !is.na(city) 추가

data_top_city <- join_data %>%
  dplyr::filter(city %in% c("Beijjing","Busan","Osaka") & !is.na(city))



# 🔤 문제 4. 문자열 전처리 및 감성 분석 변수 생성
# feedback_comment에서 다음 규칙으로 feedback_sentiment 파생 변수를 만들어라:
#   
#   "bad", "Bad!!", "poor" 등이 포함되면 "Negative"
# 
# "great", "excellent", "good", "will return" 등이 포함되면 "Positive"
# 
# 나머지는 "Neutral"
# (대소문자 무시, 정규표현식 사용 가능)
# 
# 공백 또는 결측치는 "Unknown"으로 처리

data_complex %>%
  mutate(feedback_comment = tolower(feedback_comment)) %>%
  mutate(
    feedback_sentiment =
      case_when(
        feedback_comment %in% c("great","excellent","good","will return") ~ "Positive",
        feedback_comment %in% c("bad","poor","terrible") ~ "Negative",
        is.na(feedback_comment) | feedback_comment == "" ~ "Unknown",
        T ~ "Neutral"
      )
  )

# 정답 코드

# grepl("키워드", 변수이름, ignore.case = T) # 대소문자 무시


data_complex <- data_complex %>%
  mutate(
    feedback_sentiment =
      case_when(
        is.na(feedback_comment) | feedback_comment == "" ~ "Unknown",
        grepl("great|excellent|good|will return", feedback_comment, ignore.case = T) ~ "Positive",
        grepl("bad|poor|terrible", feedback_comment, ignore.case = T) ~ "Negative",
        T ~ "Neutral"
      )
  )


table(data_complex$feedback_comment)

# 📊 문제 5. 전처리 완료 데이터 기반 파생지표 생성 및 이상치 탐지
# income, purchases가 모두 존재하는 행을 기준으로,
# 
# income_per_purchase = income / purchases 파생 변수를 만들 것.
# 
# 단, purchases == 0 또는 NA인 경우는 계산 제외
# 
# income_per_purchase의 상위 1% 이상치를 별도 변수(is_outlier)로 플래그 처리하라.
# 
# 최종 데이터셋에서 is_outlier == TRUE인 고객 수를 출력하라.

# 오답
data_complex %>%
  dplyr::filter(!is.na(income) & !is.na(purchases) & purchases != 0) %>%
  mutate(income_per_purchase = income / purchases) %>%
  mutate(is_outlier = quantile(income_per_purchase, 0.99)) %>%
  mutate(is_outlier = case_when(
    income_per_purchase >= 115400 ~ TRUE,
    T ~ F
  )) %>%
  dplyr::filter(is_outlier == T)

# 정답 코드

a <- data_complex %>%
  dplyr::filter(!is.na(income) & !is.na(purchases) & purchases != 0) %>%
  mutate(income_per_purchase = income / purchases)

b <- quantile(a$income_per_purchase, 0.99)


data_complex %>%
  dplyr::filter(!is.na(income) & !is.na(purchases) & purchases != 0) %>%
  mutate(income_per_purchase = income / purchases) %>%
  mutate(is_outlier = case_when(
    income_per_purchase >= b ~ TRUE,
    T ~ F
  )) %>%
  dplyr::filter(is_outlier == T)


#### 250507/수 ####

data1 <- read.csv("C:/Users/jaewon/Desktop/자격증/1. ADP/complex_user_data.csv")

# 🔧 문제 1. 다중 결측 처리 및 범주 정리
# 다음 조건에 따라 complex_user_data를 전처리하라. 이 결과를 user_cleaned로 저장하라.
# 
# 🎯 [A] 결측 처리 조건
# gender, marketing_opt_in, tier, device_type, browser, subscription_type, active_status는
# NA 또는 "" 값을 "unknown"으로 통일하라.

data1$marketing_opt_in

data1_1 <- data1 %>%
  mutate(
    across(c("gender", "marketing_opt_in", "tier", "device_type", "browser", "subscription_type", "active_status"),~
             case_when(
               is.na(.) | . == "" ~ "unknown",
               TRUE ~ .
             )))

# country 결측률이 10% 이상이면 "UNK"로 대체, 10% 미만이면 해당 행 제거하라.
# referrer가 NA 또는 공백일 경우 "direct"로 대체하라.


summary(data1_1$country)
sum(is.na(data1_1$country)) / nrow(data1_1)
table(data1_1$country) # NA값은 없으나 공백이 있어서 해당값은 결측임

# 결측률이 10% 이상이므로 공백은 UNK로 대체 필요
118/1000

data1_2 <- data1_1 %>%
  mutate(country =
           case_when(
             country == "" ~ "UNK",
             TRUE ~ country
           )
  )

table(data1_2$country)

data1_3 <- data1_2 %>%
  mutate(referrer =
           case_when(
             is.na(referrer) | referrer == "" ~ "direct",
             TRUE ~ referrer
           ))


# 🎯 [B] 삭제 조건
# 다음 조건을 동시에 만족하는 행은 제거하라:
#   
#   age가 NA
# 
# total_spent가 NA
# 
# purchases가 NA

data1_4 <- data1_3 %>%
  dplyr::filter(!is.na(age)|!is.na(total_spent)|!is.na(purchases))

data1_4 <- data1_3 %>%
  drop_na(age,total_spent,purchases)


# 🎯 [C] 문자열 소문자 통일
# marketing_opt_in, subscription_type, support_ticket 컬럼을 모두 소문자로 변환하라.
# 
# 🧠 이 문제는 이후 문제의 기반이 되며, user_cleaned 데이터셋이 다음 단계에서 반드시 사용됩니다.
# 
# 풀이 완료되면 코드와 함께 알려주세요! 다음 문제(시간 파생 + 유효성 필터링)로 이어가겠습니다 😊

data1_5 <- data1_4 %>%
  mutate(across(c("marketing_opt_in","subscription_type","support_ticket"),tolower))

user_cleaned <- data1_5

 
# ⏱ 문제 2. 시간 기반 파생 및 유효성 필터링 (user_cleaned 기준)
# 🎯 [A] 초 단위 시간차 계산
# signup_time, last_active_time 간 차이를 초 단위로 계산해 seconds_active 파생 변수로 추가하라.
# 단, last_active_time이 NA이거나 seconds_active가 음수이면 해당 행은 제거하라.

user_cleaned$signup_time <- ymd_hms(user_cleaned$signup_time)
user_cleaned$last_active_time <- ymd_hms(user_cleaned$last_active_time)

user_cleaned_2 <- user_cleaned %>%
  mutate(
    seconds_active = difftime(last_active_time, signup_time, units="secs") 
  ) %>%
  dplyr::filter(last_active_time >0)

# [A] 오류: last_active_time > 0 은 불가능한 비교
# ❌ last_active_time은 **POSIXct (날짜-시간 객체)**인데 숫자 0과 비교하면 의미 없는 조건입니다.
# 
# difftime() 결과인 seconds_active가 음수인지 확인해야 합니다.

# 정답처리방식(숫자형으로 바꿔서 비교)
user_cleaned_2 <- user_cleaned %>%
  mutate(
    seconds_active = as.numeric(difftime(last_active_time, signup_time, units = "secs"))
  ) %>%
  filter(!is.na(seconds_active) & seconds_active >= 0)


# 🎯 [B] 시간 파생 변수
# signup_time을 기준으로:
#   
#   signup_hour (시간 단위, 0~23)
# 
# signup_period (오전: 511시 / 오후: 1217시 / 야간: 그 외)

user_cleaned_2$signup_hour <- hour(user_cleaned_2$signup_time)

user_cleaned_3 <- user_cleaned_2 %>%
  mutate(
    signup_period = 
      case_when(
        signup_hour >= 5 & signup_hour <= 11 ~ "morning",
        signup_hour >= 12 & signup_hour <= 17 ~ "afternoon",
        TRUE ~ "night"
      )
  )

user_time_cleaned<- user_cleaned_3 

# 문제3

# [A] 가입 시간대별(signup_period) 고객 분포 확인
# signup_period별 고객 수를 집계하고, 전체 고객 중 각 그룹의 **비율 (%)**을 계산하라.
# → 결과는 **signup_period_summary**라는 데이터프레임에 저장할 것.

summary(user_time_cleaned)

signup_period_summary <- user_time_cleaned %>%
  group_by(signup_period) %>%
  summarize(count = n(),
            ratio = n()/nrow(user_time_cleaned))


# [B] 평균 session_length_sec 비교
# 각 signup_period별로 session_length_sec의 평균값을 계산하라.
# 
# 단, session_length_sec가 NA인 경우는 제외할 것.
# 
# 결과는 **session_summary**로 저장하고, signup_period별 평균을 내림차순 정렬하라.

session_summary <- user_time_cleaned %>%
  group_by(signup_period) %>%
  summarize(means = mean(session_length_sec, na.rm = T)) %>%
  arrange(desc(means))

#### 250511/일 ####

# 🔤 문제 4. 텍스트 감성 분석 기반 파생 변수 생성

# 🎯 [A] 감성 파생 변수 sentiment_label 생성
# feedback_comment에 포함된 키워드를 기준으로 다음과 같이 분류하라.
# (대소문자 구분 없이 처리할 것)

user_time_cleaned

user_time_cleaned$feedback_comment

table(user_time_cleaned$feedback_comment)

user_time_cleaned <- user_time_cleaned %>%
  mutate(feedback_comment = tolower(feedback_comment)) %>%
  mutate(sentiment_label = case_when(
    is.na(feedback_comment) | feedback_comment == "" ~ "missing",
    str_detect(feedback_comment, "excellent|love") ~ "positive",
    str_detect(feedback_comment, "bad|hate") ~ "negative",
    str_detect(feedback_comment, "okay|not bad") ~ "neutral",
    TRUE ~ "other"
  ))

# 🎯 [B] 시간대별 감성 분포 분석
# 각 signup_period별로 sentiment_label이 "positive", "negative", **"neutral"**인 비율을 각각 계산하라.
# 
# 단, sentiment_label이 "missing" 또는 "other"인 행은 분석 대상에서 제외한다.
# 
# 최종 결과는 다음 컬럼을 포함한 데이터프레임 **sentiment_by_period**로 저장하라.

sentiment_by_period <- user_time_cleaned %>%
  dplyr::filter(sentiment_label %in% c("positive","negative","neutral")) %>%
  group_by(signup_period) %>%
  mutate(positive_pct = sum(sentiment_label == "positive") / nrow(user_time_cleaned),
         negatvie_pct = sum(sentiment_label == "negative") / nrow(user_time_cleaned),
         neutral_pct = sum(sentiment_label == "neutral") / nrow(user_time_cleaned)) %>%
  ungroup()


## [B] mutate(... / nrow(user_time_cleaned))  # → 전체 데이터 수로 나눔 ❌
# ✅ 정답은 group_by()된 각 그룹에서 n()을 사용해야 합니다:

sentiment_by_period <- user_time_cleaned %>%
  filter(sentiment_label %in% c("positive", "negative", "neutral")) %>%
  group_by(signup_period) %>%
  summarize(
    positive_pct = sum(sentiment_label == "positive") / n() * 100,
    negative_pct = sum(sentiment_label == "negative") / n() * 100,
    neutral_pct  = sum(sentiment_label == "neutral") / n() * 100
  )

# → n()은 현재 그룹(signup_period) 내 관측치 수입니다.  

  
  
  
  
  
  
  
  
  





















































































#### 250608/일 ####
data1 <- read.csv("C:/Users/jaewon/Desktop/자격증/1. ADP/complex_user_data.csv")

# 🔧 문제 1. 다중 결측 처리 및 범주 정리
# complex_user_data를 기반으로 다음 조건에 따라 전처리를 수행하고, 결과를 **user_cleaned**라는 데이터프레임에 저장하시오.

# 다음 변수들의 결측치(NA) 또는 공백("") 값을 "unknown"으로 통일하시오:

# gender
# marketing_opt_in
# tier
# device_type
# browser
# subscription_type
# active_status

# [A] 결측 처리 조건

summary(data1)

# case_when()은 벡터 또는 열 단위로만 동작 -> case_when()은 mutate()안에서 각 변수에 대해 동작하도록 사용되서 오류남
# data1 %>%
#   select(c(gender,marketing_opt_in, tier, device_type, browser, subscription_type, active_status)) %>%
#   case_when(
#     is.na(.) | . == "" ~"unknown",
#     TRUE ~ .
#   )
  
data1_1 <- data1 %>%
  mutate(
    across(c(gender,marketing_opt_in, tier, device_type, browser, subscription_type, active_status),
           ~ case_when(
             is.na(.) | . == ""~ "unknown",
           ))
  )

data1 %>%
  dplyr::select(gender) %>%
  mutate(gender = ifelse(is.na(gender) | gender == "", "unknown", gender))

# [B] 삭제 조건
# 아래 조건을 모두 만족하는 행은 제거하시오
#   
# age가 NA
# 
# total_spent가 NA
# 
# purchases가 NA


data1_2 <- data1_1 %>%
  dplyr::filter(!is.na(age) & !is.na(total_spent) & !is.na(purchases))

# 🎯 [C] 문자열 소문자 통일
# 다음 변수는 모두 소문자로 변환하시오
#marketing_opt_in
# 
# subscription_type
#
# support_ticket

data1_3 <- data1_2 %>%
  mutate(
    across(
      c(marketing_opt_in,subscription_type,support_ticket),tolower
    )
  )

user_cleaned <- data1_3

# ⏱ 문제 2. 시간 기반 파생 변수 생성 및 유효성 필터링

# 🎯 [A] 초 단위 시간차 계산
# signup_time과 last_active_time 간의 차이를 초 단위로 계산하여
# seconds_active라는 파생 변수를 생성하시오.

# user_cleaned %>%
#   difftime(signup_time, last_active_time, units = c("secs"))

summary(user_cleaned)
 
user_cleaned$signup_time <- ymd_hms(user_cleaned$signup_time)
user_cleaned$last_active_time <- ymd_hms(user_cleaned$last_active_time)

str(user_cleaned)

# # difftime() 함수는 벡터 간 시간 차이를 계산하는 함수이지만, %>% 파이프와 함께 사용할 때는 조심해야 합니다. 현재 코드는 다음과 같은 구조로 되어 있죠:
# user_cleaned_2 <- user_cleaned %>%
#   difftime(signup_time, last_active_time, units = "secs")

# 하지만 이 방식은 user_cleaned 전체 데이터프레임을 difftime의 첫 번째 인자로 넣게 되어 작동하지 않습니다. difftime()은 두 개의 벡터(시간)만을 받아야 하거든요.

# ✅ 해결 방법
# mutate() 안에서 difftime()을 사용해야 합니다. 예를 들어, 다음과 같이 작성해야 해요:

user_cleaned_2 <- user_cleaned %>%
  mutate(
    seconds_active = as.numeric(difftime(last_active_time, signup_time, units = "secs"))
  ) %>%
  dplyr::filter(!is.na(last_active_time) & seconds_active >= 0)


# 단, 다음 조건을 만족하는 행만 남기시오:
# - last_active_time이 NA가 아니고
# - 계산된 seconds_active가 음수가 아닌 경우

# 🎯 [B] 시간 파생 변수 생성
# signup_time을 기준으로 다음 두 파생 변수를 생성하시오:
# - signup_hour: 시(hour)만 추출한 변수 (0~23 범위)
# - signup_period: 가입 시간대를 분류한 변수. 기준은 다음과 같음
#   • 오전(morning): 05시 ~ 11시
#   • 오후(afternoon): 12시 ~ 17시
#   • 야간(night): 그 외 시간대

user_cleaned_3 <- user_cleaned_2 %>%
  mutate(signup_hour = hour(signup_time)) %>%
  mutate(signup_period = case_when(
    signup_hour >= 5 & signup_hour <= 11 ~ "morning",
    signup_hour >= 12 & signup_hour <= 17 ~ "afternoon",
    TRUE ~ "night"
  ))

#### 250609/월 ####

# 📊 문제 3. 가입 시간대별 고객 통계 및 세션 분석

# 🎯 [A] 가입 시간대별 고객 분포
# signup_period별 고객 수를 집계하고, 전체 고객 중 각 그룹의 비율(%)을 계산하시오.
# 결과는 signup_period_summary라는 데이터프레임으로 저장하시오.
# 
# 출력 컬럼: signup_period, count, ratio

signup_period_summary <- user_cleaned_3 %>%
  group_by(signup_period) %>%
  count()
  
a <- 111+114+177

signup_period_summary$ratio <- round((signup_period_summary$n/a)*100,2)
signup_period_summary <- signup_period_summary %>%
  rename(count = n)


# 🎯 [B] 평균 session_length_sec 비교
# signup_period별로 session_length_sec의 평균을 계산하시오.
# 단, session_length_sec가 NA인 경우는 제외하시오.
# 결과는 session_summary라는 데이터프레임으로 저장하고,
# 평균값을 기준으로 내림차순 정렬하시오.
#
# 출력 컬럼: signup_period, session_avg

session_summary <- user_cleaned_3 %>%
  dplyr::filter(!is.na(session_length_sec)) %>%
  group_by(signup_period) %>%
  summarize(session_avg = mean(session_length_sec)) %>%
  arrange(desc(session_avg))

## 최적화 코드

signup_period_summary <- user_cleaned_3 %>%
  group_by(signup_period) %>%
  summarize(count = n()) %>%
  mutate(ratio = round(count / sum(count) * 100, 2))

# 🔤 문제 4. 텍스트 기반 감성 분석 및 시간대별 감성 분포

# 🎯 [A] 감성 레이블 파생 변수 생성
# feedback_comment 변수의 내용을 바탕으로 sentiment_label 파생 변수를 생성하시오.
# 다음 조건에 따라 분류하며, 대소문자는 구분하지 말 것:
# 
# - "positive": "excellent", "love"가 포함된 경우
# - "negative": "bad", "hate"가 포함된 경우
# - "neutral" : "okay", "not bad"가 포함된 경우
# - "missing": feedback_comment가 NA 또는 ""
# - 나머지 경우는 "other"로 분류할 것
# 
# 처리 결과는 user_sentiment라는 데이터프레임으로 저장하시오.

# 오류
user_cleaned_3 %>%
  mutate(feedback_comment = tolower(feedback_comment)) %>%
  mutate(
    sentiment_label = case_when(
      is.na(feedback_comment) | feedback_comment == "" ~ "missing",
      str_detect(feedback_comment, c("excellent","love")) ~ "positive",
      str_detect(feedback_comment, c("bad","hate")) ~ "negative",
      str_detect(feedback_comment, c("okay","not bad")) ~ "neutral",
      TRUE ~ "other"))
# 이유: str_detect에 두개의 문자형 벡터를 받으면 오류가남
# OR을 이용한 정규표현식으로 작성 필요

# 이렇게 쓰면 not bad도 bad로 판단될 수 있어서 수정 필요
# case_when절은 순서대로 검정하기 때문에 내츄럴을 먼저 작성
user_sentiment <- user_cleaned_3 %>%
  mutate(feedback_comment = tolower(feedback_comment)) %>%
  mutate(
    sentiment_label = case_when(
      is.na(feedback_comment) | feedback_comment == "" ~ "missing",
      str_detect(feedback_comment, "okay|not bad") ~ "neutral",
      str_detect(feedback_comment, "excellent|love") ~ "positive",
      str_detect(feedback_comment, "bad|hate") ~ "negative",
      TRUE ~ "other"
    )
  )

  
# 🎯 [B] 시간대별 감성 비율 분석
# sentiment_label이 "positive", "negative", "neutral"인 데이터만 대상으로,
# signup_period별로 각 감성 레이블의 비율(%)을 계산하시오.
# (즉, "missing"과 "other"는 제외)
#
# 출력 컬럼: signup_period, positive_pct, negative_pct, neutral_pct
# 결과는 sentiment_by_period라는 데이터프레임으로 저장하시오.

sentiment_by_period <- user_sentiment %>%
  dplyr::filter(str_detect(sentiment_label, "positive|negative|neutral")) %>%
  group_by(sentiment_label, signup_period) %>%
  summarize(count = n()) %>%
  mutate(ratio = round(count/sum(count)*100,2))

#### 250612/목 ####

# 📊 문제 5. 이상치 탐지 및 그룹별 요약 지표 생성

# 🎯 [A] 이상치 플래그 생성
# 'total_spent'와 'purchases' 모두 NA가 아니고, purchases가 0이 아닌 경우에 한해
# 아래 파생 변수를 생성하시오:
#   • unit_spent = total_spent / purchases

# 이후 unit_spent 값이 상위 1% 이상이면 is_outlier = TRUE, 아니면 FALSE로 표시하시오.
# (NA는 계산 제외)

# 🎯 [B] tier별 요약 통계 생성
# tier별로 다음 요약 지표를 구하시오 (단, unit_spent는 NA 제외):
#   • 평균 unit_spent
#   • 이상치 고객 비율 (is_outlier == TRUE 비율)
# 결과는 tier_summary라는 데이터프레임으로 저장하시오.

summary(user_cleaned_3)

user_cleaned_4 <- user_cleaned_3 %>%
  dplyr::filter((!is.na(total_spent)) & (!purchases == 0)) %>%
  mutate(unit_spent = total_spent / purchases)
  
out <- as.numeric(quantile(user_cleaned__4$unit_spent, 0.99))

user_cleaned_5 <- user_cleaned_4 %>%
  mutate(is_outlier = case_when(
    unit_spent >= out ~ TRUE,
    TRUE ~ FALSE
  ))

tier_summary <- user_cleaned_5 %>%
  dplyr::filter(!is.na(unit_spent)) %>%
  group_by(tier) %>%
  summarize(unit_spent_mean = mean(unit_spent),
            is_outlier_ratio = is.na(is_outlier)/nrow(is_outlier))


## 수정된 전체코드 ##

library(dplyr)

# A. unit_spent 계산 및 이상치 플래그 생성
user_cleaned_4 <- user_cleaned_3 %>%
  filter(!is.na(total_spent), !is.na(purchases), purchases != 0) %>%
  mutate(unit_spent = total_spent / purchases)

# 상위 1% 기준값
cutoff <- quantile(user_cleaned_4$unit_spent, 0.99, na.rm = TRUE)

user_cleaned_5 <- user_cleaned_4 %>%
  mutate(is_outlier = ifelse(unit_spent >= cutoff, TRUE, FALSE))  # 명확히 TRUE/FALSE 부여

# B. tier별 요약 통계
tier_summary <- user_cleaned_5 %>%
  filter(!is.na(unit_spent)) %>%
  group_by(tier) %>%
  summarize(
    unit_spent_mean = mean(unit_spent, na.rm = TRUE),
    is_outlier_ratio = mean(is_outlier, na.rm = TRUE),
    .groups = "drop"
  )

tier_summary
