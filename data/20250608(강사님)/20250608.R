data = data.frame(
  성별 = c('M','M','M','M','M','F','F','F','F','F'),
  키 = c(175, 168, 180, 170, 172, 160, 158, 165, 162, 155),
  몸무게 = c(70, 65, 80, 72, 68, NA, 50, 60, 52, 48)
)
# 데이터프레임
# View(data) 

결과 = data #data에 들어있는 정보를 `결과`라는 변수에 대입
#View(결과)
결과$x = c(70, 65, 80, 72, 68, NA, 50, 60, 52, 48) #컬럼추가
#View(결과) #새로운 컬럼 x 추가
#View(data)

# 전처리 도구
# install.packages("dplyr") 설치는 한번만
library(dplyr) #설치된 도구를 불러옵니다. (디플리알)

# mutate : 새로운 열 추가
# 필터링된 결과를 새로운 열로 추가할 때는 디플리알을 이용하자.
#data = data %>% mutate(x = 10)
#View(data)

결과 = data %>% mutate(x = 10)
#View(결과)
#View(data)

# 상관계수
# 상관계수는 두 변수가 서로 어떻게 관련되어 있는지를 측정하며, 수치로 표현
data = data.frame(
  성별 = c('M','M','M','M','M','F','F','F','F','F'),
  키 = c(175, 168, 180, 170, 172, 160, 158, 165, 162, 155),
  몸무게 = c(70, 65, 80, 72, 68, 55, 50, 60, 52, 48)
)
# cor은 correlation의 약자로 `상관계수` 뜻
상관계수 = cor(data$키, data$몸무게)
print(상관계수) # NA결과가 나옴...
# NA : 결측값, 데이터 수집에 누락된 값(혹은 실패한 값)

# 결측값 처리
# obs -> observation (NA를 제외한 값들 계산)
상관계수 = cor(data$키, data$몸무게, use = 'complete.obs')
print(상관계수) # 0.9685347

# 상관계수는 보통 -1에서 1까지의 값을 가집니다.
# 1: 두 변수가 완벽하게 양의 선형 관계를 가짐. 즉, 한 변수가 증가할 때
# 다른 변수도 증가함.
# -1: 두 변수가 완벽하게 음의 선형 관계를 가짐. 즉, 한 변수가 증가할 때 다른 변수는 감소함.
# 0 : 두 변수 간에 선형 관계가 전혀 없음

# 디플리알을 이용해서 성별 키와 몸무게 상관계수를 조회
성별_상관계수 = data %>% group_by(성별) %>% summarise(
  상관계수 = cor(키, 몸무게, use = 'complete.obs')
)
print(성별_상관계수)

# 산점도 시각화
# main : 그래프 이름
# xlab : x 축 이름
# ylab : y 축 이름
# col : 점 색깔 지정
# pch : 점 크기
plot(data$키 , data$몸무게, main = '산점도와 회귀선', xlab = 'X 축', ylab = 'Y축', col = "blue", pch = 19)

# 회귀선 추가
# lm : linear mobel(선형 모델)
model = lm(data$몸무게 ~ data$키)
print(summary(model)) # 모델 요약
# add a line : 선 추가
abline(model, col = 'red', lwd = 2)

# if/else ***
data$신체점수 = c(90, 95, 85, 72, 69, 79, 77, 80, 82, 68)
# 등급 추가
data$신체점수_등급 = ifelse(data$신체점수 >= 90, 'High', 'Low')

# if/else if/else
# 90점 이상은 High
# 70점 미만은 Low
# 그 외 Mid
data$신체점수_등급 = ifelse(data$신체점수 >= 90, 'High', ifelse(
  data$신체점수 < 70, 'Low', 'Mid'
))
View(data) #최종 결과 확인















