weather = read.csv('날씨데이터.csv', #csv파일이름
                   na.strings = c(""),
                   fileEncoding = 'CP949', #한글깨짐 방지
                   encoding = 'UTF-8', #한글깨짐 방지
                   check.names = FALSE)


str(weather)
print(head(weather))   
print(nrow(weather))
print(summary(weather))
print(table(weather$지점명))
colnames(weather) = c('station_id', 'station_name', 'datatime', 'temp', 'precip',
                      'windspeed', 'winddir', 'humidity', 'CA')

print(head(weather))

weather$datatime = as.POSIXct(weather$datatime, format = '%Y-%m-%d %H:%M')
str(weather)
print(colSums(is.na(weather)))
weather$precip = ifelse(is.na(weather$precip), 0 , weather$precip)
print(colSums(is.na(weather)))

# 누락된 컬럼이 있는지?
# 체감온도는 없네? 만들어보자
weather$feels_like=weather$temp - ((100- weather$humidity) / 5)

print(head(weather))

# 3.기초 통계량 확인 및 시각적 탐색(EDA)
# 3-1. 분석할 컬럼 통계량 산출
print(summary(weather$temp)) #온도 통계량 요약
print(summary(weather$humidity)) #습도 통계량 요약
print(summary(weather$precip)) #강수량 통계량 요약
print(summary(weather$windspeed)) #풍속 통계량 요약

# 온도 표준편차
# na.rm =TRUE : 결측값을 제외하고 표준편차 구하기
# 표준편차가 작다는 것은 데이터가 평균 주변 잘 모여있음
# 표준편차가 크게 나왔음 -> 아하, 이상치 제거를 해야겠구나
temp_sd = sd(weather$temp, na.rm = TRUE)
print(round(temp_sd,2)) #round 소수점 반올림

# 변동계수
temp_avg = mean(weather$temp, na.rm = TRUE)
CV = (temp_sd / temp_avg) * 100
cat('온도 변동 계수 :', CV, '\n')
# 보통 cv가 10~20 이하면 고르게 분포
# 50 이상이면, 데이터가 평균에 비해 상당히 넓게 분포되어 있음

# 상관계수 행렬
# 상관계수 값은 -1 ~ 1 사이로, 0에 가까울수록 상관관계가 약함을 의미.
# mat(행렬)
# $ : 컬럼접근, [] : 데이터 접근
# 온도, 강수량, 풍속, 습도 상관관계 확인하기
cor_mat = cor(weather[, c('temp','precip','windspeed','humidity')], use='complete.obs')
print(cor_mat)

# 그래프로 해당 상관관계 표현
# 시각적 탐색(EDA)
# 단일변수 시각화
# 두 변수간의 관계 시각화
library(corrgram) #상관관계 그래프

#시각화
#corrgram(cor_mat, main = '온도,강수량,풍속,습도 상관계수', 
         lower.panel = panel.shade, upper.panel = panel.cor)

# 기본 plot으로 시각화하기
# ggplot2은 복잡한 시각화에 적합

# 히스토그램(단일변수 시각화)



hist(weather$temp, main='온도데이터 분포', xlab='온도(c)')
# 습도 히스토그램
hist(weather$humidity, main='습도 분포', xlab='습도')
# 박스플롯
boxplot(weather$temp, main = '온도 박스플롯', ylab = '온도(c)')

#par(mfrow = c(1,3)) #1행 3열로 그래프 배치(단,기본그래픽가능)

# 두 변수 간의 관계를 시각적으로 표현
# 1. 시간별 기온 변화
# type='l' : line을 의미
par(mfrow = c(1,1)) #1행 1열로 다시 나오게 복구
plot(weather$datatime, weather$temp, type="l", main = '시간에 따른 온도변화', xlab='시간',ylab='온도')

# 2. 기온과 습도 관계
# 산점도로 회귀선까지 추가.
# 기온과 습도는 음의관계
plot(weather$temp, weather$humidity, main='기온 vs 습도', xlab='온도', ylab = '습도')
#회귀선
#lm : linear model
model = lm(weather$humidity ~ weather$temp) #선형회귀 모델 생성
#abline : add line
abline(model, col='red', lwd = 2)
# 습도가 높을수록 기온이 낮아지는 경향이 있습니다.

# 풍속과 기온 관계를 산점도 표현

plot(weather$temp, weather$windspeed, main='기온 vs 풍속',
     xlab='온도', ylab='풍속')
model = lm(weather$windspeed ~ weather$temp) #선형회귀 모델 생성
abline(model, col = 'red', lwd = 2)


# 풍속이 높을수록 기온도 약간 높아지는 경향이 있습니다.

# 4. 데이터 전처리
# 이상치 제거
# 디플리알
library(dplyr) # 디플리알 불러오기
# 기온 이상치 판단
# 1. z-score, IQR
# z-score(평균, 표준편차을 기준): 키,몸무게,성적 등 정규분포
# IQR(사분위수) : 근로소득, 강수량, 기온(비대칭 데이터)

# IQR을 이용해서 이상치 판단
# 데이터를 크기 순서대로 줄 세웠을 때, 가운데 50%가 얼마나 퍼져 있는지를 알려줍니다.

Q1 = quantile(weather$temp, 0.25, na.rm = TRUE)
Q3 = quantile(weather$temp, 0.75, na.rm = TRUE)
IQR = Q3 - 01

# 1.5 ? => 통계학자 '존튜키' 제안한 것으로
# 1.5수치는 너무 좁지도, 너무 넓지도 않은 적절한 범위를 설정하기 위해 표준으로 사용하고 있습니다.
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR

# mutate : 컬럼 수정 or 추가할 떄
weather = weather %>% mutate(temp = ifelse(temp < lower_bound | temp > upper_bound, NA, temp))
#이상치 데이터 개수 확인
#기온데이터가 NA라는건 이상치라는 뜻
#sum : 총합
print(sum(is.na(weather$temp)))
cat('이상치 데이터 개수 : ', sum(is.na(weather$temp)), '\n')

# 5월 1일부터 ~ 5월 31일 데이터 대전 지점 데이터 필터링(디플리알)
# 대전 5월날씨 가져옴
weather_filter = weather %>% filter(datatime >= '2025-05-01 00:00' &
                                       datatime <= '2025-05-31 23::59' & station_name == '대전')
# View(weather_filter)
# 시간별 데이터를 수집 => 날짜별 평균을 내어 하루에 하나의 값만 남기는 전처리
# 디플리알 Hint==> as.DATA(): 시간정보를 무시
# data라는 컬럼만들기
weather_daily = weather_filter %>% mutate(date = as.Date(datatime)) %>% group_by(data) %>% summarise(temp_avg
                                                                                                     = mean(temp, na.rm = TRUE))
print(head(weather_daily)) # 필터링 결과 확인
# 5. 실용적분석
# 시간마다 기록된 숫자 => 시계열 데이터
# 시계열 분석은 이 숫자들이 어떻게 바뀌는지 어떤 규칙이 있는지 알아 볼 수 있습니다.

# 시계열데이터(time series)
# frequency = 30 : 일별 평균 기온데이터를 한달 주기의 시계열 데이터로 변환
temp_ts = ts(weather_daily$temp_avg, frequency = 30)
print(temp_ts)

#install.packages('forecast')
library(forecast) #시계열 데이터를 바탕으로 미래를 예측하는 통계 모델

auto_model = auto.arima(temp_ts)

# 향후 30시간 기온 예측
#forecast : 미래 패턴을 예측하고 신뢰구간까지 산출함.
forecasted = forecast(auto_model, h= 30)

# 예측 결과 데이터프레임 생성
predict_data = data.frame(
  time = as.numeric(time(forecasted$mean)),
  forecast = as.numeric(forecasted$mean),
  lower = as.numeric(forecasted$lower[,2]),  # 95% 신뢰구간 하한
  upper = as.numeric(forecasted$upper[,2])   # 95% 신뢰구간 상한
)
print(predict_data)

# 실제 값 데이터 프레임 생성
actual_data = data.frame(
  time = as.numeric(time(temp_ts)),
  temp = as.numeric(temp_ts)
  
  library(ggplot2) #고급 시각화
  
  line_plot = ggplot() +
    # 실제값 선그래프
    geom_line(data = actual_data, aes(x = time, y = temp), color = "steelblue", size = 1) +
    # 예측 신뢰구간 리본
    geom_ribbon(data = predict_data, aes(x = time, ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
    # 예측값 선그래프
    geom_line(data = predict_data, aes(x = time, y = forecast), color = "red", size = 1.2, linetype = "dashed") +
    labs(
      title = "Temperature Forecast",
      x = "Time",
      y = "Temperature"
    ) +
    theme_minimal()
  
  print(line_plot)
  
