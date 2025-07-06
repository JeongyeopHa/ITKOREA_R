weather = read.csv('대구날씨데이터.csv', na.strings = c(""), fileEncoding = 'CP949', encoding = 'UTF-8', check.names = FALSE)

str(weather)

colnames(weather) = c('station_id', 'station_name','datetime', 'temp', 'precip','windspeed','winddir','humidity','CA')

#데이터 요약
# summary : 내가 분석하고자 하는 컬럼 통계 확인

print(summary(weather$humidity))
print(table(weather$station_name)) # 지점별 데이터 수 확인

print(nrow(weather)) #데이터프로임 총 행 수 

#데이터 형변환
weather$datetime = as.POSIXct(weather$datetime, format = '%Y-%m-%d %H:%M')
weather$station_name = as.factor(weather$station_name)
weather$precip = ifelse(is.na(weather$precip),0,weather$precip)
weather$CA = ifelse(is.na(weather$CA),0,weather$CA)
print(colSums(is.na(weather)))

#온도와 습도의 상관관계 표현

colors = rainbow(6) # 지점 총 6개
# 시각화
plot(weather$temp, weather$humidity, col = colors, xlab = '온도', ylab = '습도', main='온도와 습도의 관계')

#pch=19 : 점크기
#cex=0.5 : 크리를 50%로 줄인다.
print(weather$station_name)
legend('topright', legend=unique(weather$station_name), col = colors, pch=19, cex=0.5)

# 풍속과 습도 k평균 군집화 시각화
weather_var = weather[,c('windspeed','humidity')]
set.seed(123) #항상 같은 결과가 나오게 설정
clusters = kmeans(weather_var, centers = 3)
plot(weather$windspeed, weather$humidity, col=clusters$cluster)

#온도 변화 시계열 그래프
#as.Date : 년/월/일 까지 표기 시간 생략
weather$date = as.Date(weather$date)
plot(weather$date, weather$temp, type='l',col='blue', main="날짜별 온도 변화", xact='n')
month = seq(as.Date('2025-01-01'), as.Date('2025-07-01'), by="month")
axis.Date(1, at=month[month<= max(weather$date)],format='%Y-%m', las=1, cex.axis = 0.7)


#각 지점별로 평균 기온을 구하시오. (dplyr)
# hint: group_by()와 summarise() 

mean_temp_by_station = weather %>% group_by(station_name) %>% summarise(avg_temp = mean(temp, na.rm = TRUE))
print(mean_temp_by_station)


#풍속이 3 m/s 이상인 데이터만 골라서, 해당 데이터의 평균 습도를 구하시오. (dplyr)
# hint: filter()와 summarise()

mean_hum_high_wind = weather %>% filter(windspeed >=3) %>% summarise(평균습도 = mean(humidity, na.rm = TRUE))
print(mean_hum_high_wind)

#3월부터 5월까지 서울의 평균 강수량, 최대 강수량, 최소 강수량 구하시오. (dplyr)
# hint: filter()와 summarise()

seoul_data = weather %>% filter(datetime >= '2025-03-01 01:00' & 
                                  datetime <= '2025-05-31 24:00' & 
                                  station_name == '서울') %>%
  summarise(평균강수량 = mean(precip, na.rm = TRUE),
            최대강수량 = max(precip, na.rm = TRUE),
            최소강수량 = min(precip, na.rm = TRUE))

print(seoul_data)

seoul_data2 = weather %>% mutate(month = format(datetime, '%m')) %>%
  filter(month %in% c('03','04','05') & station_name == '서울') %>%
  summarise(평균강수량 = mean(precip, na.rm = TRUE),
            최대강수량 = max(precip, na.rm = TRUE),
            최소강수량 = min(precip, na.rm = TRUE))
print(seoul_data2)




#각 지점별로 기온이 가장 높았던 시간대와 그 값을 구하시오. (dplyr) 단, 값을 기준으로 내림차순 할 것
# hint: group_by()와 summarise(), arrange()

max_temp_by_station = weather %>% group_by(station_name) %>% filter(temp== max(temp,na.rm = TRUE)) %>% select(station_id, station_name, datetime, temp) %>% arrange(desc(temp))
print(max_temp_by_station)

max_temp_by_station2 = weather %>% group_by(station_name) %>% select(station_id, station_name, datetime, temp) %>% slice_max(temp, n = 1) %>% arrange(desc(temp))
print(max_temp_by_station2)


#날짜별 습도 평균 구하기 (dplyr)

mean_hum_by_date = weather %>% mutate(data = as.Date(datetime)) %>% group_by(date) %>% summarise(평균습도 = mean(humidity, na.rm = TRUE))
print(mean_hum_by_date) 
     

humidity_ts = ts(mean_hum_by_date$평균습도, frequency = 30)

library(forecast)
auto_model = auto.arima(humidity_ts)
#  h = 에 올바른 값 넣기
forecasted = forecast(auto_model, h = 10 )

predict_data = data.frame(
  time = as.numeric(time(forecasted$mean)),
  forecast = as.numeric(forecasted$mean),
  lower = as.numeric(forecasted$lower[,2]),  # 95% 신뢰구간 하한
  upper = as.numeric(forecasted$upper[,2])   # 95% 신뢰구간 상한
)
actual_data = data.frame(
  time = as.numeric(time(humidity_ts)),
  temp = as.numeric(humidity_ts)
)

library(ggplot2) #고급 시각화

line_plot = ggplot() +
  # 실제값 선그래프
  geom_line(data = actual_data, aes(x = time, y = temp), color = "steelblue", size = 1) +
  # 예측 신뢰구간 리본
  geom_ribbon(data = predict_data, aes(x = time, ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
  # 예측값 선그래프
  geom_line(data = predict_data, aes(x = time, y = forecast), color = "red", size = 1.2, linetype = "dashed") +
  labs(
    title = "습도 예측",
    x = "Time",
    y = "습도"
  ) +
  theme_minimal()

print(line_plot)








