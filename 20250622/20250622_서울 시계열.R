#1. 데이터 수집
#1-1. 수집한 데이터를 불러오기
weather = read.csv('서울데이터.csv', #csv파일이름
                   na.strings = c(""),
                   fileEncoding = 'CP949', #한글깨짐 방지
                   encoding = 'UTF-8', #한글깨짐 방지
                   check.names = FALSE)

#View(weather)
str(weather)
print(head(weather))
print(nrow(weather))
print(summary(weather))
print(table(weather$지점명))

colnames(weather) = c('station_id', 'station_name', 'datetime', 'temp', 'precip',
                      'windspeed', 'winddir', 'humidity','CA')

print(head(weather))

weather$datetime = as.POSIXct(weather$datetime, format = '%Y-%m-%d %H:%M')

str(weather)
print(head(weather, 10))

print(colSums(is.na(weather)))

weather$precip = ifelse(is.na(weather$precip), 0 , weather$precip)

print(colSums(is.na(weather)))

weather$temp = ifelse(is.na(weather$temp), 0 , weather$temp)
print(colSums(is.na(weather)))
weather$windspeed = ifelse(is.na(weather$windspeed), 0 , weather$windspeed)
print(colSums(is.na(weather)))
weather$winddir = ifelse(is.na(weather$winddir), 0 , weather$winddir)
print(colSums(is.na(weather)))
weather$humidity = ifelse(is.na(weather$humidity), 0 , weather$humidity)
print(colSums(is.na(weather)))
weather$CA = ifelse(is.na(weather$CA), 0 , weather$CA)
print(colSums(is.na(weather)))

weather$feels_like = weather$temp - ((100 - weather$humidity) / 5)

print(head(weather,90))

print(summary(weather$temp))
print(summary(weather$humidity))
print(summary(weather$precip))
print(summary(weather$windspeed))

temp_sd = sd(weather$temp, na.rm = TRUE)
print(round(temp_sd,2))

temp_avg = mean(weather$temp, na.rm = TRUE)
CV = (temp_sd / temp_avg) * 100
cat('온도 변동 계수:', CV, '\n')

cor_mat = cor(weather[, c('temp','precip','windspeed','humidity')], use='complete.obs')
print(cor_mat)

library(corrgram)
par(mfrow = c(2,3))
hist(weather$temp, main='온도데이터 분포', xlab='온도(c)')
hist(weather$humidity, main='습도데이터 분포', xlab ='습도')
boxplot(weather$temp, main = '온도 박스플롯', ylab = '온도(c)')
plot(weather$datetime, weather$temp, type='l', main = '시간에 따른 온도변화', xlab='시간', ylab='온도')
plot(weather$temp, weather$humidity, main='기온 vs 습도', xlab='온도', ylab='습도')
model = lm(weather$humidity ~ weather$temp)
abline(model, col = 'red', lwd=2)
plot(weather$temp, weather$sindspeed, main='기온 vs 풍속', xlab='온도', ylab='풍속')
model = lm(weather$windspeed ~ weather$temp)
abline(model, col = 'red', lwd = 2)

library(dplyr)
Q1 = quantile(weather$temp, 0.25, na.rm = TRUE)
print(Q1)
Q3 = quantile(weather$temp, 0.75, na.rm = TRUE)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
print(lower_bound)
upper_bound = Q3 + 1.5 * IQR

weather = weather %>% mutate(temp=ifelse(temp < lower_bound | temp > upper_bound, NA, temp)) 
print(weather)
cat('이상치 데이터 개수:',sum(is.na(weather$temp)), '\n')

weather_daily = weather %>% mutate(data = as.Date(datetime)) %>% group_by(data) %>% summarise(temp_avg = mean(temp, na.rm =TRUE))
print(nrow(weather_daily))
print(head(weather_daily,90)
temp_ts = ts(weather_daily$temp_avg, frequency = 30)
print(temp_ts)
library(forecast)
auto_model = auto.arima(temp_ts)
forecasted = forecast(auto_model, h = 30)
print(forecasted)

predict_data = data.frame(time = as.numeric(time(forecasted$mean)),
                          forecast = as.numeric(forecasted$mean),lower = as.numeric(forecasted$lower[,2]), upper = as.numeric(forecasted$upper[,2]))
print(predict_data)
actual_data = data.frame(time = as.numeric(time(temp_ts)),temp = as.numeric(temp_ts))

library(ggplot2)

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