weather = read.csv('대구날씨데이터.csv', na.strings = c(""), fileEncoding = 'CP949', encoding = 'UTF-8', check.names = FALSE)
#View(weather)
str(weather)
print(head(weather))
print(tail(weather))
print(summary(weather))
print(table(weather$지점명))
print(nrow(weather))
colnames(weather) = c('station_id', 'station_name','datetime', 'temp', 'precip','windspeed','winddir','humidity','CA')
str(weather)
weather$datetime = as.POSIXct(weather$datetime, format = '%Y-%m-%d %H:%M')
str(weather)
print(colSums(is.na(weather)))
weather$precip = ifelse(is.na(weather$precip),0,weather$precip)
weather$CA = ifelse(is.na(weather$CA),0,weather$CA)
print(colSums(is.na(weather)))
summary(weather$windspeed)
summary(weather$winddir)
summary(weather$humidity)

습도_평균값 = mean(weather$humidity, na.rm = TRUE)
cat('습도 평균값:', 습도_평균값,'\n')
습도_표준편차 = sd(weather$humidity, na.rm = TRUE)
cat('습도 표준편차:', 습도_표준편차,'\n')
CV = round((습도_표준편차 / 습도_평균값)*100)
cat('변동계수:', CV, '\n')

풍속_습도의상관관계 = cor(weather$windspeed, weather$humidity, use = 'complete.obs')
cat('풍속과 습도의 상관계수:',풍속_습도의상관관계,'\n')

#기온, 풍속, 풍향, 습도, 전운량 상관관계 확인 및 시각화
cor_mat = cor(weather[, c('temp',"windspeed",'humidity','CA')], use='complete.obs')
print(cor_mat)
library(corrgram)
corrgram(cor_mat, main = '기온,풍속,풍향,습도,전운량 상관계수', lower.panel = panel.shade, upper.panel = panel.cor)

#온도와 습도 데이터 분포를 히스토그램으로 표현
hist(weather$temp, main='온도데이터 분포', xlab='온도(c)')
hist(weather$humidity, main='습도데이터 분포', xlab='습도')

#풍속의 이상치를 탐색하기 위해 박스플롯 표현

boxplot(weather$windspeed, main = '풍속 박스플롯', ylab = '풍속(s)')

# 온도와 습도의 상관관계 표현, 도시 별 색깔 다르게 지정. 단, 범례도 추가할 것

plot(weather$temp, weather$humidity, col = c('red','blue','yellow','purple','green','orange'),xlab = '온도', ylab = '습도', 
     main='온도와 습도의 관계')

legend('topright', legend=unique(weather$station_name), col=c('red','blue','yellow','purple','green','orange'),pch=19, cex=0.8)

#풍속과 풍향 시각화
library(openair)
windRose(weather, ws = 'windspeed', wd = 'winddir')

#풍속과 습도 K 평균 군집화 시각화

weather_var = weather[,c('windspeed','humidity')]
set.seed(123)
clusters = kmeans(weather_var, centers = 3)
plot(weather$windspeed, weather$humidity, col=clusters$cluster, xlab = '풍속', ylab = '습도', main = '
     풍속과 습도 클러스터링')

#온도 변화 시계열 그래프 그리기

plot(weather$datetime, weather$temp, type='l', col = 'blue',xlab = '날짜', ylab ='온도', main = '날짜별 온도 변화')

# = > 줄은 왜 생기는지??


