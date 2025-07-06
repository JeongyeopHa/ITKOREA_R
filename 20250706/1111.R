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
