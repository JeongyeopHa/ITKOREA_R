# 폴더 경로 확인

# CSV파일 불러오기

#print(getwd())#폴더 조회

#print(list.files()) #폴더에 저장된 파일 조회

setwd('C:/Users/admin/Desktop/r_workspaces') #강제로 경로지정


# CSV 파일 불러오기
weather = read.csv('대전날씨데이터.csv', na.strings = c(""), fileEncoding = 'CP949', encoding = 'UTF-8', check.names = FALSE)
#View(weather)
str(weather)

# 9개 컬럼명(벡터명)을 한글 -> 영어 수정
# col : 컬럼
colnames(weather) = c('station_id', 'station_name', 'datetime', 'temp', 'precip','windspeed','winddir','humidity','CA')
str(weather)

# 각 컬럼들 결측값 조회
print(colSums(is.na(weather)))
weather$precip = ifelse(is.na(weather$precip),0,weather$precip)
print(colSums(is.na(weather)))

# datetime 문자에서 -> 날짜형으로 변환
# 수집한 날짜 데이터에 시간,분,초가 있으면 as.Date로 변환 시 생략됨
weather$datetime = as.POSIXct(weather$datetime, format= '%Y-%m-%d %H:%M')
str(weather)

# 산점도
plot(weather$temp, weather$humidity, col = c('red','blue'),xlab='온도', ylab = '습도', main='온도와 습도의 관계')

# 범례 추가

legend('topright', legend=unique(weather$station_name), col=c('black'), pch=19)

#클러스터링 시각화
#클러스터 : 덩어리, 군집, 무리
#클러스터 + ing : 군집을 형성하다.
#ex) 비슷한 날짜 패턴을 가진 시점을 그룹화

# Kmeans(K 평균 군집화)
# 기온하고 습도 열만 선택[]
weatehr_var = weather[,c('temp','humidity')]

set.seed(123)

# 데이터를 3개의 그룹으로 나눠줘
clusters = kmeans(weatehr_var, centers = 3)
#군집 결과 시각화
plot(weather$temp, weather$humidity, col=clusters$cluster, xlab = '온도', ylab='습도', main='날씨와 습도 클러스터링')

#풍향, 풍속 시각화

library(openair) #바람데이터를 시각화할 때 사용
windRose(weather, ws='windspeed', wd = 'winddir')

#풍속과 습도의 상관관계
print(cor(weather$windspeed, weather$humidity)) #음의 관계

#온도 변화 시계열 그래프 그리기
#시계열 : 시간의 흐름에 따라 연속적으로 수집된 데이터
#시계열DB 따로 있음
#lab(라벨)
plot(weather$datetime,weather$temp,type='l', col='blue', xlab = '날짜', ylab='온도', main = '날짜별 온도 변화')


