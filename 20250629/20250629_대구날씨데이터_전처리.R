weather = read.csv('대구날씨데이터.csv', na.strings = c(""), fileEncoding = 'CP949', encoding = 'UTF-8', check.names = FALSE)
str(weather)
colnames(weather) = c('station_id', 'station_name','datetime', 'temp', 'precip','windspeed','winddir','humidity','CA')
str(weather)
weather$datetime = as.POSIXct(weather$datetime, format = '%Y-%m-%d %H:%M')
str(weather)
weather$precip = ifelse(is.na(weather$precip),0,weather$precip)
weather$CA = ifelse(is.na(weather$CA),0,weather$CA)
print(colSums(is.na(weather))) 

#1. 풍속 IQR 이상치 제거
Q1 = quantile(weather$windspeed, 0.25)
cat('Q1:', Q1, '\n')
Q3 = quantile(weather$windspeed, 0.75)
cat('Q3:', Q3, '\n')

IQR_VALUE = Q3-Q1
cat('IQR_VALUE:', IQR_VALUE, '\n')

lower_bound = Q1 - 1.5*IQR_VALUE
upper_bound = Q3 + 1.5*IQR_VALUE

cat('lower_bound:', lower_bound, '\n')
cat('upper_bound:', upper_bound, '\n')
library(dplyr)
outliers = weather %>% filter(windspeed < lower_bound | windspeed > upper_bound)
print(outliers)
#View(weather)

#각 지점별로 평균 기온을 구하시오. (dplyr)
# hint: group_by()와 summarise() 

group_result = weather %>% group_by(station_name) %>% summarize(AVG_temp = mean(temp))
print(group_result)

#3.풍속이 3 m/s 이상인 데이터만 골라서, 해당 데이터의 평균 습도를 구하시오. (dplyr)
windspeed3m = weather %>% filter(windspeed>=3) %>% select(humidity) %>% summarize(AVG_humidity = mean(humidity))
print(windspeed3m)
# hint: filter()와 summarise()


#4. 3월부터 5월까지 서울의 평균 강수량, 최대 강수량, 최소 강수량 구하시오. (dplyr)
# hint: filter()와 summarise()
result = weather %>% filter(datetime>=3) %>% filter(datetime<=5) %>% filter()

#5.각 지점별로 기온이 가장 높았던 시간대와 그 값을 구하시오. (dplyr) 단, 값을 기준으로 내림차순 할 것
# hint: group_by()와 summarise(), arrange()
result = weather %>% group_by(station_name) %>% select(temp) %>% summarize(MAX_temp = max(temp)) %>% arrange(desc(MAX_temp))
print(result)



#6. 날짜별 습도 평균 구하기 (dplyr)
# hint: mutate()와 group_by(), summarise()
str(weather)
