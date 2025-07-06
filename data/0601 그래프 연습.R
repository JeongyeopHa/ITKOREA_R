library(dplyr)  # 로드
# 한국어 파일의 경우, CP949 또는 EUC-KR 인코딩을 사용하는 것이 일반적
data = read.csv('서울특별시_공공자전거_이용정보.csv',
                na.strings = c(""), # ""를 NA로 표현한다.
                fileEncoding = 'CP949', 
                encoding = 'UTF-8', 
                check.names = FALSE)
View(data)
성별_데이터확인 = data %>% group_by(성별) %>% summarise(CNT=n())
print(성별_데이터확인)
문제1=data %>% filter(`이용시간(분)`>3 & !is.na(성별))
boxplot(운동량~성별, data = 문제1, main = '성별 운동량 박스플롯',xlab='성별',ylab='운동량',
col=c('grey','pink','skyblue'))

문제2=data %>% group_by(연령대코드) %>% summarise(평균이동거리=mean(`이동거리(M)`,na.rm=TRUE))
barplot(문제2$평균이동거리,names.arg=문제2$연령대코드,main='연령대 별 평균 이동거리', 
xlab='연령대',ylab='평균 이동거리')

문제4= data %>% filter(대여구분코드== '정기권' & `이용시간(분)` > 5) %>% group_by(연령대코드) %>% summarise(평균운동량 = 
  mean(운동량,na.rm=TRUE))
pie(문제4$평균운동량, main='연령대별 평균 운동량',col=rainbow(7),labels=문제4$연령대코드)
symbols(0,0,circles=0.5, inches = FALSE, add=TRUE, bg='white')
문제5=data %>%filter(대여구분코드 == '정기권' & 연령대코드 %in% c('10대','20대','30대','40대','50대'))
plot(문제5$`이용시간(분)`, 문제5$운동량,main='이용시간과 운동량 관계',xlab='이용시간(분)', ylab='운동량')
model = lm(운동량 ~ `이용시간(분)`, 문제5)
abline(model, col = 'red', lwd=2)
