library(dplyr)  # 로드
# 한국어 파일의 경우, CP949 또는 EUC-KR 인코딩을 사용하는 것이 일반적
data = read.csv('서울특별시_공공자전거_이용정보.csv',
                na.strings = c(""), # ""를 NA로 표현한다.
                fileEncoding = 'CP949', 
                encoding = 'UTF-8', 
                check.names = FALSE)
View(data)
# 문제 1: 성별 운동량 박스플롯 표현. 단, 이용시간(분) 3분 이하는 제외

성별_데이터확인 = data %>% group_by(성별) %>% summarise(CNT = n())
# print(성별_데이터확인)

문제1 = data %>% filter(`이용시간(분)` > 3 & !is.na(성별))
# 박스플롯
boxplot(운동량 ~ 성별, 
        data = 문제1, 
        main = '성별 운동량 박스플롯',
        xlab = '성별',
        ylab = '운동량',
        col = c('grey', 'pink', 'skyblue')
        )

# 문제 2: 연령대별 평균 이동거리(M)를 막대그래프로 표현.
문제2 = data %>% group_by(연령대코드) %>% summarise(
  평균이동거리 = mean(`이동거리(M)`, na.rm = TRUE)
)
# 막대그래프 
barplot(문제2$평균이동거리, #데이터
        names.arg = 문제2$연령대코드, #x축 데이터
        main = '연령대 별 평균 이동거리',
        xlab = '연령대',
        ylab = '평균 이동거리'
        )
#  정기권을 구매한 이용자 중 연령대 별 평균 운동량 파이차트로 표현. 
#  단, 이용시간(분) 5분 이하는 평균에서 제외
문제4 = data %>% filter(대여구분코드 == '정기권' & `이용시간(분)` > 5) %>%
  group_by(연령대코드) %>% summarise(평균운동량 = mean(운동량, na.rm = TRUE))
# 도넛

pie(문제4$평균운동량, main='연령대별 평균 운동량', col=rainbow(7),
    labels = 문제4$연령대코드)
# 도넛 효과
symbols(0, 0, circles = 0.5, inches = FALSE, add=TRUE, bg='white')

# 문제 5: 정기권 회원 중 연령대가 10대에서 50대 사이의 이용시간과 운동량을 비교하는 산점도그래프 표현하시오.(이용시간 x축, 운동량 y축) 회귀선까지 추가하시오.
문제5 = data %>% filter(대여구분코드 == '정기권' & 
                        연령대코드 %in% c('10대','20대','30대','40대','50대'))
# 산점도 
plot(문제5$`이용시간(분)`, 문제5$운동량, main = '이용시간과 운동량 관계',
     xlab = '이용시간(분)', ylab = '운동량')
#회귀선 
model = lm(운동량 ~ `이용시간(분)`, 문제5)
abline(model, col = 'red' , lwd = 2)





