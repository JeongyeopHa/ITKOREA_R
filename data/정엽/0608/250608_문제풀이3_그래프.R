library(dplyr)  # 로드
# 한국어 파일의 경우, CP949 또는 EUC-KR 인코딩을 사용하는 것이 일반적
data = read.csv('국민건강보험공단_건강검진정보.csv',
                na.strings = c(""), # ""를 NA로 표현한다.
                fileEncoding = 'CP949', 
                encoding = 'UTF-8', 
                check.names = FALSE)
View(data)

#문제 2. : 성별 허리둘레와 체중 상관관계 조회.

상관계수 = cor(data$허리둘레,data$체중))
print(상관계수)

# 문제 5. : 연령대 코드가 5~8인 사람 중 
# 혈색소의 중앙값, 하위 30%, 상위 10%, 표준편차 조회.

