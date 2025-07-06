library(dplyr)  # 로드
# 한국어 파일의 경우, CP949 또는 EUC-KR 인코딩을 사용하는 것이 일반적
data = read.csv('서울특별시_공공자전거_이용정보.csv',
                na.strings = c(""), # ""를 NA로 표현한다.
                fileEncoding = 'CP949', 
                encoding = 'UTF-8', 
                check.names = FALSE)
#View(data)
str(data)
# 문제 1: 이동거리(M)가 2000 이상인 데이터 중 해당 대여소명과 이동거리(M), 이용시간(분)만 조회.
문제1 = data %>% filter(`이동거리(M)` >= 2000) %>% select(대여소명, `이동거리(M)`, `이용시간(분)`)
#View(문제1)

# 문제 2: 대여소 별 이용건 수 조회.
# na.rm = TRUE : 결측값(누락된 데이터) 제외
# summarise 요약
문제2 = data %>% group_by(대여소명) %>% summarise(이용건수_합계 = sum(이용건수, na.rm= TRUE))
#View(문제2)

# 문제 3: 일일회원과 정기회원 이용 건 수, 평균 이용시간 조회. 단, 일일회원권 중 비회원은 제외
회원타입 = data %>% group_by(대여구분코드) %>% summarise(CNT = sum(이용건수))
# print(회원타입)
문제3 = data %>% filter(대여구분코드 != '일일권(비회원)') %>% 
  group_by(대여구분코드) %>% summarise(총이용건수 = sum(이용건수, na.rm = TRUE),
                                 평균이용시간 = mean(`이용시간(분)`, na.rm = TRUE)
                                 )
# 문제 4: 탄소량이 0.8 이상인 이용 건수는 몇 건인지 조회.
# n(): 행 수
문제4 = data %>% filter(탄소량 >= 0.8) %>% summarise(CNT = n())

# 문제 6: 연령대별로 이용건수의 합과 평균 운동량을 구한 뒤, 운동량 평균이 가장 높은 연령대 조회.
# arrange : 정렬, slice : 자르다
문제6 = data %>% group_by(연령대코드) %>% summarise(
  이용건수 = sum(이용건수, na.rm = TRUE), 
  평균운동량 = mean(운동량, na.rm = TRUE)
  ) %>% arrange(desc(평균운동량)) %>% slice(1)
print(문제6)
# 문제7. 대여소명에 "역"이 포함된 대여소에서 발생한 총 운동량의 합 조회.

text = c('apple', 'banana', 'grape', 'bread')
# 'ap'가 들어간 단어찾기
# grep(잡다) logical(TRUE, FALSE)
print(grepl('ap',text))
문제7 = data %>% filter(grepl('역',대여소명)) %>% summarise(총운동량 = sum(운동량, na.rm = TRUE))
print(문제7)
#문제 9: 운동량을 데이터 스케일링 min-max로 변환한 scaled_운동량 컬럼을 추가 0.8 이하인 회원 이동거리 사분위수 출력
# min-max : 데이터의 범위를 0과 1사이로 변환하는 데이터 전처리 기법
# 예) 간수치 30 
# 공식 : (기존값 - 최솟값) / (최댓값 - 최솟값)
운동량_min = min(data$운동량)
운동량_max = max(data$운동량)

min_max_결과 = (data$운동량 - 운동량_min) / (운동량_max - 운동량_min)
print(min_max_결과)# 0 ~ 1사의 숫자로 변환

data$scaled_운동량 = min_max_결과 #새로운 컬럼에 min-max값 대입
# 0.8 이하인 회원 이동거리 
운동량_0.8_이하_회원 = data %>% filter(scaled_운동량 <= 0.8)
# 사분위수 출력
print(quantile(운동량_0.8_이하_회원$`이동거리(M)`))
# 0%(최솟값)   25%(하위 25%)   0%(중앙값)      75%(상위 25%)     100%(최댓값)

#문제 10. 나이대 별 운동량과 이동거리의 상관관계 조회.
# cor = correlation 
# use = 'complete.obs' : 결측값 제외
문제10 = data %>% group_by(연령대코드) %>% summarise(
  상관계수 = cor(운동량, `이동거리(M)`, use = 'complete.obs')
  )
print(문제10)


