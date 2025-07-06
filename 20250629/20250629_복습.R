data = data.frame(#벡터 집합
  name = c('a','b','c','d','e','f'), #벡터(열)
  heights  = c(120, 130, 140, 150, 160, 200) #벡터
)

print(data$heights) # $(R에서만 씀) : 열접근
print(data[ 3,c('name','heights')]) #콤마를 기준으로 왼쪽은 행, 오른쪽 열

# $ : 새로운 컬럼을 생성할 수 있음

data$birth = c('1999-01-01','1999-01-01','1999-01-01','1999-01-01','1999-01-01','1999-01-01')

#데이터 구조확인
str(data)
# 문자를 날짜형으로 변환
# 날짜데이터에 년/월/일만 있으면 as.Date 로
# $ : 조회, 수정 , 추가
# [] : 조회 (행,열)
data$birth = as.Date(data$birth)
str(data)

# 이상치 제거 -> IQR
# IQR => 소득(비대칭 데이터) 이런 데이터를 탐지할 때 사용

# 1. Q1과 Q3을 구하기
Q1 = quantile(data$heights, 0.25) # 하위 25%
cat('Q1 :', Q1, '\n')
Q3 = quantile(data$heights, 0.75) # 하위 75%
cat('Q3 :', Q3, '\n')

# 2. IQR(사분위 범위) 구하기
# 프로그래밍에서 변수이름이 대문자? -> 수정 X
IQR_VALUE = Q3 - Q1 # 157.5 - 132.5 = 25
cat('IQR_vALUE:', IQR_VALUE, '\n')

# 3. 이상치 기준선 만들기
# 변수이름이 소문자 -> 수정 O
# 1.5(존 튜키, 통계학자가 제안한 것. 표준 기준으로 널리 사용하고 있음)
lower_bound = Q1 - 1.5 * IQR_VALUE
upper_bound = Q3 + 1.5 * IQR_VALUE

# 키 95 ~ 195가 아닌 학생들 데이터는 이상치로 판단.

cat('lower_bound:', lower_bound, '\n')
cat('upper_bound:', upper_bound, '\n')

# 4. 이상치 확인
library(dplyr)
outliers = data %>% filter(heights < lower_bound | heights > upper_bound)
print(outliers)

#키 데이터 변환 
data$heights  = c('120cm', '130cm', '140cm', '150cm', '160cm', '200cm') #벡터
)
str(data) #키가 문자로 수정됨

#숫자로 변환
# 1. 문자열 처리 'cm' 제거
# gsub: 문자에서 특정 패턴(cm)을 찾아 다른문자('')로 '대체'
data$heights = gsub('cm','',data$heights)
str(data) # 확인
data$heights = as.numeric(data$heights) #숫자로 형변환
str(data) # 확인
print(sum(data$heights)) #키 총합
