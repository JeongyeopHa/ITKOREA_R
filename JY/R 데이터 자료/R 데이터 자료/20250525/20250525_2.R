setwd('C:/Users/admin/Desktop/r_workspaces/data') #폴더(디렉토리) 변경
print(getwd()) #현재 디렉토리 경로 확인
print(list.files()) #현재 디렉토리의 파일 목록 출력

# emp.csv 파일을 불러오겠습니다.
emp = read.csv('emp.csv') #데이터프레임 불러오기
# View(emp)#확인
# 문제 1: 행과 열의 개수 파악
print(dim(emp))
# 문제 2: 전체 컬럼만 조회
print(colnames(emp))
# 문제 3: 데이터 상위 1~4행 출력하기
print(head(emp,4))
# 문제 4: 데이터 마지막 3행을 출력하기
print(tail(emp,3))
# 문제 5: ****데이터 타입 확인 (암기 必)
str(emp) #각 벡터들이 숫자인지? 문자인지? 날짜형인지? 

### dplyr(디플리알) : data frame plier의 줄임말 '데이터 프레임을 다루는 공구'
# 실무에서 자주 사용되고, 특히 대규모 데이터셋에서 빠른 속도를 제공합니다.

# install.packages('dplyr') #설치 명령어 -> 설치 후 주석처리
library(dplyr) #설치된 디플리알 불러오기

# 문제. 급여가 3000이상인 사원조회
# 방향 표현(실무에선, 람다식 표현)
급여3000 = emp %>% filter(SAL >= 3000)
print(급여3000)
# 급여가 3000이상인 사원 이름, 급여, 사원번호 조회
급여3000이름 = emp %>% filter(SAL >= 3000) %>% select(ENAME, SAL, empNO)
print(급여3000이름)
# 사원직책(JOB)이 MANAGER인 사원의 이름,직책,부서번호,급여 조회
결과 = emp %>% filter(JOB == 'MANAGER') %>% select(ENAME, JOB, DEPTNO, SAL)

# 새로운 열 추가& 수정
# mutate : 변화하다
# TOTAL_COMM 새로운 열 이름
# SAL + 100 : 직원급여 + 100
결과 = emp %>% mutate(TOTAL_COMM = SAL + 100)
# View(결과) # 결과 확인

# 급여와 커미션의 합계를 TOTAL_COMM에 수정 ***
결과 = emp %>% mutate(TOTAL_COMM = SAL + ifelse(is.na(COMM), 0, COMM))
# View(결과) # 결과확인

# group by + summarize
# 데이터를 특정 기준으로 묶어 그룹화 합니다.
# 단, summarize(요약하다)와 함께 사용합니다.

# 직책별(JOB) 평균 급여
group_result = emp %>% group_by(JOB) %>% summarize(AVG_SAL = mean(SAL))
print(group_result)
# 부서번호(DEPTNO)별 평균급여
dept_result = emp %>% group_by(DEPTNO) %>% summarize(AVG_SAL = mean(SAL))
# 부서별(DEPTNO) 최소,최대,평균 급여 조회
dept_result = emp %>% group_by(DEPTNO) %>% 
  summarize(MIN_SAL = min(SAL), MAX_SAL = max(SAL), AVG_SAL = mean(SAL))
print(dept_result)
# 직책별 직원 수 조회
# n() : 각 그룹의 행 개수 계산
job_count = emp %>% group_by(JOB) %>% summarise(COUNT = n())
print(job_count)

# 정렬 arrange(조정하다, 배열하다)
# arrange(COUNT) : COUNT 기준으로 오름차순
# 오름차순
job_count = emp %>% group_by(JOB) %>% summarise(COUNT = n()) %>% 
  arrange(COUNT) 
print(job_count)
# 내림차순
# desc : Descending(데이터를 내림차순)
job_count = emp %>% group_by(JOB) %>% summarise(COUNT = n()) %>% 
  arrange(desc(COUNT)) 
print(job_count)
# 급여 기준으로 내림차순 정렬 (급여하고 이름만 출력)
# 항상 정렬은 마지막에 진행
급여내림차순 = emp %>% select(ENAME,SAL) %>% arrange(desc(SAL))
print(급여내림차순)
# 부서별 최대 급여 직원 조회
# ~별 -> group by!
# slice : 자르다
부서별급여킹 = emp %>% group_by(DEPTNO) %>%slice_max(SAL, n = 1)
print(부서별급여킹)

# 박스플롯
# boxplot(emp$SAL ~ emp$DEPTNO, main = '부서별 급여', xlab = '부서번호', ylab = '급여',
#         col = c('orange', 'green', 'lightblue'))

#문제. 근속연수일 컬럼 추가

오늘날짜 = Sys.Date() #현재 날짜 조회
str(emp) # 데이터 프레임 데이터 확인
# HIREDATE 문자에서 날짜형으로 형변환
#방법 1
emp = emp %>% mutate(HIREDATE = as.Date(HIREDATE)) # 형변환
#방법 2
# emp$HIREDATE = as.Date(emp$HIREDATE)
str(emp) # 꼭 꼭 확인
결과 = emp %>% mutate(근속일 = difftime(오늘날짜, HIREDATE))
# View(결과)

#문제. 급여가 2000 이상인 직원 중 세후 급여(SAL_TAX) 컬럼 추가하기.
#단, 사원 이름, 급여만 조회, 급여 내림차순까지

# 3.3% 원천징수
# mutate (컬럼 추가하거나 컬럼 수정할 때)
결과 = emp %>% filter(SAL >= 2000) %>% select(ENAME,SAL) %>% 
  mutate(SAL_TAX = SAL * 0.967) %>% arrange(desc(SAL))
print(결과)

# 문제. 부서번호가 30인 직원 중 이름, 직업, 부서번호만 조회 단, 이름 오름차순 정렬
결과 = emp %>% filter(DEPTNO == 30) %>% select(ENAME, JOB, DEPTNO) %>%
  arrange(ENAME)
print(결과)

# 문제 1981-01-01 이후 입사한 직원 이름, 입사일, 급여, 근무연수 조회
str(emp) # 데이터타입 확인

결과 = emp %>% filter(HIREDATE > as.Date('1981-01-01')) %>%
  select(ENAME, HIREDATE, SAL) %>%
  mutate(근무연수 = difftime(오늘날짜, HIREDATE) / 365) #날짜 -> 숫자
print(결과)
  




