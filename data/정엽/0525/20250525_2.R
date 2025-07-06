setwd('C:/Users/admin/Desktop/r_workspaces/data') #폴더(디렉토리) 변경
print(getwd()) #디렉토리 경로 확인
print(list.files()) #현재 디렉토리의 파일 목록 출력


#emp.csv 파일을 불러오겠습니다.
emp = read.csv('emp.csV') #데이터프레임 불러오기
#View(emp)#확인

# 문제 1 : 행과 열의 개수 파악
# 문제 2 : 전체 컬럼만 조회
# 문제 3 : 데이터 상위 1~4행 출력하기
# 문제 4 : 데이터 마지막 3행을 출력하기
# 문제 5 : 데이터 타입 확인 ******* 암기 필

print(colnames(emp))
print(dim(emp))
print(head(emp))
print(head(emp,4))
print(tail(emp,3))
str(emp) # 각 벡터들이 숫자(int)인지? 문자(chr)인지? 날짜형인지?

### dplyr(디플리알) : data frame plier의 줄임말 '데이터 프레임을 다루는 공구'
# 실무에서 자주 사용되고, 특히 대규모 데이터셋에서 빠른 속도를 제공합니다.

#install.packages('dplyr') #설치 명령어 -> 설치 후 주석처리
library(dplyr) #설치된 디플리알 불러오기

#문제. 급여가 3000이상인 사원조회
# emp$ 필요없음
# 방향 표현(실무에선, 람다식 표현)
SAL3000 = emp %>% filter(SAL>=3000)
print(SAL3000)
#급여가 3000이상인 사원 이름, 급여, 사원번호 조회
SAL3000NAME= emp %>% filter(SAL>=3000) %>% select(ENAME, SAL, empNO)
print(SAL3000NAME)
# 사원직책(JOB)이 MANAGER인 사원의 이름,직책,부서번호,급여 조회
result = emp %>% filter(JOB=='MANAGER') %>% select(ENAME,JOB,DEPTNO,SAL)
print(result)
# 새로운 열 추가 & 수정
# mutate : 변화하다
# TOTAL_COMM 새로운 열 이름
# SAL + 100 : 직원급여 + 100
결과 = emp %>% mutate(TOTAL_COME=SAL+100)
#View(결과)
#  급여와 커미션의 합계를 TOTAL_COMM 에 수정
# ifelse(is.na(COMM),0,COMM)) ****** 자주보기
결과 = emp %>% mutate(TOTAL_COME=SAL+ ifelse(is.na(COMM),0,COMM))
#View(결과)

# group by + summarize
# 데이터를 특정 기준으로 묶어 그룹화 합니다.
# 단, summarize (요약하다)와 함께 사용합니다.

#. 직책별(JOB) 평균 급여
group_result = emp %>% group_by(JOB) %>% summarize(AVG_SAL = mean(SAL))
print(group_result)
# 부서번호별 평균급여
group_result = emp %>% group_by(DEPTNO) %>% summarize(AVG_SAL = mean(SAL))
print(group_result)
# 부서별(DEPTNO) 최소,최대,평균 급여 조회
dept_result = emp %>% group_by(DEPTNO) %>% summarize(MIN_SAL = min(SAL), MAX_SAL = max(SAL), AVG_SAL = mean(SAL))
print(dept_result)
#  직책별 직원 수 조회
# n() : 각 그룹의 행 개수 계산
job_count = emp %>% group_by(JOB) %>% summarise(COUNT = n())
print(job_count)

# 정렬 arrange (조정하다, 배열하다)
# arrange(COUNT): COUNT 기준으로 오름차순
# 오름차순
job_count = emp %>% group_by(JOB) %>% summarise(COUNT = n()) %>% arrange(COUNT)
print(job_count)

#내림차순
#desc:Descending(데이터를 내림차순)
job_count = emp %>% group_by(JOB) %>% summarise(COUNT = n()) %>% arrange(desc(COUNT))
print(job_count)

# 급여 기준으로 내림차순 정렬 (급여하고 이름만 출력)
# 항상 정렬은 마지막에 진행
job_count = emp %>% select(ENAME,SAL) %>% arrange(desc(SAL))
print(job_count)

#부서별 최대 급여 직원 조회
# ~별 -> group by!

Departking = emp %>% group_by(DEPTNO) %>% slice_max(SAL, n = 1)
print(Departking)

# 박스플롯
# boxplot(emp$SAL ~ emp$DEPTNO, main = '부서별 급여', xlab = '부서번호', ylab = '급여', col = c('orange', 'green', 'lightblue'))

# 근속연수일 컬럼 추가
# HIREDATE 문자에서 날짜형으로 형변환
str(emp)
#방법 1
emp = emp %>% mutate(HIREDATE = as.Date(HIREDATE))
#방법 2
#emp$HIREDATE = as.DATE(emp$HIREDATE)
str(emp)
date = Sys.Date()
result = emp %>% mutate(근속일 = difftime(date, HIREDATE))
#View(result)

#문제. 급여가 2000 이상인 직원 중 세후 급여(SAL_TAX) 컬럼 추가하기.
#단, 사원이름, 급여만 조회, 급여 내림차순까지
#mutate(컬럼 추가하거나 컬럼 수정할 때)
# 3.3% 원천징수
result = emp %>% filter(SAL >= 2000) %>% select(ENAME,SAL) %>% mutate(SAL_tax = SAL *0.967) %>% arrange(desc(SAL))
print(result)

#문제. 부서번호가 30인 직원 중 이름, 직업, 부서번호만 조회 단, 이름 오름차순 정렬
result = emp %>% filter(DEPTNO == 30) %>% select(ENAME,JOB,DEPTNO)  %>% arrange(ENAME)
print(result)

#문제 1981-01-01 이후 입사한 직원 이름, 입사일, 급여, 근무연수 조회
str(emp) # 데이터타입 확인

result = emp %>% filter(HIREDATE > as.DATE('1981-01-01')) %>% select(ENAME,HIREDATE,SAL) %>% mutate(근무연수 = as.numeric(오늘날짜-HUREDATE)/365)
print(result)

오늘날짜 = Sys.date()

  

