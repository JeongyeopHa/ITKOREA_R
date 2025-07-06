#1. emp.csv 불러오기
emp=read.csv('emp.csv')
str(emp)

cat('평균 급여 :', mean(emp$SAL), '\n')

#문제 1981-01-01 이후 입사한 직원 이름, 입사일, 급여, 근무연수 조회
emp$HIREDATE = as.Date(emp$HIREDATE) #날짜 -> 문자 형변환

오늘날짜 = Sys.Date()                                                                                                         
result = emp %>% filter(HIREDATE > as.Date('1981-01-01')) %>% select(ENAME,HIREDATE,SAL) %>% mutate(근무연수 = as.numeric(오늘날짜- HIREDATE) / 365)

print(result)

print(dim(emp))
print(colnames(emp))
print(head(emp,2))
print(tail(emp,3))
cat('사원 급여 평균:', mean(emp$SAL), '\n')
cat('사원 급여 최댓값:', max(emp$SAL), '\n')
cat('사원 급여 최솟값:', min(emp$SAL), '\n')
print(sum(emp$COMM, na.rm= TRUE))
cat('사원이름:', emp$ENAME, '\n')

# 문제 10: 각 사원의 근속일을 구해서 근속일(WORKDAY) 컬럼을 추가하기
오늘날짜 = Sys.Date()
print(오늘날짜)

# diff+time
# $ : 접근도 가능하고 추가도 가능함
emp$WORKDAY = difftime(오늘날짜, emp$HIREDATE) # difftime(오늘날짜, emp$HIREDATE) -> (오늘날짜 - 입사날짜)

# 문제 12 : 조건문을 활용해서 급여가 평균보다 높은 사원은 High 낮은사람은 Low를 나타내는 컬럼 'Grade' 추가
emp$Grade = ifelse(emp$SAL > mean(emp$SAL),'HIGH','LOW')
emp$korea = 'korea'
  
View(emp)
