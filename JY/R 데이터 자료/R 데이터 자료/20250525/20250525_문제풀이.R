# 1. emp.csv 불러오기
emp = read.csv('emp.csv')
str(emp) # 데이터프레임 구조 확인

# 문제 19: 매니저(MGR)가 없는 직원과, (매니저(MGR)가 있는 직원 중 급여가 2,000 이상)인 직원만 추출

# 매니저(MGR)가 없는 직원
# NA는 매니저가 없음 -> is.na?
# 매니저(MGR)가 있는 직원 -> !is.na?
결과 = emp %>% filter(is.na(MGR) | (!is.na(MGR) & SAL >= 2000))
