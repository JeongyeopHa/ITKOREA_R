

scereal = read.csv('UScereal.csv')
View(scereal)

# 칼로리 vs 단백질

# 기본 그래프 산점도
plot(scereal$calories, scereal$protein, main = '칼로리 vs 단백질')

# (종속변수) ~ (독립변수)
# 독립변수(칼로리)가 변할 때 종속변수(단백질)가 어떻게 변화하는지 나타냄
# 칼로리가 높을수록 단백질 함량도 높아지는가?
# lm(선형회귀) 두 변수 사이의 관계를 '직선'으로 설명하는 분석 방법
model = lm(scereal$protein ~ scereal$calories)

print(summary(model))#model 요약하기
# p-value: 5.071e-11 -> 0.00000000005071
# ***** p-value(유의확률): 1700년도 영국에서 개념 도입 -> 1900년대 공식적 도입
# 과학, 의학, 사화과학 등 다양한 분야에서 유효하게 사용

# 유의확률은 통계 분석에서 '관계가 우연히 나타날 확률'을 의미
# 즉, 칼로리와 단백질 사이에 실제로 아무런 관계가 없는데도, 
# 우연히 지금처럼 강한 관계를 관찰될 확률을 나타냄

# p-value가 0.05 보다 작거나 같으면 '칼로리와 단백질 사이에 통계적으로 의미 있는 관계가 있다'고 볼 수 있습니다.

# 매우 강하게, 우연이 아니라 실제로 강한 관계다 라고 결론을 내릴 수 있습니다.


abline(model, col = 'red')