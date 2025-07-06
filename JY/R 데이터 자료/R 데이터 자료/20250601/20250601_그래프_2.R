# 산점도 
# 변수 간 관계를 시각화
# 예) 키와 몸무게 관계, 온도와 에너지 소비량 관계, 흡연과 건강결과 관계
x = c(1,4,2,6,10,15)
y = c(2,3,6,6,10,2)

# pch : 점 크기
plot(x, y, main = '산점도 예시', xlab = 'x값', ylab = 'y값', col= 'blue',pch = 20)
# 회귀선 추가
# linear model : 선형 모델
model = lm(y ~ x) # 선형 모델 생성
# abline : add a line
abline(model, col = 'red', lwd = 2)

### 데이터 스케일링
# 데이터 스켈일링은 전처리 방법 중 하나
# 분석과 머신러닝에서 중요한 과정입니다. 

# 데이터프레임 생성
data = data.frame(
  height_cm = c(150, 160, 170, 180, 190), # 키(cm)
  weight_kg = c(50, 60, 70, 80 ,90) # 몸무게(kg)
)
# 수치를 통일
# min-max 라는 기법을 통해서 분석할 데이터를 0과 1사이로 변환
# 즉 모든 데이터는 0과 1사이에 존재함.

# 암기 
# 스케일링 = 기존값 - 최솟값 / 최댓값 - 최솟값
height_min = min(data$height_cm) #키 최솟값
height_max = max(data$height_cm) #키 최댓값

#스케일링 결과 컬럼 추가
data$height_sacled = (data$height_cm - height_min) / (height_max - height_min)

# 몸무게 스케일링해서 결과확인
weight_kg_min = min(data$weight_kg) #몸무게 최솟값
weight_kg_max = max(data$weight_kg) #몸무게 최댓값

#스케일링 결과 컬럼 추가
data$weight_sacled = (data$weight_kg - weight_kg_min) / (weight_kg_max - weight_kg_min)










