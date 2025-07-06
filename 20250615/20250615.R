scereal = read.csv('UScereal.csv')
#View(scereal)

#상관관계 시각화
#cor,correlation의 약자로 상관계수라는 뜻

#설탕과 칼로리의 상관관계
#complete.obs 결측값 처리
#round 반올림 처리
설탕과칼로리_상관관계 = cor(scereal$sugars, scereal$calories, use='complete.obs')
cat('설탕과칼로리_상관관계:', round(설탕과칼로리_상관관계,2),'\n')
#0.5:보통 상관관계
#-1~1: -1(음의 관계), 0(관계없음),1(양의관계)
#상관계수 해석: 0.3(이하) => 약한관계, 0.3~0.7 => 중간, 0.7(강한관계)

#install.packages('corrgram')
library(corrgram) # 설치한 프로그램 불러오기
library(dplyr)


테스트 = scereal %>% select(calories, protein,fat)
print(head(테스트)) #추출확인

corrgram(테스트,,main='칼로리, 단백질, 지방 상관관계 행렬', lower.panel = panel.shade,
         ,upper.panel = panel.cor,diag.panel = panel.minmax )


#Scereal 데이터에서 제조사(mfr)별로 평균 칼로리(calories), 평균 단백질(protein), 평균 식이섬유(fibre)를 구하고, 이 세 변수로 상관관계 행렬을 만들어 corrgram으로 

제조사별_데이터 = scereal %>% group_by(mfr) %>% summarise(cal_avg = mean(calories, na.rm= TRUE),pro_avg = mean(protein, na.rm=TRUE),fi_avg=mean(fibre,na.rm=TRUE))
corrgram(제조사별_데이터,main='제조사별 평균 칼로리, 단백질, 식이섬유 상관관계 행렬', lower.panel = panel.shade,
         ,upper.panel = panel.cor,diag.panel = panel.minmax)  

#데이터에서 나트륨(sodium), 식이섬유(fibre), 복합탄수화물(carbo), 칼륨(potassium) 컬럼을 선택하고, 결측치가 아닌 데이터만 corrgram으로 상관관계 시각화하시오.

결측치가_아닌_데이터 = scereal %>% filter(!is.na(sodium) & !is.na(fibre) & !is.na(carbo) & !is.na(potassium)) %>% select(sodium,fibre,carbo,potassium,fat,sugars)
corrgram(결측치가_아닌_데이터, main='나트륨,식이섬유,복합탄수화물,칼륨 상관관계 ', lower.panel = panel.shade,
         ,upper.panel = panel.cor)

상관관계 = cor(scereal$fibre, scereal$potassium, use='complete.obs')
print(상관관계)

