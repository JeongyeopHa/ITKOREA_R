scereal = read.csv('UScereal.csv')
설탕과칼로리_상관관계 = cor(scereal$sugars, scereal$calories, use='complete.obs')
cat('설탕과칼로리_상관관계:', round(설탕과칼로리_상관관계,2),'\n')

library(corrgram)
library(dplyr)

테스트 = scereal %>% select(calories, protein)
print(head(테스트))

corrgram(테스트,main='칼로리,단백질,지방 상관관계 행렬', lower.panel = panel.shade,upper.panel = panel.cor, diag.panel = panel.minmax)

#Scereal 데이터에서 제조사(mfr)별로 평균 칼로리(calories), 평균 단백질(protein), 평균 식이섬유(fibre)를 구하고, 이 세 변수로 상관관계 행렬을 만들어 corrgram으로 
시각화 하시오

제조사별_데이터 = scereal %>% group_by(mfr) %>% summarise(cal_avg = mean(calories, na.rm= TRUE),pro_avg = mean(protein, na.rm=TRUE),fi_avg=mean(fibre,na.rm=TRUE))
corrgram(제조사별_데이터,main='제조사별 평균 칼로리, 단백질, 식이섬유 상관관계 행렬', lower.panel = panel.shade,
         ,upper.panel = panel.cor,diag.panel = panel.minmax)  