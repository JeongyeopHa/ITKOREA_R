#install.packages('ggplot2') 설치는 한번만
library(ggplot2) # 설치한 ggplot2 불러오기

scereal = read.csv('UScereal.csv')

#기본 산점도 표현 칼로리 vs 단백질
# aes : aesthetics (미적 속성)
# labs: labels
# lm : linear model (선형모델)
# se : 회귀선에서 신뢰구간 표시
plot = ggplot(scereal,aes(x = calories, y= protein)) + 
  geom_point() + labs(title='칼로리 vs 단백질', x='칼로리', y='단백질') + geom_smooth(method='lm', se= TRUE) + theme_minimal()
# print(plot)

scereal$label = rownames(scereal)
# View(scereal) #시퀀스 확인


# 산점도 그래프에 텍스트 추가
# check_overlap: 점이 겹칠 경우 일부 라벨을 표시하지 않도록 합니다.

plot = ggplot(scereal,aes(x = calories, y= protein)) + 
  geom_point() + labs(title='칼로리 vs 단백질', x='칼로리', y='단백질') + geom_smooth(method='lm', se= TRUE) + geom_text(aes(label = label), hjust=0, vjust=1, size=3, check_overlap = TRUE) + theme_minimal()
print(plot)

# 칼로리가 낮은 제품은 Good, 높은 제품은 Bad를 구분하는 새로운 열을 추가한다.
# 칼로리가 평균보다 낮으면 Good, 아니면 Bad
# 새로운 열 'grade' 추가

칼로리_평균 = mean(scereal$calories, na.rm = TRUE)
cat('칼로리 평균 : ', 칼로리_평균, '\n')

scereal$grade = ifelse(scereal$calories <= 칼로리_평균, 'Good',"Bad")
print(head(scereal))

# 상관계수

칼로리_단백질_상관계수 = round(cor(scereal$calories, scereal$protein, use='complete.obs'),2)
cat('칼로리와 단백질 상관계수 : ', 칼로리_단백질_상관계수, '\n')

#Good이면 동그라미, Bad면 세모표시
plot = ggplot(scereal,aes(x = calories, y= protein, shape = grade, color = grade)) + 
  geom_point() + 
  annotate('text', x = 250, y = 15,label = paste0('단백질 vs 칼로리 상관계수:', 칼로리_단백질_상관계수), color = 'black', size = 5
  ) +  
  labs(title='칼로리 vs 단백질', x='칼로리', y='단백질') + 
  scale_shape_manual(values = c("Good" = 16, "Bad" = 17)) + 
  scale_color_manual(values = c("Good" = "blue", "Bad" = "red")) + 
  geom_smooth(method='lm', se= TRUE) + 
  geom_text(aes(label = label), hjust=0, vjust=1, size=3, check_overlap = TRUE) + 
  theme_minimal()
print(plot)

#ggplot에서는 ggsave라는 명령어로 쉽게 저장할 수 있습니다.
# dpi = 해상도까지 조절가능
ggsave('myplot.pdf', width = 10, height =6, dpi=300)



