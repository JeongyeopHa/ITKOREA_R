# 박스플롯 : 데이터의 분포를 시각적 표현, 최솟값, 최댓값, 중앙값 등 요약 통계치를 보여줍니다.

# 제조사별 칼로리 분포를 박스플롯
library(ggplot2)

scereal = read.csv('UScereal.csv')


boxplot = ggplot(scereal, aes(x = mfr, y = calories, fill = mfr)) + geom_boxplot() + labs(title = "제조사별 칼로리 분포", x = "제조사", y = "칼로리") + theme_minimal() + theme(panel.grid = element_blank())

print(boxplot)

#뒤에 회색 배경 제거 : theme_minimal()
#뒤에 바둑판(그리드) 제거 : them panel.grid = element_blank

# 제조사(mfr)별 나트륨(sodium) 분포 박스플롯
boxplot = ggplot(scereal, aes(x = mfr, y = sodium, fill = mfr)) +
  geom_boxplot() +
  labs(title = "제조사별 칼로리 분포", 
       x = "제조사",
       y = "칼로리") +
  theme_minimal() + #뒤에 회색배경 제거
  scale_y_continuous(breaks = seq(0, max(scereal$sodium, na.rm = TRUE), by = 30)) + 
  theme(panel.grid = element_blank()) #뒤에 바둑판(그리드) 제거
print(boxplot)
