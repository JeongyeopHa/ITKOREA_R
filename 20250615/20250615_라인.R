# economics 데이터셋 확인
print(head(economics))
# unemploy : 실업자 수
# pop: 전체 인구
# View(economics)
library(ggplot2)
# 시간(date)에 따른 실업자 수(unemploy)의 변화를 선 그래프
q= ggplot(data = economics, aes(x = date, y = unemploy)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Time Series of Unemployment in the US",
       x = "Year",
       y = "Number of Unemployed") +
  theme_minimal()

print(q)