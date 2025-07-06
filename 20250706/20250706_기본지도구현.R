# 지도 시각화 구현
# 필요한 패키지 설치
# install.packages('sf') # 공간 데이터를 처리하고 시각화하는 프로그램(패키지)
library(sf) # 공간 데이터를 처리하고 시각화하기 위한 패키지(프로그램)
library(ggplot2)

#R 경로확인
print(getwd())
# SHP파일 불러오기
# SHP파일? 공간데이터를 저장하고 표현하는데 사용하는 파일.
shp = 'sig.shp' #파일이름
korea_map = st_read(shp, quiet = TRUE)
#print(korea_map)

library(dplyr)
# substr : 특정 위치의 부분 문자열 출력
# 서울은 11로 시작
seoul_map = korea_map %>% filter(substr(SIG_CD,1,2) == '11')


result = ggplot(seoul_map) + geom_sf(fill = 'white', color = 'black') + labs(x= '경도', y='위도', title ='서울시 행정구역') + 
   coord_sf() + theme_minimal() #회색 배경제거

print(result)