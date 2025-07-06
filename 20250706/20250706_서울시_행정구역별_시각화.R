library(sf) # 공간 데이터를 처리하고 시각화하기 위한 패키지(프로그램)
library(ggplot2)
#install.packages('ggiraph')
library(ggiraph) #지도 이벤트 패키지


shp = 'sig.shp' 
korea_map = st_read(shp, quiet = TRUE)

library(dplyr)
seoul_map = korea_map %>% filter(substr(SIG_CD,1,2) == '11')
str(seoul_map)



#csv 
서울시_상권분석_데이터 = read.csv('서울시_상권분석서비스.csv', na.strings = c(""), fileEncoding = 'CP949', encoding = 'UTF-8', check.names = FALSE)

# shp파일에 있는 행정동 코드와 서울시 행정동 코드 데이터타입이 서로 다르다.

서울시_상권분석_데이터$행정동_코드 = as.character(서울시_상권분석_데이터$행정동_코드) #타입 변환
str(서울시_상권분석_데이터)
print(head(서울시_상권분석_데이터))

# 각 컬럼별 결측값 조회
print(colSums(is.na(서울시_상권분석_데이터)))
summary(서울시_상권분석_데이터)
options(scipen = 999) # 10진수 표현
print(summary(서울시_상권분석_데이터$음식_지출_총금액))

#서울시 각 행정구 월 평균 소득 금액을 지도로 시각화
# 1. shp 파일과 서울시 CSV 파일을 병합.
# 2. 교집합 컬럼이 필요함.

# iner_join : 두 파일을 병합
# 22년 ~ 25년까지 데이터가 존재해서 group_by로 각 자치구별 평균소득 조회

merged_data = inner_join(seoul_map, 서울시_상권분석_데이터, by = c('SIG_CD' = '행정동_코드' )) %>% group_by(SIG_CD, 행정동_코드_명) %>% summarise(월평균소득 = mean(월_평균_소득_금액, na.rm = TRUE)) %>% select(SIG_CD, 행정동_코드_명, 월평균소득)
# View(merged_data)

# 지도 시각화

result = ggplot(merged_data) + scale_fill_gradient(low = '#ececec', high = 'blue', name = '월평균소득') + geom_sf_interactive(aes(fill = 월평균소득, tooltip = 행정동_코드_명, data_id = SIG_CD)) + labs(title = '서울시 22~25년 월평균 소득', x ='경도', y='위도') + theme_minimal()

giraph = girafe(ggobj = result)

print(giraph)





