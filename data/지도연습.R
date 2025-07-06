# 교육_지출_총금액 지도로 시각화 하기

library(sf) # 공간 데이터를 처리하고 시각화하기 위한 패키지(프로그램)
library(ggplot2)
#install.packages('ggiraph')
library(ggiraph)

# 1. 지도 파일 불러오기
shp = 'sig.shp' 
korea_map = st_read(shp, quiet = TRUE)
str(korea_map)
#행정구역 코드(SIG_CD) ex) 종로구 -> 11110



library(dplyr)
# 서울 행정구역만 조회
seoul_map = korea_map %>% filter(substr(SIG_CD,1,2) == '11')
str(seoul_map)
print(colnames(korea_map)) #

#csv 파일 불러오기
서울시_상권분석_데이터 = read.csv('서울시_상권분석서비스.csv', na.strings = c(""), fileEncoding = 'CP949', encoding = 'UTF-8', check.names = FALSE)

서울시_상권분석_데이터$행정동_코드 = as.character(서울시_상권분석_데이터$행정동_코드) #타입 변환

# 3가지 확인하기
str(서울시_상권분석_데이터)
print(head(서울시_상권분석_데이터))
print(colSums(is.na(서울시_상권분석_데이터)))

# csv에 있는 행정구역코드와 shp(지도파일)에 있는 행정구역 코드 데이터타입이 서로 다르다.
# 두 파일을 병합하기 위해서는 교집합컬럼(행정구역코드) 데이터타입이 서로 동일해야됨.

서울시_상권분석_데이터$행정동_코드 = as.character(서울시_상권분석_데이터$행정동_코드) #타입 변환
# => 숫자에서 문자로 변환


options(scipen = 999)
print(summary(서울시_상권분석_데이터$교육_지출_총금액))
merged_data = inner_join(seoul_map, 서울시_상권분석_데이터, by = c('SIG_CD' = '행정동_코드' )) %>% group_by(SIG_CD, 행정동_코드_명) %>% summarise(교육지출총금액 = mean(교육_지출_총금액, na.rm = TRUE)) %>% select(SIG_CD, 행정동_코드_명, 
                                                                                                                                                                                 교육지출총금액) %>% arrange(desc(교육지출총금액)) %>% head(5)

#View(merged_data)
result = ggplot(merged_data) + scale_fill_gradient(low = '#ececec', high = 'blue', name = '교육지출총금액') + geom_sf_interactive(aes(fill = 교육지출총금액, tooltip = 행정동_코드_명, data_id = SIG_CD)) + labs(title = '서울시 22~25년 교육지출총금액', x ='경도', y='위도') + theme_minimal()
giraph = girafe(ggobj = result)
print(giraph)