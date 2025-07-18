
library(dplyr)
서울시_상권분석_데이터 = read.csv('서울시_상권분석서비스.csv', na.strings = c(""), fileEncoding = 'CP949', encoding = 'UTF-8', check.names = FALSE)

# 1. 데이터 타입 조회
# 2. 컬럼이름만 조회
# 3. 컬럼별 결측값 수 조회
# 4. 상위 10개 데이터 조회
# 5. 교육_지출_총금액이 가장 큰 행정자치구 찾기
# 교육 지출 금액 최댓값
교육_지출_max = max(서울시_상권분석_데이터$교육_지출_총금액, na.rm = TRUE)
print(교육_지출_max)
교육_지출_행정자치구 = 서울시_상권분석_데이터 %>% filter(교육_지출_총금액 == 교육_지출_max) %>% select(행정동_코드, 행정동_코드_명)
print(교육_지출_행정자치구) 

교육_지출_min = min(서울시_상권분석_데이터$교육_지출_총금액, na.rm = TRUE)
print(교육_지출_min)
교육_지출_행정자치구2 = 서울시_상권분석_데이터 %>% filter(교육_지출_총금액 == 교육_지출_min) %>% select(행정동_코드, 행정동_코드_명)
print(교육_지출_행정자치구2) 
# 6. 25년도 데이터 전체 조회

서울시_25년도 = 서울시_상권분석_데이터 %>% filter(substr(기준_년분기_코드,1,4) == '2025')



# 7. 25년 데이터 지출 총금액 대비 음식 지출 비율 구하기 (음식지출/총금액) * 100
서울시_25년도 = 서울시_25년도 %>% mutate(식비비율 = (음식_지출_총금액) * 100)
#print(head(서울시_25년도))

str(서울시_상권분석_데이터)
colnames(서울시_상권분석_데이터)
print(colSums(is.na(서울시_상권분석_데이터)))
print(head(서울시_상권분석_데이터,10))
print(summary(서울시_상권분석_데이터$식료품_지출_총금액))
barplot(서울시_25년도$교육_지출_총금액, names.arg = 서울시_25년도$행정동_코드_명, col = 'orange', xlab = '행정동명', ylab = '소득금액', main = '25년 행정동별 교육지출')

#강남구

#View(서울시_25년도)

# 8. 25년 데이터 중 생활용품지출, 의류_신발지출, 교통_지출, 유흥지출 상관계수 조회

cor_mat = cor(서울시_25년도[,c('생활용품_지출_총금액','의류_신발_지출_총금액','교통_지출_총금액','유흥_지출_총금액')])
print(cor_mat) #상관계수 행렬 조회


# 24년도 구로구 데이터 조회

서울_구로_24년 = 서울시_상권분석_데이터 %>% filter(substr(기준_년분기_코드,1,4) == '2024'& 행정동_코드_명='구로구')


