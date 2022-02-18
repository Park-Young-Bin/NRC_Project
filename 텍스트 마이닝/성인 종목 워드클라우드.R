library(KoNLP)
library(openxlsx)
library(RColorBrewer)
library(dplyr)
library(wordcloud2)

# 데이터 불러오기
adult <- read.xlsx("C:/R_pjt/public/데이터/성인 종목 데이터.xlsx")

# 건수 변수 생성 및 필요한 변수 추출
cnt <- adult %>% 
  mutate(sum =  지체 + 시각 + 청각 + 지적 + 뇌병변) %>% 
  select(종목, sum)

# 중복 행 수정 및 제거
cnt[4, 2] <- 17
cnt <- cnt[-9,]

# 개수만큼 반복하여 변수에 저장
a <- rep(cnt$종목, cnt$sum)

# 리스트 형식 해제
a2  <- unlist(a)
a2

# 빈도수 내림차순 정렬
a3 <- sort(table(a2), decreasing = T)
a3

# 초기 난수 설정
set.seed(123)

# 워드클라우드 생성
wordcloud2(a3, 
           size = 1, 
           fontFamily = "나눔바른고딕", 
           shape = 'diamond', 
           color = brewer.pal(9, "RdYlBu"))
