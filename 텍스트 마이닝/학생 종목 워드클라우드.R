library(KoNLP)
library(openxlsx)
library(RColorBrewer)
library(dplyr)
library(wordcloud2)

# 데이터 불러오기
student <- read.xlsx("C:/R_pjt/public/데이터/학생 종목 데이터.xlsx")

# 중복 행 제거
student$sum <- student$지체 + student$시각 + student$청각 + student$지적 + student$뇌병변
student[7,8] <- 13
student <- student[-8, ]

# 필요한 변수 추출
cnt <- student %>% select(종목, sum)
cnt

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
           size = 0.4, 
           fontFamily = "나눔바른고딕", 
           shape = 'diamond', 
           color = brewer.pal(9, "RdYlBu"))
