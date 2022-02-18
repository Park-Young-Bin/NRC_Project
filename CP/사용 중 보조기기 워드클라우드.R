library(KoNLP)
library(openxlsx)
library(RColorBrewer)
library(wordcloud2)

# 데이터 불러오기
word=read.xlsx("C:/Users/user/OneDrive/바탕 화면/사용중인 보조기기.xlsx")
a=rep(word$보조기기,word$빈도)
a

# 리스트 형식 해제
a2  <- unlist(a)
a2

# 빈도수 내림차순 정렬
a3 <- sort(table(a2), decreasing = T)
a3

# 초기 난수 설정
set.seed(1243)

# 워드클라우드 생성
  wordcloud2(a3, 
           size = 1, 
           fontFamily = "나눔바른고딕", 
           shape = 'diamond', 
           color = brewer.pal(9, "Paired"))



