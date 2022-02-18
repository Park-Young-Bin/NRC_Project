## 종목 데이터 워드클라우드

# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("openxlsx")
# install.packages("RColorBrewer")
# install.packages("wordcloud2")
library(KoNLP)
library(openxlsx)
library(RColorBrewer)
library(wordcloud2)

# 파일 불러오기
t1<- read.table(file = "종목.txt")
t1

# 리스트 형식 해제
t2  <- unlist(t1)
t2

# 빈도수 내림차순 정렬
t3 <- sort(table(t2), decreasing = T)
t3

# 초기 난수 설정
set.seed(123)

# 워드클라우드 생성
wordcloud2(t3, 
           size = 1, 
           fontFamily = "나눔바른고딕", 
           shape = 'diamond', 
           color = brewer.pal(9, "RdYlBu"))

# 색상표 출력
display.brewer.all()

# 색 지정
brewer.pal(9, "Set3")

# Viewer 초기화
dev.off()
