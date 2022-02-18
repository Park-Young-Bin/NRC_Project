library(openxlsx)
library(reshape2)
library(tidyverse)

# 데이터 불러오기
data <- read.xlsx('data/불편사항(보조기기) 응답.xlsx')

ggplot(data, aes(x = reorder(불편사항, -빈도), y = 빈도)) +
  geom_col() +
  geom_text(aes(label=빈도), vjust=1.5,color="white",fontface="bold", size=5) +   # 데이터 레이블 지정
  theme_classic()+
  ggtitle("불편사항(보조기기) 응답") + 
  ylab('빈도(건수)') +
  xlab('불편사항')+
  theme(plot.title = element_text(face = 'bold',
                                  size = 20,
                                  hjust = .5))


  