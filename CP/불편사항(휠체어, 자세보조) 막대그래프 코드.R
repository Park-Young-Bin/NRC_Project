
library(dplyr)
library(ggplot2)
library(openxlsx)
library(esquisse)
library(tidyverse)


data <- read.xlsx('불편사항(휠체어,자세보조).xlsx')
data
colnames(data)=c('불편사항','빈도')

ggplot(data, aes(x = reorder(불편사항, -빈도), y = 빈도)) +   #빈도 내림차순

  geom_col(fill = "#112446") +  #바 색상 지정 
  theme_gray() + # 그래프 테마 설정 
  geom_text(aes(label=빈도), vjust=1.5,color="white",fontface="bold", size=5) +   # 데이터 레이블 지정
  xlab('불편사항') + # x축 제목 
  scale_y_continuous(breaks=seq(0, 70, 10))+   #y축 스케일 
  ggtitle('불편사항 응답') + # 그래프 제목
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 30,
                                  hjust = 0.5)) +
  theme(axis.title=element_text(size=20)) +     #축 타이틀
  theme(axis.text.x = element_text(size=18)) +   #x축 텍스트
  theme(axis.text.y = element_text(size= 20))    #y축 테스트


