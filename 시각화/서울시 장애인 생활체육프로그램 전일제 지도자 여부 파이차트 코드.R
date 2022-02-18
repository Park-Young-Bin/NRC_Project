library(devtools)
library(ggrepel)
require(ggplot2)
require(moonBook)
require(webr)
library(reshape2)
library(showtext)
library(RColorBrewer)
font_add_google(name = "Noto Serif KR", family = "Noto Serif KR")
showtext_auto()


exercise <- data.frame(
  group = c("있다", "없다"),
  value = c(round(23/186*100,2), round((1-23/186)*100,2))
)
ggplot(exercise, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('서울시 장애인 생활체육프로그램 전일제 지도자 여부 (21.08.31. 기준)') +
  geom_text(aes(label= paste0(group, "(", value, "%", ")")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"))
format(23/186*100, nsmall = 1)

