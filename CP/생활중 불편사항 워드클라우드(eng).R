library(openxlsx)
library(dplyr)
library(RColorBrewer)
display.brewer.all()
display.brewer.all()

df <- read.xlsx('data/생활중 불편사항(eng).xlsx')

# 8. 워드클라우드 생성
# 폰트 설정
library(showtext)
font_add_google(name = "Do Hyeon", family = "Do Hyeon")
showtext_auto()


library(ggwordcloud)
ggplot(df, aes(label = eng, size = Freq, col = Freq)) +
  geom_text_wordcloud(seed = 1234, family = "Do Hyeon") +
  scale_radius(limits = c(3, NA),    # 최대, 최소 단어 빈도
               range = c(4, 16)) +   # 최소, 최대 글자 크기
  scale_color_gradient(low = "#90c1f5", high = "#074282") +
  theme_minimal()
