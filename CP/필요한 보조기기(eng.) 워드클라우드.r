library(openxlsx)
library(dplyr)

needs=read.xlsx('필요한 보조기기(eng.).xlsx')
needs=needs[,-1]
needs
df <- as.data.frame(needs)
new_df <- df %>% group_by(needs) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# 워드클라우드 생성
# 폰트 설정
library(showtext)
font_add_google(name = "Do Hyeon", family = "Do Hyeon")
showtext_auto()

library(ggwordcloud)

ggplot(new_df, aes(label = needs, size = count, col = count)) +
  geom_text_wordcloud(seed = 1234, family = "Do Hyeon") +
  scale_radius(limits = c(2, NA),    # 최대, 최소 단어 빈도
               range = c(10, 25)) +   # 최소, 최대 글자 크기
  scale_color_gradient(low = "#90c1f5", high = "#074282") +
  theme_minimal()


# Black Han Sans
# Jua
# Nanum Gothic
# Noto Serif KR
# Nanum Myeongjo
# Do Hyeon