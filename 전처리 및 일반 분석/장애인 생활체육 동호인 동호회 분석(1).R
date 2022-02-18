library(dplyr)
library(openxlsx)
library(ggplot2)

seoul <-  read.xlsx('rdata/장애인 생활체육 동호인 동호회 조회 정보(202110).xlsx', sheet = 2)

seoul1 <- seoul[!is.na(seoul$장애유형명),]
# 서울시 장애유형별 동호회 운영 개수
df1 <- seoul1 %>% 
  group_by(장애유형명) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = round(n/total*100, 1)) %>% 
  arrange(desc(pct))

ggplot(df1, aes(reorder(장애유형명, -n), n, fill = 장애유형명)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(label = paste(n)), vjust=-.4) +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ") +
  xlab('') + 
  ylab('운영 동호회 개수(단위: 개수)') +
  ggtitle('서울시 장애유형별 운영 동호회 현황')

# 서울시 장애유형별 동호회 운영 운동 종목 종류
df2 <- seoul %>% group_by(장애유형명, 종목명) %>% summarise(n = n())
## NA: 클럽에 소속되지 않은 동호인이 해당 운동 종목 대회에 참가하기위해 신청하는 클럽입니다.


df2_group <- seoul %>% 
  group_by(장애유형명, 종목명) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = round(n/total*100, 1)) %>% 
  arrange(desc(pct))

df2_group %>% filter(장애유형명 == '기타장애')

df2_group %>% filter(장애유형명 == '뇌병변장애')

df2_group %>% filter(장애유형명 == '시각장애')

df2_group %>% filter(장애유형명 == '지체장애')

df2_group %>% filter(장애유형명 == '지적장애')

df2_group %>% filter(장애유형명 == '청각장애')


ggplot(df2, aes(x=종목명, y=n, fill = 장애유형명)) +
  geom_col() +
  facet_grid('장애유형명')

# 기타장애
ggplot(df2 %>% filter(장애유형명 == '기타장애'), aes(x=reorder(종목명, -n), y=n, fill = 종목명)) + 
  geom_col() +
  theme_minimal() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ") +
  xlab('기타장애') + 
  ylab('운영 종목수(단위: 개수)') +
  ylim(c(0, 15))

# 뇌병변장애
ggplot(df2 %>% filter(장애유형명 == '뇌병변장애'), aes(x=reorder(종목명, -n), y=n,fill = 종목명)) + 
  geom_col()+
  theme_minimal() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ") +
  xlab('뇌병변장애') + 
  ylab('운영 종목수(단위: 개수)') +
  ylim(c(0, 15))

# 시각장애
ggplot(df2 %>% filter(장애유형명 == '시각장애'), aes(x=reorder(종목명, -n), y=n, fill = 종목명)) + 
  geom_col() +
  theme_minimal() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ") +
  xlab('시각장애') + 
  ylab('운영 종목수(단위: 개수)') +
  ylim(c(0, 15))

# 지적장애
ggplot(df2 %>% filter(장애유형명 == '지적장애'), aes(x=reorder(종목명, -n), y=n, fill = 종목명)) + 
  geom_col() +
  theme_minimal() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ") +
  xlab('지적장애') + 
  ylab('운영 종목수(단위: 개수)') +
  ylim(c(0, 15))

# 지체장애
ggplot(df2 %>% filter(장애유형명 == '지체장애'), aes(x=reorder(종목명, -n), y=n, fill = 종목명)) + 
  geom_col() +
  theme_minimal() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ") +
  xlab('지체장애') + 
  ylab('운영 종목수(단위: 개수)') +
  ylim(c(0, 15))

# 청각장애
ggplot(df2 %>% filter(장애유형명 == '청각장애'), aes(x=reorder(종목명, -n), y=n, fill = 종목명)) + 
  geom_col() +
  theme_minimal() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ") +
  xlab('청각장애') + 
  ylab('운영 종목수(단위: 개수)') +
  ylim(c(0, 15))
