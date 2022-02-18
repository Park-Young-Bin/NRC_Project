library(dplyr)
library(openxlsx)
library(ggplot2)
library(reshape2)
library(webr)

df1 <- read.xlsx('rdata/update.xlsx')

df2 <- df1 %>% select(- c(성별구분코드, 연령대, 장애등급명))

df3 <- melt(data = df2, id.vars = c('장애유형명', 'part')) %>% na.omit() %>% select(- variable)

#### 시각장애인
visual <- df3 %>% filter(장애유형명 == '시각장애')

visual_group <- visual %>% 
  group_by(value) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = round(n/total*100, 1)) %>% 
  arrange(desc(pct))

visual_group_copy <- visual_group[c(1:10),]
etc1 <- data.frame(value = '기타', n = 40, total = 255, pct = 16)
visual_new <- rbind(visual_group_copy, etc1)
visual_new$value <- factor(visual_new$value, 
                              levels = c('앉아서 위로 밀기', '실내 자전거타기', '앉아서 모으기', '트레드밀에서 걷기', 
                                         '비스듬히 누워서 밀기', '바벨들어올리기', '앉아서 다리 모으기', '누워서 밀기', 
                                         '거꾸로 누워서 밀기', '서서 어깨 들어올리기', '기타'))

# ggplot(visual_new, aes(x="", y=pct, fill=value))+
#   geom_bar(stat = "identity") +
#   theme_minimal() +
#   geom_text(aes(label = paste(pct, '%')), vjust=2) +
#   xlab('시각장애인') +
#   ylab('비울(단위: %)') +
#   scale_fill_brewer(palette = "Spectral") +
#   ggtitle('시각장애인의 처방 운동 비율') +
#   theme(plot.title = element_text(face='bold',
#                                   size=20,
#                                   hjust = .5))
#   
# 
# 
# ggplot(visual_new, aes(x="", y=pct, fill=value))+
#   geom_bar(stat = "identity", color = 'white') +
#   theme_void() +
#   coord_polar('y') +
#   geom_text(aes(label = pct), color = "white")+
#   scale_fill_brewer(palette = "Spectral") +
#   ggtitle('시각장애인의 처방 운동 비율') +
#   theme(plot.title = element_text(face='bold',
#                                   size=20,
#                                   hjust = .5))

#### 지적장애인
intell <- df3 %>% filter(장애유형명 == '지적장애')

intell_group <- intell %>% 
  group_by(value) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = round(n/total*100, 1)) %>% 
  arrange(desc(pct))


sum(intell_group[c(22:31), 2])
intell_group_copy <- intell_group[c(1:21),]
etc2 <- data.frame(value = '기타', n = 211, total = 255, pct = 6.2)
intell_new <- rbind(intell_group_copy, etc2)


#### 청각장애인
sound <- df3 %>% filter(장애유형명 == '청각장애')

sound_group <- sound %>% 
  group_by(value) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = round(n/total*100, 1)) %>% 
  arrange(desc(pct))

sum(sound_group[c(16:27), 4])
sound_group_copy <- sound_group[c(1:15),]
etc3 <- data.frame(value = '기타', n = 29, total = 177, pct = 16.4)
sound_new <- rbind(sound_group_copy, etc3)

sound_new$pct

#### 척수장애인
spinal <- df3 %>% filter(장애유형명 == '척수장애')

spinal_group <- spinal %>% 
  group_by(value) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = round(n/total*100, 1)) %>% 
  arrange(desc(pct))

sum(spinal_group[c(17:25), 2])
spinal_group_copy <- spinal_group[c(1:16),]
etc4 <- data.frame(value = '기타', n = 16, total = 181, pct = 9.1)
spinal_new <- rbind(spinal_group_copy, etc4)

####################
visual %>% 
  group_by(part) %>% 
  summarise(n = n()) %>% 
  mutate(total = sum(n)) %>% 
  mutate(pct = round(n/total*100, 1)) %>% 
  arrange(desc(pct))

visual %>% arrange(desc(part))

a <- visual %>% 
  group_by(part, value) %>% 
  summarise(n = n())

PieDonut(a, aes(part, value, count=n), title = "Titanic: Survival by Class")
