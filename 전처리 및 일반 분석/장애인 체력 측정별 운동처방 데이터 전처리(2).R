library(dplyr)
library(readxl)
library(reshape2)
library(writexl)
df <-  read_excel('rdata/update.xlsx')
colnames(df)
df_melt <- melt(data = df, 
     id.vars = c("성별구분코드", "장애유형명", "연령대", "장애등급명", "part")) %>% select(-variable)

# df_melt %>% 
#   group_by(성별구분코드, 장애유형명, 연령대, 장애등급명, part, value) %>% 
#   count(value, sort = TRUE)

df_excersie_count <- df_melt %>% 
  count(성별구분코드, 장애유형명, 연령대, 장애등급명, part, value, sort = F) %>% 
  na.omit()

df_excersie_count %>%
  group_by(성별구분코드, 장애유형명, 연령대, 장애등급명, part) %>%
  summarise(max_time = max(n))
  
a <- df_excersie_count %>% 
  group_by(성별구분코드, 장애유형명, 연령대, 장애등급명, part) %>% 
  arrange(desc(n))

b <- a %>% filter(n == max(n)) %>% arrange(성별구분코드, 장애유형명, 연령대) %>% print()

b <- dplyr::rename(b,
                   운동순서 = part,
                   처방운동 = value,
                   처방횟수 = n)
write_xlsx(b, 'rdata/장애인 체력 측정별 운동처방 데이터(2021.01.~2021.08.)_ver2.0.xlsx')

table(b$성별구분코드)
table(b$장애유형명)
table(b$연령대)
table(b$장애등급명)
table(b$처방운동)

