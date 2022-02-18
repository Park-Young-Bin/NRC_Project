# 장애유무별 건강검진(OU) 항목 분석----
# ou 데이터 검사종류 ~ 장애인/비장애인 비교
# 필요한 라이브러리 불러오기
library(writexl)
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape)
# library(RColorBrewer)

# 데이터 불러오기
# hh <- read_excel('rdata/khealth/xlsx/t18hh.xlsx', col_names = T) # 연간 총 가구소득, 총가구원수
ind <- read_excel('rdata/khealth/xlsx/t18ind.xlsx', col_names = T) # 장애종류, 장애등급
ou <- read_excel('rdata/khealth/xlsx/t18ou.xlsx', col_names = T) # 검사종류(골밀도_3, 생체_4, 심전도_9)
# appen <- read_excel('rdata/khealth/xlsx/t18appen.xlsx', col_names = T) # 격렬한 신체활동, 중증도 신체활동, 걷기&지속시간, 운동능력

# 필요한 변수만 추출
select_ind <- ind %>% 
  select(PIDWON, C14)

select_ou <- ou %>% 
  select(PIDWON, OUNUM, OU19, OU20, OU21, OU190, OU200) # OUNUM: 외래서비스 이용 횟수를 구분하기 위해 사용

# 데이터 병합
merge_df <- merge(select_ind, select_ou, by = 'PIDWON')

# 변수명 변경(영 -> 한)
df <- dplyr::rename(merge_df,
                    장애등급 = C14,
                    외래이용일련번호 = OUNUM,
                    '검사종류1' = OU19,
                    '검사종류2' = OU20,
                    '검사종류3' = OU21,
                    '검사종류4' = OU190,
                    '검사종류5' = OU200)

str(df)

# 장애인/비장애인 분류
df$장애등급 <- ifelse(df$장애등급 == -1, '비장애인', '장애인')

# melt 함수 적용(검사종류1~5)
melt_df <- melt(data=df, id.vars = c('PIDWON', '장애등급', '외래이용일련번호'))

# value 변수 filter 적용(골밀도_3, 생체_4, 심전도_9)
dt1 <- melt_df %>% 
  filter(value %in% c(3, 4, 9)) %>% 
  group_by(장애등급, value)

# 검사종류 값 변경
dt1$value <- ifelse(dt1$value == 3, '골밀도', 
                    ifelse(dt1$value ==4, '생체', '심전도'))
  
# 연간 외래 건강검진 횟수 확인 및 변수명 변경
dt2 <- dt1 %>% 
  group_by(장애등급, value) %>% 
  summarise(n = n())%>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

dt2 <- dplyr::rename(dt2, 검사종류 = value)

# 장애여부별 연간 외래 건강검진 비율
ggplot(dt2, aes(x = reorder(검사종류, -pct), y = pct, fill = 검사종류)) +
  geom_col() +
  geom_text(aes(label = paste(pct)), vjust=-.4) +
  theme_bw() +
  facet_wrap(장애등급~.) +
  ggtitle('장애여부별 연간 외래 건강검진 비율') +
  ylab('연간 외래 건강검진 비율(단위: %)') +
  xlab('검사 종류') +
  # scale_fill_viridis_d() + 
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ")

# 장애여부별 연간 외래 건강검진 횟수
ggplot(dt2, aes(x = reorder(검사종류, -n), y = n, fill = 검사종류)) +
  geom_col() +
  geom_text(aes(label = paste(n)), vjust=-.4) +
  theme_bw() +
  facet_wrap(장애등급~.) +
  ggtitle('장애여부별 연간 외래 건강검진 횟수') +
  ylab('연간 외래 건강검진 횟수') +
  xlab('검사 종류') +
  # scale_fill_viridis_d() + 
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ")

# 장애인, 비장애인 총 수
distinct(df %>% select(PIDWON, 장애등급)) %>% 
  group_by(장애등급) %>% 
  summarise(n=n())

# 한 번이라도 심전도 or 골밀도 or 생체 검진을 받은 장애인, 비장애인 총 수
# 한 번이라도 받은 사람이므로 '외래이용일련번호' 컬럼을 삭제하여 계산한다.
## 외래이용일련번호가 달라도 같은 검진을 여러 번 받은 경우가 있는데 이는 검진날짜가 다르기 때문이다.
## 한 번이라도 받은 경우이므로 날짜가 달라서 발생하는 같은 내용의 검진 경우는 없애야 한다.
dt1_1 <- dt1 %>% 
  select(-외래이용일련번호) %>% 
  distinct()

table(dt1_1$장애등급)

distinct(dt1 %>% select(PIDWON, 장애등급)) %>% 
  group_by(장애등급) %>% 
  summarise(n=n())

# 장애유무별 측정항목(appen_운동능력) 분석(장애유무별)----
appen <- read_excel('rdata/khealth/xlsx/t18appen.xlsx', col_names = T) # 삶의 질 영역

# 필요한 변수만 추출
select_appen <- appen %>% 
  select(PIDWON, SJ1, SJ2, SJ3) # 운동능력, 자기관리, 일상활동

# 데이터 병합
merge_df <- merge(select_ind, select_appen, by = 'PIDWON')

# 변수명 변경(영 -> 한)
rename_df <- dplyr::rename(merge_df, 
                    '장애유무' = C14,
                    '운동능력' = SJ1,
                    '자기관리' = SJ2,
                    '일상활동' = SJ3)

str(rename_df)

# 장애인/비장애인 분류
rename_df$장애유무 <- ifelse(rename_df$장애유무 == -1, '비장애인', '장애인')

# melt 함수 적용
melt_df <- melt(data=rename_df, id.vars = c('PIDWON', '장애유무'))

# 결측치(-9, -1) 제거
dt2 <- melt_df %>% 
  filter(value %in% c(1, 2, 3))

# 그룹화
dt3 <- dt2 %>% 
  group_by(장애유무, variable, value) %>% 
  summarise(n = n())
head(dt3)

# 측정항목 중 '운동능력' 항목만 추출
dt3_excerise <- dt3 %>% 
  filter(variable == '운동능력') %>% 
  group_by(장애유무) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group * 100, 1))

# value 값 변경
dt3_excerise$value <- ifelse(dt3_excerise$value == 1, '걷기 지장 없음',
                             ifelse(dt3_excerise$value == 2, '걷기 다소 지장 있음', '종일 누워있음'))

head(dt3_excerise)

# 장애유무별 운동능력 응답자 수
ggplot(dt3_excerise, aes(x = reorder(value, -pct), y = pct, fill = value)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste(pct)), vjust=-.4) +
  theme_bw() +
  facet_wrap(장애유무~.) +
  ggtitle('장애여부별 운동능력 응답자 비율') +
  ylab('응답 비율(단위: %)') +
  xlab('보기 문항') +
  # scale_fill_viridis_d() + 
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ",
        axis.text.x = element_text(angle = 12, hjust = 0.5, vjust = .7))

# 장애유무별 측정항목(appen_운동능력) 분석(장애여부별_성별)----
select_ind <- ind %>% 
  select(PIDWON, C14, C3, C4_0) # 장애등급, 성별, 출생년도

# 필요한 변수만 추출
select_appen <- appen %>% 
  select(PIDWON, SJ1, SJ2, SJ3) # 운동능력, 자기관리, 일상활동

# 데이터 병합
merge_df <- merge(select_ind, select_appen, by = 'PIDWON')

# 변수명 변경(영 -> 한)
rename_df <- dplyr::rename(merge_df, 
                           '성별' = C3,
                           '출생년도' = C4_0,
                           '장애유무' = C14,
                           '운동능력' = SJ1,
                           '자기관리' = SJ2,
                           '일상활동' = SJ3)

# 장애인/비장애인 분류
rename_df$장애유무 <- ifelse(rename_df$장애유무 == -1, '비장애인', '장애인')

# 나이 및 연령대 계산
rename_df$나이 <- 2018 - rename_df$출생년도 + 1
rename_df$연령대 <- ifelse(rename_df$나이 < 8 , '학령기', 
       ifelse(rename_df$나이 >= 8 & rename_df$나이 <= 19, '미성년', 
              ifelse(rename_df$나이 >= 20 & rename_df$나이 <= 39, '청년',
                     ifelse(rename_df$나이 >= 40 & rename_df$나이 <= 64, '중년', '고령'))))

# 성별 변수 변환
rename_df$성별 <- ifelse(rename_df$성별 == 1, '남', '여')

# melt 함수 적용
melt_df <- melt(data=rename_df %>% select(- c(출생년도, 나이, 연령대)), 
                id.vars = c('PIDWON', '장애유무', '성별'))

# 결측치(-9, -1) 제거
dt2 <- melt_df %>% 
  filter(value %in% c(1, 2, 3))

# 그룹화
dt3 <- dt2 %>% 
  group_by(장애유무, 성별, variable, value) %>% 
  summarise(n = n())
head(dt3)

# 측정항목 중 '운동능력' 항목만 추출
dt3_excerise_gender <- dt3 %>% 
  filter(variable == '운동능력') %>% 
  group_by(장애유무) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group * 100, 1))
dt3_excerise_gender

# value 값 변경
dt3_excerise_gender$value <- ifelse(dt3_excerise_gender$value == 1, '걷기 지장 없음',
                             ifelse(dt3_excerise_gender$value == 2, '걷기 다소 지장 있음',
                                    ifelse(dt3_excerise_gender$value == 3, '종일 누워있음', '불편 없음')))

# 성별 장애유무별 운동능력 응답자 수
ggplot(dt3_excerise_gender, aes(x = reorder(value, -pct), y = pct, fill = 성별)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste(pct)), vjust=-.25, position = position_dodge(.9), size = 3.5) +
  theme_bw() +
  facet_wrap(장애유무~.) +
  ggtitle('성별 장애여부별 운동능력 응답자 비율') +
  ylab('응답 비율(단위: %)') +
  xlab('보기 문항') +
  # scale_fill_viridis_d() + 
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        axis.text.x = element_text(angle = 12, hjust = 0.5, vjust = .7))

# 장애유무별 측정항목(appen_운동능력) 분석(장애여부별_연령별)----
# melt 함수 적용
melt_df <- melt(data=rename_df %>% select(- c(출생년도, 나이, 성별)), 
                id.vars = c('PIDWON', '장애유무', '연령대'))

# 결측치(-9, -1) 제거
dt2 <- melt_df %>% 
  filter(value %in% c(1, 2, 3))

# 그룹화
dt3 <- dt2 %>% 
  group_by(장애유무, 연령대, variable, value) %>% 
  summarise(n = n())
head(dt3)

# 측정항목 중 '운동능력' 항목만 추출
dt3_excerise_age <- dt3 %>% 
  filter(variable == '운동능력') %>% 
  group_by(장애유무) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group * 100, 1))
dt3_excerise_age

# value 값 변경
dt3_excerise_age$value <- ifelse(dt3_excerise_age$value == 1, '걷기 지장 없음',
                             ifelse(dt3_excerise_age$value == 2, '걷기 다소 지장 있음', '종일 누워있음'))

# 연령대 facotr형으로 변경
dt3_excerise_age$연령대 <- factor(dt3_excerise_age$연령대,
                               levels = c('미성년', '청년', '중년', '고령'))

# 연령별 장애유무별 운동능력 응답자 수
ggplot(dt3_excerise_age, aes(x = reorder(value, -pct), y = pct, fill = 장애유무)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste(pct)), vjust=-.25, position = position_dodge(.9), size = 3.5) +
  theme_bw() +
  facet_wrap(연령대~.) +
  ggtitle('연령별 장애여부별 운동능력 응답자 비율') +
  ylab('응답 비율(단위: %)') +
  xlab('보기 문항') +
  # scale_fill_viridis_d() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        axis.text.x = element_text(angle = 12, hjust = 0.5, vjust = .7))

a <- dt3_excerise_age %>% 
  select(장애유무, 연령대, value, n)

hist(rename_df$나이, main = '운동능력 설문 응답자 연령', xlab = '나이', ylab = '응답자 수')

# 장애여부별 측정항목(appen_자기관리) 분석(장애여부별)----
## 장애유무별 자기관리 
select_ind <- ind %>% 
  select(PIDWON, C14, C3, C4_0) # 장애등급, 성별, 출생년도

# 필요한 변수만 추출
select_appen <- appen %>% 
  select(PIDWON, SJ1, SJ2, SJ3) # 운동능력, 자기관리, 일상활동

# 데이터 병합
merge_df <- merge(select_ind, select_appen, by = 'PIDWON')

# 변수명 변경(영 -> 한)
rename_df <- dplyr::rename(merge_df, 
                           '성별' = C3,
                           '출생년도' = C4_0,
                           '장애유무' = C14,
                           '운동능력' = SJ1,
                           '자기관리' = SJ2,
                           '일상활동' = SJ3)

# 장애인/비장애인 분류
rename_df$장애유무 <- ifelse(rename_df$장애유무 == -1, '비장애인', '장애인')

# 나이 및 연령대 계산
rename_df$나이 <- 2018 - rename_df$출생년도 + 1

###################################################
# 자기관리 응답자 연령 분포
a <- rename_df %>% filter(자기관리 %in% c(1, 2, 3))
hist(a$나이, main = '자기관리 응답자 연령 분포')

# 자기관리 비장애인 응답자 연령 분포
an <- a %>% filter(장애유무 == -1)
hist(an$나이, main = '자기관리 비장애인 응답자 연령 분포', breaks=seq(19,100,by=1), ylim=c(0,250))

# 자기관리 장애인 응답자 연령 분포
ad <- a %>% filter(장애유무 != -1)
hist(ad$나이, main = '자기관리 장애인 응답자 연령 분포', breaks=seq(21,100,by=1), ylim=c(0,250))

# 65세 이상 자기관리 비장애인 응답자 연령 분포
an <- a %>% filter(장애유무 == -1)
hist(an$나이, main = '65세 이상 자기관리 비장애인 응답자 연령 분포', breaks=seq(19,100,by=1), xlim=c(65,100), ylim=c(0,250))

# wkddodu

# 65세 이상 자기관리 장애인 응답자 연령 분포
ad <- a %>% filter(장애유무 != -1)
hist(ad$나이, main = '65세 이상 자기관리 장애인 응답자 연령 분포', breaks=seq(21,100,by=1), xlim=c(65,100), ylim=c(0,250))


rename_df$장애정도 <- ifelse(rename_df$장애유무 %in% c(1, 2, 3), '중증', 
                         ifelse(rename_df$장애유무 %in% c(4, 5, 6), '경증', '비장애인'))

# 나이 및 연령대 계산
rename_df$나이 <- 2018 - rename_df$출생년도 + 1

# 자기관리 장애정도 경증 응답자 연령 분포
# b <- rename_df %>% filter(장애정도 != '비장애인' & 자기관리 %in% c(1, 2, 3))
# bl <- b %>% filter(장애정도 == '경증')
hist(bl$나이 , main = '65세 이상 자기관리 경증 장애응답자 연령 분포', breaks=seq(20,100,by=1), xlim=c(65,100), ylim=c(0,40))

# 자기관리 장애정도 중증 응답자 연령 분포
# c <- rename_df %>% filter(장애정도 != '비장애인' & 자기관리 %in% c(1, 2, 3))
# bh <- c %>% filter(장애정도 == '중증')
hist(bh$나이 , main = '65세 이상 자기관리 중증 장애응답자 연령 분포', breaks=seq(20,100,by=1), xlim=c(65,100), ylim=c(0,40))

# 65세 이상 장애유무별 자기관리 응답 분포
age65 <- rename_df %>% filter(나이 >= 65) # 65세 이상

age65 <- age65 %>% filter(자기관리 %in% c(1, 2, 3)) # 결측값 제외

age65$자기관리 <- ifelse(age65$자기관리 == 1, '지장 없음',
                         ifelse(age65$자기관리 == 2, '다소 지장 있음', '혼자 불가'))

age65$장애유무 <- ifelse(age65$장애유무 == -1, '비장애인', '장애인')

age65_1 <- age65 %>% select(장애유무, 자기관리) %>% 
  group_by(장애유무, 자기관리) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group * 100, 1)) # 65세 이상 장애유무별 응답 분포 확인

ggplot(age65_1, aes(x = reorder(자기관리, -pct), y = pct, fill = 자기관리)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste(pct)), vjust=-.25, position = position_dodge(.9), size = 3.5) +
  theme_bw() +
  facet_wrap(장애유무~.) +
  ggtitle('장애여부별 자기관리 고령 응답자 비율') +
  ylab('응답 비율(단위: %)') +
  xlab('보기 문항') +
  # scale_fill_viridis_d() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = "",
        axis.text.x = element_text(angle = 12, hjust = 0.5, vjust = .7)) # 시각화

table(age65$장애유무, age65$자기관리)

##########################################################

# 장애여부별 자기관리 응답 비율
ggplot(age65, aes(x = reorder(자기관리, -pct), y = pct, fill = value)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste(pct)), vjust=-.25, position = position_dodge(.9), size = 3.5) +
  theme_bw() +
  facet_wrap(장애유무~.) +
  ggtitle('장애유무별 자기관리 응답자 비율') +
  ylab('응답 비율(단위: %)') +
  xlab('보기 문항') +
  scale_fill_viridis_d() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = "",
        axis.text.x = element_text(angle = 12, hjust = 0.5, vjust = .7))


rename_df$연령대 <- ifelse(rename_df$나이 < 8 , '학령기', 
                        ifelse(rename_df$나이 >= 8 & rename_df$나이 <= 19, '미성년', 
                               ifelse(rename_df$나이 >= 20 & rename_df$나이 <= 39, '청년',
                                      ifelse(rename_df$나이 >= 40 & rename_df$나이 <= 64, '중년', '고령'))))

# 성별 변수 변환
rename_df$성별 <- ifelse(rename_df$성별 == 1, '남', '여')

# melt 함수 적용
melt_df <- melt(data=rename_df, id.vars = c('PIDWON', '장애유무'))

# 결측치(-9, -1) 제거
dt2 <- melt_df %>% 
  filter(value %in% c(1, 2, 3))

# 그룹화
dt3 <- dt2 %>% 
  group_by(장애유무, variable, value) %>% 
  summarise(n = n())
head(dt3)

# 측정항목 중 '운동능력' 항목만 추출
dt3_self <- dt3 %>% 
  filter(variable == '자기관리') %>% 
  group_by(장애유무) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group * 100, 1))

# value 값 변경
dt3_self$value <- ifelse(dt3_self$value == 1, '지장 없음',
                                 ifelse(dt3_self$value == 2, '다소 지장 있음',
                                        ifelse(dt3_self$value == 3, '혼자 불가', '불편 없음')))

# 장애여부별 자기관리 응답 비율
ggplot(dt3_self, aes(x = reorder(value, -pct), y = pct, fill = value)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste(pct)), vjust=-.25, position = position_dodge(.9), size = 3.5) +
  theme_bw() +
  facet_wrap(장애유무~.) +
  ggtitle('장애여부별 자기관리 응답자 비율') +
  ylab('응답 비율(단위: %)') +
  xlab('보기 문항') +
  # scale_fill_viridis_d() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = "",
        axis.text.x = element_text(angle = 12, hjust = 0.5, vjust = .7))

# 장애유무별 측정항목(appen_자기관리) 분석(장애유무별_성별)----
# melt 함수 적용
melt_df <- melt(data=rename_df, id.vars = c('PIDWON', '장애유무', '성별'))

# 결측치(-9, -1) 제거
dt2 <- melt_df %>% 
  filter(value %in% c(1, 2, 3))

# 그룹화
dt3 <- dt2 %>% 
  group_by(장애유무, 성별, variable, value) %>% 
  summarise(n = n())
head(dt3)

# 측정항목 중 '자기관리' 항목만 추출
dt3_self <- dt3 %>% 
  filter(variable == '자기관리') %>% 
  group_by(장애유무) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group * 100, 1))

# value 값 변경
dt3_self$value <- ifelse(dt3_self$value == 1, '지장 없음',
                         ifelse(dt3_self$value == 2, '다소 지장 있음', '혼자 불가'))

# 성별 장애여부별 자기관리 응답 비율
ggplot(dt3_self, aes(x = reorder(value, -pct), y = pct, fill = 성별)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste(pct)), vjust=-.25, position = position_dodge(.9), size = 3.5) +
  theme_bw() +
  facet_wrap(장애유무~.) +
  ggtitle('성별 장애여부별 자기관리 응답자 비율') +
  ylab('응답 비율(단위: %)') +
  xlab('보기 문항') +
  # scale_fill_viridis_d() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        axis.text.x = element_text(angle = 12, hjust = 0.5, vjust = .7))

# 장애여부별 측정항목(appen_자기관리) 분석(장애여부별_연령별)----
# melt 함수 적용
melt_df <- melt(data=rename_df, id.vars = c('PIDWON', '장애유무', '연령대'))

# 결측치(-9, -1) 제거
dt2 <- melt_df %>% 
  filter(value %in% c(1, 2, 3))

# 그룹화
dt3 <- dt2 %>% 
  group_by(장애유무, 연령대, variable, value) %>% 
  summarise(n = n())
head(dt3)

# 측정항목 중 '자기관리' 항목만 추출
dt3_self <- dt3 %>% 
  filter(variable == '자기관리') %>% 
  group_by(장애유무) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group * 100, 1))

# value 값 변경
dt3_self$value <- ifelse(dt3_self$value == 1, '지장 없음',
                         ifelse(dt3_self$value == 2, '다소 지장 있음', '혼자 불가'))

# 연령대 facotr형으로 변경
dt3_self$연령대 <- factor(dt3_self$연령대,
                               levels = c('미성년', '청년', '중년', '고령'))

# 연령별 장애여부별 자기관리 응답 비율
ggplot(dt3_self, aes(x = reorder(value, -pct), y = pct, fill = 장애유무)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste(pct)), vjust=-.25, position = position_dodge(.9), size = 3.5) +
  theme_bw() +
  facet_wrap(연령대~.) +
  ggtitle('연령별 장애여부별 자기관리 응답자 비율') +
  ylab('응답 비율(단위: %)') +
  xlab('보기 문항') +
  # scale_fill_viridis_d() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        axis.text.x = element_text(angle = 12, hjust = 0.5, vjust = .7))

barplot(rename_df$연령대)

# 운동능력 범주형 변수 독립성 검정----
# rename_df 데이터 이용

# value 값 변경
rename_df$운동능력 <- ifelse(rename_df$운동능력 == 1, '걷기 지장 없음', 
                          ifelse(rename_df$운동능력 == 2, '걷기 다소 지장 있음', '종일 누워있음'))

rename_df$자기관리 <- ifelse(rename_df$자기관리 == 1, '지장 없음',
                         ifelse(rename_df$자기관리 == 2, '다소 지장 있음', '혼자 불가'))

rename_df$일상활동 <- ifelse(rename_df$자기관리 == 1, '지장 없음',
                         ifelse(rename_df$자기관리 == 2, '다소 지장 있음', '일상생활 불가능'))

mod_df <- rename_df %>% 
  select(장애유무, 성별, 연령대, 운동능력)

# 카이제곱 검정
# install.packages('prettyR')
# library(prettyR)
str(mod_df)

# 교차표 생성
crosstb <- table(mod_df$장애유무, mod_df$운동능력) # 장애유무별 운동능력
crosstb_gender <- table(mod_df$성별, mod_df$운동능력) # 성별 운동능력
crosstb_ageg <- table(mod_df$연령대, mod_df$운동능력) # 연령별 운동능력

# 독립성 검정 실시
chisq.test(crosstb) # 서로 연관 있다.
chisq.test(crosstb_gender) # 서로 연관 있다.
chisq.test(crosstb_ageg) # 서로 연관 있다.

# 범주형 변수 상관관계 파악
# install.packages('DescTools')
# library(DescTools)
# CramerV(mod_df$장애유무, mod_df$운동능력)

# 운동능력 다항 로지스틱 회귀분석----
# 필요한 변수만 추출
select_ind <- ind %>% 
  select(PIDWON, C14, C3, C4_0, i_MEDICALEXP1)

select_appen <- appen %>% 
  select(PIDWON, SJ1, SH16, SH18, SH20, SH21, SH23, SH24, SH25)

# PIDWON 기준으로 merge
merge_df <- merge(select_ind, select_appen, by = 'PIDWON')

# 변수명 및 값 변경
# 변수명 변경(영 -> 한)
rename_df <- dplyr::rename(merge_df, 
                           '성별' = C3,
                           '출생년도' = C4_0,
                           '장애유무' = C14,
                           '운동능력' = SJ1,
                           '와병률' = SH16,
                           '개인지출의료비' = i_MEDICALEXP1,
                           '결근결석' = SH18,
                           '시력문제' = SH20,
                           '청력문제' = SH21,
                           '기억력' = SH23,
                           '의사결정' = SH24,
                           '질병/손상 등으로 활동제한' = SH25)

table(rename_df$기억력)

# pairplot 작성
# install.packages('PerformanceAnalytics')
# library(PerformanceAnalytics)
# chart.Correlation(rename_df, histogram = T)
# 
# install.packages('psych')
# library(psych)
# pairs.panels(rename_df, method = "spearman", hist.col = "green", density = TRUE, ellipses = TRUE)
# 
# cor(rename_df, method = 'spearman', use = 'complete.obs')
# 
# library(corrplot)
# corrplot(cor(rename_df, method = 'spearman', use = 'complete.obs'))


# 장애인/비장애인 분류
rename_df$장애유무 <- ifelse(rename_df$장애유무 == -1, '비장애인', '장애인')

# 나이 및 연령대 계산
rename_df$나이 <- 2018 - rename_df$출생년도 + 1
rename_df$연령대 <- ifelse(rename_df$나이 < 8 , '학령기 이전', 
                        ifelse(rename_df$나이 >= 8 & rename_df$나이 <= 19, '미성년', 
                               ifelse(rename_df$나이 >= 20 & rename_df$나이 <= 39, '청년',
                                      ifelse(rename_df$나이 >= 40 & rename_df$나이 <= 64, '중년', '고령'))))

# 성별 변수 변환
rename_df$성별 <- ifelse(rename_df$성별 == 1, '남', '여')

# 종속변수(운동능력) 변환
rename_df$운동능력 <- ifelse(rename_df$운동능력 == 1, '걷기 지장 없음',
                             ifelse(rename_df$운동능력 == 2, '걷기 다소 지장 있음', '종일 누워있음'))

# 와병률 변수 변환
rename_df$와병률 <- ifelse(rename_df$와병률 == 1, '예', 
                         ifelse(rename_df$와병률 == 2, '아니오', NA))

# 결근결석 변수 변환
rename_df$결근결석 <- ifelse(rename_df$결근결석 == 1, '예', 
                        ifelse(rename_df$결근결석 == 2, '아니오', 
                               ifelse(rename_df$결근결석 == 3, '학교나 직장 안 다님', NA)))

# 시력문제 변수 변환
rename_df$시력문제 <- ifelse(rename_df$시력문제 == 1, '문제 없음', 
                         ifelse(rename_df$시력문제 == 2, '조금 문제 있음', 
                                ifelse(rename_df$시력문제 == 3, '많이 문제 있음', 
                                       ifelse(rename_df$시력문제 == 4, '전혀 보지 못함', NA))))

# 청력문제 변수 변환
rename_df$청력문제 <- ifelse(rename_df$청력문제 == 1, '문제 없음', 
                         ifelse(rename_df$청력문제 == 2, '조금 문제 있음', 
                                ifelse(rename_df$청력문제 == 3, '많이 문제 있음', 
                                       ifelse(rename_df$청력문제 == 4, '전혀 듣지 못함', NA))))

# 기억력 변수 변환
rename_df$기억력 <- ifelse(rename_df$기억력 == 1, '문제 있음', 
                         ifelse(rename_df$기억력 == 2, '문제 없음', NA))

# 의사결정 변수 변환
rename_df$의사결정 <- ifelse(rename_df$의사결정 == 1, '문제 있음', 
                        ifelse(rename_df$의사결정 == 2, '문제 없음', NA))

# 질병/손상 등으로 활동제한 변수 변환
rename_df$'질병/손상 등으로 활동제한' <- ifelse(rename_df$'질병/손상 등으로 활동제한' == 1, '문제 있음', 
                         ifelse(rename_df$'질병/손상 등으로 활동제한' == 2, '문제 없음', NA))

# PIDWON, 나이, 출생년도 삭제
data <- rename_df %>% select(-c(PIDWON, 나이, 출생년도))
str(data)

# NA 확인
sum(is.na(data))

# 카이제곱 검정과 피셔 검정 동시 진행(warning 발생 시, fisher 결과 해석)

# install.packages('gmodels')
# install.packages('rcompanion')
library(gmodels)
library(rcompanion)

# 빈도표 생성 후, 검정 실시
## 성별
xtabs(~ 성별 + 운동능력, data = data)
print(fit.fis <- CrossTable(as.matrix(xtabs(~ 성별 + 운동능력, data = data)), expected = T, chisq = T, fisher = T))
# print(fit.fisph <- pairwiseNominalIndependence(as.matrix(xtabs(~ 성별 + 운동능력, data = data))))

## 장애유무
xtabs(~ 장애유무 + 운동능력, data = data)
print(fit.fis <- CrossTable(as.matrix(xtabs(~ 장애유무 + 운동능력, data = data)), expected = T, chisq = T, fisher = T))
# print(fit.fisph <- pairwiseNominalIndependence(as.matrix(xtabs(~ 장애유무 + 운동능력, data = data))))

## 와병률
xtabs(~ 와병률 + 운동능력, data = data)
print(fit.fis <- CrossTable(as.matrix(xtabs(~ 와병률 + 운동능력, data = data)), expected = T, chisq = T, fisher = T))
# print(fit.fisph <- pairwiseNominalIndependence(as.matrix(xtabs(~ 와병률 + 운동능력, data = data))))

## 결근결석
xtabs(~ 결근결석 + 운동능력, data = data)
chisq.test(xtabs(~ 결근결석 + 운동능력, data = data))$expected # 카이제곱 검정법 사용
print(fit.fis <- CrossTable(as.matrix(xtabs(~ 결근결석 + 운동능력, data = data)), expected = T, chisq = T, fisher = T))
print(fit.fisph <- pairwiseNominalIndependence(as.matrix(xtabs(~ 결근결석 + 운동능력, data = data)), fisher = F))

## 시력문제
xtabs(~ 시력문제 + 운동능력, data = data)
print(fit.fis <- CrossTable(as.matrix(xtabs(~ 시력문제 + 운동능력, data = data)), 
                            expected = T, chisq = T, fisher = T)) # 피셔의 검정 결과 이용
# chisq.test(table(data$시력문제, data$운동능력))
# chisq.test(table(data$시력문제, data$운동능력))$expected
fisher.test(x = data$시력문제, data$운동능력, alternative = 'two.sided', simulate.p.value=TRUE)
print(fit.fisph <- pairwiseNominalIndependence(as.matrix(xtabs(~ 시력문제 + 운동능력, data = data)))) # p.adj.Fisher 값 확인

## 청력문제
xtabs(~ 청력문제 + 운동능력, data = data)
print(fit.fis <- CrossTable(as.matrix(xtabs(~ 청력문제 + 운동능력, data = data)), 
                            expected = T, chisq = T, fisher = T)) # 피셔의 검정 결과 이용
chisq.test(table(data$청력문제, data$운동능력))
fisher.test(x = data$청력문제, data$운동능력, alternative = 'two.sided', simulate.p.value=TRUE)
print(fit.fisph <- pairwiseNominalIndependence(as.matrix(xtabs(~ 청력문제 + 운동능력, data = data)))) # p.adj.Fisher 값 확인

## 기억력
xtabs(~ 기억력 + 운동능력, data = data)
print(fit.fis <- CrossTable(as.matrix(xtabs(~ 기억력 + 운동능력, data = data)), expected = T, chisq = T, fisher = T))
# print(fit.fisph <- pairwiseNominalIndependence(as.matrix(xtabs(~ 기억력 + 운동능력, data = data))))

## 의사결정
xtabs(~ 의사결정 + 운동능력, data = data)
chisq.test(table(data$의사결정, data$운동능력))$expected
print(fit.fis <- CrossTable(as.matrix(xtabs(~ 의사결정 + 운동능력, data = data)), expected = T, chisq = T, fisher = T))
# print(fit.fisph <- pairwiseNominalIndependence(as.matrix(xtabs(~ 의사결정 + 운동능력, data = data))))

# 개인지출의료비 EDA----
# install.packages('formattable') # 천단위 표시 가능한 패키지
library(formattable)
options(scipen=999) # 지수 표현 -> 수치 표현

# 연간 개인지출의료비 분포(hist)
ggplot(data = data, aes(x = 개인지출의료비)) + 
  geom_histogram(binwidth=1000000, color="black", fill="gray") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  ggtitle('응답자 연간 개인지출의료비 히스토그램') +
  ylab('응답자 수(단위: 명)') +
  xlab('개인지출의료비(단위: 원)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5)) +
  annotate('text', x=9000000, y=7000, label = '연간 개인지출의료비 평균: 801,716원') +
  geom_vline(aes(xintercept=mean(개인지출의료비)), color="red", linetype="dashed", size=1.4)

mean(data$개인지출의료비) # 801,716.2원

# 연간 개인지출의료비 분포(box)
ggplot(data = data, aes(y = 개인지출의료비)) + 
  geom_boxplot() +
  theme_minimal() +
  ylab('연간 개인지출의료비(단위: 원)') +
  ggtitle('응답자 연간 개인지출의료비 상자그림') +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))

# 연령별 연간 평균 개인지출의료비
dt_age <- data %>% group_by(연령대) %>% summarise(n = n(), avg_price = mean(개인지출의료비))
dt_age

ggplot(data = dt_age, aes(x = reorder(연령대, -avg_price), y = avg_price, fill = 연령대)) +
  geom_col(width = .8) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  geom_text(aes(label = paste0(comma(avg_price, format = 'd'), '원')), vjust=-.25, 
            position = position_dodge(.9), size = 4) +
  geom_text(aes(label = paste0(comma(n, format='d'), '명')), colour = 'white', position = position_stack(vjust=0.5)) +
  # scale_fill_viridis_d() +
  xlab('') +
  ylab('연간 평균 개인지출의료비(단위: 원)') +
  ggtitle('연령별 연간 평균 개인지출의료비') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ")

# 연령별 연간 개인지출의료비 분포
ggplot(data=data, aes(x=연령대, 개인지출의료비, fill = 연령대))+
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  geom_boxplot() +
  xlab('') + 
  ylab('연간 개인지출의료비(단위: 원)') +
  scale_x_discrete(limits=c("미성년", "청년", "중년", "고령")) +
  ggtitle('연령별 연간 개인지출의료비 상자그림') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = "")

# 장애여부별 평균 개인지출의료비
dt <- data %>% group_by(장애유무) %>% summarise(n = n(), avg_price = mean(개인지출의료비))
dt

ggplot(dt, aes(x = 장애유무, y = avg_price, fill = 장애유무)) +
  geom_col(width=.57) +
  geom_text(aes(label = paste0(comma(avg_price, format='d'), '원')), vjust=-.25, position = position_dodge(.9), size = 4) +
  geom_text(aes(label = paste0(comma(n, format='d'), '명')), colour = 'white', position = position_stack(vjust=0.5)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  ggtitle('장애여부별 연간 평균 개인지출의료비') +
  ylab('연간 평균 지출의료비(단위: 원)') +
  xlab('') +
  # scale_fill_viridis_d() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = "")

# 장애여부별 연간 개인지출의료비 분포
ggplot(data=data, aes(x=장애유무, 개인지출의료비, fill = 장애유무))+
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  geom_boxplot(width = .6) +
  xlab('') + 
  ylab('연간 개인지출의료비(단위: 원)') +
  # scale_x_discrete(limits=c("미성년", "청년", "중년", "고령")) +
  ggtitle('장애여부별 연간 개인지출의료비 상자그림') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = "")

data_normal <- data %>% filter(장애유무 == '비장애인')

ggplot(data = data_normal, aes(x = 개인지출의료비)) + 
  geom_histogram(binwidth=1000000, color="black", fill="gray") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  ggtitle('비장애인 연간 개인지출의료비 히스토그램') +
  ylab('응답자 수(단위: 명)') +
  xlab('개인지출의료비(단위: 원)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5)) +
  annotate('text', x=9000000, y=7000, label = '연간 개인지출의료비 평균: 782,783원') +
  geom_vline(aes(xintercept=mean(개인지출의료비)), color="red", linetype="dashed", size=1.4)

# 방법1_장애유무별 개인지출지출비 분포
ggplot(data, aes(x=개인지출의료비, fill=장애유무)) + geom_histogram(alpha=0.5)

# 방법2_장애유무별 개인의료지출비 분포
ggplot(data, aes(x=개인지출의료비)) + geom_histogram() +
  theme_bw() +
  facet_grid(장애유무 ~ .)+
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  ylab('응답자 수(단위: 명)') +
  ggtitle('장애여부별 연간 개인지출의료비 히스토그램') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))
  
  # annotate('text', x=9000000, y=7000, label = '연간 개인지출의료비 평균: 782,783원') +
  # geom_vline(aes(xintercept=mean(개인지출의료비)), color="red", linetype="dashed", size=1.4)



mean(as.matrix(data %>% filter(장애유무 == '비장애인') %>% select(개인지출의료비))) # 782,783원
mean(as.matrix(data %>% filter(장애유무 == '장애인') %>% select(개인지출의료비))) # 1,050,378원



# 운동능력에 따른 연간 평균 개인지출의료비
dt_ex_avg <- data %>% 
  group_by(운동능력) %>% 
  summarise(n = n(), avg_price = comma(mean(개인지출의료비), format = 'd'))
dt_ex_avg

ggplot(dt_ex_avg, aes(x = reorder(운동능력, -avg_price), y = avg_price, fill = 운동능력)) +
  geom_col(width=.6) +
  geom_text(aes(label = paste0(comma(avg_price, format='d'), '원')), vjust=-.25, position = position_dodge(.9), size = 4) +
  geom_text(aes(label = paste0(comma(n, format='d'), '명')), colour = 'white', position = position_stack(vjust=0.5)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  ggtitle('운동능력에 따른 연간 평균 개인지출의료비') +
  ylab('연간 평균 지출의료비(단위: 원)') +
  xlab('') +
  # scale_fill_viridis_d() +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = "")
