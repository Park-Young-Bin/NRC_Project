# install.packages('formattable') # 천단위 표시 가능한 패키지
# install.packages('gmodels')
# install.packages('VGAM')
library(gmodels)
library(VGAM) # 로지스틱 회귀분석
library(formattable)
library(writexl)
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape)

# 데이터 불러오기 및 병합----
appen <- read_excel('rdata/khealth/xlsx/t18appen.xlsx')
ind <- read_excel('rdata/khealth/xlsx/t18ind.xlsx')

select_ind <- ind %>% 
  select(PIDWON, C14, C3, C4_0, i_MEDICALEXP1)

select_appen <- appen %>% 
  select(PIDWON, SJ1, SJ2, SH16, SH18, SH20, SH21, SH23, SH24, SH25)

# PIDWON 기준으로 merge
merge_df <- merge(select_ind, select_appen, by = 'PIDWON')

# 데이터 전처리----
# 변수명 변경(영 -> 한)
rename_df <- dplyr::rename(merge_df, 
                           '성별' = C3,
                           '출생년도' = C4_0,
                           '장애여부' = C14,
                           '운동능력' = SJ1,
                           '자기관리' = SJ2,
                           '와병률' = SH16,
                           '개인지출의료비' = i_MEDICALEXP1,
                           '결근결석' = SH18,
                           '시력문제' = SH20,
                           '청력문제' = SH21,
                           '기억력' = SH23,
                           '의사결정' = SH24,
                           '질병/손상 등으로 활동제한' = SH25)
str(rename_df)

# 나이 및 연령대 계산
rename_df$나이 <- 2018 - rename_df$출생년도 + 1

rename_df$연령대 <- ifelse(rename_df$나이 < 8, 0, 
                        ifelse(rename_df$나이 >= 8 & rename_df$나이 <= 19, 1,
                               ifelse(rename_df$나이 >=20 & rename_df$나이 <= 39, 2,
                                      ifelse(rename_df$나이 >= 40 & rename_df$나이 <= 64, 3, 4))))

# 연령대 변수 facotr형 변환(chr -> factor)
# data$연령대<- factor(data$연령대, levels=c("미성년", '청년', '중년', '고령'))

# 장애인/비장애인 분류
rename_df$장애여부 <- ifelse(rename_df$장애여부 == -1, 1, 2) # 1: 비장애인, 2: 장애인

# 운동능력 변수 변환
table(rename_df$운동능력)
rename_df$운동능력 <- ifelse(rename_df$운동능력 %in% c(-1, -9), NA, rename_df$운동능력)

# 종속변수(자기관리) 변환_목욕/옷입기 능력 여부
table(rename_df$자기관리)
rename_df$자기관리 <- ifelse(rename_df$자기관리 %in% c(-1, -9), NA, rename_df$자기관리)
rename_df$자기관리 <- factor(rename_df$자기관리, levels = c(1, 2, 3))

# 와병률 변수 변환
table(rename_df$와병률)
rename_df$와병률 <- ifelse(rename_df$와병률 == -9, NA, 
                        ifelse(rename_df$와병률 == 1, 2, 1)) # 1(예) -> 2(예), 2(아니오) -> 1(아니오)

# 결근결석 변수 변환 (추후에 3번 문항 제거)
table(rename_df$결근결석)
rename_df$결근결석 <- ifelse(rename_df$결근결석 == -9, NA, 
                         ifelse(rename_df$결근결석 == 1, 2, 
                                ifelse(rename_df$결근결석 == 2 ,1, 3))) # 1(예) -> 2(예), 2(아니오) -> 1(아니오)
table(rename_df$결근결석)

# 시력문제 변수 변환
rename_df$시력문제 <- ifelse(rename_df$시력문제 == -9, NA, rename_df$시력문제)

# 청력문제 변수 변환
rename_df$청력문제 <- ifelse(rename_df$청력문제 == -9, NA, rename_df$청력문제)

# 기억력 변수 변환
table(rename_df$기억력)
rename_df$기억력 <- ifelse(rename_df$기억력 ==  -9, NA, 
                        ifelse(rename_df$기억력 == 1, 2, 1)) # 1(예) -> 2(예), 2(아니오) -> 1(아니오)
table(rename_df$기억력)

# 의사결정 변수 변환
table(rename_df$의사결정)
rename_df$의사결정 <- ifelse(rename_df$의사결정 ==  -9, NA, 
                         ifelse(rename_df$의사결정 == 1, 2, 1))
table(rename_df$의사결정)

# 질병/손상 등으로 활동제한 변수 변환
table(rename_df$'질병/손상 등으로 활동제한')
rename_df$'질병/손상 등으로 활동제한' <- ifelse(rename_df$'질병/손상 등으로 활동제한' ==  -9, NA, 
                                     ifelse(rename_df$'질병/손상 등으로 활동제한' == 1, 2, 1))
table(rename_df$'질병/손상 등으로 활동제한')

# PIDWON, 나이, 출생년도 삭제
data <- rename_df %>% select(-c(PIDWON, 출생년도))
str(data)

# NA 확인
sum(is.na(data)) # 547개(회귀분석시, 제거 필수)


# 순서형 로지스틱 회귀분석----
logit_data <- na.omit(data) %>% select(-나이)
str(logit_data)

# install.packages('oglmx')
library(oglmx)
dff2 <- logit_data %>% 
  filter(결근결석 != 3) # '학교/직장 안다님'

model2 <- oglmx(자기관리 ~., data = dff2, link="logit", constantMEAN = FALSE, constantSD = FALSE, delta=0, threshparam = NULL)
summary(model2)
exp(coef(model2)) # 오즈비

# 자기관리 능력에 따른 개인지출의료비----
## '지장없음' 기준 이상치 제거 전후 평균 비교
boxplot(logit_data[logit_data$자기관리 == 1, '개인지출의료비'])$stats
non2 <- data.frame(price = logit_data[logit_data$자기관리 == 1, '개인지출의료비'],
                  na_outlier = logit_data[logit_data$자기관리 == 1, '개인지출의료비']) # df 생성
non2$na_outlier <- ifelse(non2$na_outlier < 0 | non2$na_outlier > 2105170, NA, non2$na_outlier) # 이상치 -> NA
non2 %>% summarise(mean_price = mean(non2$price), # 평균(이상치 포함)
                  non_outlier_mean = mean(non2$na_outlier, na.rm = T)) # 평균(이상치 제거)

## '다소 지장있음' 이상치 제거 전후 평균 비교
boxplot(logit_data[logit_data$자기관리 == 2, '개인지출의료비'])$stats
sick2 <- data.frame(price = logit_data[logit_data$자기관리 == 2, '개인지출의료비'],
                   na_outlier = logit_data[logit_data$자기관리 == 2, '개인지출의료비']) # df 생성
sick2$na_outlier <- ifelse(sick2$na_outlier < 0 | sick2$na_outlier > 3838750, NA, sick2$na_outlier) # 이상치 -> NA
sick2 %>% summarise(mean_price = mean(sick2$price), # 평균(이상치 포함)
                   non_outlier_mean = mean(sick2$na_outlier, na.rm = T)) # 평균(이상치 제거)

## '혼자 불가능' 이상치 제거 전후 평균 비교
boxplot(logit_data[logit_data$자기관리 == 3, '개인지출의료비'])$stats
very_sick2 <- data.frame(price = logit_data[logit_data$자기관리 == 3, '개인지출의료비'],
                        na_outlier = logit_data[logit_data$자기관리 == 3, '개인지출의료비']) # df 생성
very_sick2$na_outlier <- ifelse(very_sick2$na_outlier < 0 | very_sick2$na_outlier > 4145390, NA, very_sick2$na_outlier) # 이상치 -> NA
very_sick2 %>% summarise(mean_price = mean(very_sick2$price), # 평균(이상치 포함)
                        non_outlier_mean = mean(very_sick2$na_outlier, na.rm = T)) # 평균(이상치 제거)

## 운동능력에 따른 개인지출의료비(중앙값, 평균_이상치_포함, 평균_이상치_제거)
self_price = data.frame(자기관리 = c('지장없음', '다소 지장있음', '혼자 불가능'),
                                중앙값 = c(301810, 747200, 972205),
                                mean = c(770626.1, 1424982, 2294717),
                                mean_non_outlier = c(438197.6, 882909.2, 1283793))
melt_self_price <- melt(self_price, id.vars = c('자기관리'))

label = c(
  중앙값 = '중앙값',
  mean = '평균(이상치 포함)',
  mean_non_outlier = '평균(이상치 제거)'
)

ggplot(melt(self_price, id.vars = c('자기관리')), aes(x = 자기관리, y = value, fill = 자기관리)) + 
  geom_col(width = .8) +
  facet_wrap('variable', labeller = labeller(variable = label)) +
  theme_bw() +
  scale_x_discrete(limits = c('지장없음', '다소 지장있음', '혼자 불가능')) +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ")+
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = paste0(comma(value, format='d'), '원')), vjust=-.4, size = 4) +
  ggtitle('자기관리에 따른 연간 개인지출의료비') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ",
        axis.text.x = element_text(angle = 12, hjust = 0.5, vjust = .7))+
  ylab('연간 개인지출의료비(단위: 원)')
