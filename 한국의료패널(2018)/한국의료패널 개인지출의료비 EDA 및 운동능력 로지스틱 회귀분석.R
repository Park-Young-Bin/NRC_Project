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
appen <- read_excel('t18appen.xlsx')
ind <- read_excel('t18ind.xlsx')

select_ind <- ind %>% 
  select(PIDWON, C14, C3, C4_0, i_MEDICALEXP1)

select_appen <- appen %>% 
  select(PIDWON, SJ1, SH16, SH18, SH20, SH21, SH23, SH24, SH25)

# PIDWON 기준으로 merge
merge_df <- merge(select_ind, select_appen, by = 'PIDWON')

# 변수명 및 값 변경----
# 변수명 변경(영 -> 한)
rename_df <- dplyr::rename(merge_df, 
                           '성별' = C3,
                           '출생년도' = C4_0,
                           '장애여부' = C14,
                           '운동능력' = SJ1,
                           '와병률' = SH16,
                           '개인지출의료비' = i_MEDICALEXP1,
                           '결근결석' = SH18,
                           '시력문제' = SH20,
                           '청력문제' = SH21,
                           '기억력' = SH23,
                           '의사결정' = SH24,
                           '질병/손상 등으로 활동제한' = SH25)

# 성별 변수 변환
rename_df$성별 <- ifelse(rename_df$성별 == 1, '남', '여')

# 나이 및 연령대 계산
rename_df$나이 <- 2018 - rename_df$출생년도 + 1

rename_df$연령대 <- ifelse(rename_df$나이 < 8, "학령기 이전", 
                        ifelse(rename_df$나이 >= 8 & rename_df$나이 <= 19, '미성년',
                               ifelse(rename_df$나이 >=20 & rename_df$나이 <= 39, '청년',
                                      ifelse(rename_df$나이 >= 40 & rename_df$나이 <= 64, '중년', '고령'))))

# 연령대 변수 facotr형 변환(chr -> factor)
data$연령대<- factor(data$연령대, levels=c("미성년", '청년', '중년', '고령'))

# 장애인/비장애인 분류
rename_df$장애여부 <- ifelse(rename_df$장애여부 == -1, '비장애인', '장애인')

# 종속변수(운동능력) 변환
table(rename_df$운동능력)
rename_df$운동능력 <- ifelse(rename_df$운동능력 == 1, '걷기 지장 없음', 
                         ifelse(rename_df$운동능력 == 2, '걷기 다소 지장 있음', 
                                ifelse(rename_df$운동능력 == 3, '종일 누워있음', NA)))

# 와병률 변수 변환
rename_df$와병률 <- ifelse(rename_df$와병률 == 1, '예',
                        ifelse(rename_df$와병률 == 2, '아니오', NA))
# 결근결석 변수 변환
rename_df$결근결석 <- ifelse(rename_df$결근결석 == 1, '예', 
                         ifelse(rename_df$결근결석 == 2, '아니오', 
                                ifelse(rename_df$결근결석 == 3, '학교/직장 안다님', NA)))

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
data <- rename_df %>% select(-c(PIDWON, 출생년도))
str(data)

# NA 확인
sum(is.na(data)) # 547개(회귀분석시, 제거 필수)

# 연령별 연간 개인지출의료비 분포 확인(hist, box, mean, median plot 작성)----

# 연령별 연간 개인지출의료비 분포 확인(boxplot)
# boxplot(개인지출의료비 ~ 연령대, data)
data$연령대 <- factor(data$연령대, levels = c('미성년', '청년', '중년', '고령'))
boxplot(개인지출의료비 ~ 연령대, data, 
               cex.main = 1.6, cex.axis = 1.2, 
               # las = 1,
               outline = F, 
               ylim = c(0, 3500000), 
               xlab = '', ylab = '개인지출의료비(단위: 원)',
               main = '연령별 연간 개인지출의료비 상자그림(이상치 제거)') 
grid(ny=23)
boxplot(개인지출의료비 ~ 연령대, data, 
               cex.main = 1.6, cex.axis = 1.2, 
               # las = 1,
               outline = F, 
               ylim = c(0, 3500000), 
               xlab = '', ylab = '개인지출의료비(단위: 원)',
               main = '연령별 연간 개인지출의료비 상자그림(이상치 제거)', add = TRUE, ann = FALSE) 

boxplot(개인지출의료비 ~ 연령대, data)$stats # 상자그림 통계치 출력
# 아래쪽 이상치 경계
# 1사분위수
# 중앙값
# 3사분위수
# 위쪽 이상치 경계

# 각 연령별 연간 개인지출의료비 분포(boxplot)

## 미성년
boxplot(data[data$연령대 == '미성년', '개인지출의료비'])$stats
summary(data[data$연령대 == '미성년', '개인지출의료비'])

ggplot(data = data %>% filter(연령대 == '미성년'), aes(y = 개인지출의료비)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 261650))
  
p1 <- ggplot(data = data %>% filter(연령대 == '미성년'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_minimal() +
  # ggtitle('미성년 연간 개인지출의료비 상자그림(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))+
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x=element_blank()) +
  # scale_x_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 3500000)) # 이상치 제거 상자그림
p1

# ggplot(data = data %>% filter(연령대 == '미성년'), aes(x = 개인지출의료비)) + 
#   geom_histogram() + 
#   scale_y_continuous(labels = scales::comma)

## 청년
boxplot(data[data$연령대 == '청년', '개인지출의료비'])$stats
summary(data[data$연령대 == '청년', '개인지출의료비'])

ggplot(data = data %>% filter(연령대 == '청년'), aes(y = 개인지출의료비)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma)

ggplot(data = data %>% filter(연령대 == '청년'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(labels = scales::comma)

p2 <- ggplot(data = data %>% filter(연령대 == '청년'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_minimal() +
  ylab(' ') +
  theme(axis.text.x=element_blank()) +
  # gtitle('청년 연간 개인지출의료비 상자그림(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))+
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 3500000)) # 이상치 제거 상자그림

# ggplot(data = data %>% filter(연령대 == '청년'), aes(x = 개인지출의료비)) + 
#   geom_histogram() + 
#   scale_y_continuous(labels = scales::comma)+ 
#   scale_x_continuous(labels = scales::comma)

## 중년
boxplot(data[data$연령대 == '중년', '개인지출의료비'])$stats
data[data$연령대 == '중년', '개인지출의료비']
summary(data[data$연령대 == '중년', '개인지출의료비'])

ggplot(data = data %>% filter(연령대 == '중년'), aes(y = 개인지출의료비)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::comma)

ggplot(data = data %>% filter(연령대 == '중년'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA)+ 
  scale_y_continuous(labels = scales::comma)

p3 <- ggplot(data = data %>% filter(연령대 == '중년'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  ylab(' ')+
  # ggtitle('중년 연간 개인지출의료비 상자그림(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))+
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 3500000)) # 이상치 제거 상자그림

# ggplot(data = data %>% filter(연령대 == '중년'), aes(x = 개인지출의료비)) + 
#   geom_histogram() + 
#   scale_y_continuous(labels = scales::comma)+ 
#   scale_x_continuous(labels = scales::comma)

## 고령
boxplot(data[data$연령대 == '고령', '개인지출의료비'])$stats
summary(data[data$연령대 == '고령', '개인지출의료비'])

ggplot(data = data %>% filter(연령대 == '고령'), aes(y = 개인지출의료비)) + 
  geom_boxplot()+ 
  scale_y_continuous(labels = scales::comma)

ggplot(data = data %>% filter(연령대 == '고령'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  scale_y_continuous(labels = scales::comma)

p4 <- ggplot(data = data %>% filter(연령대 == '고령'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  ylab(' ')+
  #ggtitle('고령 연간 개인지출의료비 상자그림(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))+
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(ylim = c(0, 3500000)) # 이상치 제거 상자그림

# ggplot(data=data, aes(x=연령대, 개인지출의료비, fill = 연령대))+
#   scale_y_continuous(labels = scales::comma) +
#   theme_minimal() +
#   geom_boxplot(outlier.shape = NA) +
#   xlab('') + 
#   ylab('연간 개인지출의료비(단위: 원)') +
#   scale_x_discrete(limits=c("미성년", "청년", "중년", "고령")) +
#   ggtitle('연령별 연간 개인지출의료비 상자그림') +
#   theme(plot.title = element_text(face='bold',
#                                   size=20,
#                                   hjust = .5),
#         legend.position = "")

# library(gridExtra)
# grid.arrange(p1, p2, p3, p4, ncol = 4) 

# 연령별 연간 개인지출의료비 분포 확인(histogram)
# qplot(data = data, 개인지출의료비, fill=연령대, geom="histogram") + 
#   scale_x_continuous(labels = scales::comma) +
#   scale_y_continuous(labels = scales::comma) +
#   theme_minimal() +
#   facet_grid(연령대~.)

# 연령별 연간 개인지출의료비 분포 확인(bar chart)
# data_age <- data %>% group_by(연령대) %>% summarise(n = n(), avg_price = mean(개인지출의료비))
# data_age
# 
# ggplot(data = data_age, aes(x = 연령대, y = avg_price, fill = 연령대)) +
#   geom_col(width = .8) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_x_discrete(limits=c("미성년", '청년', '중년', '고령')) +
#   theme_minimal() +
#   geom_text(aes(label = paste0(comma(avg_price, format = 'd'), '원')), vjust=-.25, 
#             position = position_dodge(.9), size = 4) +
#   geom_text(aes(label = paste0(comma(n, format='d'), '명')), colour = 'white', position = position_stack(vjust=0.5)) +
#   # scale_fill_viridis_d() +
#   xlab('') +
#   ylab('연간 평균 개인지출의료비(단위: 원)') +
#   ggtitle('연령별 연간 평균 개인지출의료비') +
#   theme(plot.title = element_text(face='bold',
#                                   size=20,
#                                   hjust = .5),
#         legend.position = " ")

# 연령대 분포
# ggplot(data, aes(x=나이)) + 
#   geom_histogram(binwidth = 1.9) +
#   theme_minimal() +
#   ylab('응답자 수(명)') +
#   ggtitle('응답자 연령대 히스토그램')+
#   scale_x_continuous(breaks=seq(min(data$나이), max(data$나이), by = 10)) +
#   scale_x_continuous(breaks=seq(min(data$나이), max(data$나이), by = 10)) +
#   theme(plot.title = element_text(face='bold',
#                                   size=20,
#                                   hjust = .5))
# 
# max(data$나이) # 102세
# min(data$나이) # 19세

# 미성년 이상치 제거 후 개인지출의료비 평균
boxplot(data[data$연령대 == '미성년', '개인지출의료비'])$stats
age_min <- data[data$연령대 == '미성년', ]
age_min$개인지출의료비 <- ifelse(age_min$개인지출의료비 < 0 | age_min$개인지출의료비 > 261650, 
                          NA, age_min$개인지출의료비)
mean(age_min$개인지출의료비, na.rm = T)
sum(is.na(age_min$개인지출의료비)) # NA 개수

# 청년 이상치 제거 후 개인지출의료비 평균
boxplot(data[data$연령대 == '청년', '개인지출의료비'])$stats
age_young <- data[data$연령대 == '청년', ]
age_young$개인지출의료비 <- ifelse(age_young$개인지출의료비 < 0 | age_young$개인지출의료비 > 752040, 
                            NA, age_young$개인지출의료비)
mean(age_young$개인지출의료비, na.rm = T)
sum(is.na(age_young$개인지출의료비)) # NA 개수

# 중년 이상치 제거 후 개인지출의료비 평균
boxplot(data[data$연령대 == '중년', '개인지출의료비'])$stats
age_long <- data[data$연령대 == '중년', ]
age_long$개인지출의료비 <- ifelse(age_long$개인지출의료비 < 0 | age_long$개인지출의료비 > 1852160, 
                           NA, age_long$개인지출의료비)
mean(age_long$개인지출의료비, na.rm = T)
sum(is.na(age_long$개인지출의료비)) # NA 개수

# 고령 이상치 제거 후 개인지출의료비 평균
boxplot(data[data$연령대 == '고령', '개인지출의료비'])$stats
age_old <- data[data$연령대 == '고령', ]
age_old$개인지출의료비 <- ifelse(age_old$개인지출의료비 < 0 | age_old$개인지출의료비 > 1852160, 
                          NA, age_old$개인지출의료비)
mean(age_old$개인지출의료비, na.rm = T)
sum(is.na(age_old$개인지출의료비)) # NA 개수


# 연령별 연간 중앙값 개인지출의료비
dt_age <- data %>% group_by(연령대) %>% summarise(n = n(), median_price = median(개인지출의료비))
dt_age

ggplot(data = dt_age, aes(x = reorder(연령대, median_price), y = median_price, fill = 연령대)) +
  geom_col(width = .8) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  geom_text(aes(label = paste0(comma(median_price, format = 'd'), '원')), vjust=-.25, 
            position = position_dodge(.9), size = 4) +
  geom_text(aes(label = paste0(comma(n, format='d'), '명')), colour = 'white',
  position = position_stack(vjust=0.5)) +
  xlab('') +
  ylab('연간 개인지출의료비(단위: 원)') +
  ggtitle('연령별 연간 개인지출의료비 중앙값') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ")

# 연령별 연간 평균 개인지출의료비(이상치 제거)
df <- data.frame('ageg' = c('미성년', '청년', '중년', '고령'),
                 'avg_price' = c(65507,	119636,	373019,	601482),
                 'n' = c(52, 2981, 6001, 4379))

ggplot(df, aes(x = reorder(ageg, avg_price), y = avg_price, fill = ageg)) + 
  geom_col(width = .8) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  xlab('') +
  ylab('연간 평균 개인지출의료비(단위: 원)') +
  geom_text(aes(label = paste0(comma(avg_price, format='d'), '원')), vjust=-.25, position = position_dodge(.9), size = 4) +
  geom_text(aes(label = paste0(comma(n, format='d'), '명')), colour = 'white', position = position_stack(vjust=0.5)) +
  ggtitle('연령별 연간 평균 개인지출의료비(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ")

# 장애여부별 연간 개인지출의료비 분포 확인(hist, box, mean, median plot 작성)----

# 비장애인 연간 개인지출의료비 상자그림(이상치 제거)
boxplot(data[data$장애여부 == '비장애인', '개인지출의료비'])$stats
# data[data$장애여부 == '비장애인', '개인지출의료비']
summary(data[data$장애여부 == '비장애인', '개인지출의료비'])

age_nor <- data[data$장애여부 == '비장애인', ]
age_nor$개인지출의료비 <- ifelse(age_nor$개인지출의료비 < 0 | age_nor$개인지출의료비 > 2087800, 
                          NA, age_nor$개인지출의료비)
mean(age_nor$개인지출의료비, na.rm = T)
sum(is.na(age_nor$개인지출의료비)) # NA 개수

ggplot(data = data %>% filter(장애여부 == '비장애인'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  ylab(' ')+
  ggtitle('비장애인 연간 개인지출의료비 상자그림(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))+
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 2087800)) # 이상치 제거 상자그림


# 장애인 연간 개인지출의료비 상자그림(이상치 제거)
boxplot(data[data$장애여부 == '장애인', '개인지출의료비'])$stats
# data[data$장애여부 == '장애인', '개인지출의료비'] # 949 rows
summary(data[data$장애여부 == '장애인', '개인지출의료비'])

age_dis <- data[data$장애여부 == '장애인', ]
age_dis$개인지출의료비 <- ifelse(age_dis$개인지출의료비 < 0 | age_dis$개인지출의료비 > 2910135, 
                          NA, age_dis$개인지출의료비)
mean(age_dis$개인지출의료비, na.rm = T)
sum(is.na(age_dis$개인지출의료비)) # NA 개수

ggplot(data = data %>% filter(장애여부 == '장애인'), aes(y = 개인지출의료비)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  ylab(' ')+
  ggtitle('장애인 연간 개인지출의료비 상자그림(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5))+
  scale_y_continuous(labels = scales::comma) + 
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, 2910135)) # 이상치 제거 상자그림


# 장애여부별 연간 개인지출의료비 상자그림(이상치 제거)
boxplot(개인지출의료비 ~ 장애여부, data, 
               cex.main = 1.6, cex.axis = 1.2, 
               # las = 1,
               outline = F, 
               ylim = c(0, 3000000), 
               xlab = '', ylab = '개인지출의료비(단위: 원)',
               main = '장애여부별 연간 개인지출의료비 상자그림(이상치 제거)') 
grid(ny=20)
boxplot(개인지출의료비 ~ 장애여부, data, 
               cex.main = 1.6, cex.axis = 1.2, 
               # las = 1,
               outline = F, 
               ylim = c(0, 3000000), 
               xlab = '', ylab = '개인지출의료비(단위: 원)',
               main = '장애여부별 연간 개인지출의료비 상자그림(이상치 제거)', add = TRUE, ann = FALSE) 


# 장애여부별 연간 평균 개인지출의료비(이상치 제거)
df_yn <- data.frame('yn' = c('비장애인', '장애인'),
                 'avg_price' = c(429247,	658089),
                 'n' = c(12464, 949))

ggplot(df_yn, aes(x = reorder(yn, avg_price), y = avg_price, fill = yn)) + 
  geom_col(width = .57) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  xlab('') +
  ylab('연간 평균 개인지출의료비(단위: 원)') +
  geom_text(aes(label = paste0(comma(avg_price, format='d'), '원')), vjust=-.25, position = position_dodge(.9), size = 4) +
  geom_text(aes(label = paste0(comma(n, format='d'), '명')), colour = 'white', position = position_stack(vjust=0.5)) +
  ggtitle('장애여부별 연간 평균 개인지출의료비(이상치 제거)') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ")


# 장애여부별 연간 중앙값 개인지출의료비
dt_yn <- data %>% group_by(장애여부) %>% summarise(n = n(), median_price = median(개인지출의료비))
dt_yn

ggplot(data = dt_yn, aes(x = reorder(장애여부, median_price), y = median_price, fill = 장애여부)) +
  geom_col(width = .57) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  geom_text(aes(label = paste0(comma(median_price, format = 'd'), '원')), vjust=-.25, 
            position = position_dodge(.9), size = 4) +
  geom_text(aes(label = paste0(comma(n, format='d'), '명')), colour = 'white',
            position = position_stack(vjust=0.5)) +
  xlab('') +
  ylab('연간 개인지출의료비(단위: 원)') +
  ggtitle('장애여부별 연간 개인지출의료비 중앙값') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ")

# 다항 로지스틱 회귀분석----

table(data$운동능력) # 12,880개
sum(is.na(data)) # 547(운동능력 -> 533개)

# NA 행 및 나이 변수 제거
vglm_data <- na.omit(data) %>% select(-나이)
str(vglm_data)

# 범주형 변수 -> factor형 변환(chr -> factor)
vglm_data$운동능력 <- factor(vglm_data$운동능력,
                         levels = c('걷기 지장 없음', '걷기 다소 지장 있음', '종일 누워있음'))

# table(vglm_data$장애여부)
vglm_data$장애여부 <- factor(vglm_data$장애여부, levels = c('비장애인', '장애인'))

vglm_data$성별 <- factor(vglm_data$성별, levels = c('남', '여'))

vglm_data$연령대 <- factor(vglm_data$연령대, levels = c('미성년', '청년', '중년', '고령'))
# vglm_data$연령대 <- relevel(vglm_data$연령대, '고령')

# table(vglm_data$와병률)
vglm_data$와병률 <- factor(vglm_data$와병률, levels = c('아니오', '예'))

# table(vglm_data$결근결석)
vglm_data$결근결석 <- factor(vglm_data$결근결석, levels = c('학교/직장 안다님', '아니오', '예'))
# vglm_data$결근결석 <- relevel(vglm_data$결근결석, '아니오')

# table(vglm_data$시력문제)
vglm_data$시력문제 <- factor(vglm_data$시력문제,
                         levels = c('문제 없음', '조금 문제 있음', '많이 문제 있음', '전혀 보지 못함'))
# vglm_data$시력문제 <- relevel(vglm_data$시력문제, '전혀 보지 못함')

vglm_data$청력문제 <- factor(vglm_data$청력문제,
                         levels = c('문제 없음', '조금 문제 있음', '많이 문제 있음', '전혀 듣지 못함'))
# vglm_data$청력문제 <- relevel(vglm_data$청력문제, '전혀 듣지 못함')

vglm_data$기억력 <- factor(vglm_data$기억력, levels = c('문제 없음', '문제 있음'))

vglm_data$의사결정 <- factor(vglm_data$의사결정,levels = c('문제 없음', '문제 있음'))

vglm_data$`질병/손상 등으로 활동제한` <- factor(vglm_data$`질병/손상 등으로 활동제한`,
                                     levels = c('문제 없음', '문제 있음'))

str(vglm_data)

# 회귀분석 실시
library(car) # 다중공선성
library(rms)
library(nnet)

### vglm 함수 사용
# pid.mlogit <- vglm(운동능력 ~., family  = multinomial(), data=vglm_data) # 모형 생성
# summary(pid.mlogit) # 결과 도출
# data.frame(exp(coef(pid.mlogit))) # 오즈비 확인
# vif(pid.mlogit) # 다중공선성 -> 오류
# anova(pid.mlogit, test="Chisq") # ANOVA(범주 3개) -> 오류 발생
# qchisq(0.95, df= 25722) # 임계치(26096.21)보다 잔차 이탈도(Residual deviance = 6291.386)가 작으므로 모형은 적합하다.

# install.packages('rms')
lrm(pid.mlogit)


### multinom 함수 사용
# pid.mlogit1 <- multinom(운동능력 ~., family  = multinomial(), data=vglm_data) # 모형 생성
# summary(pid.mlogit1) # 결과 도출
# data.frame(exp(coef(pid.mlogit1))) # 오즈비 계산
# vif(pid.mlogit1) # 다중공선성 -> NAN 
# anova(pid.mlogit1, test="Chisq") # ANOVA(범주 3개) -> 오류 발생

### 수치형 변수로 변환
vglm_data_num <- vglm_data

vglm_data_num$장애여부 <- as.integer(vglm_data_num$장애여부) # 1: 비장애인, 2: 장애인
vglm_data_num$성별  <- as.integer(vglm_data_num$성별) # 1: 남자, 2: 여자
vglm_data_num$운동능력  <- as.factor(vglm_data_num$운동능력) 
vglm_data_num$와병률  <- as.integer(vglm_data_num$와병률) # 1: 아니오, 2: 예
vglm_data_num$결근결석  <- as.integer(vglm_data_num$결근결석) # 1: 학교/직장 안다님, 2: 아니오, 3: 예
vglm_data_num$시력문제  <- as.integer(vglm_data_num$시력문제) # 1: 문제없음, 2: 조금 문제 있음, 3: 많이 문제 있음, 4: 전혀 보지 못함
vglm_data_num$청력문제  <- as.integer(vglm_data_num$청력문제) # 1: 문제없음, 2: 조금 문제 있음, 3: 많이 문제 있음, 4: 전혀 듣지 못함
vglm_data_num$기억력  <- as.integer(vglm_data_num$기억력) # 1: 문제없음, 2: 문제있음
vglm_data_num$의사결정 <- as.integer(vglm_data_num$의사결정) # 1: 문제없음, 2: 문제있음
vglm_data_num$`질병/손상 등으로 활동제한` <- as.integer(vglm_data_num$`질병/손상 등으로 활동제한`)  # 1: 문제없음, 2: 문제있음
vglm_data_num$연령대 <- as.integer(vglm_data_num$연령대) # 1: 미성년, 2: 청년, 3: 중년, 4: 고령
str(vglm_data_num)

# pid.mlogit2 <- multinom(운동능력 ~., family  = multinomial(), data=vglm_data_num) # 모형 생성
# summary(pid.mlogit2) # 결과 도출
# exp(coef(pid.mlogit2)) # 오즈비 계산
# vif(pid.mlogit2) # 다중공선성 -> NAN
# anova(pid.mlogit1) # ANOVA(범주 3개) -> 오류 발생(argument is of length zero)

# 변수 중요도(랜덤 포레스트)----
# install.packages('randomForest')
# library(randomForest)
# vglm_data_pre <- vglm_data
# 
# vglm_data_pre <- dplyr::rename(vglm_data_pre, '질병손상_활동제한' = '질병/손상 등으로 활동제한')
# write_xlsx(vglm_data_pre, 'C:/python/pydata/운동능력 데이터.xlsx')
# 
# m <- randomForest(운동능력 ~., data=vglm_data_pre, importance=TRUE)
# m$importance
# order(importance(m)[,MeanDecreaseAccuracy], decreasing=T) 
# importance(m)
# varImpPlot(m) # 시각화

# 순서형 로지스틱 회귀분석----
# install.packages('MASS')
# library(MASS)
# ologit <- polr(운동능력 ~ ., data = vglm_data, method = c('logistic'))
# summary(ologit)
# exp(coef(polr(운동능력 ~ ., data = vglm_data, Hess=TRUE)))


# install.packages('oglmx')
library(oglmx)
# summary(oglmx(운동능력 ~ ., data=vglm_data, link="logit", constantMEAN = FALSE, constantSD = FALSE, delta=0, threshparam = NULL))
# exp(cbind(coef(oglmx(운동능력 ~ ., data=vglm_data, link="logit", constantMEAN = FALSE, constantSD = FALSE, delta=0, threshparam = NULL)), 
#          confint.default(oglmx(운동능력 ~ ., data=vglm_data, link="logit", constantMEAN = FALSE, constantSD = FALSE, delta=0, threshparam = NULL))))

# summary(oglmx(운동능력 ~., data = vglm_data_num, link="logit", constantMEAN = FALSE, constantSD = FALSE, delta=0, threshparam = NULL))
# exp(coef(oglmx(운동능력 ~., data = vglm_data_num, link="logit", constantMEAN = FALSE, constantSD = FALSE, delta=0, threshparam = NULL)))

dff1 <- vglm_data_num %>% filter(결근결석 != 1) # 결근결석 제거

model1 <- oglmx(운동능력 ~., data = dff1, link="logit", constantMEAN = FALSE, constantSD = FALSE, delta=0, threshparam = NULL)
summary(model1)
exp(coef(model1))
# vif(model1)

# install.packages('lmtest')
# library(lmtest)
# lrtest(model)


# 운동능력 응답별 개인지출의료비 그래프----
## 운동능력에 따른 개인지출의료비 분포(hist)
# ggplot(vglm_data, aes(x = 개인지출의료비)) + 
#   geom_histogram()+ 
#   facet_grid(운동능력 ~.)

## 전체 지출의료비 기준으로 이상치 -> NA 정제
vglm_data$non_outlier_price <- ifelse(vglm_data$개인지출의료비 < 0 | vglm_data$개인지출의료비 > 2199760, 
                                      NA, vglm_data$개인지출의료비)

sum(is.na(vglm_data$non_outlier_price)) # 1,157개
vglm_data %>% filter(운동능력 == '걷기 지장 없음') %>% select(non_outlier_price)

vglm_data %>% 
  group_by(운동능력) %>% 
  summarise(median_price = median(개인지출의료비), # 중앙값
            mean_outlier = mean(개인지출의료비),   # 평균(이상치 포함)
            mean_non_outlier = mean(non_outlier_price, na.rm = T)) # 평균(이상치 제거)


## '걷기 지장 없음' 기준 이상치 제거 전후 평균 비교
boxplot(vglm_data[vglm_data$운동능력 == '걷기 지장 없음', '개인지출의료비'])$stats
non <- data.frame(price = vglm_data[vglm_data$운동능력 == '걷기 지장 없음', '개인지출의료비'],
                  na_outlier = vglm_data[vglm_data$운동능력 == '걷기 지장 없음', '개인지출의료비']) # df 생성
non$na_outlier <- ifelse(non$na_outlier < 0 | non$na_outlier > 1932990, NA, non$na_outlier) # 이상치 -> NA
non %>% summarise(mean_price = mean(non$price), # 평균(이상치 포함)
                  non_outlier_mean = mean(non$na_outlier, na.rm = T)) # 평균(이상치 제거)

## '걷기 다소 지장 있음 이상치 제거 전후 평균 비교
boxplot(vglm_data[vglm_data$운동능력 == '걷기 다소 지장 있음', '개인지출의료비'])$stats
sick <- data.frame(price = vglm_data[vglm_data$운동능력 == '걷기 다소 지장 있음', '개인지출의료비'],
                  na_outlier = vglm_data[vglm_data$운동능력 == '걷기 다소 지장 있음', '개인지출의료비']) # df 생성
sick$na_outlier <- ifelse(sick$na_outlier < 0 | sick$na_outlier > 3897430, NA, sick$na_outlier) # 이상치 -> NA
sick %>% summarise(mean_price = mean(sick$price), # 평균(이상치 포함)
                  non_outlier_mean = mean(sick$na_outlier, na.rm = T)) # 평균(이상치 제거)

## '종일 누워있음' 이상치 제거 전후 평균 비교
boxplot(vglm_data[vglm_data$운동능력 == '종일 누워있음', '개인지출의료비'])$stats
very_sick <- data.frame(price = vglm_data[vglm_data$운동능력 == '종일 누워있음', '개인지출의료비'],
                   na_outlier = vglm_data[vglm_data$운동능력 == '종일 누워있음', '개인지출의료비']) # df 생성
very_sick$na_outlier <- ifelse(very_sick$na_outlier < 0 | very_sick$na_outlier > 6849450, NA, very_sick$na_outlier) # 이상치 -> NA
very_sick %>% summarise(mean_price = mean(very_sick$price), # 평균(이상치 포함)
                   non_outlier_mean = mean(very_sick$na_outlier, na.rm = T)) # 평균(이상치 제거)

## 운동능력에 따른 개인지출의료비(중앙값, 평균_이상치_포함, 평균_이상치_제거)
excerise_price = data.frame(운동능력 = c('걷기 지장 없음', '걷기 다소 지장 있음', '종일 누워있음'),
                            중앙값 = c(271700, 767060, 722430),
                            mean = c(718711.7, 1410560, 2103669),
                            mean_non_outlier = c(396752.8, 962164.3, 1529078))
melt_excerise_price <- melt(excerise_price, id.vars = c('운동능력'))

label = c(
  중앙값 = '중앙값',
  mean = '평균(이상치 포함)',
  mean_non_outlier = '평균(이상치 제거)'
)

ggplot(melt(excerise_price, id.vars = c('운동능력')), aes(x = 운동능력, y = value, fill = 운동능력)) + 
  geom_col(width = .8) +
  facet_wrap('variable', labeller = labeller(variable = label)) +
  theme_bw() +
  scale_x_discrete(limits = c('걷기 지장 없음', '걷기 다소 지장 있음', '종일 누워있음')) +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ")+
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = paste0(comma(value, format='d'), '원')), vjust=-.4, size = 4) +
  ggtitle('운동능력에 따른 개인지출의료비') +
  theme(plot.title = element_text(face='bold',
                                  size=20,
                                  hjust = .5),
        legend.position = " ",
        axis.text.x = element_text(angle = 12, hjust = 0.5, vjust = .7))+
  ylab('연간 개인지출의료비(단위: 원)')
