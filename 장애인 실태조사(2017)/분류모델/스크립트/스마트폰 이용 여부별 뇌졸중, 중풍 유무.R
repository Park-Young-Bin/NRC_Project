library(dplyr)
library(ggplot2)
library(readxl)

# 데이터 불러오기
data <- read_excel('[로지스틱] 유의한 변수 추출(지체장애 뇌졸중, 중풍)ver2.xlsx') %>% 
  select('스마트폰 사용 여부', '02)만성질환명(뇌졸중,중풍)')

# 데이터 타입 확인 및 타입 변환
str(data)
data$`스마트폰 사용 여부` <- as.factor(data$`스마트폰 사용 여부`) # int 변환
data$`02)만성질환명(뇌졸중,중풍)` <- as.factor(data$`02)만성질환명(뇌졸중,중풍)`) # factor 변환

# 그룹화
group_df <- data %>% 
  group_by(`스마트폰 사용 여부`, `02)만성질환명(뇌졸중,중풍)`) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>% 
  mutate(pct = round(n/tot_group*100, 1))

# 출력
group_df

# 값 변환 및 factor의 level 값 변환
group_df$`스마트폰 사용 여부` <- ifelse(group_df$`스마트폰 사용 여부` == 1, '사용', '사용 안 함')
group_df$`02)만성질환명(뇌졸중,중풍)` <- ifelse(group_df$`02)만성질환명(뇌졸중,중풍)` == 1, '유', '무')
group_df$`02)만성질환명(뇌졸중,중풍)` <- factor(group_df$`02)만성질환명(뇌졸중,중풍)`, levels = c('유', '무'))

# 시각화
ggplot(data = group_df, aes(x = `02)만성질환명(뇌졸중,중풍)`, y=pct, fill = `02)만성질환명(뇌졸중,중풍)`)) +
  geom_col() +
  theme_bw() +
  ylab('비울(%)') +
  xlab('뇌졸중/중풍 여부') +
  ggtitle('스마트폰 사용 여부별 뇌졸중/중풍 유무') +
  geom_text(aes(label = paste(pct)), vjust=-.4, size = 4) +
  facet_grid(.~`스마트폰 사용 여부`) +
  theme(plot.title = element_text(face='bold',
                                  size=19,
                                  hjust = .5),
        legend.position = " ")

# 교차분석
library(gmodels)
CrossTable(data$`02)만성질환명(뇌졸중,중풍)`, data$`스마트폰 사용 여부`, chisq = T)

# 로지스틱 회귀분석
df <- read_excel('[로지스틱] 유의한 변수 추출(지체장애 뇌졸중, 중풍)ver2.xlsx') %>% 
  select('스마트폰 사용 여부', '02)만성질환명(뇌졸중,중풍)')

# 값 변경
df$`스마트폰 사용 여부` <- ifelse(df$`스마트폰 사용 여부` == 1, 1, 0)

# factor형 변환
df$`스마트폰 사용 여부` <- factor(df$`스마트폰 사용 여부`, levels = c(0, 1), labels = c('no', 'yes'))
df$`02)만성질환명(뇌졸중,중풍)` <- factor(df$`02)만성질환명(뇌졸중,중풍)`, levels = c(0, 1), labels = c('no', 'yes'))

str(df)

logit <- glm(`02)만성질환명(뇌졸중,중풍)` ~ `스마트폰 사용 여부`, data=df, family=binomial)
summary(logit)
exp(coef(logit))


# 모형의 적합도 검정
pchisq(q = 1140.0 - 1105.1, df = 2685-2684, lower.tail = F) # 이탈도의 차이는 통계적으로 유의하다.
