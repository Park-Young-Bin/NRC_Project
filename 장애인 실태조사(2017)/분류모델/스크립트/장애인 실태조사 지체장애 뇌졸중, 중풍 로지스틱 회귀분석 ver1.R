library(gmodels)
library(writexl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape)

# 1. 데이터 불러오기 및 id 컬럼 생성----
## 1) 원본
raw_df <- read_xlsx('C:/python/pydata/2017 장애인실태조사_최종공개.xlsx', sheet=2)

## 2) 지체장애인 + 등록장애인만 추출
df <- raw_df %>% filter(지체장애여부 == 1 & 장애등급...20 != 0)

## 3) 행 인덱스 번호 생성
id <- seq(1, nrow(df))
df <- cbind(df, id)
df <- df %>% relocate(c(id)) # 열 순서 변경

colnames(df)

## 4) 뇌졸중, 중풍 응답자만 출력(0 제외)
stroke <- df %>% filter(`02)만성질환명(뇌졸중,중풍)` != 0) # 2,686개 행 존재

# 2. 연속형 변수 테이블 생성----
## 생년, 장애등록 연도, 본인을 포함한 총 가구원수, 본인을 포함한 총 장애인수, 월 평균 총가구소득, 가구 주된 수입원,
## 가구 월평균 지출액, 장애발생 시 연령, 월 혈압약 일수, 관절통증 정도, 운동 시간(분), 키, 몸무게
continu_df <- stroke %>% 
  select(id, 생년, `장애등록 연도`, `본인을 포함한 총 가구원수`, `본인을 포함한 총 장애인수`, 
         `월 평균 총가구소득`, `가구 주된 수입원`, `가구 월평균 지출액`, `장애발생시 연령...36`, 
         `월 혈압약 일수`, `관절통증 정도`, `운동 시간(분)`, `키(센티)`, `몸무게(kg)`)

## 결측치 제거 (0개)
# sum(is.na(continu_df))
# continu_df$`관절통증 정도` <- ifelse(continu_df$`관절통증 정도` == 99, NA, continu_df$`관절통증 정도`)

# 3. 연속형 변수 스케일링----
## 연속형 변수(min-max)
# install.packages('caret')
library(caret)
model_minmax = preProcess(x = continu_df[, 2:14], method = "range")
continu_mm_df <- predict(model_minmax, continu_df)

# 4. 범주형 변수 테이블 생성----
## 1) 가구 판별1, 2 중 범주형 변수 출력
### 조사표 종류, 가구원 일련번호(장애인, 응답자), 조사지역, 응답자 유형, 대리응답이유, 장애유형확인1,2,3 제거
### 등록장애유형1,2, 가구주와의 관계, 개인번호, 주된 장애유형 제거
### 이유, 인식 등 상관 없는 문항 제거
catego_df <- stroke %>%
  select(-c(`만 나이`, 생월, 생년, `장애등록 연도`, `본인을 포함한 총 가구원수`, `본인을 포함한 총 장애인수`, 
            `월 평균 총가구소득`, `가구 주된 수입원`, `가구 월평균 지출액`, `장애발생시 연령...36`, 
            `월 혈압약 일수`, `관절통증 정도`, `운동 시간(분)`, `키(센티)`, `몸무게(kg)`, # 연속형 변수 제거
            
            `조사표 종류`, `가구원 일련번호(장애인)`, `가구원 일련번호(응답자)`, `조사지역(시도)`, 
            `응답자 유형`, `대리응답이유`, `장애유형확인1`, `장애유형확인2`, `장애유형확인3`, `장애등록 여부`, `장애등록여부`,
            "등록장애유형(1순위) ", `등록장애유형(2순위)`, 개인번호, `주택 형태`, `가구주와의 관계(가구주)`,`주된 장애유형`, '지체장애여부',
            
            "재활치료서비스를 하나도 이용하지 않은 경우 이유", "휴대폰 사용하지 않는 이유", "필요한데 구입하지 않은 이유",
            "장애차별에 대한 인식", "장애인차별금지법 인지...29", "진료장소", "진료목적", "현재 지속적 진료과",                               
            "희망 지속적 진료과", "현재 진료받지않는 가장 중요한\r\n이유", "건강검진 내용", "건강검진 못받은 이유", 
            "의료진 장애 이해정도", "의료서비스 만족도", "의료시설 및 장비 만족도", "미충족 의료 이유 1", "미충족 의료 이유 2", "만성질환 유무",
            
            "향후 강화필요 보건의료서비스 혹은 기관 (1순위)", "향후 강화필요 보건의료서비스\r\n혹은 기관 (2순위)",
            "운동 장소 (1순위)", "운동 장소 (2순위)", "참여 운동 종목 (1순위)", "참여 운동 종목 (2순위)", "운동하지 않는 주된 이유",
            "칫솔질 횟수", "키나 몸무게 측정경험", "최근 측정 시기", "건강관련 정보획득처", "필요한 건강정보1", "필요한 건강정보2",
            
            "건강주치의 불필요한 이유", "정부(사회) 강화할 보건의료서비스1", "정부(사회) 강화할 보건의료서비스2",
            "도움 부족 이유", "소지하고 있는데 사용 않는 이유", "구입시 외부지원 경험", "가장 많은 지원처", "지원형태", "지원수준 충분도",
            "보조기기 만족도", "불만족 이유", "장애인 보조기기 구입 경로", "이용경험-보조기기 사용교육", "이용경험-사후(A/S)",                         
            "장애인 보조기기 개선사항", "스마트폰 사용하지 않는 이유", "컴퓨터 사용하지 않는 이유", "인터넷 사용하지 않는 이유", '현재 도움 충분도'))

## 2) 제1장. 장애특성 중 범주형 변수 출력(뇌병변~뇌전증 장애 제외)
colnames(catego_df)
catego_df <- catego_df %>% select(-c("뇌병변장애여부":"산업재해인정여부...179"))

# 3) 제2장. 보건의료.건강 중 범주형 변수 출력
### 재활치료서비스 문항 중 '이용여부' 외에 모든 문항 제거
catego_df <- catego_df %>% 
  select(-c("물리치료-이용시간":"물리치료-제공기관(5)")) %>% 
  select(-c("작업치료-이용시간":"작업치료-제공기관 (5)")) %>%
  select(-c("언어치료-이용시간":"언어치료-제공기관 (5)")) %>%
  select(-c("음악치료-이용시간":"음악치료-제공기관 (5)")) %>%
  select(-c("놀이치료-이용시간":"놀이치료-제공기관 (5)")) %>%
  select(-c("미술치료-이용시간":"미술치료-제공기관 (5)")) %>%
  select(-c("심리행동치료-이용시간":"심리행동치료-제공기관 (5)")) %>%
  select(-c("기타-이용시간":"기타-제공기관 (5)"))

## 4) 제3장. 일상생활 중 범주형 변수 출력: 제외할 부분 없음

## 5) 제4장. 장애인보조기기: 주된보조기기 장애번호, 주된보조기기 품목번호 제외
catego_df <- catego_df %>% select(-`주된보조기기 장애번호`, -`주된보조기기 품목번호`)

### 지체장애 해당 보조기기와 전체 보조기기만 추출(나머지 제외)
colnames(catego_df)
catego_df <- catego_df %>% select(-c(`안경(콘텍트렌즈)-필요`:`기타-사용...509`)) # 시각장애 부분 모두 제거
catego_df <- catego_df %>% select(-c(`화상전화기-필요`:`기타-사용...551`)) # 청각장애 中 보청기만 남기고 모두 제거 + 언어장애 모두 제거
catego_df <- catego_df %>% select(-c(`교육과목훈련용보조기구 - 필요`:`기타-사용...596`)) # 지적 및 자폐성 장애 中 의사소통 보조기기만 남기고 모두 제거 + 신장/호흡기/안면/장루/오류 장애 모두 제거

## 6) 제5장~12장 컬럼 모두 제외
colnames(catego_df)
catego_df <- catego_df %>% select(-c(학교:ws_p)) # 완료

## 7) 범주형의 결측값 파악
summary_tb <- as.data.frame(summary(catego_df)) # summary 결과 데이터프레임 변환
summary_tb <- summary_tb %>% filter(grepl('Min.|Max.', Freq)) # Min, Max만 추출
summary_tb <- separate(data = summary_tb, col = Freq, sep='.   :', into = c('summary', 'value'))%>% select(-Var1) # 셀 분할
summary_tb <- cast(data = summary_tb, Var2 ~ summary) %>% relocate(c(Var2, Min, Max)) # 구조 및 컬럼 순서 변경

## 8) 9, 99, 999, 9999 -> NA 변경
catego_df$`주로 도와주는 사람` <- ifelse(catego_df$`주로 도와주는 사람` == 99, NA, catego_df$`주로 도와주는 사람`)
catego_df$`희귀난치성질환 등록 여부` <- ifelse(catego_df$`희귀난치성질환 등록 여부` == 9, NA, catego_df$`희귀난치성질환 등록 여부`)
catego_df$동거여부 <- ifelse(catego_df$동거여부 == 9, NA, catego_df$동거여부)
catego_df$`도움 계속 필요 여부` <- ifelse(catego_df$`도움 계속 필요 여부` == 9, NA, catego_df$`도움 계속 필요 여부`)
catego_df$산업재해인정여부...39 <- ifelse(catego_df$산업재해인정여부...39 == 9, NA, catego_df$산업재해인정여부...39)

## catego_df$`필요한데 구입하지 않은 이유` <- ifelse(catego_df$`필요한데 구입하지 않은 이유` == 99, NA, catego_df$`필요한데 구입하지 않은 이유`)
## catego_df$`현재 도움 충분도` <- ifelse(catego_df$`현재 도움 충분도` == 9, NA, catego_df$`현재 도움 충분도`)
## catego_df$`재활치료서비스를 하나도 이용하지 않은 경우 이유` <- ifelse(catego_df$`재활치료서비스를 하나도 이용하지 않은 경우 이유` == 99, 
##                                                 NA, catego_df$`재활치료서비스를 하나도 이용하지 않은 경우 이유`)
## catego_df$`1일 이용시간` <- ifelse(catego_df$`1일 이용시간` == 9, NA, catego_df$`1일 이용시간`) # 결측값 아님!
## catego_df$`휴대폰 사용하지 않는 이유` <- ifelse(catego_df$`휴대폰 사용하지 않는 이유` == 9, NA, catego_df$`휴대폰 사용하지 않는 이유`)

## 9) 각 컬럼별 결측값 개수 확인
miss_df <-  as.data.frame(apply(X = catego_df, MARGIN = 2, FUN = function(x){sum(is.na(x))}))
miss_df <- cbind(newColName = rownames(miss_df), miss_df)
rownames(miss_df) <- 1:nrow(miss_df)
colnames(miss_df)
miss_df <- dplyr::rename(miss_df, 
                         colname = newColName,
                         missing_value = `apply(X = catego_df, MARGIN = 2, FUN = function(x) {     sum(is.na(x)) })`) %>% 
  arrange(desc(missing_value))

## 10) 빈도가 가장 높은 값으로 대체
# table(catego_df$'주로 도와주는 사람') # 0
# table(catego_df$'희귀난치성질환 등록 여부') # 0
# table(catego_df$'산업재해인정여부...39') # 0
# table(catego_df$'동거여부') # 0
# table(catego_df$'도움 계속 필요 여부') # 0

catego_df$`주로 도와주는 사람` <- ifelse(is.na(catego_df$`주로 도와주는 사람`), 0, catego_df$`주로 도와주는 사람`)
catego_df$`희귀난치성질환 등록 여부` <- ifelse(is.na(catego_df$`희귀난치성질환 등록 여부`), 0, catego_df$`희귀난치성질환 등록 여부`)
catego_df$`산업재해인정여부...39` <- ifelse(is.na(catego_df$`산업재해인정여부...39`), 0, catego_df$`산업재해인정여부...39`)
catego_df$`동거여부` <- ifelse(is.na(catego_df$`동거여부`), 0, catego_df$`동거여부`)
catego_df$`도움 계속 필요 여부` <- ifelse(is.na(catego_df$`도움 계속 필요 여부`), 0, catego_df$`도움 계속 필요 여부`)

sum(is.na(catego_df)) # 결측값 0개

# 5. y 레이블(만성질환)만 출력----
y <- catego_df %>% 
  select(id, "01)만성질환명(고혈압)", "02)만성질환명(뇌졸중,중풍)",  "03)만성질환명(심근경색증)", "04)만성질환명(협심증)", 
         "05)만성질환명(이상지혈증)", "06)만성질환명(당뇨병)", "07)만성질환명(갑상선장애)", "08)만성질환명(천식)", 
         "09)만성질환명(폐결핵)", "10)만성질환명(폐질환(만성기관 지염,폐기종)", "11)만성질환명(위십이지장궤양)", 
         "12)만성질환명(B형간염)" , "13)만성질환명(C형간염)", "14)만성질환명(간경변증)", "15)만성질환명(신부전)",
         "16)만성질환명(골관절염(퇴행성\r\n관절염))", "17)만성질환명(류마티스 관절염)", "18)만성질환명(골다공증)", 
         "19)만성질환명(척추측만증)", "20)만성질환명(허리목통증)", "21)만성질환명(피부염)", "22)만성질환명(백내장)", 
         "23)만성질환명(우울증)", "24)만성질환명(암)", "25)만성질환명(기타)")

## 1) y 레이블 0 or 1로 변환
for(i in 2:ncol(y)) {    # 열 개수
  for(j in 1:nrow(y)) {   # 행 개수
    if (y[j, i] == 2) {  # (j, i) 값이 2이면
      y[j, i] = 0       # 0으로 변환
    }
  }
}

# 6. catego_df 中 y 레이블 제거----
catego_noty_df <- catego_df %>% 
  select(-c("01)만성질환명(고혈압)", "02)만성질환명(뇌졸중,중풍)",  "03)만성질환명(심근경색증)", "04)만성질환명(협심증)", 
            "05)만성질환명(이상지혈증)", "06)만성질환명(당뇨병)", "07)만성질환명(갑상선장애)", "08)만성질환명(천식)", 
            "09)만성질환명(폐결핵)", "10)만성질환명(폐질환(만성기관 지염,폐기종)", "11)만성질환명(위십이지장궤양)", 
            "12)만성질환명(B형간염)" , "13)만성질환명(C형간염)", "14)만성질환명(간경변증)", "15)만성질환명(신부전)",
            "16)만성질환명(골관절염(퇴행성\r\n관절염))", "17)만성질환명(류마티스 관절염)", "18)만성질환명(골다공증)", 
            "19)만성질환명(척추측만증)", "20)만성질환명(허리목통증)", "21)만성질환명(피부염)", "22)만성질환명(백내장)", 
            "23)만성질환명(우울증)", "24)만성질환명(암)", "25)만성질환명(기타)"))

str(catego_noty_df) # 모두 수치형으로 이루어짐
catego_noty_df[ , colnames(catego_noty_df)] = lapply(catego_noty_df[ , colnames(catego_noty_df)], factor) # 모든 변수를 factor형 변환

# 7. 범주형 변수 원핫인코딩 수행----
# 참고: https://didalsgur.tistory.com/entry/R-%EB%B2%94%EC%A3%BC%ED%98%95-%EB%B3%80%EC%88%98-%EB%B3%80%ED%99%98-One-Hot-Encoding
## 1) factor형 변환
# str(catego_df) # 모두 수치형으로 이루어짐
# catego_df[ , colnames(catego_df)] = lapply(catego_df[ , colnames(catego_df)], factor) # 모든 변수를 factor형 변환

## 2) 원핫인코딩 수행
# install.packages('mltools')
library(mltools)
library(data.table)

catego_onehot_df <- one_hot(as.data.table(catego_noty_df[, 2:251])) # 인코딩
catego_onehot_df <- cbind(data.frame(catego_noty_df$id), catego_onehot_df) # id 칼럼 결합
catego_onehot_df <- dplyr::rename(catego_onehot_df, id=`catego_noty_df.id`)

# 8. 연속형, 범주형 테이블 merge(on = 'id')----
merge_df <- merge(continu_mm_df, catego_onehot_df, by = 'id')
new_df <- merge(merge_df, y, by='id') # 2,686개 행 존재

# 9. 로지스틱 회귀분석----
options(max.print=1000000)
logit_df <- new_df %>% select(-c(id))
colnames(logit_df)

## 1) 1차 부분별 회귀분석
a1 <- logit_df %>% select(c(1:47, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(a1$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=a1))

b1 <- logit_df %>% select(c(48:153, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(b1$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=b1))

c1 <- logit_df %>% select(c(154:262, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(c1$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=c1))

d1 <- logit_df %>% select(c(263:413, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(d1$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=d1))

e1 <- logit_df %>% select(c(414:526, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(e1$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=e1))

f1 <- logit_df %>% select(c(527:638, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(f1$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=f1))

g1 <- logit_df %>% select(c(639:830, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(g1$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=g1))

h1 <- logit_df %>% select(c(665:856, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(h1$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=h1))

## 2) 2차 부분별 회귀분석
colnames(logit_df)
logit_df2 <- logit_df %>% select(-c(3:6, 8, 10:15, 38:41, 46:47, 263:346, 351:413, 414:421, 426:429, 434:493, 498:523, 
                                    650:651, 677:678, 683:684, 703:704, 707:710, 715:716, 719:720, 725:726, 731:732, 737:738, 
                                    743:744, 747:748, 753:754, 762:764, 767:768, 771:772, 784:785, 790:791, 796:797, 802:803))
colnames(logit_df2)

a2 <- logit_df2 %>% select(c(1:136, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(a2$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=a2))

b2 <- logit_df2 %>% select(c(137:188, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(b2$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=b2))

c2 <- logit_df2 %>% select(c(189:264, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(c2$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=c2))

d2 <- logit_df2 %>% select(c(265:376, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(d2$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=d2))

e2 <- logit_df2 %>% select(c(377:446, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(e2$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=e2))

f2 <- logit_df2 %>% select(c(447:525, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(f2$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=f2))

## 3) 3차 부분별 회귀분석
colnames(logit_df2)
logit_df3 <- logit_df2 %>% select(-c(137:141, 145:146, 152:188, 447:460, 463:471, 474:519, 522:525))
colnames(logit_df3)

a3 <- logit_df3 %>% select(c(1:136, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(a3$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=a3))

b3 <- logit_df3 %>% select(c(137:201, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(b3$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=b3))

c3 <- logit_df3 %>% select(c(202:220, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(c3$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=c3))

d3 <- logit_df3 %>% select(c(221:269, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(d3$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=d3))

e3 <- logit_df3 %>% select(c(270:364, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(e3$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=e3))

f3 <- logit_df3 %>% select(c(365:408, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(f3$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=f3))

# summary(glm(logit_df3$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=logit_df3))

## 4) 4차 부분별 회귀분석
colnames(logit_df3)
logit_df4 <- logit_df3 %>% select(-c(148:165, 188:201, 206:213, 218:220, 
                                     221:234, 237:241, 244:266,
                                     270:322, 326:329, 333:336, 340:341, 344:347, 351:364,
                                     365:406))
colnames(logit_df4)

a4 <- logit_df4 %>% select(c(1:64, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(a4$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=a4))

b4 <- logit_df4 %>% select(c(65:136, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(b4$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=b4)) 

c4 <- logit_df4 %>% select(c(137:177, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(c4$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=c4))

d4 <- logit_df4 %>% select(c(178:200, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(d4$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=d4)) 

## 5) 5차 부분별 회귀분석
colnames(logit_df4)
logit_df5 <- logit_df4 %>% select(-c(2, 62:64, 91:122, 137:139, 178:179, 185:187, 194:198))
colnames(logit_df5)

a5 <- logit_df5 %>% select(c(1:86, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(a5$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=a5))

b5 <- logit_df5 %>% select(c(87:151, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(b5$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=b5))

## 6) 6차 부분별 회귀분석
colnames(logit_df5)
logit_df6 <- logit_df5 %>% select(-c(26:29, 101:105, 128:130, 135:149))
colnames(logit_df6)

a6 <- logit_df6 %>% select(c(1:124, `02)만성질환명(뇌졸중,중풍)`))
summary(glm(a6$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=a6))

# b6 <- logit_df6 %>% select(c(87:151, `02)만성질환명(뇌졸중,중풍)`))
# summary(glm(b6$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=b6))

summary(glm(logit_df6$`02)만성질환명(뇌졸중,중풍)` ~ ., family=binomial, data=logit_df6 %>% select(1:124, 126)))

# 10. 유의한 변수만 추출----
## 정규화 및 원핫인코딩 수행 전의 데이터를 결합 후 추출(continu_df + catego_noty_df + y) 
final_df <- merge(continu_df, catego_noty_df, by = 'id')
final_df <- merge(final_df, y, by = 'id')

colnames(final_df)

final_df <- final_df %>% select(c(생년, "가구 월평균 지출액", "월 혈압약 일수", "장애등급...20", 17, "가장 불편한부위", "장애주된 원인",
                                    "주된 진단명...40", "혈압약 복용 여부", "암 종류1", "암 종류2", "EQ-5D (자기관리)", "스마트폰 사용 여부",
                                    265:289))
write_xlsx(final_df, 'rdata/[로지스틱] 유의한 변수 추출(지체장애 뇌졸중, 중풍).xlsx')
