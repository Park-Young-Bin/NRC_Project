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
df <- raw_df %>% filter(지체장애여부 == 1)
df <- df %>% filter(장애등급 != 0)

## 3) 행 인덱스 번호 생성
id <- seq(1, 3253)
df <- cbind(df, id)
df <- df %>% relocate(c(id)) # 열 순서 변경

colnames(df)

## 4) 이상지혈증 응답자만 출력(0 제외)
Dyslipidemia <- df %>% filter(`05)만성질환명(이상지혈증)` != 0)

# 2. 연속형 변수 테이블 생성----
## 생년, 장애등록 연도, 본인을 포함한 총 가구원수, 본인을 포함한 총 장애인수, 월 평균 총가구소득, 가구 주된 수입원,
## 가구 월평균 지출액, 장애발생 시 연령, 월 혈압약 일수, 관절통증 정도, 운동 시간(분), 키, 몸무게
continu_df <- Dyslipidemia %>% 
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
catego_df <- Dyslipidemia %>% 
  select(-c(`만 나이`, 생월, 생년, `장애등록 연도`, `본인을 포함한 총 가구원수`, `본인을 포함한 총 장애인수`, 
            `월 평균 총가구소득`, `가구 주된 수입원`, `가구 월평균 지출액`, `장애발생시 연령...36`, 
            `월 혈압약 일수`, `관절통증 정도`, `운동 시간(분)`, `키(센티)`, `몸무게(kg)`, # 연속형 변수 제거
            
            `조사표 종류`, `가구원 일련번호(장애인)`, `가구원 일련번호(응답자)`, `조사지역(시도)`, 
            `응답자 유형`, `대리응답이유`, `장애유형확인1`, `장애유형확인2`, `장애유형확인3`,
            "등록장애유형(1순위) ", `등록장애유형(2순위)`, 개인번호, `가구주와의 관계(가구주)`,`주된 장애유형`, '지체장애여부',
            
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
catego_df$산업재해인정여부...39 <- ifelse(catego_df$산업재해인정여부...39 == 9, NA, catego_df$산업재해인정여부...39)
catego_df$`주로 도와주는 사람` <- ifelse(catego_df$`주로 도와주는 사람` == 99, NA, catego_df$`주로 도와주는 사람`)
## catego_df$`필요한데 구입하지 않은 이유` <- ifelse(catego_df$`필요한데 구입하지 않은 이유` == 99, NA, catego_df$`필요한데 구입하지 않은 이유`)
catego_df$동거여부 <- ifelse(catego_df$동거여부 == 9, NA, catego_df$동거여부)
catego_df$`도움 계속 필요 여부` <- ifelse(catego_df$`도움 계속 필요 여부` == 9, NA, catego_df$`도움 계속 필요 여부`)
## catego_df$`현재 도움 충분도` <- ifelse(catego_df$`현재 도움 충분도` == 9, NA, catego_df$`현재 도움 충분도`)
catego_df$`희귀난치성질환 등록 여부` <- ifelse(catego_df$`희귀난치성질환 등록 여부` == 9, NA, catego_df$`희귀난치성질환 등록 여부`)
## catego_df$`재활치료서비스를 하나도 이용하지 않은 경우 이유` <- ifelse(catego_df$`재활치료서비스를 하나도 이용하지 않은 경우 이유` == 99, 
##                                                 NA, catego_df$`재활치료서비스를 하나도 이용하지 않은 경우 이유`)
catego_df$`1일 이용시간` <- ifelse(catego_df$`1일 이용시간` == 9, NA, catego_df$`1일 이용시간`)
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
# table(catego_df$`1일 이용시간`) # 0
# table(catego_df$'산업재해인정여부...39') # 0
# table(catego_df$'희귀난치성질환 등록 여부') # 0
# table(catego_df$'주로 도와주는 사람') # 0
# table(catego_df$'동거여부') # 0
# table(catego_df$'도움 계속 필요 여부') # 0

catego_df$`1일 이용시간` <- ifelse(is.na(catego_df$`1일 이용시간`), 0, catego_df$`1일 이용시간`)
catego_df$`산업재해인정여부...39` <- ifelse(is.na(catego_df$`산업재해인정여부...39`), 0, catego_df$`산업재해인정여부...39`)
catego_df$`희귀난치성질환 등록 여부` <- ifelse(is.na(catego_df$`희귀난치성질환 등록 여부`), 0, catego_df$`희귀난치성질환 등록 여부`)
catego_df$`주로 도와주는 사람` <- ifelse(is.na(catego_df$`주로 도와주는 사람`), 0, catego_df$`주로 도와주는 사람`)
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

catego_onehot_df <- one_hot(as.data.table(catego_noty_df[, 2:253])) # 인코딩
catego_onehot_df <- cbind(data.frame(catego_noty_df$id), catego_onehot_df) # id 칼럼 결합
catego_onehot_df <- dplyr::rename(catego_onehot_df, id=`catego_noty_df.id`)

# 8. 연속형, 범주형 테이블 merge(on = 'id')----
merge_df <- merge(continu_mm_df, catego_onehot_df, by = 'id')
new_df <- merge(merge_df, y, by='id')

# 9. 로지스틱 회귀분석----
options(max.print=1000000)
logit_df <- new_df %>% select(-c(id))
colnames(logit_df)

# summary(glm(`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=logit_df))

## 1) 1차 부분별 회귀분석
a <- logit_df %>% select(c(1:44, 841))
summary(glm(a$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=a))

b <- logit_df %>% select(c(45:160, 841))
summary(glm(b$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=b))

c <- logit_df %>% select(c(161:420, 841))
summary(glm(c$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=c))

d <- logit_df %>% select(c(421:533, 841))
summary(glm(d$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=d))

e <- logit_df %>% select(c(534:645, 841))
summary(glm(e$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=e))

f <- logit_df %>% select(c(646:836, 841))
summary(glm(f$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=f))

## 2) 2차 부분별 회귀분석
colnames(logit_df)
logit_df2 <- logit_df %>% select(-c(1:6, 8, 12, 13, 16, 23:54, 86:88, 144:160, 657:658, 684:685, 690:691, 710:711, 714:717, 722:723,
                                    732:733, 738:739, 744:745, 750:751, 754:755, 760:761, 769:771, 774:775, 778:779, 791:792, 797:798,
                                    803:804, 809:810))
colnames(logit_df2)

aa <- logit_df2 %>% select(c(1:150, 738))
summary(glm(aa$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=aa))

bb <- logit_df2 %>% select(c(151:243, 738))
summary(glm(bb$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=bb))

cc <- logit_df2 %>% select(c(244:358, 738))
summary(glm(cc$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=cc))

dd <- logit_df2 %>% select(c(359:471, 738))
summary(glm(dd$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=dd))

ee <- logit_df2 %>% select(c(472:733, 738))
summary(glm(ee$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=ee))

## 3) 3차 부분별 회귀분석
colnames(logit_df2)
logit_df3 <- logit_df2 %>% select(-c(154:162, 164:171, 208:226, 230:263, 267:271, 277:307, 316:318, 322:333, 337:358, 650:651))
colnames(logit_df3)

aaa <- logit_df3 %>% select(c(1:69, 593))
summary(glm(aaa$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=aaa))

bbb <- logit_df3 %>% select(c(70:150, 593))
summary(glm(bbb$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=bbb))

ccc <- logit_df3 %>% select(c(151:176, 593))
summary(glm(ccc$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=ccc))

ddd <- logit_df3 %>% select(c(177:215, 593))
summary(glm(ddd$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=ddd))

eee <- logit_df3 %>% select(c(216:265, 593))
summary(glm(eee$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=eee))

fff <- logit_df3 %>% select(c(266:295, 593))
summary(glm(fff$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=fff))

ggg <- logit_df3 %>% select(c(296:328, 593))
summary(glm(ggg$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=ggg))

hhh <- logit_df3 %>% select(c(329:363, 593))
summary(glm(hhh$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=hhh))

iii <- logit_df3 %>% select(c(364:426, 593))
summary(glm(iii$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=iii))

jjj <- logit_df3 %>% select(c(427:494, 593))
summary(glm(jjj$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=jjj))

kkk <- logit_df3 %>% select(c(495:537, 593))
summary(glm(kkk$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=kkk))

lll <- logit_df3 %>% select(c(538:588, 593))
summary(glm(lll$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=lll))

## 4) 4차 부분별 회귀분석
colnames(logit_df3)
logit_df4 <- logit_df3 %>% select(-c(3, 99:113, 116:123, 126:127, 137:153, 166:176, 179:190, 213:231, 236:276, 287:291, 296:344,
                                     347:379, 382:449, 452:456, 459:580, 585:586))
colnames(logit_df4)

aaaa <- logit_df4 %>% select(c(1:68, 183))
summary(glm(aaaa$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=aaaa))

bbbb <- logit_df4 %>% select(c(69:146, 183))
summary(glm(bbbb$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=bbbb))

cccc <- logit_df4 %>% select(c(147:178, 183))
summary(glm(cccc$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=cccc))

## 5) 5차 부분별 회귀분석
colnames(logit_df4)
logit_df5 <- logit_df4 %>% select(-c(98, 99, 102:110, 138:140, 147:150, 167:174))
colnames(logit_df5)

aaaaa <- logit_df5 %>% select(c(98:132, 157))
summary(glm(aaaaa$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=aaaaa))

bbbbb <- logit_df5 %>% select(c(133:152, 157))
summary(glm(bbbbb$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=bbbbb))

## 6) 6차 부분별 회귀분석
colnames(logit_df5)
logit_df6 <- logit_df5 %>% select(-c(101:111, 143:146))
colnames(logit_df6)

aaaaaa<- logit_df6 %>% select(c(1:42, 142))
summary(glm(aaaaaa$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=aaaaaa))

bbbbbb <- logit_df6 %>% select(c(43:97, 142))
summary(glm(bbbbbb$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=bbbbbb))

cccccc <- logit_df6 %>% select(c(98:137, 142))
summary(glm(cccccc$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=cccccc))

## 7) 7차 부분별 회귀분석
colnames(logit_df6)
logit_df7 <- logit_df6 %>% select(-c(69:97, 136:137))
colnames(logit_df7)

summary(glm(logit_df7$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=logit_df7 %>% select(c(1:106, 111))))

## 8) 8차 부분별 회귀분석
colnames(logit_df7)
logit_df8 <- logit_df7 %>% select(-c(105, 106))
colnames(logit_df8)

result <- glm(logit_df8$`05)만성질환명(이상지혈증)` ~ ., family=binomial, data=logit_df8 %>% select(c(1:104, 109)))
summary(result)


# 12. 유의한 변수만 추출----
## 정규화 및 원핫인코딩 수행 전의 데이터를 결합 후 추출(continu_df + catego_noty_df + y) 
final_df <- merge(continu_df, catego_noty_df, by = 'id')
final_df <- merge(final_df, y, by = 'id')

colnames(final_df)

final_df <- final_df %>% select(c(8, 10, 12, 15, 17, 23, 25, 38, 50, 56, 67, 76, 78, 88, 90, 92, 
                                  118, 119, 135, 267:291))
write_xlsx(final_df, 'rdata/[로지스틱] 유의한 변수 추출(지체장애 이상지혈증).xlsx')



