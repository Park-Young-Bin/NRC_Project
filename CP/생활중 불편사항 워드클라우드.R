# 1. 필요한 패키지 설치 및 로드
library(wordcloud2)
library(KoNLP)
library(openxlsx)
library(dplyr)

# 2. 사전 불러오기
useNIADic()

# 3. 데이터 불러오기
txt1 <- readLines('data/생활중 불편사항.txt')
txt1

# 4. 명사  추출
txt2 <- extractNoun(txt1)
txt2

# 5. 리스트 형태 해제
txt3 <- unlist(txt2)
txt3

### ★ 정규식 주요 기호 & 문법----
#   사용법   |     설명요약
#   //d             숫자
#   //D             숫자가 아닌것
#   //s             공백
#   //S             공백이 아닌것
#   //w             단어
#   //W             단어가 아닌것
#   //t             탭(tab)
#   //n             줄바꿈(엔터 문자)
#     ^             시작되는 글자
#     $             마지막 글자
#     /             탈출문자, // 은 /, /+ 은 +
#     |             두개 이상의 조건을 동시에 지정
#     .             줄바꿈(엔터)를 제외한 모든 문자
#   [ab]            a 또는 b
#   [^ab]           a 와 b를 제외한 모든 문자
#   [0-9]           모든 숫자
#   [A-Z]           영어대문자
#   [a-z]           영어소문자
#   [A-z]           모든영문자(대소문자 전부)
#     i+            i가 최소 1회는 나오는 경우
#     i*            i가 최소 0회 이상 나오는 경우
#     i?            i가 최소 0회에서 최대 1회만 나오는 경우
#     i{n}          i가 연속적으로 n회 나오는 경우
#   i{n1,n2}        i가 연속적으로 n1에서 n2회 나오는 경우
#    i{n,}          i가 n회 이상 나오는 경우
#  [:alnum:]        문자와 숫자가 나오는 경우 [:alpha:] & [:digit:]
#  [:alpha:]        문자가 나오는 경우 [:lower:] & [:upper:]
#  [:blank:]        공백이 있는 경우 
#  [:cntrl:]        제어문자가 있는 경우
#  [:digit:]        숫자 0~9
#  [:graph:]        문자, 숫자, 특수문자 [:alnum:] & [:punct:]
#  [:lower:]        소문자가 있는 경우
#  [:print:]        숫자, 문자, 특수문자, 공백모두 [:alnum:] & [:punct:] & space
#  [:punct:]        특수 문자 ! " $ % & ' ( ) * + - / 등
#  [:space:]        공백문자 tab, newline, vertical tab, form feed, carriage return, space
#  [:upper:]        대문자가 있는 경우
#  [:xdigit:]       16진수가 있는 경우 

#############################

# 6. 불용어 처리
data_list <- gsub('\\d+', '', txt3) # 숫자 제거
data_list <- gsub('[:punct:]', '', data_list) # 특수문자 제거
data_list <- gsub('\\n', '', data_list) # 줄바꿈 제거
data_list <- gsub('\\t', '', data_list) # 탭 제거
data_list <- gsub(',', '', data_list)
data_list <- gsub('\"· ', '', data_list)
data_list <- gsub('· ', '', data_list)
data_list <- gsub('\"', '', data_list)
data_list <- gsub('·', '', data_list)
data_list <- gsub('^바$', '바지', data_list)
data_list <- gsub('^이$', '이동', data_list)
data_list <- gsub('^동$', '', data_list)
data_list <- gsub('^불$', '', data_list) # 불편 제외
data_list <- gsub('^편$', '', data_list) # 불편 제외
data_list <- gsub('^불편$', '', data_list) # 불편 제외
data_list <- gsub('^힘$', '', data_list) # 힘듦 제외
data_list <- gsub('^듦$', '', data_list) # 힘듦 제외
data_list <- gsub('^사용', '', data_list) # 사용 제외
data_list <- gsub('^어려움$', '', data_list) # 어려움 제외
data_list <- gsub('^입$', '입음', data_list)
data_list <- gsub('^않$', '', data_list) # 않음 제외
data_list <- gsub('경우$', '', data_list)
data_list <- gsub('^조$', '조절', data_list)
data_list <- gsub('^절$', '', data_list)
data_list <- gsub('^편$', '편리', data_list)
data_list <- gsub('^리$', '', data_list)
data_list <- gsub('^한$', '', data_list)
data_list <- gsub('^겪$', '', data_list)
data_list <- gsub('^음$', '', data_list)
data_list <- gsub('^해서$', '', data_list)
data_list <- gsub('^시$', '', data_list)
data_list <- gsub('^증치$', '진증치료', data_list)
data_list <- gsub('^료$', '', data_list)
data_list <- gsub('^치$', '', data_list)
data_list <- gsub('^부$', '부담', data_list)
data_list <- gsub('^담$', '', data_list)
data_list <- gsub('^좋겠$', '', data_list)
data_list <- gsub('^때$', '', data_list)
data_list <- gsub('^솔기$', '', data_list)
data_list <- gsub('^수$', '', data_list)
data_list <- gsub('^터$', '', data_list)
data_list <- gsub('^없$', '', data_list)
data_list <- gsub('^줄$', '', data_list)
data_list <- gsub('^적$', '', data_list)
data_list <- gsub('^ecave$', '', data_list)
data_list <- gsub('^같$', '', data_list)
data_list <- gsub('^한데$', '', data_list)
data_list <- gsub('^특히$', '', data_list)
data_list <- gsub('^이너·휠체어에$', '이너·휠체어', data_list)
data_list <- gsub('^있었$', '', data_list)
data_list <- gsub('^외부$', '외부환경', data_list)
data_list <- gsub('^환경$', '', data_list)
data_list <- gsub('^바$', '', data_list)
data_list <- gsub('^데$', '', data_list)
data_list <- gsub('^다양$', '다양화', data_list)
data_list <- gsub('^화$', '', data_list)
data_list <- gsub('^진증치료로$', '', data_list)
data_list <- gsub('^들이$', '', data_list)
data_list <- gsub('^겪$', '', data_list)
data_list <- gsub('^적$', '', data_list)
data_list <- gsub('^면소$', '면소재', data_list)
data_list <- gsub('^것$', '', data_list)
data_list <- gsub('^움$', '', data_list)
data_list <- gsub('^한$', '', data_list)
data_list <- gsub('^바우처로$', '바우처', data_list)
data_list <- gsub('^바$', '', data_list)
data_list <- gsub('^디자인적$', '디자인', data_list)
data_list <- gsub('^적$', '', data_list)
data_list <- gsub('^시간+비용+효과가$', '시간/비용/효과', data_list)
data_list <- gsub('^청소년기$', '청소년', data_list)
data_list <- gsub('^용$', '', data_list)
data_list <- gsub('^들$', '', data_list)
data_list <- gsub('^들이$', '', data_list)
data_list <- gsub('^외부$', '외부환경', data_list)
data_list <- gsub('^환경$', '', data_list)
data_list <- gsub('^신$', '', data_list)
data_list <- gsub('^신기%', '', data_list)
data_list <- gsub('^하게$', '', data_list)
data_list <- gsub('^해서', '', data_list)
data_list <- gsub('^위$', '', data_list)
data_list <- gsub('^있$', '', data_list)
data_list <- gsub('^짐$', '', data_list)
data_list <- gsub('^후$', '', data_list)
data_list <- gsub('^겪$', '', data_list)
data_list <- gsub('^뒤$', '', data_list)
data_list <- gsub('^구$', '보조기구', data_list)
data_list <- gsub('^많$', '', data_list)
data_list <- gsub('^뿐$', '', data_list)
data_list <- gsub('^어$', '', data_list)
data_list <- gsub('^임$', '', data_list)
data_list <- gsub('^점$', '', data_list)
data_list <- gsub('^중$', '', data_list)
data_list <- gsub('^쪽$', '', data_list)
data_list <- gsub('^체$', '', data_list)
data_list <- gsub('^해$', '', data_list)
data_list <- gsub('^감$', '', data_list)
data_list <- gsub('^간$', '', data_list)
data_list <- gsub('^격$', '', data_list)
data_list <- gsub('^결$', '', data_list)
data_list <- gsub('^공$', '', data_list)
data_list <- gsub('^김$', '', data_list)
data_list <- gsub('^끼$', '', data_list)
data_list <- gsub('^나$', '', data_list)
data_list <- gsub('^남$', '', data_list)
data_list <- gsub('^능$', '', data_list)
data_list <- gsub('^대$', '', data_list)
data_list <- gsub('^등$', '', data_list)
data_list <- gsub('^면$', '', data_list)
data_list <- gsub('^모$', '', data_list)
data_list <- gsub('^밖$', '', data_list)
data_list <- gsub('^받$', '', data_list)
data_list <- gsub('^비장$', '비장애', data_list)
data_list <- gsub('^생$', '', data_list)
data_list <- gsub('^성$', '', data_list)
data_list <- gsub('^애$', '', data_list)
data_list <- gsub('^용구$', '', data_list)
data_list <- gsub('^양$', '', data_list)
data_list <- gsub('^옆$', '', data_list)
data_list <- gsub('^예$', '', data_list)
data_list <- gsub('^옴$', '', data_list)
data_list <- gsub('^요$', '', data_list)
data_list <- gsub('^재$', '', data_list)
data_list <- gsub('^저$', '', data_list)
data_list <- gsub('^절$', '조절', data_list)
data_list <- gsub('^줌$', '', data_list)
data_list <- gsub('^조$', '', data_list)
data_list <- gsub('^줌$', '', data_list)
data_list <- gsub('^증$', '', data_list)
data_list <- gsub('^지$', '', data_list)
data_list <- gsub('^집$', '', data_list)
data_list <- gsub('^짧$', '', data_list)
data_list <- gsub('^차$', '', data_list)
data_list <- gsub('^탈착$', '탈착의', data_list)
data_list <- gsub('^채$', '', data_list)
data_list <- gsub('^청$', '', data_list)
data_list <- gsub('^치$', '', data_list)
data_list <- gsub('^침$', '', data_list)
data_list <- gsub('^탐$', '', data_list)
data_list <- gsub('^터$', '', data_list)
data_list <- gsub('^하나$', '', data_list)
data_list <- gsub('^옷$', '의복', data_list)
data_list <- gsub('^의류$', '의복', data_list)
data_list <- gsub('^레저용$', '레저', data_list)
data_list <- gsub('^탈착의가$', '탈착의', data_list)
data_list <- gsub('^이너휠체어에$', '이너휠체어', data_list)
data_list <- gsub('^탈착의가$', '탈착의', data_list)
data_list <- gsub('^만$', '', data_list)
data_list <- gsub('^도$', '', data_list)
data_list <- gsub('[:punct:]', '', data_list)
data_list <- gsub('^누워있다보니$', '', data_list)
data_list <- gsub('^느슨해지고$', '', data_list)
data_list <- gsub('^내리락$', '', data_list)
data_list <- gsub('^"로$', '', data_list)
data_list <- gsub('^"$', '', data_list)
data_list <- gsub('[a-z]', '', data_list)
data_list <- gsub('^높$', '', data_list)
data_list <- gsub('^없고$', '', data_list)
data_list <- gsub('^있다보니$', '', data_list)
data_list <- gsub('^되$', '', data_list)
data_list <- gsub('^앞$', '', data_list)
data_list <- gsub('^착탈$', '탈착의', data_list)
data_list <- gsub('^겉$', '', data_list)
data_list <- gsub('^“$', '', data_list)
data_list <- gsub('^”로$', '', data_list)
data_list <- gsub('^겹$', '', data_list)
data_list <- gsub('^마다$', '', data_list)
data_list <- gsub('^맞$', '', data_list)
data_list <- gsub('^만큼$', '', data_list)
data_list <- gsub('^밑위$', '', data_list)
data_list <- gsub('^이동하$', '이동', data_list)
data_list <- gsub('^제$', '', data_list)
data_list <- gsub('^됨$', '', data_list)
data_list <- gsub('^할$', '', data_list)
data_list <- gsub('^하기$', '', data_list)
data_list <- gsub('^하$', '', data_list)
data_list <- gsub('^하면$', '', data_list)
data_list <- gsub('^기$', '', data_list)
data_list <- gsub('^마른체형이라$', '마른체형', data_list)
data_list <- gsub('^요하$', '', data_list)
data_list <- gsub('^조신$', '보조신발', data_list)
data_list <- gsub('^길$', '', data_list)
data_list <- gsub('^형$', '', data_list)
data_list <- gsub('^쓰$', '', data_list)
data_list <- gsub('^이너를$', '이너', data_list)
data_list <- gsub('^이너가$', '이너', data_list)
data_list <- gsub('^제한적$', '제한', data_list)
data_list <- gsub('^조금씩$', '', data_list)
data_list <- gsub('^이너용$', '이너용 시트', data_list)
data_list <- gsub('^쉬$', '아쉬움', data_list)
data_list <- gsub('^남자$', '남자아이', data_list)
data_list <- gsub('^구가$', '', data_list)
data_list <- gsub('^우비$', '우의', data_list)
data_list <- gsub('^따뜻$', '따뜻한', data_list)
data_list <- gsub('^아이$', '아동', data_list)
data_list <- gsub('^신발이$', '신발', data_list)
data_list <- gsub('^대부분$', '', data_list)
data_list <- gsub('^하지$', '', data_list)
data_list <- gsub('^신기$', '', data_list)


# 7. 필요 없는 행 삭제
a <- as.data.frame(table(data_list))
b <- a[-1, ] %>% arrange(desc(Freq))

# 8. 정제된 데이터 불러오기
library(dplyr)
df <- read.xlsx('data/생활중 불편사항(eng).xlsx')

# 9. 워드클라우드 생성
# 폰트 설정
library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()

library(ggwordcloud)
ggplot(df, aes(label = kor, size = Freq, col = Freq)) +
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
  scale_radius(limits = c(3, NA),    # 최대, 최소 단어 빈도
               range = c(7, 22)) +   # 최소, 최대 글자 크기
  scale_color_gradient(low = "#90c1f5", high = "#004EA1") +
  theme_minimal()


# wordcloud2(txt4, 
#            size = 1.1, 
#            fontFamily = "나눔바른고딕",
#            color = 'random-light')

#### 사전 단어 추가----
# mergeUserDic(data.frame(c('불편'), c("ncn")))
# mergeUserDic(data.frame(c('이동'), c("ncn")))
# mergeUserDic(data.frame(c('바지'), c("ncn")))
# mergeUserDic(data.frame(c('움직임'), c("ncn")))
# mergeUserDic(data.frame(c('상하체'), c("ncn")))
# mergeUserDic(data.frame(c('레저'), c("ncn"))) # 레저용 -> 레저
# mergeUserDic(data.frame(c('어려움'), c("ncn")))
# mergeUserDic(data.frame(c('자세보조용구'), c("ncn")))
# mergeUserDic(data.frame(c('공간확보'), c("ncn")))
# mergeUserDic(data.frame(c('자세유지의자'), c("ncn")))
# mergeUserDic(data.frame(c('팔움직임'), c("ncn")))
# mergeUserDic(data.frame(c('특수학교'), c("ncn")))
# mergeUserDic(data.frame(c('보조기기'), c("ncn")))
# mergeUserDic(data.frame(c('힘듬'), c("ncn"))) # 힘듬 -> 힘듦
# mergeUserDic(data.frame(c('ecave'), c("ncn")))
# mergeUserDic(data.frame(c('보조기용'), c("ncn")))
# mergeUserDic(data.frame(c('신축성 의류'), c("ncn")))
# mergeUserDic(data.frame(c('자세보조기기'), c("ncn")))
# mergeUserDic(data.frame(c('책상각도조절'), c("ncn")))
# mergeUserDic(data.frame(c('머리받침대'), c("ncn")))
# mergeUserDic(data.frame(c('짧음'), c("ncn")))
# mergeUserDic(data.frame(c('비장애'), c("ncn")))
# mergeUserDic(data.frame(c('소아 치료실'), c("ncn")))
# mergeUserDic(data.frame(c('외부환경'), c("ncn")))
# mergeUserDic(data.frame(c('장애아동'), c("ncn")))
# mergeUserDic(data.frame(c('대학병원'), c("ncn")))
# mergeUserDic(data.frame(c('비좁은'), c("ncn")))
# mergeUserDic(data.frame(c('내부공간'), c("ncn")))
# mergeUserDic(data.frame(c('휘어진'), c("ncn")))
# mergeUserDic(data.frame(c('부담됨'), c("ncn"))) # 부담됨 -> 부담
# mergeUserDic(data.frame(c('교구'), c("ncn")))
# mergeUserDic(data.frame(c('발목보조기'), c("ncn")))
# mergeUserDic(data.frame(c('무릎보호대'), c("ncn")))
# mergeUserDic(data.frame(c('느슨해지고'), c("ncn")))
# mergeUserDic(data.frame(c('미적인'), c("ncn")))
# mergeUserDic(data.frame(c('허리 벨트'), c("ncn")))
# mergeUserDic(data.frame(c('발보조기'), c("ncn")))
# mergeUserDic(data.frame(c('땀띠'), c("ncn")))
# mergeUserDic(data.frame(c('유모차형 이동보조기구'), c("ncn")))
# mergeUserDic(data.frame(c('유모차형 이동 도구'), c("ncn")))
# mergeUserDic(data.frame(c('이동보조기구'), c("ncn")))
# mergeUserDic(data.frame(c('이동 도구'), c("ncn")))
# mergeUserDic(data.frame(c('180도'), c("ncn")))
# mergeUserDic(data.frame(c('장애아'), c("ncn")))
# mergeUserDic(data.frame(c('디자인적 측면'), c("ncn")))
# mergeUserDic(data.frame(c('자세보정의자'), c("ncn")))
# mergeUserDic(data.frame(c('발보조기'), c("ncn")))
# mergeUserDic(data.frame(c('휠체어 책상'), c("ncn")))
# mergeUserDic(data.frame(c('보조신발'), c("ncn")))
# mergeUserDic(data.frame(c('카시트 벨트'), c("ncn")))
# mergeUserDic(data.frame(c('쿠션 등받이'), c("ncn")))
# mergeUserDic(data.frame(c('손 사용'), c("ncn")))
# mergeUserDic(data.frame(c('세분화'), c("ncn")))
# mergeUserDic(data.frame(c('끈 조절'), c("ncn")))
# mergeUserDic(data.frame(c('등산용 캐리어'), c("ncn")))
# mergeUserDic(data.frame(c('탈착의'), c("ncn")))
# mergeUserDic(data.frame(c('여유공간'), c("ncn")))
# mergeUserDic(data.frame(c('자세보조용구'), c("ncn")))
# mergeUserDic(data.frame(c('판초우의'), c("ncn")))
# mergeUserDic(data.frame(c('전동휠체어'), c("ncn")))
# mergeUserDic(data.frame(c('좌우 길이'), c("ncn")))
# mergeUserDic(data.frame(c('재활수영활동'), c("ncn")))
# mergeUserDic(data.frame(c('보조부력기구'), c("ncn")))
# mergeUserDic(data.frame(c('학습보조기기'), c("ncn")))
# mergeUserDic(data.frame(c('다양화'), c("ncn")))
# mergeUserDic(data.frame(c('여러겹'), c("ncn")))
# mergeUserDic(data.frame(c('보조기용'), c("ncn")))
# mergeUserDic(data.frame(c('공동구매형식'), c("ncn")))
# mergeUserDic(data.frame(c('국산화'), c("ncn")))
# mergeUserDic(data.frame(c('자세 보조기구'), c("ncn")))
# mergeUserDic(data.frame(c('발판내림'), c("ncn")))
# mergeUserDic(data.frame(c('일자형 책상'), c("ncn")))
# mergeUserDic(data.frame(c('해당 부위'), c("ncn")))
# mergeUserDic(data.frame(c('리프트 차량'), c("ncn")))
# mergeUserDic(data.frame(c('이너용 시트'), c("ncn")))
# mergeUserDic(data.frame(c('고정용 봉'), c("ncn")))
# mergeUserDic(data.frame(c('장애용 튜브'), c("ncn")))
# mergeUserDic(data.frame(c('보조기용 신발'), c("ncn")))
# mergeUserDic(data.frame(c('휠체어형 유모차'), c("ncn")))
# mergeUserDic(data.frame(c('지적장애아'), c("ncn")))
# mergeUserDic(data.frame(c('환경'), c("ncn")))
# mergeUserDic(data.frame(c('환경변화'), c("ncn")))
# mergeUserDic(data.frame(c('지원 횟수'), c("ncn")))
# mergeUserDic(data.frame(c('복지관치료'), c("ncn")))
# mergeUserDic(data.frame(c('어깨 솔기'), c("ncn")))
# mergeUserDic(data.frame(c('외국 제품'), c("ncn")))
# mergeUserDic(data.frame(c('일반 운동화'), c("ncn")))
# mergeUserDic(data.frame(c('국내 여행'), c("ncn")))
# mergeUserDic(data.frame(c('그물망 기구'), c("ncn")))

# 다른 방법
# userDic <- data.frame(c("이동", '불편', "이동", "바지", "움직임", '상하체', '레저', '어려움', '자세보조용구', '공간확보',
#                              '팔움직임', '특수학교', '보조기기', '힘듬', 'ecave', '보조기용', '신축성 의류', '자세보조기기',
#                              '책상각도조절', '머리받침대', '짧음', '비장애', '소아 치료실', '외부환경', '장애아동',
#                              '대학병원', '비좁은', '내부공간', '휘어진', '부담됨', '교구', '발목보조기', '무릎보호대',
#                              '느슨해지고', '미적인', '허리 벨트', '발보조기', '땀띠', '유모차형 이동보조기구',
#                              '유모차형 이동 도구', '이동보조기구', '이동 도구', '180도', '장애아', '디자인적 측면',
#                              '자세보정의자', '발보조기', '휠체어 책상', '보조신발', '카시트 벨트', '쿠션 등받이',
#                              '손 사용', '세분화', '끈 조절', '등산용 캐리어', '탈착의', '여유공간', '자세보조용구', 
#                              '판초우의', '전동휠체어', '좌우 길이', '재활수영활동', '보조부력기구', '학습보조기기', 
#                              '다양화', '여러겹', '보조기용', '공동구매형식', '국산화', '자세 보조기구', '발판내림',
#                              '일자형 책상', '해당 부위', '리프트 차량', '이너용 시트', '고정용 봉', '장애용 튜브',
#                              '보조기용 신발', '휠체어형 유모차', '지적장애아', '환경', '환경변화', '지원 횟수', 
#                              '복지관치료', '어깨 솔기', '외국 제품', '일반 운동화', '국내 여행', '그물망 기구'), 'ncn') 
# 
# buildDictionary(ext_dic = 'sejong', user_dic = userDic) 
