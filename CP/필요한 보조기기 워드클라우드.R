useSejongDic()
useNIADic()

# # 사전에 단어 추가
# userDic <- data.frame(c("보조기구"), 'ncn') 
# userDic <- data.frame(c("보조기"), 'ncn') 
# userDic <- data.frame(c("보조기기"), 'ncn')
# userDic <- data.frame(c("체어"), 'ncn')
# userDic <- data.frame(c("용구"), 'ncn')
# userDic <- data.frame(c("카시트"), 'ncn')
# userDic <- data.frame(c("보행기"), 'ncn') 
# userDic <- data.frame(c("자전거"), 'ncn') 
# userDic <- data.frame(c("스탠더"), 'ncn') 
# userDic <- data.frame(c("트레이"), 'ncn')
# userDic <- data.frame(c("목욕의자"), 'ncn')
# userDic <- data.frame(c("용품"), 'ncn')
# userDic <- data.frame(c("유모차"), 'ncn')
# userDic <- data.frame(c("신발"), 'ncn')
# userDic <- data.frame(c("경사로"), 'ncn')
# userDic <- data.frame(c("정형구두"), 'ncn')
# userDic <- data.frame(c("리프트"), 'ncn')
# userDic <- data.frame(c("자세보조"), 'ncn')
# userDic <- data.frame(c("의자"), 'ncn')
# userDic <- data.frame(c("전동시트"), 'ncn')
# userDic <- data.frame(c("변환기구"), 'ncn')
# userDic <- data.frame(c("이너"), 'ncn')
# userDic <- data.frame(c("변기"), 'ncn')
# userDic <- data.frame(c("바운서"), 'ncn')
# userDic <- data.frame(c("탈부착체어"), 'ncn')
# userDic <- data.frame(c("전동워커"), 'ncn')
# userDic <- data.frame(c("전동휠체어"), 'ncn')
# userDic <- data.frame(c("벨트"), 'ncn')
# userDic <- data.frame(c("책상"), 'ncn')
# userDic <- data.frame(c("의복"), 'ncn')
# userDic <- data.frame(c("척추측만"), 'ncn')
# userDic <- data.frame(c("기립용"), 'ncn')
# userDic <- data.frame(c("카시트용"), 'ncn')
# userDic <- data.frame(c("장난감"), 'ncn')
# userDic <- data.frame(c("턱받이"), 'ncn')
# userDic <- data.frame(c("바퀴"), 'ncn')
# userDic <- data.frame(c("피더시트"), 'ncn')
# userDic <- data.frame(c("목발"), 'ncn')
# userDic <- data.frame(c("욕창매트"), 'ncn')
# userDic <- data.frame(c("소프트웨어"), 'ncn')
# userDic <- data.frame(c("방수커버"), 'ncn')
# userDic <- data.frame(c("싱크대"), 'ncn')

buildDictionary(ext_dic = 'sejong', user_dic = userDic) 

# 데이터 불러오기
txt1 <- readLines('rdata/필요한 보조기기.txt')
txt1

# # 명사 추출
# txt2 <- extractNoun(txt1)
# txt2

# 빈도수 내림차순 정렬
a3 <- sort(table(txt1), decreasing = T)
a3

# 초기 난수 설정
set.seed(1243)

# 워드클라우드 생성
wordcloud2(a3, 
           size = 0.7, 
           fontFamily = "나눔바른고딕")
