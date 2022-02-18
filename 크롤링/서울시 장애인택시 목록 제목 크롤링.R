## R selenium을 활용한 데이터 크롤링
### 1. cmd 접속 후, cd C:\selenium 입력 후 엔터
### 2. java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445 입력 후 엔터
### 3. cmd 켠 채로 크롤링 진행

# install.packages(c("httr","rvest","RSelenium"))
library(httr)
library(rvest)
library(RSelenium) #크롤링을 위한 패키지를 설치해줍니다.

#####서울시 장애인택시 목록 제목 크롤링#####

remD <- remoteDriver(port = 4445L, # 포트번호 입력해줍니다. 포트번호는 cmd창에서 작성했던 번호입니다.
                     browserName = "chrome") #browser는 chrome으로 작성해보겠습니다.
remD$open() # 서버에 연결


remD$navigate("https://www.sisul.or.kr/open_content/calltaxi/community/citizen.jsp") # 해당 홈페이지로 이동

html <- remD$getPageSource()[[1]]
html <- read_html(html) # 페이지의 소스 읽어오기

# [Copy >> Copy selector]

a <- c()
i <- 0
for (i in 10:1){
  if(i < 10){
    a <- append(a, paste0(',tr:nth-child(',i,') a'))
  }else
    a <- append(a, paste0('tr:nth-child(',i,') a'))
}

title <- html %>% 
  html_nodes(paste(a, collapse = "")) %>% # 위 과정에서 얻은 주소를 입력하기 # collapse: 각 원소의 분리값 지정 
  html_text() #선택된 노드를 '텍스트화' 하기

title <- gsub("\n","",title) # 불용어 처리
title <- trimws(title)
title[1:10]