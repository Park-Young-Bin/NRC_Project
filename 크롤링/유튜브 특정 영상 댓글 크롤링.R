## R selenium을 활용한 데이터 크롤링
### java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445

install.packages(c("httr","rvest","RSelenium"))
library(httr)
library(rvest)
library(RSelenium) #크롤링을 위한 패키지를 설치해줍니다.


#####특정 영상 댓글 크롤링#####
#install.packages("RSelenium")
library(rvest)
library(RSelenium)
library(httr) # 크롤링에 필요한 라이브러리 불러오기

remD <- remoteDriver(remoteServerAddr = 'localhost', # 원격으로 서버 주소를 통해 통제한다는 뜻입니다.
                     port = 4445L,
                     browserName = "chrome") # 명령 프롬프트에 작성된 포트 번호를 입력합니다.

remD$open() #서버에 연결하면 크롬 창이 나옵니다.


remD$navigate("https://www.youtube.com/watch?v=MbNuLUEzDJM") #이 홈페이지로 이동

btn <- remD$findElement(using = "css selector",
                        value = '.html5-main-video')

btn$clickElement() #유튜브 영상을 일시정지 해줍니다.

remD$executeScript("window.scrollTo(0,100000)")  #페이지스크롤 


html <- remD$getPageSource()[[1]]
html <- read_html(html) # 페이지의 소스 읽어오기

youtube_comments <- html %>% html_nodes("#content-text") %>%
  html_text() #선택된 노드를 텍스트 화

youtube_comments[1:20]       #그페이지 댓글 개수로 잡아주면 됨

youtube_comments <- gsub("\n","", youtube_comments)
youtube_comments <- trimws(youtube_comments)
youtube_comments[1:20]

###############################################################################

