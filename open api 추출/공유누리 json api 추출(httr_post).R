library(tidyverse)
library(jsonlite)
library(httr) 


res = POST("https://www.eshare.go.kr/eshare-openapi/rsrc/list/010500/f6aa30fee33833beb1b147b11dc23032"
           , content_type_json(), body = '{"numOfRows": "1000","ctpvCd": "11"}') #content_type을 json 방식으로 하여 request 요청. body에는 요청하고 싶은 파라매터값을 json 형태로 입력
res


res %>% content(as='text',encoding='UTF-8') %>% 
  fromJSON()-> json

str(object = json)


df=json$data
