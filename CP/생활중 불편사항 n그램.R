# 1. 필요한 패키지 설치 및 로드
library(KoNLP)
library(openxlsx)
library(dplyr)
library(tidytext)
library(tidyr)
library(tidygraph)
library(ggraph)
library(showtext)
library(TeachingDemos)
library(ggplot2)
library(stringr)

# remotes::install_github("GuangchuangYu/shadowtext",force = T)

# 2. 사전 불러오기
useNIADic()

# 3. 데이터 불러오기
txt=read.xlsx('생활중 불편사항_어려움,힘듦,불편_정제.xlsx')
txt1=txt[,-3] #품사 칼럼 삭제

#문장번호=id열 만들어주기, 나중에 같은 문장끼리 합치기 위해 필요
j=1                                         #id에 넣을 초기값 j=1
txt1$id[1]=j                                #첫칼럼엔 수동 대입
for (i in 1:nrow(txt1)) {               
  if(txt1$reply[i]==txt1$reply[i+1])        #reply 전행과 현재행이 같으면
    txt1$id[i+1]=j  else {                  #현재행에 j넣고
      j=j+1                                 #아니면 j를 +1해서
      txt1$id[i+1]=j                        #넣어줌
    }
}                                           #에러 무시

#문장 내 단어번호=id2열 만들어주기, 나중에 한 행으로 묶을 때 단어 출현 순서대로 정렬하기 위함 
txt1$id2=row(txt1)

####추가 불용어 처리하기####
# 때/NNG 6건 삭제
txt1= txt1 %>% 
  filter(word!="때/NNG")

txt1 <- txt1 %>%
  mutate(word = ifelse(word == "밀리/VV", "밀/VV", word),
         word = ifelse(word == "하/VV", "하/VX", word),
         word = ifelse(word == "입/NNG", "입/VV", word))
# 밀리지 않아 -> 밀다로 대체
# 움직이지 못"하"게 하다-> 동사를 보조동사로 대체
# 수선을 "하"다 ->동사를 보조동사로 대체
# 동사 '입다'를 명사 '입'으로 인식한 오류 



# 명사 추출하기
noun <- txt1 %>%
  filter(str_detect(word, "/NNG|/NNP")) %>%
  mutate(word = str_remove(word, "/.*$"))
noun %>%
  select(word, reply)


# 동사, 형용사 추출하기
vvva <- txt1 %>%
  filter(str_detect(word, "/VV|/VA")) %>%         # "/VV", "/VA" 추출
  mutate(word = str_replace(word, "/.*$", "다"))  # "/"로 시작 문자를 "다"로 바꾸기
vvva %>%
  select(word, reply)

# 명사, 동사·형용사 테이블 결합
## 2글자 이상만 추출 (667건)
df_2 <- bind_rows(noun, vvva) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id,id2)
df_2 %>%
  select(word, reply)

##최종 : 1글자라도 모두 추출 (710건)
df_1 <- bind_rows(noun, vvva) %>%
#  filter(str_count(word) >= 2) %>%
  arrange(id,id2)
df_1 %>%
  select(word, reply)

#이유 : 1글자 단어 중 중요한 의미를 가진 단어 존재함 확인 ex) 폭, 팔, 옷, 키, 끈, 겹 등
df_1 %>% 
  filter(str_count(word)==1)


# 한 댓글이 하나의 행이 되도록 결합하기, 명사 동사 형용사 조합의 한 행 
df_1 %>%
  select(word)
line_df1 <- df_1 %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))
line_df1

#### 불편사항으로 바이그램 만들기####
# 바이그램으로 토큰화하기
bigram_df1 <- line_df1 %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)
bigram_df1


# 바이그램 분리하기
bigram_seprated <- bigram_df1 %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_seprated

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()
pair_bigram

# 바이그램 단어쌍
pair_bigram %>%
  filter(word2 == "힘듦"|word2 == "어려움"|word2 == "불편")

#폰트 설정
font_add_google(name = "Nanum Gothic", family = "Nanum Gothic")
showtext_auto()
# Black Han Sans
# Jua
# Nanum Gothic
# Noto Serif KR
# Nanum Myeongjo
# Do Hyeon


# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_bigram <- pair_bigram %>%
  filter(word2 == "힘듦"|word2 == "어려움"|word2 == "불편") %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),    # 중심성
         group = as.factor(group_infomap()))  # 커뮤니티

# 네트워크 그래프 만들기 by geom_node_text
set.seed(1234)
ggraph(graph_bigram, layout = "fr") +         # 레이아웃
  geom_edge_link(color = "gray50",            # 엣지 색깔
                 alpha = 0.5) +               # 엣지 명암
  geom_node_point(aes(size = centrality,      # 노드 크기
                      color = group),         # 노드 색깔
                  show.legend = T) +          # 범례 삭제
  scale_size(range = c(10, 15)) +               # 노드 크기 범위
  geom_node_text(aes(label = name),           # 텍스트 표시
                 repel = F,                   # 노드밖 표시
                 size = 5,                    # 텍스트 크기
                 family = "Nanum Gothic",     # 폰트
                 fontface="bold") +           # 볼드
  theme_graph()                               # 배경 삭제

# # 네트워크 그래프 만들기 by geom_node_label (geom_node_text로 만든게 더 예쁨)
# set.seed(1234)
# ggraph(graph_bigram, layout = "fr") +         # 레이아웃
#   geom_edge_link(color = "gray50",            # 엣지 색깔
#                  alpha = 0.5) +               # 엣지 명암
#   geom_node_point(aes(size = centrality,      # 노드 크기
#                       color = group),         # 노드 색깔
#                   show.legend = T) +          # 범례 삭제
#   scale_size(range = c(10, 20)) +               # 노드 크기 범위
#   geom_node_label(aes(label = name),           # 텍스트 표시
#                  repel = F,                   # 노드밖 표시
#                  size = 4.5,           # 텍스트 크기
#                  family = "Nanum Gothic",      # 폰트
#                  fontface="bold",           # 볼드
#                  nudge_y = 0.0001,
#                  label.size =NA ) +
#   theme_graph()      

#--------------------------------------------------------------------------------------#

# 위에 그래프보다 트라이그램으로 실행 후 word1 제거해 그리는 게 더 예쁨
# 트라이그램으로 토큰화하기
trigram_df1 <- line_df1 %>%
  unnest_tokens(input = sentence,
                output = trigram,
                token = "ngrams",
                n = 3)
trigram_df1

# 트라이그램 분리하기
trigram_seprated <- trigram_df1 %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")
trigram_seprated

# 단어쌍 빈도 구하기
pair_trigram <- trigram_seprated %>%
  count(word1, word2,word3, sort = T) %>%
  na.omit()
pair_trigram

# 트라이그램 단어쌍
k=pair_trigram %>%
  filter(word3 == "힘듦"|word3 == "어려움"|word3 == "불편") %>% 
  select(word2,word3,n)


# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_trigram <- k %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),    # 중심성
         group = as.factor(group_infomap()))  # 커뮤니티

# 네트워크 그래프 만들기 by geom_node_text
set.seed(1234)
ggraph(graph_trigram, layout = "fr") +         # 레이아웃
  geom_edge_link(color = "gray50",            # 엣지 색깔
                 alpha = 0.5) +               # 엣지 명암
  geom_node_point(aes(size = centrality,      # 노드 크기
                      color = group),         # 노드 색깔
                  show.legend = T) +          # 범례 삭제
  scale_size(range = c(8, 25)) +               # 노드 크기 범위
  geom_node_text(aes(label = name),           # 텍스트 표시
                 repel = F,                   # 노드밖 표시
                 size = 5,                    # 텍스트 크기
                 family = "Nanum Gothic",     # 폰트
                 fontface="bold") +           # 볼드
  theme_graph()         +                      # 배경 삭제
  scale_color_discrete(label=c("불편","어려움","힘듦"))+ #범레 레이블 설정
  theme(legend.text= element_text(size = 14, family = "Nanum Gothic"))+ #범례 레이블 서식
  theme(legend.title= element_text(size = 20, family = "Nanum Gothic",face="bold")) #범례 제목 서식

# # 네트워크 그래프 만들기 by geom_node_label (geom_node_text로 만든게 더 예쁨)
# set.seed(1234)
# ggraph(graph_bigram, layout = "fr") +         # 레이아웃
#   geom_edge_link(color = "gray50",            # 엣지 색깔
#                  alpha = 0.5) +               # 엣지 명암
#   geom_node_point(aes(size = centrality,      # 노드 크기
#                       color = group),         # 노드 색깔
#                   show.legend = T) +          # 범례 삭제
#   scale_size(range = c(10, 20)) +               # 노드 크기 범위
#   geom_node_label(aes(label = name),           # 텍스트 표시
#                  repel = F,                   # 노드밖 표시
#                  size = 4.5,           # 텍스트 크기
#                  family = "Nanum Gothic",      # 폰트
#                  fontface="bold",           # 볼드
#                  nudge_y = 0.0001,
#                  label.size =NA ) +
#   theme_graph()      


#### 불편사항으로 트라이그램 만들기(미완성)####
# 트라이그램으로 토큰화하기
trigram_df1 <- line_df1 %>%
  unnest_tokens(input = sentence,
                output = trigram,
                token = "ngrams",
                n = 3)
trigram_df1


# 트라이그램 분리하기
trigram_seprated <- trigram_df1 %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ")
trigram_seprated

# 단어쌍 빈도 구하기
pair_trigram <- trigram_seprated %>%
  count(word1, word2,word3, sort = T) %>%
  na.omit()
pair_trigram

# 트라이그램 단어쌍
k=pair_trigram %>%
  filter(word3 == "힘듦"|word3 == "어려움"|word3 == "불편")


# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_trigram <- pair_trigram %>%
  filter(word3 == "힘듦"|word3 == "어려움"|word3 == "불편") %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),    # 중심성
         group = as.factor(group_infomap()))  # 커뮤니티

# 네트워크 그래프 만들기 by geom_node_text
set.seed(1234)
ggraph(graph_trigram, layout = "fr") +         # 레이아웃
  geom_edge_link(color = "gray50",            # 엣지 색깔
                 alpha = 0.5) +               # 엣지 명암
  geom_node_point(aes(size = centrality,      # 노드 크기
                      color = group),         # 노드 색깔
                  show.legend = T) +          # 범례 삭제
  scale_size(range = c(5, 12)) +               # 노드 크기 범위
  geom_node_text(aes(label = name),           # 텍스트 표시
                 repel = F,                   # 노드밖 표시
                 size = 5,                    # 텍스트 크기
                 family = "Nanum Gothic",     # 폰트
                 fontface="bold")+
  theme_graph()                               # 배경 삭제

# 네트워크 그래프 만들기 by geom_node_label
set.seed(1234)
ggraph(graph_trigram, layout = "fr") +         # 레이아웃
  geom_edge_link(color = "gray50",            # 엣지 색깔
                 alpha = 0.5) +               # 엣지 명암
  geom_node_point(aes(size = centrality,      # 노드 크기
                      color = group),         # 노드 색깔
                  show.legend = T) +          # 범례 삭제
  scale_size(range = c(10, 20)) +               # 노드 크기 범위
  geom_node_label(aes(label = name),           # 텍스트 표시
                  repel = F,                   # 노드밖 표시
                  size = 4.5,           # 텍스트 크기
                  family = "Nanum Gothic",      # 폰트
                  fontface="bold",
                  nudge_y = 0.0001,
                  label.size =NA ) +
  theme_graph()      

