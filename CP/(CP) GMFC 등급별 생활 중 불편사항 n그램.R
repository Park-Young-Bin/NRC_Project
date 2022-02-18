# 1. 필요한 패키지 설치 및 로드

# install.packages("TeachingDemos")
# remotes::install_github("GuangchuangYu/shadowtext",force = T)
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

# 2. 사전 불러오기
useNIADic()

### 3등급----
# 데이터 불러오기
df3 <- read.xlsx('rdata/(CP) GMFC 등급별 생활중 불편사항(형태소 분석 후).xlsx', sheet = 2) %>% select(-pos)

# 문장번호=id열 만들어주기, 나중에 같은 문장끼리 합치기 위해 필요
j = 1                                         # id에 넣을 초기값 j=1
df3$id[1] = j                                 # 첫칼럼엔 수동 대입
for (i in 1:nrow(df3)) {               
  if(df3$reply[i] == df3$reply[i+1])        # reply 전행과 현재행이 같으면
    df3$id[i+1] = j  else {                  # 현재행에 j넣고
      j = j+1                                 # 아니면 j를 +1해서
      df3$id[i+1] = j                         # 넣어줌
    }
}                                           #에러 무시

# 문장 내 단어번호 = id2열 만들기, 나중에 한 행으로 묶을 때 단어 출현 순서대로 정렬하기 위함 
df3$id2 <- row(df3)
row(df3)

# 명사 추출하기
noun_df3 <- df3 %>%
  filter(str_detect(word, "/NNG|/NNP")) %>%
  mutate(word = str_remove(word, "/.*$"))

noun_df3 <- noun_df3 %>% filter(word != '경우') # '경우' 명사 제거

noun_df3 %>%
  select(word, reply)

# 동사, 형용사 추출하기
vvva_df3 <- df3 %>%
  filter(str_detect(word, "/VV|/VA")) %>%         # "/VV", "/VA" 추출
  mutate(word = str_replace(word, "/.*$", "다"))  # "/"로 시작 문자를 "다"로 바꾸기

vvva_df3 %>%
  select(word, reply)

# 명사, 동사·형용사 테이블 결합
rbind_df3 <- bind_rows(noun_df3, vvva_df3) %>%
  arrange(id,id2)

rbind_df3 %>%
  select(word, reply)

# 한 댓글이 하나의 행이 되도록 결합하기, 명사 동사 형용사 조합의 한 행 
line_df3 <- rbind_df3 %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " ")) # 각각의 결과값에 옵션을 주어 이어붙일 때 사용

line_df3

# 바이그램으로 토큰화하기
bigram_comment3 <- line_df3 %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)
bigram_comment3

# 바이그램 분리하기
bigram_seprated3 <- bigram_comment3 %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_seprated3

# 단어쌍 빈도 구하기
pair_bigram3 <- bigram_seprated3 %>% 
  count(word1, word2, sort = T) %>% 
  na.omit()
pair_bigram3

# 폰트 설정
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
graph_bigram3 <- pair_bigram3 %>% 
  filter(n >= 1) %>% 
  as_tbl_graph(directed = F) %>% 
  mutate(centrality = centrality_degree(), # 중심성
         group = as.factor(group_infomap())) # 커뮤니티

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_bigram3, layout = 'fr') + # 레이아웃
  
  geom_edge_link(color = 'gray50',    # 엣지 색깔
                 alpha = .5) +        # 엣지 명암
  
  geom_node_point(aes(size = centrality,  # 노드 크기
                      color = group),     # 노드 색깔
                  show.legend = F) +      # 범례 삭제
  scale_size(range = c(8, 18)) +           # 노드 크기 범위
  
  geom_node_text(aes(label = name),       # 텍스트 표시
                 repel = F,               # 노드 안 표시
                 size = 5.1,                # 텍스트 크기
                 family = 'Nanum Gothic', # 폰트
                 fontface = 'bold') +
  theme_graph()                           # 배경 삭제


## 트라이그램
# 트라이그램으로 토큰화하기
trigram_comment3 <- line_df3 %>%
  unnest_tokens(input = sentence,
                output = trigram,
                token = "ngrams",
                n = 3)
trigram_comment3

# 트라이그램 분리하기
trigram_seprated3 <- trigram_comment3 %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
trigram_seprated3

# 단어쌍 빈도 구하기
pair_trigram3 <- trigram_seprated3 %>% 
  count(word1, word2, word3, sort = T) %>% 
  na.omit()

pair_trigram3

# k <- pair_trigram3 %>% select(word2, word3, n)
# k

# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_trigram3 <- pair_trigram3 %>% 
  filter(n >= 1) %>% 
  as_tbl_graph(directed = F) %>% 
  mutate(centrality = centrality_degree(), # 중심성
         group = as.factor(group_infomap())) # 커뮤니티

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_trigram3, layout = 'fr') + # 레이아웃
  
  geom_edge_link(color = 'gray50',    # 엣지 색깔
                 alpha = .5) +        # 엣지 명암
  
  geom_node_point(aes(size = centrality,  # 노드 크기
                      color = group),     # 노드 색깔
                  show.legend = F) +      # 범례 삭제
  scale_size(range = c(8, 18)) +           # 노드 크기 범위
  
  geom_node_text(aes(label = name),       # 텍스트 표시
                 repel = F,               # 노드 안 표시
                 size = 5.1,                # 텍스트 크기
                 family = 'Nanum Gothic', # 폰트
                 fontface = 'bold') +
  theme_graph()                           # 배경 삭제


### 4등급----
# 데이터 불러오기
df4 <- read.xlsx('rdata/(CP) GMFC 등급별 생활중 불편사항(형태소 분석 후).xlsx', sheet = 4) %>% select(-pos)

# 문장번호=id열 만들어주기, 나중에 같은 문장끼리 합치기 위해 필요
j = 1                                         # id에 넣을 초기값 j=1
df4$id[1] = j                                 # 첫칼럼엔 수동 대입
for (i in 1:nrow(df4)) {               
  if(df4$reply[i] == df4$reply[i+1])        # reply 전행과 현재행이 같으면
    df4$id[i+1] = j  else {                  # 현재행에 j넣고
      j = j+1                                 # 아니면 j를 +1해서
      df4$id[i+1] = j                         # 넣어줌
    }
}                                           #에러 무시

# 문장 내 단어번호 = id2열 만들기, 나중에 한 행으로 묶을 때 단어 출현 순서대로 정렬하기 위함 
df4$id2 <- row(df4)
row(df4)

# 명사 추출하기
noun_df4 <- df4 %>%
  filter(str_detect(word, "/NNG|/NNP")) %>%
  mutate(word = str_remove(word, "/.*$"))

noun_df4 <- noun_df4 %>% filter(word != '경우') # '경우' 명사 제거

noun_df4 %>%
  select(word, reply)

# 동사, 형용사 추출하기
vvva_df4 <- df4 %>%
  filter(str_detect(word, "/VV|/VA")) %>%         # "/VV", "/VA" 추출
  mutate(word = str_replace(word, "/.*$", "다"))  # "/"로 시작 문자를 "다"로 바꾸기

vvva_df4 %>%
  select(word, reply)

# 명사, 동사·형용사 테이블 결합
rbind_df4 <- bind_rows(noun_df4, vvva_df4) %>%
  arrange(id,id2)

rbind_df4 %>%
  select(word, reply)

# 한 댓글이 하나의 행이 되도록 결합하기, 명사 동사 형용사 조합의 한 행 
line_df4 <- rbind_df4 %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " ")) # 각각의 결과값에 옵션을 주어 이어붙일 때 사용

line_df4

# 바이그램으로 토큰화하기
bigram_comment4 <- line_df4 %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)
bigram_comment4

# 바이그램 분리하기
bigram_seprated4 <- bigram_comment4 %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_seprated4

# 단어쌍 빈도 구하기
pair_bigram4 <- bigram_seprated4 %>% 
  count(word1, word2, sort = T) %>% 
  na.omit()
pair_bigram4

# 폰트 설정
# font_add_google(name = "Nanum Gothic", family = "Nanum Gothic")
# showtext_auto()
# Black Han Sans
# Jua
# Nanum Gothic
# Noto Serif KR
# Nanum Myeongjo
# Do Hyeon

# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_bigram4 <- pair_bigram4 %>% 
  filter(n >= 1) %>% 
  as_tbl_graph(directed = F) %>% 
  mutate(centrality = centrality_degree(), # 중심성
         group = as.factor(group_infomap())) # 커뮤니티

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_bigram4, layout = 'fr') + # 레이아웃
  
  geom_edge_link(color = 'gray50',    # 엣지 색깔
                 alpha = .5) +        # 엣지 명암
  
  geom_node_point(aes(size = centrality,  # 노드 크기
                      color = group),     # 노드 색깔
                  show.legend = F) +      # 범례 삭제
  scale_size(range = c(8, 18)) +           # 노드 크기 범위
  
  geom_node_text(aes(label = name),       # 텍스트 표시
                 repel = F,               # 노드 안 표시
                 size = 5.1,                # 텍스트 크기
                 family = 'Nanum Gothic', # 폰트
                 fontface = 'bold') +
  theme_graph()                           # 배경 삭제


## 트라이그램
# 트라이그램으로 토큰화하기
trigram_comment4 <- line_df4 %>%
  unnest_tokens(input = sentence,
                output = trigram,
                token = "ngrams",
                n = 3)
trigram_comment4

# 트라이그램 분리하기
trigram_seprated4 <- trigram_comment4 %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
trigram_seprated4

# 단어쌍 빈도 구하기
pair_trigram4 <- trigram_seprated4 %>% 
  count(word1, word2, word3, sort = T) %>% 
  na.omit()

pair_trigram4

# k <- pair_trigram4 %>% select(word2, word3, n)
# k

# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_trigram4 <- pair_trigram4 %>% 
  filter(n >= 1) %>% 
  as_tbl_graph(directed = F) %>% 
  mutate(centrality = centrality_degree(), # 중심성
         group = as.factor(group_infomap())) # 커뮤니티

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_trigram4, layout = 'fr') + # 레이아웃
  
  geom_edge_link(color = 'gray50',    # 엣지 색깔
                 alpha = .5) +        # 엣지 명암
  
  geom_node_point(aes(size = centrality,  # 노드 크기
                      color = group),     # 노드 색깔
                  show.legend = F) +      # 범례 삭제
  scale_size(range = c(8, 18)) +           # 노드 크기 범위
  
  geom_node_text(aes(label = name),       # 텍스트 표시
                 repel = F,               # 노드 안 표시
                 size = 4.9,                # 텍스트 크기
                 family = 'Nanum Gothic', # 폰트
                 fontface = 'bold') +
  theme_graph()                           # 배경 삭제

### 5등급----
# 데이터 불러오기
df5 <- read.xlsx('rdata/(CP) GMFC 등급별 생활중 불편사항(형태소 분석 후).xlsx', sheet = 6) %>% select(-pos)

# 문장번호=id열 만들어주기, 나중에 같은 문장끼리 합치기 위해 필요
j = 1                                         # id에 넣을 초기값 j=1
df5$id[1] = j                                 # 첫칼럼엔 수동 대입
for (i in 1:nrow(df5)) {               
  if(df5$reply[i] == df5$reply[i+1])        # reply 전행과 현재행이 같으면
    df5$id[i+1] = j  else {                  # 현재행에 j넣고
      j = j+1                                 # 아니면 j를 +1해서
      df5$id[i+1] = j                         # 넣어줌
    }
}                                           #에러 무시

# 문장 내 단어번호 = id2열 만들기, 나중에 한 행으로 묶을 때 단어 출현 순서대로 정렬하기 위함 
df5$id2 <- row(df5)
row(df5)

# 명사 추출하기
noun_df5 <- df5 %>%
  filter(str_detect(word, "/NNG|/NNP")) %>%
  mutate(word = str_remove(word, "/.*$"))

noun_df5 <- noun_df5 %>% filter(word != '경우') # '경우' 명사 제거

noun_df5 %>%
  select(word, reply)

# 동사, 형용사 추출하기
vvva_df5 <- df5 %>%
  filter(str_detect(word, "/VV|/VA")) %>%         # "/VV", "/VA" 추출
  mutate(word = str_replace(word, "/.*$", "다"))  # "/"로 시작 문자를 "다"로 바꾸기

vvva_df5 %>%
  select(word, reply)

# 명사, 동사·형용사 테이블 결합
rbind_df5 <- bind_rows(noun_df5, vvva_df5) %>%
  arrange(id,id2)

rbind_df5 %>%
  select(word, reply)

# 한 댓글이 하나의 행이 되도록 결합하기, 명사 동사 형용사 조합의 한 행 
line_df5 <- rbind_df5 %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " ")) # 각각의 결과값에 옵션을 주어 이어붙일 때 사용

line_df5

# 바이그램으로 토큰화하기
bigram_comment5 <- line_df5 %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)
bigram_comment5

# 바이그램 분리하기
bigram_seprated5 <- bigram_comment5 %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_seprated5

# 단어쌍 빈도 구하기
pair_bigram5 <- bigram_seprated5 %>% 
  count(word1, word2, sort = T) %>% 
  na.omit()
pair_bigram5

# 폰트 설정
# font_add_google(name = "Nanum Gothic", family = "Nanum Gothic")
# showtext_auto()
# Black Han Sans
# Jua
# Nanum Gothic
# Noto Serif KR
# Nanum Myeongjo
# Do Hyeon

# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_bigram5 <- pair_bigram5 %>% 
  filter(n >= 2) %>% 
  as_tbl_graph(directed = F) %>% 
  mutate(centrality = centrality_degree(), # 중심성
         group = as.factor(group_infomap())) # 커뮤니티

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_bigram5, layout = 'fr') + # 레이아웃
  
  geom_edge_link(color = 'gray50',    # 엣지 색깔
                 alpha = .5) +        # 엣지 명암
  
  geom_node_point(aes(size = centrality,  # 노드 크기
                      color = group),     # 노드 색깔
                  show.legend = F) +      # 범례 삭제
  scale_size(range = c(8, 18)) +           # 노드 크기 범위
  
  geom_node_text(aes(label = name),       # 텍스트 표시
                 repel = F,               # 노드 안 표시
                 size = 4.9,                # 텍스트 크기
                 family = 'Nanum Gothic', # 폰트
                 fontface = 'bold') +
  theme_graph()                           # 배경 삭제


## 트라이그램
# 트라이그램으로 토큰화하기
trigram_comment5 <- line_df5 %>%
  unnest_tokens(input = sentence,
                output = trigram,
                token = "ngrams",
                n = 3)
trigram_comment5

# 트라이그램 분리하기
trigram_seprated5 <- trigram_comment5 %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
trigram_seprated5

# 단어쌍 빈도 구하기
pair_trigram5 <- trigram_seprated5 %>% 
  count(word1, word2, word3, sort = T) %>% 
  na.omit()

pair_trigram5

# k <- pair_trigram5 %>% select(word2, word3, n)
# k

# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_trigram5 <- pair_trigram5 %>% 
  filter(n >= 2) %>% 
  as_tbl_graph(directed = F) %>% 
  mutate(centrality = centrality_degree(), # 중심성
         group = as.factor(group_infomap())) # 커뮤니티

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_trigram5, layout = 'fr') + # 레이아웃
  
  geom_edge_link(color = 'gray50',    # 엣지 색깔
                 alpha = .5) +        # 엣지 명암
  
  geom_node_point(aes(size = centrality,  # 노드 크기
                      color = group),     # 노드 색깔
                  show.legend = F) +      # 범례 삭제
  scale_size(range = c(8, 18)) +           # 노드 크기 범위
  
  geom_node_text(aes(label = name),       # 텍스트 표시
                 repel = F,               # 노드 안 표시
                 size = 4.9,                # 텍스트 크기
                 family = 'Nanum Gothic', # 폰트
                 fontface = 'bold') +
  theme_graph()                           # 배경 삭제


# 특정 문자가 포함된 행 추출
line_df5 %>% filter(grepl('가격|부담', sentence)) # 4건
line_df5 %>% filter(str_detect(sentence, "가격|부담")) # 4건
count(line_df5 %>% filter(str_detect(sentence, "가격|부담"))) # 4건

line_df5 %>% filter(grepl('키|크다|마르다', sentence)) # 16건
line_df5 %>% filter(str_detect(sentence, "키|크다|마르다")) # 16건
count(line_df5 %>% filter(str_detect(sentence, "키|크다|마르다"))) # 16건