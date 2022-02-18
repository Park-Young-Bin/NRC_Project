#install.packages("gridExtra")
library(gridExtra)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
######################################시###############################################
df_si=read.xlsx("전국횡단보도표준데이터-20211124.xlsx",sheet=1)


#필요한 열 추출+녹색신호시간 NA값 제외  (1747건)
df_si=df_si %>% select(시도명,시군구명,횡단보도관리번호,위도, 경도,횡단보도연장,음향신호기설치여부,녹색신호시간,보도턱낮춤여부,점자블록유무) %>% 
  filter(!is.na(녹색신호시간)) %>% filter(녹색신호시간!=0)

#데이터 타입 확인 
str(df_si)

#numeric화
df_si$횡단보도연장=as.numeric(df_si$횡단보도연장)
df_si$녹색신호시간=as.numeric(df_si$녹색신호시간)

#보행진입시간 7초
df_si$일반인_적정신호시간=round(7+df_si$횡단보도연장)
df_si$교통약자_적정신호시간=round(7+(df_si$횡단보도연장/0.8))

#평가
df_si$평가=ifelse(df_si$녹색신호시간<df_si$일반인_적정신호시간,"부적당",
       ifelse(df_si$녹색신호시간>=df_si$교통약자_적정신호시간,"적당","교통약자기준 부적당"))

#내보내기
#write.xlsx(df_si,"적정신호시간횡단보도평가(시).xlsx")

#빈도수 테이블 
graph_si=df_si %>% group_by(시군구명,평가) %>% 
  summarise(빈도=n()) %>% 
arrange(시군구명,match(평가, c("적당", "교통약자기준 부적당", "부적당")))

#--------------------------------------------------------------------------------------------------------------#
#시 누적비율막대그래프
ggplot(graph_si) +
  aes(x = 시군구명, fill = factor(평가, levels=c("적당", "교통약자기준 부적당", "부적당")), weight = 빈도) +
  geom_bar(position = "fill") +
  labs(fill = "평가") +
  scale_fill_manual(
    values = c(부적당 = "#F8766D",`교통약자기준 부적당` = "#F8DB6D",
                  적당 = "#51C447"),
    breaks=c("적당", "교통약자기준 부적당", "부적당")) +
  theme_minimal()

#시 막대그래프
ggplot(graph_si) +
  aes(x = 시군구명, fill = factor(평가, levels=c("적당", "교통약자기준 부적당", "부적당")), weight = 빈도) +
  geom_bar(position = "dodge") +
  labs(fill = "평가") +
  scale_fill_manual(
    values = c(부적당 = "#F8766D",`교통약자기준 부적당` = "#F8DB6D",
                  적당 = "#51C447"),
    breaks=c("적당", "교통약자기준 부적당", "부적당")) +
  theme_minimal()

#시 누적막대그래프
ggplot(graph_si) +
  aes(x = 시군구명, fill = factor(평가, levels=c("적당", "교통약자기준 부적당", "부적당")), weight = 빈도) +
  geom_bar(position = "stack") +
  labs(fill = "평가") +
  scale_fill_manual(
    values = c(부적당 = "#F8766D",`교통약자기준 부적당` = "#F8DB6D",
                  적당 = "#51C447"),
    breaks=c("적당", "교통약자기준 부적당", "부적당")) +
  theme_minimal()

#점자블록유무(시)_누적비율막대그래프
ggplot(df_si) +
  aes(x = 시군구명, fill = 점자블록유무) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(title = "점자블록유무") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.x = element_text(size = 15L)
  )

#점자블록유무(시)_누적막대그래프
ggplot(df_si) +
  aes(x = 시군구명, fill = 점자블록유무) +
  geom_bar(position = "stack") +
  scale_fill_hue(direction = 1) +
  labs(title = "점자블록유무") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.x = element_text(size = 15L)
  )

#점자블록유무(시)_막대그래프
ggplot(df_si) +
  aes(x = 시군구명, fill = 점자블록유무) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(title = "점자블록유무") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.x = element_text(size = 15L)
  )
######################################군###############################################
df_gun=read.xlsx("전국횡단보도표준데이터-20211124.xlsx",sheet=2)

#필요한 열 추출+녹색신호시간 NA값 제외  (1747건)
df_gun=df_gun %>% select(시도명,시군구명,횡단보도관리번호,위도, 경도,횡단보도연장,음향신호기설치여부,녹색신호시간,보도턱낮춤여부,점자블록유무) %>% 
  filter(!is.na(녹색신호시간)) %>% filter(녹색신호시간!=0)

#데이터 타입 확인 
str(df_gun)

#numeric화
df_gun$횡단보도연장=as.numeric(df_gun$횡단보도연장)
df_gun$녹색신호시간=as.numeric(df_gun$녹색신호시간)

#보행진입시간 7초
df_gun$일반인_적정신호시간=round(7+df_gun$횡단보도연장)
df_gun$교통약자_적정신호시간=round(7+(df_gun$횡단보도연장/0.8))


#평가
df_gun$평가=ifelse(df_gun$녹색신호시간<df_gun$일반인_적정신호시간,"부적당",
                ifelse(df_gun$녹색신호시간>=df_gun$교통약자_적정신호시간,"적당","교통약자기준 부적당"))

#내보내기
#write.xlsx(df_gun,"적정신호시간횡단보도평가(군).xlsx")

#빈도수 테이블 
graph_gun=df_gun %>% group_by(시군구명, 평가) %>% 
  summarise(빈도=n()) %>% 
  arrange(시군구명,match(평가, c("적당", "교통약자기준 부적당", "부적당")))

#군 누적비율막대그래프
ggplot(graph_gun) +
  aes(x = 시군구명, fill = factor(평가, levels=c("적당", "교통약자기준 부적당", "부적당")), weight = 빈도) +
  geom_bar(position = "fill") +
  labs(fill = "평가") +
  scale_fill_manual(
    values = c(부적당 = "#F8766D",`교통약자기준 부적당` = "#F8DB6D",
                  적당 = "#51C447"),
    breaks=c("적당", "교통약자기준 부적당", "부적당")) +
  theme_minimal()

#군 막대그래프
ggplot(graph_gun) +
  aes(x = 시군구명, fill = factor(평가, levels=c("적당", "교통약자기준 부적당", "부적당")), weight = 빈도) +
  geom_bar(position = "dodge") +
  labs(fill = "평가") +
  scale_fill_manual(
    values = c(부적당 = "#F8766D",`교통약자기준 부적당` = "#F8DB6D",
                  적당 = "#51C447"),
    breaks=c("적당", "교통약자기준 부적당", "부적당")) +
  theme_minimal()

#군 누적막대그래프
ggplot(graph_gun) +
  aes(x = 시군구명, fill = factor(평가, levels=c("적당", "교통약자기준 부적당", "부적당")), weight = 빈도) +
  geom_bar(position = "stack") +
  labs(fill = "평가") +
  scale_fill_manual(
    values = c(부적당 = "#F8766D",`교통약자기준 부적당` = "#F8DB6D",
                  적당 = "#51C447"),
    breaks=c("적당", "교통약자기준 부적당", "부적당")) +
  theme_minimal()


#점자블록유무(군)_누적비율막대그래프
ggplot(df_gun) +
  aes(x = 시군구명, fill = 점자블록유무) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(title = "점자블록유무") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.x = element_text(size = 15L)
  )

#점자블록유무(군)_누적막대그래프
ggplot(df_gun) +
  aes(x = 시군구명, fill = 점자블록유무) +
  geom_bar(position = "stack") +
  scale_fill_hue(direction = 1) +
  labs(title = "점자블록유무") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.x = element_text(size = 15L)
  )

#점자블록유무(군)_막대그래프
ggplot(df_gun) +
  aes(x = 시군구명, fill = 점자블록유무) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  labs(title = "점자블록유무") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.x = element_text(size = 15L)
  )
######################3###########점자블록유무&시각장애인비율##############################
df_sigun=rbind(df_si,df_gun)
df_sigun=df_sigun %>% filter(시군구명=="양산시"|시군구명=="남해군"|시군구명=="영덕군")




df_sigak=read.xlsx("시각장애인현황(4개시,3개군).xlsx")
df_sigak=df_sigak %>% select(1:3) %>% filter(지역=="양산시"|지역=="남해군"|지역=="영덕군")
df_sigak$그외장애인=df_sigak$전체장애인-df_sigak$시각장애인
df_sigak$시각=round(df_sigak$시각장애인/df_sigak$전체장애인*100,2)
df_sigak$그외=round(df_sigak$그외장애인/df_sigak$전체장애인*100,2)

df_sigak=df_sigak[,c(1,5,6)]


df_sigak2=melt(df_sigak)
colnames(df_sigak2)=c("지역","장애인","비율")
df_sigak2=df_sigak2 %>% arrange(지역)

df_양산시=df_sigak2 %>% filter(지역=="양산시")
df_남해군=df_sigak2 %>% filter(지역=="남해군")
df_영덕군=df_sigak2 %>% filter(지역=="영덕군")


p1=ggplot(df_양산시, aes(x=지역, y=비율, fill=장애인))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('양산시') +
  geom_text(aes(label= paste0(장애인, " (", 비율, "%", ")")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 15,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"))

p2=ggplot(df_남해군, aes(x=지역, y=비율, fill=장애인))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('남해군') +
  geom_text(aes(label= paste0(장애인, " (", 비율, "%", ")")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 15,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"))

p3=ggplot(df_영덕군, aes(x=지역, y=비율, fill=장애인))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle("영덕군") +
  geom_text(aes(label= paste0(장애인, " (", 비율, "%", ")")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 15,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"))


# 그래프 결합 방법1(비율 계산X)

grid.arrange(p1, p2, p3, ncol=3) # 그래프 크기 비율 계산 불가


#점자블록유무(시군)_누적비율막대그래프
ggplot(df_sigun) +
  aes(x = factor(시군구명,level=c("양산시","남해군","영덕군")), fill = 점자블록유무) +
  labs(title="점자블록유무")+
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5)
  )+
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  xlab("시군구명") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.x = element_text(size = 15L)
  )



