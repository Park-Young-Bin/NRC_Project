# 통계표 PieDonut 코드

# 참고자료
# https://cardiomoon.github.io/webr/reference/PieDonut.html
# https://ggrepel.slowkow.com/articles/examples.html
# https://rpubs.com/cardiomoon/398623
# https://stackoverflow.com/questions/52273659/multi-level-pie-chart-ggplot-label-overlap-and-legend
# https://waterdata.usgs.gov/blog/beyond-basic-plotting/
# https://yutannihilation.github.io/allYourFigureAreBelongToUs/cowplot/plot_grid/
# https://github.com/slowkow/ggrepel/issues/112
# https://stackoverflow.com/questions/52258420/unexpected-behaviour-in-ggplot2-pie-chart-labeling

#----------------------------------------------------------------------------------------------
df=read.xlsx('data/주_이용_운동_장소.xlsx', startRow=2)

df

df1=df[df[,1]=="권역별"&df[,2]=="서울",]
df2=melt(df1)
df3 <- df2[-c(7:8), ]
df3
df3$variable <- gsub('[.]', " ", df3$variable)
df3 <- df3 %>% arrange(desc(value))

PieDonut(df3, 
         aes(variable, count = value), 
         explode = c(-4), # 쪼갤 부분 선택
         r1 = 0.9,
         explodeDonut = TRUE,
         explodePos = .3, # 쪼개짐 정도 설정
         start = 3*pi/2, 
         labelposition=1, # 라벨 위치 선택(1 or 2)
         family = getOption("PieDonut.family", "Noto Serif KR"), # 폰트 설정
         pieLabelSize=4,
         donutLabelSize=5,
         labelpositionThreshold = 0.0,
         maxx = 2,
         showPieName=FALSE, # 파이 이름 미출력
         showRatioThreshold=0.0)+
  theme_void() +
  ggtitle("주 이용 운동 장소") +
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 25,
                                  hjust = 0.5, vjust = -18,
                                  family = "Noto Serif KR")) 

# Black Han Sans
# Jua
# Nanum Gothic
# Noto Serif KR
# Nanum Myeongjo
# Do Hyeon

# install.packages("plotrix")
# library(plotrix)
# radial.pie(df3$value, labels=paste(df3$variable, "(", df3$value, ")"), radlab=TRUE)
# 
# ggplot(df3, aes(x=2 , y=value, fill=variable)) + 
#   geom_bar(stat = "identity", color="black" ) + 
#   coord_polar(theta="y", start=pi/2) + 
#   xlim(1, 2.5) +
#   theme_void()
  
#----------------------------------------------------------------------------------------------

# 주 이용 운동 장소(2020)----

# 필요한 패키지 설치 및 로드
# devtools::install_github("cardiomoon/moonBook")
# devtools::install_github("cardiomoon/webr")
# install.packages("ggrepel")
# devtools::install_github("slowkow/ggrepel")
library(devtools)
library(ggrepel)
require(ggplot2)
require(moonBook)
require(webr)
library(reshape2)
library(showtext)
library(RColorBrewer)
font_add_google(name = "Noto Serif KR", family = "Noto Serif KR")
showtext_auto()


da=read.xlsx("data/주 이용 운동 장소.xlsx")
da$category="var2"
da[7,]=da[1,]
da[7,4]="var1"
da[8,]=da[7,]
da[8,2]="2.이용함"
da[8,3]=sum(da$value[2:6])

ggplot(da, aes(x = category, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text_repel(aes(label= paste(variable,"(",value, "%)")), # 라벨 표시
                  position = position_fill(), size = 3.5, family="Noto Serif KR")+
  coord_polar(theta = "y")+
  theme_void()+ # 배경 서식
  ggtitle("주 이용 운동 장소(2020)")+ # 제목 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                    size = 15,
                                  hjust = 0.5,
                                  family = "Noto Serif KR"),
        legend.position = "none") + # 범례 제거
  scale_fill_brewer(palette = "Pastel2", direction = 1)  # 색 지정

p1 <- ggplot(da, aes(x = category, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "fill", size = 1, color = "white") +
  geom_text_repel(aes(label= paste(value, "%")), # 라벨 표시
                  position = position_fill(), size = 5, family="Noto Serif KR")+
  coord_polar(theta = "y")+
  theme_void()+ # 배경 서식
  ggtitle("주 이용 운동 장소(2020)")+ # 제목 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 15,
                                  hjust = 0.5,
                                  family = "Noto Serif KR"),
        legend.title = element_blank()) +
  scale_fill_brewer(palette = "Pastel2", direction = 1)  # 색 지정
p1

# ggplot(da, aes(x = category, y = value, fill = variable)) +
#   geom_bar(width=1, stat = "identity", size = 1, color = "white") +
#   geom_text_repel(aes(label = paste(value)), size=4, show.legend = F, nudge_x = 0, nudge_y = 0,family="Noto Serif KR")+
#   coord_polar(theta = "y")+
#   theme_void()+ # 배경 서식
#   ggtitle("주 이용 운동장소")+ # 제목 지정
#   theme(plot.title = element_text(face = 'bold', # 제목 서식
#                                   size = 15,
#                                   hjust = 0.5, vjust = 2,
#                                   family = "Noto Serif KR"),
#         legend.title = element_blank()) +
#   scale_fill_brewer(palette = "Pastel2", direction = 1)  # 색 지정

# 집안/집밖
home <-  data.frame(
  group = c("집안", "집밖"),
  value = c(9, 91)
)

p2 <- ggplot(home, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") + 
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('집안/밖 운동 장소 비율') +
  geom_text(aes(label= paste0(group, "(", value, "%", ")")), 
            position = position_stack(vjust = 0.5), # 위치 지정
            family="Noto Serif KR") + # 폰트 지정정
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"))
p2

# 근처 야외 등산로나 공원 사용 비율
park <- data.frame(
  group = c("이용함", "이용안함"),
  value = c(31.8, 100-31.8)
)

p3 <- ggplot(park, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('근처 야외 등산로 및 공원 사용 비율') +
  geom_text(aes(label= paste0(group, "(", value, "%", ")")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"))

# 그래프 결합 방법1(비율 계산X)
# install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2, p3, ncol=2) # 그래프 크기 비율 계산 불가

# 그래프 결합 방법2(비율 계산O)
# install.packages('cowplot')
library(cowplot)
bottom_row <- plot_grid(p2, p3, ncol = 2) # 하위 그래프 묶기
bottom_row # 출력 상태 확인
plot_grid(p1, bottom_row, align="hv", rel_heights=c(1.6, 1), nrow = 2) # 최종 묶기


# 최후의 수단
# ggplot(da, aes(x = category, y = value, fill = variable)) +
#   geom_bar(stat = "identity", position = "fill") +
#   coord_polar(theta = "y") +
#   theme_void()+ # 배경 서식
#   ggtitle("주 이용 운동 장소") + # 제목 지정
#   theme(plot.title = element_text(face = 'bold', # 제목 서식
#                                   size = 15,
#                                   hjust = 0.5, vjust = -2,
#                                   family = "Noto Serif KR"),
#         legend.position = "none") + # 범례 제거
#   scale_fill_brewer(palette = "Pastel2", direction = -1)  # 색 지정


# 체육시설의 이용 편의성 향상을 위한 시설 1순위(2020)----
library(openxlsx)
raw_data <- read.xlsx('data/체육시설의 이용 편의성 향상을 위한 시설 1순위.xlsx')
df1 <- melt(raw_data)
df1$variable <- gsub('[.]', " ", df1$variable)
df2 <- df1 %>% arrange(desc(value))
df3 <- rename(df2, "group" = "variable")

# df3 <- data_edit(df3)

ggplot(df3, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel1") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('체육시설의 이용 편의성 향상을 위한 시설 1순위(2020)') +
  geom_text(aes(label= paste0(ifelse(value > 2.0, paste0(value, "%"), ""))), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"), 
        legend.title = element_blank())

ㄴ# 최근 1년간 운동 실시 여부(2020)----
exercise <- data.frame(
  group = c("있다", "없다"),
  value = c(65.4, 100-65.4)
)

ggplot(exercise, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('최근 1년간 운동 실시 여부(2020)') +
  geom_text(aes(label= paste0(group, "(", value, "%", ")")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"))

# 최근 1년간 장애인생활체육 지도자의 전문 지도를 받은 경험 여부(2020)----
teach <- data.frame(
  group = c("있다", "없다"),
  value = c(3, 97)
)

ggplot(teach, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('최근 1년간 장애인생활체육 지도자의 전문 지도를 받은 경험 여부(2020)') +
  geom_text(aes(label= paste0(group, "(", value, "%", ")")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"))

# 응답자특성별 전문지도자의 필요성(2017)----
need <- data.frame(
  group = c("1. 약간그렇다", "2. 그렇지않다", "3. 보통", "4. 매우그렇다", "5. 전혀그렇지않다"),
  value = c(31.7, 28.5, 17.4, 13.6, 8.8)
)

ggplot(need, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('응답자특성별 전문지도자의 필요성(2017)') +
  geom_text(aes(label= paste0(value, "%")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(legend.title = element_blank(),
        plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"))

# 장애인 생활체육 실행 유형----
type <- data.frame(
  group = c("1. 완전 실행자", "2. 불완전 실행자", "3. 현재 운동하지 않지만 운동 의지가 있는 자", "4. 현재 운동하지 않고 운동 의지가 없는 자"),
  value = c(34.6, 30.9, 20.9, 13.7)
)

ggplot(type, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('장애인 생활체육 실행 유형(2020)') +
  geom_text(aes(label= paste0( value, "%")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(# legend.position = 'none',
        plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"),
        legend.title = element_blank())

# 장애인 생활체육 지도자 만족도----
sat <- data.frame(
  group = c("1. 만족", "2. 매우만족", "3. 보통", "4. 불만족", "5. 매우불만족"),
  value = c(51.8, 37.3, 10.9, 0, 0)
)

ggplot(sat, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('장애인 생활체육 지도자 만족도(2020)') +
  geom_text(aes(label = ifelse(value > 2.0, paste0(value, "%"), "")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(# legend.position = 'none',
    plot.title = element_text(face = 'bold', # 제목 서식
                              size = 13,
                              hjust = 0.5, vjust = -2,
                              family = "Noto Serif KR"),
    legend.title = element_blank())

# 운동시 가장 필요한 사항----
need_ex <- data.frame(
  group = c("1. 비용 지원", "2. 장애인생활체육프로그램", "3. 장애인생활체육 지도", 
            "4. 체육시설의 장애인편의시설", "5. 장애인용 운동용품 및 장비", "6. 이동지원(교통, 차량 등)", 
            "7. 보조인력(이동할 때 등 도와주는 사람)", "8. 기타"),
  value = c(43.5,	19.6, 11.5,	10.7, 8.3, 3.2,	2.7, 0.4)
)

ggplot(need_ex, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('운동시 가장 필요한 사항(2020)') +
  geom_text(aes(label= paste0(ifelse(value > 2.7, paste0(value, "%"), ""))), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                              size = 13,
                              hjust = 0.5, vjust = -2,
                              family = "Noto Serif KR"),
    legend.title = element_blank())

# 21.10.14----
# 운동 경험자 운동 참여 방식(2020)----
way <- data.frame(
  group = c("1. 홈트레이닝(개인)", "2. 시설 방문(개인)", "3. 강습 및 강좌(교실 포함)", "4. 클럽/동호회"),
  value = c(77.8,	13.6,	7.2,	1.5)
)

ggplot(way, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('운동 경험자 운동 참여 방식(2020)') +
  geom_text(aes(label= paste0( value, "%")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"),
        legend.title = element_blank())

# 운동 경험자 운동시 지원받은 경험(2020)----
experience <- data.frame(
  group = c("있다", "없다"),
  value = c(32.7, 100-32.7)
)

ggplot(experience, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('운동 경험자 운동시 지원받은 경험(2020)') +
  geom_text(aes(label= paste0(group, "(", value, "%", ")")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(legend.position = 'none',
        plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"))

# 운동 비경험자 운동을 하지 않는 이유(2020)----
reason <- read.xlsx('data/운동 비경험자 운동을 하지 않는 이유.xlsx')

ggplot(reason, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Set3") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('운동 비경험자 운동을 하지 않는 이유(2020)') +
  geom_text(aes(label= paste0(ifelse(value > 3.2, paste0(value, "%"), ""))), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"),
        legend.title = element_blank())

# 운동시 가장 도움이 되는 지원 사항(2020)----
help <- read.xlsx('data/운동시 가장 도움이 되는 지원 사항.xlsx')

ggplot(help, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('운동시 가장 도움이 되는 지원 사항(2020)') +
  geom_text(aes(label = paste0(value, "%")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"),
        legend.title = element_blank())

# 생활권 주변 이용하고 싶은 체육시설(2020)----
around <- read.xlsx('data/생활권 주변 이용하고 싶은 체육시설.xlsx')

ggplot(around, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('생활권 주변 이용하고 싶은 체육시설(2020)') +
  geom_text(aes(label = paste0(ifelse(value > 1.4, paste0(value, "%"), ""))), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"),
        legend.title = element_blank())

# 생활권 주변 체육시설 주요 이동 수단(2020)----
move <- read.xlsx('data/생활권 주변 체육시설 주요 이동 수단.xlsx')

ggplot(move, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Set3") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('생활권 주변 체육시설 주요 이동 수단(2020)') +
  geom_text(aes(label = paste0(ifelse(value > 3.5, paste0(value, "%"), ""))), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"),
        legend.title = element_blank())

# 생활권 주변 체육시설을 이용하는 이유(2020)----
use <- read.xlsx('data/생활권 주변 체육시설을 이용하는 이유.xlsx')

ggplot(use, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('생활권 주변 체육시설을 이용하는 이유(2020)') +
  geom_text(aes(label = paste0(ifelse(value > 2.3, paste0(value, "%"), ""))), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"),
        legend.title = element_blank())

# 생활권 주변 체육시설을 이용하지 않는 이유(2020)----
donot <- read.xlsx('data/생활권 주변 체육시설을 이용하지 않는 이유.xlsx')

ggplot(donot, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Set3") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('생활권 주변 체육시설을 이용하지 않는 이유(2020)') +
  geom_text(aes(label = paste0(value, "%")), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"),
        legend.title = element_blank())

# 운동 경험자 운동 동반 참여자(2020)----
accom <- read.xlsx('data/운동 경험자 운동 동반 참여자.xlsx')

ggplot(accom, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity", size = 1, color = "white") +
  scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle('운동 경험자 운동 동반 참여자(2020)') +
  geom_text(aes(label = paste0(ifelse(value > 3.5, paste0(value, "%"), ""))), 
            position = position_stack(vjust = 0.5), # 위치 조정
            family="Noto Serif KR") + # 폰트 지정
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5, vjust = -2,
                                  family = "Noto Serif KR"),
        legend.title = element_blank())

# 장애인 생활체육 관련 전문적인 지도를 받은 목적(2020)----
purpose <- data.frame(
  group = c('건강 및 체력 키우기', '놀이 및 레크리에이션(함께하는 오락 또는 게임)',
            '새로운 운동 종목 배우기', '기타'),
  value = c(56.7,	37.5,	8.7,	0)
)
purpose

ggplot(data=purpose, aes(x = reorder(group, -value), y = value, fill = group)) +
  geom_col() + 
  ggtitle('장애인 생활체육 관련 전문적인 지도를 받은 목적(2020)') +
  scale_fill_brewer(palette = "Pastel2") +
  xlab('목적') + 
  ylab('비율(단위: %)') + 
  theme_light() +
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5,
                                  family = "Noto Serif KR"),
        axis.title.x = element_text(family = "Noto Serif KR"),
        axis.title.y = element_text(family = "Noto Serif KR"),
        axis.text.x = element_text(family = "Noto Serif KR", face = 'bold'),
        legend.position = 'none') +
  geom_text(aes(label = paste0(value, "%")),
            position = position_dodge(width=1.8),
            vjust=-0.5, 
            family="Noto Serif KR")
  
# 희망 운동 종목 소분류 상위 10개 종목(2020)----
library(ggplot2)
library(readxl)
library(showtext)
library(RColorBrewer)
font_add_google(name = "Noto Serif KR", family = "Noto Serif KR")
showtext_auto()

hope <- read_excel('data/희망 운동 종목 소분류 상위 10개 종목(2020).xlsx')
hope$value <- hope$value * 100

ggplot(data=hope, aes(x =reorder(group, -value), y = value, fill = group)) +
  geom_col() + 
  # scale_x_discrete(limits=c("맨손 체조", "체력 단련 및 생활 운동 그외 종목", "요가",
  #                           "등산", "걷기 및 가벼운 달리기", "재활 운동", "배드민턴", "자전거, 사이클",
  #                           "보디빌딩(웨이트 트레이닝)", "수영")) +
  ggtitle('희망 운동 종목 소분류 상위 10개 종목(2020)') +
  scale_fill_brewer(palette = "Set3") +
  xlab('목적') + 
  ylab('비율(단위: %)') + 
  theme_light() +
  theme(plot.title = element_text(face = 'bold', # 제목 서식
                                  size = 13,
                                  hjust = 0.5,
                                  family = "Noto Serif KR"),
        axis.title.x = element_text(family = "Noto Serif KR"),
        axis.title.y = element_text(family = "Noto Serif KR"),
        axis.text.x = element_text(family = "Noto Serif KR", face = 'bold', angle=20, vjust = 1, hjust = 1),
        legend.position = 'none') +
  geom_text(aes(label = paste0(rank, "(", value, "%", ")")),
            position = position_dodge(width=1.8),
            vjust=-1.25, 
            family="Noto Serif KR")

