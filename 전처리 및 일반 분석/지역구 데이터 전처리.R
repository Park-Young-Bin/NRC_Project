#### 구별 데이터 정제 ####

## 중랑 대신 다른 구로 변경 후 쭉 돌리기

library(openxlsx)
# install.packages('reshape2')
library(reshape2)
library(dplyr)

영등포 = read.xlsx("data/영등포구 등록장애인_1차정제.xlsx", startRow = 2)

nrow(영등포)
for (i in 1:nrow(영등포)) {
  if(is.na(영등포$X1[i+1])==T)
    영등포$X1[i+1]=영등포$X1[i] }


ncol(영등포)
temp = 영등포[,18:ncol(영등포)] #심하지 않은 장애 분리 저장(경증)
영등포 = 영등포[,-18:-ncol(영등포)] #심하지 않은 장애 삭제 (중증)

장애정도=rep("중증",count(영등포))
영등포=cbind(장애정도,영등포)
장애정도=rep("경증",count(temp))

names(영등포)[2] = c('읍면동')
names(영등포)[3] = c('연령')

읍면동=영등포$읍면동
연령=영등포$연령
tempt=cbind(장애정도,읍면동,연령,temp)



ex=tempt
ex2=영등포 # 중증

names(ex)
names(ex2)


ex=transform(ex,지적=rep(NA,count(ex))) # NA 고정
ex=transform(ex,자폐성=rep(NA,count(ex))) # NA 고정
ex=transform(ex,정신=rep(0,count(ex)))

ex3=rbind(ex,ex2)             
ex4=melt(ex3, id.vars = c('읍면동', '장애정도', '연령'), variable.name = '장애유형', value.name = '빈도')

ex4$연령대 <- ifelse(ex4$연령 < 8, '학령기 이전',
                  ifelse(ex4$연령 >= 8 & ex4$연령 < 20, '미성년자',
                         ifelse(ex4$연령 >= 20 & ex4$연령 < 40, '청년',
                                ifelse(ex4$연령 >= 40 & ex4$연령 < 65, '중장년', '고령'))))

ex5 =ex4 %>% relocate(c(읍면동,장애유형,장애정도,연령대,빈도)) %>% 
  select(-연령)

ex5$빈도 <- as.numeric(ex5$빈도)

ex6=ex5  %>% 
  group_by(읍면동, 장애유형, 장애정도,연령대) %>% 
  summarise(빈도 = sum(빈도))
ex7=as.data.frame(ex6)

자치구=rep("영등포구",count(ex7))
영등포구=cbind(자치구,ex7)
write.csv(영등포구,"data/영등포구.csv")

