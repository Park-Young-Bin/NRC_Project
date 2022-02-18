####구별 데이터 정제####

##강동 대신 다른 구로 변경 후 쭉 돌리기

library(openxlsx)
# install.packages('reshape2')
library(reshape2)
library(dplyr)

setwd("G:/내 드라이브/2021 공공빅데이터 인턴/4. 국립재활원 인턴/data")
강동=read.xlsx("강동구 등록장애인_1차 정제.xlsx",startRow = 2)

count(강동)
for (i in 1:1668) {
  if(is.na(강동$X1[i+1])==T)
    강동$X1[i+1]=강동$X1[i] }



temp=강동[,18:29] #심하지 않은 장애 분리 저장 
강동=강동[,-18:-29] #심하지 않은 장애 삭제 
         
장애정도=rep("중증",count(강동))
강동=cbind(장애정도,강동)
장애정도=rep("경증",count(temp))
읍면동=강동$읍면동
연령=강동$연령
tempt=cbind(장애정도,읍면동,연령,temp)



ex=tempt
ex=transform(ex,지적=rep(NA,count(ex)))
ex=transform(ex,자폐성=rep(NA,count(ex)))                    
ex=transform(ex,호흡기=rep(0,count(ex)))

ex2=강동
ex3=rbind(ex,ex2)             
ex4=melt(ex3, id.vars = c('읍면동', '장애정도', '연령'), variable.name = '장애유형', value.name = '빈도')

ex4$연령대 <- ifelse(ex4$연령 < 10, '10대 미만',
                   ifelse(ex4$연령 >= 10 & ex4$연령 < 20, '10대',
                          ifelse(ex4$연령 >= 20 & ex4$연령 < 30, '20대',
                                 ifelse(ex4$연령 >= 30 & ex4$연령 < 40, '30대',
                                        ifelse(ex4$연령 >= 40 & ex4$연령 < 50, '40대',
                                               ifelse(ex4$연령 >= 50 & ex4$연령 < 60, '50대',
                                                      ifelse(ex4$연령 >= 60 & ex4$연령 < 70, '60대',
                                                             ifelse(ex4$연령 >= 70 & ex4$연령 < 80, '70대',
                                                                    ifelse(ex4$연령 >= 80 & ex4$연령 < 90, '80대', '90대' 
                                                                    )))))))))
ex5 =ex4 %>% relocate(c(읍면동,장애유형,장애정도,연령대,빈도)) %>% 
  select(-연령)


ex6=ex5  %>% 
group_by(읍면동, 장애유형, 장애정도,연령대) %>% 
  summarise(빈도 = sum(빈도))
ex7=as.data.frame(ex6)

자치구=rep("강동구",count(ex7))
강동구=cbind(자치구,ex7)
write.csv(강동구,"강동구.csv")
