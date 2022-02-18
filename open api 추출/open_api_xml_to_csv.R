install.packages("XML")
library(XML)



# xmlTreeParse()로 파일부르기
a1 <- DocFromXML<-xmlTreeParse("C:/Users/user/OneDrive/바탕 화면/calltaxi.xml",useInternal=T)
a2<-xmlRoot(a1)
a2

#태그값 불러오기

no <- xpathSApply(a2,"//no",xmlValue)
cartype <- xpathSApply(a2,"//cartype",xmlValue)
receipttime <- xpathSApply(a2,"//receipttime",xmlValue)
settime <- xpathSApply(a2,"//settime",xmlValue)
ridetime <- xpathSApply(a2,"//ridetime",xmlValue)
startpos1 <- xpathSApply(a2,"//startpos1",xmlValue)
startpos2 <- xpathSApply(a2,"//startpos2",xmlValue)
endpos1 <- xpathSApply(a2,"//endpos1",xmlValue)
endpos2 <- xpathSApply(a2,"//endpos2",xmlValue)

#dataframe으로 만들기

calltaxi <- data.frame(차량고유번호=no, 
                차량타입=cartype,
                예정일시=receipttime,
                배차일시=settime,
                승차일시=ridetime,
                출발지구군=startpos1,
                출발지상세=startpos2,
                목적지구군=endpos1,
                목적지상세=endpos2)
write.csv(calltaxi,"C:/Users/user/OneDrive/바탕 화면/calltaxi.csv")
