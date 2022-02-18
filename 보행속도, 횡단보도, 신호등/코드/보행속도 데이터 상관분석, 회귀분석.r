####보행속도 데이터 상관분석, 회귀분석####

#install.packages("PerformanceAnalytics")
install.packages("psych")
install.packages("car")

library(openxlsx)
library(PerformanceAnalytics)
library(psych)
library(car)


data=read.xlsx("보행속도.xlsx")
colnames(data)=c("성별","연령","평균속도(m/s)")
str(data)

#상관행렬 
chart.Correlation(data,histogram = TRUE)

# 성별을 범주형으로 바꿔줌 
data$성별 <- as.factor(data$성별)


#정규성 확인 QQ-plot (직선에 가까울수록 정규성이 만족) 
qqnorm(data$`평균속도(m/s)`) #point형태로 그리기
qqline(data$`평균속도(m/s)`, col = 2)

#정규성 검정 (유의수준=0.05)
shapiro.test(data$`평균속도(m/s)`)

#정규성 확인 QQ-plot (직선에 가까울수록 정규성이 만족) 
qqnorm(data$연령) #point형태로 그리기
qqline(data$연령, col = 2)

#정규성 검정 (유의수준=0.05)
shapiro.test(data$연령)

#다중선형 회귀모형 
lm=lm(data$`평균속도(m/s)`~data$성별+data$연령)
lm
par(mfrow=c(2,2))
plot(lm) #이상치가 몇몇 보이나 선형성, 정규성, 등분산성 등 만족
summary(lm)
# 모든 설명변수 유의함
# R^2가 낮고, p-value도 낮은 경우 -> 모형이 유의하나, 설명력이 낮음 

#다중공선성 확인 위해 산점도 
pairs.panels(data[names(data)])
car::vif(lm)
#10보다 작아서 다중공선성 없다고 판단 

