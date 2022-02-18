cate_class=read.xlsx("C:/Users/user/Downloads/sports.xlsx")
cate_class
row.names(cate_class)=cate_class$종목
cate_class=cate_class[,-1]

matrix=data.matrix(cate_class)

#히트맵 
par("mar")
par(mar=c(1,1,1,1))
heatmap=heatmap(matrix,Rowv=NA, Colv=NA, col=brewer.pal(9,"Blues"), scale="column",margin=c(5,5))
?heatmap
dev.off()

#ggplot
library(openxlsx)
library(ggplot2)
library(reshape2)

data <- melt(df)

ggplot(data) + 
  geom_tile(aes(x = variable, y = 종목, fill = value),alpha = 0.6) + 
  scale_fill_gradient(low = "#FFFFFF", high = "#3333CC") + 
  theme_classic()+
  xlab('장애유형')

