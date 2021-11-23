#实验四 假设检验
#2021年11月10日

# 作业1
#双样本，方差不相等
two_sample_mean_unequal<-function(a,b,n1,n2,alpha)
{
  a_mean = mean(a)
  a_std = sd(a)
 
  b_mean = mean(b)
  b_std = sd(b)
  
  mean = a_mean - b_mean
  v = (((a_std*a_std/n1)+(b_std*b_std/n2))^2) / ((((a_std*a_std/n1)^2)/(n1-1)) + (((b_std*b_std/n2)^2)/(n2-1)))
  temp = sqrt(a_std*a_std/n1 + b_std*b_std/n2)
  
  x_left = mean - ( qt(1-(1-alpha)/2, v) * temp )
  x_right = mean + ( qt(1-(1-alpha)/2, v) * temp )
  print(paste("置信度为", alpha*100, "%的置信区间是:(", x_left, ",", x_right, ")"))
}

a <-c(628,583,510,554,612,523,530,615)
b <-c(535,433,398,470,567,480,498,560,503,426)
two_sample_mean_unequal(a,b,8,10,0.95)

t.test(a,b,alternative="two.sided")

#作业2
#两个总体下方差比
two_sample_radio<-function(a,b,n1,n2,alpha)
{
  a_std = sd(a)
  b_std = sd(b)
  radio = (a_std*a_std)/(b_std*b_std)
  
  #lower.tail参数要设置为False，否则算的是左边的面积
  F_left = qf((1-alpha)/2, n1-1, n2-1,lower.tail=FALSE)
  F_right = qf(1-(1-alpha)/2, n1-1, n2-1,lower.tail=FALSE)
  
  x_left = radio/F_left
  x_right = radio/F_right
  print(paste("置信度为", alpha*100, "%的置信区间是:(", x_left, ",", x_right, ")"))
  
}

X<-c(20.5,19.8,19.7,20.4,20.1,20,19,19.9)
Y<-c(20.7,19.8,19.5,20.8,20.4,19.6,20.2)
two_sample_radio(X,Y,8,7,0.95)

var.test(X,Y,alternative="two.sided")



#作业3
#第一问
library(XML);library(RCurl)
# 访问英文版的网站（中文版会让后面解码不方便）
url<-"https://www.bitpush.news/covid19/en.php"
url<-getURL(url) #https需要用getURL处理；http则不需要
tbls<-readHTMLTable(url)
world<-tbls[[1]]

# 去除数据第一行的全球数据
world <- world[-1,]
# 去掉数据中的逗号，并转为数值类型
world$`Confirmed cases` <- as.numeric(gsub('[,]','',world$`Confirmed cases`))
world$Deaths <- as.numeric(gsub('[,]','',world$Deaths))
# 保存数据到本地
write.csv(world,'world_Covid19_data.csv')

library(rworldmap)
# 数据副本，方便修改的时候保持world源数据不变
world_data <- read.csv('world_Covid19_data.csv')
# 去掉读取后的第一列没用值
world_data <- world_data[,-1]
# 将国家名转为ISO编码格式
world_data$Location <- unlist(lapply(world_data$Location, rwmGetISO3))

png('confirmed heat.png',height=600,width=1000,units='px',res=100)
# 获取地图数据
world_map = joinCountryData2Map(world_data, joinCode = "ISO3", nameJoinColumn = "Location")  
# 绘制确诊人数热力图
mapCountryData(world_map, nameColumnToPlot = "Confirmed.cases",
               mapRegion = "world", colourPalette = "heat",
               mapTitle = "Global Covid Confirmed", catMethod = "fixedWidth", lwd = 1)
dev.off()

png('dead heat.png',height=600,width=1000,units='px',res=100)
# 获取地图数据
world_map = joinCountryData2Map(world_data, joinCode = "ISO3", nameJoinColumn = "Location") 
# 绘制死亡人数热力图
mapCountryData(world_map, nameColumnToPlot = "Deaths",
               mapRegion = "world", colourPalette = "heat",
               mapTitle = "Global Covid Death", catMethod = "fixedWidth", lwd = 1)
dev.off()

#第二问
China_data <- read.csv('China_COVID19_month_data.csv',header=TRUE)
typeof(China_data)
China_data_df <- data.frame(matrix(unlist(China_data),ncol=4),stringsAsFactors=FALSE)
names <- colnames(China_data)
colnames(China_data_df) <- names
#读取后是char类型，要转换为数值型
China_data_df$confirmed <- as.numeric(China_data_df$confirmed)
China_data_df$cured <- as.numeric(China_data_df$cured)
China_data_df$dead <- as.numeric(China_data_df$dead)

#使用ggplot画图调整x的顺序，必须x轴要设置成有序factor类型

sort = c('Jan,2020','Feb,2020','Mar,2020','Apr,2020','May,2020','Jun,2020','Jul,2020','Aug,2020',
         'Sep,2020','Oct,2020','Nov,2020','Dec,2020','Jan,2021','Feb,2021','Mar,2021','Apr,2021',
         'May,2021','Jun,2021','Jul,2021','Aug,2021','Sep,2021','Oct,2021','Nov,2021')
# 把time类型设置成因子类型，用于排序
China_data_df$time <- factor(China_data_df$time,levels=sort, ordered=TRUE)


png('confirmed.png',height=3000,width=10000,units='px',res=500)
library(ggplot2)
# 绘制确诊图
ggplot(data=China_data_df,aes(time,confirmed,group=1,color='confirmed'))+
  geom_line()+
  # 设置y轴刻度
  scale_y_continuous(limits=c(0,120000))+
  # 绘制点
  geom_point()+
  # 显示每个节点数值
  geom_text(aes(label=confirmed),vjust=-3)+
  # 设置标题
  ggtitle("trend of confirmed")+
  # 设置标题格式
  theme(plot.title=element_text(size=16,hjust=0.5,colour="black",family="Times"))+
  # 设置x轴刻度的角度
  theme(axis.text.x=element_text(angle=15))
dev.off()

# 绘制治愈图  
png('cured.png',height=3000,width=10000,units='px',res=500)
ggplot(data=China_data_df,aes(time,cured,group=1,color='cured'))+
  geom_line()+
  # 设置y轴刻度
  scale_y_continuous(limits=c(0,120000))+
  # 绘制点
  geom_point()+
  # 显示每个节点数值
  geom_text(aes(label=cured),vjust=-3)+
  # 设置标题
  ggtitle("trend of cured")+
  # 设置标题格式
  theme(plot.title=element_text(size=16,hjust=0.5,colour="black",family="Times"))+
  # 设置x轴刻度的角度
  theme(axis.text.x=element_text(angle=15))
dev.off()

# 绘制死亡图  
png('dead.png',height=3000,width=10000,units='px',res=500)
ggplot(data=China_data_df,aes(time,dead,group=1,color='dead'))+
  geom_line()+
  # 设置y轴刻度
  scale_y_continuous(limits=c(0,6000))+
  # 绘制点
  geom_point()+
  # 显示每个节点数值
  geom_text(aes(label=dead),vjust=-3)+
  # 设置标题
  ggtitle("trend of dead")+
  # 设置标题格式
  theme(plot.title=element_text(size=16,hjust=0.5,colour="black",family="Times"))+
  # 设置x轴刻度的角度
  theme(axis.text.x=element_text(angle=15))
dev.off()

# 绘制总的图  
png('result.png',height=3000,width=10000,units='px',res=500)
ggplot(data=China_data_df,aes(time,confirmed,group=1,color='confirmed'))+
  geom_line()+
  # 设置y轴刻度
  scale_y_continuous(limits=c(0,120000))+
  # 设置标题
  ggtitle("trend of confirmed, cured and dead")+
  # 设置标题格式
  theme(plot.title=element_text(size=16,hjust=0.5,colour="black",family="Times"))+
  # 设置x轴刻度的角度
  theme(axis.text.x=element_text(angle=15))+
  # 绘制治愈图
  geom_line(data=China_data_df,aes(time,cured,group=1,color='cured'))+
  # 绘制死亡图
  geom_line(data=China_data_df,aes(time,dead,group=1,color='dead'))
dev.off()

# 第三问
library(dplyr)
library(ggplot2)

world_data_2 <- world
# 修改列名
colnames(world_data_2) <- c('Location','Confirmed','Death')
# 计算死亡占比，并作为新的列添加到数据集中
world_data_2$rate<-world_data_2$Death / world_data_2$Confirmed
# 按照比率倒序排列
world_data_2_sort<-arrange(world_data_2,desc(rate))
# 取前10个国家的数据
world_data_2_sort_10<-world_data_2_sort[1:10,]

png('top10_dead_rate.png',width=3000,height=3000,units='px',res=300)

ggplot(data=world_data_2_sort_10,aes(x=Location,y=rate,fill=Location))+
  geom_bar(stat = 'identity')+
  # 设置标题
  ggtitle("rate of top10 Confirmed")+
  # 设置标题格式
  theme(plot.title=element_text(size=16,hjust=0.5,colour="black",family="Times"))+
  # 设置x轴刻度的角度
  theme(axis.text.x=element_text(angle=15))

dev.off()

# 按照确诊倒序排列
world_data_2_con_sort<-arrange(world_data_2,desc(Confirmed))
# 取前10个国家的数据
world_data_2_con_sort_10<-world_data_2_con_sort[1:10,]

png('top10_confirmed_rate.png',width=3000,height=3000,units='px',res=300)

ggplot(data=world_data_2_con_sort_10,aes(x=Location,y=rate,fill=Location))+
  geom_bar(stat = 'identity')+
  # 设置标题
  ggtitle("rate of top10 Confirmed")+
  # 设置标题格式
  theme(plot.title=element_text(size=16,hjust=0.5,colour="black",family="Times"))+
  # 设置x轴刻度的角度
  theme(axis.text.x=element_text(angle=15))

dev.off()
  

