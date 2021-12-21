# 问题一
a <-c(1600,1610,1650,1680,1700,1720,1800)
b <-c(1580,1640,1640,1700,1750)
c <-c(1540,1550,1600,1620,1640,1660,1740,1820)
d <-c(1510,1520,1530,1570,1600,1680)

# 先判断是否符合正态分布
shapiro.test(a)
shapiro.test(b)
shapiro.test(c)
shapiro.test(d)

data <- c(a,b,c,d)
account = data.frame(data,class=factor(rep(1:4,c(7,5,8,6)),labels=c('甲','乙','丙','丁')))
# 方差齐性检验
bartlett.test(data~class,data=account)
# 箱式图观察
plot(account$data~account$class, xlab='class', ylab='time')
# 方差分析
a.aov=aov(data~class,data=account)
# 列出总结
summary(a.aov)

# 问题二
class1 <- c(75,77,70,88,72)
class2 <- c(83,80,85,90,84)
class3 <- c(65,67,77,68,65)
class4 <- c(72,70,71,65,82)
# 先判断是否符合正态分布
shapiro.test(class1)
shapiro.test(class2)
shapiro.test(class3)
shapiro.test(class4)

score_data <- c(class1,class2,class3,class4)
# class= 这里是自己起的名字，意思是类别，和后面的class班级不是一回事
score_account = data.frame(score_data,class=factor(rep(1:4,c(5,5,5,5)),labels=c('class1','class2','class3','class4')))
# 非正态分布,使用Levene方差齐性检验
library(car)
leveneTest(score_account$score_data,score_account$class)
# 箱式图观察
plot(score_account$score_data~score_account$class, xlab='class', ylab='score')
# ANOVA为正态方差分析，这里非正态使用KW分析
kruskal.test(score_account$score_data,score_account$class)

# 问题三
a1 = c(3.72, 3.90, 4.20)
a2 = c(5.22, 5.24, 5.08)
a3 = c(5.28, 5.74, 5.54)
b1 = c(3.72, 5.22, 5.28)
b2 = c(3.90, 5.24, 5.74)
b3 = c(4.20, 5.08, 5.54)
# 正态分布检验
shapiro.test(a1)
shapiro.test(a2)
shapiro.test(a3)
shapiro.test(b1)
shapiro.test(b2)
shapiro.test(b3)

wood_data <- c(b1, b2, b3)
wood_account = data.frame(wood_data,speed=gl(3,3,labels=c('B1','B2','B3')),
                          rate=gl(3,1,9,labels=c('A1','A2','A3')))
# 方差齐性检验
bartlett.test(wood_data~rate, data = wood_account) 
bartlett.test(wood_data~speed, data = wood_account) 


# 箱式图观察
plot(wood_account$wood_data ~ wood_account$rate, xlab = "rate", ylab = "strength")
plot(wood_account$wood_data ~ wood_account$speed, xlab = "speed", ylab = "strength")

# 双因素方差分析
wood.aov = aov(wood_data ~ rate+speed, data = wood_account)
summary(wood.aov)

# 问题四
A1 = c(9,10,9,8,9,10,12,11)
A2 = c(12,11,9,8,12,13,11,12)
A3 = c(13,14,15,12,22,16,20,18)
B1 = c(9,10,9,8,12,11,9,8,13,14,15,12)
B2 = c(9,10,12,11,12,13,11,12,22,16,20,18)

# 正态分布检验
shapiro.test(A1) 
shapiro.test(A2) 
shapiro.test(A3) 
shapiro.test(B1) 
shapiro.test(B2) 

wheat_data = c(B1,B2)
wheat_account = data.frame(wheat_data, fertilizer=gl(2,12,labels=c("B1","B2")), 
                           variety=gl(3,4,24,labels=c("A1","A2","A3")))
# 方差齐性检验
bartlett.test(wheat_data ~ variety, data = wheat_account) 
bartlett.test(wheat_data ~ fertilizer, data = wheat_account) 


# 画图来观察数据的特点
op = par(mfrow = c(1, 2)) #分割图形区域
plot(wheat_data ~ variety+fertilizer, data = wheat_account)

# 画图观察两个元素的交互关系
attach(wheat_account)
interaction.plot(variety,fertilizer,wheat_data,legend=F)
interaction.plot(fertilizer,variety,wheat_data,legend=F)

# 双因素方差分析,无交互用+，有交互用*
wheat.aov = aov(wheat_data ~ variety*fertilizer, data = wheat_account)
summary(wheat.aov)



