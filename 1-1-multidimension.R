
######################################################################
##### 接下来是单年份多维度
##### 展示单年份单城市的城市功能维度图
# GDP
# CityRoadArea
# School
# AreaBuilt

##### 0. 指标选择，数据准备
##### 1. 计算多维度残差，扔进同一个df
##### 2. 可视化：雷达图
##### 3. 各个功能维度的residual之间有什么相互作用
##### 4. 受什么宏观属性影响？
##### 5. 建立城市功能可持续性/平衡性的指标；
#####    参数的使用取决于它的相对数值或不同参数之间的可比关系，而不是它的绝对量
##### 6. 整个城市系统可持续性/平衡性的分布情况；四分法
##### 7. 聚类


######################################################################
######################################################################

setwd('C:/Users/Zheyi-LT/OneDrive/mycoffer/')
for (rdat in dir('data')){load(paste0('data/',rdat))}

##### 0. 指标选择，数据准备
#####    按R2大小排序
betadf = read.csv('otherdata/200_sumlmHorizontal_Districts.csv',header=T)
bdf = na.omit(betadf)
b = aggregate(bdf$Rsquare, by=list(bdf$yIndex), FUN=mean)
bb = aggregate(bdf$Observation, by=list(bdf$yIndex), FUN=mean)
a = as.data.frame(table(bdf$yIndex))
forchoose = merge(b,a, by.x='Group.1',by.y='Var1')
forchoose = merge(forchoose,bb, by.x='Group.1',by.y='Group.1')
colnames(forchoose) = c('yIndex','R2','yearFreq','observation')
forchoose = forchoose[order(forchoose$R2, forchoose$yearFreq, forchoose$observation, decreasing=T),]




#### 一个维度由一个指标代表
# dflist1 = c('GDP', 'CityRoadArea', 'PrimarySchool', 'Green', 'ElectricityResident')
## 把换用GDP换用DepositHousehold或Salary，画图差别不大，但排名略有变化。
# dflist = paste0(dflist0, '_POP_Districts')




##### 一个维度由多个指标代表
# social-economic
nice1 = c('Retail','GDP','DepositHousehold','Salary'
         #,'Bus','Book','BusPassenger'
)
# infrastructure，这一类R2都不是很高，但只要残差正态就可以
# 'AreaBuilt'0.51,'CityRoadArea'0.50,'FixedAssets'0.46
nice2 = c('AreaBuilt','CityRoadArea','FixedAssets')
# social service
nice3 = c('PrimaryTeacher','HospitalBerth','Doctor','Hospital','PrimarySchool')
# energy / innovation 我还没有数据
nice4 = c('WaterResident','ElectricityResident', 'Electricity','Water'
		# 'Electricity','Water' R2低于0.5
)
# environment, R2低
nice5 = c('Green')
dfmat = data.frame(
	yname = c(nice1,nice2,nice3,nice4,nice5),
	type = c(rep('economy', length(nice1)), rep('infras', length(nice2)), 
			 rep('survice', length(nice3)), rep('energy', length(nice4)),
			 rep('envir', length(nice5))),
	weight = c(rep(1/length(nice1),length(nice1)), rep(1/length(nice2),length(nice2)),
			   rep(1/length(nice3),length(nice3)), rep(1/length(nice4),length(nice4)),
			   rep(1/length(nice5),length(nice5))),
	dfname = paste0(c(nice1,nice2,nice3,nice4,nice5), '_POP_Districts'),
	stringsAsFactors = FALSE
)




#【每个维度用多指标代表，修改到这里】



### extract data of focal index and focal time
ydf = get(dfmat$dfname[1])
for (i in 2:nrow(dfmat)) {ydf = cbind(ydf, get(dfmat$dfname[i])[,4])}
colnames(ydf)[c(-1:-3)] = dfmat$yname
yeari = 2017
df = subset(ydf, ydf$year %in% yeari)
dfc = df[complete.cases(df),]

### data transformation，Y取log，X标准化，我忘了为啥X做标准化
dflog = cbind(dfc[,1:2], log(dfc[,-1:-2]))
dfstd = cbind(dfc[,1:2], scale(dflog[,-1:-2]))

################################################################################
##### 1. 计算多维度残差，扔进同一个df，可视化：雷达图
### collect residuals for standardized indicators 
dfres = dfc[,1:3]

for (yi in 4:ncol(dflog)){

	# 最小二乘 OLS
	dfcal = data.frame(y = dflog[,yi], x = dflog[,3])
	m1 = lm(y ~ x, data=dfcal)

	# bayesian
	library(rethinking)
	a.start = mean(dfcal$y)
	sigma.start <- log(sd(dfcal$y))
	bm1 = map(
	  alist(
		y ~ dnorm(mu, exp(log.sigma)),
		mu <- a + b * x,
		a ~ dnorm(mean(y), sd(y)),
		b ~ dnorm(0, 1),
		log.sigma ~ dnorm(0, 1)
	  ),
	  start = list(a=a.start, b=0, log.sigma=sigma.start),
	  data = dfcal
	)
	#mu <- coef(bm1)['a'] + coef(bm1)['b']*dfcal$x
	#m.resid <- dfcal$y - mu
	mu <- link( bm1 )
	mu.mean <- apply( mu , 2 , mean )
	m.resid1 <- dfcal$y - mu.mean

	dfres[paste0(colnames(dflog)[yi], '.resid.lm')] = resid(m1)
	dfres[paste0(colnames(dflog)[yi], '.resid.bm')] = m.resid1
}

model = '.lm'
lmcn = dfres[grepl(model,colnames(dfres))]
lmcns = data.frame(scale(lmcn))
rownames(lmcns) = dfres$city
lmcns = lmcns[order(lmcns[,1]),]
colnames(lmcns) = gsub('resid.','', colnames(lmcns))

#################################################################################
### 2. 可视化：雷达图。选择一种模型(lm/bm)画图展示一个/几个城市单年份多维度功能图
### 去量纲 standardize 
up = ceiling(max(abs(lmcns)))
dat = rbind(rep(up, ncol(lmcns)), 
            rep(-up, ncol(lmcns)), 
#            rep(0, ncol(lmcns)),
            lmcns[c('徐州市','合肥市','深圳市'),])

### radar chart
library(fmsb)
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

radarchart( dat  , axistype=1 , 
   pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1, seg=up*2,
   cglcol="grey", cglty=1, axislabcol="grey",  cglwd=0.8,
   vlcex=0.8, centerzero=T, caxislabels=seq(-up, up, length.out=up*2+1),
   )
legend(x=0.7, y=1, legend = rownames(dat[-c(1,2),]), 
  bty = "n", pch=20 , col=colors_in , text.col = "grey", 
  cex=1.2, pt.cex=3)

################################################################################
##### 3. 各个功能维度的residual之间有什么相互作用
### pairwise correlation of indicators & their linear regression
cor(lmcns)
library(GGally)
ggpairs(lmcns)
#ggpairs(lmcns, lower=list(continuous="smooth"),
#  diag=list(continuous="barDiag")) 
ggcorr(lmcns, method = c("everything", "pearson"),label = T) 
summary(lm(lmcns[,1] ~ lmcns[,2]+lmcns[,3]+lmcns[,4]+lmcns[,5]))
summary(lm(lmcns[,2] ~ lmcns[,1]+lmcns[,3]+lmcns[,4]+lmcns[,5]))
summary(lm(lmcns[,3] ~ lmcns[,1]+lmcns[,2]+lmcns[,4]+lmcns[,5]))
summary(lm(lmcns[,4] ~ lmcns[,1]+lmcns[,2]+lmcns[,3]+lmcns[,5]))
summary(lm(lmcns[,5] ~ lmcns[,1]+lmcns[,2]+lmcns[,3]+lmcns[,4]))
# GDP: CityRoadArea, HospitalBerth, Green 0.53
# CityRoadArea: GDP, Green, LivingSpace 0.60
# HospitalBerth: GDP, LivingSpace 0.29
# Green: GDP, CityRoadArea, LivingSpace  0.67
# LivingSpace: CityRoadArea, HospitalBerth, Green 0.54
summary(lm(lmcns[,1] ~ lmcns[,2]+lmcns[,3]+lmcns[,4]+lmcns[, 2]:lmcns[, 4]))
# GDP: CityRoadArea, HospitalBerth, Green, CityRoadArea:Green 0.54

#################################################################################
##### 4. 受什么宏观属性影响？
load("data/GDP1st_POP_Districts.Rdata")
gdp1df = subset(GDP1st_POP_Districts, GDP1st_POP_Districts$year %in% yeari)

load("data/GDP3rd_POP_Districts.Rdata")
gdp3df = subset(GDP3rd_POP_Districts, GDP3rd_POP_Districts$year %in% yeari)

load("data/GDP23_POP_Districts.Rdata")
gdp23df = subset(GDP23_POP_Districts, GDP23_POP_Districts$year %in% yeari)

lmcnc = data.frame(city=dfres$city, lmcn)
newcityinfo = read.csv("otherdata/new_city_info.csv", header=T)
intd = data.frame()
intd = merge(lmcnc, newcityinfo, by.x='city', by.y='City_ch', all.x=T)
intd = merge(intd, gdp1df[c(1,4)], by='city')
intd = merge(intd, gdp3df[c(1,4)], by='city')
intd = merge(intd, gdp23df[c(1,4)], by='city')
intds = intd
intds$Founded_year = scale(yeari-intd$Founded_year)
intds$baidu_lat = scale(intd$baidu_lat)
intds$baidu_lng = scale(intd$baidu_lng)
intds$gdp1 = scale(intd$GDP1st)
intds$gdp3 = scale(intd$GDP3rd)
intds$gdp23 = scale(intd$GDP23)

d = na.omit(intds[,c('Founded_year','baidu_lat','baidu_lng','gdp3','gdp23')])
for (ncc in 1:ncol(d)){d[,ncc]=as.numeric(d[,ncc])}
ggcorr(d, method = c("everything", "pearson"),label = T)

# plot effect size
attach(intds)
for (ni in 2:(ncol(lmcns)+1)){
    a = step(lm(intd[,ni] ~ Founded_year + Economic_level + baidu_lat + baidu_lng + gdp3 + gdp23))
    yname = colnames(intd)[ni]

	#show effect size
	effectsize = data.frame(summary(a)$coef[-1,])
	r2 = round(summary(a)$r.squared, 2)
	attach(effectsize)
	factors = row.names(effectsize)
	png(filename=paste0('result/effectsize_',yname,'.png'), width=600, height=600,res=100)
	gp1 = ggplot(effectsize, aes(x = nrow(effectsize):1, y = Estimate, label = factors)) + 
	  geom_point(size = 3) + 
	  geom_errorbar(aes(ymax=Estimate+1.96*Std..Error, ymin=Estimate-1.96*Std..Error), width = 0.2) + 
	  geom_text(vjust = -1.2, col = "red") + 
	  geom_hline(yintercept = 0, linetype = 2) + 
	  xlab("") + 
	  ylab(paste0(yname, " - ", yeari, " - Effect size and 95% confidence interval (R-sqaure=",r2, ")")) + 
	  theme_bw() + 
	  theme(axis.text.y = element_blank(), 
			axis.ticks.y = element_blank(), 
			panel.grid = element_blank()
			) + 
	  coord_flip()
	print(gp1)
	dev.off()
}


##################################################################################
##### 5. 建立城市功能可持续性/平衡性的指标；
# 参数的使用取决于它的相对数值或不同参数之间的可比关系，而不是它的绝对量
# 数值越大越优渥，富裕（经济），基建便利（基建），医疗条件好（社会服务），居住环境好（LivingSpace, Green）
# 数值越平均越平衡。
#     不能用1/sd，因为当sd为0时1/sd是正无穷，最好限制在[0,1]之间
#     但是用evenness的话，1/evenness也是正无穷，好像也没啥问题
#     发发推荐了：拿矩形为例，面积跟边长比，在边长一定的情况下，各边长分布越均匀，面积越大
# 能反映整体的正负
# 比较residual时，是不是应该除以人口或人口^beta？

sustain = data.frame(
	city = rownames(lmcns)
)
for (ri in 1:nrow(lmcns)){
	ci = as.numeric(lmcns[ri,])
	### 把各个城市功能的数值维度都换算到人口的维度上，log(Y) = log(a) + b*log(X) + resid
	### 则 Y = a * X^b * exp(resid) = a * (exp(resid/b) * x )^b,
	### 该城市的Y值相当于exp(resid/b)倍X的规模效应效果，比较 resid/b 的含义是 比较是x的几倍
	### 但这种比较是基于规模效应的，但需要注意，单单用这一步并没有去量纲，只是换算到人口维度上比较
	### 如果直接用resid，比较的是“不能被规模效应所解释的performance的偏差”
	### 如果不换算: 
	#dinv = as.numeric(c(1, 1, 1, 1, 1))
	dinv = as.numeric(c(7/6, 5/6, 1, 2/3, 1))
	ci.d = ci/dinv
	
	### 考虑各个功能维度的权重，如：
	### w = c(1.15, 0.85, 1, 0.6, 1)
	w = rep(1,length(ci))
	weight = w/sum(w)
	mean.weight = sum(weight*ci)
	
	### Pielou's evenness, 如果所有物种具有相同的相对丰度，则该值为1
	### 因为有log，所以要先绝对值，pi的含义是：
	### 在该城市的 所有对平均水平的偏差中，由某城市功能维度i贡献的比例
	### 还是没有处理有正有负的问题
	ci.p = abs(ci.d)/sum(abs(ci.d))
	b.evenness = (-sum(ci.p*log(ci.p)))/log(length(ci))
	### 为了考虑有正有负的问题，可以用极值差，或者纳入更多信息，用距离之和
	### 但这会出现一个问题就是残值均值离0越远，残差之间的距离也会越远
	### 所以参考工业生产的均匀度 max(x)-min(x)/mean(x)，改成 mean(dist)/mean(x)
	### （极值差相当于最大距离，mean(dist)相当于平均距离）
	### 其实都叫变异系数：全距系数(极差)，平均差系数，标准差系数
	meanforcv = sqrt(sum(ci.d^2)) # abs(mean(ci.d)) # abs(mean.weight)
	b.dist = (sum(dist(ci.d))/(nrow(lmcns)*nrow(lmcns)-1))/ meanforcv
	#b.sd = sd(ci.d)
	b.cv = sd(ci.d) / meanforcv
	
	#myindex.even = mean.weight * b.evenness
	#myindex.dist = mean.weight / b.dist
	#myindex.cv = mean.weight / b.cv
	
	sustain$b.evenness[ri] = b.evenness
	sustain$b.dist[ri] = b.dist
	#sustain$b.sd[ri] = b.sd
	sustain$b.cv[ri] = b.cv
	sustain$mean.weight[ri] = mean.weight
	#sustain$myindex.even[ri] = myindex.even
	#sustain$myindex.dist[ri] = myindex.dist
	#sustain$myindex.cv[ri] = myindex.cv
}

#### 还需要一个考虑了mean.weight和balance指标的综合性指标
#### 如果直接加减的话，会出现还是更依赖均值的问题
#### 如果相除的话，当b.cv=0时，会出现无意义情况
#### 所以试试去尺度化，但中心化会让mean.weight丢掉正负，center=F
mean.ws = scale(sustain$mean.weight, scale=T, center=F)
b.even.s = scale(sustain$b.evenness)
b.dist.s = scale(sustain$b.dist)
b.cv.s = scale(sustain$b.cv)

sustain$myindex.even = mean.ws + b.even.s
sustain$myindex.dist = mean.ws - b.dist.s
sustain$myindex.cv = mean.ws - b.cv.s

a = head(sustain[order(sustain$b.evenness, decreasing=T),],20)
b = head(sustain[order(sustain$b.dist, decreasing=F),],20)
c = head(sustain[order(sustain$b.cv, decreasing=T),],20)
d = head(sustain[order(sustain$myindex.even, decreasing=T),],20) #mean.weight * evenness
e = head(sustain[order(sustain$myindex.dist, decreasing=T),],20) #mean.weight / extrem
f = head(sustain[order(sustain$myindex.cv, decreasing=T),],20) #mean.weight * balance
g = head(sustain[order(sustain$mean.weight, decreasing=T),],20) #mean.weight * balance
length(intersect(a$city, b$city))/20

sustain[sustain$city%in%c('东莞市','深圳市','淮北市'),]
lmcns[c(225,226,87),]

# 画可持续性指标的排序图
dfc = sustain
subject = dfc$myindex.dist
dfc$rank = rank(-subject)
dfc$resrankcity = paste(dfc$rank, dfc$city, sep='. ')

#on = c(1:5, seq(5,(n-5),length.out=6),(n-5):n)
on = round(seq(1,nrow(dfc),length.out=20))
dfc$labelon = NA
dfc$labelon[dfc$rank %in% on] = dfc$resrankcity[dfc$rank %in% on]

o <- order(subject)
# make the plot

png(filename='result/rank-best10-sustain.png', width=400, height=800,res=50)
dotchart( subject[o], labels=dfc$labelon[o], xlim=range(subject), pch=20, cex=1.5)
abline( v=0 , col=col.alpha("black",0.8) )
for ( i in 1:nrow(dfc) ) {
  j <- o[i] # which State in order
  lines( dfc$GDP.t[j]-c(mu.PI[1,j],mu.PI[2,j]) , rep(i,2) )
  points( dfc$GDP.t[j]-c(GDP.t.PI[1,j],GDP.t.PI[2,j]) , rep(i,2),
          pch=3 , cex=0.6 , col="gray" )
}
dev.off()

##### 6. 整个城市系统可持续性/平衡性的分布情况
hist(sustain$b.evenness, breaks=60)
hist(sustain$b.dist, breaks=60)
hist(sustain$b.cv, breaks=60)
hist(sustain$myindex.even, breaks=60)
hist(sustain$myindex.cv,breaks=60)
hist(sustain$mean.weight, breaks=60)

# 四分法：按照mean和balance的大小，给城市分类：成熟城市-优渥平衡，低水平均衡-贫瘠平衡，过渡1-贫瘠失衡，过渡2-优渥失衡
#sustain0 = sustain[sustain$b.cv > quantile(sustain$b.cv, 0.05) & sustain$b.cv < quantile(sustain$b.cv,0.95),]
sustain0 = sustain
sustain0$ba = -sustain0$b.cv
plot(mean.weight~ba, data=sustain0,col='grey70')
identify(sustain0$ba, y=sustain0$mean.weight, labels=sustain0$city, cex=0.8)
sustain1 = sustain0[sustain0$city %in% c('北京市','上海市','广州市','深圳市'),]
sustain2 = sustain0[sustain0$city %in% c('成都市','杭州市','重庆市','武汉市','西安市',
				'苏州市','天津市','南京市','长沙市','郑州市','东莞市','青岛市',
				'沈阳市','宁波市','昆明市'),]
sustain_compare = sustain0[sustain0$city %in% c('成都市','重庆市','青岛市','济南市'),]

points(mean.weight~ba, data=sustain1, pch=16, col='red')
points(mean.weight~ba, data=sustain2, pch=16, col='blue')
points(mean.weight~ba, data=sustain_compare, pch=16, col='green')


### point out resource-based city
nci = read.csv("otherdata/new_city_info.csv", header=T)
sustain_rbc = merge(sustain0, nci[,c('City_ch','RBCity')], 
					by.x='city', by.y='City_ch', all.x=T)
a = 200
b = 50
sustain_rbc$color = NA
sustain_rbc[sustain_rbc$RBCity=='Decline', 'color']= rgb(220,20,60,a,max=255)
sustain_rbc[sustain_rbc$RBCity=='Grow', 'color'] = rgb(60,179,113,a,max=255)
sustain_rbc[sustain_rbc$RBCity=='Mature', 'color'] = rgb(255,140,0,a,max=255)
sustain_rbc[sustain_rbc$RBCity=='Non-Resource-Based-City', 'color'] = rgb(169,169,169,b,max=255)
sustain_rbc[sustain_rbc$RBCity=='Revive', 'color'] = rgb(65,105,225,a,max=255)

plot(ba~mean.weight, col=color, data=sustain_rbc, pch=16,cex=1.2)
legend("topright",legend=c('Grow', 'Mature', 'Decline', 'Revive', 'Non-Resource-Based-City'),
		col=c(rgb(60,179,113,a,max=255), rgb(255,140,0,a,max=255), 
			rgb(220,20,60,a,max=255), rgb(65,105,225,a,max=255), rgb(169,169,169,b,max=255)), 
		cex=0.8,bty="n",pch=20, pt.cex=3)
abline(v=quantile(sustain_rbc$ba,0.5),lty=3)
abline(h=0,lty=3)
identify(sustain_rbc$ba, y=sustain_rbc$mean.weight, labels=sustain_rbc$city, cex=0.8)


# cols = alpha(c('Grow'='#DC143C', 'Mature'='#4169E1', 'Decline'='#008080',
  # 'Revive'='#DAA520', 'Non-Resource-Based-City'='#A9A9A9'),c(rep(0.9,4),0.3))
# p = ggplot(sustain_rbc, aes(x=ba, y=mean.weight, color=RBCity)) +
	# geom_point(aes(x=ba, y=mean.weight),cex=2) +
	# scale_colour_manual(values=cols) +
	# geom_vline(xintercept=quantile(sustain0$ba,0.5)) +
	# geom_hline(yintercept=0)
# print(p)
# identify(sustain_rbc$ba, y=sustain_rbc$mean.weight, labels=sustain_rbc$city, cex=0.8)

###################################################################
### 7. cluster 
### 标准化后的残差df和sustain指标矩阵都无法聚类（最佳分1类）
### sustain指标可能还需要改良
### https://blog.csdn.net/woodcorpse/article/details/79274461
### 
library(factoextra)
library(cluster)
# 用于聚类的df
sustain1 = sustain0[,c('mean.weight','ba')]
rownames(sustain1) = sustain0$city
cls = sustain1
#cls = lmcns
res = get_clust_tendency(cls, 50, graph = TRUE)
res$hopkins_stat
res$plot
### Hopkins统计量的值>0.5，表明数据并非高度可聚合的

### k-means
res.km = eclust(cls, "kmeans")
# Gap statistic plot
fviz_gap_stat(res.km$gap_stat)
# 试试3类
km.res = kmeans(cls, 3, nstart = 25)
# Visualize clusters using factoextra
fviz_cluster(km.res, cls)
sil = silhouette(km.res$cluster, dist(cls))
rownames(sil) = rownames(cls)
head(sil[, 1:3])
# Visualize
fviz_silhouette(sil)

### 层级聚类,报错，k must be between 2 and 226
# Enhanced hierarchical clustering
res.hc = eclust(cls, "hclust") # compute hclust
fviz_dend(res.hc, rect = F) # dendrogam
# 基础款层级聚类
hc<-hclust(dist(cls),method = "ave")  #注意hcluster里边传入的是dist返回值对象
plot(hc,hang=-1,labels=rownames(cls))  #这里的hang=-1使得树的节点在下方对齐
rect.hclust(hc,k=3)  
groups<-cutree(hc,k=3) 

### k-medoids
library(fpc)
iris2.pamk<-pamk(cls)
#table(iris2.pamk$pamobject$clustering,iris$Species)
layout(matrix(c(1,2),1,2)) #每页显示两个图
plot(iris2.pamk$pamobject)

###
ds<-dbscan(cls,eps=0.42,MinPts = 5)
#打印出ds和iris2的聚类散点图
plot(ds,cls)
#plot(ds,cls[,c(1,4)])
plotcluster(cls,ds$cluster)