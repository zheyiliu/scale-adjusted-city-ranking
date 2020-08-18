######################################################################
##### 接下来是多年份多维度
# 多维度发展轨迹
# 时间序列聚类
# 寻找pattern：时间序列
# 寻找pattern：空间依赖

##### 0. 指标选择，数据准备
##### 1. 计算多年份残差，扔进同一个df
# 突然发现，多年份多维度的话，每个城市是个 时间*指标 的矩阵，没法做聚类？
# 是可以的，只要定义矩阵距离就行了
# 首要目标还是对mean.weight和balance指标以及它们的综合指标聚类

##### 7. 聚类

######################################################################
######################################################################

library(rethinking)

setwd('C:/Users/Zheyi-LT/OneDrive/mycoffer/')
for (rdat in dir('data')){load(paste0('data/',rdat))}

##### 0. 指标选择，数据准备
#dflist0 = gsub('.Rdata', '', dir('data')) #全部都需要预处理
dflist1 = c('GDP', 'CityRoadArea', 'PrimarySchool', 'Green', 'ElectricityResident')
### 把换用GDP换用DepositHousehold或Salary，画图差别不大，但排名略有变化。
dflist = paste0(dflist1, '_POP_Districts')

### extract data of focal index and focal time
ydf = get(dflist[1])
for (i in 2:length(dflist)) {ydf = cbind(ydf, get(dflist[i])[,4])}
colnames(ydf)[c(-1:-3)] = dflist1

####################################################################
##### 1. 计算单年份残差和sustain指标，拼接多年份
dfresall = data.frame()
sustainall = data.frame()
yearrange = 1985:2017
for (yeariii in yearrange) {
	yeari = yeariii
	df = subset(ydf, ydf$year %in% yeari)
	dfc = df[complete.cases(df),]
	if (dim(dfc)[1]<130){
		print(paste0('imcomplete-',yeariii))
		next
	}

	### data transformation，Y取log，X标准化，我忘了为啥X做标准化
	dflog = cbind(dfc[,1:2], log(dfc[,-1:-2]))
	dfstd = cbind(dfc[,1:2], scale(dflog[,-1:-2]))

############ 单年份残差df dfres (最小二乘和贝叶斯) ##################
	
	dfres = dfc[,1:3]
	
	for (yi in 4:ncol(dflog)){

		# 最小二乘 OLS
		dfcal = data.frame(y = dflog[,yi], x = dflog[,3])
		m1 = lm(y ~ x, data=dfcal)

		# bayesian
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
	print(paste0('residual-', yeari))
	
	model = '.lm'
	lmcn = dfres[grepl(model,colnames(dfres))]
	lmcns = data.frame(scale(lmcn))
	rownames(lmcns) = dfres$city
	#lmcns = lmcns[order(lmcns[,1]),]
	colnames(lmcns) = gsub('resid.','', colnames(lmcns))
	
#### 用dfres得到lmcns算得单年份sustain指标mean.weight和balance指标 #####
	
	sustain = data.frame(
		city = rownames(lmcns),
		year = yeariii
	)
	for (ri in 1:nrow(lmcns)){
		ci = as.numeric(lmcns[ri,])
		### 把各个城市功能的数值维度都换算到人口的维度上
		dinv = as.numeric(c(7/6, 5/6, 1, 2/3, 1))
		ci.d = ci/dinv
		
		### 考虑各个功能维度的权重，如：
		### w = c(1.15, 0.85, 1, 0.6, 1)
		w = rep(1,length(ci))
		weight = w/sum(w)
		mean.weight = sum(weight*ci)
		
		### Pielou's evenness, 如果所有物种具有相同的相对丰度，则该值为1
		ci.p = abs(ci.d)/sum(abs(ci.d))
		b.evenness = (-sum(ci.p*log(ci.p)))/log(length(ci))
		### 为了考虑有正有负的问题，用变异系数
		meanforcv = sqrt(sum(ci.d^2)) # abs(mean(ci.d)) # abs(mean.weight)
		b.dist = (sum(dist(ci.d))/(nrow(lmcns)*nrow(lmcns)-1))/ meanforcv
		#b.sd = sd(ci.d)
		b.cv = sd(ci.d) / meanforcv
		
		sustain$b.evenness[ri] = b.evenness
		sustain$b.dist[ri] = b.dist
		#sustain$b.sd[ri] = b.sd
		sustain$b.cv[ri] = b.cv
		sustain$mean.weight[ri] = mean.weight
	}
	
	mean.ws = scale(sustain$mean.weight, scale=T, center=F)
	b.even.s = scale(sustain$b.evenness)
	b.dist.s = scale(sustain$b.dist)
	b.cv.s = scale(sustain$b.cv)
	
	sustain$myindex.even = mean.ws + b.even.s
	sustain$myindex.dist = mean.ws - b.dist.s
	sustain$myindex.cv = mean.ws - b.cv.s
	
	print(paste0('sustain-', yeari))
	
#### 拼接多年份 #####	
	dfresall = rbind(dfresall, dfres)
	sustainall = rbind(sustainall, sustain)
}
save(dfresall, file='result/dfresall.Rdata')
save(sustainall, file='result/sustainall.Rdata')




#################################################################
########## 聚类
library(longitudinalData)
load(file='result/sustainall.Rdata')

#nci = read.csv("otherdata/new_city_info.csv", header=T)
#rbc = merge(sustainall, nci[,c('City_ch','RBCity')],by.x='city', by.y='City_ch', all.x=T)
#rbc1 = rbc[rbc$RBCity != 'Non-Resource-Based-City',]
#sustainall = rbc1

### 准备可供分类的矩阵 样本城市 * 指标
Forcls = function(sustainall = sustainall, index='b.cv'){
	cityname = unique(sustainall$city)
	cls = matrix(nrow=length(cityname), ncol=length(1985:2017), 
				dimname = list(cityname, as.character(1985:2017)))
	for (cri in 1:nrow(cls)){
		cityi = unique(sustainall$city)[cri]
		sustaincity = sustainall[sustainall$city==cityi,]
		sustaincity = merge(data.frame(year=1985:2017), sustaincity, by='year',all=T)
		cityv = sustaincity[,index]
		cls[cri,]=cityv
	}
	return(cls)
}

cls.cv = Forcls(sustainall, 'b.cv')
cls.mean = Forcls(sustainall, 'mean.weight')
cls.syn = Forcls(sustainall, 'myindex.cv')

### imputation，插值补足所有缺失值用于聚类，以后用完整数据就不用插值了
Imp = function(cls){
	x0 = as.data.frame(cls)
	tt = apply(is.na.data.frame(x0), 2, sum)
	ss = apply(is.na.data.frame(x0), 1, sum)
	x0 = x0[c(names(ss[ss<20])),c(names(tt[tt<50]))]
	x0 = as.matrix(x0)

	x1 = imputation(x0,method="copyMean.bisector")
	return(x1)
}
#plot(as.numeric(dimnames(cls.syn.imp)[[2]]),cls.syn.imp[1,],type="o",ylab="",xlab="LI-Global")
#lines(as.numeric(dimnames(x0)[[2]]),x1[3,],col=2,type="o",lwd=3)

cls.cv.imp = Imp(cls.cv)
cls.mean.imp = Imp(cls.mean)
cls.syn.imp = Imp(cls.syn)

library(factoextra)
library(cluster)
# 用于聚类的df
cls = cls.syn.imp
res = get_clust_tendency(cls, 50, graph = TRUE)
res$hopkins_stat
#res$plot
### Hopkins统计量的值>0.5，表明数据并非高度可聚合的

covv <- cov(t(cls))
corr <- cor(t(cls))
d <- as.dist(1-corr)
hc <- hclust(d)
plot(hc,hang=-1,labels=rownames(cls))
rect.hclust(hc,k=4)  
groups<-cutree(hc,k=4)

hcd = as.dendrogram(hc)



nci = read.csv("otherdata/new_city_info.csv", header=T)

D = nci[nci$RBCity=='Decline', 'City_ch']
G = nci[nci$RBCity=='Grow', 'City_ch']
M = nci[nci$RBCity=='Mature', 'City_ch']
N = nci[nci$RBCity=='Non-Resource-Based-City', 'City_ch']
R = nci[nci$RBCity=='Revive', 'City_ch']

a = 200
b = 50
col1 = rep(1,length(D))
names(col1) = as.character(D)
col2 = rep(2,length(G))
names(col2) = as.character(G)
col3 = rep(3,length(M))
names(col3) = as.character(M)
col4 = rep(4,length(N))
names(col4) = as.character(N)
col5 = rep(5,length(R))
names(col5) = as.character(R)
collist = c(col1,col2,col3,col4,col5)

labelColors = c(rgb(220,20,60,a,max=255), rgb(60,179,113,a,max=255),
	rgb(255,140,0,a,max=255), rgb(169,169,169,b,max=255), rgb(65,105,225,a,max=255))
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[collist[which(names(collist) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

clusDendro = dendrapply(hcd, colLab)
# make plot
png(filename=paste0(home, '/Results/',modelname,'/clustering/',methodimp,'_P_hClustering.png'),
    width=11,height=10, units='cm',res=150)
par(mar=c(7,2,1,1)+0.1,mgp=c(3,1,0))
plot(clusDendro)
dev.off() 





