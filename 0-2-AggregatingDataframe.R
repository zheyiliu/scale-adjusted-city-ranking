
#######################################################################
##### 1. 我需要一个dataframe装有（先做一年的，2019年）
# 城市：city
# 地理：geo_lat, geo_lng, geo_class(too ambiguous), 
# 城市发展水平：economic_level几线城市, urbanization_rate|urban_popolation
# 【城市网络角色】：city rank / migration_class-发射地接收地中转地Barthelemy
# longitudinal经济增长水平：βt?
# population, GDP, residual
# 【POI：社区/城市的紧凑程度】，功能多样性程度，smart sprawl程度， diversity of urban spatial patterns
# 【行政边界与自然边界的关系】

##### 找到了一些数据或许可以用，http://www.resdc.cn/data.aspx?DATAID=277 
# 生态系统服务，植被指数，NPP初级生产力二氧化碳相关，空气质量
# POI

##### 2. 然后我要比较不同的统计方法，看看需不需要优化
# OLS: lm(), nls()
# MLE
# SMA
# Bayesian

##### 3. 控制住人口后，GDP的residuals和城市什么宏观属性有关？

##### 4. 写好画图的代码
# 展示原始数据、参数值和估计值 figure 1 & 2
# 展示residuals、residuals的排序、residuals在地图上的样子 figure 3-5

#######################################################################
################ make index~pop dataframe #############################
#######################################################################

IDDELF = function(ddat){
  #ddat = GDP
  iddelf = vector()
  wield = paste0(ddat[,2], ddat[,3], ddat[,4])
  f= ddat[duplicated(wield),]
  if (nrow(f)>0){
    for (i in 1:dim(f)[1]){
      ff = subset(ddat,ddat$city==f$city[i] & ddat$year==f$year[i] & ddat$index==f$index[i])
      iddelf = c(iddelf, ff$id[-1])
    }
    ddat = ddat[which(!ddat$id %in% iddelf),]
  }
  return(ddat)
}


SearchCorValue = function(ORI, COR){ #找到某指标对应的另一指标的值
  ORI = IDDELF(ORI)
  COR = IDDELF(COR)
  corValue = vector()
  for (i100i in 1:dim(ORI)[1]){
    cityL = COR$city==ORI$city[i100i]
    yearL = COR$year==ORI$year[i100i]
    corrr = COR[which(cityL & yearL),]
    corV = corrr$value
    if (length(corV)==0){
      corV = NA}
    if (length(corV)>1){
      print(corrr)
      corV = corV[!is.na(corV)][1]}
    corValue[i100i] = corV
  }
  if (length(ORI$value)==0){
    corValueDF = data.frame(NA)
  }else{
    corValueDF = data.frame(city=ORI$city, year=ORI$year, xindex = as.numeric(ORI$value), yindex = as.numeric(corValue))
  }
  corValueDF = corValueDF[order(corValueDF$year),]
  return(corValueDF)
}


### loading data - imputated version
setwd('C:/Users/Zheyi-LT/OneDrive/mycoffer/')
for (rdat in dir('indexSQL')){load(paste0('indexSQL/',rdat))}

dflist0 = gsub('.Rdata', '', dir('indexSQL')) #全部都需要预处理
dflist1 = grep('POP|\\d', dflist0, invert=T, value=T) #用来算OLS的
dflist2 = c("GDP1st","GDP23","GDP2nd","GDP3rd","POPdensity","POPResident","POPurban")
dflist = c(dflist1, dflist2)
for (yi in 1:length(dflist)){
  xdfname = 'POP'
  ydfname = dflist[yi]
  rangeStatList = c('市辖区', 'Districts', 'BetaD/', 'FigD/')
  rangeStat = rangeStatList[1] #按照指标对应的区域更改
  year = 1985:2017
  xdf = get(xdfname)
  #delcity = c('三沙市')
  #delcity = c('昌都市','拉萨市','林芝市','日喀则市','山南市','那曲市','三沙市','海东市','儋州市','哈密市','吐鲁番市','重庆市')
  #xdf = xdf[which(!(xdf$city %in% delcity)),]
  ydf = get(ydfname)
  ORII = xdf[grepl(rangeStat, xdf$index) & xdf$year%in%year,]
  CORR = ydf[grepl(rangeStat, ydf$index) & ydf$year%in%year,]
  cordf = SearchCorValue(ORII, CORR)
  colnames(cordf)[3:4] = c(xdfname, ydfname)
  f = paste0(ydfname,'_POP_',rangeStatList[2])
  assign(f, cordf)
  eval(parse(text=paste0('save(',f,",file='C:/Users/Zheyi-LT/OneDrive/mycoffer/data/",f,".Rdata')")))
  
  print(c(yi, ydfname))
}

table(na.omit(Area_POP_Districts)$year)

####################################################################
########## compare models and get best residuals ###################
####################################################################

setwd('C:/Users/Zheyi-LT/OneDrive/mycoffer/')
load("data/GDP_POP_Districts.Rdata")

### extract data of focal index and focal time
yeari = 2017
df = subset(GDP_POP_Districts, GDP_POP_Districts$year %in% yeari)
dfc = df[complete.cases(df),]

### data transformation
dfc$GDP.t = log(dfc$GDP)
dfc$POP.t = log(dfc$POP)
dfc$POP.s = (log(dfc$POP)-mean(log(dfc$POP)))
#dfi$POP = as.numeric(as.character(dfi$POP))
# 最小二乘 OLS
m1 = lm(dfc$GDP.t~dfc$POP.t)
# 高斯牛顿法，数值法寻找最小二乘
m2 = nls(GDP ~ a*POP^b, data=dfc, start=list(a=mean(dfc$GDP),b=0))
library(deming)
m3 = deming(GDP.t~POP.t,data=dfc,cv=T) #1.23
library(smatr)
m4 = sma(GDP.t~POP.t,data=dfc, robust=T) #1.44,1.48


########################## bayesian ################################
library(rethinking)

# model
a.start = mean(dfc$GDP.t)
sigma.start <- log(sd(dfc$GDP.t))
bm1 = map(
  alist(
    GDP.t ~ dnorm(mu, exp(log.sigma)),
    mu <- a + b * POP.t,
    a ~ dnorm(mean(GDP.t), sd(GDP.t)),
    b ~ dnorm(0, 1),
    log.sigma ~ dnorm(0, 1)
  ),
  start = list(a=a.start, b=0, log.sigma=sigma.start),
  data = dfc
)



# figure 1: posterior distritbuion of parameter beta, 
#           and predictive distribution of model
POP.t.seq <- seq(range(dfc$POP.t)[1],range(dfc$POP.t)[2], length.out=50 )
mu <- link( bm1 , data=data.frame(POP.t=POP.t.seq) )
mu.mean = apply(mu, 2, mean)
mu.HPDI = apply(mu, 2, HPDI)

GDP.t.sim <- sim( bm1 , data=list(POP.t=POP.t.seq) )
GDP.t.PI <- apply( GDP.t.sim , 2 , PI , prob=0.89 )

png(filename='result/unidimension&singleyear/posterior&prediction.png', width=800, height=600,res=100)
plot( GDP.t ~ POP.t , dfc, col=col.alpha(rangi2,0.7),
      xlab='Population (log)', ylab='GDP (log)', main=yeari)
lines( POP.t.seq , mu.mean )
shade( mu.HPDI , POP.t.seq )
shade( GDP.t.PI , POP.t.seq)
dev.off()

# figure 2: observation GDP vs. prediction GDP
mu <- link( bm1 )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI )
GDP.t.sim <- sim( bm1 , n=1e4 )
GDP.t.PI <- apply( GDP.t.sim , 2 , PI )

plot( mu.mean ~ dfc$GDP.t, col=rangi2 , ylim=range(mu.PI) ,
      xlab="Observed GDP (log)" , ylab="Predicted GDP (log)", main=yeari)
abline( a=0 , b=1 , lty=2 )
for ( i in 1:nrow(dfc) ){
  lines( rep(dfc$GDP.t[i],2) , c(mu.PI[1,i],mu.PI[2,i]) ,
         col=rangi2 )
}
identify( dfc$GDP.t , y=mu.mean , labels=dfc$city , cex=0.8 )


# figure 3: residuals
mu <- coef(bm1)['a'] + coef(bm1)['b']*dfc$POP.t
m.resid <- dfc$GDP.t - mu
#m.resid <- dfc$GDP.t - mu.mean

plot(GDP.t ~ POP.t, dfc, col=rangi2,
     xlab = 'Population (log)', ylab=paste0(colnames(dfc)[4], ' (log)'), main=yeari)
abline(coef(bm1)['a'], coef(bm1)['b'])
#lines( dfc$POP.t, mu.mean)
# loop over States
for ( i in 1:length(m.resid) ) {
  x <- dfc$POP.t[i] # x location of line segment
  y <- dfc$GDP.t[i] # observed endpoint of line segment
  lines( c(x,x) , c(mu.mean[i],y) , lwd=0.5 , col=col.alpha("black",0.7) )
}
identify( x=dfc$POP.t , y=dfc$GDP.t , labels=dfc$city , cex=0.8 )
dspe = subset(dfc, dfc$city %in% c('北京市','上海市','广州市'))
points(dspe$POP.t, dspe$GDP.t, col='red', pch=20)


# figure 4: rank residuals
dfc$resid = m.resid
dfc$rank = rank(-dfc$resid)
dfc$resrankcity = paste(dfc$rank, dfc$city, sep='. ')

#on = c(1:5, seq(5,(n-5),length.out=6),(n-5):n)
on = round(seq(1,length(m.resid),length.out=20))
dfc$labelon = NA
dfc$labelon[dfc$rank %in% on] = dfc$resrankcity[dfc$rank %in% on]

o <- order(dfc$resid)
# make the plot

png(filename='result/rank-best10.png', width=400, height=800,res=50)
dotchart( dfc$resid[o], labels=dfc$labelon[o], xlim=range(dfc$resid), pch=20, cex=1.5)
abline( v=0 , col=col.alpha("black",0.8) )
for ( i in 1:nrow(dfc) ) {
  j <- o[i] # which State in order
  lines( dfc$GDP.t[j]-c(mu.PI[1,j],mu.PI[2,j]) , rep(i,2) )
  points( dfc$GDP.t[j]-c(GDP.t.PI[1,j],GDP.t.PI[2,j]) , rep(i,2),
          pch=3 , cex=0.6 , col="gray" )
}
dev.off()
#ggplot(interdata, aes(x=resid, y=-(rank), colour=baidu_lng)) + 
#  geom_point() + geom_vline(xintercept=0)


newcityinfo = read.csv("otherdata/new_city_info.csv", header=T)

load("data/GDP1st_POP_Districts.Rdata")
gdp1df = subset(GDP1st_POP_Districts, GDP1st_POP_Districts$year %in% yeari)

load("data/GDP3rd_POP_Districts.Rdata")
gdp3df = subset(GDP3rd_POP_Districts, GDP3rd_POP_Districts$year %in% yeari)

load("data/GDP23_POP_Districts.Rdata")
gdp23df = subset(GDP23_POP_Districts, GDP23_POP_Districts$year %in% yeari)


newcityinfo = read.csv("otherdata/new_city_info.csv", header=T)
intd = data.frame()
intd = merge(dfc, newcityinfo, by.x='city', by.y='City_ch', all.x=T)
intd = merge(intd, gdp1df[c(1,4)], by='city')
intd = merge(intd, gdp3df[c(1,4)], by='city')
intd = merge(intd, gdp23df[c(1,4)], by='city')
intds = intd
intds$Founded_year = scale(2019-intd$Founded_year)
intds$baidu_lat = scale(intd$baidu_lat)
intds$baidu_lng = scale(intd$baidu_lng)
intds$gdp1 = scale(intd$GDP1st)
intds$gdp3 = scale(intd$GDP3rd)
intds$gdp23 = scale(intd$GDP23)

attach(intds)
try = lm(resid ~ Founded_year + Economic_level + baidu_lat)
trybest = lm(resid ~ Founded_year + Economic_level + baidu_lat)
try = lm(resid ~ Founded_year + Economic_level + baidu_lat + baidu_lng + gdp3 + gdp23)
trybest = lm(resid ~ Economic_level + baidu_lat + gdp3 + gdp23)

# figure 5 China map
library(maptools)
library(rgdal)
library(ggplot2)
Sys.setlocale("LC_ALL", "chinese")
# reading data: 九段线 chinamap
l9<-rgdal::readOGR('otherdata/mapchina/SouthSea/九段线.shp')
china_map = rgdal::readOGR('otherdata/mapchina/china/bou2_4p.shp')
names(china_map)

# ggplot(china_map,aes(x=long,y=lat,group=group)) +
#   geom_polygon(fill="white",colour="grey") +
#   coord_map("polyconic")

x <- china_map@data #读取行政信息
xs <- data.frame(x,id=seq(0:924)-1) #含岛屿共925个形状
china_map1 <- fortify(china_map) #转化为数据框

library(plyr)
china_map_data <- join(china_map1, xs, type = "full")
#合并两个数据框
 
#china_data <- join(china_map_data, dfc, type="full") 
#china_data$ratio <- cut(china_data$ratio20361,breaks=c(1.20,1.25,1.30,1.35,1.40,1.45,1.50),labels=c('1.20~1.25','1.25~1.30','1.30~1.35','1.35~1.40','1.40~1.45','1.45~1.50'),right=FALSE,order=TRUE)
windowsFonts(myFont = windowsFont("Times New Roman"))
#rhg_cols1 <- c("#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C") #分等级并填充颜色
rhg_cols1 <- c("#fee0d2","#FCBBA1","#fc9272","#FB6A4A","#ef3b2c","#99000d") 
province_city <- read.csv("otherdata/mapchina/chinaprovincecity2.csv") #省会坐标

p1 <- ggplot(china_map_data, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group),fill='white',colour="black") +
  coord_map("polyconic") +
  #geom_text(size=3,aes(x = jd,y = wd,label = province), hjust=0.5,vjust=0.5,data =province_city)+ #添加省会标签
  geom_point(aes(x = baidu_lng, y = baidu_lat, colour=resid), size = 2, alpha=0.8, data=intd) +
  scale_colour_gradient2(low='blue',high='red',mid='grey90', midpoint=0)+
  #scale_colour_gradient(low='#E1FA72', high='#F46FEE'))+
  theme(                            #清除不必要的背景元素
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
 
#绘制小图
p2<-ggplot()+
  geom_polygon(data=china_map_data,aes(x=long,y=lat,group=group),color="grey40",fill="white")+ #绘制分省图
  geom_line(data=l9,aes(x=long,y=lat,group=group),color="red",size=0.5)+ #9段线
  coord_cartesian(xlim=c(105,125),ylim=c(3,30))+ #缩小显示范围在南部区域
  theme(
    aspect.ratio = 1.25, #调节长宽比
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA,color="grey20",linetype=1,size=0.8),
    plot.margin=unit(c(0,0,0,0),"mm"))
    
#嵌合2个图
library(grid) #ggplot也是grid图
vie <- viewport(width=0.15,height=0.2,x=0.7,y=0.35) #定义小图的绘图区域
p1 #绘制大图
print(p2,vp=vie) #在p1上按上述格式增加小图


#################### 单年份的就做到这里 ##############################
# 统计方法：
### 不需要换成Clauset的方法（MLE），因为残差接近正态
### 贝叶斯和OLS结果差不多，但是可视化更好
### 贝叶斯并不相当于MLE，因为首先取了双对数(是吗？)
### nls和lm结果差很多，我个人站lm，因为大部分数据都聚集在小x那里，log比较好
### 主轴回归的参数有较大波动，而且跟前人结果难以比较
### 这个我们先放着，以后可以尝试pre-history那篇文章里的方法
# 残差lm的结果：
### 设市时间越早，纬度越低，（控制了人口规模之后的）经济表现越好；
### 四五线城市经济表现低，一线和新一线城市却不显著
### 加入gdp3(第三产业GDP占比)和gdp23(第二三产业GDP占比之和)作为城市化率之后，
### R2显著提高，23.5→46.2，设市时间不显著了。但gdp23不符合高斯分布
### 事实上gdp23自己就有0.40的R2
#######################################################################


#################### 接下来是单年份多维度 ##########