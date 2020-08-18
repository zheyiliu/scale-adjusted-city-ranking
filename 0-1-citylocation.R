
############################################################################
#### 1. 从百度地图扒下来城市坐标
### install packages REmap & Baidumap
# library(devtools)
# install_github('badbye/baidumap')
# install_github('Lchiffon/REmap')
### invalid, install from zip

get_xy = function(city_vec){
  library(rjson)
  library(RCurl)
  library(geosphere)
  AK <- "xE01AbGxGgt8GUnQDG3QqbeggD36sePu" 
  
  #设定空向量
  baidu_lat <- c()
  baidu_lng <- c()
  baidu_geo <- c()
  locations <-c()
  for (location in city_vec) {
    #建立地址转换网址
    url = paste('http://api.map.baidu.com/geocoding/v3/?address=',location,'&output=json&coordtype=wgs84ll&ak=',AK,sep='')
    url_string <- URLencode(url)
    # 捕获连接对象
    connect <- url(url_string)
    
    # 处理json对象
    temp_geo <- fromJSON(paste(readLines(connect,warn = F),collapse=''))
    temp_lat<-temp_geo$result$location$lat
    temp_lng<-temp_geo$result$location$lng
	
	if(length(temp_lat)==0){temp_lat=NA}
	if(length(temp_lng)==0){temp_lng=NA}
    
    # 关闭连接
    close(connect)
    
    baidu_geo  <-c(baidu_geo,temp_geo)
    baidu_lat <- c(baidu_lat,temp_lat)
    baidu_lng <- c(baidu_lng,temp_lng)
    locations <- c(locations,location)
  }
  
  content <- data.frame(locations,baidu_lat,baidu_lng)
  return(content)
}



### 本来是直接用城市名找坐标，但发现百度地图分不清区市，所以要加上省份
### 但我发现加上了省份也不靠谱，因此放弃百度地图的经纬度数据
citylist = read.csv(file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/XY_new_city_info.csv',stringsAsFactors=F)
city1 = substr(citylist$City_ch, start=1, stop=nchar(citylist$City_ch)-1)
citylist$locations = paste0(citylist$Province.1, city1)

citylist1 = subset(citylist, grepl('市',citylist$City_ch))

b = get_xy(citylist1$locations)
r = subset(b, is.na(b$baidu_lat))
r1 = merge(r, citylist1, by='locations')
r2 = get_xy(r1$City_ch)
colnames(r2)[1] = 'City_ch'
r3 = merge(r2, citylist1[c('City_ch','locations')], by.x='City_ch')
allb = rbind(na.omit(b), r3[c('locations','baidu_lat','baidu_lng')])

newcity = merge(citylist, allb, by='locations', all=T)
write.csv(newcity, file='C:/Sync/CoolGirl/Fhe/ecosocialDATA/XY_new_city_info_findxy.csv',row.names=F)

new = newcity[c('City_ch','baidu_lat.x','new$baidu_lat.y','new$baidu_lng.x','new$baidu_lng.y')]
difx = new[(round(new$baidu_lat.x)!=round(new$baidu_lat.y)),]
dify = setdiff(round(new$baidu_lng.x), round(new$baidu_lng.y))




############################################################################
#### 2. 资源型城市分类
setwd('C:/Users/Zheyi-LT/OneDrive/mycoffer/')
nci = read.csv("otherdata/new_city_info.csv", header=T)
#nci$City_ch = as.character(nci$City_ch)
nci$RBCity = 'Non-Resource-Based-City'
grow = c("朔州市、呼伦贝尔市、鄂尔多斯市、松原市、贺州市、
			南充市、六盘水市、毕节市、黔南布依族苗族自治州、
			黔西南布依族苗族自治州、昭通市、楚雄彝族自治州、
			延安市、咸阳市、榆林市、武威市、庆阳市、陇南市、
			海西蒙古族藏族自治州、阿勒泰地区、
		    霍林郭勒市、锡林浩特市、永城市、禹州市、灵武市、
			哈密市、阜康市") #地级行政区+县级市
grow = strsplit(grow, '、')[[1]]
grow = gsub('\n|\\s+', '', grow)

mature = c("张家口市、承德市、邢台市、邯郸市、大同市、阳泉市、
			长治市、晋城市、忻州市、晋中市、临汾市、运城市、
			吕梁市、赤峰市、本溪市、吉林市、延边朝鲜族自治州、
			黑河市、大庆市、鸡西市、牡丹江市、湖州市、宿州市、
			亳州市、淮南市、滁州市、池州市、宣城市、南平市、
			三明市、龙岩市、赣州市、宜春市、东营市、济宁市、
			泰安市、莱芜市、三门峡市、鹤壁市、平顶山市、鄂州市、
			衡阳市、郴州市、邵阳市、娄底市、云浮市、百色市、
			河池市、广元市、广安市、自贡市、攀枝花市、达州市、
			雅安市、凉山彝族自治州、安顺市、曲靖市、保山市、
			普洱市、临沧市、渭南市、宝鸡市、金昌市、平凉市、
			克拉玛依市、巴音郭楞蒙古自治州、
			鹿泉市、任丘市、古交市、调兵山市、凤城市、尚志市、
			巢湖市、龙海市、瑞昌市、贵溪市、德兴市、招远市、
			平度市、登封市、新密市、巩义市、荥阳市、应城市、
			宜都市、浏阳市、临湘市、高要市、岑溪市、东方市、
			绵竹市、清镇市、安宁市、开远市、和田市")
mature = strsplit(mature, '、')[[1]]
mature = gsub('\n|\\s+', '', mature)

decline = c("乌海市、阜新市、抚顺市、辽源市、白山市、伊春市、
			鹤岗市、双鸭山市、七台河市、大兴安岭地区、淮北市、
			铜陵市、景德镇市、新余市、萍乡市、枣庄市、焦作市、
			濮阳市、黄石市、韶关市、泸州市、铜川市、白银市、
			石嘴山市、
			霍州市、阿尔山市、北票市、九台市、舒兰市、敦化市、
			五大连池市、新泰市、灵宝市、钟祥市、大冶市、松滋市、
			潜江市、常宁市、耒阳市、资兴市、冷水江市、涟源市、
			合山市、华蓥市、个旧市、玉门市")
decline = strsplit(decline, '、')[[1]]
decline = gsub('\n|\\s+', '', decline)

revive = c("唐山市、包头市、鞍山市、盘锦市、葫芦岛市、通化市、
			徐州市、宿迁市、马鞍山市、淄博市、临沂市、洛阳市、
			南阳市、阿坝藏族羌族自治州、丽江市、张掖市、
			孝义市、大石桥市、龙口市、莱州市")
revive = strsplit(revive, '、')[[1]]
revive = gsub('\n|\\s+', '', revive)		

nci[nci$City_ch %in% grow,'RBCity'] = 'Grow' #Developing
nci[nci$City_ch %in% mature,'RBCity'] = 'Mature' #Developed
nci[nci$City_ch %in% decline,'RBCity'] = 'Decline' #Degenerate
nci[nci$City_ch %in% revive,'RBCity'] = 'Revive' #Regenerate

write.csv(nci, file='otherdata/new_city_info.csv',row.names=F)
