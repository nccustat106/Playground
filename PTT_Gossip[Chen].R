library(dplyr)
library(rvest)
library(RCurl)
library(tm)
library(tmcn)
library(magrittr)
library(tidyr)
#找出最新的頁數在哪裡

curl <- getCurlHandle()# 設定已滿18歲
curlSetOpt(cookie="over18=1", followlocation = TRUE, curl=curl)#一樣問號
url <- paste0("https://www.ptt.cc/bbs/Gossiping/index.html")#終於要放進來惹
gos <- getURL(url, curl=curl)#啊啊粗乃惹
tmp<-read_html(gos) %>% html_nodes(".wide") %>% html_attrs() %>% .[[2]] %>%
  .[[2]] 
min<-tmp %>% gregexpr("[0-9]", .) %>% .[[1]] %>% min()
max<-tmp %>% gregexpr("[0-9]", .) %>% .[[1]] %>% max()
index<-substr(tmp,min,max) %>% as.numeric()


#做一個函數抓所有文章的列表包含標題、日期、作者、推文數


gos.data<-NULL
for(j in (index-10):index){
curl <- getCurlHandle()
curlSetOpt(cookie="over18=1", followlocation = TRUE, curl=curl)
url <- paste0("https://www.ptt.cc/bbs/Gossiping/index",j,".html")
gos <- getURL(url, curl=curl)
gos.title<-read_html(gos) %>%html_nodes(".title a") %>% html_text()
gos.author<-read_html(gos) %>%html_nodes(".author") %>% html_text()
gos.push<-read_html(gos) %>%html_nodes(".nrec") %>% html_text()
gos.date<-read_html(gos) %>%html_nodes(".date") %>% html_text()
gos.link<- read_html(gos) %>% html_nodes(".title a") %>%  html_attr('href')
a<-grep("-",gos.author) 
if(length(gos.link)==length(gos.author)){
  gos.data1=data.frame(gos.title=gos.title,gos.author=gos.author,gos.push=gos.push,gos.date=gos.date,
                       gos.link=gos.link)
}else{
  gos.data1=data.frame(gos.title=gos.title,gos.author=gos.author[-a],gos.push=gos.push[-a],gos.date=gos.date[-a],
                       gos.link=gos.link)
}

gos.data=rbind(gos.data,gos.data1)
print(j)
}

#對推文數做簡單處理
gos.data$gos.push<-gsub("X","-",gos.data$gos.push)
gos.data$gos.push<-gsub("爆","100",gos.data$gos.push)
gos.data$gos.push%<>%as.numeric()
gos.data$gos.link %<>% as.character()




for(i in 1:dim(gos.data)[1]){
  url <-paste0("https://www.ptt.cc/",gos.data$gos.link[i])  
  curl <- getCurlHandle()
  curlSetOpt(cookie="over18=1", followlocation = TRUE, curl=curl)
  url1 <- getURL(url, curl=curl)
  gos.data[i,6]<- read_html(url1) %>% html_nodes("#main-content") %>% html_text()
  print(i)
}

#只想要拿出內文(純參考、待改進)
head(ptt_data)
texx<-strsplit(ptt_data,split = "\n",fixed=T)

q<-c()
for(i in 1:length(ptt_data)){
  q1<-texx[[i]][3:which(texx[[1]]=="--")-1] %>% paste(collapse="")
  q<-c(q,q1)
}


