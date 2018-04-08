install.packages(c("dplyr","rvest"))
library(dplyr)
library(rvest)




beauty.data<-NULL
######資料搜集#####
for(j in 2000:2154){
beauty <- paste0("https://www.ptt.cc/bbs/Beauty/index",j,".html")
beauty.title<-read_html(beauty) %>%html_nodes(".title a") %>% html_text()
beauty.author<-read_html(beauty) %>%html_nodes(".author") %>% html_text()
beauty.push<-read_html(beauty) %>%html_nodes(".nrec") %>% html_text()
beauty.date<-read_html(beauty) %>%html_nodes(".date") %>% html_text()
beauty.link<- read_html(beauty) %>% html_nodes(".title a") %>%  html_attr('href')
a<-grep("-",beauty.author) 
if(length(beauty.link)==length(beauty.author)){
  beauty.data1=data.frame(beauty.title=beauty.title,beauty.author=beauty.author,beauty.push=beauty.push,beauty.date=beauty.date,
                          beauty.link=beauty.link)
}else{
  beauty.data1=data.frame(beauty.title=beauty.title,beauty.author=beauty.author[-a],beauty.push=beauty.push[-a],beauty.date=beauty.date[-a],
                          beauty.link=beauty.link)
}

beauty.data=rbind(beauty.data,beauty.data1)
rm(beauty.data1)
}
####資料處理####
beauty.data$beauty.push<-gsub("X","-",beauty.data$beauty.push)
beauty.data$beauty.push<-gsub("爆","100",beauty.data$beauty.push)
beauty.data$beauty.push%<>%as.numeric()
beauty.data$beauty.link %<>% as.character()
#####選定要抓哪些文章#####
#我們這裡用90推以上的唷！！！
links_ptt_beauty<-beauty.data %>% filter(beauty.push>90) %>% select(beauty.link) 
title_ptt_beauty<-beauty.data %>% filter(beauty.push>90) %>% select(beauty.title) 
push_ptt_beauty<-beauty.data %>% filter(beauty.push>90) %>% select(beauty.push) 

push_ptt_beauty<-push_ptt_beauty[[1]]
links_ptt_beauty<-links_ptt_beauty[[1]]
title_ptt_beauty<-title_ptt_beauty[[1]]
#####想要我的寶藏嗎 都給你 按下run吧####
for(j in 1:length(links_ptt_beauty)){
url <- paste0('https://www.ptt.cc',links_ptt_beauty[j])
url1<-url %>% read_html() %>% html_nodes("a") %>% html_text()
k<-grep(".jpg",url1)
for(r in k){
  
  download.file(url1[r],paste("~/Desktop/NEW/",title_ptt_beauty[j],r,".jpg"), mode="wb")
}
}

newdata=NULL
for(h in 1:length(links_ptt_beauty)){
url<-paste0('https://www.ptt.cc',links_ptt_beauty[h])
tex<-url %>% read_html() %>% html_nodes(".push-content") %>% html_text()
texx<-removePunctuation(tex)
texx<-removeNumbers(texx)
texx<-gsub("[a-zA-Z]","",texx)



row = length(texx)
Gf <-  paste(texx[1:row],collapse = "")
Gfinal <- gsub(" ","",Gf)
Gfinal
word = NULL
n = nchar(Gfinal)
for(i in 1:n-1){
  word <- c(word, substr(Gfinal,i,i+1))
}

wordtable_i <- table(word)
wordtable <- sort(wordtable_i,decreasing = TRUE)
freq<-wordtable[1:5] %>% rownames() 
data1<-data.frame(title_ptt_beauty=title_ptt_beauty[1],push=push_ptt_beauty[1],freq1=freq[1],freq2=freq[2],freq3=freq[3],freq4=freq[4],freq5=freq[5])
newdata<-rbind(newdata,data1)
rm(data1)
}


