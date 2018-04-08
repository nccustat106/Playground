# install.packages("rJava")
# install.packages("Rwordseg", repos="http://R-Forge.R-project.org")
# install.packages("tm")
# install.packages("tmcn", repos="http://R-Forge.R-project.org", type="source")
# install.packages("XML")
# install.packages("RCurl")

library(XML)
library(RCurl)
library(magrittr)
library(stringi)
library(rvest)





link_cut <- function(str,start,end){
    initial <- regexpr(pattern = start,text = str) %>% as.numeric
    final <- regexpr(pattern = end,text = str) %>% as.numeric %>%
        `+`(nchar(end)) %>% `-`(1)
    cut <- substr(x = str, start = initial, stop = final) %>% paste0("https://www.ptt.cc/",.)
    cut[!grepl("html",cut)] <- "Not Found"
    return(cut)
}

get_content <- function(link){
    content <- NULL
   
    for(j in 1:length(link)){
        if(link[j]!="Not Found"){
            cont.tmp <- link[j] %>% getURL(curl=curl)%>% read_html %>%
                html_nodes("#main-content") %>% html_text %>% content_cut
            # tmp <- link[j] %>% getURL(curl=curl)%>% read_html %>%
            #     html_nodes(xpath = '//div[contains(@id,"main-content")]') %>%
            #     html_text
            content <- c(content,cont.tmp)
        }else{
            content <- c(content,"No Content")
        }
    } 
    return(content)
}



content_cut<- function(content){
    initial <- regexpr(pattern = "作者",text = content) %>% as.numeric
    final <- regexpr(pattern = "發信站",text = content) %>% as.numeric %>% `-`(1)
    main_content <- substr(x = content, start = initial, stop = final)
    return(main_content)
}


get_push <- function(link){
    push.tmp <- link %>% getURL(curl=curl)%>% read_html %>%
        html_nodes(".push") %>% html_text 
}


# stri_encode( attr(html, "Content-Type")[2], "utf8")

Gossip <- function(num1,num2){
    
ptt.GS <- NULL

for(i in num1:num2){
    curl <<- getCurlHandle()
    curlSetOpt(cookie="over18=1", followlocation = TRUE, curl=curl)
    url <- paste0("https://www.ptt.cc/bbs/Gossiping/index",i,".html") %>% getURL(curl = curl)
    title <- read_html(url) %>% html_nodes(".title") %>% html_text %>% stringr::str_trim()
    date <- read_html(url) %>% html_nodes(".date") %>% html_text
    author <- read_html(url) %>% html_nodes(".author") %>% html_text
    nrec <- read_html(url) %>% html_nodes(".nrec") %>% html_text
    mark <- read_html(url) %>% html_nodes(".mark") %>% html_text
    link <- read_html(url) %>% html_nodes(".title") %>% link_cut(.,"bbs","html") 
    content <- get_content(link)
    
    
    tmp <- data.frame(標題 = title, 日期 = date, 作者 = author, 
                      推數 = nrec, 標記 = mark, 連結 = link, 內容 = content)
    ptt.GS <- rbind(ptt.GS, tmp)
    #Sys.sleep(runif(1,min = 2, max = 5)) #休息2~5秒
}

for (i in 1:length(ptt.GS)) {
    ptt.GS[,i] <- as.character(ptt.GS[,i])
}

ptt.GS[(ptt.GS$推數 %in% ""),"推數"] <- "0"
ptt.GS[grepl("爆",ptt.GS$推數),"推數"] <- "100"
ptt.GS[,"推數"] <- gsub(pattern ="X" ,replacement ="-" ,x = ptt.GS[,"推數"] )
ptt.GS$推數 <- as.numeric(ptt.GS$推數)

return(ptt.GS)  #data.frame
}

ptt.GS <- Gossip(28460,29460)













