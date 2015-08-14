########### INSTALL NEEDED PACKAGES ##################################

#install.packages("RCurl")
#install.packages("XML")
#install.packages("ggplot2")

library(RCurl)
library(XML)

######################################################################


########### PREP FOR DATAFRAME & MATCHING LETTERS ####################
trim <- function(x) gsub("^\\s+|\\s+$", "", x)
Dance.Complete<-NA

######################################################################

########### ACTUAL SCRAPPING WITH DEFINED KEYSEARCH ##################
url<-"https://www.youtube.com"
front_url<-"https://www.youtube.com/results?search_sort=video_view_count&search_query=%22world+of+dance%22&filters=video&page="

#Dance <- data.frame(matrix(vector(), 300, 3, dimnames=list(c(), c("Title", "Title_Link", "Duration"))), stringsAsFactors=TRUE)
Dance <- data.frame(matrix(vector(), 1000, 3, dimnames=list(c(), c("Title", "Title_Link", "Duration"))), stringsAsFactors=TRUE) #There are roughly 202,000 videos in the result section

lastPage <- 1
title_link <- NULL

## looping through 30 pages of results
for(i in 1:30) { 
  page_num<-i
  site<-paste(front_url,page_num,sep="")
  web.code<-getURLContent(site,ssl.verifypeer = FALSE)
  web.html<-htmlTreeParse(web.code,asText=T,useInternalNodes=T)
  
  #web.html
  
  #get title
  title<-xpathSApply(web.html,"//a[contains(@title,'Dance')]", xmlValue)
  
  #get title link
  title_link<-xpathSApply(web.html,"//a[contains(@title,'Dance')]/@href")
  
  #get duration
  command <-"//a[@href='"
  end <-"']/"
  link_end <- paste(title_link,end,sep="")
  additional <- "span[@class='video-time']"
  link_end_1 <- paste(link_end, additional, sep="")
  search_command <- paste(command,link_end_1, sep="")
  duration <- xpathSApply(web.html, search_command, xmlValue)
  
  #looping through to put in the data
  for(count in 1:length(title_link)) {
    Dance$Title[lastPage] <- title[count]
    Dance$Title_Link[lastPage] <- title_link[count]
    Dance$Duration[lastPage] <- duration[count]
    lastPage <- lastPage+1
  }
}

## extracting information on each video

### loop for each video
eleNum <- lastPage - 1 #since lastPage +1 happens one more time than necessary due to the for loop above
for (vid in 1:eleNum){
  view_url<-paste(url,Dance$Title_Link[[vid]],sep="")
  view.code<-getURL(view_url,ssl.verifypeer=FALSE)
  view.html<-htmlTreeParse(view.code,asText=T,useInternalNodes=T)
  
  #unavailable.message<-xpathSApply(view.html,"//h1[@id='unavailable-message']",xmlValue)
  #view count
  view.count<-xpathSApply(view.html,"//*[@class='watch-view-count']",xmlValue)
  if(length(view.count)>0){
    Dance$view.count[vid]<-gsub("\\.","",view.count)
  }else{
    Dance$view.count[vid]<-NA
  }
  
  #Channel Name
  #channel.name<-xpathSApply(view.html,"//span[@class=' g-hovercard']",xmlValue)
  #channel.name
  #if(length(channel.name)>0) {
  #  Dance$channel.name[vid]<-gsub("\\.","",channel.name)
  #}else{
  #  Dance$channel.name[vid]<-NA
  #}
  
  #subscriber count
  subs.count<-xpathSApply(view.html,"//*[@class='yt-subscription-button-subscriber-count-branded-horizontal']",xmlValue)
  if(length(subs.count)>0) {
    Dance$subs.count[vid]<-gsub("\\.","",subs.count)
  }else{
    Dance$subs.count[vid]<-NA
  }
  
  #upload date
  upload.date<-xpathSApply(view.html,"//*[@class='watch-time-text']",xmlValue)
  if(length(upload.date)>0) {
    if(substring(upload.date,0,1)=="U") {
      Dance$upload.date[vid]<-gsub("Uploaded on ","",upload.date) 
    } else {
      Dance$upload.date[vid]<-gsub("Published on ","",upload.date) 
    }
  }else{
    Dance$upload.date[vid]<-NA
  }
  
  #likes count
  likes.count<-xpathSApply(view.html,"//*[@id='watch-like']",xmlValue)
  if(length(likes.count)>0){
    Dance$likes.count[vid]<-likes.count
  }else{
    Dance$likes.count[vid]<-NA
  }
  
  #dislikes count
  dislikes.count<-xpathSApply(view.html,"//*[@id='watch-dislike']",xmlValue)
  if(length(dislikes.count)>0){
    Dance$dislikes.count[vid]<-dislikes.count
  }else{
    Dance$dislikes.count[vid]<-NA
  }
  
  #description text count
  description<-xpathSApply(view.html,"//*[@id='eow-description']",xmlValue)
  if(length(description)>0) {
    Dance$description[vid]<-gsub("","",description)
  }else{
    Dance$description[vid]<-NA
  }
}

## Done