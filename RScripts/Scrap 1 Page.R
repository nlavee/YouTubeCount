########### INSTALL NEEDED PACKAGES ##################################

#install.packages("RCurl")
#install.packages("XML")
library(RCurl)
library(XML)

######################################################################

########### PREP FOR DATAFRAME & MATCHING LETTERS ####################
trim <- function(x) gsub("^\\s+|\\s+$", "", x)
Dance.Complete<-NA

######################################################################

########### ACTUAL SCRAPPING DATA ####################################
url<-"https://www.youtube.com"
front_url<-"https://www.youtube.com/results?search_sort=video_view_count&search_query=%22world+of+dance%22&filters=video&page="
Dance <- NULL

#for(pages in 1:7) {
page_num<-1
site<-paste(front_url,page_num,sep="")
web.code<-getURLContent(site,ssl.verifypeer = FALSE)
web.html<-htmlTreeParse(web.code,asText=T,useInternalNodes=T)

title<-xpathSApply(web.html,"//a[contains(@title,'WOD')]",xmlValue)
title_link<-xpathSApply(web.html,"//a[contains(@title,'WOD')]/@href")
command <-"//a[@href='"
end <-"']/"
link_end <- paste(title_link,end,sep="")
additional <- "span[@class='video-time']"
link_end_1 <- paste(link_end, additional, sep="")
search_command <- paste(command,link_end_1, sep="")
search_command
duration <- xpathSApply(web.html, search_command, xmlValue)
Dance<-data.frame(Title=title,link=title_link, Duration=duration)

##extracting information on each video
###loop for each video
for (vid in 1:length(title)){
  view_url<-paste(url,title_link[[vid]],sep="")
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
#}