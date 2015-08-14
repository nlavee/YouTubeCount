#This is going to be where we clean/transform the data

# To do list:
# Title - 
# Duration - Change into numeric value
# view.count - Change into numeric value
# subs.count - Change into numeric value
# upload.date - Change into date type
# likes.count - Change into numeric value
# dislikes.count - Change into numeric value
# Description - A way to find keywords, a way to see super repetition, length of the text

#After tranform, put data into a new data frame for analysiss

attach(Dance)
head(Dance)

case <- 404 # We cut of at 404 now since it's the last cutoff point between 30k and 12k views. For future use, can increase this to get more results
Dance.Complete <- data.frame(matrix(vector(), case, 3, dimnames=list(c(), c("Title", "Title_Link", "Duration"))), stringsAsFactors=TRUE)

for (i in 1:case) {
  Dance.Complete$Title[i] <- Dance$Title[i] #cleaned titled
  Dance.Complete$Title_Link[i] <- Dance$Title_Link[i] #cleaned linked
  
  #cleaned duration
  duration <- Dance$Duration[i] 
  duration.split <- strsplit(duration,split=":", fixed = TRUE)
  min <- as.numeric(duration.split[[1]][1])
  sec <- as.numeric(duration.split[[1]][2])
  duration_actual <- min*60 + sec
  Dance.Complete$Duration[i] <- duration_actual
  
  #cleaned view counts
  view <- Dance$view.count[i]
  view.split <- NULL
  view.noSpace <- strsplit(view, split=" ", fixed=TRUE)
  
  if(length(view.noSpace[[1]]) == 2) {
    view.split <- view.noSpace[[1]][1]
  } else {
    view.split <- gsub(",","",view.noSpace[[1]][1])
  }
  
  Dance.Complete$View[i] <- as.numeric(view.split)
  
  #cleaned subscriber counts
  subs <- Dance$subs.count[i]
  subs.split <- strsplit(subs, split=",", fixed = TRUE)
  if(length(subs.split[[1]]) == 1) {
    subs.cleaned <- subs.split[[1]][1]
  } else {
    subs.cleaned <-gsub(",","",subs)
  }
  Dance.Complete$Sub[i] <- as.numeric(subs.cleaned)
  
  #cleaned likes counts
  likes <- Dance$likes.count[i]
  likes.split <-gsub(",","",likes)
  Dance.Complete$Likes[i] <- as.numeric(likes.split)
  
  #cleaned dislike counts
  dislikes <- Dance$dislikes.count[i]
  dislikes.split <-gsub(",","",dislikes)
  Dance.Complete$Dislikes[i] <- as.numeric(dislikes.split)
  
  #created likes/dislikes ratio
  
  
  #cleaned dates
  date <- Dance$upload.date[i]
  date.correct <- as.Date(date, "%b %d, %Y")
  Dance.Complete$Date[i] <- date.correct
  
  Dance.Complete$Description[i] <- Dance$description[i]
}

# We have some channel without any subscriber. However, if we think about it, each channel subscribes to itself.
# Therefore, we go in and add 1 to each sub count to take into account this. 
# Also, so that we can take log(Sub) without Sub = 0

Dance.Complete$Sub[is.na(Dance.Complete$Sub)] <- 0
for(i in 1:case) {
  Dance.Complete$Sub[i] <- Dance.Complete$Sub[i] + 1
}

Dance.Complete$Likes[which(!is.finite(Likes))] <- 0
Dance.Complete$Dislikes[which(!is.finite(Dislikes))] <- 0
