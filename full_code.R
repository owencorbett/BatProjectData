#####bat data compilation#####
df <- c()
for (i in dir(pattern='*.csv$', recursive = T)){
  u <- read.csv(i)
  u$sensor = factor(i)
  u$sensor <- gsub("^.*?t","",u$sensor)
  u$sensor <- gsub(".csv","",u$sensor)
  df <- rbind(df,u)
  cat(i, "\n")
}

batdatafull <- df

batdata$datetime <- as.POSIXct(as.numeric(as.character(batdata$timestamp))/1000,origin="1970-01-01",tz="GMT")
batdata$date <- format(batdata$datetime, "%Y-%m-%d")
batdata$time <- format(batdata$datetime, "%H:%M:%S")

batdata <- batdata[complete.cases(batdata), ]


batdata <- batdata[c(batdata$sensor=="1"|batdata$sensor=="2"|batdata$sensor=="5"|batdata$sensor=="6"|batdata$sensor=="7"|batdata$sensor=="8"|batdata$sensor=="9"|batdata$sensor=="10"|batdata$sensor=="11"|batdata$sensor=="12"|batdata$sensor=="13"|batdata$sensor=="14"),]

batdata <- batdata[batdata$p1>0.5,]

#genus column#
df <- c()
for(i in batdata$c1){
  if(i %in% c("Pipistrellus pipistrellus","Pipistrellus pygmaeus","Pipistrellus nathusii")){
    a <- "Pipistrellus"
    df <- rbind(df,a)
  } else if(i %in% c("Nyctalus noctula","Nyctalus leisleri")){
    a <- "Nyctalus"
    df <- rbind(df,a)
  } else if(i %in% c("Myotis brandtii","Myotis daubentonii","Myotis nattereri")){
    a <- "Myotis"
    df <- rbind(df,a)
  } else if(i %in% c("Plecotus auritus","Plecotus austriacus")){
    a <- "Plecotus"
    df <- rbind(df,a)
  }
}

batdata$genus <- as.factor(df)

###load sun data####
setwd("~/climatedata/olympicbats_visualizations-597a463668acf19f90ff434b0ecdce31765f3c86/sun_2")
df <- c()
file_list <- list.files()
for (i in file_list){
  u <- read.table(i, skip=1)
  colnames(u)<-c("day","sunrise","sunset","x","daylength")
  u$month = factor(i)
  u$month <- gsub(".txt","",u$month)
  u$date <- as.Date(with(u,paste("2017", month, day, sep="-")), "%Y-%m-%d")
  df <- rbind(df,u)
  cat(i, "\n")
}
###collate weather data from files####

setwd("~/climatedata/olympicbats_visualizations-597a463668acf19f90ff434b0ecdce31765f3c86/weather")
max <- c()
min <- c()
mean <- c()
pr <- c()
maxh <- c()
minh <- c()
meanh <- c()
maxw <- c()
meanw <- c()
file_list <- list.files()
for (i in file_list){
  u <- read_csv(i)
  a <- u[3,]
  a <- gsub(" °C","",a)
  a <- strsplit(as.character(a),"   ")
  a <- as.data.frame(a)
  b <- as.character(a[2,])
  c <- as.character(a[3,])
  d <- as.character(a[4,])
  max <- rbind(max,b)
  min <- rbind(min,c)
  mean <- rbind(mean,d)
  e <- u[6,]
  e <- gsub(" mm","",e)
  e <- strsplit(as.character(e),"   ")
  e <- as.data.frame(e)
  f <- as.character(e[2,])
  pr <- rbind(pr,f)
  g <- u[5,]
  g <- gsub("%","",g)
  g <- strsplit(as.character(g),"   ")
  g <- as.data.frame(g)
  h <- as.character(g[2,])
  i <- as.character(g[3,])
  j <- as.character(g[4,])
  maxh <- rbind(maxh,h)
  minh <- rbind(minh,i)
  meanh <- rbind(meanh,j)
  k <- u[8,]
  k <- gsub(" km/h","",k)
  k <- strsplit(as.character(k),"   ")
  k <- as.data.frame(k)
  l <- as.character(k[2,])
  m <- as.character(k[4,])
  maxw <- rbind(maxw,l)
  meanw <- rbind(meanw,m)
  cat(i, "\n")
}

weather <- as.data.frame(cbind(file_list,max,min,mean,pr,maxh,minh,meanh,maxw,meanw))
colnames(weather) <- c("date","max_temp","min_temp","mean_temp","precip_mm","max_hum","min_hum","mean_hum","max_wind","mean_wind")
weather$date <- gsub(".txt","2017",weather$date)
rownames(weather) <- c()

library(stringi)

dates <- c()
for (i in weather$date){
  a <- i
  stri_sub(a, 3, 2) <- "-"
  stri_sub(a, 6, 5) <- "-"
  dates <- rbind(dates,a)
}

dates <- format(as.Date(dates, "%m-%d-%Y"),"%Y-%m-%d")
weather$date <- dates


####compile occurrences#####

library(readr)

weather <- read_csv("~/weather.csv", col_types = cols(X1 = col_skip()))

sundata <- read_csv("~/sundata.csv", col_types = cols(date = col_date(format = "%d/%m/%Y")))

batdata <- read_csv("~/batdatanights.csv")

library(readxl)

stadiumevents <- read_excel("~/stadiumevents.xlsx")

sundata$daylength <- (sundata$sunset-sundata$sunrise)

moondata <- read_excel("~/moondata.xlsx", 
                       +     col_types = c("date", "numeric"))


#first#
batdata$date <- as.Date(batdata$date)
stadiumevents$events <- as.Date(stadiumevents$events)
sundata$date <- as.Date(sundata$date)
weather$date <- as.Date(weather$date)
moondata$date <- as.Date(moondata$date)

batdata <- batdata[complete.cases(batdata), ]


#then#

occurrences <- as.data.frame(table(unlist(batdata$date)))
colnames(occurrences) <- c("date","freq")

occurrences2<-as.data.frame(table(unlist(paste(batdata$date,batdata$sensor,sep=','))))
library(stringr)
a <- as.data.frame(str_split_fixed(occurrences2$Var1, ",", 2))
occurrences2 <- cbind(a,occurrences2$Freq)
colnames(occurrences2) <- c("date","sensor","freq")



occurrences3<-as.data.frame(table(unlist(paste(batdata$date,batdata$sensor, batdata$c1, sep=','))))

library(stringr)
a <- as.data.frame(str_split_fixed(occurrences3$Var1, ",", 3))
occurrences3 <- cbind(a,occurrences3$Freq)
colnames(occurrences3) <- c("date","sensor","species","freq")

occ_genus<-as.data.frame(table(unlist(paste(batdata$date,batdata$sensor, batdata$genus, sep=','))))

library(stringr)
a <- as.data.frame(str_split_fixed(occ_genus$Var1, ",", 3))
occ_genus <- cbind(a,occ_genus$Freq)
colnames(occ_genus) <- c("date","sensor","genus","freq")


occurrences4<-as.data.frame(table(unlist(paste(batdata$date, batdata$c1, sep=','))))

library(stringr)
a <- as.data.frame(str_split_fixed(occurrences4$Var1, ",", 2))
occurrences4 <- cbind(a,occurrences4$Freq)
colnames(occurrences4) <- c("date","species","freq")

########add zeros######

dateslist <- occurrences$date

sensor <- as.factor(c("1","2","5","6","7","8","9","10","11","12","13","14"))

df <- c()
for(i in dateslist){
  a <- c(as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i))
  b <- as.data.frame(a)
  b$sensor <- sensor
  df <- rbind(df,b)
}

full_list <- df
colnames(full_list) <- c("date","sensor")

occurrences2$x <- paste(occurrences2$date,occurrences2$sensor,sep=',')
full_list$x <- paste(full_list$date, full_list$sensor,sep=',')

df <- c()
df2 <- c()
for(i in full_list$x){
  if(i %in% occurrences2$x == TRUE){
    a <- i
    df <- rbind(df,a)
  }else{
    b <- i
    df2 <- rbind(df2,b)
    cat(i, "\n")
  }
}

a <- as.data.frame(str_split_fixed(df2, ",", 2))
colnames(a) <- c("date","sensor")
a$freq <- matrix(0, length(a$date), 1)
occurrences2zeros <- a
occurrences2$x <- NULL
occurrences2 <- rbind(occurrences2,occurrences2zeros)
occurrences2$date <- as.Date(occurrences2$date)
occurrences2 <- occurrences2[order(occurrences2$date),]
occurrences2 <- occurrences2[!(occurrences2$date =="2017-07-10"),]
occurrences2 <- occurrences2[!(occurrences2$date =="2017-07-11"),]

sensor2 <- as.factor(c("1","1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","5","5","5","5","5","5","5","5","5","5","6","6","6","6","6","6","6","6","6","6","7","7","7","7","7","7","7","7","7","7","8","8","8","8","8","8","8","8","8","8","9","9","9","9","9","9","9","9","9","9","10","10","10","10","10","10","10","10","10","10","11","11","11","11","11","11","11","11","11","11","12","12","12","12","12","12","12","12","12","12","13","13","13","13","13","13","13","13","13","13","14","14","14","14","14","14","14","14","14","14"))
species <- c("Myotis brandtii","Myotis daubentonii", "Myotis nattereri", "Nyctalus leisleri", "Nyctalus noctula", "Pipistrellus nathusii", "Pipistrellus pipistrellus", "Pipistrellus pygmaeus", "Plecotus auritus", "Plecotus austriacus")


df <- c()
for(i in dateslist){
  a <- c(as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i),as.Date(i))
  b <- as.data.frame(a)
  b$sensor <- sensor2
  b$species <- species
  df <- rbind(df,b)
}



full_list_sp <- df
colnames(full_list_sp) <- c("date","sensor","species")

occurrences3$x <- paste(occurrences3$date,occurrences3$sensor,occurrences3$species,sep=',')
full_list_sp$x <- paste(full_list_sp$date, full_list_sp$sensor,full_list_sp$species,sep=',')

df <- c()
df2 <- c()
for(i in full_list_sp$x){
  if(i %in% occurrences3$x == TRUE){
    a <- i
    df <- rbind(df,a)
  }else{
    b <- i
    df2 <- rbind(df2,b)
    cat(i, "\n")
  }
}

a <- as.data.frame(str_split_fixed(df2, ",", 3))
colnames(a) <- c("date","sensor","species")
a$freq <- matrix(0, length(a$date), 1)
occurrences3zeros <- a
occurrences3$x <-NULL
occurrences3 <- rbind(occurrences3,occurrences3zeros)
occurrences3$date <- as.Date(occurrences3$date)
occurrences3 <- occurrences3[order(occurrences3$date, occurrences3$sensor),]
occurrences3 <- occurrences3[!(occurrences3$date =="2017-07-10"),]
occurrences3 <- occurrences3[!(occurrences3$date =="2017-07-11"),]



sensor3 <- as.factor(c("1","1","1","1","2","2","2","2","5","5","5","5","6","6","6","6","7","7","7","7","8","8","8","8","9","9","9","9","10","10","10","10","11","11","11","11","12","12","12","12","13","13","13","13","14","14","14","14"))
genus <- c("Myotis", "Nyctalus", "Pipistrellus","Plecotus")


df <- c()
for(i in dateslist){
  a <- c(as.Date(i), as.Date(i), as.Date(i), as.Date(i),as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i), as.Date(i))
  b <- as.data.frame(a)
  b$sensor <- sensor3
  b$genus <- genus
  df <- rbind(df,b)
}


full_list_gn <- df
colnames(full_list_gn) <- c("date","sensor","genus")

occ_genus$x <- paste(occ_genus$date,occ_genus$sensor,occ_genus$genus,sep=',')
full_list_gn$x <- paste(full_list_gn$date, full_list_gn$sensor,full_list_gn$genus,sep=',')

df <- c()
df2 <- c()
for(i in full_list_gn$x){
  if(i %in% occ_genus$x == TRUE){
    a <- i
    df <- rbind(df,a)
  }else{
    b <- i
    df2 <- rbind(df2,b)
    cat(i, "\n")
  }
}

a <- as.data.frame(str_split_fixed(df2, ",", 3))
colnames(a) <- c("date","sensor","genus")
a$freq <- matrix(0, length(a$date), 1)
occ_genuszeros <- a
occ_genus$x <-NULL
occ_genus <- rbind(occ_genus,occ_genuszeros)
occ_genus$date <- as.Date(occ_genus$date)
occ_genus <- occ_genus[order(occ_genus$date, occ_genus$sensor),]
occ_genus <- occ_genus[!(occ_genus$date =="2017-07-10"),]
occ_genus <- occ_genus[!(occ_genus$date =="2017-07-11"),]


####logtransform###

occurrences$logfreq <- log(occurrences$freq+1)
occurrences2$logfreq <- log(occurrences2$freq+1)
occurrences3$logfreq <- log(occurrences3$freq+1)
occurrences4$logfreq <- log(occurrences4$freq+1)
occ_genus$logfreq <- log(occ_genus$freq+1)

####add events####
df <- c()
for(i in occurrences$date){
  if(i %in% stadiumevents$events == TRUE){
    u <- "1"
  }else{
    u <- "0"}
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences$event <- df

df <- c()
for(i in occurrences2$date){
  if(i %in% stadiumevents$events == TRUE){
    u <- "1"
  }else{
    u <- "0"}
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences2$event <- df

df <- c()
for(i in occurrences3$date){
  if(i %in% stadiumevents$events == TRUE){
    u <- "1"
  }else{
    u <- "0"}
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences3$event <- df



df <- c()
for(i in occurrences4$date){
  if(i %in% stadiumevents$events == TRUE){
    u <- "1"
  }else{
    u <- "0"}
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences4$event <- df

df <- c()
for(i in occ_genus$date){
  if(i %in% stadiumevents$events == TRUE){
    u <- "1"
  }else{
    u <- "0"}
  df <- rbind(df,u)
  cat(i, "\n")
}
occ_genus$event <- df

########add daylength###

df <- c()
for(i in occurrences$date){
  u <- sundata[which(i == sundata$date),c(1:4)]
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences$daylength <- df$daylength

df <- c()
for(i in occurrences2$date){
  u <- sundata[which(i == sundata$date),c(1:4)]
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences2$daylength <- df$daylength

df <- c()
for(i in occurrences3$date){
  u <- sundata[which(i == sundata$date),c(1:4)]
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences3$daylength <- df$daylength

df <- c()
for(i in occurrences4$date){
  u <- sundata[which(i == sundata$date),c(1:4)]
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences4$daylength <- df$daylength

df <- c()
for(i in occ_genus$date){
  u <- sundata[which(i == sundata$date),c(1:4)]
  df <- rbind(df,u)
  cat(i, "\n")
}
occ_genus$daylength <- df$daylength

###########add moon illumination########

df <- c()
for(i in occurrences$date){
  u <- moondata[which(i == moondata$date),c(1:2)]
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences$moon_illum <- df$illumination

df <- c()
for(i in occurrences2$date){
  u <- moondata[which(i == moondata$date),c(1:2)]
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences2$moon_illum <- df$illumination

df <- c()
for(i in occurrences3$date){
  u <- moondata[which(i == moondata$date),c(1:2)]
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences3$moon_illum <- df$illumination

df <- c()
for(i in occurrences4$date){
  u <- moondata[which(i == moondata$date),c(1:2)]
  df <- rbind(df,u)
  cat(i, "\n")
}
occurrences4$moon_illum <- df$illumination

df <- c()
for(i in occ_genus$date){
  u <- moondata[which(i == moondata$date),c(1:2)]
  df <- rbind(df,u)
  cat(i, "\n")
}
occ_genus$moon_illum <- df$illumination

#######add weather####

df <- c()
for(i in occurrences$date){
  u <- weather[which(i == weather$date),c(1:10)]
  df <- rbind(df,u)
  cat(i, "\n")
}

occurrences$max_temp <- df$max_temp
occurrences$min_temp <- df$min_temp
occurrences$mean_temp <- df$mean_temp
occurrences$precip_mm <- df$precip_mm
occurrences$max_hum <- df$max_hum
occurrences$min_hum <- df$min_hum
occurrences$mean_hum <- df$mean_hum
occurrences$max_wind <- df$max_wind
occurrences$mean_wind <- df$mean_wind

df <- c()
for(i in occurrences2$date){
  u <- weather[which(i == weather$date),c(1:10)]
  df <- rbind(df,u)
  cat(i, "\n")
}

occurrences2$max_temp <- df$max_temp
occurrences2$min_temp <- df$min_temp
occurrences2$mean_temp <- df$mean_temp
occurrences2$precip_mm <- df$precip_mm
occurrences2$max_hum <- df$max_hum
occurrences2$min_hum <- df$min_hum
occurrences2$mean_hum <- df$mean_hum
occurrences2$max_wind <- df$max_wind
occurrences2$mean_wind <- df$mean_wind

df <- c()
for(i in occurrences3$date){
  u <- weather[which(i == weather$date),c(1:10)]
  df <- rbind(df,u)
  cat(i, "\n")
}

occurrences3$max_temp <- df$max_temp
occurrences3$min_temp <- df$min_temp
occurrences3$mean_temp <- df$mean_temp
occurrences3$precip_mm <- df$precip_mm
occurrences3$max_hum <- df$max_hum
occurrences3$min_hum <- df$min_hum
occurrences3$mean_hum <- df$mean_hum
occurrences3$max_wind <- df$max_wind
occurrences3$mean_wind <- df$mean_wind

df <- c()
for(i in occurrences4$date){
  u <- weather[which(i == weather$date),c(1:10)]
  df <- rbind(df,u)
  cat(i, "\n")
}

occurrences4$max_temp <- df$max_temp
occurrences4$min_temp <- df$min_temp
occurrences4$mean_temp <- df$mean_temp
occurrences4$precip_mm <- df$precip_mm
occurrences4$max_hum <- df$max_hum
occurrences4$min_hum <- df$min_hum
occurrences4$mean_hum <- df$mean_hum
occurrences4$max_wind <- df$max_wind
occurrences4$mean_wind <- df$mean_wind

df <- c()
for(i in occ_genus$date){
  u <- weather[which(i == weather$date),c(1:10)]
  df <- rbind(df,u)
  cat(i, "\n")
}

occ_genus$max_temp_rescale <- scale(df$max_temp)
occ_genus$min_temp_rescale <- scale(df$min_temp)
occ_genus$mean_temp_rescale <- scale(df$mean_temp)
occ_genus$precip_mm_rescale <- scale(df$precip_mm)
occ_genus$max_hum_rescale <- scale(df$max_hum)
occ_genus$min_hum_rescale <- scale(df$min_hum)
occ_genus$mean_hum_rescale <- scale(df$mean_hum)
occ_genus$max_wind_rescale <- scale(df$max_wind)
occ_genus$mean_wind_rescale <- scale(df$mean_wind)
occ_genus$daylength_rescale <- scale(occ_genus$daylength)

#####

occurrences4$max_temp_rescale <- scale(occurrences4$max_temp, center=FALSE)
occurrences4$min_temp_rescale <- scale(occurrences4$min_temp, center=FALSE)
occurrences4$mean_temp_rescale <- scale(occurrences4$mean_temp, center=FALSE)
occurrences4$precip_mm_rescale <- scale(occurrences4$precip_mm, center=FALSE)
occurrences4$max_hum_rescale <- scale(occurrences4$max_hum, center=FALSE)
occurrences4$min_hum_rescale <- scale(occurrences4$min_hum, center=FALSE)
occurrences4$mean_hum_rescale <- scale(occurrences4$mean_hum, center=FALSE)
occurrences4$max_wind_rescale <- scale(occurrences4$max_wind, center=FALSE)
occurrences4$mean_wind_rescale <- scale(occurrences4$mean_wind, center=FALSE)
occurrences4$daylength_rescale <- scale(occurrences4$daylength, center=FALSE)


occurrences$max_temp_rescale <- scale(occurrences$max_temp, center=FALSE)
occurrences$min_temp_rescale <- scale(occurrences$min_temp, center=FALSE)
occurrences$mean_temp_rescale <- scale(occurrences$mean_temp, center=FALSE)
occurrences$precip_mm_rescale <- scale(occurrences$precip_mm, center=FALSE)
occurrences$max_hum_rescale <- scale(occurrences$max_hum, center=FALSE)
occurrences$min_hum_rescale <- scale(occurrences$min_hum, center=FALSE)
occurrences$mean_hum_rescale <- scale(occurrences$mean_hum, center=FALSE)
occurrences$max_wind_rescale <- scale(occurrences$max_wind, center=FALSE)
occurrences$mean_wind_rescale <- scale(occurrences$mean_wind, center=FALSE)
occurrences$daylength_rescale <- scale(occurrences$daylength, center=FALSE)

occurrences2$max_temp_rescale <- scale(occurrences2$max_temp)
occurrences2$min_temp_rescale <- scale(occurrences2$min_temp)
occurrences2$mean_temp_rescale <- scale(occurrences2$mean_temp)
occurrences2$precip_mm_rescale <- scale(occurrences2$precip_mm)
occurrences2$max_hum_rescale <- scale(occurrences2$max_hum)
occurrences2$min_hum_rescale <- scale(occurrences2$min_hum)
occurrences2$mean_hum_rescale <- scale(occurrences2$mean_hum)
occurrences2$max_wind_rescale <- scale(occurrences2$max_wind)
occurrences2$mean_wind_rescale <- scale(occurrences2$mean_wind)
occurrences2$daylength_rescale <- scale(occurrences2$daylength)

occurrences3$max_temp_rescale <- scale(occurrences3$max_temp)
occurrences3$min_temp_rescale <- scale(occurrences3$min_temp)
occurrences3$mean_temp_rescale <- scale(occurrences3$mean_temp)
occurrences3$precip_mm_rescale <- scale(occurrences3$precip_mm)
occurrences3$max_hum_rescale <- scale(occurrences3$max_hum)
occurrences3$min_hum_rescale <- scale(occurrences3$min_hum)
occurrences3$mean_hum_rescale <- scale(occurrences3$mean_hum)
occurrences3$max_wind_rescale <- scale(occurrences3$max_wind)
occurrences3$mean_wind_rescale <- scale(occurrences3$mean_wind)
occurrences3$daylength_rescale <- scale(occurrences3$daylength)

######### models######
###all bats whole park###
model_all_sp <- lmer(data=occurrences3, logfreq ~ precip_mm_rescale + mean_hum_rescale + mean_wind_rescale + moon_illum + event + (1|sensor) + (1|species))


all_NORM_2 <- lmer(data=occurrences3_no7, 
                   logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                     mean_wind_rescale + moon_illum + event + (1|sensor) + (1|species))

all_P_2 <- glmer(data=occurrences3_no7, 
                 logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                   mean_wind_rescale + moon_illum + event + (1|sensor) + (1|species), family='poisson')

all_ZI_2 <- glmmadmb(data=occurrences3_no7, 
                     logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                       mean_wind_rescale + moon_illum + event + (1|sensor) + (1|species), zeroInflation=TRUE)

all_ZI_F_2 <- glmmadmb(data=occurrences3_no7, 
                       logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                         mean_wind_rescale + moon_illum + event + (1|sensor) + (1|species), zeroInflation=FALSE)

all_P_GLMMADMB_2 <- glmmadmb(data=occurrences3_no7, 
                             freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                               mean_wind_rescale + moon_illum + event + (1|sensor) + (1|species), zeroInflation=FALSE, family='poisson')

all_AICtab <- AICtab(all_NORM, all_ZI, all_ZI_F, all_P, all_P_GLMMADMB)
all_AICtab

all_ZI_2_dredge <- dredge(all_ZI_2)
all_ZI_2_top <- subset(all_ZI_2_dredge, delta<2)
all_ZI_2_avg <- model.avg(all_ZI_2_top)
summary(all_ZI_2_avg)

#### Pipistrellus whole park#####

Pip <- occ_genus[c(occ_genus$genus=="Pipistrellus"),]
Nyc <- occ_genus[c(occ_genus$genus=="Nyctalus"),]
Ple <- occ_genus[c(occ_genus$genus=="Plecotus"),]
Myo <- occ_genus[c(occ_genus$genus=="Myotis"),]

Pip_no7 <- Pip[c(Pip$sensor=="1"|Pip$sensor=="2"|Pip$sensor=="5"|Pip$sensor=="6"|Pip$sensor=="8"|Pip$sensor=="9"|Pip$sensor=="10"|Pip$sensor=="11"|Pip$sensor=="12"|Pip$sensor=="13"|Pip$sensor=="14"),]


Pip_NORM <- lmer(data=Pip_no7, 
                 logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                   mean_wind_rescale + moon_illum + event + (1|sensor))

Pip_P <- glmer(data=Pip_no7, 
               logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                 mean_wind_rescale + moon_illum + event + (1|sensor), family='poisson')

Pip_ZI <- glmmadmb(data=Pip_no7, 
                   logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                     mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=TRUE)

Pip_ZI_F <- glmmadmb(data=Pip_no7, 
                     logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                       mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE)

Pip_P_GLMMADMB <- glmmadmb(data=Pip_no7, 
                           freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                             mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE, family='poisson')


Pip_AICtab <- AICtab(Pip_NORM, Pip_ZI, Pip_ZI_F, Pip_P, Pip_P_GLMMADMB)
Pip_AICtab

Pip_ZI_dredge <- dredge(Pip_ZI)
Pip_ZI_top <- subset(Pip_ZI_dredge, delta<2)
Pip_ZI_avg <- model.avg(Pip_ZI_top)
summary(Pip_ZI_avg)

####Nyctalus whole park######

Nyc_no7 <- Nyc[c(Nyc$sensor=="1"|Nyc$sensor=="2"|Nyc$sensor=="5"|Nyc$sensor=="6"|Nyc$sensor=="8"|Nyc$sensor=="9"|Nyc$sensor=="10"|Nyc$sensor=="11"|Nyc$sensor=="12"|Nyc$sensor=="13"|Nyc$sensor=="14"),]

Nyc_NORM <- lmer(data=Nyc_no7, 
                 logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                   mean_wind_rescale + moon_illum + event + (1|sensor))

Nyc_P <- glmer(data=Nyc_no7, 
               logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                 mean_wind_rescale + moon_illum + event + (1|sensor), family='poisson')

Nyc_ZI <- glmmadmb(data=Nyc_no7, 
                   logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                     mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=TRUE)

Nyc_ZI_F <- glmmadmb(data=Nyc_no7, 
                     logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                       mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE)

Nyc_P_GLMMADMB <- glmmadmb(data=Nyc_no7, 
                           freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                             mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE, family='poisson')

Nyc_AICtab <- AICtab(Nyc_NORM, Nyc_ZI, Nyc_ZI_F,Nyc_P,Nyc_P_GLMMADMB)
Nyc_AICtab

Nyc_ZI_F_dredge <- dredge(Nyc_ZI_F)
Nyc_ZI_F_top <- subset(Nyc_ZI_F_dredge, delta<2)
Nyc_ZI_F_avg <- model.avg(Nyc_ZI_F_top)
summary(Nyc_ZI_F_avg)

### P.pip whole park####
Pip_pip <- occurrences3[c(occurrences3$species=="Pipistrellus pipistrellus"),]
Pip_pyg <- occurrences3[c(occurrences3$species=="Pipistrellus pygmaeus"),]
Pip_nat <- occurrences3[c(occurrences3$species=="Pipistrellus nathusii"),]

Pip_pip_no7 <- Pip_pip[c(Pip_pip$sensor=="1"|Pip_pip$sensor=="2"|Pip_pip$sensor=="5"|Pip_pip$sensor=="6"|Pip_pip$sensor=="8"|Pip_pip$sensor=="9"|Pip_pip$sensor=="10"|Pip_pip$sensor=="11"|Pip_pip$sensor=="12"|Pip_pip$sensor=="13"|Pip_pip$sensor=="14"),]
Pip_pyg_no7 <- Pip_pyg[c(Pip_pyg$sensor=="1"|Pip_pyg$sensor=="2"|Pip_pyg$sensor=="5"|Pip_pyg$sensor=="6"|Pip_pyg$sensor=="8"|Pip_pyg$sensor=="9"|Pip_pyg$sensor=="10"|Pip_pyg$sensor=="11"|Pip_pyg$sensor=="12"|Pip_pyg$sensor=="13"|Pip_pyg$sensor=="14"),]
Pip_nat_no7 <- Pip_nat[c(Pip_nat$sensor=="1"|Pip_nat$sensor=="2"|Pip_nat$sensor=="5"|Pip_nat$sensor=="6"|Pip_nat$sensor=="8"|Pip_nat$sensor=="9"|Pip_nat$sensor=="10"|Pip_nat$sensor=="11"|Pip_nat$sensor=="12"|Pip_nat$sensor=="13"|Pip_nat$sensor=="14"),]

Pip_pip_NORM <- lmer(data=Pip_pip_no7, 
                     logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                       mean_wind_rescale + moon_illum + event + (1|sensor))

Pip_pip_P <- glmer(data=Pip_pip_no7, 
                   logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                     mean_wind_rescale + moon_illum + event + (1|sensor), family='poisson')

Pip_pip_ZI <- glmmadmb(data=Pip_pip_no7, 
                       logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                         mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=TRUE)

Pip_pip_ZI_F <- glmmadmb(data=Pip_pip_no7, 
                         logfreq ~ precip_mm_rescale + mean_hum_rescale + mean_temp_rescale +
                           mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE)

Pip_pip_P_GLMMADMB <- glmmadmb(data=Pip_pip_no7, 
                               freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                 mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE, family='poisson')


Pip_pip_AICtab <- AICtab(Pip_pip_NORM, Pip_pip_ZI, Pip_pip_ZI_F, Pip_pip_P, Pip_pip_P_GLMMADMB)
Pip_pip_AICtab

Pip_pip_ZI_dredge <- dredge(Pip_pip_ZI)
Pip_pip_ZI_top <- subset(Pip_pip_ZI_dredge, delta<2)
Pip_pip_ZI_avg <- model.avg(Pip_pip_ZI_top)
summary(Pip_pip_ZI_avg)

####Pip pyg whole park###

Pip_pyg_NORM <- lmer(data=Pip_pyg, 
                     logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                       mean_wind_rescale + moon_illum + event + (1|sensor))

Pip_pyg_P <- glmer(data=Pip_pyg, 
                   logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                     mean_wind_rescale + moon_illum + event + (1|sensor), family='poisson')

Pip_pyg_ZI <- glmmadmb(data=Pip_pyg, 
                       logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                         mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=TRUE)

Pip_pyg_ZI_F <- glmmadmb(data=Pip_pyg, 
                         logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                           mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE)

Pip_pyg_P_GLMMADMB <- glmmadmb(data=Pip_pyg, 
                               freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                 mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE, family='poisson')

Pip_pyg_AICtab <- AICtab(Pip_pyg_NORM, Pip_pyg_P, Pip_pyg_ZI, Pip_pyg_ZI_F, Pip_pyg_P)

Pip_pyg_ZI_dredge <- dredge(Pip_pyg_ZI)
Pip_pyg_ZI_top <- subset(Pip_pyg_ZI_dredge, delta<2)
Pip_pyg_ZI_avg <- model.avg(Pip_pyg_ZI_top)
summary(Pip_pyg_ZI_avg)

#####Pip nathusii whole park#####

Pip_nat_NORM <- lmer(data=Pip_nat, 
                     logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                       mean_wind_rescale + moon_illum + event + (1|sensor))

Pip_nat_P <- glmer(data=Pip_nat, 
                   logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                     mean_wind_rescale + moon_illum + event + (1|sensor), family='poisson')

Pip_nat_ZI <- glmmadmb(data=Pip_nat, 
                       logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                         mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=TRUE)

Pip_nat_ZI_F <- glmmadmb(data=Pip_nat, 
                         logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                           mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE)

Pip_nat_P_GLMMADMB <- glmmadmb(data=Pip_nat, 
                               freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                 mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE, family='poisson')

Pip_nat_AICtab <- AICtab(Pip_nat_NORM, Pip_nat_ZI, Pip_nat_ZI_F, Pip_nat_P, Pip_nat_P_GLMMADMB)

Pip_nat_ZI_dredge <- dredge(Pip_nat_ZI)
Pip_nat_ZI_top <- subset(Pip_nat_ZI_dredge, delta<2)
Pip_nat_ZI_avg <- model.avg(Pip_nat_ZI_top)
summary(Pip_nat_ZI_avg)

#############S7 models###########

###Pipistrellus s7####

Pip_s7 <- Pip[Pip$sensor=="7",]

Pip_s7_NORM <- lm(data=Pip_s7, 
                  logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                    mean_wind_rescale + moon_illum + event)

Pip_s7_P <- glm(data=Pip_s7, 
                logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                  mean_wind_rescale + moon_illum + event, family='poisson')

Pip_s7_ZI <- glmmadmb(data=Pip_s7, 
                      logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                        mean_wind_rescale + moon_illum + event, zeroInflation=TRUE)

Pip_s7_ZI_F <- glmmadmb(data=Pip_s7, 
                        logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                          mean_wind_rescale + moon_illum + event, zeroInflation=FALSE)

Pip_s7_P_GLMMADMB <- glmmadmb(data=Pip_s7, 
                              freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                mean_wind_rescale + moon_illum + event, zeroInflation=FALSE, family='poisson')

Pip_s7_AICtab <- AICtab(Pip_s7_NORM, Pip_s7_ZI, Pip_s7_ZI_F, Pip_s7_P, Pip_s7_P_GLMMADMB)
Pip_s7_AICtab

Pip_s7_ZI_dredge <- dredge(Pip_s7_ZI)
Pip_s7_ZI_top <- subset(Pip_s7_ZI_dredge, delta<2)
Pip_s7_ZI_avg <- model.avg(Pip_s7_ZI_top)
summary(Pip_s7_ZI_avg)


######Nyctalus s7####

Nyc <- occ_genus[c(occ_genus$genus=="Nyctalus"),]
Nyc_no7 <- Nyc[c(Nyc$sensor=="1"|Nyc$sensor=="2"|Nyc$sensor=="5"|Nyc$sensor=="6"|Nyc$sensor=="8"|Nyc$sensor=="9"|Nyc$sensor=="10"|Nyc$sensor=="11"|Nyc$sensor=="12"|Nyc$sensor=="13"|Nyc$sensor=="14"),]

Nyc_NORM <- lmer(data=Nyc_no7, 
                 logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                   mean_wind_rescale + moon_illum + event + (1|sensor))

Nyc_P <- glmer(data=Nyc_no7, 
               logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                 mean_wind_rescale + moon_illum + event + (1|sensor), family='poisson')

Nyc_ZI <- glmmadmb(data=Nyc_no7, 
                   logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                     mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=TRUE)

Nyc_ZI_F <- glmmadmb(data=Nyc_no7, 
                     logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                       mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE)

Nyc_P_GLMMADMB <- glmmadmb(data=Nyc_no7, 
                           freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                             mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE, family='poisson')

Nyc_AICtab <- AICtab(Nyc_NORM, Nyc_ZI, Nyc_ZI_F,Nyc_P,Nyc_P_GLMMADMB)
Nyc_AICtab

Nyc_ZI_F_dredge <- dredge(Nyc_ZI_F)
Nyc_ZI_F_top <- subset(Nyc_ZI_F_dredge, delta<2)
Nyc_ZI_F_avg <- model.avg(Nyc_ZI_F_top)
summary(Nyc_ZI_F_avg)

########stadium models########

all_s1256 <- occurrences3[c(occurrences3$sensor=="1"|occurrences3$sensor=="2"|occurrences3$sensor=="5"|occurrences3$sensor=="6"),]


all_s1256_NORM <- lmer(data=all_s1256, 
                       logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                         mean_wind_rescale + moon_illum + event + (1|sensor) + (1|species))

all_s1256_P <- glmer(data=all_s1256, 
                     logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                       mean_wind_rescale + moon_illum + event + (1|sensor) + (1|species), family='poisson')

all_s1256_ZI <- glmmadmb(data=all_s1256, 
                         logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                           mean_wind_rescale + moon_illum + event + (1|sensor) + (1|species), zeroInflation=TRUE)

all_s1256_ZI_F <- glmmadmb(data=all_s1256, 
                           logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                             mean_wind_rescale + moon_illum + event + (1|sensor) + (1|species), zeroInflation=FALSE)

all_s1256_P_GLMMADMB <- glmmadmb(data=all_s1256, 
                                 freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                   mean_wind_rescale + moon_illum + event + (1|sensor) + (1|species), zeroInflation=FALSE, family='poisson')

all_s1256_AICtab <- AICtab(all_s1256_NORM, all_s1256_ZI, all_s1256_ZI_F, all_s1256_P, all_s1256_P_GLMMADMB)
all_s1256_AICtab

all_s1256_ZI_dredge <- dredge(all_s1256_ZI)
all_s1256_ZI_top <- subset(all_s1256_ZI_dredge, delta<2)
all_s1256_ZI_avg <- model.avg(all_s1256_ZI_top)
summary(all_s1256_ZI_avg)

########Pipistrellus stadium######

Pip_s1256 <- Pip[c(Pip$sensor=="1"|Pip$sensor=="2"|Pip$sensor=="5"|Pip$sensor=="6"),]



Pip_s1256_NORM <- lmer(data=Pip_s1256, 
                       logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                         mean_wind_rescale + moon_illum + event + (1|sensor))

Pip_s1256_P <- glmer(data=Pip_s1256, 
                     logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                       mean_wind_rescale + moon_illum + event + (1|sensor), family='poisson')

Pip_s1256_ZI <- glmmadmb(data=Pip_s1256, 
                         logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                           mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=TRUE)

Pip_s1256_ZI_F <- glmmadmb(data=Pip_s1256, 
                           logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                             mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE)

Pip_s1256_P_GLMMADMB <- glmmadmb(data=Pip_s1256, 
                                 freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                   mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE, family='poisson')

Pip_s1256_AICtab <- AICtab(Pip_s1256_NORM, Pip_s1256_ZI, Pip_s1256_ZI_F, Pip_s1256_P, Pip_s1256_P_GLMMADMB)
Pip_s1256_AICtab

Pip_s1256_ZI_dredge <- dredge(Pip_s1256_ZI)
Pip_s1256_ZI_top <- subset(Pip_s1256_ZI_dredge, delta<2)
Pip_s1256_ZI_avg <- model.avg(Pip_s1256_ZI_top)
summary(Pip_s1256_ZI_avg)

##############Nyctalus stadium#########

Nyc_s1256 <- Nyc[c(Nyc$sensor=="1"|Nyc$sensor=="2"|Nyc$sensor=="5"|Nyc$sensor=="6"),]


Nyc_s1256_NORM <- lmer(data=Nyc_s1256, 
                       logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                         mean_wind_rescale + moon_illum + event + (1|sensor))

Nyc_s1256_P <- glmer(data=Nyc_s1256, 
                     logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                       mean_wind_rescale + moon_illum + event + (1|sensor), family='poisson')

Nyc_s1256_ZI <- glmmadmb(data=Nyc_s1256, 
                         logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                           mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=TRUE)

Nyc_s1256_ZI_F <- glmmadmb(data=Nyc_s1256, 
                           logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                             mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE)

Nyc_s1256_P_GLMMADMB <- glmmadmb(data=Nyc_s1256, 
                                 freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                   mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE, family='poisson')

Nyc_s1256_AICtab <- AICtab(Nyc_s1256_NORM, Nyc_s1256_ZI_F, Nyc_s1256_P, Nyc_s1256_P_GLMMADMB)

Nyc_s1256_AICtab

Nyc_s1256_ZI_F_dredge <- dredge(Nyc_s1256_ZI_F)
Nyc_s1256_ZI_F_top <- subset(Nyc_s1256_ZI_F_dredge, delta<2)
Nyc_s1256_ZI_F_avg <- model.avg(Nyc_s1256_ZI_F_top)
summary(Nyc_s1256_ZI_F_avg)

###########Pip species stadium######

Pip_pip_s1256 <- Pip_pip[c(Pip_pip$sensor=="1"|Pip_pip$sensor=="2"|Pip_pip$sensor=="5"|Pip_pip$sensor=="6"),]


Pip_pip_s1256_NORM <- lmer(data=Pip_pip_s1256, 
                           logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                             mean_wind_rescale + moon_illum + event + (1|sensor))

Pip_pip_s1256_P <- glmer(data=Pip_pip_s1256, 
                         logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                           mean_wind_rescale + moon_illum + event + (1|sensor), family='poisson')

Pip_pip_s1256_ZI <- glmmadmb(data=Pip_pip_s1256, 
                             logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                               mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=TRUE)

Pip_pip_s1256_ZI_F <- glmmadmb(data=Pip_pip_s1256, 
                               logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                 mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE)

Pip_pip_s1256_P_GLMMADMB <- glmmadmb(data=Pip_pip_s1256, 
                                     freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                       mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE, family='poisson')

Pip_pip_s1256_AICtab <- AICtab(Pip_pip_s1256_NORM, Pip_pip_s1256_ZI, Pip_pip_s1256_ZI_F, Pip_pip_s1256_P)



Pip_pyg_s1256 <- Pip_pyg[c(Pip_pyg$sensor=="1"|Pip_pyg$sensor=="2"|Pip_pyg$sensor=="5"|Pip_pyg$sensor=="6"),]

Pip_pyg_s1256_NORM <- lmer(data=Pip_pyg_s1256, 
                           logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                             mean_wind_rescale + moon_illum + event + (1|sensor))

Pip_pyg_s1256_P <- glmer(data=Pip_pyg_s1256, 
                         logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                           mean_wind_rescale + moon_illum + event + (1|sensor), family='poisson')

Pip_pyg_s1256_ZI <- glmmadmb(data=Pip_pyg_s1256, 
                             logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                               mean_wind_rescale +  moon_illum + event + (1|sensor), zeroInflation=TRUE)

Pip_pyg_s1256_ZI_F <- glmmadmb(data=Pip_pyg_s1256, 
                               logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                 mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE)

Pip_pyg_s1256_P_GLMMADMB <- glmmadmb(data=Pip_pyg_s1256, 
                                     freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                       mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE, family='poisson')

Pip_pyg_s1256_AICtab <- AICtab(Pip_pyg_s1256_NORM, Pip_pyg_s1256_ZI, Pip_pyg_s1256_ZI_F, Pip_pyg_s1256_P, Pip_pyg_s1256_P_GLMMADMB)

Pip_nat_s1256 <- Pip_nat[c(Pip_nat$sensor=="1"|Pip_nat$sensor=="2"|Pip_nat$sensor=="5"|Pip_nat$sensor=="6"),]

Pip_nat_s1256_NORM <- lmer(data=Pip_nat_s1256, 
                           logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                             mean_wind_rescale + moon_illum + event + (1|sensor))

Pip_nat_s1256_P <- glmer(data=Pip_nat_s1256, 
                         logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                           mean_wind_rescale + moon_illum + event + (1|sensor), family='poisson')

Pip_nat_s1256_ZI <- glmmadmb(data=Pip_nat_s1256, 
                             logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                               mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=TRUE)

Pip_nat_s1256_ZI_F <- glmmadmb(data=Pip_nat_s1256, 
                               logfreq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                 mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE)

Pip_nat_s1256_P_GLMMADMB <- glmmadmb(data=Pip_nat_s1256, 
                                     freq ~ mean_temp_rescale + precip_mm_rescale + mean_hum_rescale + 
                                       mean_wind_rescale + moon_illum + event + (1|sensor), zeroInflation=FALSE, family='poisson')

Pip_nat_s1256_AICtab <- AICtab(Pip_nat_s1256_NORM, Pip_nat_s1256_ZI, Pip_nat_s1256_ZI_F, Pip_nat_s1256_P, Pip_nat_s1256_P_GLMMADMB)

Pip_pip_s1256_AICtab
Pip_pyg_s1256_AICtab
Pip_nat_s1256_AICtab

Pip_pip_s1256_ZI_dredge <- dredge(Pip_pip_s1256_ZI)
Pip_pip_s1256_ZI_top <- subset(Pip_pip_s1256_ZI_dredge, delta<2)
Pip_pip_s1256_ZI_avg <- model.avg(Pip_pip_s1256_ZI_top)
summary(Pip_pip_s1256_ZI_avg)

Pip_pyg_s1256_ZI_dredge <- dredge(Pip_pyg_s1256_ZI)
Pip_pyg_s1256_ZI_top <- subset(Pip_pyg_s1256_ZI_dredge, delta<2)
Pip_pyg_s1256_ZI_avg <- model.avg(Pip_pyg_s1256_ZI_top)
summary(Pip_pyg_s1256_ZI_avg)

Pip_nat_s1256_ZI_dredge <- dredge(Pip_nat_s1256_ZI)
Pip_nat_s1256_ZI_top <- subset(Pip_nat_s1256_ZI_dredge, delta<2)
Pip_nat_s1256_ZI_avg <- model.avg(Pip_nat_s1256_ZI_top)
summary(Pip_nat_s1256_ZI_avg)

##########plots etc############

Pip_dailyavg <- ddply(Pip, .(date), summarize, freq = sum(freq))
Pip_sensoravg <- ddply(Pip, .(sensor), summarize, freq = mean(freq))

sum(Pip_dailyavg$freq)

mean(Pip_dailyavg$freq)

sqrt(var(Pip_dailyavg$freq))


Nyc_dailyavg <- ddply(Nyc, .(date), summarize, freq = sum(freq))
Nyc_sensoravg <- ddply(Nyc, .(sensor), summarize, freq = mean(freq))

sum(Nyc_dailyavg$freq)

mean(Nyc_dailyavg$freq)

sqrt(var(Nyc_dailyavg$freq))


Ple_dailyavg <- ddply(Ple, .(date), summarize, freq = sum(freq))
Ple_sensoravg <- ddply(Ple, .(sensor), summarize, freq = mean(freq))

sum(Ple_dailyavg$freq)

mean(Ple_dailyavg$freq)

sqrt(var(Ple_dailyavg$freq))


Myo_dailyavg <- ddply(Myo, .(date), summarize, freq = sum(freq))
Myo_sensoravg <- ddply(Myo, .(sensor), summarize, freq = mean(freq))

sum(Myo_dailyavg$freq)

mean(Myo_dailyavg$freq)

sqrt(var(Myo_dailyavg$freq))


library(dplyr)

library(ggplot2)

library(chron)

library(lubridate)

library(gridExtra)

grid.arrange()

occ_genus_no7 <- occ_genus[c(occ_genus$sensor=="1"|occ_genus$sensor=="2"|occ_genus$sensor=="5"|occ_genus$sensor=="6"|occ_genus$sensor=="8"|occ_genus$sensor=="9"|occ_genus$sensor=="10"|occ_genus$sensor=="11"|occ_genus$sensor=="12"|occ_genus$sensor=="13"|occ_genus$sensor=="14"),]

occ_genus_no7_nosensors <- occ_genus_no7 %>% group_by(date, genus) %>% summarize(freq = sum(freq))

occ_genus_no7_nosensors$logfreq <- log(occ_genus_no7_nosensors$freq+1)

Pip_no7_nosensors <- occ_genus_no7_nosensors[genus == "Pipistrellus",]

p1 <- ggplot(Pip_no7_nosensors, aes(x= date, y= logfreq, group=week(date))) +
  geom_boxplot(aes()) +
  labs(x="Month", y = "log(Daily Calls + 1)") +
  ggtitle("(A)") +
  theme_classic()

Nyc_no7_nosensors <- occ_genus_no7_nosensors[genus == "Nyctalus",]

p2 <- ggplot(Nyc_no7_nosensors, aes(x= date, y= logfreq, group=week(date))) +
  geom_boxplot(aes()) +
  labs(x="Month", y = "log(Daily Calls + 1)") +
  ggtitle("(B)") +
  theme_classic()

#####sensor7######

occ_genus_s7 <- occ_genus[c(occ_genus$sensor=="7"),]

occ_genus_s7_nosensors <- occ_genus_s7 %>% group_by(date, genus) %>% summarize(freq = sum(freq))

occ_genus_s7_nosensors$logfreq <- log(occ_genus_s7_nosensors$freq+1)

Pip_s7_nosensors <- occ_genus_s7_nosensors[genus == "Pipistrellus",]

p3 <- ggplot(Pip_s7_nosensors, aes(x= date, y= logfreq, group=week(date))) +
  geom_boxplot(aes()) +
  labs(x="Month", y = "log(Daily Calls + 1)") +
  ggtitle("(C)") +
  theme_classic()

Nyc_s7_nosensors <- occ_genus_s7_nosensors[genus == "Nyctalus",]

p4 <- ggplot(Nyc_s7_nosensors, aes(x= date, y= logfreq, group=week(date))) +
  geom_boxplot(aes()) +
  labs(x="Month", y = "log(Daily Calls + 1)") +
  ggtitle("(D)") +
  theme_classic()

grid.arrange(p1, p2,                          
             p3, p4,
             ncol = 2, nrow = 2,
             layout_matrix = rbind(c(1,2), c(3,4))) 
#####


Pip_sensoravg$sensor2 <- as.factor(Pip_sensoravg$sensor)
Nyc_sensoravg$sensor <- as.factor(Nyc_sensoravg$sensor)
Ple_sensoravg$sensor <- as.factor(Ple_sensoravg$sensor)
Myo_sensoravg$sensor <- as.factor(Myo_sensoravg$sensor)


b1 <- ggplot(Pip_sensoravg, aes(x= sensor, y= freq)) +
  geom_bar(stat = "identity") +
  labs(x="Sensor", y = "Mean Daily Calls") +
  ggtitle("(A)") +
  theme_classic()

b2 <- ggplot(Nyc_sensoravg, aes(x= sensor, y= freq)) +
  geom_bar(stat = "identity") +
  labs(x="Sensor", y = "Mean Daily Calls") +
  ggtitle("(B)") +
  theme_classic()

b3 <- ggplot(Ple_sensoravg, aes(x= sensor, y= freq)) +
  geom_bar(stat = "identity") +
  labs(x="Sensor", y = "Mean Daily Calls") +
  ggtitle("(C)") +
  theme_classic()

b4 <- ggplot(Myo_sensoravg, aes(x= sensor, y= freq)) +
  geom_bar(stat = "identity") +
  labs(x="Sensor", y = "Mean Daily Calls") +
  ggtitle("(D)") +
  theme_classic()

grid.arrange(b1, b2,                          
             b3, b4,
             ncol = 2, nrow = 2,
             layout_matrix = rbind(c(1,2), c(3,4)))

#######testing assumptions#######

plot(fitted(model_no7), residuals(model_no7))
hist(residuals(model_no7))
qqnorm(residuals(model_no7))
boxplot(residuals(model_no7))




