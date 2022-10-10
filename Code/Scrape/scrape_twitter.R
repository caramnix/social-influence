#a<- readRDS("116_USERTWEETS.rds")


setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence")

#ifelse(!dir.exists(file.path("data1/")),
#       dir.create(file.path("data1/"), showWarnings = FALSE),
#       FALSE)
#ifelse(!dir.exists(file.path("includes1/")),
#       dir.create(file.path("includes1/"), showWarnings = FALSE),
#       FALSE)

#install.packages("academictwitteR")
library(academictwitteR)


vignette("academictwitteR-auth")

set_bearer()


MC_data<- read.csv(file = '/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/116/moc116_joined.csv')

MC_data<- MC_data[MC_data$chamber == "House",]

houseusers <- as.character(unlist((MC_data["handle"])))

#okay, so have author ID 
#

#going to want to instead loop through and save jsons for each user, that way we can use username like we did previously 


## looks like this will work! (woo), might want to make author id --> username dictionary w/ dataverse stuff 
## 


#scraping 2/9/22 

#116 House 
setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/116/House/house_116_tweets")

for (i in 1:length(houseusers)) {
  get_all_tweets(
    users = houseusers[i],
    is_retweet = NULL,
    start_tweets = "2019-01-03T00:00:00Z",
    end_tweets = "2021-01-02T00:00:00Z",
    data_path = paste(paste("data99",houseusers[i],  sep=""), "/", sep=""),
    bind_tweets = FALSE,
    n = Inf
  )
}



#116House- rescrape 
#

new_users_116H<- c("DonnaShalala", "LanceGooden", "benmcadams", "RepRaulRuizMD", "RepShimkus", "RepWatkins", "TeamRoby", "benraylujan", "RepJamesComer", 
                   "realc_collins", "SeanDuffyWI", "JoeKennedy", "hornforcongress", "RepMikeLevin", "DennyHeck", "tomreedcongress", "tomreedny23", 
                   "congressman_jvd")



for (i in 1:length(new_users_116H)) {
  get_all_tweets(
    users = new_users_116H[i],
    is_retweet = NULL,
    start_tweets = "2019-01-03T00:00:00Z",
    end_tweets = "2021-01-02T00:00:00Z",
    data_path = paste(paste("data99",new_users_116H[i],  sep=""), "/", sep=""),
    bind_tweets = FALSE,
    n = Inf
  )
}



#test<- bind_tweets("data1/", user =TRUE)

#paste(users[1],"/data1/", "4" sep="")

#paste(paste("data99",users[1],  sep=""), "/", sep="")

#116 Senate 
MC_data<- read.csv(file = '/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/116/moc116_joined.csv')

MC_data<- MC_data[MC_data$chamber == "Senate",]

senateusers <- as.character(unlist((MC_data["handle"])))

setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/116/Senate/senate_116_tweets")

for (i in 1:length(senateusers)) {
  get_all_tweets(
    users = senateusers[i],
    is_retweet = NULL,
    start_tweets = "2019-01-03T00:00:00Z",
    end_tweets = "2021-01-02T00:00:00Z",
    data_path = paste(paste("data99",senateusers[i],  sep=""), "/", sep=""),
    bind_tweets = FALSE,
    n = Inf
  )
}




##########

getwd()
tweets<- get_all_tweets(
  users = houseusers[1:5],
  is_retweet = NULL,
  start_tweets = "2019-01-03T00:00:00Z",
  end_tweets = "2019-01-05T00:00:00Z",
  data_path = "data1/",
  bind_tweets = FALSE,
  n = Inf
)



#115 House
MC_data_115<- read.csv(file = '/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/115/115_congress.csv')
MC_data_115<- MC_data_115[MC_data_115$chamber == "House",]

houseusers_115 <- as.character(unlist((MC_data_115["screen_name"])))


setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/115/House/house_116_tweets")

for (i in 1:length(houseusers_115)) {
  get_all_tweets(
    users = houseusers_115[i],
    is_retweet = NULL,
    start_tweets = "2017-01-03T00:00:00Z",
    end_tweets = "2019-01-02T00:00:00Z",
    data_path = paste(paste("data99",houseusers_115[i],  sep=""), "/", sep=""),
    bind_tweets = FALSE,
    n = Inf
  )
}


#GOT ERROR W/ 430 
#...RESCRAPE 

houseusers_115_p<-houseusers_115[431:476]

setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/115/115_House/house_115_tweets")

for (i in 1:length(houseusers_115_p)) {
  get_all_tweets(
    users = houseusers_115_p[i],
    is_retweet = NULL,
    start_tweets = "2017-01-03T00:00:00Z",
    end_tweets = "2019-01-02T00:00:00Z",
    data_path = paste(paste("data99",houseusers_115_p[i],  sep=""), "/", sep=""),
    bind_tweets = FALSE,
    n = Inf
  )
}





######## 


#115 Senate 

MC_data<- read.csv(file = '/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/115/115_Senate/senators-accounts-1.csv')

#MC_data<- MC_data[MC_data$chamber == "Senate",]

senateusers_id <- as.character(unlist((MC_data["Uid"])))
senateusers<- as.character(unlist((MC_data["Token"])))
setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/115/115_Senate/senate_115_tweets")

#whoops, need to query 6 (803694000000000000, 811314000000000000, 816683000000000000)

#start @95 next
for (i in 95:length(senateusers)) {
  get_all_tweets(
    users = senateusers_id[i],
    is_retweet = NULL,
    start_tweets = "2017-01-03T00:00:00Z",
    end_tweets = "2019-01-02T00:00:00Z",
    data_path = paste(paste("data99",senateusers[i],  sep=""), "/", sep=""),
    bind_tweets = FALSE,
    n = Inf
  )
}

sen_long<- c("KamalaHarris") #,"SenCortezMasto") #,", "SenJeffMerkley")
#id_s <- c("803694000000000000") #, "811314000000000000") #, "816683000000000000")
for (i in 1: length(id_s) ) {
  get_all_tweets(
    users = sen_long[i],
    is_retweet = NULL,
    start_tweets = "2017-01-03T00:00:00Z",
    end_tweets = "2019-01-02T00:00:00Z",
    data_path = paste(paste("data99",sen_long[i],  sep=""), "/", sep=""),
    bind_tweets = FALSE,
    n = Inf
  )
}



