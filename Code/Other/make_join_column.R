
rm(list=ls())
#code to combine Connectedness data, user data, and LES data for 116th Congress
#written 3.2.22 by @caramnix

#######################################
#######################################
########## 116th House- COSPO #########
#######################################
#######################################
#read in data 
setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data")

#LES data
LEShousedat116<- read.csv("LEShousedat116.csv")
#445 obs 

#connectedness data
Connectedhousedat116<- read.csv("Congress/116/House/connectedness_house_116.csv")
#448 obs

#leginfo data
leginfohousedat116<- read.csv("Congress/116/House/leginfo_116H.csv")
#448 obs 

#o.g. house data -TWITTER (need to join) 
MCDATA116<- read.csv("Twitter/116/moc116_joined.csv")

MCDATA116house<- MCDATA116[which(MCDATA116$chamber == 'House'),]
#494 obs 

#Q are the ICPSR #'s the same? 
#df<- MCDATA116house[which(MCDATA116house$icpsr%in% LEShousedat116$ICPSR.number..according.to.Poole.and.Rosenthal), ]

#first step, join connectedness and leginfohouse
Connected_info= merge(x=leginfohousedat116,y=Connectedhousedat116,by.x="ID", by.y="ID",all=TRUE)

#NOTE: useful code from last year: leg_effectivenss116.R
  
n <-dim(LEShousedat116['Two.letter.state.code'])[1]

vec1 <- vector(mode="character", length=n)
for (i in 1:n) {
  state <- LEShousedat116[i,]['Two.letter.state.code']$Two.letter.state.code[1]
  district<- LEShousedat116[i,]['Congressional.district.number']
  if (as.integer(district) < 10) {
    vec1[i]<- paste(state,"-0", district, sep="")
  } else{
    vec1[i]<- paste(state,"-",district, sep="")
  }
  
}
LEShousedat116["cleaned_state_code"] <- vec1

#then can have connectedness --> LES (as desired)

Connected_info

st<- "Rep. Larsen, Rick [D-WA-2]"

make_join<- function(st) {
  st= gsub("[", "!", st, fixed = TRUE) 
  st= gsub("]", "!", st, fixed = TRUE) 

  st= unlist(strsplit(st, split="!"))[2]

  st= strsplit(st,split="-")
  statecode = st[[1]][2]
  districtcode= st[[1]][3]
  if (districtcode == "At Large") {
    st<- paste(statecode, "-01", sep="")
  }  else if (as.integer(districtcode) < 10) {
    st<- paste(statecode,"-0", districtcode, sep="")
  } else{
    st<- paste(statecode,"-",districtcode, sep="")
  }
  return(st)
}


Connected_info["cleaned_state_code"] <- unlist(lapply(Connected_info$Full.Name, make_join))

length(unique(Connected_info$cleaned_state_code)) #441 (versus 448) 
length(unique(LEShousedat116$cleaned_state_code)) #441 versus 448

LES_Connectedness<- merge(x=Connected_info,y=LEShousedat116,by.x="cleaned_state_code", by.y="cleaned_state_code",all.y=TRUE)

a<- unique(LES_Connectedness$Full.Name)
b<- LES_Connectedness$Full.Name

#cleaned_state_code

LEShousedat116_short<- LEShousedat116[!duplicated(LEShousedat116["cleaned_state_code"]),]

#look at duplicated and fix the join
LEShousedat116_dups<-  LEShousedat116[duplicated(LEShousedat116["cleaned_state_code"]),]

#	Duffy WI-07 --> WI-07-1
#	TIFFANY, Thomas P. WI-07 --> WI-07-2

#fix within LES data 
LEShousedat116[LEShousedat116$Name.in.bioguide=="LEWIS, John R.",]
#row is 240 
LEShousedat116$cleaned_state_code[[240]] = "GA-05-1"

LEShousedat116[LEShousedat116$Name.in.bioguide=="HALL, Kwanza",]
#row is 166 
LEShousedat116$cleaned_state_code[[240]] = "GA-05-2"


LEShousedat116[LEShousedat116$Name.in.bioguide=="DUFFY, Sean",]
#113
LEShousedat116$cleaned_state_code[[113]] = "WI-07-1"

LEShousedat116[LEShousedat116$Name.in.bioguide=="TIFFANY, Thomas P.",]
#399
LEShousedat116$cleaned_state_code[[399]] = "WI-07-2"


LEShousedat116[LEShousedat116$Name.in.bioguide=="COLLINS, Chris",]
#74
LEShousedat116$cleaned_state_code[[74]] = "NY-27-1"

LEShousedat116[LEShousedat116$Name.in.bioguide=="JACOBS, Chris",]
#193
LEShousedat116$cleaned_state_code[[193]] = "NY-27-2"


LEShousedat116[LEShousedat116$Name.in.bioguide=="JONES, Walter Beaman, Jr.",]
#201
LEShousedat116$cleaned_state_code[[201]] = "NC-03-1"

LEShousedat116[LEShousedat116$Name.in.bioguide=="MURPHY, Gregory Francis",]
#287
LEShousedat116$cleaned_state_code[[287]] = "NC-03-2"


##FIX w.n connected data 

Connected_info[Connected_info$Last.Name == "DUFFY",]$cleaned_state_code = "WI-07-1"
Connected_info[Connected_info$Last.Name == "TIFFANY",]$cleaned_state_code = "WI-07-2"


Connected_info[Connected_info$Last.Name == "LEWIS",]$cleaned_state_code = "GA-05-1"
Connected_info[Connected_info$Last.Name == "HALL",]$cleaned_state_code = "GA-05-2"

Connected_info[Connected_info$Last.Name == "COLLINS",]
#
Connected_info$cleaned_state_code[[232]] = "NY-27-1"

Connected_info[Connected_info$Last.Name == "JACOBS",]$cleaned_state_code = "NY-27-2"

Connected_info[Connected_info$Last.Name == "JONES",]$cleaned_state_code= "NC-03-1"

Connected_info[Connected_info$Last.Name == "MURPHY",] #$cleaned_state_code= "NC-03-2"
Connected_info$cleaned_state_code[[443]] = "NC-03-2"

LES_Connectedness<- merge(x=Connected_info,y=LEShousedat116,by.x="cleaned_state_code", by.y="cleaned_state_code")


#LOOK, delete the ones that are still dups 
#NOTE; there are 7 instances where the state codes overlap (due to deaths/ transitions)- going to need to make UNIQUE 
Connected_info_short<- Connected_info[!duplicated(Connected_info["cleaned_state_code"]),]
connected_dups<-  Connected_info[duplicated(Connected_info["cleaned_state_code"]),]
#-- we fixed the four we have LES data for above 

LES_Connectedness<- LES_Connectedness[-c(200, 344, 48), ]  #keep elijah cummings MD-07
#LES_Connectedness<- LES_Connectedness[-c(344), ]  #keep fred PA-12
#LES_Connectedness<- LES_Connectedness[-c(47), ] #keep katie CA-25

length(unique(LEShousedat116$cleaned_state_code))

length(unique(LES_Connectedness$cleaned_state_code))

a<- LES_Connectedness[duplicated(LES_Connectedness["cleaned_state_code"]),]

#write.csv(LES_Connectedness, "Congress/116/House/house116_LES_Connected.csv")

### RESULTS ###


LES_Connectedness$duration<- 2021-LES_Connectedness$Year.first.elected.to.House

# AS WE INCREASE CONNECTEDNESS WE INCREASE LEG EFFECTIVENESS for 116th House
summary(m3<- lm(Legislative.Effectiveness.Score..1.5.10. ~ Connectedness + duration, data=LES_Connectedness))


#######################################
#######################################
######### 116th Senate- COSPO #########
#######################################
#######################################

LESsendat116<- read.csv("LESsenatedat116.csv")
#445 obs 

#connectedness data
Connectedsendat116<- read.csv("Congress/116/Senate/connectedness_senate_116.csv")
#448 obs

#leginfo data
leginfosendat116<- read.csv("Congress/116/Senate/leginfo_116S.csv")
#448 obs 

#o.g. house data -TWITTER (need to join) 
MCDATA116<- read.csv("Twitter/116/moc116_joined.csv")

MCDATA116senate<- MCDATA116[which(MCDATA116$chamber == 'Senate'),]
#494 obs 
#

Connected_info116S= merge(x=leginfosendat116,y=Connectedsendat116,by.x="ID", by.y="ID",all=TRUE)


##add join column to LES data - can't be state + district (no district) 
n <-dim(LESsendat116['two.letter.state.abbreviation'])[1]

#vec1 <- vector(mode="character", length=n)
#for (i in 1:n) {
#  state <- LESsendat116[i,]['two.letter.state.abbreviation']$Two.letter.state.code[1]
#  district<- LESsendat116[i,]['Congressional.district.number']
#  if (as.integer(district) < 10) {
#    vec1[i]<- paste(state,"-0", district, sep="")
#  } else{
#    vec1[i]<- paste(state,"-",district, sep="")
#  }
  
#LESsendat116["cleaned_state_code"] <- vec1

length(unique(LESsendat116$last.name))

LESsendat116["lower_last_name"] <- tolower(LESsendat116$last.name)
Connected_info116S["lower_last_name"] <- tolower(Connected_info116S$Last.Name)

#issue: two scotts 
Connected_info116S[duplicated(Connected_info116S["lower_last_name"]),]

Connected_info116S$lower_last_name[[100]] = "scottfl"
Connected_info116S$lower_last_name[[63]] = "scottsc"

LESsendat116$lower_last_name[[82]] = "scottfl"
LESsendat116$lower_last_name[[83]] = "scottsc"

LESsendat116$lower_last_name%in% Connected_info116S$lower_last_name

LES_Connectedness_116S<- merge(x=Connected_info116S,y=LESsendat116,by.x="lower_last_name", by.y="lower_last_name")

#write.csv(LES_Connectedness_116S, "Congress/116/Senate/senate116_LES_Connected.csv")
#
LES_Connectedness_116S$duration<- 2021- LES_Connectedness_116S$year.first.elected.to.the.senate

summary(m4<- lm(legislative.effectiveness.score ~ Connectedness + duration, data=LES_Connectedness_116S))

#interesting, significant at the .1 level,...hmm - THIS IS FOWLER's version 
LES_Connectedness_116S$legislative.effectiveness.score



#######################################
#######################################
########## 115th House- COSPO #########
#######################################
#######################################

#read in data 
setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data")

#LES data
LEShousedat115<- read.csv("LEShousedat115.csv")
#445 obs 

#connectedness data
Connectedhousedat115<- read.csv("Congress/115/House/connectedness_house_115.csv")
#448 obs

#leginfo data
leginfohousedat115<- read.csv("Congress/115/House/leginfo_115H.csv")
#448 obs 

#o.g. house data -TWITTER (need to join) 
MCDATA115<- read.csv("Twitter/115/115_congress.csv")

MCDATA115house<- MCDATA115[which(MCDATA115$chamber == 'House'),]
#494 obs 

#first merge connectedness & leg info 

leginfo115_connectedness<- merge(x=leginfohousedat115,y=Connectedhousedat115,by="ID")


#so now may the cleaned state code 

make_join<- function(st) {
  st= gsub("[", "!", st, fixed = TRUE) 
  st= gsub("]", "!", st, fixed = TRUE) 
  
  st= unlist(strsplit(st, split="!"))[2]
  
  st= strsplit(st,split="-")
  statecode = st[[1]][2]
  districtcode= st[[1]][3]
  if (districtcode == "At Large") {
    st<- paste(statecode, "-01", sep="")
  }  else if (as.integer(districtcode) < 10) {
    st<- paste(statecode,"-0", districtcode, sep="")
  } else{
    st<- paste(statecode,"-",districtcode, sep="")
  }
  return(st)
}


leginfo115_connectedness["cleaned_state_code"] <- unlist(lapply(leginfo115_connectedness$Full.Name, make_join))
leginfo115_connectedness["cleaned_state_code_lastname"] <-paste0(leginfo115_connectedness$cleaned_state_code, "-", tolower(leginfo115_connectedness$Last.Name))


#now make the same join column in LES data 

LEShousedat115

n<- 448

vec1 <- vector(mode="character", length=n)
vec2 <- vector(mode="character", length=n)

for (i in 1:n) {
  state <- LEShousedat115[i,]['Two.letter.state.code']$Two.letter.state.code[1]
  district<- LEShousedat115[i,]['Congressional.district.number']
  lastname<- tolower(unlist(strsplit(as.character(as.character(LEShousedat115$Legislator.name..as.given.in.THOMAS)[i]), split=","))[1])
  if (as.integer(district) < 10) {
    vec1[i]<- paste(state,"-0", district, sep="")
    vec2[i]<- paste(state,"-0", district, "-", lastname, sep="")
  } else{
    vec1[i]<- paste(state,"-",district, sep="")
    vec2[i]<- paste(state,"-", district, "-", lastname, sep="")
  }
  
}
LEShousedat115["cleaned_state_code"] <- vec1
LEShousedat115["cleaned_state_code_lastname"] <- vec2


#try joining  by last name column 

LESConnected_115H<- merge(x=leginfo115_connectedness,y=LEShousedat115,by.x="cleaned_state_code_lastname", by.y="cleaned_state_code_lastname")

#write.csv(LESConnected_115H, "Congress/115/House/house115_LES_Connected.csv")
#

LESConnected_115H$duration<- 2019- LESConnected_115H$Year.first.elected.to.House

summary(m5<- lm(Legislative.Effectiveness.Score..1.5.10. ~ Connectedness + duration, data=LESConnected_115H))

#m5<- lm(Legislative.Effectiveness.Score..1.5.10. ~ Connectedness, data=testing)

#summary(m5)









#######################################
#######################################
########## 115th Senate- COSPO ########
#######################################
#######################################
rm(list=ls())

#read in data 
setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data")

#LES data
LESsendat115<- read.csv("LESsenatedat115.csv")
#445 obs 

#connectedness data
Connectedsenatedat115<- read.csv("Congress/115/Senate/connectedness_senate_115.csv")
#448 obs

#leginfo data
leginfosenatedat115<- read.csv("Congress/115/Senate/leginfo_115S.csv")
#448 obs 

#o.g. house data -TWITTER (need to join) 
MCDATA115<- read.csv("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/115/115_Senate/senators-accounts-1.csv")


#first merge connectedness & leg info 

leginfo115_connectedness<- merge(x=leginfosenatedat115,y=Connectedsenatedat115,by="ID")


#now join based on last name (leginfo115_connectedness, LESsendat115)

length(unique(LESsendat115$lastnamelower)) # all unique so easy join- praise the Lord. 

LESsendat115$lastnamelower<- tolower(LESsendat115$last.name)

leginfo115_connectedness$lastnamelower <- tolower(leginfo115_connectedness$Last.Name)

LESConnected_115S<- merge(x=leginfo115_connectedness,y=LESsendat115,by.x="lastnamelower", by.y="lastnamelower")

#write.csv(LESConnected_115S, "Congress/115/Senate/senate115_LES_Connected.csv")
#

LESConnected_115S$duration <- 2019- LESConnected_115S$year.first.elected.to.the.senate

summary(m6<- lm(legislative.effectiveness.score ~ Connectedness + duration, data=LESConnected_115S))

#sign. at .1 level 

class(LESConnected_115S$duration)


#######################################
#######################################
########## Build table- COSPO #########
#######################################
#######################################

library(xtable)

xtable(m6)

#m3 <- 116 House

#
#install.packages("rockchalk")
library(rockchalk)

ex3 <- outreg(list("116 House" = m3, "116 Senate" = m4, "115 House" = m5, "115 Senate" = m6),
               varLabels = list (x1 = " Connectedness", x2= "Seniority"))
cat ( ex3 )

#title = " My Two Linear Regressions ( uncentered ) ",
#  label = " tab : ex3 " , request = c ( fstatistic = " F " ) , print.results = FALSE )



######## NOW NEED TO DO IT ALL FOR TWITTER TOO - ack- sigh- cry ########




#######################################
#######################################
######### 116th House- TWITTER ########
#######################################
#######################################
#######################################
LES_Connectedness
f<- '/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/116/House/house_116_tweets/connectedness_house_twitter_116_updated.csv' 

C_T_116H<- read.csv(f)

C_T_116H_LES<- merge(LES_Connectedness,C_T_116H, by.x="ID" ,by.y="X")

colnames(C_T_116H_LES)

C_T_116H_LES$Connectedness.x

C_T_116H_LES$Connectedness.y
colnames(C_T_116H_LES)[69] <- "Twitter_Connectedness"

C_T_116H_LES$duration <- 2021- C_T_116H_LES$Year.first.elected.to.House
C_T_116H_LES$Legislative.Effectiveness.Score..1.5.10.
m7<- lm( Legislative.Effectiveness.Score..1.5.10. ~ Twitter_Connectedness +duration, data=C_T_116H_LES)
summary(m7)
C_T_116H_LES$duration

class(C_T_116H_LES$duration)


write.csv(C_T_116H_LES, "Twitter/116/House/twitter116_LES_Connected.csv")



followers <- read.csv("/Users/caranix/OneDrive - The Ohio State University/2020-2021/Spring/7560/FINAL_PAPER/output/modified_output/username_followers_checked.csv", header= F)
names(followers)[names(followers ) == "V1"] <- "handle"
names(followers)[names(followers ) == "V2"] <- "number_of_followers"

followers$number_of_followers<- as.numeric(as.character(followers$number_of_followers))

test<- merge(followers,leg_info, by.x="handle" ,by.y="handle")

C_T_116H_LES
test2<- merge(test,C_T_116H_LES, by.x="X.x" ,by.y="X")


m75<- lm( Legislative.Effectiveness.Score..1.5.10. ~ number_of_followers + duration, data=test2)

summary(m75)

xtable(m75)

test4<- test2[!is.na(test2$number_of_followers),]

cor(test4$number_of_followers, test4$Twitter_Connectedness)


#######################################
#######################################
######### 116th Senate- TWITTER #######
#######################################
#######################################
#######################################


f2<- '/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/116/Senate/connectedness_senate_twitter_116.csv' 

C_T_116S<- read.csv(f2)

C_T_116S_LES<- merge(LES_Connectedness_116S,C_T_116S, by.x="ID" ,by.y="X")

C_T_116S_LES$Connectedness.y
colnames(C_T_116S_LES)[69]  <- "Twitter_Connectedness"

m8<- lm(legislative.effectiveness.score ~ Twitter_Connectedness + duration, data=C_T_116S_LES)
summary(m8)

write.csv(C_T_116S_LES, "Twitter/116/Senate/twitter116_LES_Connected.csv")



#######################################
#######################################
######### 115th House- TWITTER ########
#######################################
#######################################
#######################################


f3<- '/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/115/115_House/connectedness_house_twitter_115.csv' 

C_T_115H<- read.csv(f3)

C_T_115H_LES<- merge(LESConnected_115H,C_T_115H, by.x="ID" ,by.y="X")
#colnames(C_T_115H_LES)

colnames(C_T_115H_LES)[72] <- "Twitter_Connectedness"

C_T_115H_LES$duration<- 2021- C_T_115H_LES$Year.first.elected.to.House

m9<- lm(Legislative.Effectiveness.Score..1.5.10. ~ Twitter_Connectedness + duration, data=C_T_115H_LES)
summary(m9)

write.csv(C_T_115H_LES, "Twitter/115/115_House/twitter115_LES_Connected.csv")




#######################################
#######################################
######### 115th Senate- TWITTER #######
#######################################
#######################################
#######################################


f4<- '/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/115/115_Senate/connectedness_senate_twitter_115.csv' 

C_T_115S<- read.csv(f4)


C_T_115S_LES<- merge(LESConnected_115S,C_T_115S, by.x="ID" ,by.y="X")

colnames(C_T_115S_LES)[69] <- "Twitter_Connectedness"

m10<- lm(legislative.effectiveness.score ~ Twitter_Connectedness + duration, data=C_T_115S_LES)
summary(m10)

write.csv(C_T_115S_LES, "Twitter/115/115_Senate/twitter115_LES_Connected.csv")


ex4 <- outreg(list("116 House" = m7, "116 Senate" = m8, "115 House" = m9, "115 Senate" = m10),
              varLabels = list (x1 = "Connectedness", x2= "Seniority"), title = "Connectedness Twitter")


#
























































#now we can join! 

LES_Connectedness_115S<- merge(x=leginfo115_connectedness,y=LEShousedat115,by.x="cleaned_state_code", by.y="cleaned_state_code")
#467 


LES_Connectedness_115_no_dups<- LES_Connectedness_115S[!duplicated(LES_Connectedness_115S["cleaned_state_code"]),]
#note: this cuts some ppl! 

#this tells us who was cut: 
LES_Connectedness_115S_dups<-  LES_Connectedness_115S[duplicated(LES_Connectedness_115S["cleaned_state_code"]),]

#so now, all we have to do is join the *correct* information from leginfo115_connectedness and we'll be in business! 

LES_Connectedness_115S_dups$cleaned_state_code


test<- merge(x=LES_Connectedness_115S_dups,y=leginfo115_connectedness,by.x="cleaned_state_code_lastname.y", by.y="cleaned_state_code_lastname", all.y=FALSE)

test2<- test[!duplicated(test["cleaned_state_code_lastname.x"]),]

LES_Connectedness_115S_dups$
  
  LEShousedat115_dups


#note, some dulpicated
LES_Connectedness_115S$clea
LEShousedat115_short<- LEShousedat115[!duplicated(LEShousedat115["cleaned_state_code"]),]

#look at duplicated and fix the join
LEShousedat115_dups<-  LEShousedat115[duplicated(LEShousedat115["cleaned_state_code"]),]

#for the duplicated ones, use the last-name! And now just add these back into the data 
test<- merge(x=LEShousedat115_dups,y=LEShousedat115_short,by.x="cleaned_state_code_lastname", by.y="cleaned_state_code_lastname",all.x=TRUE)

test2<- merge(x=test,y=leginfo115_connectedness,by.x="cleaned_state_code_lastname", by.y="cleaned_state_code_lastname",all.x=TRUE)

LES_Connectedness_115<- rbind(LEShousedat115_short,test)



LEShousedat115_short$
  
  
  
  
  
  
  
  split(str)
[1]

unlist(lapply(LEShousedat115$Name.in.bioguide, last_name))

last_name <- function(str) {
  return(tolower(unlist(strsplit(as.character(LEShousedat115$Name.in.bioguide), split=","))[1]))
}





as.character(LEShousedat115$Name.in.bioguide)[1]

make_join_last_name<- function(st) {
  st= gsub("[", "!", st, fixed = TRUE) 
  st= gsub("]", "!", st, fixed = TRUE) 
  
  st= unlist(strsplit(st, split="!"))[2]
  
  st= strsplit(st,split="-")
  statecode = st[[1]][2]
  districtcode= st[[1]][3]
  if (districtcode == "At Large") {
    st<- paste(statecode, "-01", sep="")
  }  else if (as.integer(districtcode) < 10) {
    st<- paste(statecode,"-0", districtcode, sep="")
  } else{
    st<- paste(statecode,"-",districtcode, sep="")
  }
  return(st)
}
















#also need this cleaned_state_code for MCDATA116house -- which has ICPSR code 

#ss<- function (str) {
#  return(gsub("[()]", "", str))
#}
#MCDATA116house["cleaned_state_code"]<- unlist(lapply(MCDATA116house$cqlabel,ss))

#length(LEShousedat116$cleaned_state_code) #445
#length(MCDATA116house$cleaned_state_code) #494

#LEShousedat116$cleaned_state_code %in% MCDATA116house$cleaned_state_code

#joined <- merge(LEShousedat116, MCDATA116house, by="cleaned_state_code", all= TRUE)
#so joined length is 509 due to duplicate twitter accts. 
#just need unique ones 


#^^ actually don't need this, just make cleaned state code w/n connectedness data 







