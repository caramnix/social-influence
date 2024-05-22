#5.23.22, rewritten 4.25.24 

rm(list=ls())

library(reshape2)
library(caret)

setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data")

#################################
#################################
######### COSPONSORSHIP #########
#################################
#################################

###################
###################
#HOUSE
###################
###################

house_116<- read.csv("Congress/116/House/house116_LES_Connected.csv")
house_115<-  read.csv("Congress/115/House/house115_LES_Connected.csv")

#check_116_house_COSPO <- house_116[c("ID", "Connectedness" , "ICPSR.number..according.to.Poole.and.Rosenthal")]
#check_115_house_COSPO <- house_115[c("ID", "Connectedness" , "ICPSR.number..according.to.Poole.and.Rosenthal")]

#https://www.digitalocean.com/community/tutorials/normalize-data-in-r
process <- preProcess(as.data.frame(house_116$Connectedness), method=c("range"))
a<- predict(process, as.data.frame(house_116$Connectedness))
house_116$Connectedness_norm <- unlist(a)

process_115 <- preProcess(as.data.frame(house_115$Connectedness), method=c("range"))
b<- predict(process_115, as.data.frame(house_115$Connectedness))
house_115$Connectedness_norm <- unlist(b)

#Connectedness was normed to be between 0 and 1 using min-max scaling. 

min(house_115$Legislative.Effectiveness.Score..1.5.10.)
max(house_115$Legislative.Effectiveness.Score..1.5.10.)
mean(house_115$Legislative.Effectiveness.Score..1.5.10.)

min(house_116$Legislative.Effectiveness.Score..1.5.10.)
max(house_116$Legislative.Effectiveness.Score..1.5.10.)
mean(house_116$Legislative.Effectiveness.Score..1.5.10.)

library("car")
qqPlot(log(house_116$Legislative.Effectiveness.Score..1.5.10. + .0001))


hist(sqrt(house_116$Legislative.Effectiveness.Score..1.5.10.))
hist(house_115$Legislative.Effectiveness.Score..1.5.10.)

# FIXED EFFECT HOUSE 

FE_house_data<- merge(x=house_115, y=house_116, by="ID", all.x=TRUE)

#cut out legs who are only one time point (115th or 116th Congress)
FE_house_data1<- FE_house_data[!is.na(FE_house_data$Connectedness.y),]


FE_house_data1$duration<- 2021-FE_house_data1$Year.first.elected.to.House.x


columns_to_keep<- c("ID", "Full.Name.x", "Party.x", "duration", "Connectedness_norm.x", "Connectedness_norm.y", #"Connectedness.x", "Connectedness.y"
                    "Legislative.Effectiveness.Score..1.5.10..x", "Legislative.Effectiveness.Score..1.5.10..y",
                    "Percent.vote.received.to.enter.this.Congress.x", "First.dimension.DW.NOMINATE.score.x", 
                    "Size.of.House.delegation.from.member.s.state.y", 
                    "X1...committee.chair..according.to.Almanac.of.American.Politics.x", 
                    "X1...Majority.party.leadership.x", "X1...Minority.party.leadership.x",
                    "X1...female.y", "X1..Speaker.y",
                    "X1...African.American.x", "X1...Latino.a.x", "ICPSR.number..according.to.Poole.and.Rosenthal.x")#, "handle", "number_of_followers")


FE_house_data3<- FE_house_data1[columns_to_keep]

colnames(FE_house_data3)[3] <- "Party"
colnames(FE_house_data3)[4] <- "Seniority"
colnames(FE_house_data3)[5] <- "Connectedness115"
colnames(FE_house_data3)[6] <- "Connectedness116"
colnames(FE_house_data3)[7] <- "LES115"
colnames(FE_house_data3)[8] <- "LES116"
colnames(FE_house_data3)[9] <- "VoteShare"
colnames(FE_house_data3)[10] <- "DWNom"
colnames(FE_house_data3)[11] <- "SizeDel"
colnames(FE_house_data3)[12] <- "ComChair"
colnames(FE_house_data3)[13] <- "MajPLead"
colnames(FE_house_data3)[14] <- "MinPLead"
colnames(FE_house_data3)[15] <- "Female"
colnames(FE_house_data3)[16] <- "Speaker"
colnames(FE_house_data3)[17] <- "Black"
colnames(FE_house_data3)[18] <- "Latinx"
colnames(FE_house_data3)[19] <- "icpsr"


#no NA's - 336!
FE_house_data3_nonas<- na.omit(FE_house_data3)

#joined_retweets_LES <- read.csv("/Users/caranix/OneDrive - The Ohio State University/2020-2021/Spring/7560/FINAL_PAPER/IMPT_DATA/retweets_LES_handle_ID_joined.csv")
#followers_data<- joined_retweets_LES[c("ICPSR.number..according.to.Poole.and.Rosenthal", "handle", "number_of_followers") ]
#house_116_followers<- merge(x=FE_house_data3_nonas,y=followers_data, by.x= "icpsr",by.y="ICPSR.number..according.to.Poole.and.Rosenthal", how="left")


#FE_house_data3_nonas<- na.omit(house_116_followers)


FE_house_long2<- reshape(FE_house_data3, direction='long', 
                         varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
                         times=c('c115', 'c116'),
                         v.names=c('LES', 'Connectedness'),
                         idvar='id')


House_FE<- lm(LES~ Connectedness + Seniority + VoteShare + DWNom+ SizeDel + ComChair+ Party + 
                MajPLead+ MinPLead+  Female + Speaker+ Black+ Latinx +factor(Full.Name.x)
              ,data=FE_house_long2)

summary(House_FE)

plot(House_FE)
#https://library.virginia.edu/data/articles/diagnostic-plots

House_FE2<- lm(log(LES + .00001)~ Connectedness + Seniority + VoteShare + DWNom+ SizeDel + ComChair+ Party + 
                MajPLead+ MinPLead+  Female + Speaker+ Black+ Latinx +factor(Full.Name.x)
              ,data=FE_house_long2)

summary(House_FE2)

plot(House_FE2)

#min(FE_house_long2$LES)

FE_house_long2$LES

###################
###################
#SENATE
###################
###################

senate_116<- read.csv("Congress/116/Senate/senate116_LES_Connected.csv")
senate_115<- read.csv("Congress/115/Senate/senate115_LES_Connected.csv")

process <- preProcess(as.data.frame(senate_116$Connectedness), method=c("range"))
a<- predict(process, as.data.frame(senate_116$Connectedness))
senate_116$Connectedness_norm <- unlist(a)

process_115 <- preProcess(as.data.frame(senate_115$Connectedness), method=c("range"))
b<- predict(process_115, as.data.frame(senate_115$Connectedness))
senate_115$Connectedness_norm <- unlist(b)


FE_sen_data<- merge(x=senate_115,y=senate_116, by="ID", all.x=TRUE)

#cut out legs who are only one time point (115th or 116th Congress)
#91 senators were repeats 
FE_senate_data1<- FE_sen_data[!is.na(FE_sen_data$Connectedness.y),]

FE_senate_data1$duration<- 2021-FE_senate_data1$year.first.elected.to.the.senate.x

columns_to_keep<- c("ID", "Full.Name.x", "Party.x", "duration", "Connectedness_norm.x", "Connectedness_norm.y",
                    "legislative.effectiveness.score.x", "legislative.effectiveness.score.y",
                    "vote.share.in.last.election.x", "First.dimensions.dw.nominate.score.x", 
                    "number.of.members.in.state.congressional.delegation.x", 
                    "X1.if.senator.is.a.committee.chair.x", 
                    "X1.if.senator.is.majority.party.leader.x", "X1.if.senator.is.minority.party.leader.x",
                    "X1.if.senator.is.female.x","X1.if.senator.is.african.american.x", "X1.if.senator.is.latino.x")

FE_senate_data3<- FE_senate_data1[columns_to_keep]

colnames(FE_senate_data3)[3] <- "Party"
colnames(FE_senate_data3)[4] <- "Seniority"
colnames(FE_senate_data3)[5] <- "Connectedness115"
colnames(FE_senate_data3)[6] <- "Connectedness116"
colnames(FE_senate_data3)[7] <- "LES115"
colnames(FE_senate_data3)[8] <- "LES116"
colnames(FE_senate_data3)[9] <- "VoteShare"
colnames(FE_senate_data3)[10] <- "DWNom"
colnames(FE_senate_data3)[11] <- "SizeDel"
colnames(FE_senate_data3)[12] <- "ComChair"
colnames(FE_senate_data3)[13] <- "MajPLead"
colnames(FE_senate_data3)[14] <- "MinPLead"
colnames(FE_senate_data3)[15] <- "Female"
colnames(FE_senate_data3)[16] <- "Black"
colnames(FE_senate_data3)[17] <- "Latinx"

FE_senate_data3_nonas<- na.omit(FE_senate_data3)

FE_senate_long2<- reshape(FE_senate_data3_nonas, direction='long', 
                          varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
                          times=c('c115', 'c116'),
                          v.names=c('LES', 'Connectedness'),
                          idvar='Full.Name.x')


#here's the right model
Senate_FE<- lm(LES~  Connectedness + Seniority + VoteShare + DWNom+ SizeDel + ComChair+ Party + 
                 MajPLead+ MinPLead+  Female + Black+ Latinx +factor(Full.Name.x)
               ,data=FE_senate_long2)







###########################
###########################
######### TWITTER #########
###########################
###########################

setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data")



####### SENATE #######
####### SENATE #######
####### SENATE #######

#115 Senate 
#write.csv(C_T_115S_LES, "Twitter/115/115_Senate/twitter115_LES_Connected.csv")
C_T_115S_LES<- read.csv("Twitter/115/115_Senate/twitter115_LES_Connected.csv")


#116 Senate 
#write.csv(C_T_116S_LES, "Twitter/116/Senate/twitter116_LES_Connected.csv")
C_T_116S_LES<- read.csv("Twitter/116/Senate/twitter116_LES_Connected.csv")


process <- preProcess(as.data.frame(C_T_115S_LES$Twitter_Connectedness), method=c("range"))
a<- predict(process, as.data.frame(C_T_115S_LES$Twitter_Connectedness))
C_T_115S_LES$Twitter_connectedness_norm <- unlist(a)

process <- preProcess(as.data.frame(C_T_116S_LES$Twitter_Connectedness), method=c("range"))
a<- predict(process, as.data.frame(C_T_116S_LES$Twitter_Connectedness))
C_T_116S_LES$Twitter_connectedness_norm <- unlist(a)


FE_sen_data_T<- merge(x=C_T_115S_LES,y=C_T_116S_LES, by="ID", all.x=TRUE)

#cut out legs who are only one time point (115th or 116th Congress)
#91 senators were repeats 
FE_senate_data1_T<- FE_sen_data_T[!is.na(FE_sen_data_T$Twitter_Connectedness.y),]

FE_senate_data1_T$duration<- 2021-FE_senate_data1_T$year.first.elected.to.the.senate.x


columns_to_keep<- c("ID", "Full.Name.x", "Party.x", "duration", "Twitter_connectedness_norm.x", "Twitter_connectedness_norm.y",
                    "legislative.effectiveness.score.x", "legislative.effectiveness.score.y",
                    "vote.share.in.last.election.x", "First.dimensions.dw.nominate.score.x", 
                    "number.of.members.in.state.congressional.delegation.x", 
                    "X1.if.senator.is.a.committee.chair.x", 
                    "X1.if.senator.is.majority.party.leader.x", "X1.if.senator.is.minority.party.leader.x",
                    "X1.if.senator.is.female.x","X1.if.senator.is.african.american.x", "X1.if.senator.is.latino.x")

FE_senate_data3_T<- FE_senate_data1_T[columns_to_keep]

colnames(FE_senate_data3_T)

colnames(FE_senate_data3_T)[3] <- "Party"
colnames(FE_senate_data3_T)[4] <- "Seniority"
colnames(FE_senate_data3_T)[5] <- "Connectedness115"
colnames(FE_senate_data3_T)[6] <- "Connectedness116"
colnames(FE_senate_data3_T)[7] <- "LES115"
colnames(FE_senate_data3_T)[8] <- "LES116"
colnames(FE_senate_data3_T)[9] <- "VoteShare"
colnames(FE_senate_data3_T)[10] <- "DWNom"
colnames(FE_senate_data3_T)[11] <- "SizeDel"
colnames(FE_senate_data3_T)[12] <- "ComChair"
colnames(FE_senate_data3_T)[13] <- "MajPLead"
colnames(FE_senate_data3_T)[14] <- "MinPLead"
colnames(FE_senate_data3_T)[15] <- "Female"
colnames(FE_senate_data3_T)[16] <- "Black"
colnames(FE_senate_data3_T)[17] <- "Latinx"

FE_senate_data3_nonas_T<- na.omit(FE_senate_data3_T)

FE_senate_long2_T<- reshape(FE_senate_data3_nonas_T, direction='long', 
                            varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
                            times=c('c115', 'c116'),
                            v.names=c('LES', 'Connectedness'),
                            idvar='Full.Name.x')

Senate_FE_T<- lm(LES~  Connectedness + Seniority + VoteShare + DWNom+ SizeDel + ComChair+ Party + 
                   MajPLead+ MinPLead+  Female + Black+ Latinx +factor(Full.Name.x)
                 ,data=FE_senate_long2_T)

summary(Senate_FE_T)

#comb_models<- stargazer(House_FE_T,Senate_FE_T)
#write(comb_models, "testing comb.txt")

####### HOUSE #######
####### HOUSE #######
####### HOUSE #######

#116 House 
#write.csv(C_T_116H_LES, "Twitter/116/House/twitter116_LES_Connected.csv")
C_T_116H_LES<- read.csv("Twitter/116/House/twitter116_LES_Connected.csv")


#115 House
#write.csv(C_T_115H_LES, "Twitter/115/115_House/twitter115_LES_Connected.csv")
C_T_115H_LES<- read.csv("Twitter/115/115_House/twitter115_LES_Connected.csv")

#check_116_house_TWITTER <- C_T_116H_LES[c("ID", "Connectedness.x" ,"Connectedness.y", "Twitter_Connectedness", "ICPSR.number..according.to.Poole.and.Rosenthal")]
#check_115_house_TWITTER <- C_T_115H_LES[c("ID", "Connectedness.x", "Twitter_Connectedness", "ICPSR.number..according.to.Poole.and.Rosenthal")]

#weird naming, but confirmed against connectedness_house_twitter_116_updated.csv that Connectedness.y is Twitter connectedess 
process <- preProcess(as.data.frame(C_T_116H_LES$Connectedness.y), method=c("range"))
a<- predict(process, as.data.frame(C_T_116H_LES$Connectedness.y))
C_T_116H_LES$Twitter_connectedness_norm <- unlist(a)

process_115 <- preProcess(as.data.frame(C_T_115H_LES$Twitter_Connectedness), method=c("range"))
b<- predict(process_115, as.data.frame(C_T_115H_LES$Twitter_Connectedness))
C_T_115H_LES$Twitter_connectedness_norm <- unlist(b)


FE_house_data_T<- merge(x=C_T_115H_LES,y=C_T_116H_LES, by="ID", all.x=TRUE)

#cut out legs who are only one time point (115th or 116th Congress)- 333 left
FE_house_data1_T<- FE_house_data_T[!is.na(FE_house_data_T$Connectedness.x.y),]

FE_house_data1_T$duration<- 2021-FE_house_data1_T$Year.first.elected.to.House.x


columns_to_keep<- c("ID", "Full.Name.x", "Party.x", "duration", "Twitter_connectedness_norm.x", "Twitter_connectedness_norm.y",
                    "Legislative.Effectiveness.Score..1.5.10..x", "Legislative.Effectiveness.Score..1.5.10..y",
                    "Percent.vote.received.to.enter.this.Congress.x", "First.dimension.DW.NOMINATE.score.x", 
                    "Size.of.House.delegation.from.member.s.state.y", 
                    "X1...committee.chair..according.to.Almanac.of.American.Politics.x", 
                    "X1...Majority.party.leadership.x", "X1...Minority.party.leadership.x",
                    "X1...female.y", "X1..Speaker.y",
                    "X1...African.American.x", "X1...Latino.a.x", "ICPSR.number..according.to.Poole.and.Rosenthal.x")


FE_house_data3_T<- FE_house_data1_T[columns_to_keep]

colnames(FE_house_data3_T)[3] <- "Party"
colnames(FE_house_data3_T)[4] <- "Seniority"
colnames(FE_house_data3_T)[5] <- "Connectedness115"
colnames(FE_house_data3_T)[6] <- "Connectedness116"
colnames(FE_house_data3_T)[7] <- "LES115"
colnames(FE_house_data3_T)[8] <- "LES116"
colnames(FE_house_data3_T)[9] <- "VoteShare"
colnames(FE_house_data3_T)[10] <- "DWNom"
colnames(FE_house_data3_T)[11] <- "SizeDel"
colnames(FE_house_data3_T)[12] <- "ComChair"
colnames(FE_house_data3_T)[13] <- "MajPLead"
colnames(FE_house_data3_T)[14] <- "MinPLead"
colnames(FE_house_data3_T)[15] <- "Female"
colnames(FE_house_data3_T)[16] <- "Speaker"
colnames(FE_house_data3_T)[17] <- "Black"
colnames(FE_house_data3_T)[18] <- "Latinx"
colnames(FE_house_data3_T)[19] <- "icpsr"


#no NA's - 333 still! 
FE_house_data3_nonas_T<- na.omit(FE_house_data3_T)

## add in # followers 

#joined_retweets_LES <- read.csv("/Users/caranix/OneDrive - The Ohio State University/2020-2021/Spring/7560/FINAL_PAPER/IMPT_DATA/retweets_LES_handle_ID_joined.csv")
#followers_data<- joined_retweets_LES[c("ICPSR.number..according.to.Poole.and.Rosenthal", "handle", "number_of_followers") ]
#FE_house_data3_nonas_T<- merge(x=FE_house_data3_nonas_T,y=followers_data, by.x="icpsr", by.y="ICPSR.number..according.to.Poole.and.Rosenthal", how="left")
FE_house_data3_nonas_T<- na.omit(FE_house_data3_nonas_T)


#wide to long
library(reshape2)

FE_house_long2_T<- reshape(FE_house_data3_nonas_T, direction='long', 
                           varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
                           times=c('c115', 'c116'),
                           v.names=c('LES', 'Connectedness'),
                           idvar='id')

House_FE_T<- lm(LES~ Connectedness + Seniority + VoteShare + DWNom+ SizeDel + ComChair+ Party + 
                  MajPLead+ MinPLead+  Female + Speaker+ Black+ Latinx +factor(Full.Name.x)
                ,data=FE_house_long2_T)

summary(House_FE_T)

library(stargazer)


####### FULL MODEL HERE #######
full_model<- stargazer(House_FE, Senate_FE, House_FE_T, Senate_FE_T, scalebox='0.7')
write(full_model, "regression_FE_Cospo_Twitter_4_25_24.txt")






###################
###################
#POOLED
###################
###################

FE_house_data4<- FE_house_data3_nonas[!FE_house_data3_nonas["Speaker"]]

FE_house_data4 = subset(FE_house_data3_nonas, select = -Speaker)
FE_house_data4 = subset(FE_house_data4, select = -number_of_followers)
FE_house_data4 = subset(FE_house_data4, select = -icpsr)
FE_house_data4 = subset(FE_house_data4, select = -handle)

#colnames()
#colnames(FE_house_data4) <- c("ID", "Name", "Party", "duration", 
#                              "Connectedness115", "Connectedness116",
#                              "LES115", "LES116", "VoteShare", "DWNom",
#                              "sizedel", "chair", "majpartyleader", "minpartyleader",
#                              "female", "africanamerican", "latinx")

FE_senate_data4<- FE_senate_data3_nonas 

#colnames(FE_senate_data4) <- c("ID", "Name", "Party", "duration", 
#                               "Connectedness115", "Connectedness116",
#                               "LES115", "LES116", "VoteShare", "DWNom",
#                               "sizedel", "chair", "majpartyleader", "minpartyleader",
#                               "female", "africanamerican", "latinx")


pooled_house_senate<- rbind(FE_house_data4,FE_senate_data4)

FE_pooled_long<- reshape(pooled_house_senate, direction='long', 
                         varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
                         times=c('c115', 'c116'),
                         v.names=c('LES', 'Connectedness'),
                         idvar='id')


FE_pooled_long_cospo<- lm(LES~ Connectedness + Seniority + Party  + 
                            VoteShare+ DWNom + SizeDel + ComChair +  MajPLead + MinPLead +
                            Female + Black + Latinx + factor(Full.Name.x), data=FE_pooled_long)

#summary(m7)

############
############
############

FE_house_data4_T<- FE_house_data3_nonas_T[!FE_house_data3_nonas_T["Speaker"]]

FE_house_data4_T = subset(FE_house_data3_nonas_T, select = -Speaker)
FE_house_data4_T = subset(FE_house_data4_T, select = -number_of_followers)
FE_house_data4_T = subset(FE_house_data4_T, select = -icpsr)
FE_house_data4_T = subset(FE_house_data4_T, select = -handle)

FE_senate_data4_T<- FE_senate_data3_nonas_T 

pooled_house_senate_T<- rbind(FE_house_data4_T,FE_senate_data4_T)


FE_pooled_long_T<- reshape(pooled_house_senate_T, direction='long', 
                           varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
                           times=c('c115', 'c116'),
                           v.names=c('LES', 'Connectedness'),
                           idvar='id')


FE_pooled_long_twitter<- lm(LES~ Connectedness + Seniority + Party  + 
                              VoteShare+ DWNom + SizeDel + ComChair +  MajPLead + MinPLead +
                              Female + Black + Latinx + factor(Full.Name.x), data=FE_pooled_long_T)


library(stargazer)
a<-stargazer(FE_pooled_long_cospo, FE_pooled_long_twitter)

write(a, "appendixresultspooled_4_1.txt")


# 





## add in # followers 

joined_retweets_LES <- read.csv("/Users/caranix/OneDrive - The Ohio State University/2020-2021/Spring/7560/FINAL_PAPER/IMPT_DATA/retweets_LES_handle_ID_joined.csv")

joined_retweets_LES_cut<- joined_retweets_LES[!duplicated(joined_retweets_LES[c("number_of_followers","icpsr")]),]

colnames(joined_retweets_LES_cut)


joined_retweets_LES_cut$duration<- 2021-joined_retweets_LES_cut$Year.first.elected.to.House

columns_to_keep<- c("ID.y", "Legislator.name..as.given.in.THOMAS", "Party.x", "duration", "Legislative.Effectiveness.Score..1.5.10.", 
                    "Percent.vote.received.to.enter.this.Congress", "First.dimension.DW.NOMINATE.score", 
                    "Size.of.House.delegation.from.member.s.state", 
                    "X1...committee.chair..according.to.Almanac.of.American.Politics", 
                    "X1...Majority.party.leadership", "X1...Minority.party.leadership",
                    "X1...female", "X1..Speaker",
                    "X1...African.American", "X1...Latino.a", "ICPSR.number..according.to.Poole.and.Rosenthal", "number_of_followers")


joined_retweets_LES_cut<- joined_retweets_LES_cut[columns_to_keep]


colnames(joined_retweets_LES_cut)[3] <- "Party"
colnames(joined_retweets_LES_cut)[4] <- "Seniority"
colnames(joined_retweets_LES_cut)[5] <- "LES"
colnames(joined_retweets_LES_cut)[6] <- "VoteShare"
colnames(joined_retweets_LES_cut)[7] <- "DWNom"
colnames(joined_retweets_LES_cut)[8] <- "SizeDel"
colnames(joined_retweets_LES_cut)[9] <- "ComChair"
colnames(joined_retweets_LES_cut)[10] <- "MajPLead"
colnames(joined_retweets_LES_cut)[11] <- "MinPLead"
colnames(joined_retweets_LES_cut)[12] <- "Female"
colnames(joined_retweets_LES_cut)[13] <- "Speaker"
colnames(joined_retweets_LES_cut)[14] <- "Black"
colnames(joined_retweets_LES_cut)[15] <- "Latinx"
colnames(joined_retweets_LES_cut)[16] <- "icpsr"

#followers_data<- joined_retweets_LES[c("ICPSR.number..according.to.Poole.and.Rosenthal", "handle", "number_of_followers") ]
#data1 <- merge(x=FE_house_data3_nonas_T,y=followers_data, by.x="icpsr", by.y="ICPSR.number..according.to.Poole.and.Rosenthal", how="left")
#data1<- na.omit(data1)


folllowers_reg<- lm(LES~ log(number_of_followers) + Seniority + VoteShare + DWNom+ SizeDel + ComChair+ Party + 
                      MajPLead+ MinPLead+  Female + Speaker+ Black+ Latinx 
                    ,data=joined_retweets_LES_cut)

summary(folllowers_reg)

a<-stargazer(folllowers_reg)

write(a, "regression_followers_4_25_24.txt")

####### FULL MODEL HERE #######
#full_model<- stargazer(House_FE, Senate_FE, House_FE_T, Senate_FE_T, scalebox='0.7')





















##########
#colinearity? 
#
#

col2<- c("duration", "Connectedness115", "Connectedness116",
         "LES115", "LES116",
         "Percent.vote.received.to.enter.this.Congress.x", "First.dimension.DW.NOMINATE.score.x", 
         "Size.of.House.delegation.from.member.s.state.y", 
         "X1...committee.chair..according.to.Almanac.of.American.Politics.x", 
         "X1...Majority.party.leadership.x", "X1...Minority.party.leadership.x",
         "X1...female.y", "X1..Speaker.y",
         "X1...African.American.x", "X1...Latino.a.x")

a<-cor(FE_house_data3_nonas[col2])




