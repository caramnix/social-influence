rm(list=ls())

#5.23.22
#
library(reshape2)

setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data")

######### COSPONSORSHIP #########

###################
###################
#HOUSE
###################
###################

house_116<- read.csv("Congress/116/House/house116_LES_Connected.csv")
house_115<-  read.csv("Congress/115/House/house115_LES_Connected.csv")

# FIXED EFFECT HOUSE 

FE_house_data<- merge(x=house_115,y=house_116, by="ID", all.x=TRUE)

#cut out legs who are only one time point (115th or 116th Congress)
FE_house_data1<- FE_house_data[!is.na(FE_house_data$Connectedness.y),]

FE_house_data1$duration<- 2021-FE_house_data1$Year.first.elected.to.House.x

#FE_house_data1$Legislative.Effectiveness.Score..1.5.10..x
#FE_house_data1$Legislative.Effectiveness.Score..1.5.10..y

#columns_to_keep<- c("ID", "Full.Name.x", "Party.x", "duration", "Connectedness.x", "Connectedness.y", "Legislative.Effectiveness.Score..1.5.10..x", "Legislative.Effectiveness.Score..1.5.10..y")

#FE_house_data2<- FE_house_data1[columns_to_keep]

#colnames(FE_house_data2)[5] <- "Connectedness115"
#colnames(FE_house_data2)[6] <- "Connectedness116"
#colnames(FE_house_data2)[7] <- "LES115"
#colnames(FE_house_data2)[8] <- "LES116"
#colnames(FE_house_data2)

#wide to long


#not quite sure what to do about LES 
#FE_house_long<- melt(FE_house_data2, id.vars=c("Full.Name.x", "ID",  "Party.x", "duration"), value.name = "Connectedness")

#this looks right now!
#FE_house_long<- reshape(FE_house_data2, direction='long', 
#        varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
#        times=c('c115', 'c116'),
#        v.names=c('LES', 'Connectedness'),
#        idvar='Full.Name.x')


#m3<- lm(LES~ Connectedness + duration + Party.x + Full.Name.x, data=FE_house_long)

#m1<- lm(LES~ Connectedness + duration + Party.x , data=FE_house_long)
#summary(m3)
#only 341 house members who stay the same 


#going to want to subset for NA's and add controls 

#all controls in LES data/ following (Battaglini et al.)
#duration
#FE_house_data1$Percent.vote.received.to.enter.this.Congress.x
#FE_house_data1$First.dimension.DW.NOMINATE.score.x
#FE_house_data1$Size.of.House.delegation.from.member.s.state.y
#party 
#FE_house_data1$X1...committee.chair..according.to.Almanac.of.American.Politics.x
#FE_house_data1$X1...Majority.party.leadership.x
#FE_house_data1$X1...Minority.party.leadership.x
#FE_house_data1$X1..Speaker.y
#FE_house_data1$X1...female.y
#FE_house_data1$X1...African.American.x
#FE_house_data1$X1...Latino.a.x


columns_to_keep<- c("ID", "Full.Name.x", "Party.x", "duration", "Connectedness.x", "Connectedness.y",
                    "Legislative.Effectiveness.Score..1.5.10..x", "Legislative.Effectiveness.Score..1.5.10..y",
                    "Percent.vote.received.to.enter.this.Congress.x", "First.dimension.DW.NOMINATE.score.x", 
                    "Size.of.House.delegation.from.member.s.state.y", 
                    "X1...committee.chair..according.to.Almanac.of.American.Politics.x", 
                    "X1...Majority.party.leadership.x", "X1...Minority.party.leadership.x",
                    "X1...female.y", "X1..Speaker.y",
                    "X1...African.American.x", "X1...Latino.a.x")


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


#no NA's - 336!
FE_house_data3_nonas<- na.omit(FE_house_data3)

FE_house_long2<- reshape(FE_house_data3_nonas, direction='long', 
                        varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
                        times=c('c115', 'c116'),
                        v.names=c('LES', 'Connectedness'),
                        idvar='Full.Name.x')

#m4<- lm(LES~ Connectedness + duration + Percent.vote.received.to.enter.this.Congress.x + First.dimension.DW.NOMINATE.score.x+
#          Size.of.House.delegation.from.member.s.state.y + X1...committee.chair..according.to.Almanac.of.American.Politics.x+ 
#          Party.x + X1...Majority.party.leadership.x+ X1...Minority.party.leadership.x+ 
#          + X1...female.y + X1..Speaker.y+ X1...African.American.x+ X1...Latino.a.x +factor(Full.Name.x), data=FE_house_long2)

#m4<- lm(LES~ Connectedness + duration + Percent.vote.received.to.enter.this.Congress.x + First.dimension.DW.NOMINATE.score.x+
#                Size.of.House.delegation.from.member.s.state.y + X1...committee.chair..according.to.Almanac.of.American.Politics.x+ 
#                Party.x + X1...Majority.party.leadership.x+ X1...Minority.party.leadership.x+ 
#                + X1...female.y + X1..Speaker.y+ X1...African.American.x+ X1...Latino.a.x +factor(Full.Name.x), data=FE_house_long2)
#summary(m4)
#

House_FE<- lm(LES~ Connectedness + Seniority + VoteShare + DWNom+ SizeDel + ComChair+ Party + 
                        MajPLead+ MinPLead+  Female + Speaker+ Black+ Latinx +factor(Full.Name.x)
                ,data=FE_house_long2)


#b<-stargazer(m4, m6, title="Results", align=TRUE)
#write(b, "myFile2.txt")

###################
###################
#SENATE
###################
###################

senate_116<- read.csv("Congress/116/Senate/senate116_LES_Connected.csv")
senate_115<- read.csv("Congress/115/Senate/senate115_LES_Connected.csv")

FE_sen_data<- merge(x=senate_115,y=senate_116, by="ID", all.x=TRUE)

#cut out legs who are only one time point (115th or 116th Congress)
#91 senators were repeats 
FE_senate_data1<- FE_sen_data[!is.na(FE_sen_data$Connectedness.y),]

FE_senate_data1$duration<- 2021-FE_senate_data1$year.first.elected.to.the.senate.x

#FE_senate_data1$legislative.effectiveness.score.x
#columns_to_keep<- c("ID", "Full.Name.x", "Party.x", "duration", "Connectedness.x", "Connectedness.y", "legislative.effectiveness.score.x", "legislative.effectiveness.score.y")

#FE_senate_data2<- FE_senate_data1[columns_to_keep]

#colnames(FE_senate_data2)[5] <- "Connectedness115"
#colnames(FE_senate_data2)[6] <- "Connectedness116"
#colnames(FE_senate_data2)[7] <- "LES115"
#colnames(FE_senate_data2)[8] <- "LES116"
#colnames(FE_senate_data2)

#reshape
#this looks right now!
#FE_senate_long<- reshape(FE_senate_data2, direction='long', 
#                        varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
#                        times=c('c115', 'c116'),
 #                       v.names=c('LES', 'Connectedness'),
 #                       idvar='Full.Name.x')

#m5<- lm(LES~ Connectedness + duration + Party.x + Full.Name.x, data=FE_senate_long)

#summary(m5)

#m2<- lm(LES~ Connectedness + duration + Party.x, data=FE_senate_long)


#now add more controls 
#

#FE_senate_data1$X1.if.senator.is.democrat.x
#FE_senate_data1$X1.if.senator.is.female.x
#FE_senate_data1$X1.if.senator.is.african.american.x
#FE_senate_data1$X1.if.senator.is.latino.x
#FE_senate_data1$vote.share.in.last.election.x
#FE_senate_data1$First.dimensions.dw.nominate.score.x
#FE_senate_data1$number.of.members.in.state.congressional.delegation.x
#FE_senate_data1$X1.if.senator.is.a.committee.chair.x
#FE_senate_data1$X1.if.senator.is.majority.party.leader.x
#FE_senate_data1$X1.if.senator.is.minority.party.leader.x

#FE_senate_data1$d

columns_to_keep<- c("ID", "Full.Name.x", "Party.x", "duration", "Connectedness.x", "Connectedness.y",
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




#FE_senate_data3_nonas<- na.omit(FE_senate_data3)

#FE_senate_long2<- reshape(FE_senate_data3_nonas, direction='long', 
#                         varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
#                         times=c('c115', 'c116'),
#                         v.names=c('LES', 'Connectedness'),
#                         idvar='Full.Name.x')


#m6<- lm(LES~ Connectedness + duration + Party.x  + 
#          vote.share.in.last.election.x+ First.dimensions.dw.nominate.score.x
#         + number.of.members.in.state.congressional.delegation.x + X1.if.senator.is.a.committee.chair.x + 
#          X1.if.senator.is.majority.party.leader.x + X1.if.senator.is.majority.party.leader.x +
#          +X1.if.senator.is.minority.party.leader.x +X1.if.senator.is.female.x + X1.if.senator.is.african.american.x
 #       + X1.if.senator.is.latino.x + factor(Full.Name.x), data=FE_senate_long2)


#col2<- c("duration", "Connectedness115", "Connectedness116",
 #        "LES115", "LES116",
##         "vote.share.in.last.election.x", "First.dimensions.dw.nominate.score.x", 
 #        "number.of.members.in.state.congressional.delegation.x", 
 #        "X1.if.senator.is.a.committee.chair.x", 
 #        "X1.if.senator.is.majority.party.leader.x", "X1.if.senator.is.minority.party.leader.x",
 #        "X1.if.senator.is.female.x","X1.if.senator.is.african.american.x", "X1.if.senator.is.latino.x")

#a<-cor(FE_senate_data3_nonas[col2])



######### TWITTER #########
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

FE_sen_data_T<- merge(x=C_T_115S_LES,y=C_T_116S_LES, by="ID", all.x=TRUE)

#cut out legs who are only one time point (115th or 116th Congress)
#91 senators were repeats 
FE_senate_data1_T<- FE_sen_data_T[!is.na(FE_sen_data_T$Twitter_Connectedness.y),]

FE_senate_data1_T$duration<- 2021-FE_senate_data1_T$year.first.elected.to.the.senate.x

FE_senate_data1$X1.if.senator.is.democrat.x
FE_senate_data1$X1.if.senator.is.female.x
FE_senate_data1$X1.if.senator.is.african.american.x
FE_senate_data1$X1.if.senator.is.latino.x
FE_senate_data1$vote.share.in.last.election.x
FE_senate_data1$First.dimensions.dw.nominate.score.x
FE_senate_data1$number.of.members.in.state.congressional.delegation.x
FE_senate_data1$X1.if.senator.is.a.committee.chair.x
FE_senate_data1$X1.if.senator.is.majority.party.leader.x
FE_senate_data1$X1.if.senator.is.minority.party.leader.x


columns_to_keep<- c("ID", "Full.Name.x", "Party.x", "duration", "Twitter_Connectedness.x", "Twitter_Connectedness.y",
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

#summary(Senate_FE_T)

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


FE_house_data_T<- merge(x=C_T_115H_LES,y=C_T_116H_LES, by="ID", all.x=TRUE)

#cut out legs who are only one time point (115th or 116th Congress)- 333 left
FE_house_data1_T<- FE_house_data_T[!is.na(FE_house_data_T$Connectedness.y),]

FE_house_data1_T$duration<- 2021-FE_house_data1_T$Year.first.elected.to.House.x

#all controls in LES data/ following (Battaglini et al.)
#duration
FE_house_data1$Percent.vote.received.to.enter.this.Congress.x
FE_house_data1$First.dimension.DW.NOMINATE.score.x
FE_house_data1$Size.of.House.delegation.from.member.s.state.y
#party 
FE_house_data1$X1...committee.chair..according.to.Almanac.of.American.Politics.x
FE_house_data1$X1...Majority.party.leadership.x
FE_house_data1$X1...Minority.party.leadership.x
FE_house_data1$X1..Speaker.y
FE_house_data1$X1...female.y
FE_house_data1$X1...African.American.x
FE_house_data1$X1...Latino.a.x

columns_to_keep<- c("ID", "Full.Name.x", "Party.x", "duration", "Connectedness.x.x", "Connectedness.x.y",
                    "Legislative.Effectiveness.Score..1.5.10..x", "Legislative.Effectiveness.Score..1.5.10..y",
                    "Percent.vote.received.to.enter.this.Congress.x", "First.dimension.DW.NOMINATE.score.x", 
                    "Size.of.House.delegation.from.member.s.state.y", 
                    "X1...committee.chair..according.to.Almanac.of.American.Politics.x", 
                    "X1...Majority.party.leadership.x", "X1...Minority.party.leadership.x",
                    "X1...female.y", "X1..Speaker.y",
                    "X1...African.American.x", "X1...Latino.a.x")


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

colnames(FE_house_data3_T)

#no NA's - 333 still! 
FE_house_data3_nonas_T<- na.omit(FE_house_data3_T)

#wide to long
library(reshape2)

FE_house_long2_T<- reshape(FE_house_data3_nonas_T, direction='long', 
                         varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
                         times=c('c115', 'c116'),
                         v.names=c('LES', 'Connectedness'),
                         idvar='Full.Name.x')

House_FE_T<- lm(LES~ Connectedness + Seniority + VoteShare + DWNom+ SizeDel + ComChair+ Party + 
                        MajPLead+ MinPLead+  Female + Speaker+ Black+ Latinx +factor(Full.Name.x)
                        ,data=FE_house_long2_T)

summary(House_FE_T)



library(stargazer)

full_model<- stargazer(House_FE, Senate_FE, House_FE_T, Senate_FE_T, scalebox='0.7')
write(full_model, "regression_FE_Cospo_Twitter.txt")









###################
###################
#POOLED
###################
###################

FE_house_data4<- FE_house_data3_nonas[!FE_house_data3_nonas["Speaker"]]

FE_house_data4 = subset(FE_house_data3_nonas, select = -Speaker)

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
                         idvar='Full.Name.x')


FE_pooled_long_cospo<- lm(LES~ Connectedness + Seniority + Party  + 
                VoteShare+ DWNom + SizeDel + ComChair +  MajPLead + MinPLead +
                        Female + Black + Latinx + factor(Full.Name.x), data=FE_pooled_long)

#summary(m7)

############
############
############

FE_house_data4_T<- FE_house_data3_nonas_T[!FE_house_data3_nonas_T["Speaker"]]

FE_house_data4_T = subset(FE_house_data3_nonas_T, select = -Speaker)

FE_senate_data4_T<- FE_senate_data3_nonas_T 

pooled_house_senate_T<- rbind(FE_house_data4_T,FE_senate_data4_T)


FE_pooled_long_T<- reshape(pooled_house_senate_T, direction='long', 
                         varying=c('LES115', 'LES116', 'Connectedness115', 'Connectedness116'), 
                         times=c('c115', 'c116'),
                         v.names=c('LES', 'Connectedness'),
                         idvar='Full.Name.x')


FE_pooled_long_twitter<- lm(LES~ Connectedness + Seniority + Party  + 
                                  VoteShare+ DWNom + SizeDel + ComChair +  MajPLead + MinPLead +
                                  Female + Black + Latinx + factor(Full.Name.x), data=FE_pooled_long_T)


library(stargazer)
a<-stargazer(FE_pooled_long_cospo, FE_pooled_long_twitter)

write(a, "appendixresultspooled.txt")


#
















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


