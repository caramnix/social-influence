rm(list=ls())

setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Congress/116/")

library(network)
library(sna) 
library(readxl)
library(igraph)

H116_cospo <- as.data.frame(read_excel("House/adj_matrix_116_House_Fowler.xlsx"))
H116_leg_info <-  read.csv("House/leginfo_116H.csv")
legs_id_116<- as.list(H116_cospo[,1])
H116_cospo <- H116_cospo[,-1]

rownames(H116_cospo) <-legs_id_116

H116_cospoNet <- graph_from_adjacency_matrix(as.matrix(H116_cospo),diag=F)

#tet<- as.matrix(H116_cospo)

#data <- matrix(sample(0:2, 25, replace=TRUE), nrow=5)

H116_cospoNet<- network(as.matrix(H116_cospo))


H116_cospoNet_colors <- vector(mode = "character", length = dim(H116_leg_info)[1])

for (i in 1:dim(H116_leg_info)[1]){
  if (H116_leg_info$Party[i] == "D") {
    H116_cospoNet_colors[i] <- "darkblue"
  }
  else if (H116_leg_info$Party[i] == "R") {
    H116_cospoNet_colors[i] <- "darkred"
  }
  else{
    H116_cospoNet_colors[i] <- "green"
  }
}
plot(H116_cospoNet,displaylabels=F,vertex.cex=1,vertex.col=H116_cospoNet_colors, edge.col= "lightgrey", usearrows=T,main="Network of Cosponsorships, 116th House",displayisolates=F)




S116_cospo <- as.data.frame(read_excel("Senate/adj_matrix_116_Senate_Fowler.xlsx"))
S116_leg_info <-  read.csv("Senate/leginfo_116S.csv")
legs_id_116S<- as.list(S116_cospo[,1])
S116_cospo <- S116_cospo[,-1]

rownames(S116_cospo) <-legs_id_116S

#H116_cospoNet <- graph_from_adjacency_matrix(as.matrix(S116_cospo))

S116_cospoNet<- network(as.matrix(S116_cospo))


S116_cospoNet_colors <- vector(mode = "character", length = dim(S116_leg_info)[1])

for (i in 1:dim(S116_leg_info)[1]){
  if (S116_leg_info$Party[i] == "D") {
    S116_cospoNet_colors[i] <- "darkblue"
  }
  else if (S116_leg_info$Party[i] == "R") {
    S116_cospoNet_colors[i] <- "darkred"
  }
  else{
    S116_cospoNet_colors[i] <- "green"
  }
}

#plot(S116_cospoNet,displaylabels=F,vertex.cex=1,vertex.col=S116_cospoNet_colors, edge.col= "lightgrey", usearrows=T,main="Network of Cosponsorships, 116th Senate",displayisolates=F)


setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Congress/115/")


H115_cospo <- as.data.frame(read_excel("House/adj_matrix_115_House_Fowler.xlsx"))
H115_leg_info <-  read.csv("House/leginfo_115H.csv")
legs_id_115<- as.list(H115_cospo[,1])
H115_cospo <- H115_cospo[,-1]

rownames(H115_cospo) <-legs_id_115

#H115_cospoNet <- graph_from_adjacency_matrix(as.matrix(H115_cospo))

#tet<- as.matrix(H116_cospo)

#data <- matrix(sample(0:2, 25, replace=TRUE), nrow=5)

H115_cospoNet<- network(as.matrix(H115_cospo))


H115_cospoNet_colors <- vector(mode = "character", length = dim(H115_leg_info)[1])

for (i in 1:dim(H115_leg_info)[1]){
  if (H115_leg_info$Party[i] == "D") {
    H115_cospoNet_colors[i] <- "darkblue"
  }
  else if (H115_leg_info$Party[i] == "R") {
    H115_cospoNet_colors[i] <- "darkred"
  }
  else{
    H115_cospoNet_colors[i] <- "green"
  }
}
#plot(H116_cospoNet,displaylabels=F,vertex.cex=1,
plot(H115_cospoNet,displaylabels=F,vertex.cex=1,vertex.col=H115_cospoNet_colors, edge.col= "lightgrey", usearrows=T,main="Network of Cosponsorships, 115th House",displayisolates=F)



S115_cospo <- as.data.frame(read_excel("Senate/adj_matrix_115_Senate_Fowler.xlsx"))
S115_leg_info <-  read.csv("Senate/leginfo_115S.csv")
legs_id_115S<- as.list(S115_cospo[,1])
S115_cospo <- S115_cospo[,-1]

rownames(S115_cospo) <-legs_id_115S

S115_cospoNet <- graph_from_adjacency_matrix(as.matrix(S115_cospo))

#S116_cospoNet<- network(as.matrix(S116_cospo))


S115_cospoNet_colors <- vector(mode = "character", length = dim(S115_leg_info)[1])

for (i in 1:dim(S115_leg_info)[1]){
  if (S115_leg_info$Party[i] == "D") {
    S115_cospoNet_colors[i] <- "darkblue"
  }
  else if (S115_leg_info$Party[i] == "R") {
    S115_cospoNet_colors[i] <- "darkred"
  }
  else{
    S115_cospoNet_colors[i] <- "green"
  }
}



par(mfrow=c(2,2))
plot(H116_cospoNet,displaylabels=F,vertex.cex=1,vertex.col=H116_cospoNet_colors, edge.col= "lightgrey", usearrows=T,main="Network of Cosponsorships, 116th House",displayisolates=F)
plot(H115_cospoNet,displaylabels=F,vertex.cex=1,vertex.col=H115_cospoNet_colors, edge.col= "lightgrey", usearrows=T,main="Network of Cosponsorships, 115th House",displayisolates=F)
plot(S116_cospoNet,displaylabels=F,vertex.cex=1,vertex.col=S116_cospoNet_colors, edge.col= "lightgrey", usearrows=T,main="Network of Cosponsorships, 116th Senate",displayisolates=F)
plot(S115_cospoNet,displaylabels=F,vertex.cex=1,vertex.col=S115_cospoNet_colors, edge.col= "lightgrey", usearrows=T,main="Network of Cosponsorships, 115th Senate",displayisolates=F)






##### twitter 

H116_twitter <- as.data.frame(read.csv("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Twitter/116/House/adj_mat_Twitter_House_116.csv"), header=TRUE )
setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data/Congress/116/")
H116_leg_info <-  read.csv("House/leginfo_116H.csv")
legs_id_116<- as.list(H116_twitter[,1])
H116_twitter <- H116_twitter[,-1]


rownames(H116_twitter) <-legs_id_116
names(H116_twitter) <- sub("^X", "", names(H116_twitter))

H116_twitter<- as.matrix(H116_twitter)

diag(H116_twitter) <- 0

#H116_twitter_net <- graph_from_adjacency_matrix(as.matrix(H116_twitter),diag=F)

#tet<- as.matrix(H116_cospo)

#data <- matrix(sample(0:2, 25, replace=TRUE), nrow=5)

H116_twitter_net<- network(H116_twitter)

leg_id_list<- unlist(legs_id_116)

H116_tNet_colors <- vector(mode = "character", length =length(leg_id_list))

#H116_leg_info$ID.loc(leg_id_list[1])

for (i in 1:length(leg_id_list)){
  ind<- which(H116_leg_info == leg_id_list[i])[1]
  party<- H116_leg_info$Party[ind]
  
  if (party == "D") {
    H116_tNet_colors[i] <- "darkblue"
  }
  else if (party == "R") {
    H116_tNet_colors[i] <- "darkred"
  }
  else{
    H116_tNet_colors[i] <- "green"
  }
}
plot(H116_twitter_net,displaylabels=F,vertex.cex=1,vertex.col=H116_tNet_colors, edge.col= "lightgrey", usearrows=T,main="Network of Twitter Interactions, 116th House",displayisolates=F)

