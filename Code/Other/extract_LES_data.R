
#code to subset LES data 
#written 3.2.22 by @caramnix 

setwd("/Users/caranix/Library/CloudStorage/OneDrive-TheOhioStateUniversity/Social Influence/Data")

library("readxl")

housedatn= "CELHouse93to116.xlsx"
sendatn ="CELSenate93to116.xlsx"


housedat <- read_excel(housedatn)
housedat115<- housedat[which(housedat$`Congress number` == '115'),]
housedat116<- housedat[which(housedat$`Congress number` == '116'),]

senatedat <- read_excel(sendatn)
senatedat115<- senatedat[which(senatedat$`congress number` == '115'),]
senatedat116<- senatedat[which(senatedat$`congress number` == '116'),]


write.csv(housedat115,"LEShousedat115.csv", row.names = FALSE)
write.csv(housedat116,"LEShousedat116.csv", row.names = FALSE)
write.csv(senatedat115,"LESsenatedat115.csv", row.names = FALSE)
write.csv(senatedat116,"LESsenatedat116.csv", row.names = FALSE)


