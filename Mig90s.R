##
## Project:          Visualizing Feminized International Migration Flows in the 1990s.
## Team:             Leal, Diego F. Ragini S. Malhotra, and Joya Misra.
## Code written by:  Diego F. Leal (www.diegoleal.info)
## Last update:      10/24/18
##

## R session Info for full replication:

# R version 3.5.0 (2018-04-23)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] bindrcpp_0.2.2       sna_2.4              statnet.common_4.1.4 		network_1.13.0.1    
# [5] dplyr_0.7.6          migest_1.7.4         reshape2_1.4.3      
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_0.12.17     lattice_0.20-35  assertthat_0.2.0  grid_3.5.0      plyr_1.8.4      
# [6] R6_2.2.2         magrittr_1.5     coda_0.19-1       pillar_1.2.3    rlang_0.2.1     
# [11] stringi_1.1.7   tools_3.5.0      stringr_1.3.1     glue_1.2.0      purrr_0.2.5     
# [16] compiler_3.5.0  pkgconfig_2.0.1  tidyselect_0.2.4  bindr_0.1.1     tibble_1.4.2 

## clear all
rm(list=ls())

## Call relevant packages
library("reshape2")
library("migest")
library("dplyr")
library("network")
library("sna")

#set working directory where preformatted data are stored
setwd("C:/Users/leald/Dropbox/DLeal/USC/papers/MigWomen90s")

#create a list to store the output data files
edgelists<-list()

#how many countries should be included in the circos plot (Fig. 1)
numberCountries<-52

#for each gender (h == 1 women data; h == 2 men data):
for (h in 1:2) 
{
  
  #import data
  ifelse (h == 1, 
          load("C:/Users/leald/Dropbox/DLeal/USC/papers/MigWomen90s/MigWomenData.RData"),
          load("C:/Users/leald/Dropbox/DLeal/USC/papers/MigWomen90s/MigMenData.RData"))
  
  # input data:  
  #df0 = birth & death data
  #df1 = country and region labels
  #dd  = geographic distance
  #stw = migrant stock data
  
  #decades under analysis 
  decades <- unique(df0$period)
  years<- c("1960", "1970", "1980", "1990", "2000" )
  
  #extract country labels
  iso <- df1$iso3
  
  #set up an empty data.frame (df2) and array (st) to store results
  df2 <- expand.grid(orig = iso, dest = iso, pob=iso, period=decades)
  df2$flow <- NA
  st <- array(stw$stock, c(length(iso),length(iso),length(years)), dimnames=list(pob=iso, por=iso,   time=years))
  
  #offset for orig X dest X pob tables
  g_dist     <- array(NA, c(length(iso),length(iso),length(iso)), dimnames=list(orig=iso, dest=iso, pob=iso))
  g_dist[,,] <- 1/dd
  
  
  # Estimate migration flows using the WB stock data & Abel's (2013) flows from stocks methodology
  
  count_p <-1
  
  s<-Sys.time()                             	    ### keep track of time
  for(p in decades)                  	            ### for each period (1960-1970, 1970-1980, etc.)
  {  
    t0 <- years[count_p]                          ### store t0 in the ith decade (e.g. 1960 if period is 1960-1970)
    t1 <- years[count_p + 1]                      ### store t1 in the ith decade (e.g. 1970 if period is 1960-1970)
    message("t0 = P1 = ", t0)                    	### print t0
    message("t1 = P2 = ", t1)                     ### print t1
    gf <- ffs(                                    ### use fss function, flows from stocks methodology
      P1 = st[,,t0],                              ### P1= stock data at time t (e.g. 1960)
      P2 = st[,,t1],                              ### P2= stock data at t1me + 1 (e.g. 1970)
      b = df0 %>% filter(period==p) %>% .[["b"]], ### births in decade p
      d = df0 %>% filter(period==p) %>% .[["d"]], ### deaths in decade p
      m = g_dist,                                 ### geo distances between all countries
      method = "outside"                          ### this method reproduces the results in Abel (2013)  
    )
    df2$flow[df2$period==p] <- round(c(gf$mu))    ### save estimates in df in column "flow"
    count_p <- count_p + 1                        ### go to migrant stock in next decade   
  }
  
  #Edgelsit: country to country migration flows by decade
  FLOWS.all_edgelist  <-df2 %>% group_by(period, orig, dest) %>% summarise(flow = sum(flow)) %>% mutate(flow = ifelse(orig!=dest, flow, 0))
  
  #create a string to attach the relevant labels to the resulting output data sets
  ifelse (h==1,string <-"women_", string<-"men_")
  
  #from edgelist to array of matrices
  FLOWS.all_matrix  <-array(FLOWS.all_edgelist$flow, c(length(iso),length(iso),length(unique(FLOWS.all_edgelist$period))), dimnames=list(pob=iso, por=iso, time=decades))
  
  #extract migration flows for 1990s in sociomatrix format
  FLOWs.90s_matrix<-t(FLOWS.all_matrix[,,decades[4]])
  
  #flows in 1990s in edgelist format
  edgelist90s<-as.data.frame(subset(FLOWS.all_edgelist,period=="1990-2000"))
  
  #store current edgelist fo flows in the "edgelists" list
  edgelists[[h]]<-edgelist90s
}

#extract women data
women90s <-as.data.frame(edgelists[[1]])
colnames(women90s)[4]<-"women"

#extract men data
men90s <-as.data.frame(edgelists[[2]][,"flow"])
colnames(men90s)<-"men"

#bind women and men data
data<-cbind(women90s,men90s)

#compute total migration (i.e. women + men)
data$total<-data[,"women"]+data[,"men"]

#create an indicator variable: did women migrate in the context of a given dyad?
data$women_flowed<-ifelse(data[,"women"]>0,1,0)

#create an indicator variable: did men migrate in the context of a given dyad?
data$men_flowed<-ifelse(data[,"men"]>0,1,0)

#create an indicator variable: did not women and men migrate in the context of a given dyad? 
data$no_flows<-ifelse(data[,"women_flowed"] + data[,"men_flowed"] == 0,1,0)

#feminization  threshold: based Donato and Gabaccia (2015), in order to consider a given flow 
# to be feminized, 53% of the migrants have to be women
data$threshold<-data[,"total"] * 0.53

#create an indicator variable: does a given flow have more than 500 migrants?  
data$size_threshold<-ifelse(data[,"total"]>500,1,0)

#risk set is composed by the dyadic flows for which the following is true:
# a) the flow has to exist; b) women have to comprise more than 53% of the migrants; and 
# c) flow has to have more than 500 migrants
data$risk_set<-ifelse(data[,"no_flows"]==0 & data[,"women"]>data[,"threshold"] &
                        data[,"size_threshold"]==1,
                      1,0)

#order data showing all dyads in the risk set ranked by their flow size
data<-data[ order(-data[,c("risk_set","total")]), ]

####create the final list of countries to include in the circos plots###

# create an empty vector to store countries of origin
origin<-vector(mode="character")

# create an empty vector to store countries of destination
destination<-vector(mode="character")

# create an empty object to store the final list of (destination and origin) countries
finalList<-NULL

#loop trough all dyads in the object "data"
#extract label of country of origin
#extract label of country of destination
#combine countries of origin with countries of destination. 
#if this list is > than the user-defined "numberCountries" parameter, stop
for (i in 1:nrow(data))
{
  origin[i]  <-as.character(data[i,"orig"])
  destination[i]<-as.character(data[i,"dest"])
  finalList<-unique(c(origin,destination))
  if (length(finalList)>=numberCountries) break
}

#print the finalList
finalList

#remove Israel and West Bank and Gaza due to large number of refugees
remove.labels<-c("h.ISR","h.PSE")
finalList<-finalList[ - which(finalList %in% remove.labels)]

#extract labels
labels.w.regions<-as.vector(women90s[1:length(iso),"dest"])

#extract selected countries' positions to subset the data later on
positions<-vector(length=length(finalList))

w<-0
for (t in 1:length(labels.w.regions))
{
  for (d in 1:length(finalList))
    if(isTRUE(labels.w.regions[t]==finalList[d]))
    {
      w<-w+1  
      positions[w]<-t     
    }
}

#migration of women in the 90s in edgelist form
net<-as.data.frame(women90s[,c("orig","dest","women")])

#from edgelist to sociomatrix
net<-acast(net,orig~dest,variable="women")

#make the sociomatrix an SNA object
net<-as.sociomatrix.sna(net)

#subset the matrix to keep the selected countries
net<-net[positions,positions]

data2<-subset(data,risk_set==1,select=c("orig","dest"))

## create an empty sociomatrix with the top "numberCountries" number of countries
net2<-net
net2[,]<-0

#make net2 be a binary matrix where 1 indicates that countries in a given dyad/cell are inlcuded in net2
for (i in 1:nrow(data2))
{
  orig<-data2[i,1]
  dest<-data2[i,2]
  for (j in 1:ncol(net))
  {
    for (k in 1:nrow(net))
    {
     if(
        (isTRUE(orig == colnames(net2)[k])) && 
        (isTRUE(dest == rownames(net2)[j]))
       )
       {
        net2[k,j]<-1
       }
    }
  }
}

# do an element-wise multiplication between net and net2. Since "net2" is binary, only the
# relevant (i.e. feminized) flows in the original "net" matrix will remain in "net3"
net3<-net*net2

#assign labels
 rownames(net3)<- colnames(net3)<-c("g.ALB","c.ARG","i.AZE","e.BEL",
                                    "g.BLR","c.BRA","a.CAN","j.CHN",
                                    "h.COD","c.COL","e.DEU","e.ESP","e.FRA","e.GBR","h.GIN","b.GUY",
                                    "j.HKG","b.HTI","k.IDN","l.IND","i.IRN","e.ITA","b.JAM","j.JPN",
                                    "i.KAZ","i.KGZ","j.KOR","i.KWT","g.MDA","h.MOZ","k.MYS","l.NPL", "l.PAK",
                                    "c.PER","k.PHL","g.POL","b.PRI","c.PRY","g.ROU","g.RUS","h.RWA", "i.SAU",
                                    "k.SGP","h.SLE","k.THA","b.TTO","j.TWN","g.UKR","a.USA","c.VEN", "h.ZAF")
 

#export final matrix of flows in csv format. These are the data needed to produce the circos plot
write.csv(x=net3,"net.csv", row.names=TRUE)



#assign labels for sociogram
rownames(net3)<- colnames(net3)<-c("ALB","ARG","AZE","BEL",
                                   "BLR","BRA","CAN","CHN",
                                   "COD","COL","DEU","ESP","FRA","GBR","GIN","GUY",
                                   "HKG","HTI","IDN","IND","IRN","ITA","JAM","JPN",
                                   "KAZ","KGZ","KOR","KWT","MDA","MOZ","MYS","NPL","PAK",
                                   "PER","PHL","POL","PRI","PRY","ROU","RUS","RWA","SAU",
                                   "SGP","SLE","THA","TTO","TWN","UKR","USA","VEN","ZAF")

#assign colors for nodes/countries based on geographic region
nodeColors<-c("olivedrab1","khaki1","steelblue1","lightpink",
       "olivedrab1","khaki1","aquamarine1","violet",
       "snow","khaki1","lightpink","lightpink","lightpink","lightpink","snow","khaki1",
       "violet","khaki1","violet","violet","steelblue1","lightpink","khaki1","violet",
       "steelblue1","steelblue1","violet","steelblue1","olivedrab1","snow","violet","violet","violet",
       "khaki1","violet","olivedrab1","khaki1","khaki1","olivedrab1","olivedrab1","snow","steelblue1",
       "violet","snow","violet","khaki1","violet","olivedrab1","aquamarine1","khaki1","snow")

#make nodeColors a matrix object
nodeColors<-as.matrix(nodeColors)
row.names(nodeColors)<-NULL


#from sociomatrix (net3) to edgelist (edges)
edges<-melt(net3)
edges<-subset.data.frame(edges,value>=1)
attr(edges, 'n') = 51

## create a network object based on the resulting edgelist (edges)
graph<-as.network.matrix(edges,matrix.type = "edgelist",vnames=all_IDs,ignore.eval = F)

## calculate out and indegree
odeg<-(degree(graph,cmode="outdegree"))
ideg<-(degree(graph,cmode="indegree"))

## make in and outdegree node-level attirbutes
set.vertex.attribute(graph,"odeg",odeg)
set.vertex.attribute(graph,"ideg",ideg)

## set a seed for random numbers
set.seed(5)

## save node coordinates based on the kamadakawa algorithm
capturedCoordinates<-gplot(graph,gmode="digraph", mode = "kamadakawai",pad = 0.0005)

#open a pdf
pdf("sociogram.pdf",width=15,height = 15) 


## draw a sociogram
gplot(dat=graph,
      gmode="digraph",                               ## directed network
      coord=capturedCoordinates,                     ## use the coordinates computed earlier
      displaylabels = T,                             ## display node-level labels
      pad = 0.0005,                                  ## white space around sociogram
      label.pos=5,                                   ## nodes' labels should be inside the nodes
      edge.col= rgb(red=0,green=0,blue=0,alpha=0.25),## make ties clear grayish
      displayisolates = TRUE,                        ## show isolates
      vertex.cex = ((ideg)/28)+1,                    ## make nodes' size a function of theiir number of ties/flows
      vertex.border="grey",                          ## make nodes' borders be gray
      label.cex = 0.8,                               ## label size
      vertex.col=nodeColors,                         ## nodes' color are a function of geographic region
      label.col="black",                             ## label color
      edge.lwd = ((get.edge.value(graph,"value"))) / 10000 # edge width is a fuinction of flo3 size
)

#close the pdf
dev.off()



