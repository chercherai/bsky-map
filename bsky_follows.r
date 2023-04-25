library(tidyverse)
library(DBI)
#library(igraph)
library(fastcluster)
source("~/Downloads/FastPCA.R")
library(Matrix)
library(Rtsne)
library(parsedate)
library(colorspace)
library(ggthemes)
#library(ghibli)
library(scales)
library(gplots)
library(viridis)

setwd("~/Desktop/nonduality/bsky")
datestring = format(Sys.time(), "%Y%m%d")
dir.create(datestring)
setwd(datestring)

# get from https://huggingface.co/datasets/andrewconner/bluesky_profiles/resolve/main/bsky.db

mydb <- dbConnect(RSQLite::SQLite(), "~/Downloads/bsky.db")

dbListTables(mydb)
#accounts accounts_pagerank follows
#dbGetQuery(mydb, 'SELECT * FROM accounts LIMIT 5')

dbListFields(mydb,"accounts")
#[1] "did"                "handle"             "displayName"        "indexedAt"         
#[5] "followsCount"       "followersCount"     "isDetailed"         "profileLastUpdated"
#[9] "graphLastUpdated"   "postsCount"         "description"

dbListFields(mydb,"accounts_pagerank")
#[1] "did"         "pagerank"    "lastUpdated"

dbListFields(mydb,"follows")
#[1] "sourceDid" "targetDid" "createdAt"

#folfol = dbGetQuery(mydb, 'SELECT did,followsCount,followersCount FROM accounts') %>% column_to_rownames("did")
#plot(folfol)

follows = dbGetQuery(mydb, 'SELECT * FROM follows')
follows$follow = 1

allpage = dbGetQuery(mydb, 'SELECT did,pagerank FROM accounts_pagerank')
rownames(allpage) = allpage$did
allhand = dbGetQuery(mydb, 'SELECT did,handle,postsCount FROM accounts')
rownames(allhand) = allhand$did

#this reduces dimension based on follower similarity (who each person follows)
#rows = follower
#cols = followee
# followsMat <- with(follows, sparseMatrix(i=as.numeric(as.factor(sourceDid)), 
#                              j=as.numeric(as.factor(targetDid)), 
#                              x=as.numeric(follow),
#                              dimnames=list(levels(as.factor(sourceDid)), levels(as.factor(targetDid)))))
# 
# 
# #memory limit reached with full matrix
# folpca = FastPCA(followsMat[,1:10000], 30)
# folpca1 = FastPCA(followsMat[,10001:20000], 30)
# folpca2 = FastPCA(followsMat[,20001:ncol(followsMat)], 30)
# 
# folpca3 = FastPCA(as.matrix(cbind(folpca$x,folpca1$x,folpca2$x)), 30)
# 
# foltsne = Rtsne(folpca3$x, dims = 2, check_duplicates = F, pca = FALSE, verbose = TRUE, max_iter = 1000)
# plot(foltsne$Y,pch='.')

#this reduces dimension based on followee similarity (who follows each person)
#rows = followee
#cols = follower
followeMat <- with(follows, sparseMatrix(i=as.numeric(as.factor(targetDid)), 
                                         j=as.numeric(as.factor(sourceDid)), 
                                         x=as.numeric(follow),
                                         dimnames=list(levels(as.factor(targetDid)), levels(as.factor(sourceDid)))))


# #memory limit reached with full matrix
# folepca = FastPCA(followeMat[,1:10000], 30)
# folepca1 = FastPCA(followeMat[,10001:20000], 30)
# folepca2 = FastPCA(followeMat[,20001:ncol(followeMat)], 30)
# 
# folepca3 = FastPCA(as.matrix(cbind(folepca$x,folepca1$x,folepca2$x)), 30)
# 
# foletsne = Rtsne(folepca3$x, dims = 2, check_duplicates = F, pca = FALSE, verbose = TRUE, max_iter = 1000)
# plot(foletsne$Y,pch='.')



#this reduces dimension based on mutuals
#rows = followee
#cols = follower

hasboth = intersect(rownames(followeMat),colnames(followeMat))
bothmat = followeMat[hasboth,hasboth]
# mutuals get score = 2
mutmat = bothmat + t(bothmat)
# mutmat[mutmat<2] = 0 #out of memory
#mutmat = (mutmat>1) + 0
mutmat = mutmat[rowSums(mutmat)>0,colSums(mutmat)>0]

# 
# #memory limit reached with full matrix
# mutpca = FastPCA(mutmat[,1:10000], 30)
# mutpca1 = FastPCA(mutmat[,10001:ncol(mutmat)], 30)
# 
# mutpca3 = FastPCA(as.matrix(cbind(mutpca$x,mutpca1$x)), 30)
# 
# muttsne = Rtsne(mutpca3$x, dims = 2, check_duplicates = F, pca = FALSE, verbose = TRUE, max_iter = 1000)
# posts = dbGetQuery(mydb, 'SELECT postsCount FROM accounts WHERE did IN (:x)',   params = list(x = rownames(mutmat)))
# 
# plot(muttsne$Y,pch=21,cex=0.1*sqrt(rowSums(mutmat)))
# plot(muttsne$Y,pch=21,cex=0.1*sqrt(posts$postsCount))
# plot(rowSums(mutmat),posts$postsCount,pch='.',log='xy')
# 
# mostmuts = names(tail(sort(rowSums(mutmat)),50))
# mostmuts.info = dbGetQuery(mydb, 'SELECT * FROM accounts WHERE did IN (:x)',   params = list(x = mostmuts))

## 10k accounts with 10 or more mutuals
mutmat2 = mutmat[rowSums(mutmat)>0,colSums(mutmat)>0]
cut2 = as.logical(round(runif(ncol(mutmat2))))
mutpca2a = FastPCA(mutmat2[,cut2], 50)
mutpca2b = FastPCA(mutmat2[,!cut2], 50)
mutpca3 = FastPCA(as.matrix(cbind(mutpca2a$x,mutpca2b$x)),50)
muttsne2 = Rtsne(mutpca3$x, dims = 2, check_duplicates = F, pca = FALSE, verbose = TRUE, max_iter = 2500)

mutmap = muttsne2$Y
rownames(mutmap) = rownames(mutmat2)
px = rownames(mutmap)

#posts2 = dbGetQuery(mydb, 'SELECT postsCount FROM accounts WHERE did IN (:x)',   params = list(x = rownames(mutmat2)))
#desc2 = dbGetQuery(mydb, 'SELECT description FROM accounts WHERE did IN (:x)',   params = list(x = rownames(mutmat2)))

pdf(file="mutuals_tsne_testing.pdf",width=24,height=24)
plot(mutmap,pch=21,cex=.01*allhand[px,"postsCount"]^.5)
plot(mutmap,pch=21,cex=.01*rowSums(mutmat2)^.5)
plot(mutmap,pch=21,cex=20*sqrt(allpage$pagerank))

#plot(muttsne2$Y,pch=19, col='blue', cex=1000*page2[rownames(mutmat2),"pagerank"])
plot(mutmap,pch=19, col='blue', cex=.1*allhand[px,"postsCount"]^.5)
text(mutmap,labels = allhand[px,"handle"],cex=0.2,col = 'red')
dev.off()

### cluster testing

muttree = hclust(dist(mutmap),method="average")
dir.create("treeheight")

png(file="./treeheight/tree%06d.png",width=1000,height=1000)
for(i in 1:36) {
print(i)
mutclus = cutree(muttree,k = i)
mutcols = qualitative_hcl(i)[mutclus]
names(mutcols) = rownames(mutmap)

par(bg="black")
plot(mutmap, pch=19, col=mutcols, cex=0.1*rowSums(mutmat2 > 1)^.5, col.main="white",main=paste0(i, " clusters"))

}
dev.off()

system("~/Downloads/ffmpeg -y -r 1 -f image2 -s 1000x1000 -i ~/Desktop/nonduality/bsky/treeheight/tree%06d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p treeheight.mp4")
system("open treeheight.mp4")

#choose
nclus=36





png(file="mutual_tsne_pretty.png",width=1000,height=1000)
k = rownames(mutmap)

par(bg = "black")
plot(mutmap,cex=0,axes=FALSE, xaxt='n', yaxt='n', ann=FALSE, bty='n')
points(mutmap[k,], pch=19, col=mutrankcol[k], cex=0.1*follsize[k]^.5)
title(main="BLUESKY MAP 4/23/23", col.main="white", cex.main=3,line=-3)
#taglocs = mutmap[c(346,1255,1647,2076,2162,3625,4439,4500),]
#text(taglocs,labels=c("KOREA", "PERSIA", "BRASIL", "OG BSKY", "DEVS", "ARTWORLD", "JAPAN","TPOT"),col="white",cex=3)

dev.off()

#indicator accounts
indacc = list(
  "KOREA" = "eggry.bsky.social",
  "PERSIA" = "navidm.net",
  "BRASIL" = "leogo.cloud",
  "DEVS" = "jay.bsky.team",
  "ARTWORLD" = "mxvoid.com",
  "JAPAN" = "a-a.bsky.social",
  "TPOT" = "brooke.vibe.camp",
  "SWIFTIES" = "evermorer.bsky.social"
              )

inddid = allhand$did[match(indacc,allhand$handle)]
#taglocs = mutmap[c(346,1255,1647,2076,2162,3625,4439,4500),]


pdf(file="mutual_tsne_followers.pdf",width=72,height=72)

par(bg = "black")
plot(mutmap,cex=0,axes=FALSE, xaxt='n', yaxt='n', ann=FALSE, bty='n')
#points(mutmap[k,], pch=19, col=mutrankcol[k], cex=rescale((allpage[k,"pagerank"])^.5,to=c(1,10)))
text(mutmap[inddid,],labels=names(indacc),col="grey",cex=10)
title(main="BLUESKY FOLLOWER COUNT 4/22/23", col.main="white", cex.main=5,line=-1)
points(mutmap[k,], pch=19, col=mutrankcol[k], cex=rescale(0.1*follsize[k]^.5,to=c(1,10)))
text(mutmap[k,],labels = allhand[k,"handle"],cex=0.1,col = 'white')

dev.off()


# ~/Downloads/ffmpeg -r 60 -f image2 -s 1000x1000 -i ~/Desktop/nonduality/bsky/mutmap%06d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p test1.mp4
# ~/Downloads/ffmpeg -r 60 -f image2 -s 1000x1000 -i ~/Desktop/nonduality/bsky/mutmap2%06d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p test2.mp4
# ~/Downloads/ffmpeg -f concat -safe 0 -i myfiles.txt -c copy output.mp4
# 
# ~/Downloads/ffmpeg -r 24 -f image2 -s 1000x1000 -i ~/Desktop/nonduality/bsky/mutmap3%06d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p test3.mp4

# so the order of addition was basically
# 
# JAN: DEVS
# FEB: OG BSKY
# MAR 02: BRASIL
# MAR 03: JAPAN
# MAR 30: PERSIA
# APR 09: TPOT
# APR 12: KOREA
# APR 18: ARTWORLD


## clusters inter-follow matrix
foll2mut = foll2mat[follint,follint]
clusmat = foll2mut[intersect(rownames(foll2mut),names(mutclus)),intersect(rownames(foll2mut),names(mutclus))]
clusmat = clusmat + t(clusmat)
#clusmat = (clusmat > 0) + 0
tranmat = matrix(data=NA, nrow=length(unique(mutclus)), ncol=length(unique(mutclus)))
tranclus = mutclus[rownames(clusmat)]

for(clus_i in unique(tranclus)) {
  print(clus_i)
  for(clus_j in unique(tranclus)) {
    tmp_clus = clusmat[tranclus==clus_i, tranclus==clus_j]
    tranmat[clus_i, clus_j] = sum(tmp_clus==2)/(nrow(tmp_clus)+ncol(tmp_clus))
  }
}

pdf(file="mutual_crossover.pdf",width=24,height=24)
par(bg="black")
heatmap.2(tranmat^.5,
          RowSideColors = qualitative_hcl(nclus)[1:nclus],
          ColSideColors = qualitative_hcl(nclus)[1:nclus],
          trace="none",legend=NULL,key=FALSE,
          col=viridis,
          scale="none")
dev.off()



## cluster-crossers

# require(data.table)
# require(Matrix.utils)
library(vegan)

## SHANNON diversity of mutuals
foll2mut = foll2mat[follint,follint]
clusmat = foll2mut[intersect(rownames(foll2mut),names(mutclus)),intersect(rownames(foll2mut),names(mutclus))]
clusmat = clusmat + t(clusmat)

clusagg = aggregate(as.matrix(clusmat), by=list(tranclus), FUN=function(x) sum(x > 1))
rownames(clusagg) = clusagg$Group.1
clusagg$Group.1 = NULL
clusdiv = diversity(t(clusagg),index = "shannon")

k = rownames(mutmap)

pdf(file="cluster_shannon_mutual.pdf",width=36,height=36)

par(bg = "black")
plot(mutmap,cex=0,axes=FALSE, xaxt='n', yaxt='n', ann=FALSE, bty='n')
points(mutmap[k,], pch=19, col=mutrankcol[k], cex=rescale((clusdiv[k]),to=c(1,8)))
title(main="BLUESKY MUTUAL DIVERSITY 4/23/23", col.main="white", cex.main=3,line=-1)
taglocs = mutmap[c(346,1255,1647,2076,2162,3625,4439,4500),]
text(taglocs,labels=c("KOREA", "PERSIA", "BRASIL", "OG BSKY", "DEVS", "ARTWORLD", "JAPAN","TPOT"),col="white",cex=3)
text(mutmap[k,],labels = accinfo[k,"handle"],cex=0.2,col = 'white')

dev.off()


mutrank = rank(firstfollow$date)
mutrankcol = qualitative_hcl(nrow(mutmap))[mutrank]
names(mutrankcol) = rownames(mutmap)

clusdivrank = rank(clusdiv)
clusdivrankcol = qualitative_hcl(nrow(mutmap))[clusdivrank]
names(clusdivrankcol) = names(clusdiv)

pdf(file="cluster_shannon_mutual_alt.pdf",width=36,height=36)

par(bg = "black")
plot(mutmap,cex=0,axes=FALSE, xaxt='n', yaxt='n', ann=FALSE, bty='n')
points(mutmap[k,], pch=19, col=clusdivrankcol[k], cex=rescale((firstfollow[k,"yd"]),to=c(8,1)))
title(main="BLUESKY MUTUAL DIVERSITY 4/23/23", col.main="white", cex.main=3,line=-1)
taglocs = mutmap[c(346,1255,1647,2076,2162,3625,4439,4500),]
text(taglocs,labels=c("KOREA", "PERSIA", "BRASIL", "OG BSKY", "DEVS", "ARTWORLD", "JAPAN","TPOT"),col="white",cex=3)
text(mutmap[k,],labels = accinfo[k,"handle"],cex=0.2,col = 'white')

dev.off()



### SHANNON vs onboarding

plot(x=firstfollow[k,"date"],y=clusdiv[k],pch='.',xlab="Date of first follow",ylab="Shannon index")
### SHANNON vs follows+followers
j= intersect(k,rownames(clusmat))
plot(x=rowSums( (clusmat[j,j]+t(clusmat[j,j])) > 0),y=clusdiv[j],log='x',pch='.',xlab="Follows + Followers",ylab="Shannon index")




## SHANNON diversity of follows

foll2mut = foll2mat[follint,follint]
clusmat = foll2mut[intersect(rownames(foll2mut),names(mutclus)),intersect(rownames(foll2mut),names(mutclus))]

clusagg = aggregate(as.matrix(clusmat), by=list(tranclus), FUN=function(x) sum(x > 0))
rownames(clusagg) = clusagg$Group.1
clusagg$Group.1 = NULL
clusdiv = diversity(t(clusagg),index = "shannon")

k = rownames(mutmap)

pdf(file="cluster_shannon_follows.pdf",width=36,height=36)
par(bg = "black")
plot(mutmap,cex=0,axes=FALSE, xaxt='n', yaxt='n', ann=FALSE, bty='n')
points(mutmap[k,], pch=19, col=mutrankcol[k], cex=rescale((clusdiv[k]),to=c(1,8)))
title(main="BLUESKY FOLLOW DIVERSITY 4/23/23", col.main="white", cex.main=3,line=-1)
taglocs = mutmap[c(346,1255,1647,2076,2162,3625,4439,4500),]
text(taglocs,labels=c("KOREA", "PERSIA", "BRASIL", "OG BSKY", "DEVS", "ARTWORLD", "JAPAN","TPOT"),col="white",cex=3)
text(mutmap[k,],labels = accinfo[k,"handle"],cex=0.2,col = 'white')

dev.off()



## SHANNON diversity of followers

foll2mut = foll2mat[follint,follint]
clusmat = foll2mut[intersect(rownames(foll2mut),names(mutclus)),intersect(rownames(foll2mut),names(mutclus))]

clusagg = aggregate(t(as.matrix(clusmat)), by=list(tranclus), FUN=function(x) sum(x > 0))
rownames(clusagg) = clusagg$Group.1
clusagg$Group.1 = NULL
clusdiv = diversity(t(clusagg),index = "shannon")

k = rownames(mutmap)

pdf(file="cluster_shannon_followers.pdf",width=36,height=36)
par(bg = "black")
plot(mutmap,cex=0,axes=FALSE, xaxt='n', yaxt='n', ann=FALSE, bty='n')
points(mutmap[k,], pch=19, col=mutrankcol[k], cex=rescale((clusdiv[k]),to=c(1,8)))
title(main="BLUESKY FOLLOW DIVERSITY 4/23/23", col.main="white", cex.main=3,line=-1)
taglocs = mutmap[c(346,1255,1647,2076,2162,3625,4439,4500),]
text(taglocs,labels=c("KOREA", "PERSIA", "BRASIL", "OG BSKY", "DEVS", "ARTWORLD", "JAPAN","TPOT"),col="white",cex=3)
text(mutmap[k,],labels = accinfo[k,"handle"],cex=0.2,col = 'white')

dev.off()


#OUTPUT TO APPS
output = as.data.frame(mutmap)
colnames(output) = c("x","y")
output$cluster = mutclus[k]
output$handle = allhand[k,"handle"]
output$postsCount = allhand[k,"postsCount"]
output$color_onboarding = mutrankcol[k]
output$color_clus36 = mutcols[k]
output$pagerank = allpage[k,"pagerank"]
output$pagerank_resize = rescale(output$pagerank^.5,to=c(1,10))
output$postsCount_resize = rescale(output$postsCount^.5, to=c(1,10))

alldesc = dbGetQuery(mydb, 'SELECT did,description FROM accounts')
rownames(alldesc) = alldesc$did

output$desc = alldesc[k,"description"]
output$desc = gsub("\n","",output$desc)

## for import to js
outputjs = output
outputjs$postsCount[is.na(outputjs$postsCount)] = 0
outputjs$postsCount_resize[is.na(outputjs$postsCount_resize)] = 0
outputjs$pagerank[is.na(outputjs$pagerank)] = min(outputjs$pagerank,na.rm=T)
outputjs$pagerank_resize[is.na(outputjs$pagerank_resize)] = min(outputjs$pagerank_resize,na.rm=T)
outputjs$desc[is.na(outputjs$desc)] = ""

write.table(outputjs,file="bsky.tsv",sep="\t",quote=TRUE)





#### follower timeline
# takes a long time to make

accfollow = dbGetQuery(mydb, 'SELECT * FROM follows')
accfollow$date = parse_date(accfollow$createdAt)
accfollow$follow = 1
accfollow = accfollow %>% arrange(date)
accfollow$year = as.numeric(format(as.Date(accfollow$date), "%Y"))
accfollow = accfollow %>% filter(year >= 2022)
accfollow$doy = as.numeric(format(as.Date(accfollow$date), "%j"))
accfollow$hour = as.numeric(format(as.POSIXct(accfollow$date), "%H"))

accfollow$yd = accfollow$year + accfollow$doy/365 + accfollow$hour/24/365

firstfollow = accfollow[!duplicated(accfollow$sourceDid),]
rownames(firstfollow) = firstfollow$sourceDid
firstfollow = firstfollow[rownames(mutmap),]

mutrank = rank(firstfollow$date)
mutrankcol = qualitative_hcl(nrow(mutmap))[mutrank]
names(mutrankcol) = rownames(mutmap)

# color key for start dates
pickranks = seq(1,nrow(firstfollow),round(nrow(firstfollow)/24))
pickdids = rownames(firstfollow)[order(firstfollow$date)][pickranks]
plot(pickranks,pickranks,cex=0)
text(pickranks,pickranks,col=mutrankcol[pickdids],labels=firstfollow[order(firstfollow$date),"date"][pickranks])

# mutpage = allpage[rownames(mutmap),]


# to watch it double every second = 20 second clip
# at 24 fps thats
n = log(nrow(accfollow))/log(20*24)
frames = round((1:480)^n)

dir.create("growthmovie")
png(file="./growthmovie/growth%06d.png",width=1000,height=1000)
for(i in frames) {
  # if(i %% 500 != 0) next
  yd = accfollow$yd[i]
  print(yd)
  foll2date = accfollow[accfollow$yd <= yd,]
  foll2mat = with(foll2date, sparseMatrix(i=as.numeric(as.factor(targetDid)), 
                                          j=as.numeric(as.factor(sourceDid)), 
                                          x=as.numeric(follow),
                                          dimnames=list(levels(as.factor(targetDid)), levels(as.factor(sourceDid)))))
  follint = intersect(rownames(foll2mat),colnames(foll2mat))
  follint = intersect(follint,rownames(mutmap))
  foll2mut = foll2mat[follint,follint]
  foll2mut = foll2mut + t(foll2mut)
  follsize = rowSums(foll2mut > 1)
  par(bg = "black")
  plot(mutmap,cex=0,axes=FALSE, xaxt='n', yaxt='n', ann=FALSE, bty='n')
  points(mutmap[follint,], pch=19, col=mutrankcol[follint], cex=0.1*follsize^.5)
  title(main=accfollow$date[accfollow$yd %in% yd][1], col.main="white")
  
}

dev.off()

system("
       ~/Downloads/ffmpeg -y -r 24 -f image2 -s 1000x1000 -i ./growthmovie/growth%06d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p growth.mp4
       ")
system("
       open growth.mp4
       ")

# testcol = rgb( 
#   rescale(mutmap[,1],to=c(0,1)),
#   rescale(mutmap[,2],to=c(0,1)),
#   rescale(1/(mutmap[,1]+mutmap[,2]),to=c(0.5,1))
# )
# names(testcol) = rownames(mutmap)



