#Set directory path for your environment 

setwd("~/Text-Analysis")

input.dir <- "data/shakespeareFolger"
files.v <- dir(input.dir, "\\.txt$")


text.make.file.word.l<-function(text.filename){
  
  text.v<-scan(paste(input.dir, text.filename, sep="/"), what="character", sep="\n")
  text.v<-paste(text.v, collapse=" ")
  
  text.words.v<-strsplit(text.v, "\\W")
  text.word.v<-unlist(text.words.v)
  
  text.lower.v<-tolower(text.word.v)
  
  text.freqs.t<-table(text.lower.v[which(text.lower.v!="")])
  
  text.freqs.rel.t<-100*(text.freqs.t/sum(text.freqs.t))
  return(text.freqs.rel.t)
}

text.freqs.l <- list()
for(i in 1:length(files.v)) {
  doc.object<-paste(files.v[i])
  worddata <- text.make.file.word.l(doc.object) #function call 
  text.freqs.l[[files.v[i]]] <- worddata
}



#Verify values returned to `text.freqs.l`
class(text.freqs.l)
names(text.freqs.l)
str(text.freqs.l)

#To begin author attribution convert list `text.freqs.l` into Data Matrix using "mapply"
freqs.l  <-  mapply(data.frame, ID=seq_along(text.freqs.l), text.freqs.l, SIMPLIFY=FALSE, MoreArgs=list(stringsAsFactors=FALSE))

#Verify class of `freqs.l`
class(freqs.l[[1]])
#Display first ten items inside first list item
freqs.l[[1]][1:10,]
#Search lower to begin to see actual words
freqs.l[[1]][100:110,]

#Bind data in all the lists to a single, three-column `data.frame`
freqs.df <- do.call(rbind,freqs.l)

#Convert long form table to wide format using "xtabs" (for creating "contigency tables" or "cross tabulations")
result <- xtabs(Freq ~ ID+Var1, data=freqs.df)
#Check size of `data.frame` (rows = texts, columns = words)
dim(result)
#Display short fragment of the `data.frame`
colnames(result)[100:200]

#To perform attribution analysis transform "xtabs table" to "numberical matrix"
class(result)
final.m <- apply(result, 2, as.numeric)

#Create Cluster Dendrogram to see authors closest to "anonymous.xml"
#descrease data size by setting threshold at 0.25%)
smaller.m <- final.m[,apply(final.m,2,mean)>=.25]
#Create a distance object
dm <- dist(smaller.m)

#Perform a cluster analysis on the distance object 
cluster <- hclust(dm)
#Get the text file names to use as labels
cluster$labels <- names(text.freqs.l)
#Plot the result as a dendrogram

hcd = as.dendrogram(cluster)
labelColors = c("darkred", "darkorange", "gold", "darkgreen", "darkblue", "orchid", "pink", "mediumvioletred")
# cut dendrogram in 4 clusters
clusMember = cutree(cluster, 8)
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}
# using dendrapply
clusDendro = dendrapply(hcd, colLab)
par(mar = c(4,8,4,11))
# make plot
plot(clusDendro, main = "Shakespeare Plays", type = "rectangle", horiz = TRUE)
