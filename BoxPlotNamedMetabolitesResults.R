options(warn=-1)

infile="http://www.metabolomicsworkbench.org/rest/study/analysis_id/AN000001/datatable/file"

mycol <- 7

library(data.table)
x <-as.data.frame(fread(infile,header=TRUE))
x[,1]<-NULL

n <- sapply(x[1],as.factor)
v <- sapply(x[mycol], as.numeric)

TITLE <- colnames(x)[mycol]
YLAB=""
sgmean <-tapply(v,n,mean,na.rm=TRUE)

colors = matrix(c(0, 2, 3, 4, 5, 6, 7, 8, "rosybrown4", 
                  "orange", "pink", "khaki3", "thistle", "turquoise3", "palegreen1", 
                  "moccasin", "olivedrab3", "azure4", "gold3", "deeppink","gray","red","orange","white"),  ncol = 1)
mymax <- max(sgmean)*1.5

sgsd <-tapply(v,n,sd)
sgp.freq <- table(n)

stderr <- function(x) sqrt(var(na.omit(x))/length(na.omit(x)))
sem <- tapply(v, n, stderr)

barx <- barplot(sgmean,ylim=c(0,mymax),main=TITLE,col=colors,xlab="Group", ylab=YLAB, cex.names=0.5)

error.bar2 <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y, angle=90, code=3, length=length, ...)
}

error.bar2(barx,sgmean,sem)
print("Factors:")
levels(as.factor(n))
