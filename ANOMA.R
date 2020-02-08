library(GA)
library(dbscan)
library(dplyr)
library(optparse)

option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="CSV file of data to detect outliers from", metavar="character"),
  make_option(c("-h", "--header"), type="character", default="T", 
              help="does the CSV file have a header? T for True, F for False", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

normalize <- function(x) {
  return ((x - mean(x)) / (sd(x)))
}

euc.dist <- function(x1, x2){
  sqrt(sum((x1 - x2)^2))
} 

wcss <- function(df_1,db_1){
  #map clusters to data
  df_1$cluster <- db_1$cluster
  clust_no <- unique(df_1$cluster)
  
  ss <- 0
  
  for(i in 1:length(clust_no)){
    col_mean <- colMeans(df_1[df_1$cluster == i,])
    df_sub <- df_1[df_1$cluster == i,]
    for(j in 1:length(df_sub)){
      ss <- ss + euc.dist(df_sub[j,],col_mean)
    }
  }
  
  ss
}


df <- read.csv(opt$file,header = opt$header)

#automatically remove columns that aren't numeric
df <- select_if(df, is.numeric)

#compute WCSS
f <- function(x1,x2){
  db <- dbscan::dbscan(df,eps = x1,minPts =ceiling(x2))
  if(max(db$cluster) == 0){
    1000
  } else {
  wcss(df,db)}
}

#normalize data
for(i in names(df)){
  df[,i] <- normalize(df[,i])
}
plot(df)
#Search the solution space using a genetic algorithm
# function should be to maximize the cluster density

GA <- ga(type = "real-valued", fitness = function(x) -f(x[1],x[2]),
         lower = c(1,3), upper = c(10,20), pmutation = 0.5,
         popSize = 10, maxiter = 1000, run = 50)
summary(GA)
plot(GA)
db <- dbscan::dbscan(df,eps = GA@solution[1,1],minPts =ceiling(GA@solution[1,2]))