#Load Libraries
library(data.table)
library(ggplot2)
library(knitr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(reshape2)

#Load the Dataset
dataset <- as.data.table(readRDS("/Users/sebastianmontero/Desktop/R Group/solar_dataset.RData"))

#Basic EDA
basic_eda <- function(data){
  dim(data)
  head(data,5)
  str(data)
  summary(data)
}
basic_eda(dataset)

#Check for missing values per column
check_NA <- function(x){
  return(sum(is.na(x)))
}
missings <- dataset[,sapply(.SD,check_NA)]
missings

dataset2 <- dataset[c(1:5113),c(2:98)] #dataset of target variables
dataset3 <- dataset[c(1:5113),c(2:456)]

#Correlation
View(round(cor(dataset2),2))
View(round(cor(dataset3),2))

res2 <- rcorr(as.matrix(dataset3))
res2

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =round((cormat)[ut],3),
    p = round(pmat[ut],3)
  )
}

correlation_dt <- flattenCorrMatrix(res2$r, res2$P)
correlation_dt <- as.data.table(correlation_dt)
View(correlation_dt)

#outliers
install.packages("outliers")
library(outliers)

func_outliers <- function(x){
   for(i in names(x)){
    outliers <- boxplot(as.list(x[,i]), plot=FALSE)$out
    print(sprintf("Outliers in %s:",i))
    print(outliers)
   }
}

func_outliers(dataset3)

#scaling
dataset4 <- as.data.table(sapply(dataset3, scale))
sapply(dataset3, class)
View(dataset4)

#Plot target variable boxplots
my.data <- melt(dataset2, measure.vars=colnames(dataset2))
myplot <- ggplot(data=my.data, aes(x=variable, y=value, fill=variable)) + geom_boxplot() + labs(title="Solar Energy", y="Solar Energy Production", x="Station") + guides(fill="none") + coord_flip() + theme(text = element_text(size=6))
myplot

#Plot mean per column
rownames(mean_data)
mean_data <- as.data.table(dataset2[,sapply(.SD,mean)])
myplo2 <-ggplot(mean_data, aes(x=1:nrow(mean_data),y=V1)) + geom_line() + scale_x_continuous(breaks = seq(1,nrow(mean_data),1),labels=colnames(dataset2)) + labs(title="Mean Solar Energy by Station", y="Solar Energy Production", x="Station") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
myplo2  

#plot top 10 strongest correlations
top10 <- correlation_dt[order(cor, decreasing = T)][1:10]

install.packages("ggpubr")
library(cowplot)

plot_corr1 <- ggplot(dataset3, aes(MARE, STIL)) + geom_point()
plot_corr2 <- ggplot(dataset3, aes(PERK, STIL)) + geom_point()
plot_corr3 <- ggplot(dataset3, aes(MCAL, STUA)) + geom_point()
plot_corr4 <- ggplot(dataset3, aes(MARE, PERK)) + geom_point()
plot_corr5 <- ggplot(dataset3, aes(BYAR, PAUL)) + geom_point()
plot_corr6 <- ggplot(dataset3, aes(HINT, WEAT)) + geom_point()
plot_corr7 <- ggplot(dataset3, aes(REDR, STIL)) + geom_point()
plot_corr8 <- ggplot(dataset3, aes(BESS, WEAT)) + geom_point()
plot_corr9 <- ggplot(dataset3, aes(BREC, LAHO)) + geom_point()
plot_corr10 <- ggplot(dataset3, aes(GUTH, MARE)) + geom_point()
plot_grid(plot_corr1, plot_corr2, plot_corr3, plot_corr4, plot_corr5, plot_corr6, plot_corr7, plot_corr8, plot_corr9, plot_corr10, ncol = 2)
