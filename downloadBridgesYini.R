# goal: create a dataset containing vars: state code, lat, long, year built, traffic, traffic safety, feature, condition, bridge costs, 
# check COST, might not have enough obsv
library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)

#get state abbrv
states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")

#create url links to download and read data
dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes) 

B = x16
#data cleaning, remove bad columns and rows
colnames(B)[14] #check the col name @ certain pos

is.na(B) %>% colSums %>% hist(breaks = 100)
is.na(B) %>% colSums %>% hist

#remove columns containing more than 20 missing values
fun = function(x){return(which(x>20))}
(bad = is.na(B) %>% colSums %>% fun)
B = B[, -bad]

