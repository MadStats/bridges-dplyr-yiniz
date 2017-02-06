# goal: create a dataset containing vars: state code, lat, long, year built, traffic, traffic safety, feature, condition, bridge costs, 
# check COST, might not have enough obsv
# 1) investigate national bridges, by levels of ownership, condition, year built, daily traffice, history, on bridge service, 
# 2) investigate the costs, improvement, results

library(tidyverse)
library(plyr)
library(choroplethr)
library(data.table)

#get state abbrv
# states= read.delim("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt", header = FALSE, sep = ",")
# #read.delim: into a data frame

states = read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt", col_names = c("stateFull", "stateAbbr"))
# read_csv: into a tibble
# read_tsv() reads tab delimited files, and read_delim() reads in files with any delimiter.

states=states[-(1:13),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
(states)

#create url links to download and read data
dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
#read data into x16
x16 = ldply(dest, fread) 
str(x16)
#ldply: For each element of a list, apply function then combine results into a data frame.
save(x16, file = "x16.RData") #store in R for later easy access and reuse
#load(x16.RData)

B = x16

#data cleaning, remove bad columns and rows
colnames(B)[14] #check the col name @ certain pos
#see and select variables of interest to me
colnames(B)
keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008", "LAT_016", "LONG_017", "TOLL_020", "OWNER_022", "YEAR_BUILT_027", 
         "ADT_029", "APPR_RAIL_END_036D", "HISTORY_037", "SERVICE_ON_042A", "STRUCTURE_KIND_043A", 
         "STRUCTURE_LEN_MT_049", "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060", "WORK_PROPOSED_075A", "WORK_DONE_BY_075B", "IMP_LEN_MT_076", 
         "BRIDGE_IMP_COST_094", "ROADWAY_IMP_COST_095", "TOTAL_IMP_COST_096")

B1 = select(B, one_of(keep))
save(B1, file = "B1.RData")
load("B1.RData")

#plot national bridges
NatnlB = select(B1, STATE_CODE_001:SUBSTRUCTURE_COND_060)
summary(NatnlB$LAT_016)
summary(NatnlB$LONG_017)

#according to us census Extent: (-124.848974, 24.396308) - (-66.885444, 49.384358), 
#https://www.quora.com/What-is-the-longitude-and-latitude-of-a-bounding-box-around-the-continental-United-States
#mtguide: lat 8 digits, long 9 digits
NatnlB = filter (NatnlB, LAT_016 > 23e+06 & LAT_016 < 51e+06 & LONG_017 > 63e+06 & LONG_017 < 126e+06)

conv = function (x){
 round((x%/%1e+06 + (x%%1e+06)/60e+06), digits = 6) %>%return
}
conv(90110000)
NatnlB = mutate(NatnlB, lat = conv(LAT_016), long = conv(LONG_017)) 
summary(NatnlB$lat)
summary(NatnlB$long)
ggplot(data = NatnlB) + geom_point(mapping = aes(y = lat, x = long), size = 0.3)
str(NatnlB$TOLL_020)
NatnlB$TOLL_020 = as.factor(NatnlB$TOLL_020)
ggplot(data = NatnlB) + geom_point(mapping = aes(y = lat, x = long, col =TOLL_020), size = 0.3)
#why rounding the data? data points are not granular

### read california bridges
url = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/CA16.txt"
ca = fread (url)
# can do subset of NatnlB
is.na(ca) %>% colSums %>% hist
is.na(ca) %>% rowSums %>% hist
keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008", "LAT_016", "LONG_017", "TOLL_020", "OWNER_022", "YEAR_BUILT_027", 
         "ADT_029", "APPR_RAIL_END_036D", "HISTORY_037", "SERVICE_ON_042A", "STRUCTURE_KIND_043A", 
         "STRUCTURE_LEN_MT_049", "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060", "WORK_PROPOSED_075A", "WORK_DONE_BY_075B", "IMP_LEN_MT_076", 
         "BRIDGE_IMP_COST_094", "ROADWAY_IMP_COST_095", "TOTAL_IMP_COST_096")

conv = function (x){
  round((x%/%1e+06 + (x%%1e+06)/60e+06), digits = 6) %>%return
}

ca1 = select(ca, one_of(keep)) %>% filter(LAT_016 > 23e+06 & LAT_016 < 51e+06 & LONG_017 > 63e+06 & LONG_017 < 126e+06) %>%
  mutate(lat = conv(LAT_016), long = conv(LONG_017))

#plot ca
ggplot(data = ca1) +geom_point(mapping = aes(y = lat, x = long))
ca1$TOLL_020 = as.factor(ca1$TOLL_020)
summary(ca1$TOLL_020)
ggplot(data = ca1) +geom_point(mapping = aes(y = lat, x = long, col = TOLL_020))
ca1$OWNER_022 = as.factor(ca1$OWNER_022)
summary(ca1$OWNER_022)

colnames(ca1)

#summarize adt by toll and year built
ca1 %>% 
  group_by(YEAR_BUILT_027, TOLL_020) %>% 
  summarise(mean = mean(ADT_029, na.rm = TRUE)) ## reason for the problem?

ca1$STRUCTURE_NUMBER_008 %>% 
  group_by(YEAR_BUILT_027, TOLL_020) %>% 
  summarise(mean = mean(ADT_029)) ##???

ca1 = arrange(ca1, YEAR_BUILT_027, TOLL_020)

daily <- group_by(ca1, YEAR_BUILT_027, TOLL_020, HISTORY_037)
(per_day   <- summarise(daily, numBridges = n()))
#Error in n() : This function should not be called directly

#
dfSub = select(ca1, YEAR_BUILT_027, TOLL_020, HISTORY_037)
dfSub %>% spread (key = TOLL_020, value = HISTORY_037)
