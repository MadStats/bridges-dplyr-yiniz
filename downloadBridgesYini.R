# goal: create a dataset containing vars: state code, lat, long, year built, traffic, traffic safety, feature, condition, bridge costs, 
# check COST, might not have enough obsv
# 1) investigate national bridges, by levels of ownership, condition, year built, daily traffice, history, on bridge service, 
# 2) investigate the costs, improvement, results

library(choroplethr)
library(choroplethrMaps)
library(data.table)
library(tidyverse)
library(plyr)


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
load("x16.RData")

B = x16

#data cleaning, remove bad columns and rows
colnames(B)[14] #check the col name @ certain pos
#see and select variables of interest to me
colnames(B)
keep = c("STATE_CODE_001","COUNTY_CODE_003","STRUCTURE_NUMBER_008", "LAT_016", "LONG_017", "TOLL_020", "OWNER_022", "YEAR_BUILT_027", 
         "ADT_029", "APPR_RAIL_END_036D", "HISTORY_037", "SERVICE_ON_042A", "STRUCTURE_KIND_043A", 
         "STRUCTURE_LEN_MT_049", "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060", "WORK_PROPOSED_075A", "WORK_DONE_BY_075B", "IMP_LEN_MT_076", 
         "BRIDGE_IMP_COST_094", "ROADWAY_IMP_COST_095", "TOTAL_IMP_COST_096", "YEAR_OF_IMP_097")

B1 = select(B, one_of(keep))

#plot national bridges
NatnlB = select(B1, STATE_CODE_001:SUBSTRUCTURE_COND_060) %>% mutate( fip = STATE_CODE_001*1000+COUNTY_CODE_003)

### getting help from bridges-dplyr-GuanxuSu
min2dec = function(x){
  n = nchar(x) #
  as.numeric(substr(x,1,n-6)) + as.numeric(substr(x,n-5,n))/6e+05 %>% return
}
NatnlB = NatnlB %>% mutate(lat = min2dec(LAT_016), long = min2dec(LONG_017)) %>% filter(long < 130, long>60,lat<50,lat>20)

###Yini's clusmy way of converting the lat and long
summary(NatnlB$LAT_016)
summary(NatnlB$LONG_017)
#according to us census Extent: (-124.848974, 24.396308) - (-66.885444, 49.384358), 
#https://www.quora.com/What-is-the-longitude-and-latitude-of-a-bounding-box-around-the-continental-United-States
#mtguide: lat 8 digits, long 9 digits
NatnlB = filter (NatnlB, LAT_016 > 23e+06 & LAT_016 < 51e+06 & LONG_017 > 63e+06 & LONG_017 < 126e+06)
conv = function (x){
 round((x%/%1e+06 + (x%%1e+06)/6e+05), digits = 6) %>%return
}
conv(90533000)
NatnlB = mutate(NatnlB, lat = conv(LAT_016), long = conv(LONG_017)) 
summary(NatnlB$lat)
summary(NatnlB$long)
#why rounding the data? data points are not granular

sm = NatnlB[sample(nrow(NatnlB),10000),]

######plotting
ggplot(data = NatnlB) + geom_point(mapping = aes(y = lat, x = long), size = 0.3) 
ggplot(data = NatnlB) + geom_point(mapping = aes(y = LAT_016, x = LONG_017), size = 0.3) 

#plot bridges by toll and toll free
str(NatnlB$TOLL_020)
NatnlB$TOLL_020 = as.factor(NatnlB$TOLL_020)
summary(NatnlB$TOLL_020)
NatnlB$Toll = as.factor(ifelse(NatnlB$TOLL_020 == 3, 0, 1))
summary(NatnlB$Toll)
ggplot(data = NatnlB) + geom_point(mapping = aes(y = lat, x = long, col =Toll), size = 0.3) + ggtitle("Toll and toll free bridges across the nation")

#plot bridges by historical sig
str(NatnlB$HISTORY_037)
NatnlB %>% mutate(history = as.factor(ifelse(HISTORY_037 == 1, 1, 0))) %>%
ggplot() + geom_point(mapping = aes(y = lat, x = long, col = history), size = 0.3) + ggtitle("Bridges with historical significance")

NatnlB %>% mutate(history = ifelse(HISTORY_037 == 1, 1, 0)) %>%
  group_by(YEAR_BUILT_027) %>%
  summarise(numHistory = sum(history)) %>%
  ggplot(mapping = aes(x = YEAR_BUILT_027, y = numHistory)) + geom_point() + ggtitle("# of historical bridges per year")


#plot bridges by daily average traffic
NatnlB %>% group_by(YEAR_BUILT_027) %>% summarise(yearAveT = log(mean(ADT_029))) %>%
  ggplot() + geom_point(mapping = aes(x = YEAR_BUILT_027, y = yearAveT)) + ggtitle("Average ADT in bridges by year")

NatnlB %>%group_by(fip) %>% summarise(adt = mean(ADT_029, na.rm = TRUE)) %>% 
  mutate(ADT_fip = 
           ifelse(adt <= quantile(adt)[1], 2, 
                  ifelse(adt <= quantile(adt)[2], 3, 
                         ifelse(adt <= quantile(adt)[3], 4, 1)))
  ) %>%
  transmute(region = fip, value = ADT_fip) %>%
  county_choropleth(title = "ADT by states: CO", legend = "ADT quartiles", state_zoom = c("colorado")) 

x = seq(1, 20, by = 2)
quantile(x)[1]

#plot owners of bridges
B1 %>% mutate(fip = STATE_CODE_001*1000+COUNTY_CODE_003,
              federal = ifelse(OWNER_022 %in% 61:76, 1, 0), na.rm = TRUE) %>%
  group_by(fip) %>% summarise(numFed = sum(federal)) %>%
  transmute(region = fip, value = ifelse(numFed >= mean(numFed), 1, 0)) %>%
  county_choropleth (title = "Above/below average # federal bridges")

#plot cost of bridges
rateIt = function(x){
  # gives a good to fail rating for cond.
  rate = rep("good", length(x))
  rate[x<6] = "bad"
  rate[x <2]= "fail"
  return(rate)
}

str(B1$SUPERSTRUCTURE_COND_059)

B2 = B1 %>% select(STATE_CODE_001, COUNTY_CODE_003, YEAR_BUILT_027, OWNER_022, ADT_029, SUPERSTRUCTURE_COND_059:TOTAL_IMP_COST_096, YEAR_OF_IMP_097) %>%
  mutate(fip = STATE_CODE_001*1000+COUNTY_CODE_003,
         federal = ifelse(OWNER_022 %in% 61:76, 1, 0), 
         cond = rateIt((as.numeric(SUPERSTRUCTURE_COND_059) + as.numeric(SUBSTRUCTURE_COND_060))/2),
         cost = TOTAL_IMP_COST_096, 
         length = IMP_LEN_MT_076) %>%
  filter(cost > 0, YEAR_OF_IMP_097 <= 2016) 
  
summary(B2$YEAR_OF_IMP_097)
B2$YEAR_OF_IMP_097 = as.factor(B2$YEAR_OF_IMP_097)

B2 %>% ggplot() + geom_bar(mapping = aes(x = YEAR_OF_IMP_097)) + ggtitle ("# of bridge having cost estimated per year")

B2 %>% group_by(YEAR_BUILT_027) %>%
  summarise(aveCost = log(mean(cost)), aveLength = log(mean(length)), count = n()) %>%
  ggplot() + geom_point(mapping = aes(x = YEAR_BUILT_027, y = aveCost)) + 
  ggtitle("Mean improvement cost of bridges built over the years")

B2 %>% group_by(cond, YEAR_BUILT_027) %>% summarise(aveCost = log(mean(cost)), aveLength = log(mean(length)), count = n()) %>%
  ggplot() + geom_point(mapping = aes(x = YEAR_BUILT_027, y = aveCost, col = as.factor(cond))) + ggtitle("Mean improvement cost of 154,074 bridges built over the years")

B2 %>% group_by(federal, YEAR_BUILT_027) %>% summarise(aveCost = log(mean(cost)), aveLength = log(mean(length)), count = n()) %>%
  ggplot() + geom_point(mapping = aes(x = YEAR_BUILT_027, y = aveCost, col = as.factor(federal))) + ggtitle("Mean improvement cost of 154,074 bridges built over the years")

B2 %>% group_by(cond, YEAR_OF_IMP_097, YEAR_BUILT_027) %>% summarise(aveCost = log(mean(cost)), aveLength = log(mean(length)), count = n()) %>%
  ggplot(mapping = aes(x = YEAR_BUILT_027, y = aveCost)) + geom_point() + geom_smooth() +
  facet_wrap(~ as.factor(YEAR_OF_IMP_097), nrow = 3) + ggtitle("Improvement cost by year of estimation")





