#download and read epa air quality data: 2006 to 2016
# https://www.epa.gov/outdoor-air-quality-data/air-quality-statistics-report

library(tidyverse)
library(plyr)
library(choroplethr)
library(data.table)

url = "https://www3.epa.gov/cgi-bin/broker?_service=data&_server=134.67.99.91&_port=4075&_sessionid=8oNmAsVqO52&areaname=United%20States&sumlevel=groupbycbsa&event=Included%20(if%20any)&year=2016&type=csv&_PROGRAM=dataprog.ad_rep_con_getdata.sas"
year = seq (2006, 2016, 1)
dest= rep("", 11)
for (i in 1:11) dest[i] = paste("https://www3.epa.gov/cgi-bin/broker?_service=data&_server=134.67.99.91&_port=4075&_sessionid=8oNmAsVqO52&areaname=United%20States&sumlevel=groupbycbsa&event=Included%20(if%20any)&year=", year[i], "&type=csv&_PROGRAM=dataprog.ad_rep_con_getdata.sas", sep = "")
for (j in 1:11) paste("df", year[j], sep = "") = fread(dest[j])
#unable to establish connection, data access issue

#brute force read
df2005 = read.csv(file.choose(), header = TRUE)
