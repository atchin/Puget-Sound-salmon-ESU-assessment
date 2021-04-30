# plot PDO

library(here)
library(tidyverse)
library(zoo)
#install.packages('chron')
library(chron)

PDO <- read_csv(here('data//PDOindex.csv'))

PDO$DATE <- format(as.Date(PDO$time, format='%m/%d/%Y'),'%b %Y') # creates the Mon-Year format
#PDO$YEAR <- format(as.Date(PDO$time, format='%m/%d/%Y'),'%Y')
glimpse(PDO)
head(PDO)

barplot(height=PDO$INDEX,
        col=ifelse(PDO$INDEX>0,'red','blue'), border=NA,
        ylab='PDO Index', xlab='Year', names.arg=PDO$DATE)
