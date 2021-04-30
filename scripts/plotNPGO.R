library(here)
library(tidyverse)
# install.packages('zoo')
library(zoo)

NPGO <- read_csv(here('data//NPGOindex_toJuly2020.csv'))
#NPGO$YEAR <- format(NPGO$YEAR, '%Y')
#NPGO$MONTH <- format(NPGO$MONTH, '%m')
glimpse(NPGO)
NPGO$DATE <- as.yearmon(paste(NPGO$YEAR, NPGO$MONTH), '%Y %m')
NPGO <- filter(NPGO, YEAR >= 1970)
#ggplot(NPGO, aes(x=DATE, y=index)) +
#    geom_bar(stat='identity',
#             width=0.8, aes(fill=color)) +
#    scale_x_yearmon(format='%m %Y', expand=c(0,0)) +
# #  scale_x_date() +
#    scale_fill_manual(values=c(positive='red',negative='blue')) +
#    geom_line(aes(y=0), color='black') + theme_bw()


barplot(height=NPGO$INDEX,
        col=ifelse(NPGO$INDEX>0,'red','blue'), border=NA,
        ylab='NPGO Index', xlab='Year', names.arg=NPGO$DATE)
