##### PS pop assessment - lag time

library(tidyverse)
library(here)
# install.packages('FSA')
library(FSA)
here() # check file path


# manual geomean computation
# x <- c(vector)
# exp(mean(log(x)))
glimpse(Chinookspawners)
geomeancalc <- Chinookspawners %>% group_by(year) %>% summarize(n=sum(x)) %>% filter(year >= 1970)

# compare geomean from blob years to geomeans since 2000 ?

geomean2000 <- PSHCspawners %>% group_by(year) %>% summarize(n=sum(`abundance quantity`)) %>%
  filter(year>=2000 & year<=2013) %>% select(n)
geomean(unlist(geomean2000))
geosd(unlist(geomean2000))

baselinegeomeanfunc <- function(sp, data=PSHCspawners,  ...){ tblvec <- data %>% filter(species.x==sp) %>% group_by(year) %>% summarize(n=sum(`abundance quantity`)) %>% filter(year>=2000 & year<=2013) %>% select(n)
print(geomean(unlist(tblvec)))
}

baselinegeomeanvec <- vector(length=5)
names(baselinegeomeanvec) <- spvec
for (i in 1:length(spvec)) {
  baselinegeomeanvec[i] <-baselinegeomeanfunc(sp=spvec[i])
}
baselinegeomeanvec

# during blob (2014, 2015, 2016)
blobyeargeomeanfunc <- function(sp, data=PSHCspawners,  ...){ tblvec <- data %>% filter(species.x==sp) %>% group_by(year) %>% summarize(n=sum(`abundance quantity`)) %>% filter(year==2014 | year==2015 | year==2016) %>% select(n)
print(geomean(unlist(tblvec)))
}

duringblobgeomeanvec <- vector(length=5)
names(duringblobgeomeanvec) <- spvec
for (i in 1:length(spvec)) {
  duringblobgeomeanvec[i] <-blobyeargeomeanfunc(sp=spvec[i])
}
duringblobgeomeanvec

duringblobvec <- PSHCspawners %>% group_by(year) %>% summarize(n=sum(`abundance quantity`)) %>%
  filter(year==2014 | year==2015 | year==2016) %>% select(n)
geomean(unlist(duringblobvec))
geosd(unlist(duringblobvec))


# post blob (2017, 2018, 2019)
postblobgeomeanfunc <- function(sp, data=PSHCspawners,  ...){ tblvec <- data %>% filter(species.x==sp) %>% group_by(year) %>% summarize(n=sum(`abundance quantity`)) %>% filter(year==2017 | year==2018 | year==2019) %>% select(n)
print(geomean(unlist(tblvec)))
}

postblobgeomeanvec <- vector(length=5)
names(postblobgeomeanvec) <- spvec
for (i in 1:length(spvec)) {
  postblobgeomeanvec[i] <- postblobgeomeanfunc(sp=spvec[i])
}
postblobgeomeanvec

postblobvec <- PSHCspawners %>% group_by(year) %>% summarize(n=sum(`abundance quantity`)) %>%
  filter(year==2017 | year==2018 | year==2019) %>% select(n)
geomean(unlist(postblobvec))
geosd(unlist(postblobvec))


threeyeargeomean(timeper=c(2017,2018,2019))

matrix(ncol=3, nrow=15) # 2019 - 1974
threeyeargeomeanseq <- seq(from=1975, to=2019, by=1)



# extract vector from 5 year intervals (1970-2019)
for (i in length(spvec)){
  # filter time series for species
  # then, pull out vector for every five years
  # use geomean
  # place back into eggcarton with length=10
}
# plug into geomean



# find out hatchery contribution = sum(hatchery fish)/sum(five year vector)
