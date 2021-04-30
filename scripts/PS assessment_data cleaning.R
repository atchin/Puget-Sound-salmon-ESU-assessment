##### PS pop assessment

library(tidyverse)
library(here)
here() # check file path
library(viridis)

# import data
inventory <- read_csv(here('data//WDFW-Salmonid_Stock_Inventory_Populations.csv'))
adult <- read_csv(here('data//WDFW-Salmonid_Stock_Inventory_Population_Escapement.csv'))

colnames(inventory) <- str_to_lower(colnames(inventory))
colnames(adult) <- str_to_lower(colnames(adult))
colnames(inventory)

unique(inventory$`esu/dps name`)
unique(inventory$`population name`)
unique(inventory$`salmon recovery region`)

##### step 1: pull out any population that is 'Puget Sound' or 'Hood Canal' in `salmon recovery region` col from inventory file #####
#        - includes: Population Name, Species, Stock Number, Federal Status, Listing Date

spvec <- c("Chinook","Chum","Coho","Pink","Sockeye") # these are focal species
rivervec <- c('Nooksack', 'Samish', 'Stillaguamish', 'Snohomish', 'Skagit', 'Duwamish', 'Lake Washington', 'Deschutes', 'Nisqually', 'Puyallup', 'Skokomish', 'Hamma Hamma', 'Duckabush', 'Dosewallips', 'Elwha', 'Dungeness')

PSHCstocks <- inventory %>%
    filter(`salmon recovery region` == 'Puget Sound' |
            `salmon recovery region` == 'Hood Canal') %>% # recovery regions for Puget Sound and Hood Canal
    filter(species %in% spvec) %>% # filter for species
    select(-`goal references`, -`goal notes`,-`recovery plan`,
      -`recovery plan year`,-`hatchery standards met`,
      -`local biologist name`,-`local biologist email`,
      -`last updated`) %>%
    arrange(`stock number`)

# check and explore
head(PSHCstocks)
glimpse(PSHCstocks)
unique(PSHCstocks$species)
unique(PSHCstocks$`population name`)
unique(PSHCstocks$`salmon recovery region`)
unique(PSHCstocks$`esu/dps name`)

# write.csv(PSHCstocks, here('data//PSHCstocks.csv'))
# do some manual naming here for run timing
PSHCstocks <- read.csv(here('data//PSHCstocks.csv'))

##### step 2: Match up Puget Sound and sub-pop listings to escapement runs from SCoRE data (Losee et al. 2019) #####
#             - match stock number to stock number
#             - filter for `data type`
#             - filter for SCoRE data
# try this
colnames(adult)
unique(adult$`data type`)
unique(adult$`report types`)
unique(adult$`population name`)

vec <- PSHCstocks %>%
    pull(`stock number`) # these are the PS/HC stock numbers
# or use intersect() ? and then filter
PSHCmerge <- merge(x=PSHCstocks, by.x="population.name",
                         y=adult, by.y='population name')
## the same result, but more columns
unique(PSHCmerge$population.name)
unique(PSHCmerge$run_timing_LH)
unique(PSHCmerge$`data type`)
unique(PSHCmerge$`report types`)
unique(PSHCmerge$)

# escapement
PSHCescapement <- PSHCmerge %>%
    # filter(`stock number` %in% vec) %>% # pull out values that match strings %in% the specified vector
    # https://stackoverflow.com/questions/35845903/dplyr-filter-value-is-contained-in-a-vector
    filter(`data type` == 'Escapement Fish') %>% # (expanded) escapement data from SCoRE, and eliminate even-year pinks because there are so few
    drop_na(year) %>% #pesky cleanup
        drop_na(`abundance quantity`) %>%
    select(-`sub-population name`, -`stock number`, -species.y) %>%
    arrange(population.name, year)

# spawners
PSHCspawners <- PSHCmerge %>%
    # filter(`stock number` %in% vec) %>% # pull out values that match strings %in% the specified vector
    # https://stackoverflow.com/questions/35845903/dplyr-filter-value-is-contained-in-a-vector
    filter(`data type` == 'Spawner Fish') %>% # (expanded) escapement data from SCoRE, and eliminate even-year pinks because there are so few
    drop_na(year) %>% #pesky cleanup
    drop_na(`abundance quantity`) %>%
    select(-`escapement methodology description`, -`sub-population name`, -`escapement methodology`, -`stock number`, -species.y) %>%
    arrange(population.name)

# explore data
glimpse(PSHCescapement)
unique(PSHCescapement$`data type`)
unique(PSHCescapement$species)
unique(PSHCescapement$population.name)
unique(PSHCescapement$run_timing_LH)

glimpse(PSHCspawners)
unique(PSHCspawners$`data type`)
unique(PSHCspawners$species)
unique(PSHCspawners$population.name)
unique(PSHCspawners$run_timing_LH)


# intersection between escapement and spawner data
escapementpops <- unique(PSHCescapement$population.name)
spawnerpops <- unique(PSHCspawners$population.name)
escapementpops[escapementpops %in% spawnerpops] # which escapement data also have corresponding spawner data?
# figure out how to get the left anf right join - the remainder of the data


# rivervec <- c('Nooksack', 'Samish', 'Stillaguamish', 'Snohomish', 'Skagit', 'Duwamish', 'Lake Washington', 'Deschutes', 'Nisqually', 'Puyallup', 'Skokomish', 'Hamma Hamma', 'Duckabush', 'Dosewallips', 'Elwha', 'Dungeness')
unique(PSHCescapement$population.name)
unique(PSHCescapement$year)
unique(PSHCescapement$`calculation type`)
unique(PSHCescapement$`production type`)
unique(PSHCescapement$`data series`)

# *****************************
### step 3: plot populations based on Species and Stock Number #####
# production type on each system vary: some are all natural, others have composition early in the time series and then seperate them out later. Have to go through system by system?
# going back to the drawing board...

# figure out how to plot data on top of one another - hatchery on top of wild fish, and composite as an entirely seperate set on the x-axis Solution!!: use aes(col=`production type`)
# OR use a Stacked Area chart with aes(fill=`production type`) + geom_area()


##### filter by species
# plot species overall ######

# escapement
escapementtrend_byspecies <- aggregate(x=PSHCescapement$`abundance quantity`,
                                       by=list(year=PSHCescapement$year,
                                               species=PSHCescapement$species.x,
                                               production=PSHCescapement$`production type`,
                                               LH=PSHCescapement$run_timing_LH), FUN=sum) # using aggregate since there's no sum() function in dplyr::group_by()
escapementtrend_byspecies <-
  escapementtrend_byspecies %>%
#  filter(!production=='Unknown') %>%
  transmute(year, species, production,LH, x = x)
yearbreaks <- seq(from=1970, to=2019, by=10)
yearbreaks[length(yearbreaks)+1] <- 2019

test <- ggplot(escapementtrend_byspecies, aes(x=year, y=x, fill=production)) +
  geom_col()+
  scale_x_continuous(limits=c(1970,2019), breaks=yearbreaks) +
    scale_shape_manual(name="Origin",values=c(1,16)) +
    labs(x='Year', y='Adult Abundance (thousands of fish)') +
    facet_wrap(~LH, nrow=4, scales='free_y') +
#    facet_wrap(~species, nrow=3, scales='free_y') +
    guides(shape=guide_legend(reverse=TRUE)) +
    theme_bw()
test


salmoncol <- function(data, plottitle='', legendorder= c('Natural','Hatchery','Composite','Unknown'), whyaxis, ...) {
#  data$production <- factor(data$production, levels=legendorder)
  ggplot(data, aes(x=year, y=x,fill=production)) +
    geom_col() +
    scale_x_continuous(limits=c(1969,2020), breaks=yearbreaks) +
    scale_fill_grey(breaks=legendorder, start=0.75, end=0.15) +
    labs(x='Return Year', y=whyaxis, title=plottitle) +
    geom_vline(aes(xintercept=2015), color="blue", linetype="dashed") + # plot data break
    #guides(fill=guide_legend(reverse=TRUE)) +
    theme_bw()
}

allescapement <- salmoncol(data=escapementtrend_byspecies)
allescapement + facet_wrap(~species, nrow=3, scales='free_y')
allescapement + facet_wrap(~LH, ncol=2, scales='free_y')


### all spawner data
spawnertrend_byspecies <- aggregate(x=PSHCspawners$`abundance quantity`,
                                    by=list(year=PSHCspawners$year,
                                            species=PSHCspawners$species.x,
                                            production=PSHCspawners$`production type`,
                                            LH=PSHCspawners$run_timing_LH,
                                            pop.name=PSHCspawners$population.name), FUN=sum)
spawnertrend_byspecies <-
  spawnertrend_byspecies %>%
  filter(!production=='Unknown' & !LH=='Even-Year Pink') %>%
  transmute(year, species, pop.name, production, LH, x = x)

# does composite data equal hatchery and wild fish? am I doublecounting?
 spawnertrend_byspecies %>% filter(year==2004 & species=='Chinook') %>% select(x, production)

# allspawners_col <- ggplot(spawnertrend_byspecies, aes(x=year, y=x,
#                                 fill=production)) +
#     geom_col() +
#     scale_x_continuous(limits=c(1970,2019), breaks=yearbreaks) +
#     labs(x='Year', y='Abundance', title='Spawners') +
#     facet_wrap(~species, nrow=6, scales='free_y')
# allspawners_col

allspawnersplot <- salmoncol(spawnertrend_byspecies)
allspawnersplot + facet_wrap(~species, nrow=3, scales='free_y')
allspawnersplot + facet_wrap(~LH, nrow=4, scales='free_y')


# determin proportion of species per year
# stacked area plot?
spawnertrend_byspecies$species <- factor(spawnertrend_byspecies$species, levels=c('Sockeye','Chinook','Coho','Chum','Pink'))

propspecies <- spawnertrend_byspecies %>% group_by(year, species, LH) %>%
  summarise(n=sum(x)) %>%
  mutate(spprop = n/sum(n)) # use similar code to calculate hatchery contribution

spproportionoverstudyperiod <- spawnertrend_byspecies %>%
  group_by(species) %>%
  summarize(n=sum(x)) %>%
  mutate(propn = n/sum(n))
View(spproportionoverstudyperiod)

LHproportionoverstudyperiod <- spawnertrend_byspecies %>%
  group_by(LH) %>%
  summarize(n=sum(x)) %>%
  mutate(propn = n/sum(n))

abundanceoverall <- spawnertrend_byspecies %>%
  group_by(year) %>%
  summarize(n=sum(x))
# abundanceoverall <- abundanceoverall[19:68,]
ggplot(data=abundanceoverall, aes(x=year, y=n)) +
  geom_line() +geom_point() + geom_smooth(method='lm') +
  labs(x='Year', y='Adult Spawner Abundance') +
  scale_x_continuous(limits=c(1969,2020), breaks=yearbreaks, expand=c(0,0)) +
  geom_line(aes(x=2015), color="grey", linetype="dashed") + # plot data break
  theme_bw()
summary(abundanceoverall)



########################################
ggplot(propspecies, aes(x=year, y=spprop, fill=species)) +
  geom_area(alpha=.5, size=0.5, color='white') +
  scale_x_continuous(limits=c(1984,2019), breaks=yearbreaks) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_viridis(discrete=T) +
  labs(x='Year', y='Proportion of observed spawners') +
  theme_bw()

spawnertrend_byspecies$LH <- factor(
  spawnertrend_byspecies$LH, levels=c('Odd-Year Pink',
                                      'Summer Chum',
                                      'Fall Chum',
                                      'Winter Chum',
                                      'Coho',
                                      'Sockeye',
                                      'Spring Chinook',
                                      'Summer/Fall Chinook'))

propLH <- spawnertrend_byspecies %>% group_by(year,  LH) %>%
  summarise(n=sum(x)) %>%
  mutate(prop = n/sum(n)) # use similar code to calculate hatchery contribution

ggplot(propLH, aes(x=year, y=prop, fill=LH)) +
#  geom_area(alpha=.5, size=0.5, color='white') +
  geom_bar(stat='identity') +
  scale_x_continuous(limits=c(1969,2020), breaks=yearbreaks, expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  #scale_fill_manual(name='Life History', values=c('pink','lightgreen','#45E73B','darkgreen','blue','red',"black",'grey')) +
  scale_fill_viridis(discrete = T) +
  labs(x='Year', y='Proportion of observed spawners') +
  theme_bw() +
  theme(legend.position = 'bottom')



proporigin_LH <- spawnertrend_byspecies %>% group_by(year, species, production, LH) %>%
  summarise(n=sum(x)) %>%
  mutate(originprop = n/sum(n),
         originpercentage = originprop*100)

proporigin_pops <- spawnertrend_byspecies %>% group_by(year, species, production, pop.name) %>%
  summarise(n=sum(x)) %>%
  mutate(originprop = n/sum(n),
         originpercentage = originprop*100)

ggplot(proporigin, aes(x=year, y=originprop, fill=production)) +
  geom_bar(position = 'dodge', stat='identity') +
  scale_x_continuous(limits=c(1970,2019), breaks=yearbreaks,expand=c(0,0)) +
 scale_y_continuous(expand=c(0.5,0.5)) +
  labs(x='Year', y='Proportion of Origin') +
  theme_bw() +
  facet_wrap(~species, nrow=5)


# proportions of hatchery fish
Chinookorigin_LH <- proporigin_LH %>%
    filter(species=='Chinook' & !production=='Composite')
Chinookorigin_pop <- proporigin_pops %>%
    filter(species=='Chinook' & !production=='Composite')
Chinookorigbreak <- seq(from=1985, to=2020, by=5)

ggplot(Chinookorigin_LH, aes(x=year, y=originprop, color=production)) +
  #geom_bar(position = 'dodge', stat='identity') +
  geom_line() + geom_point() +
  scale_x_continuous(limits=c(1985,2019), breaks=Chinookorigbreak) +
#  scale_y_continuous(expand=c(0,0)) +
  scale_color_manual(values=c('black','grey'), name='Origin')+
  labs(x='Year', y='Proportion of Origin') +
  theme_bw()

ggplot(Chinookorigin_pop, aes(x=year, y=originprop, color=production)) +
  #geom_bar(position = 'dodge', stat='identity') +
  geom_line() + geom_point() +
  scale_x_continuous(limits=c(1985,2019), breaks=Chinookorigbreak) +
#  scale_y_continuous(expand=c(0,0)) +
  scale_color_manual(values=c('black','grey'), name='Origin')+
  labs(x='Year', y='Proportion of Origin') +
  theme_bw() + facet_wrap(~pop.name, ncol=2, scale='free_y')

# allspawners_line <- ggplot(spawnertrend_byspecies, aes(x=year, y=x,
#                                color=production)) +
#    geom_line() +
#    geom_point() +
#    scale_x_continuous(limits=c(1970,2019), breaks=yearbreaks) +
#    labs(x='Year', y='Abundance', title='Spawners') +
#    facet_wrap(~species, nrow=6, scales='free_y')


#pinks to test ######
pinkescapement <- PSHCescapement %>%
    filter(species.x == "Pink") %>%
    transmute(pop_name=population.name, year, production=`production type`, calculation=`calculation type`, x=`abundance quantity`) %>%
    arrange(pop_name, year)

speciesfiltering <- function(data, species, ...) {
  data2 <- data %>%
      transmute(pop_name=population.name, year=year,
                production=`production type`,
                calculation=`calculation type`,
                x=`abundance quantity`,
                LH=run_timing_LH,
                species.x=species.x) %>%
      filter(species.x == species & ) %>%
      arrange(pop_name, year)
}

pinkspawners <- speciesfiltering(PSHCspawners, "Pink")
glimpse(pinkspawners)
# do hatchery and natural spawners need to be combined?
unique(pinkspawners$production) # no
length(unique(pinkspawners$pop_name))
#plot
salmoncol(pinkspawners)
pinkspawners_bysystem <- salmoncol(pinkspawners, 'Pink Spawners 1970-2019')+ facet_wrap(~pop_name, nrow=4, scales='free_y')# Hood Canal = Dosewallips, Duckabush, Hamma Hamma
pinkspawners_bysystem
# Strait = Elwha, Dungeness

# Chinook ######
Chinookescapement <- speciesfiltering(PSHCescapement, 'Chinook')
unique(Chinookescapement$production)
unique(Chinookescapement$calculation)
length(unique(Chinookescapement$pop_name))

Chinookescapement_bysystem <- salmoncol(Chinookescapement) + facet_wrap(~pop_name, nrow=5, scales='free_y') # by system
Chinookescapement_bysystem
salmoncol(Chinookescapement) + facet_wrap(~LH, nrow=2, scales='free_y')

Chinookspawners <- speciesfiltering(PSHCspawners, 'Chinook')
unique(Chinookspawners$production)
length(unique(Chinookspawners$pop_name))

salmoncol(Chinookspawners) + facet_wrap(~LH, scales='free_y')
Chinookspawners_bysystem <- salmoncol(Chinookspawners) + facet_wrap(~pop_name, nrow=5, scales='free_y')
Chinookspawners_bysystem

# Coho ######
cohoescapement <- speciesfiltering(PSHCescapement, 'Coho')
unique(cohoescapement$production)
length(unique(cohoescapement$pop_name))

salmoncol(cohoescapement) + facet_wrap(~pop_name, nrow=2, scales='free_y')
salmoncol(cohoescapement) + facet_wrap(~LH, nrow=2, scales='free_y')



cohospawners <- speciesfiltering(PSHCspawners, 'Coho')
unique(cohospawners$production)
length(unique(cohospawners$pop_name))

cohospawners_bysystem <- salmoncol(cohospawners) + facet_wrap(~pop_name, nrow=4, scales='free_y')
cohospawners_bysystem

# Sockeye ######
sockeyeescapement <- speciesfiltering(PSHCescapement, 'Sockeye')
unique(sockeyeescapement$production)
length(unique(sockeyeescapement$pop_name))
unique(sockeyeescapement$year)

sockeyeescapement_bysystem <- salmoncol(sockeyeescapement) + facet_wrap(~pop_name, nrow=1, scales='free_y')
sockeyeescapement_bysystem

sockeyespawners <- speciesfiltering(PSHCspawners, 'Sockeye')
unique(sockeyespawners$production)
length(unique(sockeyespawners$pop_name))
unique(sockeyespawners$year)

salmoncol(sockeyespawners) + facet_wrap(~pop_name, nrow=2, scales='free_y')
sockeyespawners_bysystem

# Chum ######
chumescapement <- speciesfiltering(PSHCescapement, 'Chum')
unique(chumescapement$production)
length(unique(chumescapement$pop_name))
unique(chumescapement$year)

salmoncol(chumescapement) #+ geom_line(aes(x=2015), color="grey", linetype="dashed") + # plot data break

salmoncol(chumescapement) + facet_wrap(~pop_name, nrow=6, scales='free_y')
salmoncol(chumescapement) + facet_wrap(~LH, nrow=3, scales='free_y')


chumspawners <- speciesfiltering(PSHCspawners, 'Chum')
unique(chumspawners$production)
length(unique(chumspawners$pop_name))
unique(chumspawners$year)
chumspawnertrend <- chumspawners %>% group_by(year) %>% summarize(x=sum(x))

salmoncol(chumspawners)
chumspawners_bysystem <- salmoncol(chumspawners) + facet_wrap(~pop_name, ncol=6, scales='free_y')
chumspawners_bysystem
salmoncol(chumspawners) + facet_wrap(~LH, nrow=1, scales='free_y')


##### filter by life history
