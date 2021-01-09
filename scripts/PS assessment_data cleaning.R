##### PS pop assessment

library(tidyverse)
library(here)
here() # check file path

# import data
inventory <- read_csv(here('data//WDFW-Salmonid_Stock_Inventory_Populations.csv'))
adult <- read_csv(here('data//WDFW-Salmonid_Stock_Inventory_Population_Escapement.csv'))

colnames(inventory) <- str_to_lower(colnames(inventory))
colnames(adult) <- str_to_lower(colnames(adult))
colnames(inventory)

unique(inventory$`esu/dps name`)
unique(inventory$`population name`)
unique(inventory$`salmon recovery region`)

##### step 1: pull out any population that is 'Puget Sound' or 'Hood Canal' in `salmon recovery region` col from inventory file
#        - includes: Population Name, Species, Stock Number, Federal Status, Listing Date

spvec <- c("Chinook","Chum","Coho","Pink","Sockeye","Steelhead") # these are focal species
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




##### step 2: Match up Puget Sound and sub-pop listings to escapement runs from SCoRE data (Losee et al. 2019)
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
PSHCmerge <- merge(x=PSHCstocks, by.x="population name",
                         y=adult, by.y='population name')
## the same result, but more columns
unique(PSHCmerge$`population name`)
unique(PSHCmerge$`data type`)
unique(PSHCmerge$`report types`)

# escapement
PSHCescapement <- PSHCmerge %>%
    # filter(`stock number` %in% vec) %>% # pull out values that match strings %in% the specified vector
    # https://stackoverflow.com/questions/35845903/dplyr-filter-value-is-contained-in-a-vector
    mutate(is_SCoRE = str_detect(`report types`, 'SCoRE')) %>% # not all series have SCoRE in them?
    filter(is_SCoRE == TRUE & `data type` == 'Escapement Fish') %>% # (expanded) escapement data from SCoRE
    drop_na(year) %>% #pesky cleanup
    select(-`escapement methodology description`, -`sub-population name`, -`escapement methodology`, -is_SCoRE, -`stock number.y`, -species.y) %>%
    arrange(`population name`, year)

# spawners
PSHCspawners <- PSHCmerge %>%
    # filter(`stock number` %in% vec) %>% # pull out values that match strings %in% the specified vector
    # https://stackoverflow.com/questions/35845903/dplyr-filter-value-is-contained-in-a-vector
    mutate(is_SCoRE = str_detect(`report types`, 'SCoRE')) %>% # not all series have SCoRE in them?
    filter(is_SCoRE == TRUE & `data type` == 'Spawner Fish') %>% # (expanded) escapement data from SCoRE
    drop_na(year) %>% #pesky cleanup
    select(-`escapement methodology description`, -`sub-population name`, -`escapement methodology`, -is_SCoRE, -`stock number.y`, -species.y) %>%
    arrange(`population name`, year)

# explore data
glimpse(PSHCescapement)
unique(PSHCescapement$`data type`)
unique(PSHCescapement$species)
unique(PSHCescapement$`population name`)


glimpse(PSHCspawners)
unique(PSHCspawners$`data type`)
unique(PSHCspawners$species)
unique(PSHCspawners$`population name`)

# intersection between escapement and spawner data
escapementpops <- unique(PSHCescapement$`population name`)
spawnerpops <- unique(PSHCspawners$`population name`)
escapementpops[escapementpops %in% spawnerpops] # which escapement data also have corresponding spawner data?
# figure out how to get the left anf right join - the remainder of the data


# rivervec <- c('Nooksack', 'Samish', 'Stillaguamish', 'Snohomish', 'Skagit', 'Duwamish', 'Lake Washington', 'Deschutes', 'Nisqually', 'Puyallup', 'Skokomish', 'Hamma Hamma', 'Duckabush', 'Dosewallips', 'Elwha', 'Dungeness')
unique(PSHCescapement$`population name`)
unique(PSHCescapement$year)
unique(PSHCescapement$`calculation type`)
unique(PSHCescapement$`production type`)
unique(PSHCescapement$`data series`)

# *****************************
### step 3: plot populations based on Species and Stock Number
# production type on each system vary: some are all natural, others have composition early in the time series and then seperate them out later. Have to go through system by system?
# going back to the drawing board...

##### filter by species
# use a str_detect() function to filter species out

##### filter by system
# use another str_detect() function to filter for system?

#       - for each Stock Number, and within each year:
#           - add abundance where both `production type`=='Hatchery' and `production type`=='Natural'

#   if I can filter which systems I should focus on, that would thin the workload a little bit















# to do this:
# 1. filter for both Hatchery and Natural first
manualcomp <- PSHCescapement %>% # 'data' is placeholder - test drive first
    filter(`production type`=='Natural' | `production type`=='Hatchery') %>%
        arrange(desc(year))
autocomp <- PSHCescapement %>%
    filter(`production type`=='Composite') %>%
        arrange(year)
autocomp # check and set aside
unique(autocomp$year)
unique(PSHCescapement$year)
length(PSHCescapement$`production type`)
length(autocomp$year)

filter(species=="Chinook") # or whatever - you can function this if you want
# 1. create egg cartons for each population within the first loop
# 2. in the for-loops, call the stock number first, then the year
nyear <- length(manualcomp$year)
nstock <- length(manualcomp$`stock number`)
compmat <- matrix(nrow=nyear, ncol=nstock+1)
colnames(compmat) <- c('year', unique(manualcomp$`population name`))
for (i in nstock){ # fill columns first

    for (j in nyear) {
    # run calculation in here
    mat[j,i] # store value in mat
        }

}
# 3. pull out the Abundance number associated with both Hatchery and Natural and then add them
#   - use mutate()?
#   - or a multi-condition if-else loop ?
if (PSHCescapement$`condition type`='Natural' && ) {

}
