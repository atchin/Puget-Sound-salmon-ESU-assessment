#### PS population assessment workflow
# Final Product
# multiple escapement plots for multiple runs for each species - scatter+line, or bar?
# steps:
# extract PS listings
# filter by species
# facet_wrap() by runs


# data sources
#   ESU inventory: 'WDFW-Salmonid_Stock_Inventory_Populations.csv'
#   spawners: 'WDFW-Salmonid_Stock_Inventory_Population_Escapement.csv'
#   smolts: 'WDFW-_Juvenile_Population_Abundance.csv'

# step 1: pull out any ESU/DPS listings that has 'Puget Sound' from inventory file
#        - includes: Population Name, Species, Stock Number, Federal Status, Listing Date
#        ESU/DPS Name == strdetect('Puget Sound')

# step 2: Match up Puget Sound and sub-pop listings to spawner spawner runs
#             - match 'inventory$Population Name' in inventory to 'spawner$Population Name'
#             - Data Type == "Spawner Fish"

# step 3: plot populations based on Species and Stock Number
# for some time series, part of it is composite and later have combine hatchery+natural spawners
#       - for each Stock Number, and for each year:
#           - add abundance from where Prodection=='Hatchery' and 'Natural'

# approaches:
# one filter for species, then facet_wrap() by population number/system?
# or filter for species and then add them up?


# step 4: check on data types - plot by hatchery, wild, both? may depend on drainage and combine natural+hatchery escapement
#
