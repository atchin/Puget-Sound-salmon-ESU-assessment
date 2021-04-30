##### Lag time – better mean calculation? Geometric mean? #####


abundance_byspecies <- spawnertrend_byspecies %>%
  group_by(year, species) %>% 
  summarize(n=sum(x)) %>% # sum of spawners per year and per species
  arrange(year)

longbaseline <- abundance_byspecies %>% # average spawners per species
  group_by(species) %>%
  summarize(baselinegeomean=geomean(n), )
mean(longbaseline$baselinemean)

overallmean <- mean(abundanceoverall$n) # average spawners overall
overallsd <- sd(abundanceoverall$n)
overallmean
overallsd
baseline_sanspink <- abundance_byspecies %>% # remove Pink salmon and get the mean abundance of the rest of the species
 # group_by(species) %>%
  filter(!species=='Pink') #%>%
  #group_by(year)
overallmean_sanspink <- mean(baseline_sanspink$n)
overallmean_sanspink
overallsd_sanspink <- sd(baseline_sanspink$n)
overallsd_sanspink

# create a more recent baseline from the 14 years prior
regimesums <- abundance_byspecies %>%
  filter(year <= 2013 & year >= 1998) %>%
  group_by(year, species) %>%
  summarize(n=sum(n)) # sum of all species since 1998 regime shift

regimeaverage_sp <- regimesums %>% # average spawners per species
  group_by(species) %>%
  summarize(spmean=mean(n))
View(regimeaverage_sp)
regimeavg_all <- mean(regimesums$n) # average spawners overall
regimeavg_all

regime_all_sanspink <- 
regimesums %>%
  filter(!species=='Pink')
regime_avg_all_sanspink <- mean(regime_all_sanspink$n)
regime_avg_all_sanspink



blobyears <- abundance_byspecies %>% filter(year >= 2013) #%>% group_by(species) %>% summarize(n=sum(n)) #%>% mutate(percdecreasefrom2015 = n/)
avgblobyears <- abundance_byspecies %>% filter(year >= 2013) %>% summarize(n=mean(n)) #%>% mutate(percdecreasefrom2015 = n/)
write.csv(blobyears, here('data//blobyears.csv'))
write.csv(avgblobyears, here('data//avgblobyear.csv'))
write.csv(longbaseline, here('data//longbaseline.csv'))
write.csv(regimeaverage_sp, here('data//regimebaseline.csv'))
#write.csv(baseline, here('data//tenyearbaseline.csv'))
#do some editing in excel
regimeavg_all
regime_avg_all_sanspink


blobeffect <-read_csv(here('data//blobyears_edited.csv')) # edited to add average spawners per year, then %change from regime mean (1998-2013)
blobeffect$species <- factor(blobeffect$species, levels=c('Chinook','Coho','Chum', 'Sockeye','Pink','All'))
blobeffectplot <- ggplot(blobeffect, aes(x=year, y=perc_increaseordecrease, fill=species)) +
  geom_bar(stat='identity', position = 'dodge', width=0.5) +
  scale_fill_viridis(discrete = TRUE, name='') +
  #scale_fill_manual(name='',values=c('grey','blue','darkgreen','red','pink'))  +
  scale_x_continuous(breaks=seq(from=2013,2019,1)) +
  labs(x='Year', y='%Change from regime mean') + theme_bw()
blobeffectplot