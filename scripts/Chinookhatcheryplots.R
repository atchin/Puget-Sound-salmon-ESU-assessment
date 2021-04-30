library(tidyverse)
library(here)
here()

########## Hatchery contribution ############
# proportions of hatchery fish in spawners
Chinookspawnerorigin_all <- Chinookspawners %>%
   group_by(year, production) %>%
   filter(!production=='Composite' & !production=='Unknown') %>%
   summarise(n=sum(x)) %>%
   mutate(originprop = n/sum(n),
          originpercentage = originprop*100)



Chinookspawnerorigin_LH <- Chinookspawners %>%
   group_by(year, production, LH) %>%
   filter(!production=='Composite' & !production=='Unknown') %>%
   summarise(n=sum(x)) %>%
   mutate(originprop = n/sum(n),
          originpercentage = originprop*100)


Chinookspawnerorigin_pop <- Chinookspawners %>%
   group_by(year, production, pop_name) %>%
#   filter(!production=='Composite'& !production=='Unknown') %>%
   summarise(n=sum(x)) %>%
   mutate(originprop = n/sum(n),
          originpercentage = originprop*100) %>%
   arrange(pop_name, year)

write.csv(Chinookspawnerorigin_pop, here('data//writtendata//Chinookspawnerorigin_pops.csv')) # for exploring in further detail for report

Chinookorigbreak <- seq(from=1985, to=2019, by=5)
Chinookorigbreak[length(Chinookorigbreak)+1] <- 2019

ggplot(Chinookspawnerorigin_all, aes(x=year, y=originprop, color=production)) +
   #geom_bar(position = 'dodge', stat='identity') +
   geom_line() + geom_point() +
   scale_x_continuous(limits=c(1985,2019), breaks=Chinookorigbreak) +
   scale_y_continuous(breaks=c(0,0.2,.4,.6,.8,1.0)) +
   #  scale_y_continuous(expand=c(0,0)) +
   scale_color_manual(values=c('black','grey'), name='Origin',
                      labels=c("Natural",'Hatchery'),
                      breaks=c('Natural','Hatchery')
                      )+
   labs(x='Year', y='Proportion of Origin') +
   theme_bw()


ggplot(Chinookspawnerorigin_LH, aes(x=year, y=originprop, color=production)) +
   #geom_bar(position = 'dodge', stat='identity') +
   geom_line() + geom_point() +
   scale_x_continuous(limits=c(1985,2019), breaks=Chinookorigbreak) +
   scale_y_continuous(breaks=c(0,0.2,.4,.6,.8,1.0)) +
   scale_color_manual(values=c('black','grey'),
                      name='Origin',
                      #labels=c("Natural",'Hatchery')
                      )+
   labs(x='Year', y='Proportion of Origin', title="Chinook spawners by LH") +
   theme_bw() +
   facet_wrap(~LH, ncol=1, scale='free_y')

#############JUST REPORT, NO PLOTTING ON POP DIFFERENCES
# ggplot(Chinookspawnerorigin_pop, aes(x=year, y=originprop, color=production)) +
#    #geom_bar(position = 'dodge', stat='identity') +
#    geom_line() + geom_point() +
#    scale_x_continuous(limits=c(1985,2019), breaks=Chinookorigbreak) +
#    scale_y_continuous(breaks=c(0,0.2,.4,.6,.8,1.0)) +
#    scale_color_manual(values=c('black','grey'), name='Origin')+
#    labs(x='Year', y='Proportion of Origin', title="Chinook spawners by pop") +
#    theme_bw() + facet_wrap(~pop_name, ncol=2, scale='free_y')



#### hatchery contribution to escapement ######
Chinookescapementorigin_all <- Chinookescapement %>%
   group_by(year, production) %>%
   filter(!production=='Composite' & !production=='Unknown') %>%
   summarise(n=sum(x)) %>%
   mutate(originprop = n/sum(n),
          originpercentage = originprop*100)


Chinookescapementorigin_LH <- Chinookescapement %>%
   group_by(year, production, LH) %>%
   filter(!production=='Composite' & !production=='Unknown') %>%
   summarise(n=sum(x)) %>%
   mutate(originprop = n/sum(n),
          originpercentage = originprop*100)


Chinookescapementorigin_pop <- Chinookescapement %>%
   group_by(year, production, pop_name) %>%
   filter(!production=='Composite') %>%
   summarise(n=sum(x)) %>%
   mutate(originprop = n/sum(n),
          originpercentage = originprop*100)

ggplot(Chinookescapementorigin_all, aes(x=year, y=originprop, color=production)) +
   #geom_bar(position = 'dodge', stat='identity') +
   geom_line() + geom_point() +
   scale_x_continuous(limits=c(1985,2019), breaks=Chinookorigbreak) +
   scale_y_continuous(breaks=c(0,0.2,.4,.6,.8,1.0)) +
   scale_color_manual(values=c('black','grey'), name='Origin')+
   labs(x='Year', y='Proportion of Origin', title="Chinook escapement all") +
   theme_bw()



ggplot(Chinookescapementorigin_LH, aes(x=year, y=originprop, color=production)) +
   #geom_bar(position = 'dodge', stat='identity') +
   geom_line() + geom_point() +
   scale_x_continuous(limits=c(1996,2019), breaks=Chinookorigbreak) +
   scale_y_continuous(breaks=c(0,0.2,.4,.6,.8,1.0)) +
   scale_color_manual(values=c('black','grey'), name='Origin')+
   labs(x='Year', y='Proportion of Origin', title="Chinook escapement by LH") +
   theme_bw() + facet_wrap(~LH, ncol=1)

# not helpful
# ggplot(Chinookescapementorigin_pop, aes(x=year, y=originprop, color=production)) +
#    #geom_bar(position = 'dodge', stat='identity') +
#    geom_line() + geom_point() +
#    scale_x_continuous(limits=c(1985,2019), breaks=Chinookorigbreak) +
#    #  scale_y_continuous(expand=c(0,0)) +
#    scale_color_manual(values=c('black','grey'), name='Origin')+
#    labs(x='Year', y='Proportion of Origin', title="Chinook escapement by pop") +
#    theme_bw() + facet_wrap(~pop_name, ncol=2, scale='free_y')

