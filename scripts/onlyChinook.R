# only plot Chinook

library(tidyverse)
library(here)
here() # check file path
library(viridis)

# Numbers? Rerun and make new figures – trend,  by LH, system – fit with trend lines?

########## escapement (dont use) ###############
Chinookescapement <- speciesfiltering(PSHCescapement, 'Chinook')
unique(Chinookescapement$production)
unique(Chinookescapement$calculation)
length(unique(Chinookescapement$pop_name))

Chinookescapement_overall <- Chinookescapement %>%
   group_by(year) %>%
   summarize(n=sum(x))
# abundanceoverall <- abundanceoverall[19:68,]
ggplot(data=Chinookescapement_overall, aes(x=year, y=n,
                                           #color=production
                                           )) +
   geom_line() +geom_point() + geom_smooth(method='lm') +
   labs(x='Year', y='Adult Escapement Abundance') +
   scale_x_continuous(limits=c(1969,2020), breaks=yearbreaks, expand=c(0,0)) +
   scale_y_continuous(limits=c(0,40000)) +
   geom_vline(aes(xintercept=2015), color="blue", linetype="dashed") + # plot data break
   theme_bw()

salmoncol(Chinookescapement, whyaxis = 'Adult Escapement')
Chinookescapement_bysystem <- salmoncol(Chinookescapement) + facet_wrap(~pop_name, nrow=5, scales='free_y') # by system
Chinookescapement_bysystem
salmoncol(Chinookescapement) + facet_wrap(~LH, nrow=2, scales='free_y')


hatcherywild_abundancecomp <- Chinookescapement %>% group_by(production, year) #%>%
   #summarise(n=sum(x)) %>%
   #filter(!production=='Composite' & !production=='Unknown')

ggplot(hatcherywild_abundancecomp, aes(x=year, y=n)) +
   geom_point(aes(shape=production), size=3) +
   geom_line(aes(linetype=production, color=production)) + 
   geom_smooth(method='lm',se=F, aes(linetype=production, color=production), ) +
   scale_color_manual(values=c('black','black'),
                      breaks=c('Natural','Hatchery'),
                      name='Origin') +
   scale_shape_manual(values=c(16,1),
                      breaks=c('Natural','Hatchery'),
                      name='Origin') +
   scale_linetype_manual(values=c(1,6),
                         breaks=c('Natural','Hatchery'),
                         name='Origin') +
   labs(x='Return Year', y="Spawner Returns", title='Originabundancecomp') +
   theme_bw()


########## spawners (use since the WDFW SOS analysis did) ###############
Chinookspawners <- speciesfiltering(PSHCspawners, 'Chinook')
unique(Chinookspawners$production)
length(unique(Chinookspawners$pop_name))

salmoncol(Chinookspawners, whyaxis='Spawner Returns') + scale_y_continuous(expand=c(0,0))
salmoncol(Chinookspawners, whyaxis='Spawner Returns') +
   scale_y_continuous(expand=c(.005,0)) +
   facet_wrap(~LH, scales='free_y')

Chinookspawners_bysystem <- salmoncol(Chinookspawners, whyaxis='Spawner Returns') +
#   scale_y_continuous(expand=c(1,0)) +
   facet_wrap(~pop_name, nrow=5, scales='free_y')
Chinookspawners_bysystem

hatcherywild_abundancecomp <- Chinookspawners %>% group_by(production, year) %>%
   summarise(n=sum(x)) %>%
   filter(!production=='Composite' & !production=='Unknown')


ggplot(hatcherywild_abundancecomp, aes(x=year, y=n)) +
   geom_point(aes(shape=production), size=3) +
   geom_line(aes(linetype=production, color=production)) + 
   geom_smooth(method='glm',se=F, aes(linetype=production, color=production), ) +
   scale_color_manual(values=c('black','black'),
                      breaks=c('Natural','Hatchery'),
                      name='Origin') +
   scale_shape_manual(values=c(16,1),
                      breaks=c('Natural','Hatchery'),
                      name='Origin') +
   scale_linetype_manual(values=c(1,6),
                         breaks=c('Natural','Hatchery'),
                         name='Origin') +
   labs(x='Return Year', y="Spawner Returns", title='Originabundancecomp') +
   theme_bw()


############ Productivity - only if you have time ##############