library(rvest)
library(worldfootballR)
library(janitor)
library(tidyverse)
library(ggplot2)
library(teamcolors)





eastern_conference <- get_season_team_stats(country = 'USA', gender = 'M', season_end_year = '2021', tier = '1st', stat_type = 'league_table_home_away')
eastern_conference <- eastern_conference %>% 
  mutate(HomexGp90 = xG_Home/MP_Home) %>%
  mutate(HomexGAp90 = xGA_Home/MP_Home) %>% 
  mutate(AwayxGp90 = xG_Away/MP_Away) %>% 
  mutate(AwayxGAp90 = xGA_Away/MP_Away) %>% 
  mutate(HomeGp90 = GF_Home/MP_Home) %>% 
  mutate(AwayGp90 = GF_Away/MP_Away) %>% 
  mutate(HomeActualvxG = HomeGp90-HomexGp90) %>% 
  mutate(AwayActualvxG = AwayGp90-AwayxGp90)

scraped_mls <- read_html('https://fbref.com/en/comps/22/Major-League-Soccer-Stats')

mls_big_data <- scraped_mls %>% 
  html_table(fill = TRUE)

western_conference <- mls_big_data[[4]] %>% 
  row_to_names(1)

western_conference_home <- western_conference[,1:15]

western_conference_home$W <- as.numeric(western_conference_home$W)
western_conference_home$D <- as.numeric(western_conference_home$D)
western_conference_home$L <- as.numeric(western_conference_home$L)
western_conference_home$GF <- as.numeric(western_conference_home$GF)

western_conference_home$MP <- as.numeric(western_conference_home$MP)
western_conference_home$xG <-as.double(western_conference_home$xG)
western_conference_home$xGA <-as.double(western_conference_home$xGA)
western_conference_home <- western_conference_home %>% 
  mutate(HomexGp90 = xG/MP) %>% 
  mutate(HomexGAp90 = xGA/MP) %>% 
  mutate(HomeActualvxG = (GF-xG)/MP)

western_conference_away <- western_conference[,c(1,2, 16:28)]

western_conference_away$MP <- as.numeric(western_conference_away$MP)
western_conference_away$xG <-as.double(western_conference_away$xG)
western_conference_away$xGA <-as.double(western_conference_away$xGA)
western_conference_away$GF <-as.double(western_conference_away$GF)

western_conference_away <- western_conference_away %>% 
  mutate(AwayxGp90 = xG/MP) %>% 
  mutate(AwayxGAp90 = xGA/MP) %>% 
  mutate(AwayActualvxG = (GF - xG)/MP)


#Trying to graph xG over and under performance

ggplot(eastern_conference)+
  geom_bar(aes(Squad,HomeActualvxG),stat = 'identity')+
  coord_flip()

ggplot(eastern_conference)+
  geom_bar(aes(Squad,AwayActualvxG), stat = 'identity')+
  coord_flip()

ggplot(western_conference_home)+
  geom_bar(aes(Squad,HomeActualvxG),stat = 'identity')+
  coord_flip()

#Combine into all of MLS

xGDifferenceWest <- merge(western_conference_home,western_conference_away,'Squad') %>% 
  select(Squad,HomeActualvxG,AwayActualvxG)
xGDifferenceEast <- eastern_conference %>% 
  select(Squad,HomeActualvxG,AwayActualvxG)
xgDifference <- rbind(xGDifferenceEast,xGDifferenceWest)
xgDifference$Color <- c('#80000A', '#BBC3C6', '#06134E', '#FEF200', '#231F20', '#003087', '#F7B5CD',
                        '#FEF200', '#002B5C', '#E31351', '#6CACE4', '#61259E', '#002D55', '#AB1E2D',
                        '#00B140', '#862633', '#D11241', '#F4911E', '#00245D', '#000000', '#9BCDE4',
                        '#00482B', '#B30838', '#30457A', '#658D1B', '#93B1D7', '#04265C')

ggplot(xgDifference)+
  geom_bar(aes(Squad,HomeActualvxG),stat = 'identity', fill = xgDifference$Color)+
  labs(title = 'Actual Goals vs Expected Goals for MLS Home Teams',
       caption = 'Source: fbref',
       subtitle = 'Per game basis')+
       ylab('Actual Goals - xG per game')+
  coord_flip()

ggplot(xgDifference)+
  geom_bar(aes(Squad,AwayActualvxG),stat = 'identity', fill = xgDifference$Color)+
  labs(title = 'Actual Goals vs Expected Goals for MLS Away Teams',
       caption = 'Source: fbref',
       subtitle = 'Per game basis')+
  ylab('Actual Goals - xG per game')+
  coord_flip()

ggplot(xgDifference)+
  geom_bar(aes(Squad,AwayActualvxG,fill=Squad),stat = 'identity')+
  coord_flip()

mlsColors <- filter(teamcolors,league== 'mls') %>% 
  select(name,primary,secondary)

teamcolors

test <- data.frame(Squad = xgDifference$Squad[1:22.],numbs = 1:22)

ggplot(test)+
  geom_bar(aes(Squad,numbs),stat = 'identity')+
  scale_fill_manual(values =mlsColors$primary)+
  coord_flip()




