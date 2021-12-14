library(tidyverse)
library(stringr)
library(dplyr)
library(lme4)
library(broom)
library(scales)
library(readxl)
library(broom.mixed)
library(ggplot2)

set.seed(416)
#import
mask <- read_csv("nytimes_covid_masks.csv")   
colleges <- read_csv("nytimes_covid_college_cases.csv")
recent <- read_csv("us-counties-recent.csv")  
election <- read_csv("countypres_2000-2020.csv")
calicoll <- read_excel("colleges.xlsx")

#county_state (for joining), matching fips, cleaning imported data
mask <- mask %>%
  mutate(fips = str_pad(COUNTYFP, 5, side = c("left"), pad = "0")) %>%
  select(fips, NEVER, ALWAYS) %>%
  rename_all(str_to_lower)

colleges <- colleges %>%
  mutate(county_state = paste(county, state, sep = ", ")) %>%
  select(state, county, city, ipeds_id, college, cases, cases_2021, county_state)

recent <- recent %>%
  mutate(county_state = paste(county, state, sep = ", "))

election <- election %>%
  mutate(fips = str_pad(county_fips, 5, side = c("left"), pad = "0")) %>%
  select(fips, candidate, party, candidatevotes, totalvotes, fips)

calicoll <- calicoll %>%
  mutate(fips = str_pad(fips, 5, side = c("left"), pad = "0")) %>%
  mutate(fips = as.character(fips))

#full joins - want colleges, election, calicoll and recent
recentcoll <- full_join(recent, colleges, by = "county_state") #recent, colleges
collrecoll <- full_join(recentcoll, election, by = "fips") #recent, colleges, election
addcali <- full_join(collrecoll, calicoll, by = "fips") #recent, colleges, election, Cali - uses excel
addmask <- full_join(addcali, mask, by = "fips")

#Creating and cleaning dataframe cdf, currently not filtered to date 2021-11-29 (most recent)
cdf <- addmask %>%
  na.omit(college.x) %>%
  na.omit(`total enrollment`) %>%
  select(date, college.x, city.x, state.x, county.x, `county pop`, fips, cases.x, deaths, ipeds_id.x, `total enrollment`, `student vax`,
         cases.y, cases_2021, candidate, party, candidatevotes, totalvotes, always, never) %>%
  rename(college = college.x) %>%
  rename(ipeds_id = ipeds_id.x) %>%
  rename(city = city.x) %>%
  rename(county = county.x) %>%
  rename(state = state.x) %>%
  rename(cas_cty = cases.x) %>%
  rename(dth_cty = deaths) %>%
  rename(cas_coll = cases.y) %>%
  rename(cas_coll_21 = cases_2021) %>%
  #filter(date == "2021-11-29") %>%
  mutate(partyratio = candidatevotes/totalvotes) %>%
  mutate(cs_cty_ratio = (cas_cty/`county pop`)) %>%
  mutate(dth_cty_ratio = (dth_cty/`county pop`)) %>%
  mutate(cs_collratio = (cas_coll/`total enrollment`)) %>%
  mutate(cs_coll_21_ratio = (cas_coll_21/`total enrollment`)) %>%
  group_by(college) 

##################################finding 16 colleges in California, based around LA and San Francisco
california <- cdf %>%  #75
  filter(state == "California") %>%
  nest()

sanfran <- cdf %>%  #4
  filter(city == "San Francisco") %>%
  nest()

la <- cdf %>%   #5
  filter(city == "Los Angeles") %>%
  nest()

sacramento <- cdf %>%
  filter(city == "Sacramento") %>%
  nest()

berkeley <- cdf %>%
  filter(city == "Berkeley") %>%
  nest()

ucdavis <- cdf %>%
  filter(city == "Davis") %>%
  nest()

stockton <- cdf %>%
  filter(city == "Stockton") %>%
  nest()

sj <- cdf %>%
  filter(city == "San Jose") %>%
  nest()

lbeach <- cdf %>%
  filter(city == "Long Beach") %>%
  nest()

malibu <- cdf %>%
  filter(city == "Malibu") %>%
  nest()

################  INPUTTED IPEDS_ID FOR 16, ncdf; filtered out large duplicates, have to filter to 15 colleges
################ Using the 15 colleges selected from LA and San Francisco for which I have enrollment info, minus 2 no vax info

ncdf <- cdf %>%
  filter(ipeds_id == c("110617", "110583", "110608", "121150", "120254", "122597", "122755", 
                       "110635", "110644", "110662", "123961", "120883", "122612")) %>%
  group_by(ipeds_id) %>%
  mutate(n = n()) %>%
  select(date, college, ipeds_id, cas_cty, dth_cty, `county pop`, cas_coll, cas_coll_21, 
         `total enrollment`, `student vax`, party, candidate, candidatevotes, totalvotes, always, never, partyratio, cs_cty_ratio, 
         dth_cty_ratio, cs_collratio, cs_coll_21_ratio, n) %>%
  nest() %>%
  unnest()

ncdf

################                           Models

#Mixed effects
mod_ncdf <- ncdf %>%
  rename(never_mask = never)


mod <- mod_ncdf %>%
  lmer(dth_cty_ratio ~ 1 + cs_collratio + `student vax` + party + never_mask + cs_cty_ratio + cs_coll_21_ratio + (1 | ipeds_id), data = .)

mod2 <- mod_ncdf %>%
  lmer(dth_cty_ratio ~ 1 + `student vax` + (1 | ipeds_id), data = .)

mod3 <- cdf %>%
  lmer(dth_cty_ratio ~ 1 + `student vax`  + never + (1 | ipeds_id), data = .)

summary(mod3)
summary(mod)
summary(mod2)

mod3 %>%
  tidy()

#GLM
library(glmer)
newmod <- ncdf %>%
  glm(dth_cty_ratio ~ cs_collratio + `student vax` + party + never + cs_cty_ratio + cs_coll_21_ratio, data = .)

newmod2 <- ncdf %>%
  glm(dth_cty_ratio ~ `student vax`, data = .)

newmod3 <- ncdf %>%
  glm(dth_cty_ratio ~ `student vax` + party + never, data = .)
summary(newmod3)

nnncdf <- cdf %>%
  mutate(st_vax_count = `student vax`*`total enrollment`) %>%
  mutate(mask_count = never*`county pop`)
###############################     Poisson
gpoi <- lmer(dth_cty_ratio ~ 1 + `student vax` + never + party + (1|ipeds_id) , data = nnncdf)
summary(gpoi)
tidy(gpoi)
###############                         Plots
#ratios: , always, never, partyratio, cs_cty_ratio, dth_cty_ratio, cs_collratio, cs_coll_21_ratio
#counts: cas_cty, dth_cty, `county pop`, cas_coll, cas_coll_21, `total enrollment`, candidatevotes, totalvotes
#categorical: party, candidate

####################################### Model
mod <- ncdf %>%
  lmer(dth_cty_ratio ~ 1 + cs_collratio + party + never + cs_cty_ratio + cs_coll_21_ratio + (1 | ipeds_id), data = .)

newmod <- ncdf %>%
  glm(dth_cty_ratio ~ cs_collratio + party + never + cs_cty_ratio + cs_coll_21_ratio, data = .)

##################################################Facet plot
library(broom.mixed)
mod3 %>%
  augment(data = ncdf) %>%
  ggplot(aes(x = `student vax`, color = party)) +
  geom_line(aes(y = dth_cty_ratio), linetype = 1) +
  geom_line(aes(y = .fitted), linetype = 2) +
  geom_line(aes(y = .fixed), linetype = 3) +
  labs(x = "Percent Students Vaccinated", y = "County Death Rate", title = "County Death Rate and Political Party") +
  facet_wrap(~ college, ncol = 4)

###################################################Pairs plot
library(GGally)
exposure <- ncdf %>%
  select(`student vax`, dth_cty_ratio) %>%
  ggpairs()

counts <- ncdf %>%
  select(cas_coll, cas_coll_21, cas_cty, dth_cty) %>%
  ggpairs()

ratios <- ncdf %>%
  select(`student vax`, never, partyratio, cs_cty_ratio, dth_cty_ratio, cs_collratio, cs_coll_21_ratio) %>%
  ggpairs()

###################################################Corr Plot
library(ggcorrplot)

cormod2 <- ncdf %>%
  select(ipeds_id, dth_cty_ratio, cs_collratio, cs_coll_21_ratio, cs_cty_ratio, `student vax`, never, partyratio) %>%
  mutate(dth_cty_ratio = as.numeric(dth_cty_ratio)) %>%
  mutate(ipeds_id = as.numeric(ipeds_id)) %>%
  rename(`county deaths` = dth_cty_ratio) %>%
  rename(`college cases` = cs_collratio) %>%
  rename(`college cases 2021` = cs_coll_21_ratio) %>%
  rename(`county cases` = cs_cty_ratio) %>%
  rename(`student vaccination rate` = `student vax`) %>%
  rename(`never use mask` = never) %>%
  rename(`political party` = partyratio)

cormod3 <- cormod2 %>%
  select(ipeds_id, `student vaccination rate`, `county cases`, `college cases`, `county deaths`)

cplot2 <- cormod2 %>%
  cor(x = cormod2) %>%
  ggcorrplot(method = "circle", type = "lower", title = "Student Vaccination and COVID-19 Cases and Deaths")

############################################################### Old plots
ncdf %>%
  ggplot(aes(x = `student vax`, y = dth_cty_ratio, group = ipeds_id,
             color = party)) +
  geom_line() +
  ylab("")


library(ggbeeswarm)
ncdf %>%
  group_by(ipeds_id) %>%
  ggplot(aes(x = party, y = dth_cty_ratio, group = ipeds_id)) +
  geom_violin() +
  geom_boxplot(width = 0.1)


ncdf %>%
  ggplot(aes(x = ipeds_id, y = dth_cty_ratio)) +
  geom_boxplot()


ncdf %>%
  ggplot(aes(x = always)) +
  geom_histogram()
