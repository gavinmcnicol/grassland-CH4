library(tidyverse)
library(lubridate)
library(amerifluxr)
library(patchwork)

read_site <- function(fp) {
  df <- read_csv(fp, na='-9999') %>% 
    mutate(date = parse_datetime(as.character(TIMESTAMP_START), '%Y%m%d%H%M'))
}

append_annual <- function(df, annual) {
  df %>%
  group_by(year = floor_date(as_date(date), 'year')) %>% 
  summarize(FCH4_an = sum(FCH4_F, na.rm = TRUE)) %>% 
  filter(FCH4_an != 0) %>% 
  mutate(site = df$site[1]) %>% 
  full_join(annual)
}

append_monthly <- function(df, monthly) {
  df %>%
  group_by(month = floor_date(as_date(date), 'month')) %>% 
  summarize(FCH4_mon = sum(FCH4_F, na.rm = TRUE),
            TA_avg = mean(TA, na.rm = TRUE),
            P_mon = sum(P, na.rm = TRUE)) %>% 
  # filter(FCH4_mon != 0) %>% 
  mutate(site = df$site[1]) %>% 
  full_join(monthly)
}

sitelist_gra = c('AT-Neu', 
                 'BW-Nxr',
                 'CH-Cha', 
                 'CN-Hgu', 
                 'NL-Hor', 
                 'SE-Deg', 
                 'US-NGC', 
                 'US-Snd', 
                 'US-Sne')

# import FLUXNET half-hourly files
fp = 'data/FLUXNET-CH4/FLX_AT-Neu_FLUXNET-CH4_2010-2012_1-1/FLX_AT-Neu_FLUXNET-CH4_HH_2010-2012_1-1.csv'
df_ATneu = read_site(fp) %>% 
  mutate(site = 'AT-Neu')

fp = 'data/FLUXNET-CH4/FLX_BW-Nxr_FLUXNET-CH4_2018-2018_1-1/FLX_BW-Nxr_FLUXNET-CH4_HH_2018-2018_1-1.csv'
df_BWnxr = read_site(fp) %>% 
  mutate(site = 'BW-Nxr')

fp = 'data/FLUXNET-CH4/FLX_CH-Cha_FLUXNET-CH4_2012-2016_1-1/FLX_CH-Cha_FLUXNET-CH4_HH_2012-2016_1-1.csv'
df_CHcha = read_site(fp) %>% 
  mutate(site = 'CH-Cha')

fp = 'data/FLUXNET-CH4/FLX_CN-Hgu_FLUXNET-CH4_2015-2017_1-1/FLX_CN-Hgu_FLUXNET-CH4_HH_2015-2017_1-1.csv'
df_CNhgu = read_site(fp) %>% 
  mutate(site = 'CN-Hgu')

fp = 'data/FLUXNET-CH4/FLX_NL-Hor_FLUXNET-CH4_2007-2009_1-1/FLX_NL-Hor_FLUXNET-CH4_HH_2007-2009_1-1.csv'
df_NLhor = read_site(fp) %>% 
  mutate(site = 'NL-Hor')

fp = 'data/FLUXNET-CH4/FLX_SE-Deg_FLUXNET-CH4_2014-2018_1-1/FLX_SE-Deg_FLUXNET-CH4_HH_2014-2018_1-1.csv'
df_USsne = read_site(fp) %>% 
  mutate(site = 'SE-Deg')

fp = 'data/FLUXNET-CH4/FLX_US-NGC_FLUXNET-CH4_2017-2018_1-1/FLX_US-NGC_FLUXNET-CH4_HH_2017-2018_1-1.csv'
df_USngc = read_site(fp) %>% 
  mutate(site = 'US-NGC')

fp = 'data/FLUXNET-CH4/FLX_US-Snd_FLUXNET-CH4_2010-2015_1-1/FLX_US-Snd_FLUXNET-CH4_HH_2010-2015_1-1.csv'
df_USsnd = read_site(fp) %>% 
  mutate(site = 'US-Snd')

fp = 'data/FLUXNET-CH4/FLX_US-Sne_FLUXNET-CH4_2016-2018_1-1/FLX_US-Sne_FLUXNET-CH4_HH_2016-2018_1-1.csv'
df_USsne = read_site(fp) %>% 
  mutate(site = 'US-Sne')

# aggregate annual fluxes
annual <- df_ATneu %>%
  group_by(year = floor_date(as_date(date), 'year')) %>% 
  summarize(FCH4_an = sum(FCH4_F, na.rm = TRUE)) %>% 
  filter(FCH4_an != 0) %>% 
  mutate(site = df_ATneu$site[1])

annual <- append_annual(df_BWnxr, annual)
annual <- append_annual(df_CHcha, annual)
annual <- append_annual(df_CNhgu, annual)
annual <- append_annual(df_NLhor, annual)
annual <- append_annual(df_SEdeg, annual)
annual <- append_annual(df_USngc, annual)
annual <- append_annual(df_USsnd, annual)
annual <- append_annual(df_USsne, annual)

ggplot(annual, aes(x = year)) + 
  geom_point(aes(y=FCH4_an, color=site)) +
  theme_minimal()

# aggregate monthly fluxes
monthly <- df_ATneu %>%
  group_by(month = floor_date(as_date(date), 'month')) %>% 
  summarize(FCH4_mon = sum(FCH4_F, na.rm = TRUE),
            TA_avg = mean(TA, na.rm = TRUE),
            P_mon = sum(P, na.rm = TRUE)) %>% 
  # filter(FCH4_mon != 0) %>% 
  mutate(site = df_ATneu$site[1])

monthly <- append_monthly(df_BWnxr, monthly)
monthly <- append_monthly(df_CHcha, monthly)
monthly <- append_monthly(df_CNhgu, monthly)
monthly <- append_monthly(df_NLhor, monthly)
monthly <- append_monthly(df_SEdeg, monthly)
monthly <- append_monthly(df_USngc, monthly)
monthly <- append_monthly(df_USsnd, monthly)
monthly <- append_monthly(df_USsne, monthly)

# FCH4, P, and TA vs time
pFCH4 <- ggplot(monthly, aes(x = month)) + 
  geom_point(aes(y=FCH4_mon, color=site)) +
  theme_minimal()

pTA <- ggplot(monthly, aes(x = month)) + 
  geom_point(aes(y=TA_avg, color=site)) +
  theme_minimal()

pP <- ggplot(monthly, aes(x = month)) + 
  geom_point(aes(y=P_mon, color=site)) +
  theme_minimal()

pFCH4 / pTA / pP

# FCH4 on TA and P axes

ggplot(monthly, aes(x=TA_avg, y=P_mon)) +
  geom_point(aes(size = FCH4_mon, color = site))

# SE-deg FCH4_F vs WTD

p1 <- ggplot(df_SEdeg, aes(date, FCH4_F)) + 
  geom_point()

p2 <- ggplot(df_SEdeg, aes(date, y=WTD)) +
  geom_line()

p1 / p2

# US-Snd FCH4_F vs WTD

p1 <- ggplot(df_USsnd, aes(date, FCH4_F)) + 
  geom_point()

p2 <- ggplot(df_USsnd, aes(date, y=WTD)) +
  geom_line()

p1 / p2

# US-Sne FCH4_F vs WTD

p1 <- ggplot(df_USsne, aes(date, FCH4_F)) + 
  geom_point()

p2 <- ggplot(df_USsne, aes(date, y=WTD)) +
  geom_line()

p1 / p2

# junk plots, old

ggplot(df_CHcha, aes(x=date)) + 
  geom_point(aes(y=FCH4))

ggplot(df_CNhgu, aes(x=date)) + 
  geom_point(aes(y=FCH4))

ggplot(df_ATneu, aes(x=date)) + 
  geom_point(aes(y=FCH4))

ggplot(df_CHcha, aes(x=TA, y=P, color=FCH4)) + geom_point()

df_CHcha %>% 
  filter(!is.na(FCH4)) %>% 
  ggplot(aes(TA, P, color=FCH4)) + geom_point() +
  theme_minimal()
# ggsave('Documents/579/grassland-CH4/TvsPvsFCH4_CH-Cha.png')

df_CHcha %>% 
  filter(!is.na(FCH4_F)) %>%
  ggplot(aes(TA, P, color=FCH4_F)) + 
  geom_point() +
  theme_minimal()

df_CHcha %>% 
  filter(!is.na(FCH4_F)) %>%
  ggplot(aes(date, FCH4_F)) + 
  geom_point() +
  theme_minimal()

df_ATneu %>% 
  filter(is.na(FCH4_F))

df_CHcha %>%  
  filter(is.na(df_CHcha['FCH4_F']))

annual <- df_CHcha %>% 
  group_by(floor_date(as_date(date), 'year')) %>% 
  summarize(FCH4_an = sum(FCH4_F, na.rm = TRUE)) %>% 
  filter(FCH4_an != 0)

df_CHcha %>% 
  group_by(year(date)) %>% 
  summarize(FCH4_Fansum=sum(FCH4_F))

df_CNhgu %>% 
  group_by(year(date)) %>% 
  summarize(FCH4_Fansum=sum(FCH4_F))

df_ATneu %>% 
  group_by(year(date)) %>% 
  summarize(FCH4_Fansum=sum(FCH4_F))

# # An example of unzipped BASE files from the above zipped file
# f_CHCha <- system.file("extdata", 'FLX_CH-Cha_FLUXNET-CH4_HH_2012-2016_1-1.csv', package = "amerifluxr")
# 
# # read the BASE from a csv file, with additional parsed time-keeping columns
# base2 <- amf_read_base(
#   file = f_CHCha,
#   unzip = FALSE,
#   parse_timestamp = TRUE
# )
# 
# df_CHcha <- read_csv(fp, na='-9999') %>% 
#   mutate(date = parse_datetime(as.character(TIMESTAMP_START), '%Y%m%d%H%M'),
#          site = 'CH-Cha')
# df_CNhgu <- read_csv(fp, na='-9999') %>% 
#   mutate(date = parse_datetime(as.character(TIMESTAMP_START), '%Y%m%d%H%M'),
#          site = 'CN-Hgu')
# df_ATneu <- read_csv(fp, na='-9999') %>% 
#   mutate(date = parse_datetime(as.character(TIMESTAMP_START), '%Y%m%d%H%M'))
# annual <- df_CNhgu %>%
#   group_by(year = floor_date(as_date(date), 'year')) %>% 
#   summarize(FCH4_an = sum(FCH4_F, na.rm = TRUE)) %>% 
#   filter(FCH4_an != 0) %>% 
#   mutate(site = df_CNhgu$site[1]) %>% 
#   full_join(annual)
# 
# annual <- df_ATneu %>%
#   group_by(year = floor_date(as_date(date), 'year')) %>% 
#   summarize(FCH4_an = sum(FCH4_F, na.rm = TRUE)) %>% 
#   filter(FCH4_an != 0) %>% 
#   mutate(site = df_ATneu$site[1]) %>% 
#   full_join(annual)
