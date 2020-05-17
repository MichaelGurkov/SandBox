library(tidyverse)

library(DescTools)

raw_df = read_rds(paste0(file.path(Sys.getenv("USERPROFILE"),fsep="\\"),
                         "\\OneDrive - Bank Of Israel",
                         "\\Data\\BoI\\Corp Bond\\2000.Rds"))
df = raw_df %>% 
  select(-SECURITY_MAIN_TYPE_HEB, -SECURITY_MAIN_TYPE, - MATURITY_DATE, - HEBREW_FULL_NAME) %>% 
  rename(Date = DATE_VALUE,
         Sec_ID = SECURITY_IDENT_NUM_TASE,
         Market_Cap = MARKET_VALUE,
         Turnover = TURNOVER,
         Volume = VOLUME,
         Linkage = LINKAGE_TYPE,
         Yield_Close = YLD_CLOSE_BRUTO,
         Rating = COMBINED_RANKING,
         Close_Rate = CLOSE_RATE)

df = df %>% 
  filter(Rating %in% paste0(rep(c("A","AA"),3),c("-","","+"))) %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate_at(vars(Volume, Market_Cap, Turnover),.funs = list(~. * 10 ^ (-6)))

avg_sec_df = df %>% 
  group_by(Sec_ID) %>% 
  summarise_at(vars(Market_Cap, Volume, Turnover),.funs = list(~mean(.,na.rm = TRUE)))

# Filter liquid bonds

liquid_bonds = avg_sec_df %>% 
  arrange(desc(Volume)) %>% 
  select(Sec_ID) %>% 
  slice(1:300) %>% 
  unlist()

liquid_df = df %>% 
  filter(Sec_ID %in% liquid_bonds) %>% 
  filter(!is.na(Volume))

annual_df = liquid_df %>% 
  group_by(Date) %>% 
  summarise_at(vars(Volume, Turnover, Market_Cap),
               .funs = list(~mean(., na.rm = TRUE))) %>% 
  mutate_at(vars(Volume, Turnover, Market_Cap),
            .funs = list(~Winsorize(.,na.rm = TRUE)))


ggplot(annual_df) + 
  geom_line(aes(x = Date, y = Market_Cap / mean(Market_Cap) * 3)) + 
  geom_line(aes(x = Date, y = log(Volume + 1)), col = "blue") + 
  theme_bw()

