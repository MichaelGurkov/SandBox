library(tidyverse)

library(scales)

library(readxl)

df = read_xlsx(paste0(file.path(Sys.getenv("USERPROFILE"),fsep="\\"),
                      "\\Documents\\Data\\BOI\\FX_balance_USD_11_2019.xlsx"),
                      skip = 15,col_names = c("Date","FX_balance"))

df = df %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))

ggplot(df, aes(x = Date, y = FX_balance / 1000)) + 
  geom_line() + 
  scale_y_continuous(labels = dollar) + 
  labs(x = "", y = "", title = "FX balance  \n (Billions of $)") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))
