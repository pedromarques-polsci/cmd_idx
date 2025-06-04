# Packages --------------------------------------------------------------
library(lmtest)
library(plm)
library(comtradr)
library(concordance)
library(countrycode)
library(ggplot2)
library(haven)
library(janitor)
library(readxl)
library(tidyverse)
library(xts)
library(zoo)
library(wbstats)

# Potential conflicts
# dplyr::filter(), Hmisc::sumarize()

# SUMMARY --------------------------------------------------------------
# The current script is divided by three major parts 
# The first one builds our commodity price dataset by aggregating three sources.
# The second part extracts commodity trade data directly from UN Comtrade
# dataset through its API.
# The final one ties the previous databases together and builds up a Terms of 
# Trade Index inspired by Gruss and Kebhaj's (2019) methodology,
# although our index has some minor, yet important differences.
# The current version of this code is 19/03/2024 13:52

# 1. COMMODITY PRICES --------------------------------------------------
## 1.1 Source - IMF ----------------------------------------------------
commodity_code <- read_xlsx('raw_data/commodity_codes.xlsx', sheet = 1, skip = 1) %>%
  rename('commodity' = 1,
         'price_code' = 2)

# It's better work with a index than grepl, 
# since each of their location match the corresponding commodity
index <- 1:nrow(commodity_code)

commodity_code <- commodity_code %>%
  mutate(id = index) %>%
  relocate(id) %>%
  filter(!id %in% c(90, 96, 98:100, 103:106, 109, 111)) # duplicates

commodity_code[commodity_code$id == '23', 'commodity'] <- 'Hard Logs'
commodity_code[commodity_code$id == '26', 'commodity'] <- 'Soft Logs'
commodity_code[commodity_code$id == '24', 'commodity'] <- 'Hard Sawn'
commodity_code[commodity_code$id == '27', 'commodity'] <- 'Soft Sawn'

commodity_full <- read_xlsx('raw_data/commodity_full.xlsx', sheet = 1, skip = 1) %>%
  rename('commodity' = 1,
         'fullname' = 2)

commodity_full <- commodity_full %>%
  mutate(id = index) %>%
  relocate(id) %>%
  filter(!id %in% c(90, 96, 98:100, 103:106, 109, 111))

commodity_full[commodity_full$id == '23', 'commodity'] <- 'Hard Logs'
commodity_full[commodity_full$id == '26', 'commodity'] <- 'Soft Logs'
commodity_full[commodity_full$id == '24', 'commodity'] <- 'Hard Sawn'
commodity_full[commodity_full$id == '27', 'commodity'] <- 'Soft Sawn'

commodity_prices <- read_xlsx('raw_data/cmd_raw_prices.xlsx', sheet = 1, skip = 1) %>%
  select(-seq(from = 3, to = 69, by = 2)) %>%
  rename('commodity' = 1)

commodity_prices <- commodity_prices %>%
  mutate(id = index) %>%
  relocate(id) %>%
  filter(!id %in% c(90, 96, 98:100, 103:106, 109, 111))

# One cannot use grepl here, that's why working of an index is better
commodity_prices[commodity_prices$id == '23', 'commodity'] <- 'Hard Logs'
commodity_prices[commodity_prices$id == '26', 'commodity'] <- 'Soft Logs'
commodity_prices[commodity_prices$id == '24', 'commodity'] <- 'Hard Sawn'
commodity_prices[commodity_prices$id == '27', 'commodity'] <- 'Soft Sawn'

commodity_bind <- commodity_prices %>%
  left_join(commodity_code) %>%
  left_join(commodity_full) %>%
  relocate(id, commodity, price_code, fullname) %>%
  pivot_longer(cols = 5:38,
               names_to = 'year',
               values_to = 'cmd_price')

# We aggregate wood prices, given that trade data does not discriminate 
# between Soft/Hard Sawnwood/Logs

wood <- commodity_bind %>% 
  filter(price_code == 'PTIMB') %>% # PTIMB is just a vessel
  mutate(price_code = 'PWMEAN',
         fullname = 'Wood Prices Mean (Soft/Hard Sawnwood, Soft/Hard Logs',
         commodity = 'Wood')
  
logsk <- commodity_bind %>% 
  filter(price_code == 'PLOGSK')

sawmal <- commodity_bind %>% 
  filter(price_code == 'PSAWMAL')

logore <- commodity_bind %>% 
  filter(price_code == 'PLOGORE')

sawore <- commodity_bind %>% 
  filter(price_code == 'PSAWORE')

# The aggregation is done by taking the mean price
for (i in 1:34) {
  wood[i,6] <- (logsk[i,6] + sawmal[i,6] + logore[i,6] + sawore[i,6])/4
}

commodity_price_df <- commodity_bind %>% bind_rows(wood)

## 1.2 Source - UNCTAD -----------------------------------------------------
unctad_prices <- read_csv('raw_data/unctad_commodity_prices.csv') %>% 
  clean_names() %>% 
  pivot_longer(cols = !commodity_label,
               names_to = "year", values_to = "cmd_price") %>% 
  filter(grepl("Tobacco", commodity_label)) %>% 
  mutate(year = str_extract(year, "[0-9]+"),
         price_code = "PTOBAC") %>% 
  rename(fullname = commodity_label)

## 1.3 Source - FRED -----------------------------------------------------------
orange_prices <- read_xlsx('raw_data/fred_orange_prices.xlsx', sheet = 1, skip = 10) %>%
  rename(year = 1,
         cmd_price = 2) %>%
  mutate(year = format(year, format="%Y"),
         commodity = 'Orange',
         price_code = 'PORANGUSDM',
         fullname = 'U.S. Dollars per Pound')

## 1.4 Joining both datasets -----------------------------------------------
commodity_prices_final <- commodity_price_df %>%
  bind_rows(unctad_prices, orange_prices)

#commodity_prices_final[commodity_prices_final$price_code == '240100.01', 'price_code'] <- 'PTOBAC'
commodity_prices_final[commodity_prices_final$price_code == 'PTOBAC', 'id'] <- 111
commodity_prices_final[commodity_prices_final$price_code == 'PTOBAC', 'commodity'] <- 'Tobacco'
commodity_prices_final[commodity_prices_final$price_code == 'PORANGUSDM', 'id'] <- 112

commodity_prices_final_df <- commodity_prices_final %>% 
  mutate(year = as.numeric(year))

## 1.5 Exporting prices dataset --------------------------------------------
saveRDS(commodity_prices_final_df, "final_data/commodity_prices_final_df.RDS")
write_excel_csv2(commodity_prices, "final_data/commodity_prices_final_df.csv", 
                 na = '')

# 2. COMMODITY TRADE  ------------------------------------------------------
# Harmonized Commodity Description and Coding System
hs_code_vector <- c(
  # Agricultural materials
  "5201", "41", "4001", "4401", "4403", "4406", "2401", "5101", 
  
  # Food and beverages
  "0803", '1003', '0201', '0105', '1801', '0901', '1005', '160411', 
  '230120', '1202', '020430', '020410', '020423', '1509', '080510', '1511', 
  '0203', '1514', '1006', '030613', '2304', '1507', '1201', '1701', '1512', 
  '0902', '1001', 
  
  # Energy
  '2709', '2701', '2711',
  
  # Metals
  '2606', '7601', '2603', '7402', '7108', '2601', '2607', '2604',	
  '2609', '8001', '2612', '2608', '7901'
)

price_code_vector <- c("PCOTTIND", "PHIDE", "PRUBB", # Commodity price codes
                       'PWMEAN', 'PWMEAN', 'PWMEAN',
                       'PTOBAC', 'PWOOLC',

                       # Food and bevarages
                       'PBANSOP', 'PBARL', 'PBEEF', 'PPOULT', 'PCOCO', 'PCOFFOTM', 'PMAIZMT',
                       'PSALM', 'PFSHMEAL', 'PGNUTS', 'PLAMB', 'PLAMB', 'PLAMB', 'POLVOIL', 'PORANGUSDM',
                       'PPOIL', 'PPORK', 'PROIL', 'PRICENPQ', 'PSHRI', 'PSMEA', 'PSOIL',
                       'PSOYB', 'PSUGAISA', 'PSUNO', 'PTEA', 'PWHEAMT',

                       # Energy
                       'POILAPSP', 'PCOALAU', 'PNGASEU',

                       # Metals
                       'PALUM', 'PALUM', 'PCOPP', 'PCOPP', 'PGOLD', 'PIORECR', 'PLEAD',
                       'PNICK', 'PTIN', 'PTIN', 'PURAN', 'PZINC', 'PZINC')

trade_price_bind <- data.frame(cmdCode = hs_code_vector,
                               price_code = price_code_vector)

## 2.1 Data Transformation ------------------------------------------------
alltrade <- readRDS('raw_data/all_trade_third_imput.RDS')

# LATAM Country Codes
latam_iso <- readRDS('raw_data/latam_iso.RDS')

setdiff(latam_iso$iso3c, alltrade$reporterISO) 
# As can be seen, our trade dataset does not contain info from Puerto Rico

latam_trade <- alltrade %>%
  filter(reporterISO %in% latam_iso$iso3c)

latam_trade %>% 
count(reporterDesc, reporterISO, period, cmdCode, flowCode) %>% filter(n>1)

latam_trade_flow <- latam_trade %>% # Generating net exports
  reframe(
    primaryValue = 
      primaryValue[flowCode == "X"] - primaryValue[flowCode == "M"],
    flowCode = "NX",
    .by = c(reporterDesc, reporterISO, period, cmdCode)) %>% 
  bind_rows(latam_trade) %>% 
  group_by(reporterISO, period, cmdCode) %>% 
  arrange(flowCode, .by_group = TRUE) %>% 
  group_by(cmdCode) %>%
  fill(cmdDesc, .direction = "downup") %>%
  ungroup()

latam_trade_flow %>% 
  count(reporterDesc, reporterISO, period, cmdCode, flowCode) %>% filter(n>1)

latam_trade_df <- latam_trade_flow %>% 
  left_join(y = trade_price_bind, join_by(cmdCode))

# Counting missings
latam_trade_df %>% 
  filter(reporterISO %in% latam_iso$iso3c, flowCode == "NX") %>% 
  group_by(reporterISO) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% View()

latam_trade_df %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period, flowCode) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + 
  xlab('Ano') + ylab('Missings') +
  theme_classic() +
  geom_line(aes(linetype = flowCode)) +
  geom_point()

latam_trade_df %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period, reporterISO, flowCode) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + 
  facet_wrap(~factor(reporterISO, levels=c(unique(reporterISO))), 
             drop = T, ncol = 4, scales = "free_y") +
  xlab('Ano') + ylab('Missings') +
  theme_minimal() +
  geom_line(linewidth = 0.8, aes(linetype = flowCode)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

latam_trade_df %>% 
  count(reporterDesc, reporterISO, period, cmdCode, flowCode) %>% filter(n>1)

## 2.2 Exporting Latam Trade dataset ---------------------------------------
latam_trade_df %>%
  saveRDS("final_data/latam_trade_df.RDS")

# 3. INDEX BUILDING --------------------------------------------------------
# Only run this chunk if you haven't run all the code above.
commodity_prices_final_df <- readRDS("final_data/commodity_prices_final_df.RDS")
latam_trade_df <- readRDS('final_data/latam_trade_df.RDS')
latam_iso <- readRDS('raw_data/latam_iso.RDS')

# USA's Consumer Index Price
cpi_usa <- wb_data(indicator = "FP.CPI.TOTL", country = "United States", 
           start_date = 1980, end_date = 2022) %>% 
  clean_names() %>% 
  select(country, date, fp_cpi_totl) %>% 
  rename(cpi = fp_cpi_totl)

sum_cmd_df <- latam_trade_df %>% 
  filter(flowCode == 'X') %>% 
  group_by(reporterISO, period) %>% 
  reframe(sum_cmd = sum(primaryValue, na.rm = T))

idx_price_label <- c("PCOTTIND", "POILAPSP")

cmd_x_weight <- latam_trade_df %>% 
  mutate(cmdCode = ifelse(cmdCode == 'TOTAL', 0, cmdCode),
         price_code = ifelse(cmdCode == 0, 'TOTAL', price_code)) %>% 
  filter(flowCode == 'X') %>% 
  left_join(sum_cmd_df, join_by(reporterISO, period)) %>% 
  
  # Grouping by commodity international price code
  group_by(reporterDesc, reporterISO, period, price_code) %>% 
  
  # Aggregating HSCode commodities according to their international price code
  reframe(cmd_trade = sum(primaryValue), sum_cmd = first(sum_cmd)) %>% 
  ungroup() %>% 
  group_by(reporterISO, period) %>% 
  mutate(yweight = cmd_trade/sum_cmd) %>% 
  ungroup() %>% 
  left_join(commodity_prices_final_df, join_by(period == year, price_code)) %>% 
  left_join(cpi_usa, join_by(period == date))

cmd_x_rolling <- cmd_x_weight %>% 
  arrange(period, reporterISO, price_code) %>% 
  mutate(ma_weight = rollmean(lag(yweight), 3, na.pad = TRUE, align = "right"),
         .by = c(reporterISO, price_code)) %>% 
  group_by(period) %>% 
  mutate(cmd_price_real =
           ifelse(
             price_code %in% idx_price_label,
             cmd_price * (100/cpi), cmd_price)) %>% 
  group_by(reporterISO, period) %>% 
  mutate(sum_cmd_real = sum_cmd * (100/cpi)) %>% 
  group_by(reporterISO, period, price_code) %>% 
  mutate(cmd_trade_real = cmd_trade * (100/cpi)) %>% 
  ungroup() %>% 
  select(reporterISO, reporterDesc, period, price_code, commodity,
         fullname, cmd_trade, cmd_trade_real, sum_cmd, sum_cmd_real,
         cmd_price, cmd_price_real, yweight, ma_weight, cpi) %>% 
  group_by(price_code) %>%
  fill(commodity, .direction = "downup") %>% 
  fill(fullname, .direction = "downup") %>% 
  ungroup()

cmd_final_df <- cmd_x_rolling %>% 
  group_by(reporterISO, period) %>% 
  reframe(iv_y = sum(yweight * cmd_price_real, na.rm = T),
          iv_ma = sum(ma_weight * cmd_price_real, na.rm = T),
          sum_cmd_real = first(sum_cmd_real)) %>% 
  mutate(iv_y = ifelse(iv_y == 0, NA, iv_y),
         iv_ma = ifelse(iv_ma == 0, NA, iv_ma))

write_rds(cmd_final_df, "final_data/cmd_final_df.rds")

cmd_final_df %>% 
  ggplot(aes(x = log(iv_y), y = log(sum_cmd_real))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = T)

cmd_final_pdf <- cmd_final_df %>% 
  pdata.frame(index = c("reporterISO", "period"))

cmd_final_pdf %>% 
  plm(formula = log(sum_cmd_real) ~ log(iv_y), model = "within",
      effect = "individual") %>% 
  coeftest(., vcov = vcovHC(., type = "HC3", cluster = "group"))

lm(formula = log(sum_cmd_real) ~ log(iv_y), data = cmd_final_df) %>% 
  summary()