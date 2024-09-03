# Packages --------------------------------------------------------------
install.packages("devtools")
devtools::install_github("ropensci/comtradr@dev")

if(require(comtradr) == F) install.packages('comtradr'); require(comtradr)
if(require(concordance) == F) install.packages('concordance'); require(concordance)
if(require(countrycode) == F) install.packages('countrycode'); require(countrycode)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(haven) == F) install.packages('haven'); require(haven)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(xts) == F) install.packages('xts'); require(xts)
if(require(zoo) == F) install.packages('zoo'); require(zoo)

# 1. Commodity Trade Data -------------------------------------------------
## 1.1 Set Up -------------------------------------------------------------

# Reference tables
hs_stic_table <- hs_sitc3
hs_table <- comtradr::ct_get_ref_table('HS')
s3_table <- comtradr::ct_get_ref_table('S3')

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

# 2. Data extraction ---------------------------------------------------------
get_data <- function(r, p, y) {
  ct_get_data(
    reporter = r,
    flow_direction = c('import', 'export'),
    partner = p,
    frequency = 'A',
    start_date = y,
    end_date = y,
    commodity_classification = 'HS',
    commodity_code = c(hs_code_vector, "TOTAL")
  ) 
}

# You must get an UN Comtrade API key in order to extract the data.

# Although there are free subscriptions to API, you might have to create several accounts in order to download all the data in one day, since there is a quota limit

# See more here: https://uncomtrade.org/docs/api-subscription-keys/
# And here: https://uncomtrade.org/docs/subscriptions/

set_primary_comtrade_key("put your API Primary Key here")
set_primary_comtrade_key("492d1f5010a943099f2318c1cbae6a52")
comtradr::country_codes %>% View

## 2.1 Aggregate Trade -----------------------------------------------------
all_trade <- map(1988:2020, ~get_data("all", "World", .x)) %>% list_rbind()

latam_iso <- readRDS('final_data/db_socialx_pcp.RDS') %>%
 select(iso3c) %>%
 unique()
 
## 2.2 Bilateral Trade -----------------------------------------------------
# Since I don't have a premium subscription myself, I had to partition my code in several lines in order to circumvent quota limits

bi_trade_arg <- map(1988:2020, ~get_data("all", "ARG", .x)) %>% list_rbind()
bi_trade_bhs <- map(1988:2020, ~get_data("all", "BHS", .x)) %>% list_rbind()
bi_trade_bol <- map(1988:2020, ~get_data("all", "BOL", .x)) %>% list_rbind()
bi_trade_bra <- map(1988:2020, ~get_data("all", "BRA", .x)) %>% list_rbind()
bi_trade_brb <- map(1988:2020, ~get_data("all", "BRB", .x)) %>% list_rbind()
bi_trade_chl <- map(1988:2020, ~get_data("all", "CHL", .x)) %>% list_rbind()
bi_trade_col <- map(1988:2020, ~get_data("all", "COL", .x)) %>% list_rbind()
bi_trade_cri <- map(1988:2020, ~get_data("all", "CRI", .x)) %>% list_rbind()
bi_trade_cub <- map(1988:2020, ~get_data("all", "CUB", .x)) %>% list_rbind()
bi_trade_dom <- map(1988:2020, ~get_data("all", "DOM", .x)) %>% list_rbind()
bi_trade_ecu <- map(1988:2020, ~get_data("all", "ECU", .x)) %>% list_rbind()
bi_trade_gtm <- map(1988:2020, ~get_data("all", "GTM", .x)) %>% list_rbind()
bi_trade_guy <- map(1988:2020, ~get_data("all", "GUY", .x)) %>% list_rbind()
bi_trade_hnd <- map(1988:2020, ~get_data("all", "HND", .x)) %>% list_rbind()
bi_trade_hti <- map(1988:2020, ~get_data("all", "HTI", .x)) %>% list_rbind()
bi_trade_jam <- map(1988:2020, ~get_data("all", "JAM", .x)) %>% list_rbind()
bi_trade_mex <- map(1988:2020, ~get_data("all", "MEX", .x)) %>% list_rbind()
bi_trade_nic <- map(1988:2020, ~get_data("all", "NIC", .x)) %>% list_rbind()
bi_trade_pan <- map(1988:2020, ~get_data("all", "PAN", .x)) %>% list_rbind()
bi_trade_per <- map(1988:2020, ~get_data("all", "PER", .x)) %>% list_rbind()
bi_trade_pri <- map(1988:2020, ~get_data("all", "PRI", .x)) %>% list_rbind()
bi_trade_pry <- map(1988:2020, ~get_data("all", "PRY", .x)) %>% list_rbind()
bi_trade_slv <- map(1988:2020, ~get_data("all", "SLV", .x)) %>% list_rbind()
bi_trade_tto <- map(1988:2020, ~get_data("all", "TTO", .x)) %>% list_rbind()
bi_trade_ury <- map(1988:2020, ~get_data("all", "URY", .x)) %>% list_rbind()
bi_trade_ven <- map(1988:2020, ~get_data("all", "VEN", .x)) %>% list_rbind()

## 2.3 Exporting All Trade dataset ----------------------------------------
all_trade %>%
  saveRDS("raw_data/all_trade.RDS")

bi_trade_arg %>%
  saveRDS("raw_data/bi_trade_arg.RDS")
bi_trade_bhs %>%
  saveRDS("raw_data/bi_trade_bhs.RDS")
bi_trade_bol %>%
  saveRDS("raw_data/bi_trade_bol.RDS")
bi_trade_bra %>%
  saveRDS("raw_data/bi_trade_bra.RDS")
bi_trade_brb %>%
  saveRDS("raw_data/bi_trade_brb.RDS")
bi_trade_chl %>%
  saveRDS("raw_data/bi_trade_chl.RDS")
bi_trade_col %>%
  saveRDS("raw_data/bi_trade_col.RDS")
bi_trade_cri %>%
  saveRDS("raw_data/bi_trade_cri.RDS")
bi_trade_cub %>%
  saveRDS("raw_data/bi_trade_cub.RDS")
bi_trade_dom %>%
  saveRDS("raw_data/bi_trade_dom.RDS")
bi_trade_ecu %>%
  saveRDS("raw_data/bi_trade_ecu.RDS")
bi_trade_gtm %>%
  saveRDS("raw_data/bi_trade_gtm.RDS")
bi_trade_guy %>%
  saveRDS("raw_data/bi_trade_guy.RDS")
bi_trade_hnd %>%
  saveRDS("raw_data/bi_trade_hnd.RDS")
bi_trade_hti %>%
  saveRDS("raw_data/bi_trade_hti.RDS")
bi_trade_jam %>%
  saveRDS("raw_data/bi_trade_jam.RDS")
bi_trade_mex %>%
  saveRDS("raw_data/bi_trade_mex.RDS")
bi_trade_nic %>%
  saveRDS("raw_data/bi_trade_nic.RDS")
bi_trade_pan %>%
  saveRDS("raw_data/bi_trade_pan.RDS")
bi_trade_per %>%
  saveRDS("raw_data/bi_trade_per.RDS")
bi_trade_pri %>%
  saveRDS("raw_data/bi_trade_pri.RDS")
bi_trade_pry %>%
  saveRDS("raw_data/bi_trade_pry.RDS")
bi_trade_slv %>%
  saveRDS("raw_data/bi_trade_slv.RDS")
bi_trade_tto %>%
  saveRDS("raw_data/bi_trade_tto.RDS")
bi_trade_ury %>%
  saveRDS("raw_data/bi_trade_ury.RDS")
bi_trade_ven %>%
  saveRDS("raw_data/bi_trade_ven.RDS")

all_bi_trade <- rbind(
  bi_trade_arg, bi_trade_bhs, bi_trade_bol, bi_trade_bra, bi_trade_brb,
  bi_trade_chl, bi_trade_col, bi_trade_cri, bi_trade_cub, bi_trade_dom,
  bi_trade_ecu, bi_trade_gtm, bi_trade_guy, bi_trade_hnd, bi_trade_hti,
  bi_trade_jam, bi_trade_mex, bi_trade_nic, bi_trade_pan, bi_trade_per,
  bi_trade_pry, bi_trade_slv, bi_trade_tto, bi_trade_ury,
  bi_trade_ven
) # There is no data for Puerto Rico (PRI)

all_bi_trade %>%
  saveRDS("raw_data/all_bi_trade.RDS")

# 3. Data transformation ----------------------------------------
latam_iso <- readRDS('raw_data/latam_iso.RDS') %>% 
  filter(!iso3c %in% c('BHS', 'BRB', 'JAM', 'TTO'))

## 3.1 All Trade -----------------------------------------------------
all_trade <- readRDS("raw_data/all_trade.RDS")

all_trade <- all_trade %>% select(reporterISO, reporterDesc, partnerISO, partnerDesc, period, flowCode, cmdCode, cmdDesc, primaryValue) %>% 
  mutate(period = as.numeric(period))

### Verifying implicit missings
all_trade_complete <- all_trade %>% 
  complete(period = 1988:2020, flowCode, cmdCode, reporterISO)

setdiff(latam_iso$iso3c, all_trade_complete$reporterISO)

all_trade_complete %>% filter(reporterISO %in% latam_iso$iso3c) %>% 
  summarise_all(~ sum(is.na(.))) %>% View()

### 3.1.1 Missing counts--------------------------------------------------
all_trade_complete %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(reporterISO) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% View()

all_trade_complete %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + ggtitle("Base original") +
  xlab('Ano') + ylab('Missings') +
  theme_classic() +
  geom_line(linetype = 'dashed') +
  geom_point()

all_trade_complete %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period, reporterISO) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + ggtitle("Base original") +
  facet_wrap(~factor(reporterISO, levels=c(unique(reporterISO))), drop = T, ncol = 4, scales = "free_y") +
  xlab('Ano') + ylab('Missings') +
  theme_minimal() +
  geom_line(linewidth = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 3.2 Bilateral Trade imputation --------------------------------------------

## 3.2.1 Aggregating bilateral trade -----------------------------------------
all_bi_trade <- readRDS("raw_data/all_bi_trade.RDS")

all_bi_trade_agg <- all_bi_trade %>% 
  select(reporterISO, reporterDesc, partnerISO, partnerDesc, period, flowCode, cmdCode, cmdDesc, primaryValue) %>% 
  group_by(period, flowCode, cmdCode, partnerISO) %>% 
  reframe(primaryValue = sum(primaryValue)) %>% 
  mutate(period = as.numeric(period)) %>% 
  ungroup() %>% 
  mutate(flowCode = case_when(flowCode == "X" ~ "M",
                              flowCode == "M" ~ "X")) %>% 
  rename(reporterISO = partnerISO)

complete_bi_trade_agg <- all_bi_trade_agg %>% 
  complete(period = 1988:2020, flowCode, cmdCode, reporterISO)

### 3.2.1 Imputation ---------------------------------------------------------
all_trade_complete <- all_trade_complete %>% 
  full_join(all_bi_trade_agg, join_by(cmdCode, flowCode, period, reporterISO)) %>%
  mutate(primaryValue = coalesce(primaryValue.x, primaryValue.y),
         partnerISO = 'W00',
         partnerDesc = "World") %>% 
  select(-primaryValue.x, -primaryValue.y)

all_trade_complete <- all_trade_complete %>% 
  group_by(reporterISO) %>% 
  fill(reporterDesc, .direction = "updown")

all_trade_complete <- all_trade_complete %>% 
  group_by(cmdCode) %>% 
  fill(cmdDesc, .direction = "updown")

### 3.2.3 Missing counts--------------------------------------------------
#### Missings per country
all_trade_complete %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(reporterISO) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  View()

all_trade_complete %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + ggtitle("Primeira Imputação") +
  xlab('Ano') + ylab('Missings') +
  theme_classic() +
  geom_line(linetype = 'dashed') +
  geom_point()

all_trade_complete %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period, reporterISO) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + ggtitle("Primeira Imputação") +
  facet_wrap(~factor(reporterISO, levels=c(unique(reporterISO))), drop = T, ncol = 4, scales = "free_y") +
  xlab('Ano') + ylab('Missings') +
  theme_minimal() +
  geom_line(linewidth = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 3.3 Zero imputation -----------------------------------------------------
### 3.3.1 Identifying full missing commodities -----------------------------
fill_cmd_missing_x <- all_trade_complete %>% 
  filter(reporterISO %in% latam_iso$iso3c,
         flowCode == "X") %>% 
  group_by(reporterISO, cmdCode, period) %>% 
  summarise(sum_x_na = sum(is.na(primaryValue)))

fill_cmd_missing_m <- all_trade_complete %>% 
  filter(reporterISO %in% latam_iso$iso3c,
         flowCode == "M") %>% 
  group_by(reporterISO, cmdCode, period) %>% 
  summarise(sum_m_na = sum(is.na(primaryValue)))

fill_cmd_missing <- all_trade_complete %>%
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(reporterISO, cmdCode, period) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ungroup()

fill_cmd_missing <- fill_cmd_missing %>% 
  left_join(fill_cmd_missing_m) %>% 
  left_join(fill_cmd_missing_x) %>% 
  left_join(all_trade_complete %>% select(cmdCode, cmdDesc) %>% 
              distinct(cmdCode, .keep_all = T))

no_exports <- fill_cmd_missing %>% 
  dplyr::mutate(
    .by = c(reporterISO, cmdCode),
    constant = ifelse(max(sum_x_na) - min(sum_x_na) == 0, "constante", "variavel")
  ) %>% 
  filter(constant == "constante", sum_x_na == 1)

no_imports <- fill_cmd_missing %>% 
  dplyr::mutate(
    .by = c(reporterISO, cmdCode),
    constant = ifelse(max(sum_m_na) - min(sum_m_na) == 0, "constante", "variavel")
  ) %>% 
  filter(constant == "constante", sum_m_na == 1)

no_trade <- fill_cmd_missing %>% 
  dplyr::mutate(
    .by = c(reporterISO, cmdCode),
    constant = ifelse(max(sum_total_na) - min(sum_total_na) == 0, "constante", "variavel")
  ) %>% 
  filter(constant == "constante", sum_total_na == 2)

### 3.3.2 Imputing zeroes -----------------------------------------------------
x_zero <- no_exports %>% 
  select(-sum_total_na, -sum_m_na, -sum_x_na, -constant) %>% 
  mutate(primaryValue = 0,
         flowCode = "X")

m_zero <- no_imports %>% 
  select(-sum_total_na, -sum_m_na, -sum_x_na, -constant) %>% 
  mutate(primaryValue = 0,
         flowCode = "M")

all_trade_complete_two <- all_trade_complete %>% 
  full_join(x_zero, join_by(cmdCode, flowCode, period, reporterISO)) %>%
  mutate(primaryValue = coalesce(primaryValue.x, primaryValue.y),
         partnerISO = 'W00',
         partnerDesc = "World") %>% 
  select(-primaryValue.x, -primaryValue.y, -cmdDesc.y) %>% 
  rename(cmdDesc = cmdDesc.x)

all_trade_complete_two <- all_trade_complete_two %>% 
  full_join(m_zero, join_by(cmdCode, flowCode, period, reporterISO)) %>%
  mutate(primaryValue = coalesce(primaryValue.x, primaryValue.y),
         partnerISO = 'W00',
         partnerDesc = "World") %>% 
  select(-primaryValue.x, -primaryValue.y, -cmdDesc.y) %>% 
  rename(cmdDesc = cmdDesc.x)

# write_rds(x = all_trade_complete_two, file = 'raw_data/all_trade_b4_imputation.RDS')

### 3.3.3 Missing count ----------------------------------------------------
all_trade_complete_two %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(reporterISO) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  View()

all_trade_complete_two %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + ggtitle("Segunda Imputação") +
  xlab('Ano') + ylab('Missings') +
  theme_classic() +
  geom_line(linetype = 'dashed') +
  geom_point()

all_trade_complete_two %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period, reporterISO) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + ggtitle("Segunda Imputação") +
  facet_wrap(~factor(reporterISO, levels=c(unique(reporterISO))), drop = T, ncol = 4, scales = "free_y") +
  xlab('Ano') + ylab('Missings') +
  theme_minimal() +
  geom_line(linewidth = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 3.4 Linear interpolation --------------------------------------------------
interpolated_trade <- all_trade_complete_two %>% 
  group_by(reporterISO, cmdCode, flowCode) %>% 
  mutate(primaryValue = na.approx(primaryValue, na.rm=FALSE)) %>% 
  ungroup()

interpolated_trade %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(reporterISO) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  View()

interpolated_trade %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + ggtitle("Terceira Imputação") +
  xlab('Ano') + ylab('Missings') +
  theme_classic() +
  geom_line(linetype = 'dashed') +
  geom_point()

interpolated_trade %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period, reporterISO) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + ggtitle("Terceira Imputação") +
  facet_wrap(~factor(reporterISO, levels=c(unique(reporterISO))), drop = T, ncol = 4, scales = "free_y") +
  xlab('Ano') + ylab('Missings') +
  theme_minimal() +
  geom_line(linewidth = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write_rds(x = interpolated_trade, file = 'raw_data/all_trade_interpolated.RDS')

## 3.5 Mean imputation ------------------------------------------------

n <- 10

mean_imputed_trade <- all_trade_complete_two %>% 
  group_by(reporterISO, cmdCode, flowCode) %>% 
  arrange(period) %>% 
  group_by(reporterISO, period, flowCode) %>% 
  mutate(cmd_share = primaryValue/primaryValue[cmdCode=='TOTAL']) %>% 
  group_by(reporterISO, cmdCode, flowCode,
           Group = floor((period - first(period))/n)) %>%
  mutate(time_average = mean(cmd_share, na.rm = TRUE)) %>% 
  ungroup()

mean_imputed_trade <- all_trade_imputed %>% 
  group_by(reporterISO, period, flowCode) %>% 
  mutate(primaryValue = 
           coalesce(primaryValue, 
           primaryValue[cmdCode=='TOTAL']*time_average)) %>%
  ungroup()

mean_imputed_trade %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(reporterISO) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% View()

mean_imputed_trade %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + ggtitle("Terceira Imputação") +
  xlab('Ano') + ylab('Missings') +
  theme_classic() +
  geom_line(linetype = 'dashed') +
  geom_point()

mean_imputed_trade %>% 
  filter(reporterISO %in% latam_iso$iso3c) %>% 
  group_by(period, reporterISO) %>% 
  summarise(sum_total_na = sum(is.na(primaryValue))) %>% 
  ggplot(aes(x=period, y=sum_total_na)) + ggtitle("Terceira Imputação") +
  facet_wrap(~factor(reporterISO, levels=c(unique(reporterISO))), drop = T, ncol = 4, scales = "free_y") +
  xlab('Ano') + ylab('Missings') +
  theme_minimal() +
  geom_line(linewidth = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write_rds(x = mean_imputed_trade, file = 'raw_data/all_trade_mean_imputed.RDS')

## 4. Statistics ----------------------------------------------------------
all_trade_complete %>% 
  filter(reporterISO %in% latam_iso$iso3c,
         flowCode == "M") %>% 
  summary()

all_trade_complete %>% 
  filter(reporterISO %in% latam_iso$iso3c,
         flowCode == "X") %>% 
  summary()

all_trade_complete_two %>% 
  filter(reporterISO %in% latam_iso$iso3c,
         flowCode == "M") %>% 
  summary()

all_trade_complete_two %>% 
  filter(reporterISO %in% latam_iso$iso3c,
         flowCode == "X") %>% 
  summary()

interpolated_trade %>% 
  filter(reporterISO %in% latam_iso$iso3c,
         flowCode == "M") %>% 
  summary()

interpolated_trade %>% 
  filter(reporterISO %in% latam_iso$iso3c,
         flowCode == "X") %>% 
  summary()

mean_imputed_trade %>% 
  filter(reporterISO %in% latam_iso$iso3c,
         flowCode == "M") %>% 
  summary()

mean_imputed_trade %>% 
  filter(reporterISO %in% latam_iso$iso3c,
         flowCode == "X") %>% 
  summary()

# Leftovers: Might use in the future
# alltrade <- do.call(mapply, c("get_data", unname(as.list(vars)))) %>% list_rbind()