#Loading necessary libraries

library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(RColorBrewer)
library(gt)
library(gtExtras)
library(webshot2)
library(ggpubr)
################################################################################
#Importing dataset and labels

## Main Dataset
setwd("~/Desktop/Trade Analysis/Raw Data/CSV Files") #Setting working directory

df <-
  list.files(path = "~/Desktop/Trade Analysis/Raw Data/CSV Files", pattern = "*.csv") %>% #lists the filen names as a vector in the specified path
  map_df(~read_csv(., col_types = cols(.default="c", INDICATOR_VALUE="d"))) %>% #turn everything into a character
  clean_names() %>% 
  select(-declarant, -declarant_lab, -flow, -stat_regime, -stat_regime_lab, - period_lab, -indicators, -indicators_lab) %>% 
  rename(country = partner_lab,
         flow = flow_lab,
         trade_value = indicator_value) %>% 
  mutate(period=ym(period))

df$flow <- str_to_sentence(df$flow) #fixing cases in certain variables
df$country <- str_to_title(df$country)

df
summary(df)

## HS2 Labels
hs2_labs <- read_csv("~/Desktop/Trade Analysis/Raw Data/Labels/HS2/data-17101708.csv") %>% 
  clean_names() %>% 
  distinct(product, str_to_sentence(product_lab)) %>%  #converts to sentence case
  rename(hs2 = product,
         hs2_lab = 'str_to_sentence(product_lab)') %>% 
  arrange(hs2)

hs2_labs
summary(hs2_labs)

## HS4 Labels
setwd("~/Desktop/Trade Analysis/Raw Data/Labels/HS4") #Setting working directory
hs4_labs <-
  list.files(path = "~/Desktop/Trade Analysis/Raw Data/Labels/HS4", pattern = "*.csv") %>% #lists the filen names as a vector in the specified path
  map_df(~read_csv(., col_types = cols(.default="c", INDICATOR_VALUE="d"))) %>% #turn everything into a character
  clean_names() %>% 
  distinct(product, str_to_sentence(product_lab)) %>%  #converts to sentence case
  rename(hs4 = product,
         hs4_lab = 'str_to_sentence(product_lab)') %>% 
  arrange(hs4)

hs4_labs
summary(hs4_labs)
################################################################################
#Setting final working directory

setwd("~/Desktop/Trade Analysis") 
################################################################################
#Pre-processing dataset

## Creating HS2 and HS4 Variables
df <- df %>% 
  rename(hs6 = product,
         hs6_lab = product_lab) %>% 
  mutate(hs4 = substr(hs6,1,4),
         hs2 = substr(hs6,1,2)) %>% 
  relocate(c(hs4,hs2), .after = hs6_lab)

## Joining dataset and labels (HS2 & HS4)
df <- df %>%
  left_join(hs2_labs, by=("hs2")) %>% 
  left_join(hs4_labs, by=("hs4")) %>% 
  relocate(hs2_lab, .after = hs2) %>% 
  relocate(hs4_lab, .after = hs4)

## Defining HS Sections
df <- df %>% 
  mutate(hss = case_when(hs2 %in% c("01", "02", "03", "04", "05") ~ "01",
                         hs2 %in% c("06", "07", "08", "09", "10", "11", "12", "13", "14") ~ "02",
                         hs2 == "15" ~ "03",
                         hs2 %in% c("16", "17", "18", "19", "20", "21", "22", "23", "24") ~ "04",
                         hs2 %in% c("25", "26", "27") ~ "05",
                         hs2 %in% c("28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38") ~ "06",
                         hs2 %in% c("39", "40") ~ "07",
                         hs2 %in% c("41", "42", "43") ~ "08",
                         hs2 %in% c("44", "45", "46") ~ "09",
                         hs2 %in% c("47", "48", "49") ~ "10",
                         hs2 %in% c("50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63") ~ "11",
                         hs2 %in% c("65", "65", "66", "67") ~ "12",
                         hs2 %in% c("68", "69", "70") ~ "13",
                         hs2 == "71" ~ "14",
                         hs2 %in% c("72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83") ~ "15",
                         hs2 %in% c("84", "85") ~ "16",
                         hs2 %in% c("86", "87", "88", "89") ~ "17",
                         hs2 %in% c("90", "91", "92") ~ "18",
                         hs2 == "93" ~ "19",
                         hs2 %in% c("94", "95", "96") ~ "20",
                         hs2 == "97" ~ "21",
                         hs2 %in% c("98", "99") ~ "22"),
         hss_lab = case_when(hs2 %in% c("01", "02", "03", "04", "05") ~ "Live Animals & Animal Products",
                         hs2 %in% c("06", "07", "08", "09", "10", "11", "12", "13", "14") ~ "Vegetable Products",
                         hs2 == "15" ~ "Animal or Vegetable Fats and Oils",
                         hs2 %in% c("16", "17", "18", "19", "20", "21", "22", "23", "24") ~ "Prepared Foodstuffs; Beverages, Spirits, and Vinegar; Tobacco and Manufactured Tobacco Substitutes",
                         hs2 %in% c("25", "26", "27") ~ "Mineral Products",
                         hs2 %in% c("28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38") ~ "Products of the Chemical or Allied Industries",
                         hs2 %in% c("39", "40") ~ "Plastics and Articles Thereof; Rubber and Articles Thereof",
                         hs2 %in% c("41", "42", "43") ~ "Raw Hides, Skins, and Leather",
                         hs2 %in% c("44", "45", "46") ~ "Wood and Articles of Wood",
                         hs2 %in% c("47", "48", "49") ~ "Pulp of Wood, Paper",
                         hs2 %in% c("50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63") ~ "Textile and Textile Articles",
                         hs2 %in% c("65", "65", "66", "67") ~ "Footwear, Headgear etc.",
                         hs2 %in% c("68", "69", "70") ~ "Articles of Stone, Plaster, Cement, Ceramics, and Glass",
                         hs2 == "71" ~ "Pearls, Precious Stones, and Precious Metals",
                         hs2 %in% c("72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83") ~ "Base Metals and Articles of Base Metals",
                         hs2 %in% c("84", "85") ~ "Machinery, Appliances, Electrical Equipment",
                         hs2 %in% c("86", "87", "88", "89") ~ "Vehicles, Aircraft, Vessels",
                         hs2 %in% c("90", "91", "92") ~ "Precision Instruments",
                         hs2 == "93" ~ "Arms and Ammunitions",
                         hs2 %in% c("94", "95", "96") ~ "Misc. Manufactured Articles",
                         hs2 == "97" ~ "Works of Art",
                         hs2 %in% c("98", "99") ~ "Special Classification Provisions")) %>% 
  relocate(c(hss, hss_lab), .after=hs2_lab)

## Summarising trade values by year
df_year <- df %>%
  mutate(year = year(period)) %>% 
  relocate(year, .after=period) %>% 
  group_by(partner, country, hs6, hs6_lab, hs4, hs4_lab, hs2, hs2_lab, hss, hss_lab, flow, year) %>% 
  summarise(trade_value = sum(trade_value)) %>% 
  arrange(country, flow, year, hs6)

## Creating a more condensed dataset of monthly values
df_month_recent <- df %>% 
  mutate(year = year(period)) %>%
  filter(year >= 2015) %>% 
  arrange(country, flow, year, hs6) %>% 
  select(-year)

## Exporting Dataset
#write_delim(df, "~/Desktop/Trade Analysis/Trade Datasets/Kosovo_trade.csv", delim = "*")
#write_delim(df_year, "~/Desktop/Trade Analysis/Trade Datasets/Kosovo_trade_year.csv", delim = "*")
#write_delim(df_month_recent, "~/Desktop/Trade Analysis/Trade Datasets/Kosovo_trade_recent.csv", delim = "*")
################################################################################
# Data Analysis

df %>% 
  tabyl(period) # Data exists for the period of January 2004 until June 2022.

df %>% 
  tabyl(country)

summary(df)
# To avoid confusion in the analysis (due to combined regions in the Balkans), we use data only from 2007
#######################################
# Visualisations

## Exports
exp <- df_year %>%
  mutate(country = recode(country, "Montenegro (Since 2007)" = "Montenegro",
                          "Switzerland (Incl.liecht.->94)" = "Switzerland")) %>% 
  filter(year == 2021,
         flow == "Export") %>%
  group_by(country) %>% 
  summarise(total_exports = round(sum(trade_value)*1000)) %>% 
  arrange(desc(total_exports)) %>% 
  head(10) %>% 
  ggplot(aes(reorder(country, total_exports), total_exports, fill=total_exports)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), breaks = seq(0,120000000,40000000)) +
  expand_limits(y = 140000000) +
  geom_text(aes(label = label_number_si(accuracy=1)(total_exports)), hjust = -0.25, size = 5) +
  labs(y="Total Exports",
       x="Country",
       #title="Top 10 countries Kosovo exports to, in millions of €",
       caption = "Data: Eurostat") +
  coord_flip() +
  theme_minimal() +
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(size=20, face = "bold", color = "#222222"),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(hjust = 0, size=12),
        axis.text = element_text(size = 15, color = "#222222"),
        plot.title.position = "plot") #Changes the title position to corner of plot

## Imports
imp <- df_year %>%
  mutate(country = recode(country, "China (People's Republic Of)" = "China")) %>% 
  filter(year == 2021,
         flow == "Import") %>%
  group_by(country) %>% 
  summarise(total_imports = round(sum(trade_value)*1000)) %>% 
  arrange(desc(total_imports)) %>% 
  head(10) %>% 
  ggplot(aes(reorder(country, total_imports), total_imports, fill=total_imports)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), breaks = seq(0,700000000,160000000)) +
  scale_fill_gradient(low="#5c1616", high="#fa2a2a") +
  expand_limits(y = 700000000) +
  geom_text(aes(label = label_number_si(accuracy = 1)(total_imports)), hjust = -0.25, size = 5) +
  labs(y="Total Imports",
       x="Country",
       #title="Top 10 countries Kosovo imports from, in millions of €",
       caption = "Data: Eurostat") +
  coord_flip() +
  theme_minimal() +
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        legend.position = "none",
        axis.title = element_blank(),
        plot.title = element_text(size=20, face = "bold", color = "#222222"),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(hjust = 0, size=12),
        axis.text = element_text(size = 15, color = "#222222"),
        plot.title.position = "plot")

fig1 <- ggarrange(exp,imp,ncol=2,nrow=1, labels = c("Exports", "Imports"), hjust=-3.2) %>% 
annotate_figure(top = text_grob("Top 10 countries Kosovo exports to and imports from, in millions of €", color = "#222222", face = "bold", size = 18, hjust=0.5)) + 
theme(plot.margin = margin(1,1,0.5,1.2, "cm"))
fig1
ggsave("fig1.jpg", width = 15, height = 10, dpi=300)

## Trade Balance
tb <- df_year %>%
  drop_na() %>%
  filter(year>=2010, year<2022) %>% 
  mutate(trade = case_when(flow == "Export" ~ trade_value,
                           flow == "Import" ~ trade_value*-1)) %>% 
  group_by(year, flow) %>% 
  summarize(total_trade = sum(trade)*1000) %>%
  pivot_wider(names_from = flow, values_from = total_trade) %>%
  mutate('Trade Balance' = Export + Import) %>% 
  pivot_longer(!year, names_to = "flow", values_to = "total_trade")

tb %>% 
  filter(flow!="Trade Balance") %>% 
  ggplot(aes(year, total_trade, fill=flow)) +
  geom_bar(stat="identity") +
  geom_line(aes(x=year, y=total_trade), data=tb[tb$flow =="Trade Balance", ], color="navy", size=2) +
  scale_x_continuous(breaks = seq(2010, 2021, 1), guide = guide_axis(angle = 90)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), breaks = seq(-4000000000,1000000000,1000000000), limits=c(-5000000000,1000000000)) +
  scale_fill_manual(breaks = c("Export", "Import", "Trade Balance"),
                    values = c("steelblue", "#eb4034", "navy"),
                    name = "Flow") +
  labs(title="Yearly trade in Kosovo",
      caption = "Data: Eurostat") +
  theme_minimal() +
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.title = element_blank(),
        plot.title = element_text(size=20, face = "bold", color = "#222222"),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(hjust = 0, size=12),
        axis.text = element_text(size = 15, color = "#222222"),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.title = element_text(size=15, face = "bold"),
        legend.text = element_text(size=10, face="bold"),
        legend.position = "top",
        plot.title.position = "plot") 

ggsave("trade_balance.jpg", width = 10, height = 7, dpi=300)

# ## Monthly Trade Balance
# mtb <- df %>%
#   drop_na() %>%
#   mutate(date = date(period),
#          year = year(period),
#          month = month(period)) %>% 
#   filter(year>=2020) %>% 
#   mutate(trade = case_when(flow == "Export" ~ trade_value,
#                            flow == "Import" ~ trade_value*-1)) %>% 
#   group_by(year, month, date, flow) %>% 
#   summarize(total_trade = sum(trade)*1000) %>%
#   pivot_wider(names_from = flow, values_from = total_trade) %>%
#   mutate('Trade Balance' = Export + Import) %>% 
#   pivot_longer(!c(year, month, date), names_to = "flow", values_to = "total_trade")
# 
# mtb %>% 
#   filter(flow!="Trade Balance") %>%
#   ggplot(aes(date, total_trade, fill=flow)) +
#   geom_bar(stat="identity") +
#   geom_line(aes(x=date, y=total_trade), data=mtb[mtb$flow =="Trade Balance", ], color="navy", size=1.5) +
#   scale_x_date(date_breaks = '1 month', date_labels = "%b-%Y", guide = guide_axis(angle = 90), limits = c(ymd("2020-02-01"), ymd("2022-05-01")), oob = scales::oob_keep) + #limits() excludes the limit boundaries; oob= keeps them
#   scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
#   scale_fill_manual(breaks = c("Export", "Import", "Trade Balance"),
#                     values = c("steelblue", "#eb4034", "navy"),
#                     name = "Flow") +
#   labs(title="Monthly trade in Kosovo",
#        caption = "Data: Eurostat") +
#   theme_minimal() +
#   theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
#         axis.title = element_blank(),
#         plot.title = element_text(size=20, face = "bold", color = "#222222"),
#         plot.subtitle = element_text(size=18),
#         plot.caption = element_text(hjust = 0, size=12),
#         axis.text = element_text(size = 15, color = "#222222"),
#         legend.key.size = unit(0.5, 'cm'), #change legend key size
#         legend.title = element_text(size=15, face = "bold"),
#         legend.text = element_text(size=10, face="bold"),
#         legend.position = "top",
#         plot.title.position = "plot") 
# 
# ggsave("trade_balance_monthly.jpg", width = 10, height = 7, dpi=300)

## Table Trade Volume
tv <- df_year %>%
  drop_na() %>%
  filter(year>=2010, year<2022) %>% 
  mutate(trade = case_when(flow == "Export" ~ trade_value,
                           flow == "Import" ~ trade_value*-1)) %>% 
  group_by(year, flow) %>% 
  summarize(total_trade = sum(trade)*1000) %>%
  pivot_wider(names_from = flow, values_from = total_trade) %>%
  mutate('Trade Balance' = Export + Import)

tv %>% 
  ungroup() %>%
  arrange(desc(year)) %>% 
  gt() %>% 
  tab_header(title = "Yearly trade in Kosovo") %>% 
  opt_align_table_header(align = "left") %>% 
  cols_label(year = "Year") %>% 
  fmt_number(columns = c(Export, Import, 'Trade Balance'),
             decimals = 3,
             suffixing = T) %>% 
  cols_width(everything() ~ px(100)) %>% 
  tab_source_note(source_note = "The data is extracted from the Eurostat Comext portal") %>% 
  tab_style(locations = cells_body(columns = everything(),
                                   rows = 1), style = list(cell_fill(color = "#5599FF"),
                                                              cell_text(color = "white"))) %>%
  gt_theme_guardian() %>% 
  gtsave("tab_1.png", expand = 10)


# ## Table Monthly Trade Volume
# mtv <- df %>%
#   drop_na() %>%
#   mutate(date = date(period),
#          year = year(period),
#          month = month(period)) %>% 
#   filter(year==2022) %>%
#   mutate(trade = case_when(flow == "Export" ~ trade_value,
#                            flow == "Import" ~ trade_value*-1)) %>% 
#   group_by(date, flow) %>% 
#   summarize(total_trade = sum(trade)*1000) %>%
#   pivot_wider(names_from = flow, values_from = total_trade) %>%
#   mutate('Trade Balance' = Export + Import)
# 
# mtv %>% 
#   ungroup() %>%
#   arrange(desc(date)) %>% 
#   gt() %>% 
#   tab_header(title = "Monthly trade in Kosovo") %>% 
#   opt_align_table_header(align = "left") %>% 
#   cols_label(date = "Date") %>% 
#   fmt_number(columns = c(Export, Import, 'Trade Balance'),
#              decimals = 3,
#              suffixing = T) %>%
#   fmt_date(columns = date, date_style = 11, pattern = "{x} 2022") %>% 
#   cols_width(everything() ~ px(100)) %>% 
#   tab_source_note(source_note = "The data is extracted from the Eurostat Comext portal") %>% 
#   tab_style(locations = cells_body(columns = everything(),
#                                    rows = 1), style = list(cell_fill(color = "#5599FF"),
#                                                            cell_text(color = "white"))) %>%
#   gt_theme_guardian() %>% 
#   gtsave("tab_m.png", expand = 10)

## Table HS Volume
### Export
str_sub(df_year$hss, 0,0) <- "HS" #Add "HS" to the beginning of each string.

ts_exp <- df_year %>%
  drop_na() %>%
  filter(year == 2021) %>% 
  mutate(trade = case_when(flow == "Export" ~ trade_value,
                           flow == "Import" ~ trade_value*-1)) %>% 
  group_by(hss, hss_lab, flow) %>% 
  summarize(total_trade = sum(trade)*1000) %>% 
  pivot_wider(names_from = flow, values_from = total_trade) %>%
  replace(is.na(.), 0) %>% 
  mutate('Trade Balance' = Export + Import) %>% 
  arrange(desc(Export)) %>% 
  head(5) %>% 
  ungroup()

tab1 <- ts_exp %>% 
  gt() %>% 
  tab_header(title = md("Top 5 HS sections Kosovo *exports*, 2021")) %>% 
  opt_align_table_header(align = "left") %>% 
  cols_label(hss = "HS",
             hss_lab = "HS Label") %>% 
  fmt_number(columns = c(Export, Import, 'Trade Balance'),
             decimals = 3,
             suffixing = T) %>% 
  cols_width(everything() ~ px(100)) %>% 
  tab_source_note(source_note = "The data is extracted from the Eurostat Comext portal") %>%
  gt_theme_guardian() %>% 
  gtsave("tab_2.png", expand = 10)

### Import
ts_imp <- df_year %>%
  drop_na() %>%
  filter(year == 2021) %>% 
  mutate(trade = case_when(flow == "Export" ~ trade_value,
                           flow == "Import" ~ trade_value*-1)) %>% 
  group_by(hss, hss_lab, flow) %>% 
  summarize(total_trade = sum(trade)*1000) %>% 
  pivot_wider(names_from = flow, values_from = total_trade) %>%
  replace(is.na(.), 0) %>% 
  mutate('Trade Balance' = Export + Import) %>% 
  arrange(Import) %>% 
  head(5) %>% 
  ungroup()

tab2 <- ts_imp %>% 
  gt() %>% 
  tab_header(title = md("Top 5 HS sections Kosovo *imports*, 2021")) %>% 
  opt_align_table_header(align = "left") %>% 
  cols_label(hss = "HS",
             hss_lab = "HS Label") %>% 
  fmt_number(columns = c(Export, Import, 'Trade Balance'),
             decimals = 3,
             suffixing = T) %>% 
  cols_width(everything() ~ px(100)) %>% 
  tab_source_note(source_note = "The data is extracted from the Eurostat Comext portal") %>%
  gt_theme_guardian() %>% 
  gtsave("tab_3.png", expand = 10)

## Dumbbell Chart of 2021 vs. 2019 Imports by HS Section
dc_imp <- df_year %>%
  drop_na() %>%
  filter(year %in% c(2019,2021),
         flow == "Import") %>% 
  group_by(hss, hss_lab, year) %>% 
  summarize(total_trade = sum(trade_value)*1000) %>% 
  ungroup() %>% 
  pivot_wider(names_from = year, values_from = total_trade) %>% 
  mutate(diff = `2021` - `2019`) %>%
  arrange(desc(diff)) %>%
  head(10) %>% 
  pivot_longer(!c(hss,hss_lab,diff), names_to = "year", values_to = "total_trade")

y2019 <- dc_imp %>% 
  filter(year==2019)
y2021 <- dc_imp %>% 
  filter(year==2021)

diff <- dc_imp %>% 
  filter(year == 2019) %>% #you can chose Males of Females, doesn't matter
  mutate(x_pos = total_trade + (diff/2)) #x position of label (Total Trade in 2019 + diff/2)
head(diff)

dc_imp %>% 
  ggplot() +
  geom_segment(data=y2019, 
               aes(x=total_trade, y=reorder(hss, diff), xend=y2021$total_trade, yend=y2021$hss),
               color = "#aeb6bf",
               size = 4.5,
               alpha = .5) +
  geom_point(aes(x = total_trade, y = hss, color = year), size = 4, show.legend = TRUE) +
  guides(color=guide_legend(title="Year")) +
  geom_text(data = diff,
            aes(label = label_number_si(accuracy=1)(diff), x = x_pos, y = hss), #note that I changed the greek letter Delta to "D:" because of encoding reasons
            color = "#4a4e4d",
            size = 5) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_color_manual(values = c("steelblue", "#eb4034")) +
  labs(title="Highest increase in imports between 2021 and 2019, by HS section",
       caption = "Data: Eurostat") +
  theme_minimal() +
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.title = element_blank(),
        plot.title = element_text(size=20, face = "bold", color = "#222222", hjust=0),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(hjust = 0, size=12),
        axis.text = element_text(size = 15, color = "#222222"),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.title = element_text(size=15, face = "bold"),
        legend.text = element_text(size=10, face="bold"),
        legend.position = "top",
        plot.title.position = "plot")

ggsave("dc_imp.jpg", width = 10, height = 7, dpi=300)


wut_imp <- df_year %>% 
  filter(hss=="HS16",
         year %in% c(2019,2021),
         flow=="Import") %>% 
  group_by(hs6, hs6_lab, year) %>% 
  summarize(total_trade = sum(trade_value)*1000) %>% 
  ungroup() %>% 
  arrange(desc(total_trade)) %>% 
  pivot_wider(names_from = year, values_from=total_trade) %>% 
  mutate(diff=`2021`-`2019`) %>% 
  arrange(desc(diff)) %>% 
  head(15)


## Dumbbell Chart of 2021 vs. 2019 Exports by HS Section
dc_exp <- df_year %>%
  drop_na() %>%
  filter(year %in% c(2019,2021),
         flow == "Export") %>% 
  group_by(hss, hss_lab, year) %>% 
  summarize(total_trade = sum(trade_value)*1000) %>% 
  ungroup() %>% 
  pivot_wider(names_from = year, values_from = total_trade) %>% 
  mutate(diff = `2021` - `2019`) %>%
  arrange(desc(diff)) %>%
  head(10) %>% 
  pivot_longer(!c(hss,hss_lab,diff), names_to = "year", values_to = "total_trade")

y2019_exp <- dc_exp %>% 
  filter(year==2019)
y2021_exp <- dc_exp %>% 
  filter(year==2021)

diff_exp <- dc_exp %>% 
  filter(year == 2019) %>% #you can chose Males of Females, doesn't matter
  mutate(x_pos = total_trade + (diff/2)) #x position of label (Total Trade in 2019 + diff/2)
head(diff)

dc_exp %>% 
  ggplot() +
  geom_segment(data=y2019_exp, 
               aes(x=total_trade, y=reorder(hss, diff), xend=y2021_exp$total_trade, yend=y2021_exp$hss),
               color = "#aeb6bf",
               size = 4.5,
               alpha = .5) +
  geom_point(aes(x = total_trade, y = hss, color = year), size = 4, show.legend = TRUE) +
  guides(color=guide_legend(title="Year")) +
  geom_text(data = diff_exp,
            aes(label = label_number_si(accuracy=1)(diff), x = x_pos, y = hss), #note that I changed the greek letter Delta to "D:" because of encoding reasons
            color = "#4a4e4d",
            size = 5) +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_color_manual(values = c("steelblue", "#eb4034")) +
  labs(title="Highest increase in exports between 2021 and 2019, by HS section",
       caption = "Data: Eurostat") +
  theme_minimal() +
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.title = element_blank(),
        plot.title = element_text(size=20, face = "bold", color = "#222222", hjust=0),
        plot.subtitle = element_text(size=18),
        plot.caption = element_text(hjust = 0, size=12),
        axis.text = element_text(size = 15, color = "#222222"),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.title = element_text(size=15, face = "bold"),
        legend.text = element_text(size=10, face="bold"),
        legend.position = "top",
        plot.title.position = "plot")

ggsave("dc_exp.jpg", width = 10, height = 7, dpi=300)


wut_exp <- df_year %>% 
  filter(hss=="HS20",
         year %in% c(2019,2021),
         flow=="Import") %>% 
  group_by(hs6, hs6_lab, year) %>% 
  summarize(total_trade = sum(trade_value)*1000) %>% 
  ungroup() %>% 
  arrange(desc(total_trade)) %>% 
  pivot_wider(names_from = year, values_from=total_trade) %>% 
  mutate(diff=`2021`-`2019`) %>% 
  arrange(desc(diff)) %>% 
  head(15)
