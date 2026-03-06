library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(janitor)
library(psych)

ds <- readRDS("outputs/ds.rds")

# Separate data_clean by language
data_clean <- ds %>%
  mutate(
    language = case_when(
      QUESTNNR == "base" ~ "ES",
      QUESTNNR == "qnr2" ~ "EN",
      TRUE ~ NA_character_
    )
  )

dim(data_clean)
table(data_clean$QUESTNNR, useNA = "ifany")

data_es <- data_clean %>% filter(language == "ES")
data_en <- data_clean %>% filter(language == "EN")

dim(data_es)
dim(data_en)

setequal(names(data_es), names(data_en))
"QUESTNNR" %in% names(data_clean)


data_clean <- data_clean %>%
  mutate(
    language = case_when(
      QUESTNNR == "base" ~ "ES",
      QUESTNNR == "qnr2" ~ "EN",
      TRUE ~ NA_character_
    )
  )

sum(str_detect(names(data_clean), "^A\\d"))
sum(str_detect(names(data_clean), "^D\\d"))


# Build a map of equivalences between variables in ES and EN

c("A011_01", "A011_02", "D101_01", "D101_02") %in% names(data_clean)

# Create item map para DS General Rating ES and EN


#Robots Delivering Supplies General Rating
item_map_gr_ds <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "ds",      "gr",        1,     "A011_01", "D101_01", "ds_gr_1",
  "ds",      "gr",        2,     "A011_02", "D101_02", "ds_gr_2"
)

item_map <- bind_rows( #to save each block
  item_map_gr_ds
  # , item_map_use_ds
  # , item_map_open_ds
  # , item_map_pe_ds
  # ...
)

#Robots Delivering Supplies Medical Settings

item_map_use_ds <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "ds",      "use",       0,     "A012",    "D102",     "ds_use_0",
  "ds",      "use",       1,     "A012_01", "D102_01",  "ds_use_1",
  "ds",      "use",       2,     "A012_02", "D102_02",  "ds_use_2",
  "ds",      "use",       3,     "A012_03", "D102_03",  "ds_use_3",
  "ds",      "use",       4,     "A012_04", "D102_04",  "ds_use_4",
  "ds",      "use",       5,     "A012_05", "D102_05",  "ds_use_5",
  "ds",      "use",       6,     "A012_06", "D102_06",  "ds_use_6",
  "ds",      "use",       7,     "A012_06a","D102_06a",  "ds_use_7"
)

item_map <- bind_rows(
  item_map_gr_ds,
  item_map_use_ds
)

#Robots Delivering Supplies Open-Ended Questions
item_map_open_ds <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "ds",      "open",      1,     "A014_01", "D103_01",  "ds_open_1",
  "ds",      "open",      2,     "A014_02", "D103_02",  "ds_open_2",
  "ds",      "open",      3,     "A014_03", "D103_03",  "ds_open_3"
)

item_map <- bind_rows(
  item_map_gr_ds,
  item_map_use_ds,
  item_map_open_ds
)

# Robots Delivering Supplies Performance Expectancy
item_map_pe_ds <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "ds",      "pe",        1,     "A015_01", "D104_01",  "ds_pe_1",
  "ds",      "pe",        2,     "A015_02", "D104_02",  "ds_pe_2",
  "ds",      "pe",        3,     "A015_03", "D104_03",  "ds_pe_3",
  "ds",      "pe",        4,     "A015_04", "D104_04",  "ds_pe_4"
)

item_map <- bind_rows(
  item_map_gr_ds,
  item_map_use_ds,
  item_map_open_ds,
  item_map_pe_ds
)

# Robots Delivering Supplies Attitudes Toward Using Robots
item_map_ar_ds <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "ds",      "ar",        1,     "A016_01", "D105_01",  "ds_ar_1",
  "ds",      "ar",        2,     "A016_02", "D105_02",  "ds_ar_2",
  "ds",      "ar",        3,     "A016_03", "D105_03",  "ds_ar_3",
  "ds",      "ar",        4,     "A016_04", "D105_04",  "ds_ar_4"
)

item_map <- bind_rows(
  item_map_gr_ds,
  item_map_use_ds,
  item_map_open_ds,
  item_map_pe_ds,
  item_map_ar_ds
)

#Robots Delivering Supplies Self Efficacy

item_map_se_ds <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "ds",      "se",        1,     "A017_01", "D106_01",  "ds_se_1",
  "ds",      "se",        2,     "A017_02", "D106_02",  "ds_se_2",
  "ds",      "se",        3,     "A017_03", "D106_03",  "ds_se_3",
  "ds",      "se",        4,     "A017_04", "D106_04",  "ds_se_4"
)

item_map <- bind_rows(
  item_map_gr_ds,
  item_map_use_ds,
  item_map_open_ds,
  item_map_pe_ds,
  item_map_ar_ds,
  item_map_se_ds
)

#Robots Delivering Supplies Behavioral Intention 

item_map_bi_ds <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "ds",      "bi",        1,     "A018_01", "D107_01",  "ds_bi_1",
  "ds",      "bi",        2,     "A018_02", "D107_02",  "ds_bi_2",
  "ds",      "bi",        3,     "A018_03", "D107_03",  "ds_bi_3"
)

item_map <- bind_rows(
  item_map_gr_ds,
  item_map_use_ds,
  item_map_open_ds,
  item_map_pe_ds,
  item_map_ar_ds,
  item_map_se_ds,
  item_map_bi_ds
)

#Robots Delivering Supplies Moderate Effect Ethics 

item_map_me_ds <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "ds",      "me",        1,     "A019_01", "D108_01",  "ds_me_1",
  "ds",      "me",        2,     "A019_02", "D108_02",  "ds_me_2",
  "ds",      "me",        3,     "A019_03", "D108_03",  "ds_me_3",
  "ds",      "me",        4,     "A019_04", "D108_04",  "ds_me_4",
  "ds",      "me",        5,     "A019_05", "D108_05",  "ds_me_5",
  "ds",      "me",        6,     "A019_06", "D108_06",  "ds_me_6",
  "ds",      "me",        7,     "A019_07", "D108_07",  "ds_me_7"
)

item_map <- bind_rows(
  item_map_gr_ds,
  item_map_use_ds,
  item_map_open_ds,
  item_map_pe_ds,
  item_map_ar_ds,
  item_map_se_ds,
  item_map_bi_ds,
  item_map_me_ds
)

#Robots Helping Patients into Bed General Rating 

item_map_gr_bed<- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "bed",      "gr",        1,     "A110_01", "D201_01",  "bed_gr_1",
  "bed",      "gr",        2,     "A110_02", "D201_02",  "bed_gr_2"

)

item_map <- bind_rows(
  item_map_gr_bed
)

#Robots Helping Patients into Bed Robots Medical Settings

item_map_use_bed <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "bed",      "use",       0,     "A111",    "D202",     "bed_use_0",
  "bed",      "use",       1,     "A111_01", "D202_01",  "bed_use_1",
  "bed",      "use",       2,     "A111_02", "D202_02",  "bed_use_2",
  "bed",      "use",       3,     "A111_03", "D202_03",  "bed_use_3",
  "bed",      "use",       4,     "A111_04", "D202_04",  "bed_use_4",
  "bed",      "use",       5,     "A111_05", "D202_05",  "bed_use_5",
  "bed",      "use",       6,     "A111_06", "D202_06",  "bed_use_6",
  "bed",      "use",       7,     "A111_06a","D202_06a",  "bed_use_7"
)

item_map <- bind_rows(
  item_map_gr_bed,
  item_map_use_bed
)

#Robots Helping Patients into Bed Open-Ended Questions
item_map_open_bed <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "bed",      "open",      1,     "A112_01", "D203_01",  "bed_open_1",
  "bed",      "open",      2,     "A112_02", "D203_02",  "bed_open_2",
  "bed",      "open",      3,     "A112_03", "D203_03",  "bed_open_3"
)

item_map <- bind_rows(
  item_map_gr_bed,
  item_map_use_bed,
  item_map_open_bed
)

# Helping Patients into Bed Performance Expectancy
item_map_pe_bed <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "bed",      "pe",        1,     "A113_01", "D204_01",  "bed_pe_1",
  "bed",      "pe",        2,     "A113_02", "D204_02",  "bed_pe_2",
  "bed",      "pe",        3,     "A113_03", "D204_03",  "bed_pe_3",
  "bed",      "pe",        4,     "A113_04", "D204_04",  "bed_pe_4"
)

item_map <- bind_rows(
  item_map_gr_bed,
  item_map_use_bed,
  item_map_open_bed,
  item_map_pe_bed
)

# Helping Patients into Bed Attitudes Toward Using Robots
item_map_ar_bed <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "bed",      "ar",        1,     "A114_01", "D205_01",  "bed_ar_1",
  "bed",      "ar",        2,     "A114_02", "D205_02",  "bed_ar_2",
  "bed",      "ar",        3,     "A114_03", "D205_03",  "bed_ar_3",
  "bed",      "ar",        4,     "A114_04", "D205_04",  "bed_ar_4"
)

item_map <- bind_rows(
  item_map_gr_bed,
  item_map_use_bed,
  item_map_open_bed,
  item_map_pe_bed,
  item_map_ar_bed
)

#Helping Patients into Bed Self Efficacy

item_map_se_bed <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "bed",      "se",        1,     "A115_01", "D206_01",  "bed_se_1",
  "bed",      "se",        2,     "A115_02", "D206_02",  "bed_se_2",
  "bed",      "se",        3,     "A115_03", "D206_03",  "bed_se_3",
  "bed",      "se",        4,     "A115_04", "D206_04",  "bed_se_4"
)

item_map <- bind_rows(
  item_map_gr_bed,
  item_map_use_bed,
  item_map_open_bed,
  item_map_pe_bed,
  item_map_ar_bed,
  item_map_se_bed
)

#Robots Helping Patients into Bed Behavioral Intention 

item_map_bi_bed <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "bed",      "bi",        1,     "A116_01", "D207_01",  "bed_bi_1",
  "bed",      "bi",        2,     "A116_02", "D207_02",  "bed_bi_2",
  "bed",      "bi",        3,     "A116_03", "D207_03",  "bed_bi_3"
)

item_map <- bind_rows(
  item_map_gr_bed,
  item_map_use_bed,
  item_map_open_bed,
  item_map_pe_bed,
  item_map_ar_bed,
  item_map_se_bed,
  item_map_bi_bed
)

#Robots Helping Patients into Bed Moderate Effect Ethics 
item_map_me_bed <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,        ~canonical,
  "bed",     "me",        1,     "A117_01", "D208_01",      "bed_me_1",
  "bed",     "me",        2,     "A117_02", "D208_02",      "bed_me_2",
  "bed",     "me",        3,     "A117_03", NA_character_,  "bed_me_3",  # moral correctness (missing)
  "bed",     "me",        4,     "A117_04", "D208_03",      "bed_me_4",
  "bed",     "me",        5,     "A117_05", "D208_04",      "bed_me_5",
  "bed",     "me",        6,     "A117_06", "D208_05",      "bed_me_6",
  "bed",     "me",        7,     "A117_07", "D208_06",      "bed_me_7"
)

item_map <- bind_rows(
  item_map_gr_bed,
  item_map_use_bed,
  item_map_open_bed,
  item_map_pe_bed,
  item_map_ar_bed,
  item_map_se_bed,
  item_map_bi_bed,
  item_map_me_bed
)

#Robots Monitoring Vital Signs General Rating 

item_map_gr_mvs<- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "mvs",      "gr",        1,     "A209_01", "D301_01",  "mvs_gr_1",
  "mvs",      "gr",        2,     "A209_02", "D301_02",  "mvs_gr_2"
  
)

item_map <- bind_rows(
  item_map_gr_mvs
)

#Robots Monitoring Vital Signs Medical Settings

item_map_use_mvs <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "mvs",      "use",       0,     "A211",    "D302",     "mvs_use_0",
  "mvs",      "use",       1,     "A211_01", "D302_01",  "mvs_use_1",
  "mvs",      "use",       2,     "A211_02", "D302_02",  "mvs_use_2",
  "mvs",      "use",       3,     "A211_03", "D302_03",  "mvs_use_3",
  "mvs",      "use",       4,     "A211_04", "D302_04",  "mvs_use_4",
  "mvs",      "use",       5,     "A211_05", "D302_05",  "mvs_use_5",
  "mvs",      "use",       6,     "A211_06", "D302_06",  "mvs_use_6",
  "mvs",      "use",       7,     "A211_06a","D302_06a",  "mvs_use_7"
)

item_map <- bind_rows(
  item_map_gr_mvs,
  item_map_use_mvs
)

#Robots Monitoring Vital Signs Open-Ended Questions
item_map_open_mvs <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "mvs",      "open",      1,     "A212_01", "D303_01",  "mvs_open_1",
  "mvs",      "open",      2,     "A212_02", "D303_02",  "mvs_open_2",
  "mvs",      "open",      3,     "A212_03", "D303_03",  "mvs_open_3"
)

item_map <- bind_rows(
  item_map_gr_mvs,
  item_map_use_mvs,
  item_map_open_mvs
)

# Robots Monitoring Vital Signs Performance Expectancy
item_map_pe_mvs <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "mvs",      "pe",        1,     "A213_01", "D304_01",  "mvs_pe_1",
  "mvs",      "pe",        2,     "A213_02", "D304_02",  "mvs_pe_2",
  "mvs",      "pe",        3,     "A213_03", "D304_03",  "mvs_pe_3",
  "mvs",      "pe",        4,     "A213_04", "D304_04",  "mvs_pe_4"
)

item_map <- bind_rows(
  item_map_gr_mvs,
  item_map_use_mvs,
  item_map_open_mvs,
  item_map_pe_mvs
)

# Robots Monitoring Vital Signs Attitudes Toward Using Robots
item_map_ar_mvs <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "mvs",      "ar",        1,     "A214_01", "D305_01",  "mvs_ar_1",
  "mvs",      "ar",        2,     "A214_02", "D305_02",  "mvs_ar_2",
  "mvs",      "ar",        3,     "A214_03", "D305_03",  "mvs_ar_3",
  "mvs",      "ar",        4,     "A214_04", "D305_04",  "mvs_ar_4"
)

item_map <- bind_rows(
  item_map_gr_mvs,
  item_map_use_mvs,
  item_map_open_mvs,
  item_map_pe_mvs,
  item_map_ar_mvs
)

#Robots Monitoring Vital Signs Self Efficacy

item_map_se_mvs <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "mvs",      "se",        1,     "A215_01", "D306_01",  "mvs_se_1",
  "mvs",      "se",        2,     "A215_02", "D306_02",  "mvs_se_2",
  "mvs",      "se",        3,     "A215_03", "D306_03",  "mvs_se_3",
  "mvs",      "se",        4,     "A215_04", "D306_04",  "mvs_se_4"
)

item_map <- bind_rows(
  item_map_gr_mvs,
  item_map_use_mvs,
  item_map_open_mvs,
  item_map_pe_mvs,
  item_map_ar_mvs,
  item_map_se_mvs
)

#Robots Monitoring Vital Signs Behavioral Intention 

item_map_bi_mvs <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "mvs",      "bi",        1,     "A216_01", "D307_01",  "mvs_bi_1",
  "mvs",      "bi",        2,     "A216_02", "D307_02",  "mvs_bi_2",
  "mvs",      "bi",        3,     "A216_03", "D307_03",  "mvs_bi_3"
)

item_map <- bind_rows(
  item_map_gr_mvs,
  item_map_use_mvs,
  item_map_open_mvs,
  item_map_pe_mvs,
  item_map_ar_mvs,
  item_map_se_mvs,
  item_map_bi_mvs
)

#Robots Monitoring Vital Signs Moderate Effect Ethics 
item_map_me_mvs <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,        ~canonical,
  "mvs",     "me",        1,     "A217_01", "D308_01",      "mvs_me_1",
  "mvs",     "me",        2,     "A217_02", "D308_02",      "mvs_me_2",
  "mvs",     "me",        3,     "A217_03", NA_character_,  "mvs_me_3",  # moral correctness (missing)
  "mvs",     "me",        4,     "A217_04", "D308_03",      "mvs_me_4",
  "mvs",     "me",        5,     "A217_05", "D308_04",      "mvs_me_5",
  "mvs",     "me",        6,     "A217_06", "D308_05",      "mvs_me_6",
  "mvs",     "me",        7,     "A217_07", "D308_06",      "mvs_me_7"
)

item_map <- bind_rows(
  item_map_gr_mvs,
  item_map_use_mvs,
  item_map_open_mvs,
  item_map_pe_mvs,
  item_map_ar_mvs,
  item_map_se_mvs,
  item_map_bi_mvs,
  item_map_me_mvs
)

#Robots Assisting with Mobility General Rating 

item_map_gr_am<- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "am",      "gr",        1,     "A310_01", "D401_01",  "am_gr_1",
  "am",      "gr",        2,     "A310_02", "D401_02",  "am_gr_2"
  
)

item_map <- bind_rows(
  item_map_gr_am
)

#Robots Assisting with Mobility Medical Settings

item_map_use_am <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "am",      "use",       0,     "A311",    "D402",     "am_use_0",
  "am",      "use",       1,     "A311_01", "D402_01",  "am_use_1",
  "am",      "use",       2,     "A311_02", "D402_02",  "am_use_2",
  "am",      "use",       3,     "A311_03", "D402_03",  "am_use_3",
  "am",      "use",       4,     "A311_04", "D402_04",  "am_use_4",
  "am",      "use",       5,     "A311_05", "D402_05",  "am_use_5",
  "am",      "use",       6,     "A311_06", "D402_06",  "am_use_6",
  "am",      "use",       7,     "A311_06a","D402_06a",  "am_use_7"
)

item_map <- bind_rows(
  item_map_gr_am,
  item_map_use_am
)

#Robots Assisting with Mobility Open-Ended Questions
item_map_open_am <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "am",      "open",      1,     "A312_01", "D403_01",  "am_open_1",
  "am",      "open",      2,     "A312_02", "D403_02",  "am_open_2",
  "am",      "open",      3,     "A312_03", "D403_03",  "am_open_3"
)

item_map <- bind_rows(
  item_map_gr_am,
  item_map_use_am,
  item_map_open_am
)

# Robots Assisting with Mobility Performance Expectancy
item_map_pe_am <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "am",      "pe",        1,     "A313_01", "D404_01",  "am_pe_1",
  "am",      "pe",        2,     "A313_02", "D404_02",  "am_pe_2",
  "am",      "pe",        3,     "A313_03", "D404_03",  "am_pe_3",
  "am",      "pe",        4,     "A313_04", "D404_04",  "am_pe_4"
)

item_map <- bind_rows(
  item_map_gr_am,
  item_map_use_am,
  item_map_open_am,
  item_map_pe_am
)

# Robots Assisting with Mobility Attitudes Toward Using Robots
item_map_ar_am <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "am",      "ar",        1,     "A314_01", "D405_01",  "am_ar_1",
  "am",      "ar",        2,     "A314_02", "D405_02",  "am_ar_2",
  "am",      "ar",        3,     "A314_03", "D405_03",  "am_ar_3",
  "am",      "ar",        4,     "A314_04", "D405_04",  "am_ar_4"
)

item_map <- bind_rows(
  item_map_gr_am,
  item_map_use_am,
  item_map_open_am,
  item_map_pe_am,
  item_map_ar_am
)

#Robots Assisting with Mobility Self Efficacy

item_map_se_am <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "am",      "se",        1,     "A315_01", "D406_01",  "am_se_1",
  "am",      "se",        2,     "A315_02", "D406_02",  "am_se_2",
  "am",      "se",        3,     "A315_03", "D406_03",  "am_se_3",
  "am",      "se",        4,     "A315_04", "D406_04",  "am_se_4"
)

item_map <- bind_rows(
  item_map_gr_am,
  item_map_use_am,
  item_map_open_am,
  item_map_pe_am,
  item_map_ar_am,
  item_map_se_am
)

#Robots Assisting with Mobility Behavioral Intention 

item_map_bi_am <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,    ~canonical,
  "am",      "bi",        1,     "A316_01", "D407_01",  "am_bi_1",
  "am",      "bi",        2,     "A316_02", "D407_02",  "am_bi_2",
  "am",      "bi",        3,     "A316_03", "D407_03",  "am_bi_3"
)

item_map <- bind_rows(
  item_map_gr_am,
  item_map_use_am,
  item_map_open_am,
  item_map_pe_am,
  item_map_ar_am,
  item_map_se_am,
  item_map_bi_am
)

#Robots Assisting with Mobility Moderate Effect Ethics 
item_map_me_am <- tribble(
  ~scenario, ~construct, ~item, ~var_es,    ~var_en,        ~canonical,
  "am",     "me",        1,     "A317_01", "D408_01",      "am_me_1",
  "am",     "me",        2,     "A317_02", "D408_02",      "am_me_2",
  "am",     "me",        3,     "A317_03", NA_character_,  "am_me_3",  # moral correctness (missing)
  "am",     "me",        4,     "A317_04", "D408_03",      "am_me_4",
  "am",     "me",        5,     "A317_05", "D408_04",      "am_me_5",
  "am",     "me",        6,     "A317_06", "D408_05",      "am_me_6",
  "am",     "me",        7,     "A317_07", "D408_06",      "am_me_7"
)

item_map <- bind_rows(
  item_map_gr_am,
  item_map_use_am,
  item_map_open_am,
  item_map_pe_am,
  item_map_ar_am,
  item_map_se_am,
  item_map_bi_am,
  item_map_me_am
)


# lists all item_map objects that exist in your session
objs <- ls(pattern = "^item_map_")
objs <- setdiff(objs, "item_map")  # in case there is an incomplete item_map

# fetch all those dataframes and join them together
item_map <- dplyr::bind_rows(lapply(objs, get))

# Create canonical variables
data_canon <- data_clean

for (i in seq_len(nrow(item_map))) {
  es  <- item_map$var_es[i]
  en  <- item_map$var_en[i]
  new <- item_map$canonical[i]
  
  es_vec <- rep(NA, nrow(data_canon))
  en_vec <- rep(NA, nrow(data_canon))
  
  if (!is.na(es) && es %in% names(data_canon)) es_vec <- data_canon[[es]]
  if (!is.na(en) && en %in% names(data_canon)) en_vec <- data_canon[[en]]
  
  data_canon[[new]] <- ifelse(data_canon$language == "ES", es_vec, en_vec)
}


data_preview <- data_canon
data_preview %>%
  dplyr::select(CASE, language, dplyr::starts_with("bed_me_")) %>%
  head(20)

# Comparable items (robust: the EN column exists in the dataset)
item_map <- item_map %>%
  dplyr::mutate(
    comparable = !is.na(var_en) & var_en %in% names(data_canon)
  )

# Likert items to export (comparables)
likert_items <- item_map %>%
  dplyr::filter(
    construct %in% c("gr", "pe", "ar", "se", "bi", "me"),
    comparable
  ) %>%
  dplyr::pull(canonical)

# Sumscores
constructs_to_sum <- c("gr", "pe", "ar", "se", "bi", "me")

scales <- item_map %>%
  dplyr::filter(construct %in% constructs_to_sum, comparable) %>%
  dplyr::group_by(scenario, construct) %>%
  dplyr::summarise(cols = list(canonical), .groups = "drop")

row_sum_min <- function(df, cols, min_n = 1) {
  x <- df[, cols, drop = FALSE]
  n_nonmiss <- rowSums(!is.na(x))
  s <- rowSums(x, na.rm = TRUE)
  ifelse(n_nonmiss >= min_n, s, NA_real_)
}

for (k in seq_len(nrow(scales))) {
  sc   <- scales$scenario[k]
  co   <- scales$construct[k]
  cols <- intersect(scales$cols[[k]], names(data_canon))
  if (length(cols) == 0) next
  
  min_n <- ceiling(length(cols) / 2)
  sum_name <- paste0(sc, "_", co, "_sum")
  
  data_canon[[sum_name]] <- row_sum_min(data_canon, cols, min_n = min_n)
}

# define sum_vars 
sum_vars <- grep("_sum$", names(data_canon), value = TRUE)

# Export for JASP
jasp_likert <- data_canon %>%
  dplyr::select(
    CASE,
    language,
    dplyr::any_of(c("age", "gender", "years_experience", "country")),
    dplyr::all_of(likert_items),
    dplyr::all_of(sum_vars)
  )

dir.create("exports", showWarnings = FALSE)
write.csv(jasp_likert, file.path("exports", "jasp_likert.csv"), row.names = FALSE)

#Creation Robots in Medical Settings USE dataset for JASP

table(data_canon$ds_use_1, useNA="ifany")

# "use" from item_map

use_cols <- item_map %>%
  dplyr::filter(construct == "use") %>%
  dplyr::pull(canonical) %>%
  unique()

# Convert TRUE/FALSE -> 1/0  

data_canon <- data_canon %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(use_cols),
      function(x) if (is.logical(x)) as.numeric(x) else x
    )
  )

#Convert NA -> 0 ONLY for numeric "use" columns
#    (so "not selected" becomes 0; open-ended text stays as text)
data_canon <- data_canon %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(use_cols),
      function(x) {
        if (is.numeric(x)) tidyr::replace_na(x, 0) else x
      }
    )
  )
# Prepare columns to keep 
# Robust for CASE/case

id_col  <- intersect(c("CASE", "case"), names(data_canon))
covars  <- intersect(c("age", "gender", "years_experience", "country"), names(data_canon))

keep_cols <- unique(c(id_col, "language", covars, use_cols))

# Final Dataset (contexts/use)

jasp_contexts <- data_canon[, keep_cols, drop = FALSE]


# 5) Export dataset

dir.create("exports", showWarnings = FALSE)
write.csv(jasp_contexts, file.path("exports", "jasp_contexts.csv"), row.names = FALSE)

# 6) Quick Checks 

sapply(jasp_contexts[, use_cols], class)
use_numeric <- use_cols[
  sapply(jasp_contexts[, use_cols], is.numeric)
]
colSums(jasp_contexts[, use_numeric], na.rm = TRUE)



# Create Dataset Long (one line per answer)

open_cols <- item_map %>%
  dplyr::filter(construct == "open") %>%
  dplyr::pull(canonical) %>%
  unique()

open_long <- data_canon %>%
  dplyr::select(dplyr::any_of(c("CASE","case")), language, dplyr::all_of(open_cols)) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(open_cols),
    names_to = "var",
    values_to = "text"
  ) %>%
  dplyr::mutate(
    scenario  = stringr::str_extract(var, "^(ds|bed|mvs|am)"),
    question  = stringr::str_extract(var, "open_\\d+")
  ) %>%
  dplyr::select(dplyr::any_of(c("CASE","case")), language, scenario, question, text) %>%
  dplyr::filter(!is.na(text), stringr::str_trim(text) != "")

# Checks (opcionales)
table(open_long$scenario, useNA = "ifany")
table(open_long$language, useNA = "ifany")
table(open_long$question, useNA = "ifany")

open_long <- open_long %>%
  dplyr::mutate(
    robot_label = dplyr::recode(
      scenario,
      ds  = "Delivering Supplies",
      bed = "Helping Patients into Bed",
      mvs = "Monitoring Vital Signs",
      am  = "Assisting with Mobility"
    )
  )

dir.create("exports", showWarnings = FALSE)
write.csv(
  open_long,
  file.path("exports", "open_ended_long.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)







