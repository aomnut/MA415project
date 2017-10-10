################################################################################


# SUPPRESS GLOBAL WARNING
options(warn = -1)

# SET TO ONE DIGIT AFTER DECIMAL POINT
options(digits = 2)

# PREPARE PACKAGES
if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr", "ggplot2", "tibble", "tidyr", "readxl", "reshape2")

###########################
### DOWNLOAD DATA FILES ###
###########################

# HOUSING UNIT TYPE DATA
download.file(
  "https://www.eia.gov/consumption/residential/data/2015/hc/hc3.1.xlsx",
  "UNIT.xlsx", quiet = TRUE, mode = "wb")

# OWNER/RENTER STATUS DATA
download.file(
  "https://www.eia.gov/consumption/residential/data/2015/hc/hc3.2.xlsx",
  "STATUS.xlsx", quiet = TRUE, mode = "wb")

# HOUSEHOLD INCOME DATA
download.file(
  "https://www.eia.gov/consumption/residential/data/2015/hc/hc3.5.xlsx",
  "INCOME.xlsx", quiet = TRUE, mode = "wb")

# CLIMATE REGION DATA
download.file(
  "https://www.eia.gov/consumption/residential/data/2015/hc/hc3.6.xlsx",
  "CLIMATE.xlsx", quiet = TRUE, mode = "wb")

# NORTHEAST AND MIDWEST REGION DATA
download.file(
  "https://www.eia.gov/consumption/residential/data/2015/hc/hc3.7.xlsx",
  "NORTH_MID.xlsx", quiet = TRUE, mode = "wb")

# SOUTH AND WEST REGION DATA
download.file(
  "https://www.eia.gov/consumption/residential/data/2015/hc/hc3.8.xlsx",
  "SOUTH_WEST.xlsx", quiet = TRUE, mode = "wb")

###################
### IMPORT DATA ###
###################

# IMPORT AND STORE DATA
T_UNIT_0    <- read_excel("UNIT.xlsx", sheet = "data", range = "A115:G121",
                          col_names = FALSE, col_types = "text")

T_STATUS_0  <- read_excel("STATUS.xlsx", sheet = "data", range = "A117:H123",
                          col_names = FALSE, col_types = "text")

T_INCOME_0  <- read_excel("INCOME.xlsx", sheet = "data", range = "A115:J121",
                          col_names = FALSE, col_types = "text")

T_CLIMATE_0 <- read_excel("CLIMATE.xlsx", sheet = "data", range = "A115:G121",
                          col_names = FALSE, col_types = "text")

T_NM_0      <- read_excel("NORTH_MID.xlsx", sheet = "data", range = "A115:H121",
                          col_names = FALSE, col_types = "text")

T_SW_0      <- read_excel("SOUTH_WEST.xlsx", sheet = "data", range = "A118:K124",
                          col_names = FALSE, col_types = "text")

######################
### NORMALIZE DATA ###
######################

# COPY DATA FOR FUTURE MANIPULATION
T_UNIT_1    <- T_UNIT_0
T_STATUS_1  <- T_STATUS_0
T_INCOME_1  <- T_INCOME_0
T_CLIMATE_1 <- T_CLIMATE_0
T_NM_1      <- T_NM_0
T_SW_1      <- T_SW_0

# RENAME COLUMNS USING CONVENTION
colnames(T_UNIT_1)    <- c("F_TYPE", "TTL_CNT", "SGL_DR",
                           "SGL_AT", "APT_2_4", "APT_5", "MOB")

colnames(T_STATUS_1)  <- c("F_TYPE", "TTL_CNT", "TTL_OWN",
                           "TTL_RENT", "SGL_MOB_OWN", "SGL_MOB_RENT",
                           "APT_OWN", "APT_RENT")

colnames(T_INCOME_1)  <- c("F_TYPE", "TTL_CNT", "20_LESS", "20_40",
                           "40_60", "60_80", "80_100", "100_120",
                           "120_140", "140_MORE")

colnames(T_CLIMATE_1) <- c("F_TYPE", "TTL_CNT", "VERY_COLD",
                           "M_HUMID", "M_DRY", "HOT_HUMID", "MAR")

colnames(T_NM_1)      <- c("F_TYPE", "TTL_CNT", "N_TTL_NE",
                           "N_ENG", "N_MID_ATL", "M_TTL_MW",
                           "M_EN_C", "M_WN_C")

colnames(T_SW_1)      <- c("F_TYPE", "TTL_CNT", "S_TTL_S",
                           "S_S_ATL", "S_ES_C", "S_WS_C",
                           "W_TTL_W", "W_TTL_MTN", "W_MTN_N",
                           "W_MTN_S", "W_PAC")

# DROP THE TOTAL COLUMN
T_UNIT_2    <- within(T_UNIT_1, rm(TTL_CNT))
T_STATUS_2  <- within(T_STATUS_1, rm(TTL_CNT, TTL_OWN, TTL_RENT))
T_INCOME_2  <- within(T_INCOME_1, rm(TTL_CNT))
T_CLIMATE_2 <- within(T_CLIMATE_1, rm(TTL_CNT))
T_NM_2      <- within(T_NM_1, rm(TTL_CNT, N_TTL_NE, M_TTL_MW))
T_SW_2      <- within(T_SW_1, rm(TTL_CNT, S_TTL_S, W_TTL_W, W_TTL_MTN))

# COPY DATA FOR FUTURE MANIPULATION
T_UNIT_3    <- T_UNIT_2
T_STATUS_3  <- T_STATUS_2
T_INCOME_3  <- T_INCOME_2
T_CLIMATE_3 <- T_CLIMATE_2
T_NM_3      <- T_NM_2
T_SW_3      <- T_SW_2

# COERCE FRIDGE TYPE COLUMN TO FACTOR
T_UNIT_3$F_TYPE    <- as.factor(T_UNIT_3$F_TYPE)
T_STATUS_3$F_TYPE  <- as.factor(T_STATUS_3$F_TYPE)
T_INCOME_3$F_TYPE  <- as.factor(T_INCOME_3$F_TYPE)
T_CLIMATE_3$F_TYPE <- as.factor(T_CLIMATE_3$F_TYPE)
T_NM_3$F_TYPE      <- as.factor(T_NM_3$F_TYPE)
T_SW_3$F_TYPE      <- as.factor(T_SW_2$F_TYPE)

# COERCE OTHER COLUMNS TO NUMERIC, MISSING DATA COERCED AS NA
T_UNIT_3[, 2:6]    <- sapply(T_UNIT_3[, 2:6], as.numeric)
T_STATUS_3[, 2:5]  <- sapply(T_STATUS_3[, 2:5], as.numeric)
T_INCOME_3[, 2:9]  <- sapply(T_INCOME_3[, 2:9], as.numeric)
T_CLIMATE_3[, 2:6] <- sapply(T_CLIMATE_3[, 2:6], as.numeric)
T_NM_3[, 2:5]      <- sapply(T_NM_3[, 2:5], as.numeric)
T_SW_3[, 2:7]      <- sapply(T_SW_3[, 2:7], as.numeric)

# COPY DATA FOR FUTURE MANIPULATION
T_UNIT_4    <- T_UNIT_3
T_STATUS_4  <- T_STATUS_3
T_INCOME_4  <- T_INCOME_3
T_CLIMATE_4 <- T_CLIMATE_3
T_NM_4      <- T_NM_3
T_SW_4      <- T_SW_3

# UPDATE COLUMN NAMES TO MELT
colnames(T_UNIT_4)    <- c("Fridge Type", "Single-family detached",
                           "Single-family attached",
                           "Apartment (2- to 4-unit)",
                           "Apartment (5 or more unit)",
                           "Mobile home")

colnames(T_STATUS_4)  <- c("Fridge Type",
                           "Single-family and mobile homes/Own",
                           "Single-family and mobile homes/Rent",
                           "Apartments/Own", "Apartments/Rent")

colnames(T_INCOME_4)  <- c("Fridge Type", "Less than $20,000",
                           "$20,000 to $39,999", "$40,000 to $59,999",
                           "$60,000 to $79,999", "$80,000 to $99,999",
                           "$100,000 to $119,999",
                           "$120,000 to $139,999",
                           "$140,000 or more")

colnames(T_CLIMATE_4) <- c("Fridge Type", "Very cold/cold",
                           "Mixed-humid", "Mixed-dry/Hot-dry",
                           "Hot-humid", "Marine")

colnames(T_NM_4)      <- c("Fridge Type", "Northeast Census Region/New England",
                           "Northeast Census Region/Middle Atlantic",
                           "Midwest Census Region/East North Central",
                           "Midwest Census Region/West North Central")

colnames(T_SW_4)      <- c("Fridge Type", "South Census Region/South Atlantic",
                           "South Census Region/East South Central",
                           "South Census Region/West South Central",
                           "West Census Region/Mountain North",
                           "West Census Region/Mountain South",
                           "West Census Region/Pacific")

# MELT DATA TO STANDARDIZED FORMAT
T_UNIT_5    <- as_tibble(melt(T_UNIT_4, id = 1))
T_STATUS_5  <- as_tibble(melt(T_STATUS_4, id = 1))
T_INCOME_5  <- as_tibble(melt(T_INCOME_4, id = 1))
T_CLIMATE_5 <- as_tibble(melt(T_CLIMATE_4, id = 1))
T_NM_5      <- as_tibble(melt(T_NM_4, id = 1))
T_SW_5      <- as_tibble(melt(T_SW_4, id = 1))

# COPY DATA FOR FUTURE MANIPULATION
T_UNIT_6    <- T_UNIT_5
T_STATUS_6  <- T_STATUS_5
T_INCOME_6  <- T_INCOME_5
T_CLIMATE_6 <- T_CLIMATE_5
T_NM_6      <- T_NM_5
T_SW_6      <- T_SW_5

# UPDATE COLUMN NAMES
colnames(T_UNIT_6)[colnames(T_UNIT_6) == "variable"]       <- "Unit Type"
colnames(T_STATUS_6)[colnames(T_STATUS_6) == "variable"]   <- "Unit Type/Status"
colnames(T_INCOME_6)[colnames(T_INCOME_6) == "variable"]   <- "Income"
colnames(T_CLIMATE_6)[colnames(T_CLIMATE_6) == "variable"] <- "Climate Region"
colnames(T_NM_6)[colnames(T_NM_6) == "variable"]           <- "Region/Subregion"
colnames(T_SW_6)[colnames(T_SW_6) == "variable"]           <- "Region/Subregion"

# UPDATE COLUMN NAMES
colnames(T_UNIT_6)[colnames(T_UNIT_6) == "value"]       <- "Total Frequency"
colnames(T_STATUS_6)[colnames(T_STATUS_6) == "value"]   <- "Total Frequency"
colnames(T_INCOME_6)[colnames(T_INCOME_6) == "value"]   <- "Total Frequency"
colnames(T_CLIMATE_6)[colnames(T_CLIMATE_6) == "value"] <- "Total Frequency"
colnames(T_NM_6)[colnames(T_NM_6) == "value"]           <- "Total Frequency"
colnames(T_SW_6)[colnames(T_SW_6) == "value"]           <- "Total Frequency"

# SPLIT AND COERCE TO FACTOR FOR OWN/RENT STATUS TABLE ONLY
T_STATUS_6 <- separate(T_STATUS_6, `Unit Type/Status`,
                       into = c("Unit Type", "Owner/Renter Status"),
                       sep = "/")

T_NM_6     <- separate(T_NM_6, `Region/Subregion`,
                       into = c("Region", "Subregion"),
                       sep = "/")

T_SW_6     <- separate(T_SW_6, `Region/Subregion`,
                       into = c("Region", "Subregion"),
                       sep = "/")

# CONVERT COLUMN TO FACTOR
T_STATUS_6[, 2:3] <- lapply(T_STATUS_6[, 2:3], as.factor)
T_NM_6[, 2:3]     <- lapply(T_NM_6[, 2:3], as.factor)
T_SW_6[, 2:3]     <- lapply(T_SW_6[, 2:3], as.factor)

# STORE RESULTS
P_UNIT    <- T_UNIT_6
P_STATUS  <- T_STATUS_6
P_INCOME  <- T_INCOME_6
P_CLIMATE <- T_CLIMATE_6
P_NM      <- T_NM_6
P_SW      <- T_SW_6

# REMOVE TEMPORARY TABLES
rm(list = ls(pattern = "^T_"))

####################
### ANALYZE DATA ###
####################

# AN EXAMPLE HERE
ggplot(P_UNIT, aes(x = `Fridge Type`, y = `Total Frequency`)) +
  geom_col(aes(fill = `Unit Type`)) + coord_flip() +
  labs(title = "ggplot Example")

#####################
### EXPORT OUTPUT ###
#####################

# XYZ

#####################
### END OF SCRIPT ###
#####################

# Note: If your code is longer than the hashtags # below (>80 characters),
# split it over several lines just like what I am doing right now. PLEASE.
################################################################################
####################
### ANALYZE DATA ###

##############################################################
# Dplyr for Two doors, top freezer"
library(dplyr)
# extract "door top"
P_CLIMATE[is.na(P_CLIMATE)] <- 0

P_CLIMATE_vcold<-  mutate(filter(P_CLIMATE, `Climate Region` == "Very cold/cold"), per=`Total Frequency`/sum(`Total Frequency`))
P_CLIMATE_humid<-  mutate(filter(P_CLIMATE, `Climate Region`== "Mixed-humid"), per=`Total Frequency`/sum(`Total Frequency`))
P_CLIMATE_dry <-   mutate(filter(P_CLIMATE, `Climate Region` == "Mixed-dry/Hot-dry"), per=`Total Frequency`/sum(`Total Frequency`))
P_CLIMATE_hot <- mutate(filter(P_CLIMATE, `Climate Region` == "Hot-humid"), per=`Total Frequency`/sum(`Total Frequency`))
P_CLIMATE_mar <-   mutate(filter(P_CLIMATE, `Climate Region` == "Marine"), per=`Total Frequency`/sum(`Total Frequency`))

P_CLIMATE_per <- rbind(P_CLIMATE_vcold,P_CLIMATE_humid,P_CLIMATE_dry,P_CLIMATE_hot,P_CLIMATE_mar)

ggplot(P_CLIMATE_per, aes(x = `Climate Region`, y = `per`)) + geom_col(aes(fill = `Fridge Type`)) 

####################
P_CLIMATE_top<-  mutate(filter(P_CLIMATE, `F_TYPE` == "Two doors, top freezer"), per=value/sum(value))
P_CLIMATE_bot<-  mutate(filter(P_CLIMATE, `F_TYPE`== "Two doors, bottom freezer"), per=value/sum(value))
P_CLIMATE_side <-   mutate(filter(P_CLIMATE, `F_TYPE` == "Two doors, side by side"), per=value/sum(value))
P_CLIMATE_three <- mutate(filter(P_CLIMATE, `F_TYPE` == "Three or more doors"), per=value/sum(value))
P_CLIMATE_one <-   mutate(filter(P_CLIMATE, `F_TYPE` == "One door"), per=value/sum(value))
P_CLIMATE_half <-   mutate(filter(P_CLIMATE, `F_TYPE` == "Half-size/compact"), per=value/sum(value))
P_CLIMATE_dont <-   mutate(filter(P_CLIMATE, `F_TYPE` == "Do not use a refrigerator"), per=value/sum(value))

P_CLIMATE_per <- rbind(P_CLIMATE_bot,P_CLIMATE_dont,P_CLIMATE_one,P_CLIMATE_three,P_CLIMATE_top,P_CLIMATE_side,P_CLIMATE_half)

ggplot(P_CLIMATE_per, aes(x = `F_TYPE`, y = `per`)) + geom_col(aes(fill = `Climate Region`)) 


####################

# XYZ

#####################
ggplot(data=P_CLIMATE, aes(x=P_CLIMATE$`Climate Region`, y=P_CLIMATE$`Total Frequency`, group=P_CLIMATE$`Fridge Type`,colour=P_CLIMATE$`Fridge Type`))+geom_dotplot()+geom_col(aes(fill = `Fridge Type`)) +
  labs(title = "ggplot Example")

