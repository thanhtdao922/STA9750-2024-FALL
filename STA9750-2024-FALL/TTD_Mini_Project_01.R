if(!require("tidyverse")) install.packages("tidyverse")

# Obtain the Necessary Data ###################################################
## Fare Revenue ==============================================================
library(tidyverse)
if(!file.exists("2022_fare_revenue.xlsx")){
  download.file("http://www.transit.dot.gov/sites/fta.dot.gov/files/2024-04/2022%20Fare%20Revenue.xlsx", 
                destfile="2022_fare_revenue.xlsx", 
                quiet=FALSE, 
                method="wget")
}
FARES <- readxl::read_xlsx("2022_fare_revenue.xlsx") |>
  select(-`State/Parent NTD ID`, 
         -`Reporter Type`,
         -`Reporting Module`,
         -`TOS`,
         -`Passenger Paid Fares`,
         -`Organization Paid Fares`) |>
  filter(`Expense Type` == "Funds Earned During Period") |>
  select(-`Expense Type`)

## Expenses ===================================================================
if(!file.exists("2022_expenses.csv")){
  download.file("https://data.transportation.gov/api/views/dkxx-zjd6/rows.csv?date=20231102&accessType=DOWNLOAD&bom=true&format=true", 
                destfile="2022_expenses.csv", 
                quiet=FALSE, 
                method="wget")
}
EXPENSES <- readr::read_csv("2022_expenses.csv") |>
  select(`NTD ID`, 
         `Agency`,
         `Total`, 
         `Mode`) |>
  mutate(`NTD ID` = as.integer(`NTD ID`)) |>
  rename(Expenses = Total) |>
  group_by(`NTD ID`, `Mode`) |>
  summarize(Expenses = sum(Expenses)) |>
  ungroup()

FINANCIALS <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))

## Monthly Transit Numbers ====================================================
library(tidyverse)
if(!file.exists("ridership.xlsx")){
  download.file("https://www.transit.dot.gov/sites/fta.dot.gov/files/2024-09/July%202024%20Complete%20Monthly%20Ridership%20%28with%20adjustments%20and%20estimates%29_240903.xlsx", 
                destfile="ridership.xlsx", 
                quiet=FALSE, 
                method="wget")
}
library(lubridate)  # added for the my() function

TRIPS <- readxl::read_xlsx("ridership.xlsx", sheet="UPT") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="UPT") |>
  drop_na() |>
  mutate(month=my(month)) 
MILES <- readxl::read_xlsx("ridership.xlsx", sheet="VRM") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`, 
         -`Reporter Type`, 
         -`Mode/Type of Service Status`, 
         -`UACE CD`, 
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`), 
               names_to="month", 
               values_to="VRM") |>
  drop_na() |>
  group_by(`NTD ID`, `Agency`, `UZA Name`, 
           `Mode`, `3 Mode`, month) |>
  summarize(VRM = sum(VRM)) |>
  ungroup() |>
  mutate(month=my(month))

USAGE <- inner_join(TRIPS, MILES) |>
  mutate(`NTD ID` = as.integer(`NTD ID`))

# Create the Table ############################################################
if(!require("DT")) install.packages("DT")
library(DT)

sample_n(USAGE, 1000) |> 
  mutate(month=as.character(month)) |> 
  DT::datatable()

# Tasks #######################################################################

## Task 1: Create Syntactic Names =============================================
USAGE <- rename(USAGE, "metro_area" = "UZA Name")
USAGE <- rename(USAGE, "unlinked_passenger_trips" = "UPT")
USAGE <- rename(USAGE, "vehicle_revenue_miles" = "VRM")
USAGE <- rename(USAGE, "NTD_ID" = "NTD ID")
FINANCIALS <- rename(FINANCIALS, "NTD_ID" = "NTD ID")

## Task 2: Recoding the Mode Column ===========================================
unique(USAGE$Mode)   # Find the unique Mode codes

USAGE <- USAGE |>          # Interpret the Mode column (USAGE)
  mutate(Mode = case_when(
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "CC" ~ "Cable Car",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferryboat",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "LR" ~ "Light Rail",
    Mode == "MB" ~ "Bus",
    Mode == "MG" ~ "Monorail and Automated Guideway",
    Mode == "PB" ~ "Publico",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "TR" ~ "Aerial Tramways",
    Mode == "VP" ~ "Vanpool",
    Mode == "YR" ~ "Hybrid Rail",
    TRUE ~ "Unknown"))

FINANCIALS <- FINANCIALS |>          # Interpret the Mode column (Financials)
  mutate(Mode = case_when(
    Mode == "AR" ~ "Alaska Railroad",
    Mode == "CB" ~ "Commuter Bus",
    Mode == "CC" ~ "Cable Car",
    Mode == "CR" ~ "Commuter Rail",
    Mode == "DR" ~ "Demand Response",
    Mode == "FB" ~ "Ferryboat",
    Mode == "HR" ~ "Heavy Rail",
    Mode == "IP" ~ "Inclined Plane",
    Mode == "LR" ~ "Light Rail",
    Mode == "MB" ~ "Bus",
    Mode == "MG" ~ "Monorail and Automated Guideway",
    Mode == "PB" ~ "Publico",
    Mode == "RB" ~ "Bus Rapid Transit",
    Mode == "SR" ~ "Streetcar Rail",
    Mode == "TB" ~ "Trolleybus",
    Mode == "TR" ~ "Aerial Tramways",
    Mode == "VP" ~ "Vanpool",
    Mode == "YR" ~ "Hybrid Rail",
    TRUE ~ "Unknown"))

USAGE <- USAGE |>           # Remove "3 Mode"
  select(-c("3 Mode"))

if(!require("DT")) install.packages("DT")
library(DT)
sample_n(USAGE, 1000) |>    # Updated Summary Table
  mutate(month=as.character(month)) |> 
  DT::datatable()

## Task 3: Answering Instructor Specified Questions with dplyr ================
USAGE |>   # What transit agency had the most total VRM in this sample?
  group_by(Agency) |>
  summarize(total_vrm = sum(vehicle_revenue_miles, na.rm = T)) |>
  arrange(desc(total_vrm))

USAGE |>   # What transit mode had the most total VRM in this sample?
  group_by(Mode) |>
  summarize(total_vrm = sum(vehicle_revenue_miles, na.rm = T)) |>
  arrange(desc(total_vrm))

USAGE$month <- as.character(USAGE$month)  # How many trips were taken on the NYC Subway (Heavy Rail) in May 2024?
USAGE |>
  filter(USAGE$Agency == "MTA New York City Transit",
         USAGE$Mode == "Heavy Rail",
         USAGE$month == "2024-05-01")

USAGE |>  # How much did NYC subway ridership fall between April 2019 and April 2020?
  filter(USAGE$Agency == "MTA New York City Transit",
         USAGE$Mode == "Heavy Rail",
         USAGE$month == "2019-04-01")
USAGE |>
  filter(USAGE$Agency == "MTA New York City Transit",
         USAGE$Mode == "Heavy Rail",
         USAGE$month == "2020-04-01")

## Task 4: Explore and Analyze ================================================
USAGE |>   # What transit agency had the least total VRM in this sample?
  group_by(Agency) |>
  summarize(total_vrm = sum(vehicle_revenue_miles, na.rm = T)) |>
  arrange(total_vrm)

USAGE |>   # What transit mode had the least total VRM in this sample?
  group_by(Mode) |>
  summarize(total_vrm = sum(vehicle_revenue_miles, na.rm = T)) |>
  arrange(total_vrm)

USAGE |>   # What metro area had the 2nd largest average VRM in this sample?
  group_by(metro_area) |>
  summarize(average_vrm = mean(vehicle_revenue_miles, na.rm = T)) |>
  arrange(desc(average_vrm))

## Task 5: Table Summarization ================================================
USAGE_2022_ANNUAL <- USAGE |>
  mutate(year = year(month)) |>
  filter(year == 2022) |>
  group_by(NTD_ID, 
           Agency, 
           metro_area, 
           Mode, 
           unlinked_passenger_trips, 
           vehicle_revenue_miles) |>
  summarize(
    total_upt = sum(unlinked_passenger_trips, na.rm = T),
    total_vrm = sum(vehicle_revenue_miles, na.rm = T),
    .groups = "keep",
  ) |>
  ungroup()

USAGE_AND_FINANCIALS <- left_join(USAGE_2022_ANNUAL, 
                                  FINANCIALS, 
                                  join_by(NTD_ID, Mode)) |>
  drop_na()
## Task 6: Farebox Recovery Among Major Systems ===============================

mostUPT2022 <- USAGE_AND_FINANCIALS |> # Which system had the most UPT in 2022?
  group_by(Agency, Mode) |>
  filter(total_upt > 400000) |>
  summarize(total_upt2022 = sum(total_upt)) |>
  arrange(desc(total_upt2022))
head(mostUPT2022, n=1)

highestfarebox <- USAGE_AND_FINANCIALS |> # Which system had the highest farebox recovery?
  group_by(Agency, Mode) |>
  filter(total_upt > 400000) |>
  summarize(highestfarebox = sum(`Total Fares`) / sum (Expenses)) |>
  arrange(desc(highestfarebox))
head(highestfarebox, n=1)

lowestexpenses <- USAGE_AND_FINANCIALS |> # Which system has the lowest expenses per UPT?
  group_by(Agency, Mode) |>
  filter(total_upt > 400000) |>
  summarize(lowestexpenses = sum(Expenses) / sum(total_upt)) |>
  arrange(desc(lowestexpenses))
tail(lowestexpenses, n=1)

highestfares <- USAGE_AND_FINANCIALS |> # Which system has the highest total fares per UPT?
  group_by(Agency, Mode) |>
  filter(total_upt > 400000) |>
  summarize(highestfares = sum(`Total Fares`) / sum(total_upt)) |>
  arrange(desc(highestfares))
head(highestfares, n=1)

lowestexpensesvrm <- USAGE_AND_FINANCIALS |> # Which system has the lowest expenses per VRM?
  group_by(Agency, Mode) |>
  filter(total_upt > 400000) |>
  summarize(lowestexpensesvrm = sum(Expenses) / sum(total_vrm)) |>
  arrange(desc(lowestexpensesvrm))
tail(lowestexpensesvrm, n=1)

highestfaresvrm <- USAGE_AND_FINANCIALS |> # Which system has the highest total fares per VRM?
  group_by(Agency, Mode) |>
  filter(total_upt > 400000) |>
  summarize(highestfaresvrm = sum(`Total Fares`) / sum(total_vrm)) |>
  arrange(desc(highestfaresvrm))
head(highestfaresvrm, n=1)





