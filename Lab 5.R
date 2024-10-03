# Review Practice, 9/26/2024 ###################################################

## Obtain the data =============================================================
if(!file.exists("births.csv")){
  download.file("https://raw.githubusercontent.com/michaelweylandt/STA9750/main/births.csv", 
                destfile="births.csv")
}
library(readr)
library(dplyr)

births <- read_csv("births.csv")
glimpse(births)

## How many children were born on January 1st, 1984? ===========================

births |>
  filter(day == 1,
         month == 1,
         year == 1984)
print("8,013 children were born on January 1st, 1984.")

## How many total children were born in 1984? ==================================

births |> 
  filter(year == 1984) |> 
  summarize(sum(births))
print("A total of 3,673,531 children were born in 1984.")

## How many children were born each year? ======================================

births |> 
  group_by(year) |> 
  summarize(n_births = sum(births))

## How many more children were born each year than the preceeding? =============

births |> 
  group_by(year) |>
  summarize(n_births = sum(births)) |>
  mutate(increase_births = n_births - lag(n_births))

## On average, in what month are the most children born? =======================

births |> 
  group_by(month) |>
  summarize(avg_births = mean(births)) |>
  slice_max(avg_births)
print("On average, the most children are born in September, with an average of 10,343 births.")

# Multi Table Operations #######################################################

## Obtain the data =============================================================

if(!require("tidyverse")) install.packages("tidyverse")
if(!require("nycflights13")) install.packages("nycflights13")
library(tidyverse)
library(nycflights13)
glimpse(flights)
glimpse(airlines)
glimpse(airports)
glimpse(planes)
glimpse(weather)

## Join Specifications =========================================================

inner_join(flights,     # Join "origin" column of "flights" table to 
           airports,    # "faa" column of "airports" table
           join_by(origin == faa))

inner_join(flights,     # Align flights with weather at their origin airport
           weather,     # at scheduled take off time
           join_by(origin == origin,  # Here, join_by behaves like filter
                   year == year,      # "Passes" intersection of positive results
                   month == month,
                   day == day,
                   hour == hour))   

inner_join(flights,     # More concisely written code of above
           weather,     # Don't do this lol
           join_by(origin, 
                   year, 
                   month, 
                   day, 
                   hour))

## Inner Joins =================================================================

print("What is the average arrival delay of flights going to west coast airports?")

west_coast_airports <- airports |>        # Identify west coast airports
  filter(tzone == "America/Los_Angeles")  # Filter "airports" on "tzone"

inner_join(flights,              # Find only flights with destination
           west_coast_airports,  # matches in west_coast_airports
           join_by(dest == faa)) |>
  summarize(mean(arr_delay, na.rm=TRUE))  # Compute relevant summary stat

print("The average arrival delay of flights going to west coast airports is 1.28 hours.")

inner_join(flights,    # Alternative approach
           airports,   # Better to do the above way (clarity of intent)
           join_by(dest == faa)) |>
  filter(tzone == "America/Los_Angeles") |>
  drop_na() |>
  summarize(mean(arr_delay, na.rm=TRUE))

inner_join(flights,    # Another alternative approach
           airports |> 
             filter(tzone == "America/Los_Angeles"), 
           join_by(dest == faa)) |>
  drop_na() |>
  summarize(mean(arr_delay, na.rm=TRUE))

## What is the name of the airline with the longest average departure delay? ===========================================================

flights |>
  inner_join(airlines, by = "carrier") |>
  group_by(name) |>
  summarize(average_dep_delay = mean(dep_delay, na.rm = T)) |> 
  arrange(desc(average_dep_delay))
print("The airline with the longest average departure delay is Frontier Airlines Inc, with 20.2 minutes.")

## What is the name of the origin airport with the longest average departure delay? ====================================================

print("The origin airport with the longest average departure delay is _.")

## What is the name of the destination airport with the longest average departure delay? ================================

print("The destination airport with the longest average departure delay is _.")

## Are average delays longer for East-coast destinations or West-coast destinations? ==========================

print("Average delays are longer for _-coast destinations.")

## Which plane (tailnum) flew the most times leaving NYC? Who manufactured it? =================================

print("_ flew the most times leaving NYC.")
print("_ manufactured it.")

## Which manufacturer has the most planes flying out of NYC airports? ===================

print("_ has the most planes flying out of NYC airports.")

## Which manufacturer has the longest average flight? ===========================

print("_ has the longest average flight.")

## What model of plane has the smallest average delay leaving NYC? =================

print("_ has the smallest average delay leaving NYC.")
