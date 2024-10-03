## This is the pre-assignment for week 5 for STA9750.
## We are handling "Multi-Table dplyr Verbs"

# Basic Joins ##################################################################

## Obtain the data sets ========================================================
library(dplyr)
band_members
band_instruments
band_instruments2             ## The same as band_instruments, but name = artist

### inner_join:  ===============================================================
### Returns rows which match between the two tables
### Basically an intersection join
### We only get rows back which have a match in both tables

inner_join(band_members,      ## R automatically did this using 
           band_instruments)  ## the common column (name)

inner_join(band_members,      ## This is a clearer version of above
           band_instruments, 
           join_by(name))

inner_join(band_members, 
           band_instruments2, 
           join_by(name == artist))

inner_join(band_members,      ## Final, most explicit form
           band_instruments, 
           join_by(name == name))

### full_join  ===============================================================
### (or outer join) is a union join
### We get back all rows from both tables, regardless of a match or not

full_join(band_members,       ## R fills in the "blanks" with NA values
          band_instruments, 
          join_by(name == name))

### left_join ==================================================================
### Keeps all rows from one table, whether or not they have a match

left_join(band_members,       ## Mick is kept bc he's in band_members
          band_instruments,   ## Keith is dropped bc he isn't
          join_by(name == name))

### right_join  ================================================================
### Just a "flipped" left_join
### left_join(x, y) == right_join(y, x)

right_join(band_members,      ## Keith is kept bc he's in band_instruments
          band_instruments,   ## Mick is dropped bc he isn't
          join_by(name == name))

### anti_join ==================================================================
### Returns elements that appear in one data set, but not the other
### Rarer, but occasionally useful

# Joins with Repeats ###########################################################

## Hypothetical scenario =======================================================

students <- tribble(
  ~name, ~email, ~id,
  "Bernard",  "bernard@cuny.edu",  1,
  "Hunter",   "hunter@cuny.edu",   2,
  "John Jay", "john.jay@cuny.edu", 3
)

grades <- tribble(
  ~student_id, ~assignment_id, ~grade,
  1,           "A",            100,
  2,           "A",            95,
  3,           "A",            80,
  1,           "B",            95,
  2,           "B",            80,
  3,           "B",            50,
  1,           "C",            95,
  2,           "C",            50,
  3,           "C",            80
)

inner_join(students, 
           grades, 
           join_by(id == student_id))

inner_join(students, 
           grades, 
           join_by(id == student_id)) |>
  group_by(name, email, id) |>
  summarize(final_avg = mean(grade)) |>
  mutate(final_grade = 
           case_when(final_avg > 90 ~ "A", 
                     final_avg > 80 ~ "B", 
                     final_avg > 70 ~ "C", 
                     final_avg > 60 ~ "D", 
                     TRUE ~ "F")) # In a case_when, TRUE == "else"
