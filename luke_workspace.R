# libraries and directory set up
setwd("C:/Users/lavil/source/repos/LukVill/MATH-M148")

library(tidyverse)
library(lubridate)
library(hms)
library(arules)
install.packages("arulesViz")


# event definition set up
event_def <- "Event Definitions.csv"

# dataset set up
luke_export_dir <- 'C:/Users/lavil/source/repos/LukVill/Misc Data/export.csv'

# this is based off of Luke's directory!!! #
export_head <- fread(luke_export_dir, n_max = 1000)
# this is based off of Luke's directory!!! #
export <- fread(luke_export_dir)


# INITIAL DATA CLEANUP
# filter out duplicate events on the same event timestamp (based off of same event stamp and customer and account)
# mutate to get date and time and seconds
export_head <- export_head %>% mutate(date = date(event_timestamp),
                                      hours = hour(event_timestamp),
                                      mins = minute(event_timestamp),
                                      secs = trunc(second(event_timestamp)))

# based on date, time, customer, and account, filter distinct
export_head <- export_head %>% 
  distinct(customer_id,account_id,event_timestamp,date,hours,mins,secs, .keep_all = T)
print(export_head, n = 100)
# print(export_head %>% filter(customer_id == 15849251), n = 100)
# print(export_head %>% distinct(customer_id,account_id,date,hours,mins,secs, .keep_all = T), n = 100)
# print(export_head %>% distinct(customer_id,account_id,date,time), n = 100)
# CLEANED UP DUPLICATES

# GOAL 1: optimizing purchasing products, people on journeys sometimes do not purchase products
# what triggers customers to purchase products -- which ordered set of events tend to result in product purchasing
# 

# GOAL 2: optimizing starting journeys
# journeys that have 1 step
apporved_cust <- export %>% group_by(customer_id, account_id) %>% filter(sum(journey_steps_until_end) == 1)
export_head %>% filter(customer_id == 1483781929)
apporved_cust %>% group_by(ed_id) %>% count()
# most did not do anything, so look into the next step (i.e. journey steps = 2)


# GOAL 3: optimize the speed of the journeys (most popular and quickest)

# CLICK RATE ANALYSIS
# click amount on different events

# TIME SPENT ON EVENTS
# time spent criteria: if at least TWO EVENTS ARE CONSECUTIVE, same name, same day
# then get difference of time of last elem and first elem
# only work with cust, acct, event name, date, times(hms)

# 
unique(event_def$stage)
event_def %>% filter(stage == "Apply for Credit")


