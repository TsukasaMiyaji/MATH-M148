# libraries and directory set up
setwd("C:/Users/lavil/source/repos/LukVill/MATH-M148")

library(tidyverse)
library(lubridate)
library(hms)
library(arules)
library(arulesViz)
library(data.table)

# event definition set up
event_def <- "Event Definitions.csv"

# dataset set up
luke_export_dir <- 'C:/Users/lavil/source/repos/LukVill/Misc Data/export_no_dup.csv'

# this is based off of Luke's directory!!! #
export_head <- fread(luke_export_dir, nrows = 1000)
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
#export_head <- export_head %>% distinct(customer_id,account_id,event_timestamp,date,hours,mins,secs, .keep_all = T)
#print(export_head, n = 100)
#print(export_head %>% filter(customer_id == 15849251), n = 100)
# print(export_head %>% distinct(customer_id,account_id,date,hours,mins,secs, .keep_all = T), n = 100)
# print(export_head %>% distinct(customer_id,account_id,date,time), n = 100)
# CLEANED UP DUPLICATES
# EDIT: USED PYTHON TO CLEAN UP DUPLICATES




# INITIAL DATA VISUALIZATIONS
# make time series of growth of events, colored by different events

# vector of all days from earliest to latest (11-03-20 to 09-20-23)
date_vector <- seq(as.Date("2020-11-03"),as.Date("2023-09-20"), by = "days") %>% 
data.frame() %>% rename("dates" = ".")
#date_vector %>% colnames()
  
# count ed per day, pivot wider to get hot-esque notation, right join into date_vector
# mutate each column to replace na's, mutate each col to cumsum 
export_head %>% group_by(date,ed_id) %>% count() %>% 
pivot_wider(names_from = ed_id, values_from = n, values_fill = 0) %>% 
right_join(date_vector, by = c("date" = "dates")) %>% arrange(date) %>%
mutate(across(everything(), ~replace_na(.x, 0))) %>% mutate(across(everything(), ~cumsum(.x)))

help(across)
indv_df <- export_head %>% group_by(date,ed_id) %>% count() %>% ungroup() %>% group_by(ed_id) %>%
mutate(total = cumsum(n)) %>% select(date,ed_id,total) #%>% arrange(date) %>% filter(ed_id == 15) %>% 
indv_graph <- indv_df %>% ggplot(aes(x = date, y = total, color = ed_id)) + 
geom_line() + theme_minimal() + scale_color_distiller(palette = "Set1")
indv_graph
indv_df %>% filter(ed_id == 1)

# GOAL 1: optimizing purchasing products, people on journeys sometimes do not purchase products
# what triggers customers to purchase products -- which ordered set of events tend to result in product purchasing
# 

# GOAL 2: optimizing starting journeys
# journeys that have 1 step
apporved_cust <- export_head %>% group_by(customer_id, account_id) %>% filter(sum(journey_steps_until_end) == 1)
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


