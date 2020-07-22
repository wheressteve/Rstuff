######## Connections ########



library(lubridate)
library(dbplyr)
library(RPostgres)
library(tidyverse)
library(janitor)
library(formattable)
library(hrbrthemes)
library(RColorBrewer)
library(googlesheets4)
con <- dbConnect(RPostgres::Postgres(), 
                 host = "ec2-18-204-65-55.compute-1.amazonaws.com",
                 user = "u8nd18725rvush",
                 password="p9218622f7680ef1551701f34b1f5ca930b23672ae0b82155623c648fdaccbe85",
                 dbname="d87emubrgnls11",
                 port = 5432
)
users_tbl <- tbl(con, "users")
members_tbl <- tbl(con, "organization_members")
tasks_tbl <- tbl(con, "tasks")
project_tbl <- tbl(con,"projects")
task_log_tbl <- tbl(con,"task_logs")
attach_tbl <- tbl(con,"attachments")
hooks_tbl <- tbl(con,"webhooks")
orgs_tbl <- tbl(con,"organizations")
collab_tbl <- tbl(con,"collaborators")
org_exp_tbl <- tbl(con, "organization_experiments")




###### Functions #######
price_fun <- function(data, var = subscription_price){
  var <- enquo(var)
  data %>% 
    mutate(!!var := case_when(
      !!var == 290 ~ 39,
      !!var == 390 ~ 39,
      !!var == 49 ~ 59,
      !!var == 590 ~ 59,
      !!var > 609 ~ !!var / 10,
      TRUE ~ !!var)) %>%
    mutate(plan_type = case_when(
      !!var >= 99 ~ "high",
      !!var <99 ~"low",
      TRUE ~ "NA"))
}

fill_user <- function(data,group_var,...) {
  var <- quos(...)
  group_var <- enquo(group_var)
  data %>%
    group_by(!!group_var) %>% 
    fill(!!!var) %>%
    fill(!!!var, .direction =  "up")
}

recode_jobs <- function(data, var = finally_what_is_you_job_title_as_it_might_be_written_on_linked_in) {
  var <- enquo(var)
  data %>%  
    mutate(user_type = case_when(
      str_detect(!!var,"dev." ) ~ "devs",
      str_detect(!!var, "eng.") ~ "devs",
      str_detect(!!var, "tech.") ~ "devs",
      str_detect(!!var, ".programmer.") ~ "devs",
      str_detect(!!var,"web dev") ~ "devs",
      is.na(!!var) ~ "unknown",
      TRUE ~ "non-devs"))
}

opp_score_fun <- function(data, var = user_type) {
  var <- enquo(var)
  data %>%
    select( !!var,contains("task")) %>% 
    gather(question, response, 2:7) %>%
    mutate(dimension = if_else(str_detect(question, "opp"), "importance", "satisfaction")) %>%
    mutate(task = case_when( 
      str_detect(question, "1") ~ "getting feeback", 
      str_detect(question, "2") ~ "managing tasks",
      str_detect(question, "3") ~ "working with clients"
    )) %>%
    select(-question) %>% 
    group_by(!!var, task, dimension) %>% 
    summarise( score = sum((response >= 4 )/ n() * 10)) %>%
    spread(dimension, score) %>% 
    mutate(opp_score = importance + (importance - satisfaction))
}


