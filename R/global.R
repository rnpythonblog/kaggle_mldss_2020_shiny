# global.R

library(bslib)
library(thematic)

library(ggplot2)
library(dplyr, quietly = T)
library(tibble)
library(tidyr)
library(stringr)
library(forcats)
library(patchwork)
library(waiter)
library(scales)

library(shinyWidgets)

## Center Title ##
theme_update(plot.title = element_text(hjust = 0.5,
                                       colour = '#70A0BE',
                                       family = 'Helvetica',
                                       margin=margin(0,0,24,0)),
             plot.margin = margin(1, 1, 1, 1, "cm"))

DATA_DIR<-"data"
mldss_df<-read.csv(file.path(DATA_DIR,"kmldss2020.csv"))
mldss_2019_df<-read.csv(file.path(DATA_DIR,"kmldss2019.csv"))
mldss_2018_df<-read.csv(file.path(DATA_DIR,"kmldss2018.csv"))

#### Remove the question text
mldss_df<-mldss_df[-1,]
mldss_2019_df<-mldss_2019_df[-1,]
mldss_2018_df<-mldss_2018_df[-1,]

num_worldwide_respondents<-mldss_df %>% nrow(.)

top_10_df<-mldss_df %>%
  count(Q3, name = 'num_resp') %>%
  mutate(pct_resp = round(100*num_resp/sum(num_resp),1)) %>%
  filter(Q3 != "Other") %>%
  slice_max(pct_resp,n=10)

top_10_countries<-top_10_df$Q3
