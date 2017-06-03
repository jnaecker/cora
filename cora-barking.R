library(googlesheets)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggalt)
library(hrbrthemes)
library(stringr)
library(purrr)

cora <- gs_ls() %>%
  filter(str_detect(sheet_title, "243c201f9125 input received")) %>%
  collect() %>% 
  .[["sheet_title"]] %>%
  map(gs_title) %>%
  map(~gs_read(., col_names = c("Time", "Device", "Level", "Test"))) %>%
  reduce(rbind) %>%
  mutate(
    Date = as.Date(Time, format = "%B %d, %Y at %I:%M%p"),
    DateTime = as.POSIXct(strptime(Time, format = "%B %d, %Y at %I:%M%p"))
  )

cora %>%
  mutate(hour = hour(DateTime), day = date(DateTime)) %>%
  group_by(hour, day) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = hour, y = count)) + 
    geom_lollipop() + 
    scale_y_log10() +
    scale_x_continuous(breaks = seq(0, 24, 1)) +
    theme_ipsum() +
    facet_wrap(~ day, ncol = 3) + 
    labs(x = "Hour", y = "Number of Barks")
