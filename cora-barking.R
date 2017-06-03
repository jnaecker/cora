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
  reduce(rbind)

cora$Date <- as.Date(cora$Time, format = "%B %d, %Y at %I:%M%p")
cora$DateTime <- as.POSIXct(strptime(cora$Time, format = "%B %d, %Y at %I:%M%p"))

minutes <- cora %>%
  filter(DateTime >= "2017-04-24 08:00:00") %>%
  select(DateTime) %>%
  group_by(DateTime) %>%
  summarize(count = n())

ggplot(minutes, aes(x=DateTime, y=count)) + 
  geom_lollipop() + 
  scale_y_continuous(breaks = seq(0,20,2)) +
  scale_x_datetime(date_breaks = "1 hour", date_labels="%I") +
  theme_ipsum() +
  labs(x = "Time", y = "Number of Barks")

