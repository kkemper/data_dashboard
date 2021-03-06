library(tidyverse)
# Data from the last week

## Sleep

sleep <-  tribble(
  ~date   , ~fitbit, ~misfit, ~autosleep,   ~"sleep++", ~sleepwatch,
  #_________|________|________|___________|_________|____________
  as.Date("2019-08-02"),     431,     463,        401,      537, 430,      
  as.Date("2019-08-03"),     432,     506,        425,      546, 470,
  as.Date("2019-08-04"),     582,     626,        566,      598, 640,
  as.Date("2019-08-05"),     574,     430,        568,      430, 535,
  as.Date("2019-08-06"),      NA,     447,        424,      456, 470,
  as.Date("2019-08-07"),     472,     476,        463,      402, 420,
  as.Date("2019-08-08"),     627,      NA,        603,      470, 620,
  as.Date("2019-08-09"),     443,     487,        450,      426, 530
)
