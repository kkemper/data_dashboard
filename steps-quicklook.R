library(tidyverse)

# Step data for ten consecutive days starting with 2019-09-08

steps <- tibble(
  date = as.Date(x=seq.Date(as.Date("2019-08-09"), as.Date("2019-08-18"), "days")),
  fitbit = c(6476, 6853, 6181, 6607, 11023, 8115, 11992, 7162, 3505, 3619),
  misfit = c(4868, 6226, 5922, 5598, 9200, 6844, 11322, 5844, 3380, 2806),
  apple_watch = c(3820, 5350, 4019, 5042, 10060, 6059, 12229, 6296, 1980, 2292),
  spire = c(3594, 4773, 3154, NA, 6389, 2497, 8370, 3922, NA, 976),
  lumo_lift = c(4202, 4710, 3139, 3322, 6911, 3823, 8789, 4527, NA, 866)
)

# Add mean and standard deviation columns to the table.

steps %>%
  mutate(mean = apply(.[(2:6)],1,mean, na.rm = TRUE)) %>%
  mutate(standard_deviation=apply(.[(2:6)],1,sd, na.rm = TRUE))

# Create line plots of the data
ggplot(data  = steps) +
  geom_line(mapping = aes(x = date, y = fitbit), color = "red") +
  geom_line(mapping = aes(x = date, y = misfit), color = "orange") + 
  geom_line(mapping = aes(x = date, y = apple_watch), color = "yellow") + 
  geom_line(mapping = aes(x = date, y = spire), color = "green", na.rm = TRUE, show.legend = TRUE) +
  geom_line(mapping = aes(x = date, y = lumo_lift), color = "blue", na.rm = TRUE, show.legend = TRUE)

# Add labels to the chart



