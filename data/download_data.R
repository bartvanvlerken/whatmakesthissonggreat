#' download_data.R
#'
#' contributors: Bart van Vlerken
#'
#' File downloads the data used in your analysis from online.
#' This way you, and the instructors can access your data, 
#' and help you as needed.
#' 
#' We recommend uploading the data to Google Drive and using 
#' this script as a set of steps to download the data to an
#' individual computer to be analyzed.
#'

# Libraries
library(gtrendsR)
library(tidyverse)
library(googledrive)

# Below I manually gather the data required for my project, since no database was readily available with the data that I needed.
# Using the gtrends function of the 'gtrendsR' package, I download Google Trends data of 103 pairs of songs. In the gtrends()
# formula I specify the two songs, the time period (one week around the release of an episode of WMTSG), the search category
# (YouTube) and whether or not to include low search volume (yes, since all data is valuable). The formula then gathers data
# from search queries worldwide. I then filter out the variables I don't need and assign whether a song belongs to the control
# group or the treatment group.

# Next, I create the dataset out of the 103 pairs. After this, I remove the pairs from the global environment to maintain a 
# clean workspace. The benefit of this manual process is that the data is already clean. I export the data to Google Drive.

# Since Google Trends data is somewhat inconsistent, the results from the subsequent analyses will be a bit different. Therefore,
# feel free to inspect/run the code below, however the data in the output folder will come from my 'original' code.

# LOADING THE DATA ==================================================================================
pair_1 <- gtrends(keyword = c("blink 182 all the small things", "stone temple pilots plush"),
                  time = "2018-01-17 2018-01-31",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_1 <- pair_1$interest_over_time
pair_1 <- pivot_wider(data = pair_1,
                      names_from = date,
                      values_from = hits)
pair_1 <- select(pair_1, -c(geo, time, gprop, category))
names(pair_1) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_1$pair <- c(1, 1)
pair_1$group <- c('treatment', 'control')

pair_2 <- gtrends(keyword = c("the police every little thing she does is magic", "foo fighters my hero"),
                  time = "2018-01-19 2018-02-02",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_2 <- pair_2$interest_over_time
pair_2 <- pivot_wider(data = pair_2,
                      names_from = date,
                      values_from = hits)
pair_2 <- select(pair_2, -c(geo, time, gprop, category))
names(pair_2) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_2$pair <- c(2, 2)
pair_2$group <- c('treatment', 'control')

pair_3 <- gtrends(keyword = c("steely dan kid charlemagne", "the smashing pumpkins bullet with butterfly wings"),
                  time = "2018-01-25 2018-02-08",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_3 <- pair_3$interest_over_time
pair_3 <- pivot_wider(data = pair_3,
                      names_from = date,
                      values_from = hits)
pair_3 <- select(pair_3, -c(geo, time, gprop, category))
names(pair_3) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_3$pair <- c(3, 3)
pair_3$group <- c('treatment', 'control')

pair_4 <- gtrends(keyword = c("nirvana on a plain", "guns n roses live and let die"),
                  time = "2018-01-28 2018-02-11",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_4 <- pair_4$interest_over_time
pair_4 <- pivot_wider(data = pair_4,
                      names_from = date,
                      values_from = hits)
pair_4 <- select(pair_4, -c(geo, time, gprop, category))
names(pair_4) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_4$pair <- c(4, 4)
pair_4$group <- c('treatment', 'control')

pair_5 <- gtrends(keyword = c("pearl jam jeremy", "red hot chili peppers higher ground"),
                  time = "2018-01-30 2018-02-13",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_5 <- pair_5$interest_over_time
pair_5 <- pivot_wider(data = pair_5,
                      names_from = date,
                      values_from = hits)
pair_5 <- select(pair_5, -c(geo, time, gprop, category))
names(pair_5) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_5$pair <- c(5, 5)
pair_5$group <- c('treatment', 'control')

pair_6 <- gtrends(keyword = c("linkin park numb", "rem orange crush"),
                  time = "2018-02-01 2018-02-15",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_6 <- pair_6$interest_over_time
pair_6 <- pivot_wider(data = pair_6,
                      names_from = date,
                      values_from = hits)
pair_6 <- select(pair_6, -c(geo, time, gprop, category))
names(pair_6) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_6$pair <- c(6, 6)
pair_6$group <- c('treatment', 'control')

pair_7 <- gtrends(keyword = c("tom petty i wont back down", "alice in chains would"),
                  time = "2018-02-02 2018-02-16",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_7 <- pair_7$interest_over_time
pair_7 <- pivot_wider(data = pair_7,
                      names_from = date,
                      values_from = hits)
pair_7 <- select(pair_7, -c(geo, time, gprop, category))
names(pair_7) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_7$pair <- c(7, 7)
pair_7$group <- c('treatment', 'control')

pair_8 <- gtrends(keyword = c("a perfect circle judith", "rush tom sawyer"),
                  time = "2018-02-06 2018-02-20",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_8 <- pair_8$interest_over_time
pair_8 <- pivot_wider(data = pair_8,
                      names_from = date,
                      values_from = hits)
pair_8 <- select(pair_8, -c(geo, time, gprop, category))
names(pair_8) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_8$pair <- c(8, 8)
pair_8$group <- c('treatment', 'control')

pair_9 <- gtrends(keyword = c("toto rosanna", "lynyrd skynyrd free bird"),
                  time = "2018-02-09 2018-02-23",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_9 <- pair_9$interest_over_time
pair_9 <- pivot_wider(data = pair_9,
                      names_from = date,
                      values_from = hits)
pair_9 <- select(pair_9, -c(geo, time, gprop, category))
names(pair_9) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_9$pair <- c(9, 9)
pair_9$group <- c('treatment', 'control')

pair_10 <- gtrends(keyword = c("soundgarden spoonman", "blue oyster cult dont fear the reaper"),
                  time = "2018-02-11 2018-02-25",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_10 <- pair_10$interest_over_time
pair_10 <- pivot_wider(data = pair_10,
                      names_from = date,
                      values_from = hits)
pair_10 <- select(pair_10, -c(geo, time, gprop, category))
names(pair_10) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_10$pair <- c(10, 10)
pair_10$group <- c('treatment', 'control')

pair_11 <- gtrends(keyword = c("metallica enter sandman", "rem its the end of the world as we know it"),
                  time = "2018-02-17 2018-03-03",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_11 <- pair_11$interest_over_time
pair_11 <- pivot_wider(data = pair_11,
                      names_from = date,
                      values_from = hits)
pair_11 <- select(pair_11, -c(geo, time, gprop, category))
names(pair_11) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_11$pair <- c(11, 11)
pair_11$group <- c('treatment', 'control')

pair_12 <- gtrends(keyword = c("fleetwood mac go your own way", "ramones i wanna be sedated"),
                  time = "2019-02-23 2019-03-09",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_12 <- pair_12$interest_over_time
pair_12 <- pivot_wider(data = pair_12,
                      names_from = date,
                      values_from = hits)
pair_12 <- select(pair_12, -c(geo, time, gprop, category))
names(pair_12) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_12$pair <- c(12, 12)
pair_12$group <- c('treatment', 'control')

pair_13 <- gtrends(keyword = c("alice in chains them bones", "blind melon no rain"),
                  time = "2018-02-22 2018-03-08",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_13 <- pair_13$interest_over_time
pair_13 <- pivot_wider(data = pair_13,
                      names_from = date,
                      values_from = hits)
pair_13 <- select(pair_13, -c(geo, time, gprop, category))
names(pair_13) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_13$pair <- c(13, 13)
pair_13$group <- c('treatment', 'control')

pair_14 <- gtrends(keyword = c("rage against the machine killing in the name", "velvet revolver slither"),
                  time = "2018-02-28 2018-03-14",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_14 <- pair_14$interest_over_time
pair_14 <- pivot_wider(data = pair_14,
                      names_from = date,
                      values_from = hits)
pair_14 <- select(pair_14, -c(geo, time, gprop, category))
names(pair_14) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_14$pair <- c(14, 14)
pair_14$group <- c('treatment', 'control')

pair_15 <- gtrends(keyword = c("tool schism", "led zeppelin kashmir"),
                  time = "2018-03-08 2018-03-22",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_15 <- pair_15$interest_over_time
pair_15 <- pivot_wider(data = pair_15,
                      names_from = date,
                      values_from = hits)
pair_15 <- select(pair_15, -c(geo, time, gprop, category))
names(pair_15) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_15$pair <- c(15, 15)
pair_15$group <- c('treatment', 'control')

pair_16 <- gtrends(keyword = c("foo fighters everlong", "green day when i come around"),
                  time = "2018-03-12 2018-03-26",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_16 <- pair_16$interest_over_time
pair_16 <- pivot_wider(data = pair_16,
                      names_from = date,
                      values_from = hits)
pair_16 <- select(pair_16, -c(geo, time, gprop, category))
names(pair_16) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_16$pair <- c(16, 16)
pair_16$group <- c('treatment', 'control')

pair_17 <- gtrends(keyword = c("boston hitch a ride", "van halen you really got me"),
                  time = "2018-03-16 2018-03-30",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_17 <- pair_17$interest_over_time
pair_17 <- pivot_wider(data = pair_17,
                      names_from = date,
                      values_from = hits)
pair_17 <- select(pair_17, -c(geo, time, gprop, category))
names(pair_17) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_17$pair <- c(17, 17)
pair_17$group <- c('treatment', 'control')

pair_18 <- gtrends(keyword = c("ariana grande into you", "audioslave doesnt remind me"),
                  time = "2018-03-18 2018-04-01",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_18 <- pair_18$interest_over_time
pair_18 <- pivot_wider(data = pair_18,
                      names_from = date,
                      values_from = hits)
pair_18 <- select(pair_18, -c(geo, time, gprop, category))
names(pair_18) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_18$pair <- c(18, 18)
pair_18$group <- c('treatment', 'control')

pair_19 <- gtrends(keyword = c("rush closer to the heart", "velvet revolver fall to pieces"),
                  time = "2018-03-21 2018-04-04",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_19 <- pair_19$interest_over_time
pair_19 <- pivot_wider(data = pair_19,
                      names_from = date,
                      values_from = hits)
pair_19 <- select(pair_19, -c(geo, time, gprop, category))
names(pair_19) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_19$pair <- c(19, 19)
pair_19$group <- c('treatment', 'control')

pair_20 <- gtrends(keyword = c("queens of the stone age no one knows", "temple of the dog hunger strike"),
                  time = "2019-02-20 2019-03-06",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_20 <- pair_20$interest_over_time
pair_20 <- pivot_wider(data = pair_20,
                      names_from = date,
                      values_from = hits)
pair_20 <- select(pair_20, -c(geo, time, gprop, category))
names(pair_20) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_20$pair <- c(20, 20)
pair_20$group <- c('treatment', 'control')

pair_21 <- gtrends(keyword = c("stone temple pilots vasoline", "the who baba oriley"),
                  time = "2018-04-01 2018-04-15",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_21 <- pair_21$interest_over_time
pair_21 <- pivot_wider(data = pair_21,
                      names_from = date,
                      values_from = hits)
pair_21 <- select(pair_21, -c(geo, time, gprop, category))
names(pair_21) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_21$pair <- c(21, 21)
pair_21$group <- c('treatment', 'control')

pair_22 <- gtrends(keyword = c("van halen runnin with the devil", "guns n roses civil war"),
                  time = "2018-04-09 2018-04-23",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_22 <- pair_22$interest_over_time
pair_22 <- pivot_wider(data = pair_22,
                      names_from = date,
                      values_from = hits)
pair_22 <- select(pair_22, -c(geo, time, gprop, category))
names(pair_22) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_22$pair <- c(22, 22)
pair_22$group <- c('treatment', 'control')

pair_23 <- gtrends(keyword = c("the smashing pumpkins 1979", "tom petty runnin down a dream"),
                  time = "2018-04-16 2018-04-30",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_23 <- pair_23$interest_over_time
pair_23 <- pivot_wider(data = pair_23,
                      names_from = date,
                      values_from = hits)
pair_23 <- select(pair_23, -c(geo, time, gprop, category))
names(pair_23) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_23$pair <- c(23, 23)
pair_23$group <- c('treatment', 'control')

pair_24 <- gtrends(keyword = c("train drops of jupiter", "green day longview"),
                  time = "2018-04-25 2018-05-09",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_24 <- pair_24$interest_over_time
pair_24 <- pivot_wider(data = pair_24,
                      names_from = date,
                      values_from = hits)
pair_24 <- select(pair_24, -c(geo, time, gprop, category))
names(pair_24) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_24$pair <- c(24, 24)
pair_24$group <- c('treatment', 'control')

pair_25 <- gtrends(keyword = c("system of a down chop suey", "living colour cult of personality"),
                  time = "2018-04-27 2018-05-11",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_25 <- pair_25$interest_over_time
pair_25 <- pivot_wider(data = pair_25,
                      names_from = date,
                      values_from = hits)
pair_25 <- select(pair_25, -c(geo, time, gprop, category))
names(pair_25) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_25$pair <- c(25, 25)
pair_25$group <- c('treatment', 'control')

pair_26 <- gtrends(keyword = c("red hot chili peppers give it away", "jane's addiction mountain song"),
                  time = "2018-05-01 2018-05-15",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_26 <- pair_26$interest_over_time
pair_26 <- pivot_wider(data = pair_26,
                      names_from = date,
                      values_from = hits)
pair_26 <- select(pair_26, -c(geo, time, gprop, category))
names(pair_26) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_26$pair <- c(26, 26)
pair_26$group <- c('treatment', 'control')

pair_27 <- gtrends(keyword = c("peter gabriel in your eyes", "derek and the dominos layla"),
                  time = "2018-05-08 2018-05-22",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_27 <- pair_27$interest_over_time
pair_27 <- pivot_wider(data = pair_27,
                      names_from = date,
                      values_from = hits)
pair_27 <- select(pair_27, -c(geo, time, gprop, category))
names(pair_27) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_27$pair <- c(27, 27)
pair_27$group <- c('treatment', 'control')

pair_28 <- gtrends(keyword = c("david bowie lets dance", "the smashing pumpkins tonight tonight"),
                  time = "2018-05-13 2018-05-27",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_28 <- pair_28$interest_over_time
pair_28 <- pivot_wider(data = pair_28,
                      names_from = date,
                      values_from = hits)
pair_28 <- select(pair_28, -c(geo, time, gprop, category))
names(pair_28) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_28$pair <- c(28, 28)
pair_28$group <- c('treatment', 'control')

pair_29 <- gtrends(keyword = c("nine inch nails the hand that feeds", "the black crowes hard to handle"),
                  time = "2018-05-17 2018-05-31",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_29 <- pair_29$interest_over_time
pair_29 <- pivot_wider(data = pair_29,
                      names_from = date,
                      values_from = hits)
pair_29 <- select(pair_29, -c(geo, time, gprop, category))
names(pair_29) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_29$pair <- c(29, 29)
pair_29$group <- c('treatment', 'control')

pair_30 <- gtrends(keyword = c("alanis morissette you oughta know", "weezer buddy holly"),
                  time = "2018-05-19 2018-06-02",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_30 <- pair_30$interest_over_time
pair_30 <- pivot_wider(data = pair_30,
                      names_from = date,
                      values_from = hits)
pair_30 <- select(pair_30, -c(geo, time, gprop, category))
names(pair_30) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_30$pair <- c(30, 30)
pair_30$group <- c('treatment', 'control')

pair_31 <- gtrends(keyword = c("radiohead paranoid android", "the who the seeker"),
                  time = "2018-05-24 2018-06-07",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_31 <- pair_31$interest_over_time
pair_31 <- pivot_wider(data = pair_31,
                      names_from = date,
                      values_from = hits)
pair_31 <- select(pair_31, -c(geo, time, gprop, category))
names(pair_31) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_31$pair <- c(31, 31)
pair_31$group <- c('treatment', 'control')

pair_32 <- gtrends(keyword = c("coldplay clocks", "lenny kravitz are you gonna go my way"),
                  time = "2018-05-29 2018-06-12",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_32 <- pair_32$interest_over_time
pair_32 <- pivot_wider(data = pair_32,
                      names_from = date,
                      values_from = hits)
pair_32 <- select(pair_32, -c(geo, time, gprop, category))
names(pair_32) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_32$pair <- c(32, 32)
pair_32$group <- c('treatment', 'control')

pair_33 <- gtrends(keyword = c("the police every breath you take", "the police message in a bottle"),
                  time = "2018-06-02 2018-06-16",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_33 <- pair_33$interest_over_time
pair_33 <- pivot_wider(data = pair_33,
                      names_from = date,
                      values_from = hits)
pair_33 <- select(pair_33, -c(geo, time, gprop, category))
names(pair_33) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_33$pair <- c(33, 33)
pair_33$group <- c('treatment', 'control')

pair_34 <- gtrends(keyword = c("muse starlight", "janes addiction jane says"),
                  time = "2018-06-14 2018-06-28",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_34 <- pair_34$interest_over_time
pair_34 <- pivot_wider(data = pair_34,
                      names_from = date,
                      values_from = hits)
pair_34 <- select(pair_34, -c(geo, time, gprop, category))
names(pair_34) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_34$pair <- c(34, 34)
pair_34$group <- c('treatment', 'control')

pair_35 <- gtrends(keyword = c("steely dan dont take me alive", "the offspring come out and play"),
                  time = "2018-06-19 2018-07-03",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_35 <- pair_35$interest_over_time
pair_35 <- pivot_wider(data = pair_35,
                      names_from = date,
                      values_from = hits)
pair_35 <- select(pair_35, -c(geo, time, gprop, category))
names(pair_35) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_35$pair <- c(35, 35)
pair_35$group <- c('treatment', 'control')

pair_36 <- gtrends(keyword = c("yes roundabout", "cream sunshine of your love"),
                  time = "2018-07-07 2018-07-21",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_36 <- pair_36$interest_over_time
pair_36 <- pivot_wider(data = pair_36,
                      names_from = date,
                      values_from = hits)
pair_36 <- select(pair_36, -c(geo, time, gprop, category))
names(pair_36) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_36$pair <- c(36, 36)
pair_36$group <- c('treatment', 'control')

pair_37 <- gtrends(keyword = c("incubus pardon me", "eric johnson cliffs of dover"),
                  time = "2018-07-12 2018-07-26",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_37 <- pair_37$interest_over_time
pair_37 <- pivot_wider(data = pair_37,
                      names_from = date,
                      values_from = hits)
pair_37 <- select(pair_37, -c(geo, time, gprop, category))
names(pair_37) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_37$pair <- c(37, 37)
pair_37$group <- c('treatment', 'control')

pair_38 <- gtrends(keyword = c("oasis wonderwall", "beastie boys sabotage"),
                  time = "2018-07-25 2018-08-08",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_38 <- pair_38$interest_over_time
pair_38 <- pivot_wider(data = pair_38,
                      names_from = date,
                      values_from = hits)
pair_38 <- select(pair_38, -c(geo, time, gprop, category))
names(pair_38) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_38$pair <- c(38, 38)
pair_38$group <- c('treatment', 'control')

pair_39 <- gtrends(keyword = c("janes addiction been caught steeling", "aerosmith livin on the edge"),
                  time = "2018-08-03 2018-08-17",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_39 <- pair_39$interest_over_time
pair_39 <- pivot_wider(data = pair_39,
                      names_from = date,
                      values_from = hits)
pair_39 <- select(pair_39, -c(geo, time, gprop, category))
names(pair_39) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_39$pair <- c(39, 39)
pair_39$group <- c('treatment', 'control')

pair_40 <- gtrends(keyword = c("adele rumour has it", "grateful dead alabama getaway"),
                  time = "2018-08-07 2018-08-21",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_40 <- pair_40$interest_over_time
pair_40 <- pivot_wider(data = pair_40,
                      names_from = date,
                      values_from = hits)
pair_40 <- select(pair_40, -c(geo, time, gprop, category))
names(pair_40) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_40$pair <- c(40, 40)
pair_40$group <- c('treatment', 'control')

pair_41 <- gtrends(keyword = c("tool parabola", "faith no more falling to pieces"),
                  time = "2018-08-21 2018-09-04",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_41 <- pair_41$interest_over_time
pair_41 <- pivot_wider(data = pair_41,
                      names_from = date,
                      values_from = hits)
pair_41 <- select(pair_41, -c(geo, time, gprop, category))
names(pair_41) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_41$pair <- c(41, 41)
pair_41$group <- c('treatment', 'control')

pair_42 <- gtrends(keyword = c("simon garfunkel the sound of silence", "jimi hendrix all along the watchtower"),
                  time = "2018-08-24 2018-09-07",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_42 <- pair_42$interest_over_time
pair_42 <- pivot_wider(data = pair_42,
                      names_from = date,
                      values_from = hits)
pair_42 <- select(pair_42, -c(geo, time, gprop, category))
names(pair_42) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_42$pair <- c(42, 42)
pair_42$group <- c('treatment', 'control')

pair_43 <- gtrends(keyword = c("led zeppelin whole lotta love", "joe satriani surfing with the alien"),
                  time = "2018-09-03 2018-09-17",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_43 <- pair_43$interest_over_time
pair_43 <- pivot_wider(data = pair_43,
                      names_from = date,
                      values_from = hits)
pair_43 <- select(pair_43, -c(geo, time, gprop, category))
names(pair_43) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_43$pair <- c(43, 43)
pair_43$group <- c('treatment', 'control')

pair_44 <- gtrends(keyword = c("nirvana heart shaped box", "bad religion infected"),
                  time = "2018-09-07 2018-09-21",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_44 <- pair_44$interest_over_time
pair_44 <- pivot_wider(data = pair_44,
                      names_from = date,
                      values_from = hits)
pair_44 <- select(pair_44, -c(geo, time, gprop, category))
names(pair_44) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_44$pair <- c(44, 44)
pair_44$group <- c('treatment', 'control')

pair_45 <- gtrends(keyword = c("the chainsmokers closer", "soundgarden rusty cage"),
                  time = "2018-09-18 2018-10-02",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_45 <- pair_45$interest_over_time
pair_45 <- pivot_wider(data = pair_45,
                      names_from = date,
                      values_from = hits)
pair_45 <- select(pair_45, -c(geo, time, gprop, category))
names(pair_45) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_45$pair <- c(45, 45)
pair_45$group <- c('treatment', 'control')

pair_46 <- gtrends(keyword = c("periphery absolomb", "motorhead ace of spades"),
                  time = "2018-09-27 2018-10-11",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_46 <- pair_46$interest_over_time
pair_46 <- pivot_wider(data = pair_46,
                      names_from = date,
                      values_from = hits)
pair_46 <- select(pair_46, -c(geo, time, gprop, category))
names(pair_46) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_46$pair <- c(46, 46)
pair_46$group <- c('treatment', 'control')

pair_47 <- gtrends(keyword = c("pearl jam garden", "rage against the machine bulls on parade"),
                  time = "2018-10-05 2018-10-19",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_47 <- pair_47$interest_over_time
pair_47 <- pivot_wider(data = pair_47,
                      names_from = date,
                      values_from = hits)
pair_47 <- select(pair_47, -c(geo, time, gprop, category))
names(pair_47) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_47$pair <- c(47, 47)
pair_47$group <- c('treatment', 'control')

pair_48 <- gtrends(keyword = c("tears for fears head over heels", "aerosmith janie s got a gun"),
                  time = "2018-10-23 2018-11-06",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_48 <- pair_48$interest_over_time
pair_48 <- pivot_wider(data = pair_48,
                      names_from = date,
                      values_from = hits)
pair_48 <- select(pair_48, -c(geo, time, gprop, category))
names(pair_48) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_48$pair <- c(48, 48)
pair_48$group <- c('treatment', 'control')

pair_49 <- gtrends(keyword = c("mastodon stargasm", "pink floyd learning to fly"),
                  time = "2018-11-09 2018-11-23",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_49 <- pair_49$interest_over_time
pair_49 <- pivot_wider(data = pair_49,
                      names_from = date,
                      values_from = hits)
pair_49 <- select(pair_49, -c(geo, time, gprop, category))
names(pair_49) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_49$pair <- c(49, 49)
pair_49$group <- c('treatment', 'control')

pair_50 <- gtrends(keyword = c("tom petty free fallin", "metallica one"),
                  time = "2018-11-21 2018-12-05",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_50 <- pair_50$interest_over_time
pair_50 <- pivot_wider(data = pair_50,
                      names_from = date,
                      values_from = hits)
pair_50 <- select(pair_50, -c(geo, time, gprop, category))
names(pair_50) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_50$pair <- c(50, 50)
pair_50$group <- c('treatment', 'control')

pair_51 <- gtrends(keyword = c("the beatles the end", "the cult she sells sanctuary"),
                  time = "2018-11-30 2018-12-14",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_51 <- pair_51$interest_over_time
pair_51 <- pivot_wider(data = pair_51,
                      names_from = date,
                      values_from = hits)
pair_51 <- select(pair_51, -c(geo, time, gprop, category))
names(pair_51) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_51$pair <- c(51, 51)
pair_51$group <- c('treatment', 'control')

pair_52 <- gtrends(keyword = c("green day boulevard of broken dreams", "muse knights of cydonia"),
                  time = "2018-12-23 2019-01-06",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_52 <- pair_52$interest_over_time
pair_52 <- pivot_wider(data = pair_52,
                      names_from = date,
                      values_from = hits)
pair_52 <- select(pair_52, -c(geo, time, gprop, category))
names(pair_52) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_52$pair <- c(52, 52)
pair_52$group <- c('treatment', 'control')

pair_53 <- gtrends(keyword = c("faith no more epic", "the beatles helter skelter"),
                  time = "2019-01-09 2019-01-23",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_53 <- pair_53$interest_over_time
pair_53 <- pivot_wider(data = pair_53,
                      names_from = date,
                      values_from = hits)
pair_53 <- select(pair_53, -c(geo, time, gprop, category))
names(pair_53) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_53$pair <- c(53, 53)
pair_53$group <- c('treatment', 'control')

pair_54 <- gtrends(keyword = c("u2 in gods country", "wolfmother woman"),
                  time = "2019-01-24 2019-02-07",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_54 <- pair_54$interest_over_time
pair_54 <- pivot_wider(data = pair_54,
                      names_from = date,
                      values_from = hits)
pair_54 <- select(pair_54, -c(geo, time, gprop, category))
names(pair_54) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_54$pair <- c(54, 54)
pair_54$group <- c('treatment', 'control')

pair_55 <- gtrends(keyword = c("avenged sevenfold shepherd of fire", "stevie ray vaughan pride and joy"),
                  time = "2019-02-02 2019-02-16",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_55 <- pair_55$interest_over_time
pair_55 <- pivot_wider(data = pair_55,
                      names_from = date,
                      values_from = hits)
pair_55 <- select(pair_55, -c(geo, time, gprop, category))
names(pair_55) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_55$pair <- c(55, 55)
pair_55$group <- c('treatment', 'control')

pair_56 <- gtrends(keyword = c("alice in chains man in the box", "the offspring self esteem"),
                  time = "2019-02-14 2019-02-28",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_56 <- pair_56$interest_over_time
pair_56 <- pivot_wider(data = pair_56,
                      names_from = date,
                      values_from = hits)
pair_56 <- select(pair_56, -c(geo, time, gprop, category))
names(pair_56) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_56$pair <- c(56, 56)
pair_56$group <- c('treatment', 'control')

pair_57 <- gtrends(keyword = c("genesis dance on a volcano", "guns n roses paradise city"),
                  time = "2019-03-05 2019-03-19",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_57 <- pair_57$interest_over_time
pair_57 <- pivot_wider(data = pair_57,
                      names_from = date,
                      values_from = hits)
pair_57 <- select(pair_57, -c(geo, time, gprop, category))
names(pair_57) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_57$pair <- c(57, 57)
pair_57$group <- c('treatment', 'control')

pair_58 <- gtrends(keyword = c("the doors touch me", "allman brothers band jessica"),
                  time = "2019-03-19 2019-04-02",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_58 <- pair_58$interest_over_time
pair_58 <- pivot_wider(data = pair_58,
                      names_from = date,
                      values_from = hits)
pair_58 <- select(pair_58, -c(geo, time, gprop, category))
names(pair_58) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_58$pair <- c(58, 58)
pair_58$group <- c('treatment', 'control')

pair_59 <- gtrends(keyword = c("weezer hash pipe", "steely dan bodhisattva"),
                  time = "2019-04-03 2019-04-17",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_59 <- pair_59$interest_over_time
pair_59 <- pivot_wider(data = pair_59,
                      names_from = date,
                      values_from = hits)
pair_59 <- select(pair_59, -c(geo, time, gprop, category))
names(pair_59) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_59$pair <- c(59, 59)
pair_59$group <- c('treatment', 'control')

pair_60 <- gtrends(keyword = c("at the drive in one armed scissor", "beastie boys fight for your right"),
                  time = "2019-04-17 2019-05-01",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_60 <- pair_60$interest_over_time
pair_60 <- pivot_wider(data = pair_60,
                      names_from = date,
                      values_from = hits)
pair_60 <- select(pair_60, -c(geo, time, gprop, category))
names(pair_60) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_60$pair <- c(60, 60)
pair_60$group <- c('treatment', 'control')

pair_61 <- gtrends(keyword = c("van halen jump", "the rolling stones sympathy for the devil"),
                  time = "2019-04-22 2019-05-06",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_61 <- pair_61$interest_over_time
pair_61 <- pivot_wider(data = pair_61,
                      names_from = date,
                      values_from = hits)
pair_61 <- select(pair_61, -c(geo, time, gprop, category))
names(pair_61) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_61$pair <- c(61, 61)
pair_61$group <- c('treatment', 'control')

pair_62 <- gtrends(keyword = c("soundgarden outshined", "jimi hendrix purple haze"),
                  time = "2019-04-26 2019-05-10",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_62 <- pair_62$interest_over_time
pair_62 <- pivot_wider(data = pair_62,
                      names_from = date,
                      values_from = hits)
pair_62 <- select(pair_62, -c(geo, time, gprop, category))
names(pair_62) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_62$pair <- c(62, 62)
pair_62$group <- c('treatment', 'control')

pair_63 <- gtrends(keyword = c("rush limelight", "rage against the machine sleep now in the fire"),
                  time = "2019-04-27 2019-05-11",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_63 <- pair_63$interest_over_time
pair_63 <- pivot_wider(data = pair_63,
                      names_from = date,
                      values_from = hits)
pair_63 <- select(pair_63, -c(geo, time, gprop, category))
names(pair_63) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_63$pair <- c(63, 63)
pair_63$group <- c('treatment', 'control')

pair_64 <- gtrends(keyword = c("failure stuck on you", "alice cooper schools out"),
                  time = "2019-05-12 2019-05-26",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_64 <- pair_64$interest_over_time
pair_64 <- pivot_wider(data = pair_64,
                      names_from = date,
                      values_from = hits)
pair_64 <- select(pair_64, -c(geo, time, gprop, category))
names(pair_64) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_64$pair <- c(64, 64)
pair_64$group <- c('treatment', 'control')

pair_65 <- gtrends(keyword = c("stone temple pilots interstate love song", "queen tie your mother down"),
                  time = "2019-05-17 2019-05-31",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_65 <- pair_65$interest_over_time
pair_65 <- pivot_wider(data = pair_65,
                      names_from = date,
                      values_from = hits)
pair_65 <- select(pair_65, -c(geo, time, gprop, category))
names(pair_65) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_65$pair <- c(65, 65)
pair_65$group <- c('treatment', 'control')

pair_66 <- gtrends(keyword = c("metallica master of puppets", "queens of the stone age go with the flow"),
                  time = "2019-05-27 2019-06-10",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_66 <- pair_66$interest_over_time
pair_66 <- pivot_wider(data = pair_66,
                      names_from = date,
                      values_from = hits)
pair_66 <- select(pair_66, -c(geo, time, gprop, category))
names(pair_66) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_66$pair <- c(66, 66)
pair_66$group <- c('treatment', 'control')

pair_67 <- gtrends(keyword = c("megadeth symphony of destruction", "u2 sunday bloody sunday"),
                  time = "2019-05-28 2019-06-11",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_67 <- pair_67$interest_over_time
pair_67 <- pivot_wider(data = pair_67,
                      names_from = date,
                      values_from = hits)
pair_67 <- select(pair_67, -c(geo, time, gprop, category))
names(pair_67) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_67$pair <- c(67, 67)
pair_67$group <- c('treatment', 'control')

pair_68 <- gtrends(keyword = c("slipknot duality", "billy idol white wedding"),
                  time = "2019-06-03 2019-06-17",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_68 <- pair_68$interest_over_time
pair_68 <- pivot_wider(data = pair_68,
                      names_from = date,
                      values_from = hits)
pair_68 <- select(pair_68, -c(geo, time, gprop, category))
names(pair_68) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_68$pair <- c(68, 68)
pair_68$group <- c('treatment', 'control')

pair_69 <- gtrends(keyword = c("journey dont stop believin", "the rolling stones gimme shelter"),
                  time = "2019-06-16 2019-06-30",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_69 <- pair_69$interest_over_time
pair_69 <- pivot_wider(data = pair_69,
                      names_from = date,
                      values_from = hits)
pair_69 <- select(pair_69, -c(geo, time, gprop, category))
names(pair_69) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_69$pair <- c(69, 69)
pair_69$group <- c('treatment', 'control')

pair_70 <- gtrends(keyword = c("pantera walk", "joe satriani satch boogie"),
                  time = "2019-06-23 2019-07-07",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_70 <- pair_70$interest_over_time
pair_70 <- pivot_wider(data = pair_70,
                      names_from = date,
                      values_from = hits)
pair_70 <- select(pair_70, -c(geo, time, gprop, category))
names(pair_70) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_70$pair <- c(70, 70)
pair_70$group <- c('treatment', 'control')

pair_71 <- gtrends(keyword = c("boston more than a feeling", "ozzy osbourne no more tears"),
                  time = "2019-07-04 2019-07-18",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_71 <- pair_71$interest_over_time
pair_71 <- pivot_wider(data = pair_71,
                      names_from = date,
                      values_from = hits)
pair_71 <- select(pair_71, -c(geo, time, gprop, category))
names(pair_71) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_71$pair <- c(71, 71)
pair_71$group <- c('treatment', 'control')

pair_72 <- gtrends(keyword = c("plini electric sunrise", "danzig mother"),
                  time = "2019-07-10 2019-07-24",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_72 <- pair_72$interest_over_time
pair_72 <- pivot_wider(data = pair_72,
                      names_from = date,
                      values_from = hits)
pair_72 <- select(pair_72, -c(geo, time, gprop, category))
names(pair_72) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_72$pair <- c(72, 72)
pair_72$group <- c('treatment', 'control')

pair_73 <- gtrends(keyword = c("the rolling stones angie", "the cult love removal machine"),
                  time = "2019-08-09 2019-08-23",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_73 <- pair_73$interest_over_time
pair_73 <- pivot_wider(data = pair_73,
                      names_from = date,
                      values_from = hits)
pair_73 <- select(pair_73, -c(geo, time, gprop, category))
names(pair_73) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_73$pair <- c(73, 73)
pair_73$group <- c('treatment', 'control')

pair_74 <- gtrends(keyword = c("jeff buckley vancouver", "aerosmith walk this way"),
                  time = "2019-09-05 2019-09-19",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_74 <- pair_74$interest_over_time
pair_74 <- pivot_wider(data = pair_74,
                      names_from = date,
                      values_from = hits)
pair_74 <- select(pair_74, -c(geo, time, gprop, category))
names(pair_74) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_74$pair <- c(74, 74)
pair_74$group <- c('treatment', 'control')

pair_75 <- gtrends(keyword = c("red hot chili peppers under the bridge", "neil young rockin in the free world"),
                  time = "2019-10-06 2019-10-20",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_75 <- pair_75$interest_over_time
pair_75 <- pivot_wider(data = pair_75,
                      names_from = date,
                      values_from = hits)
pair_75 <- select(pair_75, -c(geo, time, gprop, category))
names(pair_75) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_75$pair <- c(75, 75)
pair_75$group <- c('treatment', 'control')

pair_76 <- gtrends(keyword = c("xtc mayor of simpleton", "billy idol rebel yell"),
                  time = "2019-10-19 2019-11-02",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_76 <- pair_76$interest_over_time
pair_76 <- pivot_wider(data = pair_76,
                      names_from = date,
                      values_from = hits)
pair_76 <- select(pair_76, -c(geo, time, gprop, category))
names(pair_76) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_76$pair <- c(76, 76)
pair_76$group <- c('treatment', 'control')

pair_77 <- gtrends(keyword = c("rem losing my religion", "weezer say it aint so"),
                  time = "2019-11-25 2019-12-09",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_77 <- pair_77$interest_over_time
pair_77 <- pivot_wider(data = pair_77,
                      names_from = date,
                      values_from = hits)
pair_77 <- select(pair_77, -c(geo, time, gprop, category))
names(pair_77) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_77$pair <- c(77, 77)
pair_77$group <- c('treatment', 'control')

pair_78 <- gtrends(keyword = c("deftones minerva", "blur song 2"),
                  time = "2019-12-03 2019-12-17",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_78 <- pair_78$interest_over_time
pair_78 <- pivot_wider(data = pair_78,
                      names_from = date,
                      values_from = hits)
pair_78 <- select(pair_78, -c(geo, time, gprop, category))
names(pair_78) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_78$pair <- c(78, 78)
pair_78$group <- c('treatment', 'control')

pair_79 <- gtrends(keyword = c("foo fighters the pretender", "david bowie ziggy stardust"),
                  time = "2019-12-12 2019-12-26",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_79 <- pair_79$interest_over_time
pair_79 <- pivot_wider(data = pair_79,
                      names_from = date,
                      values_from = hits)
pair_79 <- select(pair_79, -c(geo, time, gprop, category))
names(pair_79) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_79$pair <- c(79, 79)
pair_79$group <- c('treatment', 'control')

pair_80 <- gtrends(keyword = c("kansas carry on wayward son", "acdc let there be rock"),
                  time = "2019-12-22 2020-01-05",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_80 <- pair_80$interest_over_time
pair_80 <- pivot_wider(data = pair_80,
                      names_from = date,
                      values_from = hits)
pair_80 <- select(pair_80, -c(geo, time, gprop, category))
names(pair_80) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_80$pair <- c(80, 80)
pair_80$group <- c('treatment', 'control')

pair_81 <- gtrends(keyword = c("stevie wonder superstition", "bush machinehead"),
                  time = "2020-02-01 2020-02-15",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_81 <- pair_81$interest_over_time
pair_81 <- pivot_wider(data = pair_81,
                      names_from = date,
                      values_from = hits)
pair_81 <- select(pair_81, -c(geo, time, gprop, category))
names(pair_81) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_81$pair <- c(81, 81)
pair_81$group <- c('treatment', 'control')

pair_82 <- gtrends(keyword = c("elton john rocket man", "faith no more midlife crisis"),
                  time = "2020-02-25 2020-03-10",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_82 <- pair_82$interest_over_time
pair_82 <- pivot_wider(data = pair_82,
                      names_from = date,
                      values_from = hits)
pair_82 <- select(pair_82, -c(geo, time, gprop, category))
names(pair_82) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_82$pair <- c(82, 82)
pair_82$group <- c('treatment', 'control')

pair_83 <- gtrends(keyword = c("the cure just like heaven", "jimi hendrix foxey lady"),
                  time = "2020-03-12 2020-03-26",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_83 <- pair_83$interest_over_time
pair_83 <- pivot_wider(data = pair_83,
                      names_from = date,
                      values_from = hits)
pair_83 <- select(pair_83, -c(geo, time, gprop, category))
names(pair_83) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_83$pair <- c(83, 83)
pair_83$group <- c('treatment', 'control')

pair_84 <- gtrends(keyword = c("oasis dont look back in anger", "the offspring gone away"),
                  time = "2020-03-17 2020-03-31",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_84 <- pair_84$interest_over_time
pair_84 <- pivot_wider(data = pair_84,
                      names_from = date,
                      values_from = hits)
pair_84 <- select(pair_84, -c(geo, time, gprop, category))
names(pair_84) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_84$pair <- c(84, 84)
pair_84$group <- c('treatment', 'control')

pair_85 <- gtrends(keyword = c("third eye blind semi charmed life", "cheap trick hello there"),
                  time = "2020-03-23 2020-04-06",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_85 <- pair_85$interest_over_time
pair_85 <- pivot_wider(data = pair_85,
                      names_from = date,
                      values_from = hits)
pair_85 <- select(pair_85, -c(geo, time, gprop, category))
names(pair_85) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_85$pair <- c(85, 85)
pair_85$group <- c('treatment', 'control')

pair_86 <- gtrends(keyword = c("the white stripes seven nation army", "incubus drive"),
                  time = "2020-03-30 2020-04-13",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_86 <- pair_86$interest_over_time
pair_86 <- pivot_wider(data = pair_86,
                      names_from = date,
                      values_from = hits)
pair_86 <- select(pair_86, -c(geo, time, gprop, category))
names(pair_86) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_86$pair <- c(86, 86)
pair_86$group <- c('treatment', 'control')

pair_87 <- gtrends(keyword = c("led zeppelin ramble on", "the doors break on through to the other side"),
                  time = "2020-04-08 2020-04-22",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_87 <- pair_87$interest_over_time
pair_87 <- pivot_wider(data = pair_87,
                      names_from = date,
                      values_from = hits)
pair_87 <- select(pair_87, -c(geo, time, gprop, category))
names(pair_87) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_87$pair <- c(87, 87)
pair_87$group <- c('treatment', 'control')

pair_88 <- gtrends(keyword = c("karnivool goliath", "bush glycerine"),
                  time = "2020-04-17 2020-05-01",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_88 <- pair_88$interest_over_time
pair_88 <- pivot_wider(data = pair_88,
                      names_from = date,
                      values_from = hits)
pair_88 <- select(pair_88, -c(geo, time, gprop, category))
names(pair_88) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_88$pair <- c(88, 88)
pair_88$group <- c('treatment', 'control')

pair_89 <- gtrends(keyword = c("korn freak on a leash", "the stone roses love spreads"),
                  time = "2020-05-01 2020-05-15",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_89 <- pair_89$interest_over_time
pair_89 <- pivot_wider(data = pair_89,
                      names_from = date,
                      values_from = hits)
pair_89 <- select(pair_89, -c(geo, time, gprop, category))
names(pair_89) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_89$pair <- c(89, 89)
pair_89$group <- c('treatment', 'control')

pair_90 <- gtrends(keyword = c("nirvana smells like teen spirit", "sex pistols anarchy in the uk"),
                  time = "2020-05-13 2020-05-27",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_90 <- pair_90$interest_over_time
pair_90 <- pivot_wider(data = pair_90,
                      names_from = date,
                      values_from = hits)
pair_90 <- select(pair_90, -c(geo, time, gprop, category))
names(pair_90) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_90$pair <- c(90, 90)
pair_90$group <- c('treatment', 'control')

pair_91 <- gtrends(keyword = c("joni mitchell amelia", "lenny kravitz fly away"),
                  time = "2020-05-20 2020-06-03",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_91 <- pair_91$interest_over_time
pair_91 <- pivot_wider(data = pair_91,
                      names_from = date,
                      values_from = hits)
pair_91 <- select(pair_91, -c(geo, time, gprop, category))
names(pair_91) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_91$pair <- c(91, 91)
pair_91$group <- c('treatment', 'control')

pair_92 <- gtrends(keyword = c("pearl jam black", "helmet unsung"),
                  time = "2020-06-23 2020-07-07",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_92 <- pair_92$interest_over_time
pair_92 <- pivot_wider(data = pair_92,
                      names_from = date,
                      values_from = hits)
pair_92 <- select(pair_92, -c(geo, time, gprop, category))
names(pair_92) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_92$pair <- c(92, 92)
pair_92$group <- c('treatment', 'control')

pair_93 <- gtrends(keyword = c("coldplay the scientist", "primus tommy the cat"),
                  time = "2020-07-02 2020-07-16",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_93 <- pair_93$interest_over_time
pair_93 <- pivot_wider(data = pair_93,
                      names_from = date,
                      values_from = hits)
pair_93 <- select(pair_93, -c(geo, time, gprop, category))
names(pair_93) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_93$pair <- c(93, 93)
pair_93$group <- c('treatment', 'control')

pair_94 <- gtrends(keyword = c("gordon lightfoot if you could read my mind", "heart barracuda"),
                  time = "2020-07-28 2020-08-11",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_94 <- pair_94$interest_over_time
pair_94 <- pivot_wider(data = pair_94,
                      names_from = date,
                      values_from = hits)
pair_94 <- select(pair_94, -c(geo, time, gprop, category))
names(pair_94) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_94$pair <- c(94, 94)
pair_94$group <- c('treatment', 'control')

pair_95 <- gtrends(keyword = c("crowfield cardinal motion", "filter hey man nice shot"),
                  time = "2020-08-19 2020-09-02",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_95 <- pair_95$interest_over_time
pair_95 <- pivot_wider(data = pair_95,
                      names_from = date,
                      values_from = hits)
pair_95 <- select(pair_95, -c(geo, time, gprop, category))
names(pair_95) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_95$pair <- c(95, 95)
pair_95$group <- c('treatment', 'control')

pair_96 <- gtrends(keyword = c("the who love reign oer me", "silverchair tomorrow"),
                  time = "2020-09-01 2020-09-15",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_96 <- pair_96$interest_over_time
pair_96 <- pivot_wider(data = pair_96,
                      names_from = date,
                      values_from = hits)
pair_96 <- select(pair_96, -c(geo, time, gprop, category))
names(pair_96) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_96$pair <- c(96, 96)
pair_96$group <- c('treatment', 'control')

pair_97 <- gtrends(keyword = c("a perfect circle weak and powerless", "primus wynonas big brown beaver"),
                  time = "2020-10-06 2020-10-20",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_97 <- pair_97$interest_over_time
pair_97 <- pivot_wider(data = pair_97,
                      names_from = date,
                      values_from = hits)
pair_97 <- select(pair_97, -c(geo, time, gprop, category))
names(pair_97) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_97$pair <- c(97, 97)
pair_97$group <- c('treatment', 'control')

pair_98 <- gtrends(keyword = c("phish waste", "311 beautiful disaster"),
                  time = "2020-11-10 2020-11-24",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_98 <- pair_98$interest_over_time
pair_98 <- pivot_wider(data = pair_98,
                      names_from = date,
                      values_from = hits)
pair_98 <- select(pair_98, -c(geo, time, gprop, category))
names(pair_98) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_98$pair <- c(98, 98)
pair_98$group <- c('treatment', 'control')

pair_99 <- gtrends(keyword = c("the cars just what i needed", "ozzy osbourne crazy train"),
                  time = "2020-12-31 2021-01-14",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_99 <- pair_99$interest_over_time
pair_99 <- pivot_wider(data = pair_99,
                      names_from = date,
                      values_from = hits)
pair_99 <- select(pair_99, -c(geo, time, gprop, category))
names(pair_99) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_99$pair <- c(99, 99)
pair_99$group <- c('treatment', 'control')

pair_100 <- gtrends(keyword = c("peter frampton do you feel like we do", "the clash london calling"),
                  time = "2021-03-01 2021-03-15",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_100 <- pair_100$interest_over_time
pair_100 <- pivot_wider(data = pair_100,
                      names_from = date,
                      values_from = hits)
pair_100 <- select(pair_100, -c(geo, time, gprop, category))
names(pair_100) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_100$pair <- c(100, 100)
pair_100$group <- c('treatment', 'control')

pair_101 <- gtrends(keyword = c("kelly clarkson since u been gone", "the doors la woman"),
                  time = "2021-03-22 2021-04-05",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_101 <- pair_101$interest_over_time
pair_101 <- pivot_wider(data = pair_101,
                      names_from = date,
                      values_from = hits)
pair_101 <- select(pair_101, -c(geo, time, gprop, category))
names(pair_101) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_101$pair <- c(101, 101)
pair_101$group <- c('treatment', 'control')

pair_102 <- gtrends(keyword = c("u2 i still havent found what im looking for", "the presidents of the united states lump"),
                  time = "2021-04-19 2021-05-03",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_102 <- pair_102$interest_over_time
pair_102 <- pivot_wider(data = pair_102,
                      names_from = date,
                      values_from = hits)
pair_102 <- select(pair_102, -c(geo, time, gprop, category))
names(pair_102) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_102$pair <- c(102, 102)
pair_102$group <- c('treatment', 'control')

pair_103 <- gtrends(keyword = c("polyphia goat", "blue oyster cult burnin for you"),
                  time = "2021-05-11 2021-05-25",
                  gprop = "youtube",
                  low_search_volume = TRUE)
pair_103 <- pair_103$interest_over_time
pair_103 <- pivot_wider(data = pair_103,
                      names_from = date,
                      values_from = hits)
pair_103 <- select(pair_103, -c(geo, time, gprop, category))
names(pair_103) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7')
pair_103$pair <- c(103, 103)
pair_103$group <- c('treatment', 'control')

# CREATING THE DATASET ============================================================================================================
data <- as.data.frame(rbind(pair_1, pair_2, pair_3, pair_4, pair_5, pair_6, pair_7, pair_8, pair_9, pair_10,
                            pair_11, pair_12, pair_13, pair_14, pair_15, pair_16, pair_17, pair_18, pair_19, pair_20,
                            pair_21, pair_22, pair_23, pair_24, pair_25, pair_26, pair_27, pair_28, pair_29, pair_30,
                            pair_31, pair_32, pair_33, pair_34, pair_35, pair_36, pair_37, pair_38, pair_39, pair_40,
                            pair_41, pair_42, pair_43, pair_44, pair_45, pair_46, pair_47, pair_48, pair_49, pair_50,
                            pair_51, pair_52, pair_53, pair_54, pair_55, pair_56, pair_57, pair_58, pair_59, pair_60,
                            pair_61, pair_62, pair_63, pair_64, pair_65, pair_66, pair_67, pair_68, pair_69, pair_70,
                            pair_71, pair_72, pair_73, pair_74, pair_75, pair_76, pair_77, pair_78, pair_79, pair_80,
                            pair_81, pair_82, pair_83, pair_84, pair_85, pair_86, pair_87, pair_88, pair_89, pair_90,
                            pair_91, pair_92, pair_93, pair_94, pair_95, pair_96, pair_97, pair_98, pair_99, pair_100,
                            pair_101, pair_102, pair_103))
remove(pair_1, pair_2, pair_3, pair_4, pair_5, pair_6, pair_7, pair_8, pair_9, pair_10,
       pair_11, pair_12, pair_13, pair_14, pair_15, pair_16, pair_17, pair_18, pair_19, pair_20,
       pair_21, pair_22, pair_23, pair_24, pair_25, pair_26, pair_27, pair_28, pair_29, pair_30,
       pair_31, pair_32, pair_33, pair_34, pair_35, pair_36, pair_37, pair_38, pair_39, pair_40,
       pair_41, pair_42, pair_43, pair_44, pair_45, pair_46, pair_47, pair_48, pair_49, pair_50,
       pair_51, pair_52, pair_53, pair_54, pair_55, pair_56, pair_57, pair_58, pair_59, pair_60,
       pair_61, pair_62, pair_63, pair_64, pair_65, pair_66, pair_67, pair_68, pair_69, pair_70,
       pair_71, pair_72, pair_73, pair_74, pair_75, pair_76, pair_77, pair_78, pair_79, pair_80,
       pair_81, pair_82, pair_83, pair_84, pair_85, pair_86, pair_87, pair_88, pair_89, pair_90,
       pair_91, pair_92, pair_93, pair_94, pair_95, pair_96, pair_97, pair_98, pair_99, pair_100,
       pair_101, pair_102, pair_103)

write_csv(data, file = "bartvanvlerken.csv")
drive_upload(media = 'bartvanvlerken.csv', 
              path = as_id('https://drive.google.com/drive/folders/1eRe1lnDFZt4w6nR7enfKbWYAoFoSYtag?usp=sharing'), 
              type = 'spreadsheet',
              overwrite = FALSE)

# LOADING THE DATASET ===============================================================================================================
drive_download(file = as_id('https://docs.google.com/spreadsheets/d/1wODWrtNk0ZvA_Sb9EPP0GuGzP6jXu4K63CbXhwjv7VA/edit?usp=sharing'),
               path = '../output/bartvanvlerken.csv')
data <- read.csv('../output/bartvanvlerken.csv')
