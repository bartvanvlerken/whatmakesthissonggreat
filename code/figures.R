#' figures.R
#'
#' What this file does:
#' Create your main figures in this script
#'

# --- Load Libraries --- #
library(tidyverse)
library(ggthemes)

# --- Load Data --- #
data <- read.csv('../output/bartvanvlerken.csv')
names(data) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't',
                 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7', 'pair', 'group')

# --- Figure 1 --- #
before <- data %>% select('t-7':'t-1', group)
before <- before %>% pivot_longer('t-7':'t-1')
before$name <- 'before'
after <- data %>% select('t':'t+7', group)
after <- after %>% pivot_longer('t':'t+7')
after$name <- 'after'
figure_data <- rbind(before, after)
figure_data$name <- factor(figure_data$name, levels = c("before", "after"))

ggplot(figure_data, aes(x = name, y = value, fill = group)) + geom_boxplot(varwidth = TRUE) +
    theme_clean() + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = 'Figure 1',
         subtitle = 'Popularity of songs before and after an upload of \'What Makes This Song Great?',
         caption = 'Source: Google Trends',
         x = '', y = 'Popularity')
ggsave('../output/figure_1.pdf', width = 200, height = 200, units = "mm")
remove(after, before, figure_data)

# --- Figure 2 --- #