#' analysis.R
#'
#' What this file does:
#' Do your main analysis in this script
#'

# --- Load Libraries --- #
library(tidyverse)
library(ggthemes)
library(broom)
library(rlist)

# --- Load Data --- #
data <- read.csv("../output/bartvanvlerken.csv")
names(data) <- c('song', 't-7', 't-6', 't-5', 't-4', 't-3', 't-2', 't-1', 't',
                 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7', 'pair', 'group')

# --- Analysis --- #
# Checking the parallel trends assumption
parallel_trends <- data %>% select('t-7':'t-1', 'group')
parallel_trends <- parallel_trends %>% pivot_longer('t-7':'t-1')
ggplot(parallel_trends, aes(x = name, y = value)) + geom_boxplot(varwidth = TRUE) + 
    facet_grid(. ~ group) + theme_clean() +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = 'Figure 2',
         subtitle = 'Parallel Trends',
         caption = 'Source: Google Trends',
         x = '', y = 'Popularity')
ggsave('../output/figure_2.pdf', width = 200, height = 200, units = "mm")    
remove(parallel_trends)

# Difference-in-differences regression
did_data <- data %>% select('t-7':'t+7', group)
did_data <- did_data %>% pivot_longer('t-7':'t+7')
did_data$group <- ifelse(did_data$group == 'control', 0, 1)
did_data$time <- ifelse(did_data$name == 't-7', 0,
                        ifelse(did_data$name == 't-6', 0,
                               ifelse(did_data$name == 't-5', 0,
                                      ifelse(did_data$name == 't-4', 0,
                                             ifelse(did_data$name == 't-3', 0,
                                                    ifelse(did_data$name == 't-2', 0,
                                                           ifelse(did_data$name == 't-1', 0, 1)))))))
did_regression <- lm(value ~ group * time, data = did_data)
did_model <- tidy(did_regression, conf.int = TRUE)

# --- Save Output --- #
list.save(did_model, '../output/did_model.Rds')

# Difference-in-differences graph: I tried for my graph to resemble the one in the slides as close as possible.
# I did have some difficulties with the lines in the treatment group, but I'm satisfied with my result.
control_before <- c('control', 'before', round(did_regression$coefficients[1], digits = 1))
control_after <- c('control', 'after', round(did_regression$coefficients[1] + did_regression$coefficients[3], digits = 1))
treatment_before <- c('treatment', 'before', round(did_regression$coefficients[1] + did_regression$coefficients[2], digits = 1))
treatment_after <- c('treatment', 'after', round(did_regression$coefficients[1] + did_regression$coefficients[2] + 
                                                     did_regression$coefficients[3] + did_regression$coefficients[4], digits = 1))
reference_point <- c('treatment', 'after', round(did_regression$coefficients[1] + did_regression$coefficients[2] + did_regression$coefficients[3], digits = 1))
table <- as.data.frame(rbind(control_before, control_after, treatment_before, treatment_after, reference_point))
names(table) <- c('group', 'time', 'popularity')
remove(control_before, control_after, treatment_before, treatment_after, reference_point, did_data, did_model, did_regression)
table$time <- factor(table$time, levels = c('before', 'after'))

ggplot(data = table, aes(x = time, y = popularity, colour = group, group = group)) + geom_point() + theme_clean() +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = 'Figure 3',
         subtitle = 'Difference-in-Differences',
         caption = 'Source: Google Trends',
         x = '', y = 'Popularity') +
    annotate("text", label = expression(delta), x = 2.1, y = 4.5, size = 4, colour = "black") +
    geom_line(linetype = 'dashed')
ggsave('../output/figure_3.pdf', width = 200, height = 200, units = "mm")
remove(table)
