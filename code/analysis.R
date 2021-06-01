#' analysis.R
#'
#' What this file does:
#' Do your main analysis in this script
#'

# --- Load Libraries --- #
library(dplyr)
library(gapminder)
library(fixest)
library(forcats)
library(purrr)
library(rlist)

# --- Load Data --- #
# you will probably want to read in your data, 
# this example uses data from the gapminder library
gap_df <- gapminder

# --- Analysis --- #

reg_models <-
    gap_df %>%
    filter(continent %in% c('Africa', 'Asia', 'Oceania')) %>%
    mutate(continent = fct_drop(continent)) %>%
    split(.$continent) %>%
    map(~ feols(log(lifeExp) ~ log(gdpPercap) + log(pop)
                |
                    country,
                data = .
                )
    )

# --- Save Output --- #
# save the output to the 'output' directory
# the output could be a dataset, or it could be model output
# in this example its the regression models
list.save(reg_models, 'output/reg_models.Rds')
