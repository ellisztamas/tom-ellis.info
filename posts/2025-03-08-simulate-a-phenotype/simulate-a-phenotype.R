library(tidyverse)
set.seed(638)


# Create a table with a row for each line, indicating line name and cohort.
# Table of line names and cohorts
lines <- tibble(
  cohort = c( rep("cohort1", 211), rep("cohort2", 206), rep("cohort3", 207) )
) %>% 
  mutate(
    line = paste0(cohort, "_", 1:length(cohort))
  )

# Take that table of lines and randomise the order.
# Do that separately for replicates one to three, and add a column with the replicate ID.
# It is important that the order of randomisation be different in the three replicates.
nreps <- 3
exp_design = vector('list', nreps) # An empty list
# Randomise the order of lines in each replicate
for (r in 1:nreps){
  exp_design[[r]] <- lines[sample(1:nrow(lines)),] %>% 
    mutate(replicate = r)
}
exp_design <- bind_rows(exp_design)

# Add a column giving the tray that each pot should be placed in.
# 1872 pots fit neatly into 39 trays of 48, so this is as simple as replicating 
# each of the numbers one to 48 39 times.
ntrays <- 39
exp_design <- exp_design %>% 
  mutate(
    tray = rep(1:ntrays, each=48)
  )
# We now have a table indicating where each pot in the experiment needs to go.

## Simulate effects

# We need to simulate effects of being a plant of each line, in each cohort, in
# each tray, in each replicate.
# We will do this with a series of tables for each, then use join them.
# There are actually much less verbose ways to do this in R, but I will use the
# tidyverse way because it offers a way to do this which is clear and robust.
# 
# The following code creates a table with a row for each cohort, and an 'effect'
# for each drawn from a normal distribution with a standard deviation of 0.01.

cohort_effects <- tibble(
  cohort = unique(exp_design$cohort),
  cohort_effect = rnorm(n=3, sd=0.01)
)

# This means that individuals in cohort 1 will have a phenotype that is on
# average 0.0167 higher than the mean, those in cohort 2 will be 0.000591 above 
# the mean, and those in cohort 3 will be 0.0043 below the mean.

# The following code does the same for the three replicate cohorts, the 624 
# lines and the 39 trays

replicate_effects <- tibble(
  replicate = 1:nreps,
  replicate_effect = rnorm(n=nreps, sd=0.2)
)

genetic_effects  <- tibble(
  line = lines$line,
  genetic_effect = rnorm(n=nrow(lines), sd=0.5)
)

tray_effects <- tibble(
  tray = 1:ntrays,
  tray_effect = rnorm(n=ntrays, sd = 0.1)
  
)

## Simulate a phenotype

# Using a `join` command is a robust way to merge these tables.
# The following code adds effects for cohort, replicate, line and tray to the
# table of 1872 individual plants.
# Take a look at it and convince yourself that plants of the same replicate have
# the same replicate effect, and those of the same line have the same line effect,
# and so forth.

exp_design <- exp_design %>% 
  left_join(cohort_effects, by = "cohort") %>% 
  left_join(replicate_effects, by ='replicate') %>% 
  left_join(genetic_effects, by='line') %>%
  left_join(tray_effects, by = 'tray')

# The only thing left to do is to add up these effects, and add some residual variance.
exp_design <- exp_design %>% 
  mutate(
    normal_phenotype = cohort_effect + replicate_effect + genetic_effect + tray_effect + rnorm(nrow(exp_design), 0.19)
  )
