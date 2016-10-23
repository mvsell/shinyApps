  
  # libraries ---------------------------------------------------------------
  
  library(ggplot2)
  library(dplyr)
  library(zoo)
  
  cutoff <- 0.4
  topcut <- 3
  internal_dilution <- 10
  
  
  # load data ---------------------------------------------------------------
  DHinterval <- read.csv("data/Au_Interval.csv", header = TRUE)
  
  
  # Interval identification -------------------------------------------------
  
  # get a columns where there are 1's if Au_ppm > cutoff, and 0's if not 
  DHinterval$ints <- findInterval(DHinterval$Au_ppm, cutoff)
  
  # Accounts for specified internal dilution
  consecutive_intValues <- rle(DHinterval$ints) # creates vector with lengths of consecutive runs and the values of those runs in this case it is 0 or 1
  DHinterval$dilutionCondition <- rep(consecutive_intValues$lengths >= internal_dilution,times = consecutive_intValues$lengths) # replicates
  DHinterval$update <- DHinterval$ints
  DHinterval$update[DHinterval$ints == 0 & DHinterval$dilutionCondition == "FALSE"] <- 1
  
  
  # make a dataframe with start positions of runs of 1's 
  # starts <- filter(DHinterval, Au_ppm >= cutoff & ((lag(update) == 0 | is.na(lag(update)))& lead(update) == 1)) 
  starts <- filter(DHinterval, ((lag(update) == 0 | is.na(lag(update)))& lead(update) == 1)) 
  starts$position <- rep('start', dim(starts)[1])
  
  # make a dataframe with end positions of runs of 1's 
  # ends <- filter(DHinterval, Au_ppm >= cutoff & (lag(update) == 1 & (lead(update) == 0) | is.na(lead(update)))) 
  ends <- filter(DHinterval, (lag(update) == 1 & (lead(update) == 0) | is.na(lead(update))))
  ends$position <- rep('end', dim(ends)[1])
  
  # make a dataframe with groups of runs
  Au_run <- rbind(starts, ends)

# order them by position
Au_run <- Au_run[with(Au_run, order(From)), ] 
#Au_run$group <- rep(1:(dim(Au_run)[1]/2), each=2)
x=seq(1,(dim(starts)[1]),1)
Au_run$group <- sort(c(x,x))

# join Au_run with original dataset
DHinterval_grouped <- left_join(DHinterval, Au_run)

# change group to 0 if int is 0
DHinterval_grouped$group[DHinterval_grouped$update == 0] <- 0

# fill in NA's with last values (singles will get filled in with 0?) 
new_groups <- na.locf(DHinterval_grouped$group)

if (length(new_groups) < length(DHinterval_grouped$group)) {
  new_groups <- append(0, new_groups)
}

DHinterval_grouped$group <- new_groups


# aggregate statistics ----------------------------------------------------

# ceiling values above to top cut to top cut value
DHinterval_grouped$Au_ppm[DHinterval_grouped$Au_ppm >= topcut] <- topcut

# calculate mean for each Au group
Au_means <- DHinterval_grouped %>%
  group_by(group) %>%
  summarise(avg = mean(Au_ppm), interval_length = max(To) - min(From), start = min(From), end = max(To))

# re-order columns
Au_means <- Au_means %>% select(start, end, interval_length, avg, group)

# remove group 0 (average of < 0.2 Au_ppm & singletons) 
Au_means <- Au_means[Au_means$group != 0,]

# write to csv
write.csv(Au_means, "Au_means.csv", row.names = FALSE)

# plot means on a bar -----------------------------------------------------
Au_ppm_plot <- ggplot(DHinterval_grouped) +
  geom_line(aes(y=Au_ppm, x=From)) +
  geom_line(aes(y = 0, x=From, colour = log(Au_ppm)), size=2) +
  coord_flip() +
  scale_x_reverse() +
  scale_colour_gradientn(colours = rev(rainbow(5)))

probPlot <- ggplot(DHinterval, aes(sample = Au_ppm)) +
  stat_qq() +
  scale_y_log10() +
  geom_hline(aes(yintercept=cutoff)) + # cutoff
  geom_hline(aes(yintercept=topcut)) # top cut

probPlot
Au_ppm_plot
ggsave('Au_ppm.pdf', Au_ppm_plot)
ggsave('ProbPlot.pdf', probPlot)
Au_means
