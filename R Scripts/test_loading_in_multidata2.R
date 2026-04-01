library(tidyverse)
library(furrr)
library(progressr)
library(StrathE2E2)

plan(multisession, workers = 4)

#files <- list.files("./Objects/Heatwaves/", full.names = T)

# this loads in all of the Norwegian model runs
Norwegian_Basin_files <- list.files("./Objects/Heatwaves/",
pattern = "^Norwegian",
full.names = T)

with_progress({
  Norwegian_Basin_data <- future_map(Norwegian_Basin_files, read_rds)})

#this renames all the model runs inside ascension_files, to be called something easy
#to identify, so that it's not showing as [[1]] : [[180]]
names(Norwegian_Basin_data) <- basename(Norwegian_Basin_files)

# this combines all of the daily time series data for all model runs into one
# big data frame
mass_combined <- imap_dfr(
  Norwegian_Basin_data, 
  ~ as.data.frame(.x$output), 
  .id = "model_run")
 
# this pulls out the separate parts of the model run name, into different columns 
# and drops the ones we don't want
 mass_combined <- mass_combined %>% 
   mutate(model_run = sub("\\.rds$", "", model_run)) %>%  # this gets rid of .rds file extension at the end
   mutate(model_run = gsub("--", "-neg", model_run)) %>%  # this temporarily sorts the double negative issue
   separate(model_run,
            into = c("region", "model", "years", "gcm", "scenario", "month", "temp"),
            sep = "-") %>%  
   select(-"gcm", -"scenario", -"model", -"years") 
 
 mass_combined$temp <- as.numeric(gsub("^neg", "-", mass_combined$temp))  # this fixes the double negative issue
 
# once i'm happy that the data is okay, i'll save out as an rds so that it frees up some space and 
# this step can be skipped each time 
 
 
######### i can't get anything to show up here as an interesting result ########
# no matter what parameter i look at
mass_combined$temp_factor <- factor(mass_combined$temp)
 
plot1 <- ggplot(mass_combined, aes(
  x = time, 
  y = carngrossprod_o,
  group = temp,
  colour = factor(temp))) +
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 3600, by = 360),
                     labels = paste0(0:10))

 plot1
 
################################################################################
 
 # this shows which parameters have changed across the experiment
 
 # creating a dataframe of one specific day across all of the 10 years to see how/if things have changed
 # wants to be close ish to the HW 
 
 smaller <- mass_combined %>% 
   filter(month == 1 & time == 400)    
 
 # flip to long format and then measure the variation across temperature scenarios.
 # at that fixed time point, calculated the standard dev across temperature scenarios
 
 new <- smaller %>% 
   pivot_longer(
     cols = detritus_so:fluxDOCETAoutflow_o,
     names_to = "Variable",
     values_to = "Biomass"
   ) %>% 
   group_by(Variable, time) %>% 
   summarise(sd_across_temp = sd(Biomass), .groups = "drop") %>% 
   arrange(desc(sd_across_temp))

 # this is the plot that was at the bottom of Mike's code. 
 # this is for only 1 scenario. How do i display something like this for all 180 scenarios?
 
e2e_read('Norwegian_Basin_MA','2010-2019-CNRM-ssp370') %>% e2e_plot_ts(results = Norwegian_Basin_data[[1]])
 
###########################

#messing around trying to produce any output right now

run <- Norwegian_Basin_data[["Norwegian_Basin_MA-2010-2019-CNRM-ssp370-4-5.rds"]]

e2e_plot_ts(run, run)
# the above is just for one of the scenarios - can i do this for all the runs? Would look v busy/messy??

p1 <- plot(mass_combined$time, mass_combined$carngrossprod_i)
p1
# why does the above look so weird? The values on the y don't keep increasing in the data??

plot(mass_combined$time, mass_combined$detritus_so)
# ok this looks better. Whats happening in year 1 though, why is the line more black than others?

test <- ggplot(mass_combined, 
               aes(x = time, 
                   y = detritus_so)) +
                geom_line()
test  
# same in the base r plot above - the seasonal cycle is now working but year 2 (HW year) looks weird and black?

# ooo is the black bit showing the difference between the base run and the heatwave simulation? Would be clever if so...

# lets try another parameter 

test2 <- ggplot(mass_combined, 
                aes(x = time, 
                    y = carn_o)) + 
                  geom_line() +
  xlim(361, 722) # limits to just the year 2 to zoom in 
test2
# ok, same extra thick black line - i think that this is the difference between base and HW scenario
# but how is it doing this? thought i'd have to do this manually??

# lets sort out the time axis 
test3 <- mass_combined2 %>% 
        #filter(month == "3") %>% 
        ggplot(aes(x = time, 
                   y = phyt_so,
                   colour = temp,
                   group = temp)) +
        facet_wrap(vars(month)) +
        #facet_wrap(vars(temp)) +
        geom_line() +
        geom_line(data = filter(mass_combined2, temp == "0"), colour = "black") +  #baseline
        scale_x_continuous(breaks = seq(0, 3600, by = 360), # by 359 as ran experiment for 10 years and got 3591 lines of data
        labels = paste0(0:10)) + 
        xlab("Year") +
        ylab("Carn_o") +
        xlim(361, 722) + # limits to just the year 2 to zoom in 
        scale_colour_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0)
  
test3

# trying to sort out time in the data

# something funky going on, time starts on day 0 and as the years cross over, there are duplicate days 
# e.g. 358, 359, 359, 1, 1. Not sure why. The below code should fix this. 

# also adding new columns for year within similation, hw start and end, and day of the year.
# renamed time to 'timestep...'

mass_combined2 <- mass_combined %>% 
  group_by(time, month, temp) %>% 
  slice(1)  # %>% 
#  mutate(n = n())
  

mass_combined3 <- mass_combined2 %>% 
  mutate(
    year = ((time-1) %/% 360) + 1,
    day_of_year = ((time - 1) %% 360 + 1),
    month = as.numeric(as.character(month)), 
    hw_start = (month - 1) * 30 + 1, 
    hw_end = month * 30, 
    heatwave = year == 2 & day_of_year >= hw_start & day_of_year <= hw_end) %>% 
  relocate(year, day_of_year, hw_start, hw_end, heatwave, .after = time) %>% 
  rename(timestep_within_10yr_sim = time)

p1 <- ggplot(mass_combined3 %>%  
               dplyr::filter(.data$month == 3),
       aes(x = day_of_year, y = detritus_so, colour = factor(temp))) +
  facet_wrap(~year) + 
  geom_rect(data = mass_combined3 %>% 
              dplyr::filter(.data$month == 3, year == 2) %>% 
              dplyr::distinct(hw_start, hw_end),
            aes(
              xmin = hw_start, 
              xmax = hw_end, 
              ymin = -Inf, 
              ymax = Inf), 
            inherit.aes = F,
            alpha = 0.2) +
  geom_line() + 
  NULL  
p1 # a bit of an assault on the eyeballs. Month 4 is interesting though.  

# i don't think i want to see the year of the simulation in seperate panels. 
# i want one graph with:
# the full 10 years on the x
# whatever parameter on the y 
# the lines on the graph for different temperature simulations (maybe -2, +2.5 and +5 to begin with)
# narrowed down for one month, but then i can have month of HW in the different panels.


# ok so the below, i wanted to take the first year time series x 10 years (because if it's an annual
# seasonal cycle, then all years should be the same if it's ran for 10 years without changing anything???)

baseline <- mass_combined2 %>% 
  filter(year == 1) %>% 
  select(day_of_year, month, temp, detritus_so) # just picked detritus to start with as it's the first in the list 

# if i sort by day of year and month (to get day 1 of Jan) - all 15 values should be the same?
# because the HW only happens in year 2. 

mass_combined3 <- mass_combined2 %>% 
  filter(year == 1)

# what is happening???


## big question for jack & mike

# why is my year 1 (parameter value - detritus_so in this instance) not the same for every combination of HW month x HW temp 
# because the heatwave doesn't touch anything until year 2
# and so i thought that the year 1 would just be the 'baseline' irrespective of what experiment 
# because it's just existing as it is for one year before anything changes
# i could just filter out the temp of experiment = 0, but why would this be different 
# to the year 1 before the experiment of whatever temperature 


# trying to do the plot that i mention above

p2 <- ggplot(mass_combined2 %>%  
               dplyr::filter(month == 4, temp %in% c(-2, 2.5, 5)),
             aes(
               x = time, 
               y = detritus_so, 
               colour = factor(temp), 
               group = interaction(year, temp))) +
  geom_line() + 
  geom_rect(data = mass_combined2 %>% 
              dplyr::filter(.data$month == 4, year == 2) %>% 
              dplyr::distinct(hw_start, hw_end),
            aes(
              xmin = hw_start, 
              xmax = hw_end, 
              ymin = -Inf, 
              ymax = Inf), 
            inherit.aes = F,
            alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 3600, by = 360),
                     labels = paste0(0:10))
p2 # alright, so something is happening. but HW is year 2. why is the effect only
# showing in the 2nd half of year 3 only?


# testing the values in the data file

# checking to see if the mean changes? No it doesn't
mass_combined2 %>%
  dplyr::filter(temp %in% c(-2, 2.5, 5)) %>%
  group_by(temp) %>%
  summarise(
    mean_detritus = mean(detritus_so, na.rm = TRUE),
    sd_detritus   = sd(detritus_so, na.rm = TRUE)
  ) #ok so heat doesn't change the overall abundance of detritus

ts_summary <- mass_combined2 %>%
  filter(temp %in% c(-2, 2.5, 5)) %>%
  group_by(temp, timestep_within_10yr_sim) %>%
  summarise(mean_detritus = mean(detritus_so, na.rm = TRUE))

ggplot(ts_summary, 
       aes(
         x = timestep_within_10yr_sim, 
         y = mean_detritus, 
         colour = factor(temp))) +
  geom_line()
# this shows that there are differences in the timing.
# also painful on the eyeballs


mass_small <- mass_combined2 %>% 
  dplyr::select(year, temp, month, day_of_year, timestep_within_10yr_sim, detritus_so)

base <- mass_small %>%
  filter(temp == 0, year == 1) %>% # it doesn't matter what month is picked, just stops the duplication 12 times
  group_by(day_of_year) %>% 
  summarise(detritus_base = mean(detritus_so, na.rm = T, group = "drop"))

comparison <- mass_small %>%
  filter(temp %in% c(-2, 2.5, 5)) %>%
  left_join(base, by = "day_of_year") %>%
  mutate(diff = detritus_so - detritus_base)

p3 <- ggplot(comparison, 
       aes(
         x = timestep_within_10yr_sim)) +
  geom_line(aes(y = detritus_base), colour = "black") +
  geom_line(aes(y = detritus_so, colour = factor(temp))) +
  labs(
    x = "Time (Years)",
    y = "Detritus",
    colour = "Heatwave intensity"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 3600, by = 360), 
                     labels = paste0(0:10)) + 
  coord_cartesian(ylim = c(22.5,37.5)) # narrowing down the y axis a bit so its easier to look at
p3

p4 <- ggplot(comparison, 
             aes(
               x = timestep_within_10yr_sim,
               y = diff,
               colour = factor(temp))) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Time (Years)",
    y = "Deviation from baseline",
    colour = "Heatwave intensity"
  ) +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(0, 3600, by = 360), # by 360 as ran experiment for 10 years and got 3591 lines of data
                     labels = paste0(0:10))
p4

# ok something isnt right with my year 1 numbers
# year 1 in the experiment should be exactly the same as they are in the base, but they are not. 

