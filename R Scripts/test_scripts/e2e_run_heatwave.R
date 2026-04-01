library(StrathE2E2)   #version 4

######################################################
# Some notes

# Calling non-exported function: need this to generate the results object 
# e.g.    StrathE2E2:::aggregate_model_output


######################################################################################################
######################################################################################################
######################################################################################################
# Simple setup in which the model is run in steps of 1 year at a time, extracting end state each time
# and inserting into the model object before re-running.
# Model output is accummulated into a combined multi-year object.
# this is a heatwave function. it lets you specify one year where some heat is added 
# to a specific month only and lasts for one year only

e2e_run_heatwave <- function(model, nyears = 10, hw_year = 2, temp, month) {
  
# here for debuggging - comment out when good to go   
# nyears<-5
# model <- e2e_read("Ascension_MA", "2010-2019-CNRM-ssp370")
# nyears <- 10 
# hw_year <- 2
# temp <- 5
# month <- 6
  
for(Year in 1:nyears){
  
  if(Year==1){
    newmodel<-model
    initial_state <- newmodel$data$initial.state
  }
  
  #########################################
 
  # this is where we tell R to create the heatwave. year = hw_year links to section above where it's disclosed
  # as year 2. Section below that changes the physics drivers of surface offshore and inshore temperature
  # by '+ temp' disclosed above, in the months disclosed above.
  
  if(Year==hw_year){
    
    #surface offshore temp
      newmodel[["data"]][["physics.drivers"]][["so_temp"]][month] <-
      newmodel[["data"]][["physics.drivers"]][["so_temp"]][month] + temp
    
    #surface inshore temp
    newmodel[["data"]][["physics.drivers"]][["si_temp"]][month] <-
      newmodel[["data"]][["physics.drivers"]][["si_temp"]][month] + temp
    
  }
  
    #########################################
  
  # this undos the heatwave, so that in year 3 it's back to the temperatures as they were in year 1
  
  if(Year==(hw_year + 1)){
    
    #surface offshore temp
    newmodel[["data"]][["physics.drivers"]][["so_temp"]][month] <-
      newmodel[["data"]][["physics.drivers"]][["so_temp"]][month] - temp
    
    #surface inshore temp
    newmodel[["data"]][["physics.drivers"]][["si_temp"]][month] <-
      newmodel[["data"]][["physics.drivers"]][["si_temp"]][month] - temp
    
  }
  
  ### the stuff after here actually runs the simulation and combines the data together ###
  
  results<-e2e_run(newmodel,nyears=1,csv.output=FALSE)
  daily_output <- results$output
  offshore_annual_group_land_disc<-results$total.annual.catch$offshore_annual_group_land_disc
  inshore_annual_group_land_disc<-results$total.annual.catch$inshore_annual_group_land_disc
  offshore_annual_group_gear_land_disc<-results$annual.catch.by.gear$offshore_annual_group_gear_land_disc
  inshore_annual_group_gear_land_disc<-results$annual.catch.by.gear$inshore_annual_group_gear_land_disc
  
  if(Year==1){ 
    cum_output<-daily_output
    cum_offshore_annual_group_land_disc<-offshore_annual_group_land_disc
    cum_inshore_annual_group_land_disc<-inshore_annual_group_land_disc
    cum_offshore_annual_group_gear_land_disc<-offshore_annual_group_gear_land_disc
    cum_inshore_annual_group_gear_land_disc<-inshore_annual_group_gear_land_disc
  }
  
  if(Year>1){
    daily_output$time <- daily_output$time + (cum_output$time[nrow(cum_output)] -1)
    offshore_annual_group_land_disc$year<-Year
    inshore_annual_group_land_disc$year<-Year
    offshore_annual_group_gear_land_disc$year<-Year
    inshore_annual_group_gear_land_disc$year<-Year
    
    cum_output <- rbind(cum_output, daily_output[2:(nrow(daily_output)),])
    cum_offshore_annual_group_land_disc<- rbind(cum_offshore_annual_group_land_disc,offshore_annual_group_land_disc)
    cum_inshore_annual_group_land_disc<-  rbind(cum_inshore_annual_group_land_disc,inshore_annual_group_land_disc)
    cum_offshore_annual_group_gear_land_disc<-  rbind(cum_offshore_annual_group_gear_land_disc,offshore_annual_group_gear_land_disc)
    cum_inshore_annual_group_gear_land_disc<-  rbind(cum_inshore_annual_group_gear_land_disc,inshore_annual_group_gear_land_disc)
  }
  
 
  #this lets the next year start where we finished off 
  end_state<-daily_output[nrow(daily_output),2:(ncol(daily_output))]
  new_initial_state<-initial_state
  for(column in 1:(ncol(daily_output)-1)){
    new_initial_state[column] <- end_state[column]
  }
  newmodel$data$initial.state <- new_initial_state
  
}

#----------------

# At the end, merge the cumulatiVe results back onto a results object compatible with all other e2e functions

cum_results<-results
cum_results$output <- cum_output
cum_results$aggregates  <- StrathE2E2:::aggregate_model_output(model, cum_output)

cum_results$total.annual.catch$offshore_annual_group_land_disc <- cum_offshore_annual_group_land_disc
cum_results$total.annual.catch$inshore_annual_group_land_disc  <- cum_inshore_annual_group_land_disc

cum_results$annual.catch.by.gear$offshore_annual_group_gear_land_disc <- cum_offshore_annual_group_gear_land_disc
cum_results$annual.catch.by.gear$inshore_annual_group_gear_land_disc  <- cum_inshore_annual_group_gear_land_disc


return(cum_results)

}

### test lines below 

# model <- e2e_read("Ascension_MA", "2010-2019-CNRM-ssp370")
# test <- e2e_run_heatwave(model = model, nyears = 10, hw_year = 2, temp = 5, month = 4)
# 



#-----------

### test lines for plotting below 

# e2e_plot_ts(model, test, selection="ECO")
# 
# #...
# 
# e2e_plot_ts(model,cum_results,selection="CATCH")
# 
# #...
# 
# e2e_plot_eco(model, selection="NUT_PHYT",results=cum_results)


######################################################################################################
######################################################################################################
######################################################################################################



