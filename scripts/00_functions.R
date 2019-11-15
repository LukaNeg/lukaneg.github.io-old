### Source code for all packages and functions I need:

#####################################
#### LOAD PACKAGES AND FUNCTIONS ####
#####################################

### Load packages:
library(tidyverse)
library(lubridate)
library(knitr)
library(DT)
library(xtable)
library(pander)
library(flexdashboard)
library(png)      # For grabbing the dimensions of png files
#source("https://install-github.me/atusy/cssgrid") #https://github.com/atusy/cssgrid
# library(devtools)
# library(remef)
# library(geosphere)
# library(rgdal)
# library(vegan)

# Packages for Bayesian Analysis:
# library(rjags)
# library(R2jags) #R interface for JAGS; install JAGS separately  (runs in background)
# load.module("glm")     
# library(ggmcmc)
# library(HDInterval)
# library(beepr)
# library(xlsx)
# library(lme4)

### Load custom functions:

### Function to draw a logo or image in plot boarder (from https://stat.ethz.ch/pipermail/r-help/2011-September/290269.html)
draw.logo <- function(image, x, y, size, ...) {
  img_ratio <- ncol(image)/nrow(image)
  logo <- rasterGrob(image = image, 
                     x = unit(x, "npc"), 
                     y = unit(y, "npc"),
                     width = unit(size*img_ratio, "cm"), height = unit(size, "cm"),
                     just = c("left", "centre"), gp = gpar(...))
  grid.draw(logo)
}
#plot(1:8, c(1,3,5,8,8,2,6,3))
#draw.logo(img, .02, .04, .5, fontsize = 8)

### Function to detect and label clusters in a vector of dates
### (for example, finding all of the fieldtrips since they are a few dates close together)
dates_vector <- c(1,20,45,46,48,69,103,150,152,180,200,201,202)
fieldtrips <- c(1,2,3,3,3,4,5,6,6,7,8,8,8) #example of what results should be

# 0) set fieldtrip counter to 1 
# 1) loop across vector and extract n, n+1 values
# 2) set current value at n to fieldtrip counter
# 3) compare n and n+1. If they are the less than XX days apart, then fieldtrip counter does not advance
# 4) if the two values are more than XX days appart, then advance the fieldtrip counter
# 5) after looping, group_by the fieldtrips and create a list of dates that are the mean date of each fieldtrip grouping

### First, a function for calculating the mode of a vector:
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#dates_vector <- dat_height$plant_age
field_trip_finder <- function(dates_vector, group_size=5){
  dates_vector_sort <- sort(dates_vector)
  original_order <- rank(dates_vector, ties.method="first")
  dates_vector_order <- order(dates_vector)
  fieldtrip = 1
  trips = c(1)
  for(i in 1:length(original_order)){
    n = dates_vector_sort[i]
    n_plus = dates_vector_sort[i+1]
    if(is.na(n_plus)) break # if n is the last value then n+1 will be NA, so break the loop
    if(!(n_plus - n) <= group_size) fieldtrip = fieldtrip + 1
    trips <- c(trips,fieldtrip)
  }
  final <- bind_cols(dates=as.numeric(dates_vector_sort),trips=trips)
  trips_dat <- group_by(final, trips) %>%
    mutate(trip_dates = round(mean(dates)))
  return(trips_dat$trip_dates[original_order])
}


### Set visual features:
GV2050_colors <- function(type){
  colors <-
    c(control = "#E5C494",
      hydrogel = "#35C5CB",
      cocoon = "#FFD92F",
      cocoon_hydrogel = "#FC8D63",
      groasis = "#A6D854",
      groasis_hydrogel = "#8DA0CB",
      dead = "#CBC6AB",
      dark = "#2E2D2D",
      light = "#EEEFED",
      GVgreen = "#2EB458",
      GVgreen_light = "#7DC477",
      CDFblue = "#354AA0")
  
  return(colors[names(colors) %in% type])
  if(type=="all") return(colors)
}

# barplot(c(rep(10,12)), col=GV2050_colors(type="all"), bty="n")
# barplot(c(rep(10,12)), col=GV2050_colors(type="all"))
# display.brewer.pal(n=10, name="Set2")


GV2050_colors_dark <- function(type){
  colors <-
    c(control = "#D5A57D",
      hydrogel = "#3597CB",
      cocoon = "#DCB743",
      cocoon_hydrogel = "#CC625B",
      groasis = "#5CA860",
      groasis_hydrogel = "#706FA8",
      dead = "#CBC6AB",
      dark = "#2E2D2D",
      light = "#EEEFED",
      GVgreen = "#2EB458",
      GVgreen_light = "#7DC477",
      CDFblue = "#354AA0")
  
  return(colors[names(colors) %in% type])
}

#dat_sub <- dat_all
#i = sort(unique(dat_sub$date))[2]
#deaths = 0
### Function to generate the cumulative sum of all live individual plants:
cumsum_rest <- function(dat_sub, remove_deaths=T){
  used_codigos <- NULL
  dead_codigos <- NULL
  total_count <- 0
  cumsum_restor <- NULL
  for(i in sort(unique(dat_sub$date))){
    date_sub <- filter(dat_sub, date==i) %>%
      select(treatment_ID, state, species_full) # extract the subset of data for that date
    date <- as_date(i)
    # count how many codigos are listed that haven't been previously recorded and aren't dead
    total_count <- total_count + sum(as.numeric(!(date_sub$treatment_ID %in% used_codigos) & date_sub$state!="dead"))
    if(remove_deaths) total_count <- total_count - sum(as.numeric(!(date_sub$treatment_ID %in% dead_codigos) & date_sub$state=="dead"))
    
    used_codigos <- unique(c(used_codigos, date_sub$treatment_ID)) # Record which codigos have just been counted
    dead_codigos <- unique(c(dead_codigos, date_sub$treatment_ID[date_sub$state=="dead"])) # Record which codigos have just been counted as dead
    cumsum_restor <- bind_rows(cumsum_restor, data.frame(date, total_count))
    #print(paste(date,sum(date_sub$state=="dead")))
    #deaths <- deaths + sum(as.numeric(!(date_sub$treatment_ID %in% dead_codigos) & date_sub$state=="dead"))
    
  }
  return(as.tibble(cumsum_restor))
}


### Function to simplify species names to two words:
simp_species <- function(x) gsub("^((\\w+\\W+){1}\\w+).*$","\\1",x)

### function to standardize continuous variables for effect size comparison to logical variables
### and to ensure standardization within each species for global comparison
### https://www.listendata.com/2017/04/how-to-standardize-variable-in-regression.html
standard <- function(x) (x - mean(x, na.rm=T)) / (2*sd(x, na.rm=T))

standard_range <- function(x, na.rm = T){
  fin <- (x-min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm=T))
  fin <- ifelse(is.infinite(fin),NA, fin)
  return(fin)
}
#x <- dat_fin$height_cm


### coloring transparent function:
t_col <- function(color, percent = 50) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3], max = 255, alpha = (100-percent)*255/100)
  invisible(t.col)
}

### function for generating overall best plot y-dimensions
min_max_95 <- function(posteriors){ 
  minmax.all <- NULL
  for(i in 1:length(posteriors)){
    minmax <- c(as.numeric(hdi(posteriors[,i],0.95)))
    minmax.all <- c(minmax.all,minmax)
  }
  return(c(min(minmax.all),max(minmax.all)))
}


### Function to help with subsetting the data:
select_subset <- function(category, x){
  if(length(x)==0) return(levels(category))
  else return(x)
}
## e.g.: select_subset(dat$island, x=c())
##       select_subset(dat$island, x=c("Baltra"))

### Function to calculate the age of a plant (in days) at each sample point:
#test <- filter(dat, treatment_ID==344)
#dates <- test$date
plant_age <- function(dates){ # x is the vector of sample dates
  planting_date <- min(dates) # find the planting date (earliest date)
  ages <- dates-planting_date# subtract the planting date from each other date
  return(ages)
}
#plant_age(dates)

### Function to calculate age of death:
#test <- filter(dat, treatment_ID==2)
#(plant_ages <- test$plant_age)
#(states <- test$state)
death_age <- function(plant_ages, states){ # x is the vector of sample dates
  dead_mark <- which(states=="dead")
  if(length(dead_mark)>0){
    death_age <- as.numeric(plant_ages[dead_mark]) # find the planting date (earliest date)
    death_age <- min(death_age) #in case there are multiple recorded days after death
  } else {
    death_age <- NA
  }
  return(death_age)
}
#death_age(plant_ages, states)


### Function to label as monitored
monitor_lab <- function(death_age, date){
  max_date <- max(date)
  today <- Sys.Date()
  
  # T or F: did more than 365 days go by since last monitoring?
  year_old <- any(max_date < today-days(365))
  
  # T or F: is the plant still alive?
  alive <- any(is.na(death_age))
  
  # if both alive and a year old
  if(year_old & alive){
    return("not_monitoring")
  } else {
    return("monitoring")
  }
}


### Function to extract cumulative growth (essentially the same as the plant_age function):
cum_growth <- function(height,age){ # x is the vector of sample dates
  initial_ht <- min(height) # find the initial height
  cum_growth <- height-initial_ht # subtract the initial height from each other height
  return(cum_growth)
}

### Function to calculate the number of days since each last observation:
days_btw_observ <- function(plant_ages){
  days_btw <- NULL
  for(i in order(plant_ages)){ # loop through each plant age in order
    if(i == 1) days_btw[i] <-  0 # for the first one (age == 0), days between observations is 0
    else days_btw[i] <- plant_ages[i] - plant_ages[i-1]
  }
  return(days_btw)
}
#test <- dat[dat$treatment_ID==67,]
#plant_ages <- test$plant_age
#days_btw_observ(plant_ages)

### Function that runs through each individual to check whether for each 
### individual (treatment_ID) to test if there is any increase in height 
### greater than 1000%--to flag these as possible mistakes. Greater than
### 1000% because that would be the result of moving the decimal place
### once by accident.
outlier_heightgrowth <- function(height, plant_ages){
  perc_increase <- NULL
  for(i in order(plant_ages)){ # loop through each plant age in order
    if(i == 1) perc_increase[i] <-  0 # for the first observation (age == 0), percent growth is zero
    else perc_increase[i] <- (height[i]/height[i-1])*100
  }
  return(perc_increase)
}
#test <- dat[dat$treatment_ID==7457,]
#outlier_heightgrowth(height=test$height_cm, plant_ages=test$plant_age)

### Function to loop through each plant state and see if there is a non-muerto state after a muerto state
### to check for mistakes:
wrong_state <- function(state, plant_ages){
  state_error <- "good"
  for(i in order(plant_ages)[-which(order(plant_ages)==1)]){ # loop through each plant age in order (removing the initial)
    if(state[i-1]=="dead" & state[i]!="dead"){
      state_error[1:length(plant_ages)] <- "CHECK"
      break
    } else state_error[i] <- "good"
  }
  return(state_error)
}
#test <- dat[dat$treatment_ID==3504,]
#wrong_state(state=test$state, plant_ages=test$plant_age)

### Function to check to see if each treatment_ID (code) has only one species, site, and altitude:
check_spp_site_alt <- function(species, site, altitude){
  if(length(unique(species))>1) spp <- "check_spp" else spp <- "bueno"
  if(length(unique(site))>1) sit <- "check_site" else sit <- "bueno"
  if(length(unique(altitude))>1) alt <- "check_altitude" else alt <- "bueno"
  return(paste(spp,sit,alt,sep=", "))
}
#check_spp_site_alt(dat$species_full, dat$site, alt=dat$altitude)

### Function to add comparison lines for grouping singificance between treatments:
compar_line <- function(height, text_height = (0.025*height), bargraph, bars=c(1,2), signif="*", bracket=0.005,...){
  arrows(x0=bargraph[bars[1]], y0 = height, x1 = bargraph[bars[2]], y1 = height, code = 0, xpd = TRUE, ...)
  arrows(x0=bargraph[bars[1]], y0 = height, x1 = bargraph[bars[1]], y1 = height-bracket, code = 0, xpd = TRUE, ...)
  arrows(x0=bargraph[bars[2]], y0 = height, x1 = bargraph[bars[2]], y1 = height-bracket, code = 0, xpd = TRUE, ...)
  middle <- mean(c(bargraph[bars[1]],bargraph[bars[2]]))
  text(x=middle, y=height+text_height, signif, ...)
}



# posteriors=posteriors_all_trans
# heading="all sites"
# spec=c(1.8,1.1,.8,.4)
# ylab="Mida d'efectes posteriors"
# extra_space=.3
# lab_dist = .2
# extra_min = 0

#treatments_vector ## for color labeling the posteriors based on treatment
#groups_vector ## for grouping the posteriors based on grouping variable (site in this case)

# colors: control = "gray66", Groasis = "chartreuse3", hidrogel = "royalblue3", cocoon = "goldenrod4"
### Function to plot posterior distributions nicely
post.plot <- function(exp=T, treatments_vector=F, groups_vector=F,
                      ylabel=expression(bold("Relative effect on 1-year survival")), 
                      posteriors, heading,spec=c(2,1,.6,.4), ylab, extra_space=0.3, 
                      simple=T, lab_dist=20, extra_min = 0){
  
  if(treatments_vector==F & groups_vector==F){
    placement <- c(1:ncol(posteriors))
    label_placement <- c(1:ncol(posteriors))
    trt_col <- rep("black",ncol(posteriors))
    groups_vector <- names(posteriors)
  } else {
    # colors:
    trt_col <- recode(treatments_vector,control="gray66", groasis="chartreuse3", cocoon="goldenrod4", hidrogel = "royalblue3")
    treatments_num <- as.numeric(as.factor(treatments_vector))
    groups_num <- as.numeric(as.factor(groups_vector))
    counter <- 1
    for(g in 1:(length(groups_vector)-1)){
      if(g==1) placement <- counter
      group1 <- groups_vector[g]
      groupnext <- groups_vector[g+1]
      if(group1==groupnext) counter <- counter + .2 #if the next group is the same advance by .5
      if(group1!=groupnext) counter <- counter + .7 #if the next group is different, add a space
      placement <- c(placement, counter)
    }
    label_placement <- tapply(placement, INDEX=groups_vector, FUN=mean)
  }
  
  par(mai=spec, lwd=2)
  
  ##if exp=T, then plot axes as percentages
  boxplot(ylim=c(min_max_95(posteriors)[1]-extra_space-extra_min,min_max_95(posteriors)[2]+extra_space),
          posteriors, border = "white", cex.axis=1.2, horizontal=F,
          las=2, ylab=ylabel, font.axis=2,
          names=F,
          xaxt = "n",
          yaxt = "n",
          at=placement,
          xlim=c(min(placement)-.5,max(placement)+.5))
  
  # if(exp) axis(2,las=1,cex.axis=0.9,at=c(seq(-10,10,1),.5,-.5),labels=c(seq(-10,10,1),.5,-.5)*100, font=2,lwd=2)
  # if(!exp==T) axis(2,las=1,cex.axis=0.9,at=c(seq(-10,10,1),.5,-.5), font=2)
  # 
  if(exp) axis(2,las=1,cex.axis=0.9,at=c(seq(-20,20,2)),labels=c(seq(-20,20,2))*100, font=2,lwd=2)
  if(!exp==T) axis(2,las=1,cex.axis=0.9,at=c(seq(-20,20,2)), font=2)
  
  title(heading,adj=0, cex.main=1.5)
  abline(h=0, lwd=1.5)
  
  text(label_placement, par("usr")[3] - lab_dist, srt = 45, adj = .94, cex=1, font=2, labels = unique(groups_vector), xpd = TRUE)
  
  if(treatments_vector==T & groups_vector==T){
    legend(adj=.1,"topright",legend=unique(treatments_vector), 
           col=unique(trt_col), lwd=4, bty="n", pch=16, pt.cex=1.5, inset=.02,
           text.font=2, text.col=unique(trt_col))
  }
  
  
  
  if(simple==T){ ## If simple == T, then plot simple plots on just the 95% credible interval
    interv <- list(NULL)
    for(i in 1:length(names(posteriors))){
      # calculate the intervals of each posterior
      interv <- c(as.numeric(hdi(posteriors[,i],0.95)),as.numeric(hdi(posteriors[,i],0.8)))
      color <- trt_col[i]
      font.b <- 2
      # label each factor
      #mtext(names(posteriors)[i], side=1, line=1, at=i, las=2, font=font.b, col=color)
      #plot the median of each posterior
      points(x=placement[i], y=median(pull(posteriors,i)), cex=1.2, col=color, pch=16) # pull() function uses the same as indexing a dataframe to extract vector from tibble
      # plot the arrows for each posterior
      arrows(x0 = placement[i], y0 = interv[1], x1 = placement[i], y1 = interv[2], angle = 90,
             length = .05, lwd = 3, col=color, code=3)
    }
  } else {
    interv <- list(NULL)
    for(i in 1:length(names(posteriors))){
      # calculate the intervals of each posterior
      interv <- c(as.numeric(hdi(posteriors[,i],0.95)),as.numeric(hdi(posteriors[,i],0.8)))
      color <- "black"
      color2 <- t_col("black",50)
      font.b <- 2
      # label each factor
      #mtext(names(posteriors)[i], side=1, line=1, at=i, las=2, font=font.b, col=color)
      #plot the median of each posterior
      points(x=i, y=median(pull(posteriors,i)), cex=1.2, col=color, pch=16) # pull() function uses the same as indexing a dataframe to extract vector from tibble
      # plot the arrows for each posterior
      arrows(x0 = i, y0 = interv[1], x1 = i, y1 = interv[2], angle = 90,
             length = 0, lwd = 2, col=color2)
      arrows(x0 = i, y0 = interv[3], x1 = i, y1 = interv[4], angle = 90,
             length = 0, lwd = 4, col=color)
    }
  }
  par(lwd=1)
  return(placement)
}

### Function to extract mean waterbalance between two dates:
climate_btwn <- function(two_dates, climate_data, clim_variable){
  first.dates <- pull(two_dates, date1) #current date
  second.dates <- pull(two_dates, date2)  #previous date
  waterball_btwn <- NULL
  for(i in 1:nrow(two_dates)){
    if(i==1) date.seq <- NA else date.seq <- seq(as.Date(first.dates)[i], as.Date(second.dates)[i], by="days") #all dates in between
    climate_data_sub <- filter(climate_data, date %in% date.seq)
    variable <- select(climate_data_sub, clim_variable)
    waterball_btwn[i] <- ifelse(nrow(climate_data_sub)>0, mean(climate_data_sub$WaterBalance, na.rm=T), NA)
  }
  return(waterball_btwn)
}

### Function to extract prev and cur date for each date:
#dates_all <- all_dat$date[1:20]
btwn_dates <- function(dates_all){
  date1 <- lag(dates_all,1)
  date2 <- dates_all
  return(tibble(date1,date2))
}

### Loop to assign each date a mean climate variable based on the values 
### between the current and previous sampling dates
# waterbalance_btwn <- NA
# for(i in unique(all_dat$treatment_ID)){
#   indiv <- all_dat[all_dat$treatment_ID==i,] # Pull out each individual
#   indiv <- indiv[order(indiv$date),] # sort its observations by date
#   two_dates <- btwn_dates(indiv$date)
#   wb <- climate_btwn(two_dates=two_dates, climate_data=all_clim_dat, clim_variable='WaterBalance')
#   waterbalance_btwn <- c(waterbalance_btwn,wb)
# }

### Function to calculate the relative proportion of values in a vector:
proportion <- function(x) x/sum(x)

