################################################################################
## Main function
################################################################################
start_of_flowering <- function(df, species_list = unique(df$soortnaam), min_obs = 50, min_years = 5, rm_EP = FALSE, plot_per_year = FALSE, kernel = "gaussian" , bw = 15, flowering_threshold = 0.5){

#Set up result df
result_df <- data.frame()

# Initializeprogress bar
pb <- txtProgressBar(min = 0, max = length(species_list), style = 3)
count = 0
count_ney_species = 0 #Count species with not enough years

#Create custom labels for plotting
custom_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "")

#Filter dataframe
df %>% 
  filter(lifestage == "bloeiend") %>% #Select only flowering species
  filter(soortnaam %in% species_list) -> df

#MAIN LOOP
for (soort in unique(species_list)) {
  
  ################################################################################
  ## Wrangling dataset
  ################################################################################
  #Select one species
  df %>% filter(soortnaam == soort) -> df_loop
  
  #Calculate amount of years with more than min_obs observations
  df_loop %>% 
    group_by(jaar) %>% 
    count(soortnaam) %>% 
    filter(n > min_obs) %>% 
    nrow(.) -> aantal_jaren
  
  #Skip species with less than min_years years of data
  if (aantal_jaren < min_years) {
    #Count number of species without enough years
    count_ney_species = count_ney_species + 1
    #Count and print number of species done
    count = count + 1
    #Update the progress bar
    setTxtProgressBar(pb, count)
    next()
  }
  
  if (rm_EP == TRUE){
  #Remove 'Eindejaars plantenjacht'
  df_loop %>% 
    mutate(doy = yday(as.Date(ymd_hms(timestart)))) %>% 
    filter(doy > 5, doy <350) -> df_loop
  }
  
  ################################################################################
  ## Calculate PI and statistics
  ################################################################################
  
  #Set up start of flowering vector
  start_of_flowering <- c()
  
  #Create empty data frame for the PI
  density <- data.frame(PI = numeric(365),
                        doy = numeric(365),
                        year = numeric(365))
  
  #Create empty data frame to combine PI for all years combined
  density_df <- data.frame()
  
  #Calculate summary statistics per year
  df_loop %>% 
    group_by(jaar) %>% 
    summarize(mean = round(mean(doy),digits=0), sd = sd(doy), observations = n(), species = soort) %>% 
    filter(observations >= min_obs) -> meandoy_df #Remove years with less than 50 observations
  
  years <- unique(meandoy_df$jaar)
  
  for (i in seq_along(years)){ #Iterate over all years
    #Filter data to include only one year
    df_year <- df_loop %>% filter(jaar == years[i])
    
    #Calculate amount of observations per day
    result_table <- as.data.frame(table(df_year$doy))
    
    #Create a table with all doys
    doy_table <- data.frame(Var1 = 1:365, Freq = rep(0, 365))
    
    #Combine observations with doy table
    complete_table <- merge(doy_table, result_table, by = "Var1",all.x = TRUE)
    
    # Replace missing values with 0
    complete_table$Freq.y[is.na(complete_table$Freq.y)] <- 0
    complete_table <- complete_table[,-2] #remove extra column
    
    #Calculate PI per day using Kernel Density Estimation (KDE)
    p <- suppressWarnings( #Ignore warning because weights dont add up to 1
      density(1:365, weights = complete_table$Freq.y, from = 1, to = 365, bw = bw, n = 365, kernel = kernel))
    
    #Fill in density data frame with PI
    density$PI <- p$y
    density$PI <- density$PI / max(density$PI)
    density$year <- years[i]
    density$doy <- 1:365
    
    #Combine PI for all years
    density_df <- rbind(density_df, density)
    
    #Calculate start of flowering (25%)
    threshold <- max(p$y) * flowering_threshold
    start_of_flowering[i] <- which(p$y >= threshold)[1]
    
    if (plot_per_year == TRUE) {
    #Plot results per year
    plot(p, main = paste0(soort, ":", years[i]), col = )
    abline(v = start_of_flowering[i], col = "red") #Add start of flowering
    }
  } #Year loop
  
  #Count and print number of species done
  count = count + 1
  #Update the progress bar
  setTxtProgressBar(pb, count)
  
  #Plot results per species for all years combined
  o <- ggplot(density_df, aes(x = doy)) +
    geom_line(aes(y = PI, color = as.factor(year))) +
    ggtitle(soort) +
    scale_x_continuous(breaks = seq(1, 365, by = 30),
                       labels = custom_labels)
  
  print(o)
  
  #Add start of flowering per year to df and convert it to date
  meandoy_df %>% 
    mutate(start_of_flowering = start_of_flowering) %>% 
    mutate(date = as.Date(start_of_flowering-1, origin = as.Date(ISOdate(jaar, 1, 1)))) -> meandoy_df
  
  #Combine all results in result df
  result_df <- rbind(result_df, meandoy_df)
  
} #Species loop

cat("\n")
cat(paste0(count_ney_species," species did not have ", min_years, " years with more than ", min_obs, " observations"))
return(result_df)
}
