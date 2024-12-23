currdate <- Sys.Date()
currmonth <- month(currdate-1)
curryear <- year(currdate)
month_abbr <- month.abb[currmonth]

quarter_ranges <- list(
  Q1 = c("Jan", "Mar"),
  Q2 = c("Apr", "Jun"),
  Q3 = c("Jul", "Sep"),
  Q4 = c("Oct", "Dec")
)

# Calculate the current quarter based on the month
prevquarter <- if (currmonth %in% 1:3) {
  paste0(quarter_ranges$Q4[1], " to ", quarter_ranges$Q4[2], " ", curryear - 1)
} else if (currmonth %in% 4:6) {
  paste0(quarter_ranges$Q1[1], " to ", quarter_ranges$Q1[2], " ", curryear)
} else if (currmonth %in% 7:9) {
  paste0(quarter_ranges$Q2[1], " to ", quarter_ranges$Q2[2], " ", curryear)
} else {
  paste0(quarter_ranges$Q3[1], " to ", quarter_ranges$Q3[2], " ", curryear)
}


# Function to get the quarter range
get_quarter_range <- function(quarter) {
  if (quarter == 1) {
    return(paste0(quarter_ranges$Q1[1], " to ", quarter_ranges$Q1[2]))
  } else if (quarter == 2) {
    return(paste0(quarter_ranges$Q2[1], " to ", quarter_ranges$Q2[2]))
  } else if (quarter == 3) {
    return(paste0(quarter_ranges$Q3[1], " to ", quarter_ranges$Q3[2]))
  } else {
    return(paste0(quarter_ranges$Q4[1], " to ", quarter_ranges$Q4[2]))
  }
}

# Current year and quarter
curr_year <- curryear
curr_quarter <- ceiling(currmonth / 3)

# Generate the last 2 quarters
last2quarters <- vector("list", 2)

for (i in 1:2) {
  prev_quarter <- curr_quarter - i
  prev_year <- curr_year
  
  # Adjust the year and quarter if the quarter goes below 1
  if (prev_quarter <= 0) {
    prev_quarter <- prev_quarter + 4
    prev_year <- prev_year - 1
  }
  
  # Get the range for the quarter
  last2quarters[[i]] <- paste(get_quarter_range(prev_quarter), prev_year)
}

# Reverse the order to display the most recent quarter first
last2quarters <- rev(last2quarters)

vita_sam <- nutrition_joined |>
  filter(Category %in% c("24-59Mths, Female","6-23Mths, Male","24-59Mths, Male","6-23Mths, Female")) 
  group_by(quarter, district) |> 
  summarise(VitA1                                      = sum(VitA1, na.rm = T),
            VitA2                                      = sum(VitA2, na.rm = T),
            SAMcured                                   = sum(SAMcured, na.rm = T),
            SAM_died_OTC                               = sum(SAM_died_OTC, na.rm = T),
            SAM_succesfullytreated_ITC                 = sum(SAM_succesfullytreated_ITC, na.rm = T),
            SAM_died_ITC                               = sum(SAM_died_ITC, na.rm = T),
            Total                                      = sum(Total,na.rm = T)) |> 
  arrange(desc(VitA1))
  mutate(vitA     = sum(VitA1, VitA2, na.rm = T),
         VitA_coverage  = round((vitA/Total*0.173)*100))
