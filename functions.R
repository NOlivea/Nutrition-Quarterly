water <- read_sf(here( 'data','Shape files 2020','UGA_wat', 'UGA_water_areas_dcw.shp'))
districts_shp <- read_sf(here("data" ,"Shape files 2020","new_shapes", "uganda_districts.geojson"))
regional_shp <- read_sf(here('data' ,'Shape files 2020','Region','UDHS Regions 2019.shp'))



change_to_date <- function(df){
  df %>% rowwise() %>%
    mutate(
      year = as.character(substr(Period, start = 1, stop = 4)),
      month = as.numeric(substr(Period, start = 5, stop = 6)),
      date = as.Date( paste( year, month, 1, sep="-"),"%Y-%m-%d" )
     
    )
}


######Sparkline################

spark_func <- function(df, orgunit, title = NULL, 
                       orgunit_label = NULL) {
  df %>% 
    gt() %>% 
    cols_label(
      {{ orgunit }}:= orgunit_label
    ) %>%
    gt_theme_nytimes() %>% 
    tab_header(title = title) %>%
    gt_plt_sparkline(Trend, same_limit = FALSE) %>%
    fmt_number(columns = 2:5, use_seps = TRUE, decimals = 0)
}


# Function to replace NaN, 0, and handle division by zero recursively in a list
replace_nan_with_zero <- function(lst) {
  for (i in seq_along(lst)) {
    if (is.numeric(lst[[i]])) {
      lst[[i]][is.nan(lst[[i]]) | lst[[i]] == 0] <- 0.0
      # Handling division by zero
      lst[[i]][is.infinite(lst[[i]])] <- 0.0
    } else if (is.list(lst[[i]])) {
      lst[[i]] <- replace_nan_with_zero(lst[[i]])
    }
  }
  return(lst)
}


##############Table function

kable_table_fun <- function(data) {
  data |>
    kbl(format = "html") |>
    kableExtra::column_spec(1, bold = TRUE) |>
    kableExtra::row_spec(0, bold = TRUE) |>
    kable_styling("striped", full_width = F)
}


###create sex bar

dodged_bar <- function(data, age_col, value_col, fill_col) {
  data <- data |> 
    mutate(!!age_col := reorder(!!sym(age_col), !!sym(value_col), .desc = TRUE)) # Reorder the age_col based on value_col in descending order
  
  ggplot(data, aes_string(x = age_col, y = value_col, fill = fill_col)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes_string(label = paste0("format(", value_col, ", big.mark = \",\")")), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5, color = "black", size = 5) +
    ylim(0, max(data[[value_col]]) * 1.1) +
    theme_minimal() +
    scale_fill_manual(values = c("Female" = "#8AB8D0", "Male" = "#464E47")) +
    theme(legend.position = "top",
          legend.title = element_blank())
}








#############sungle bar graph
region_bar_fun <- function(data, varx, vary) {
  data[[varx]] <- factor(data[[varx]], levels = data[[varx]][order(data[[vary]])])
  
  data |>
    ggplot(aes_string(x = varx, y = vary, label = vary)) +
    geom_col(fill = "#1D1D70") +
    geom_text(position = position_stack(vjust = 1.01), 
              hjust = 0.01,  # Adjust hjust to position labels just outside the bars
              color = "black", size = 4.8,fontface = "bold", aes(label = scales::comma(..y..))) +  # Add value labels on top of bars
    theme_classic() +
    labs(caption = 'Data source: DHIS2') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(data[[vary]]) * 1.1)) +  # Adjust y-axis limits
    theme(
      axis.text = element_text(color = "black", size = 7),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 10),
      legend.position = 'bottom',
      legend.title = element_blank()
    ) +
    coord_flip()
}


############################################################
color_mt = c("2021" = "#BFCDD9FF",  "2022" = "#484860FF", "2023" = "#EBA07EFF", "2024" = "#BE3428FF")


multiple_line_fucn <- function(data) {
  data |>
    ggplot(aes(x = month_abbrev))+
    geom_line(aes(y = `2021`, color = "2021"), group = 1, size = 0.8)+
    geom_point(aes(y = `2021`),size = 2, color = 'black')+
    geom_line(aes(y = `2022`, color = "2022"), group = 1, size = 0.8)+
    geom_point(aes(y = `2022`),size = 2, color = 'black')+
    geom_line(aes(y = `2023`, color = "2023"), group = 1, size = 1.2)+
    geom_point(aes(y = `2023`),size = 2, color = 'black')+
    geom_line(aes(y = `2024`, color = "2024"), group = 1, linewidth = 1.5)+
    geom_point(aes(y = `2024`),size = 2, color = 'black')+
    theme_minimal()+
    scale_color_manual('Years' ,values = color_mt)+
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(hjust = 1, face = 'bold')) 
  }

############adding a row total for last4month table################
add_ttal <- function(data){
  data |>
    summarise(across(where(is.numeric), sum)) |>
    mutate(Conditions = "Total")
} 
 
##############################################################################

#############removing outlirs##################

replace_outliers_with_median <- function(data_vector) {
  # Calculate quartiles
  Q1 <- quantile(data_vector, 0.25)
  Q3 <- quantile(data_vector, 0.75)
  
  # Calculate IQR
  IQR <- Q3 - Q1
  
  # Define outlier thresholds
  lower_threshold <- Q1 - 1.5 * IQR
  upper_threshold <- Q3 + 1.5 * IQR
  
  # Replace outliers with median
  data_vector[data_vector < lower_threshold | data_vector > upper_threshold] <- median(data_vector)
  
  # Return the modified vector
  return(data_vector)
}


replace_outliers_in_dataframe <- function(df) {
  df |> 
    mutate(across(where(is.numeric), replace_outliers_with_median))
}

# # Example usage
# # Assuming 'your_data_frame' is your input data frame
# your_data_frame <- replace_outliers_in_dataframe(your_data_frame)
# 


# Define a custom theme with bold facet labels
custom_theme <- function() {
  theme_bw() +
    theme(legend.position = "bottom",
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),  # Remove panel borders
          strip.background = element_blank(),
          strip.text = element_text(face = "bold"))  # Set the facet label text to bold
}

custom_theme1 <- function() {
  theme_bw() +
    theme(legend.position = "right",
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),  # Remove panel borders
          strip.background = element_blank(),
          strip.text = element_text(face = "bold"))  # Set the facet label text to bold
}
# Create the plot with the custom theme

draw_map <- function(data,haz) {
  ggplot() +
    geom_sf(data = data, aes_string(fill = haz), color = "#828A95",linewidth = 0.0001 ) +
    geom_sf(data = water, fill = '#a6cee3',color = "#a6cee3" ) +
    facet_wrap(~month_year, nrow = 1)
}

draw_map2 <- function(data,haz) {
  ggplot() +
    geom_sf(data = data, aes_string(fill = haz), color = "#828A95",linewidth = 0.0001 ) +
    geom_sf(data = water, fill = '#a6cee3',color = "#a6cee3" ) 
  }


# draw_map <- function(data,haz) {
#   ggplot() +
#     geom_sf(data = data, aes(fill = factor(haz))) +
#     geom_sf(data = water, fill = '#a6cee3') +
#     labs(caption = 'Data Source: DHIS2') +
#     facet_wrap(~month_year, nrow = 1)
# }


trend <- function(data,vary, varx) {
  data |> 
    ggplot(aes_string(x = varx, y = vary, group = 1)) +
    geom_line(color = "#6B464D", size = 1.5) +
    geom_point(color = "black") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(data[[vary]]) * 1.1)) +
    labs(x = "Years", y = "Incedence") +
    theme_minimal() +
    theme(axis.text.x = element_text( hjust = 1))
}



# Regional bar graph with national bar ------------------------------------


myGraph <- function(df, varx){
  
  p <- df |> 
    mutate(
      
      level = factor(level),
      region = factor(region, levels = df$region[order(df$level, df[[varx]], decreasing = FALSE)])
      
    ) |>
    
    ggplot(aes(region, !!sym(varx), fill = level)) +
    geom_col(show.legend = F) +
    geom_text(position = position_stack(vjust = 1.01), hjust = 0.01, 
              color = "black", size = 4.8,fontface = "bold", aes(label = !!sym(varx)))  +
    theme_minimal() +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(df[[varx]]) * 1.1)) +  # Adjust y-axis limits
    theme(axis.text = element_text(color = "black", size = 7),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 10),
          legend.position = 'bottom',
          legend.title = element_blank()) +
    labs(x = "", y = "Incedence") +
    coord_flip()
  
  return(p)
  
}

# Create a dummy plot to get the default colors
get_default_colors <- function(levels) {
  p <- ggplot(data.frame(x = 1:2, level = factor(levels)), aes(x, fill = level)) +
    geom_bar()
  default_colors <- ggplot_build(p)$data[[1]]$fill
  names(default_colors) <- levels
  return(default_colors)
}

# Define the plotting function
myGraph1 <- function(df, varx) {
  # Get unique levels
  levels <- unique(df$level)
  
  # Get the default colors
  default_colors <- get_default_colors(levels)
  
  # Swap the colors
  swapped_colors <- default_colors[rev(levels)]
  names(swapped_colors) <- levels
  
  # Create the plot with swapped colors
  p <- df |> 
    mutate(
      level = factor(level),
      region = factor(region, levels = df$region[order(df$level, df[[varx]], decreasing = FALSE)])
    ) |>
    ggplot(aes(region, !!sym(varx), fill = level)) +
    geom_col(show.legend = FALSE) +
    geom_text(position = position_stack(vjust = 1.01), hjust = 0.01, 
              color = "black", size = 4.8, fontface = "bold", aes(label = !!sym(varx))) +
    theme_minimal() +
    labs(caption = 'Data source: DWH') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(df[[varx]]) * 1.1)) +
    theme(axis.text = element_text(color = "black", size = 7),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 10),
          legend.position = 'bottom',
          legend.title = element_blank()) +
    labs(x = "", y = "Incidence") +
    coord_flip() +
    scale_fill_manual(values = swapped_colors)
  
  return(p)
}


single_bar_fun <- function(data,varx,vary){
  data %>%
    ggplot(aes_string(x = varx, y = vary)) +
    geom_col(fill = "darkgreen") +
    theme_classic() +
    scale_y_continuous(expand = c(0,0))+
    theme(
      axis.text = element_text(color = "black", size = 10, face = "bold"),
      legend.text = element_text(color = "black", size = 10, face = "bold"),
      axis.text.x = element_text(face = "bold", size = 10),
      axis.text.y = element_text(face = "bold", size = 9),
      legend.position = 'bottom',
      legend.title = element_blank()
    )
}

###########################################################################################
age_pyramid <- function(data, age_col, value_col, sex_col) {
  # Adjust the data to make the counts for males negative (for pyramid effect)
  data <- data |> 
    mutate(!!value_col := if_else(!!sym(sex_col) == "Male", -!!sym(value_col), !!sym(value_col)))
  ggplot(data, aes_string(x = age_col, y = value_col, fill = sex_col)) +
    geom_bar(stat = "identity") +
    geom_text(aes_string(label = paste0("abs(", value_col, ")")), 
              position = position_dodge(width = 0.9),  # Adjust for dodge position
              hjust = if_else(data[[sex_col]] == "Male", 1.1, -0.1),  # Position labels outside the bars
              color = "black", size = 5) +
    coord_flip() +  # Flip coordinates to create the pyramid shape
    scale_y_continuous(labels = abs, limits = c(-max(abs(data[[value_col]])) * 1.1, max(data[[value_col]]) * 1.1)) +  # Expand limits
    scale_fill_manual(values = c("Female" = "#8AB8D0", "Male" = "#464E47"), 
                      breaks = c("Female", "Male"), 
                      labels = c("Female", "Male"), 
                      guide = guide_legend(reverse = TRUE)) +  # Reverse legend order
    theme_minimal() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.text.y = element_text(size = 12), 
          axis.text.x = element_text(size = 12))
}

replace_outliers_with_na <- function(data_vector) {
  # Calculate quartiles
  Q1 <- quantile(data_vector, 0.25, na.rm = TRUE)
  Q3 <- quantile(data_vector, 0.75, na.rm = TRUE)
  
  # Calculate IQR
  IQR <- Q3 - Q1
  
  # Define outlier thresholds
  lower_threshold <- Q1 - 1.5 * IQR
  upper_threshold <- Q3 + 1.5 * IQR
  
  # Replace outliers with NA
  data_vector[data_vector < lower_threshold | data_vector > upper_threshold] <- NA
  
  # Return the modified vector
  return(data_vector)
}

interpolate_missing_values <- function(data_vector) {
  # Perform interpolation to fill NA values
  imputed_vector <- zoo::na.approx(data_vector, na.rm = FALSE)
  
  # Return the interpolated vector
  return(imputed_vector)
}

interpolate_missing_values <- function(data_vector) {
  # Perform interpolation to fill NA values
  imputed_vector <- zoo::na.approx(data_vector, na.rm = FALSE)
  
  # Return the interpolated vector
  return(imputed_vector)
}

library(zoo)

myRemover <- function(df, column_name) {
  df[[column_name]] <- df[[column_name]] |>
    replace_outliers_with_na() |>
    interpolate_missing_values()
  
  return(df)
}

