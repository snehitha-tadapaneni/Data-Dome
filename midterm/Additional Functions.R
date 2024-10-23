
outlierKD2 <- function(df, var, rm = FALSE, boxplt = FALSE, histogram = TRUE, qqplt = FALSE) {
  dt <- df  # Duplicate the dataframe for potential alteration
  var_name <- eval(substitute(var), eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = TRUE)
  colTotal <- boxplt + histogram + qqplt  # Calculate the total number of charts to be displayed
  par(mfrow = c(2, max(2, colTotal)), oma = c(0, 0, 3, 0))  # Adjust layout for plots
  
  # Q-Q plot with custom title
  if (qqplt) {
    qqnorm(var_name, main="Q-Q plot without Outliers")
    qqline(var_name)
  }
  
  # Histogram with custom title
  if (histogram) { 
    hist(var_name,main = "Histogram without Outliers", xlab = NA, ylab = NA) 
  }
  
  # Box plot with custom title
  if (boxplt) { 
    boxplot(var_name, main= "Box Plot without Outliers")
  }
  
  # Identify outliers
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  
  # Q-Q plot without outliers
  if (qqplt) {
    qqnorm(var_name, main="Q-Q plot with Outliers")
    qqline(var_name)
  }
  
  # Histogram without outliers
  if (histogram) { 
    hist(var_name, main = "Histogram with Outliers", xlab = NA, ylab = NA) 
  }
  
  # Box plot without outliers
  if (boxplt) { 
    boxplot(var_name, main = "Boxplot with Outliers") 
  }
  
  # Add the title for the overall plot section if any plots are displayed
  if (colTotal > 0) {
    title("Outlier Check", outer = TRUE)
    na2 <- sum(is.na(var_name))
    cat("Outliers identified:", na2 - na1, "\n")
    cat("Proportion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name)) * 100, 1), "\n")
    cat("Mean of the outliers:", round(mo, 2), "\n")
    cat("Mean without removing outliers:", round(m1, 2), "\n")
    cat("Mean if we remove outliers:", round(mean(var_name, na.rm = TRUE), 2), "\n")
  }
  
  # Remove outliers if `rm = TRUE`
  if (rm) {
    dt[as.character(substitute(var))] <- invisible(var_name)
    cat("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else {
    cat("Nothing changed", "\n")
    return(invisible(df))
  }
}


---------------------------------------------------------------------------------


  # Function to convert installs to numeric
  convert_to_numeric <- function(x) {
    as.numeric(gsub("[^0-9]", "", x)) * 10^(length(gregexpr(",", x)[[1]]) - 1)
  }

 ---------------------------------------------------------------------------
    # Function to convert size strings to numeric values in MB
  convert_size <- function(size) {
    size <- gsub(",", "", size)  # Remove commas
    size <- tolower(size)  # Make lowercase for consistency
      
      # Handle "varies with device" by assigning NA
    if (size == "varies with device") return(NA)
      
      # Convert "k" to MB (1 MB = 1024 KB)
    if (grepl("k", size)) return(as.numeric(gsub("k", "", size)) / 1024)
      
      # Convert "M" to numeric MB
    if (grepl("m", size)) return(as.numeric(gsub("m", "", size)))
      
      # Handle numeric values directly (e.g., "1000+")
    if (grepl("\\d+\\+", size)) return(as.numeric(gsub("\\+", "", size)) / 1024)
      
      # Default case: return as numeric if possible
    return(as.numeric(size))
    }
    
    
    
    
    
------------------------------------------------------------------------------

extract_version <- function(version) {
  version <- tolower(version)  # Make lowercase for consistency
  
  # Handle "Varies with device" and "NaN"
  if (version == "varies with device" || version == "nan") return(NA)
  
  # Extract the first version in case of ranges (e.g., "4.1 - 7.1.1" -> "4.1")
  first_version <- strsplit(version, "[- ]")[[1]][1]
  
  # Remove "and up" if present (e.g., "4.0 and up" -> "4.0")
  first_version <- gsub("and up", "", first_version)
  
  return(as.numeric(first_version))  # Convert to numeric
}

