find_most_NAs_in_xts_object_columns <- function(data_xts){
  # Count the number of NAs in each column
  na_counts <- apply(is.na(data_xts), 2, sum) 
  
  # Find the column name with the maximum number of NAs
  max_na_column <- names(which.max(na_counts))
  
  # Get the count of NAs in the column with the most NAs
  max_na_count <- na_counts[max_na_column]
  if (as.numeric(max_na_count) > 0){
    return (as.numeric(max_na_count)+5)
  }
  else {
    return (5)
  }
}