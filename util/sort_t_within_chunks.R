sort_t_within_chunks <- function(df, data_ranges) {
  for (i in seq_along(data_ranges$start_ranges)) {

    # extract sub df
    dft <- df[data_ranges$start_ranges[i]:data_ranges$end_ranges[i],]

    # sort by t
    dft <- dft[order(dft$t),]

    # insert sorted temp df at full df
    df[data_ranges$start_ranges[i]:data_ranges$end_ranges[i],] <- dft
  }
  return(df)
}
