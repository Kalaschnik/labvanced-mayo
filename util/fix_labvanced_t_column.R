#' Fixes Labvanced t Column
#'
#' @description
#' The function checks a Labvanced dataset for missing t values. If t values are missing
#' the function will fill those using multiple methods:
#' 1. Calculate a median delay value of the time difference for all "times" and "t" values
#' 2. Use that median delay if the dataset has no t value at indexes: `df$t[1]` and `df$t[length(df)$t]`.
#' 3. Sort by times column, to get a good estimate about experiment procedure, and to keep NA rows in place
#' 4. Sort chunk-wise t column (chunks are sequential t values surrounded by NA rows)
#' 5. In rare occasions, later chunks may contain t values smaller than in former chunks, if that is the case, a row wise bubble sort is performed
#' 6. When t-column chunks are sorted, interpolate t values where t == NA
#' 7. If there were missing values for start and end index, interpolate from start to first valid chunk and likewise for end
#'
#' @param df Needs an (unmodified) labvanced dataframe (including a "times" and a "t" column)
#' @return Returns a df without any NA in the t column
#' @examples
#' fix_labvanced_t_column(df)
fix_labvanced_t_column <- function(df, dev_mode = FALSE) {

  # For easy string interpolation
  if (!require(stringr)) install.packages('stringr')
  library(stringr)

  # for solitary value treatment (lag and lead)
  if (!require(dplyr)) install.packages('dplyr')
  library(dplyr)


  # check if there is a "t" and a "times" column
  if (!"t" %in% colnames(df) &&
      !"times" %in% colnames(df) &&
      !"Task_Nr" %in% colnames(df))
    stop("Missing 't and/or 'times' and/or Task_Nr column")

  # check if df has no prior sorting
  if (is.unsorted(as.integer(row.names(df)))) {
    stop("The provided was somewhat sorted. Provide a raw/unsorted df.")
  }

  # MEDIAN DELAY
  # Calculate a median delay value of the time difference for all "times" and "t" values
  # add a diff column into df
  if (dev_mode) {
    print("Add Diff Column")
    df$times_t <- df$times - df$t
  }
  # show overview
  if (dev_mode) summary(df$times_t)

  median_delay <- round(median(df$times - df$t, na.rm = TRUE))


  # SORT BY TIMES
  df <- df[order(df$times), ]


  # Check if Task_Nr is now be in order
  if (is.unsorted(df$Task_Nr)) stop("Task_Nr is not sorted, this should no happen!")


  # check if first and last value of t is NA. If so subtract median delay ...
  # ... from this first/last times value and assign it to that t value
  # NB: Since the df is sorted by times we can safely uset the first/last entry...
  # ... which are the earliest/latest times in the df overall

  missing_start <- FALSE
  if (is.na(df$t[1])) {
    df$t[1] <- df$times[1] - median_delay
    missing_start <- TRUE
  }

  missing_end <- FALSE
  if (is.na(df$t[length(df$times)])) {
    df$t[length(df$times)] <- df$times[length(df$times)] - median_delay
    missing_end <- TRUE
  }


  # Get Chunk Stard and End Indexes/Ranges
  # Minimal Example: https://stackoverflow.com/q/68702354/2258480)
  seq <- which(!is.na(df$t))

  dx1 <- c(TRUE, diff(seq) > 1)
  dx2 <- c(diff(seq) > 1, TRUE)

  # store data ranges
  data_ranges <- list(start_ranges = seq[dx1 & !dx2],
                      end_ranges = c(seq[dx2 & !dx1]))


  if (dev_mode) {
    print("Are inner chunks sorted if times is sorted?")
    for (i in seq_along((data_ranges$start_ranges))) {
      print(i)
      print(is.unsorted(df$t[data_ranges$start_ranges[i]:data_ranges$end_ranges[i]]))
    }
  }

  # sort inner t chunks
  df <- sort_t_within_chunks(df, data_ranges)


  # if everything is already in order by t, we are good if not we need to check solitary values
  needs_solitary_sort <- is.unsorted(df$t, na.rm = TRUE)
  if (needs_solitary_sort) {

    # check for single values surrounded by NAs/NaNs
    # those solitary values are not part of data_ranges and may fuck-up the ordering
    # we need to check if they are in place
    solitary_ts <- df$t[!is.na(df$t) & is.na(lag(df$t, default = 1)) & is.na(lead(df$t, default = 1))]
    solitary_ts_indexes <- which(df$t %in% solitary_ts)

    for (i in seq_along(solitary_ts_indexes)) {

      # CHECK CHUNK BEFORE

      # get index of the last t value in the former chunk
      former_chunk_last_t_index <-
        data_ranges$end_ranges[max(which(data_ranges$end_ranges < solitary_ts_indexes[i]))]

      # compare timestamp of former chunk last t with currrent solitary value
      if (df$t[former_chunk_last_t_index] > solitary_ts[i]) {

        # get former chunk start range
        former_chunk_first_t_index <-
          data_ranges$start_ranges[max(which(data_ranges$start_ranges < solitary_ts_indexes[i]))]


        # sanity check if solitary value is even smaller then former start range
        if (solitary_ts[i] < df$t[former_chunk_first_t_index]) stop("Hope this never happens...")

        # swap solitary_ts with former chunk last index
        bucket <- df[solitary_ts_indexes[i],]
        df[solitary_ts_indexes[i],] <- df[former_chunk_last_t_index,]
        df[former_chunk_last_t_index,] <- bucket
      }

      # CHECK CHUNK AFTER

      # get index of the first t value in the later chunk
      later_chunk_first_t_index <-
        data_ranges$start_ranges[min(which(data_ranges$start_ranges > solitary_ts_indexes[i]))]

      # compare timestamp of later chunk first t with currrent solitary value
      if (df$t[later_chunk_first_t_index] < solitary_ts[i]) {

        # get later chunk end range
        later_chunk_last_t_index <-
          data_ranges$end_ranges[min(which(data_ranges$end_ranges > solitary_ts_indexes[i]))]


        # sanity check if solitary value is even smaller then former start range
        if (solitary_ts[i] > df$t[later_chunk_last_t_index]) stop("Hope this never happens...")

        # swap solitary_ts with later chunk first index
        bucket <- df[solitary_ts_indexes[i],]
        df[solitary_ts_indexes[i],] <- df[later_chunk_first_t_index,]
        df[later_chunk_first_t_index,] <- bucket
      }
    }
  }

  # perform inner sorting (chunk-wise) again
  df <- sort_t_within_chunks(df, data_ranges)



  # check if t is already sorted after sort_t_within_chunks, and solitary sort,
  # this is likely the case...yet, it can happen that some rows with multiple values are affected
  needs_bubble_sort <- is.unsorted(df$t, na.rm = TRUE)

  if (needs_bubble_sort) {
    bubble_counter <- 0

    while (needs_bubble_sort) {
      # check which indexes cause unsorted
      for (i in 2:length(data_ranges$start_ranges) - 1) {
        # if (i == 108) browser()
        former_end_index <- data_ranges$end_ranges[i]
        next_start_index <- data_ranges$start_ranges[i + 1]

        if (df$t[former_end_index] > df$t[next_start_index]) {
          warning(str_interp(
            c(
              "Need to bubble sort due to comparing:\n",
              "${df$t[former_end_index]} > ${df$t[next_start_index]} (first should be smaller)\n",
              "At t column index: ",
              "${which(df$t == df$t[former_end_index])} and ${which(df$t == df$t[next_start_index])} ",
              "(or at row name: ${row.names(df[which(df$t == df$t[former_end_index]),])} and ${row.names(df[which(df$t == df$t[next_start_index]),])})\n",
              "At data_range end: ${former_end_index} and start: ${next_start_index}\n",
              "i is at ${i}"
            )
          ))

          # start bubble sort
          bubble_counter <- bubble_counter + 1
          cat("Bubble sort count:", bubble_counter, "\n")

          # bubble sort start
          bucket <- df[former_end_index,]
          df[former_end_index,] <- df[next_start_index,]
          df[next_start_index,] <- bucket
          # bubble sort finish

          # restart sorting / bubble sort
          needs_bubble_sort <- is.unsorted(df$t, na.rm = TRUE)

          if (needs_bubble_sort) {
            # perform inner sorting first before another bubble sort
            df <- sort_t_within_chunks(df, data_ranges)
            # check if we are good now?
            needs_bubble_sort <- is.unsorted(df$t, na.rm = TRUE)
            # # redo current index
            # i <- i - 1

          }
        }
      }
    }
  }

  # (LINEAR) INTERPOLATE NAs IN t COLUMN (TREATING INNER CHUNKS)
  for (i in 2:length(data_ranges$start_ranges) - 1) {
    former_end_index <- data_ranges$end_ranges[i]
    next_start_index <- data_ranges$start_ranges[i + 1]

    NA_gap_diff <- next_start_index - former_end_index

    NA_time_diff <- df$t[next_start_index] - df$t[former_end_index]

    # round, as we want integers
    NA_interpol_value <- round(NA_time_diff / NA_gap_diff)


    NA_start_index <- former_end_index + 1
    NA_end_index <- next_start_index - 1

    # sum up current NA chunk with interpol_value
    gap_index <- 1
    for (i in NA_start_index:NA_end_index) {
      df$t[i] <-
        df$t[former_end_index] + (NA_interpol_value * gap_index)
      gap_index <- gap_index + 1
    }
  }


  # (linear) interpolate from first value (extrapolated by median delay) to first chunk
  # the chunk algorithm doesn’t cover this case, as it is doing only interpolations from two existing points
  # this is only necessary if t[1] == NA
  if (missing_start) {
    NA_start_gap_diff <- data_ranges$start_ranges[1] - 1
    NA_start_time_diff <-
      df$t[data_ranges$start_ranges[1]] - df$t[1]
    NA_start_interpol_value <-
      round(NA_start_time_diff / NA_start_gap_diff)

    NA_s_start_index <- 2
    NA_s_end_index <- data_ranges$start_ranges[1] - 1

    gap_s_index <- 1
    for (i in NA_s_start_index:NA_s_end_index) {
      df$t[i] <-
        df$t[1] + (NA_start_interpol_value * gap_s_index)
      gap_s_index <- gap_s_index + 1
    }
  }


  # same for end
  if (missing_end) {
    NA_end_gap_diff <-
      length(df$t) - (data_ranges$end_ranges[length(data_ranges$end_ranges)])
    NA_end_time_diff <-
      df$t[length(df$t)] - df$t[data_ranges$end_ranges[length(data_ranges$end_ranges)]]
    NA_end_interpol_value <-
      round(NA_end_time_diff / NA_end_gap_diff)

    NA_e_start_index <-
      data_ranges$end_ranges[length(data_ranges$end_ranges)] + 1
    NA_e_end_index <- length(df$t) - 1

    gap_e_index <- 1
    for (i in NA_e_start_index:NA_e_end_index) {
      df$t[i] <-
        df$t[data_ranges$end_ranges[length(data_ranges$end_ranges)]] +
        (NA_end_interpol_value * gap_e_index)
      gap_e_index <- gap_e_index + 1
    }
  }

  return(df)
}
