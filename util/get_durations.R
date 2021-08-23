get_durations_for_condition <- function(df, condition_length, start_end_index_list, aoicol, limit_onset_length_in_ms = 0, get_last_ms_to_video_end = 0) {

  durations <- list(left = c(), right = c(), screen = c())

  for (i in 1:condition_length) {

    current_start_index <- start_end_index_list$start[i]
    current_endofvideo_index <- start_end_index_list$end[i]

    # if get_last_ms_to_video_end is given overwrite current_start_index
    if (!missing(get_last_ms_to_video_end)) {
      current_t_minus_offset <- df$t[current_start_index] - get_last_ms_to_video_end
      current_start_index <- which(df$t < current_t_minus_offset) %>% max()
    }

    # overwrite current_endofvideo_index if a limit_onset_length is given
    # track index_start time and add limit_onset_length_in_ms to make sure we just capture the video duration time
    if (!missing(limit_onset_length_in_ms)) {
      current_t_plus_limit <- df$t[current_start_index] + limit_onset_length_in_ms
      current_endofvideo_index <- which(df$t < current_t_plus_limit) %>% max()
    }

    durations_within_current_range <-
      df$GazeEventDuration[current_start_index:current_endofvideo_index]


    # check if AOIs are boolean, if so skip the rest of the loop, as this will evaluate left/right



    left_pos_within_current_range <-
      which(df[[aoicol]][current_start_index:current_endofvideo_index] == "left")

    # sum up store durations within current preflook range that are "left"
    durations$left <- c(durations$left, durations_within_current_range[left_pos_within_current_range] %>% sum())

    # for right (recycling durations_within_current_range)
    right_pos_within_current_range <-
      which(df[[aoicol]][current_start_index:current_endofvideo_index] == "right")

    durations$right <- c(durations$right, durations_within_current_range[right_pos_within_current_range] %>% sum())
  }

  return(durations)
}
