get_end_indexes <- function(names_vector, start_end_index_list, endofvideo_indexes, update_StudioEventData = TRUE) {

  end_indexes <- c()

  for (i in start_end_index_list$start) {
    current_name <- names_vector[which(start_end_index_list$start == i)]
    end_index_of_current_i <- endofvideo_indexes[min(which(endofvideo_indexes > i))]

    # append end_index_of_current_i for get_aosi
    end_indexes <- c(end_indexes, end_index_of_current_i)

    # assign current_name in StudioEventData with the range of i and end_index_of_current_i
    # "<<-" denotes global assignment within a function call (note that this is not a pure function anymore)
    if (update_StudioEventData) df$StudioEventData[i:end_index_of_current_i] <<- current_name
  }

  return(end_indexes)
}
