get_events_from_column <- function(vec) {
  events <- list(
    Mask_names = grep(".*Mask*.", vec, value = TRUE),
    Mask_indexes = list(
      start = grep(".*Mask*.", vec, value = FALSE),
      end = c() # placeholder for the next step
    ),
    single_mask_events = list(
      Mask_names = grep(".*_Mask_*.", vec, value = TRUE),
      Mask_indexes = list(
        start = grep(".*_Mask_*.", vec, value = FALSE),
        end = c()
      ),
      halfMask_names = grep(".*halfMask*.", vec, value = TRUE),
      halfMask_indexes = list(
        start = grep(".*halfMask*.", vec, value = FALSE),
        end = c()
      ),
      noMask_names = grep(".*noMask*.", vec, value = TRUE),
      noMask_indexes = list(
        start = grep(".*noMask*.", vec, value = FALSE),
        end = c()
      )
    ),
    preflook_names = grep(".*PrefLook*.", vec, value = TRUE),
    preflook_indexes = list(
      start = grep(".*PrefLook*.", vec, value = FALSE),
      end = c()
    ),
    locationpref_names = grep("Location_Pref*.", vec, value = TRUE),
    locationpref_indexes = list(
      start = grep("Location_Pref*.", vec, value = FALSE),
      end = c()
    ),
    # not needed: endofvideo_names = grep("endofvideo", df$x_bad, value = TRUE),
    endofvideo_indexes = grep("endofvideo", vec, value = FALSE)
  )

  # store mask order
  events$Mask_order <-
    events$Mask_names %>%
    map(~ value_parser_by_key(lut_mask_filename, .)$condition) %>%
    unlist() %>%
    unique()

  return(events)
}
