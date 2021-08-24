# Clear workspace
rm(list = ls(all.names = TRUE)) # clears everything
graphics.off() # close all open graphics

options(scipen = 999)

if (!require(tidyverse)) install.packages(tidyerse)
library(tidyverse)


# import utility functions
list.files("util", "*.r$", full.names = TRUE, ignore.case = TRUE) %>% sapply(source)


# import interface
source("interface.R")

subj_list <- list.files("raw/")

# buckets to store loop data
df_preflook_all <- list()
df_calibration_all <- list()
df_fam_all <- list()

# bucket to store bad/incomplete subjects
bad_subjects <- c()

# single subject(s) check (taken from position in subj_list)
for (current_subject in subj_list[2:2]) {

# all subjects
# for (current_subject in subj_list) {
  df <- read.csv(file.path("raw", current_subject), header = FALSE, skip = 1, stringsAsFactors = FALSE)

  colnames(df) <- c(
    "Block_Name",
    "Block_Nr",
    "exp_subject_id",
    "rec_session_id",
    "session_nr",
    "Task_Name",
    "Task_Nr",
    "times",
    "Trial_Id",
    "Trial_Nr",
    "variable_name",
    "x_bad",
    "y_bad",
    "t",
    "c"
  )

  if (anyNA(df$t)) {
    # initial t column fix, as we work with old data
    df <- fix_labvanced_t_column(df, FALSE)
  }
  # perform last sanity check if t is really sorted
  if (is.unsorted(df$t)) stop("Stop, t is not ordered!")


  # check if subject has complete data (i.e., Task_Nr should go from 2:8)
  if (!identical(2:8, unique(df$Task_Nr))) {
    bad_subjects <- c(bad_subjects, current_subject)
    warning(str_interp("The current subject: ${current_subject} has not complete data."))
    next
  }



  # round x and y to integers (if no rounding R has issues with that big floating numbers!)
  # FYI: R can calculate numbers up to 15 digits (we have sometimes 17): https://stackoverflow.com/questions/2287616/controlling-number-of-decimal-digits-in-print-output-in-r
  # for x, converting to numeric will strip away text information (NA warning will occur), yet this info is still in x_bad
  # ... and it is okay to suppressWarnings here, as we are aware of it
  df$x <- df$x_bad %>%
    as.numeric() %>%
    suppressWarnings() %>%
    round() %>%
    as.integer()
  df$y <- df$y_bad %>%
    round() %>%
    as.integer()


  # replace bad names in Task_Name (i.e., No_Mask should become noMask & Half_Mask -> HalfMask)
  bad_no_Mask_name <- grep(".*No_Mask", ignore.case = TRUE, df$Task_Name, value = TRUE)[1]
  bad_no_Mask_indexes <- grep(".*No_Mask", ignore.case = TRUE, df$Task_Name)
  bad_no_Mask_prefix <- str_extract(bad_no_Mask_name, "\\w\\d_Block_\\d_")
  df$Task_Name[bad_no_Mask_indexes] <- paste(bad_no_Mask_prefix, "noMask", sep = "")

  bad_Half_Mask_name <- grep(".*Half_Mask", ignore.case = TRUE, df$Task_Name, value = TRUE)[1]
  bad_Half_Mask_indexes <- grep(".*Half_Mask", ignore.case = TRUE, df$Task_Name)
  bad_Half_Mask_prefix <- str_extract(bad_Half_Mask_name, "\\w\\d_Block_\\d_")
  df$Task_Name[bad_Half_Mask_indexes] <- paste(bad_Half_Mask_prefix, "halfMask", sep = "")

  # remove unnecessary columns (goodbye times)
  df <- select(df, -c(Block_Name, Block_Nr, rec_session_id, session_nr, Trial_Id, times, c))


  # Add GazeEventDuration column (Tobii naming) — values are in ms (like t)
  df$GazeEventDuration <- c(diff(df$t), NA)


  # Add StudioEventData column (Tobii naming)
  df$StudioEventData <- NA


  # Extract event names and indexes from x_bad column, based on regex pattern
  events <- get_events_from_column(df$x_bad)

  # shift start indexes by +1, to avoid including the marker sample
  events$Mask_indexes$start <- events$Mask_indexes$start + 1
  events$single_mask_events$Mask_indexes$start <- events$single_mask_events$Mask_indexes$start + 1
  events$single_mask_events$halfMask_indexes$start <- events$single_mask_events$halfMask_indexes$start + 1
  events$single_mask_events$noMask_indexes$start <- events$single_mask_events$noMask_indexes$start + 1
  events$preflook_indexes$start <- events$preflook_indexes$start + 1
  events$locationpref_indexes$start <- events$locationpref_indexes$start + 1

  # shift all endofvideo_indexes by -1, as this is only a marker and should not be included
  events$endofvideo_indexes <- events$endofvideo_indexes - 1

  # get corresponding end indexes of mask, single_mask_events, preflook and locationpreflook start indexes using endofvideo_indexes
  events$Mask_indexes$end <- get_end_indexes(events$Mask_names, events$Mask_indexes, events$endofvideo_indexes)
  events$single_mask_events$Mask_indexes$end <- get_end_indexes(events$single_mask_events$Mask_names, events$single_mask_events$Mask_indexes, events$endofvideo_indexes)
  events$single_mask_events$halfMask_indexes$end <- get_end_indexes(events$single_mask_events$halfMask_names, events$single_mask_events$halfMask_indexes, events$endofvideo_indexes)
  events$single_mask_events$noMask_indexes$end <- get_end_indexes(events$single_mask_events$noMask_names, events$single_mask_events$noMask_indexes, events$endofvideo_indexes)
  events$preflook_indexes$end <- get_end_indexes(events$preflook_names, events$preflook_indexes, events$endofvideo_indexes)
  events$locationpref_indexes$end <- get_end_indexes(events$locationpref_names, events$locationpref_indexes, events$endofvideo_indexes)



  # Add AOI hit column for PrefLook and LocationPrefLook events (left & right evaluation)
  df <- get_aois(df, aoi_preflook, events$preflook_indexes)
  df <- get_aois(df, aoi_locationpreflook, events$locationpref_indexes)

  # Add AOI hit column for all Mask events (TRUE/FALSE evaluation)
  df <- get_aois(df, aoi_screen, events$Mask_indexes)


  # ======================================================================================
  # OVERVIEW TABLE FOR PREFERENTIAL LOOKING
  # ======================================================================================
  # do PrefLook table and use Task_names for mask events
  df_preflook <- data.frame(matrix(NA, nrow = 3, ncol = 0), stringsAsFactors = FALSE)


  df_preflook$ID <- df$exp_subject_id[1]
  df_preflook$Trial <- 1:3

  df_preflook$Condition <- df$Task_Name %>%
    unique() %>%
    str_extract("(?<=Block_\\d_)\\w*") %>%
    na.omit() %>%
    as.vector()

  df_preflook$LT_Left <- get_durations_for_condition(df, length(events$preflook_indexes$start), events$preflook_indexes, "AOIPrefLook", 10000)$left
  df_preflook$LT_Right <- get_durations_for_condition(df, length(events$preflook_indexes$start), events$preflook_indexes, "AOIPrefLook", 10000)$right

  # total looking time
  df_preflook$LT_Total <- df_preflook$LT_Left + df_preflook$LT_Right

  # Add inclusion criterion
  df_preflook$Is_Included <- df_preflook$LT_Left > 200 | df_preflook$LT_Right > 200


  # grab the first name of the mask events (index 1, 7, 13) and put them in the value_parser and get target_object
  target_object_list <- c(
    value_parser_by_key(lut_mask_filename, events$Mask_names[1], trim_right = 4)$target_object,
    value_parser_by_key(lut_mask_filename, events$Mask_names[7], trim_right = 4)$target_object,
    value_parser_by_key(lut_mask_filename, events$Mask_names[13], trim_right = 4)$target_object
  )

  preflook_objects_left <- c(
    value_parser_by_key(lut_preflook_filename, events$preflook_names[1], trim_right = 4)$object_left,
    value_parser_by_key(lut_preflook_filename, events$preflook_names[2], trim_right = 4)$object_left,
    value_parser_by_key(lut_preflook_filename, events$preflook_names[3], trim_right = 4)$object_left
  )

  preflook_objects_right <- c(
    value_parser_by_key(lut_preflook_filename, events$preflook_names[1], trim_right = 4)$object_right,
    value_parser_by_key(lut_preflook_filename, events$preflook_names[2], trim_right = 4)$object_right,
    value_parser_by_key(lut_preflook_filename, events$preflook_names[3], trim_right = 4)$object_right
  )

  for (i in seq_along(target_object_list)) {
    if (target_object_list[i] != preflook_objects_left[i]) {
      df_preflook$Novel_Object[i] <- "left"
      df_preflook$LT_Novel[i] <- df_preflook$LT_Left[i]
      next
    }
    if (target_object_list[i] != preflook_objects_right[i]) {
      df_preflook$Novel_Object[i] <- "right"
      df_preflook$LT_Novel[i] <- df_preflook$LT_Right[i]
      next
    }
    stop("Something is wrong...")
  }

  df_preflook$Prop_LT_Novel <- df_preflook$LT_Novel / df_preflook$LT_Total


  df_preflook$PrefLookingDuration <- df$t[events$preflook_indexes$end] - df$t[events$preflook_indexes$start]


  # ======================================================================================
  # OVERVIEW TABLE FOR INCLUSION — BASED ON LOCATION CHECKS
  # ======================================================================================

  df_calibration <- data.frame(matrix(NA, nrow = 2, ncol = 0), stringsAsFactors = FALSE)

  df_calibration$ID <- df$exp_subject_id[1]
  df_calibration$Condition <- c(
    value_parser_by_key(lut_location_preflook_filename, events$locationpref_names[1], trim_right = 4)$location,
    value_parser_by_key(lut_location_preflook_filename, events$locationpref_names[2], trim_right = 4)$location
  )


  # Looking times for left/right
  df_calibration$LT_Left <- get_durations_for_condition(df, length(events$locationpref_indexes$start), events$locationpref_indexes, "AOILocationPrefLook", 4000)$left
  df_calibration$LT_Right <- get_durations_for_condition(df, length(events$locationpref_indexes$start), events$locationpref_indexes, "AOILocationPrefLook", 4000)$right


  # total looking time
  df_calibration$LT_Total <- df_calibration$LT_Left + df_calibration$LT_Right

  df_calibration$LT_Correct[1] <- df_calibration$LT_Right[1]
  df_calibration$LT_Correct[2] <- df_calibration$LT_Left[2]


  df_calibration$Prop_LT_Correct <- df_calibration$LT_Correct / df_calibration$LT_Total

  # Add inclusion criterion: Prop_LT_Correct > 0.666
  df_calibration$Is_Included <- df_calibration$Prop_LT_Correct > 0.666



  # ======================================================================================
  # OVERVIEW TABLE FOR FAMILIARIZATION PHASE
  # ======================================================================================
  df_fam <- data.frame(matrix(NA, nrow = 18, ncol = 0), stringsAsFactors = FALSE)

  df_fam$ID <- df$exp_subject_id[1]

  df_fam$Trials_per_Block <- rep(1:6, 3)

  df_fam$Condition[1:6] <- events$Mask_order[1]
  df_fam$Condition[7:12] <- events$Mask_order[2]
  df_fam$Condition[13:18] <- events$Mask_order[3]

  mask1_indexes <- events$single_mask_events[[paste(events$Mask_order[1], "_indexes", sep = "")]]
  mask2_indexes <- events$single_mask_events[[paste(events$Mask_order[2], "_indexes", sep = "")]]
  mask3_indexes <- events$single_mask_events[[paste(events$Mask_order[3], "_indexes", sep = "")]]

  df_fam$LT_Screen_start_2_end[1:6] <- get_durations_for_condition(df, 6, mask1_indexes, "AOIScreen")$screen
  df_fam$LT_Screen_start_2_end[7:12] <- get_durations_for_condition(df, 6, mask2_indexes, "AOIScreen")$screen
  df_fam$LT_Screen_start_2_end[13:18] <- get_durations_for_condition(df, 6, mask3_indexes, "AOIScreen")$screen

  df_fam$LT_Screen_last_5[1:6] <- get_durations_for_condition(df, 6, mask1_indexes, "AOIScreen", get_last_ms_to_video_end = 5000)$screen
  df_fam$LT_Screen_last_5[7:12] <- get_durations_for_condition(df, 6, mask2_indexes, "AOIScreen", get_last_ms_to_video_end = 5000)$screen
  df_fam$LT_Screen_last_5[13:18] <- get_durations_for_condition(df, 6, mask3_indexes, "AOIScreen", get_last_ms_to_video_end = 5000)$screen

  # df_fam$LT_Screen3_to_5 <- NA TODO

  df_fam$Is_Included_last_5 <- df_fam$LT_Screen_last_5 > 200

  df_fam$FamPhase_Duration <- df$t[events$Mask_indexes$end] - df$t[events$Mask_indexes$start]

  df_fam$Is_Included_SD <-
    df_fam$FamPhase_Duration < mean(df_fam$FamPhase_Duration) + (2 * sd(df_fam$FamPhase_Duration))



  # rbind df_preflook, df_calibration & df_fam together
  # FYI: https://stackoverflow.com/questions/29402528/append-data-frames-together-in-a-for-loop/29419402
  df_preflook_all[[current_subject]] <- df_preflook
  df_calibration_all[[current_subject]] <- df_calibration
  df_fam_all[[current_subject]] <- df_fam
}

# stitch everything together
df_preflook_all <- bind_rows(df_preflook_all) # old: do.call("rbind", df_preflook_all)
df_calibration_all <- bind_rows(df_calibration_all)
df_fam_all <- bind_rows(df_fam_all)

# expose global environment (for R Markdown)
# save.image("script_env.Rdata")
