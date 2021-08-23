# ==================================================
# Define AOI collections
# ==================================================

aoi_preflook <- list(
  column_name = "AOIPrefLook",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = NA,
  aoilist = list(
    aoi1 = list(
      hit_name = "left",
      x_topleft = 1,
      y_topleft = 1,
      x_bottomright = 640,
      y_bottomright = 720
    ),
    aoi2 = list(
      hit_name = "right",
      x_topleft = 641,
      y_topleft = 1,
      x_bottomright = 1280,
      y_bottomright = 720
    )
  )
)

aoi_locationpreflook <- list(
  column_name = "AOILocationPrefLook",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = NA,
  aoilist = list(
    aoi1 = list(
      hit_name = "left",
      x_topleft = 1,
      y_topleft = 1,
      x_bottomright = 640,
      y_bottomright = 720
    ),
    aoi2 = list(
      hit_name = "right",
      x_topleft = 641,
      y_topleft = 1,
      x_bottomright = 1280,
      y_bottomright = 720
    )
  )
)

aoi_screen <- list(
  column_name = "AOIScreen",
  no_evaluation_label = "NO EVAL",
  missing_coordinate_label = NA,
  aoilist = list(
    aoi1 = list(
      hit_name = TRUE,
      x_topleft = 1,
      y_topleft = 1,
      x_bottomright = 1280,
      y_bottomright = 720
    )
  )
)

# ==================================================
# Define Name Look-Up Tables / Mappings
# ==================================================

#                          "1_noMask_Left_ObLeft_2B_ObRight_2A_TargetOb_2B.mp4"
lut_mask_filename <- c("order", "condition", "gaze_direction", "xyz1", "object_left", "xyz2", "object_right", "xyz3", "target_object")


#                              "7_PrefLook_ObLeft_2B_ObRight_2A.mp4"
lut_preflook_filename <- c("order", "xyz1", "xyz2", "object_left", "xyz3", "object_right")

#                                     "Location_Pref_Right.mp4"
lut_location_preflook_filename <- c("locationkey", "prefkey", "location")

#                             6_Block_1_noMask
lut_preflook_taskname <- c("enumber", "blockkey", "blocknumber", "maskcondition")
