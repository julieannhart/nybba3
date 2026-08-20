# Create files for shiny app for data review
#
# Adapated from Gabriel Foley/MDDC BBA code
# Last updated 8/20/2026 by Julie A. Hart



#---Set paths----

# set working directory
dir_prj <- ".."

# data input
dir_dat <- file.path(dir_prj, "2026_review", "output")

# shiny app directory
dir_shiny <- file.path(dir_prj, "..", "shiny_app_for_data_review")



#---Load data----

#--load data if not already loaded
if (FALSE) {
  revtrack <- readRDS(
    file.path(dir_out, "review_tracking_final_yr2026_v20260813.rds")
  )
}



#---Create lookup table----

#breeding species lookup table (common_name x block_county x breeding)

filter_spp_list <- revtrack |>
  dplyr::mutate(breeding = dplyr::case_when(
    final_category %in% c("possible", "probable", "confirmed") ~ TRUE,
    TRUE ~ FALSE
  )) |>
  dplyr::select(common_name, block_county, breeding) |>
  dplyr::distinct()

species_list <- data.frame(common_name = unique(revtrack$common_name),
  block_county = "All Regions",
  breeding = TRUE)

filter_spp_list <- dplyr::bind_rows(filter_spp_list, species_list)

saveRDS(filter_spp_list, file.path(dir_shiny, "data", "bba3_species_list.rds"))



#---Format data for app----

# improve readability for external reviewers

#round distance
revtrack$distance_to_range_km <- round(revtrack$distance_to_range_km, digits = 2)

#change review_cat to descriptions
revtrack$review_cat[revtrack$review_cat %in% "1"] <- "confident"
revtrack$review_cat[revtrack$review_cat %in% c("2a", "2b_3", "2b_b")] <- "uncertain"
revtrack$review_cat[revtrack$review_cat %in% "3"] <- "unlikely"

#remove underscore in breeding season column
revtrack$breeding_season <- gsub("_", " ", revtrack$breeding_season)

#fix ebird url
revtrack$checklist_link <- paste0("https://ebird.org/checklist/", revtrack$sampling_event_identifier)



#---Make species files----

#--split review tracker into species-specific files for smaller loading

revtrack |>
  dplyr::group_by(common_name) |>
  dplyr::group_walk(
    ~ saveRDS(
      .x,
      file.path(
        dir_shiny,
        "data",
        "species_data",
        paste0("bba3_", gsub("/", "_", .y$common_name), ".rds")
      )
    ),
    .keep = TRUE
  )
