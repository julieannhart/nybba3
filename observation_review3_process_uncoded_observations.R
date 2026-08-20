# Purpose: Process "uncoded" observations
# For wide-ranging species (gulls, terns, etc.)
#
# Uncoded means either:
#   the obs was not coded by the observer OR
#   it was marked as NC (no code) through data review
#
# Records that are observed within core breeding season dates and in range
# should be shown on the breeding evidence map in a different color family,
# denoting that the species occurred there during the breeding season and was
# using the area for part of its nesting cycle.
#
# if observation is in core breeding season and in range:
#   breeding_code <- "W"
#   breeding_category <- "ranging"
#
# Note: this uses data exported in observation_review1_initial_screening.R
#
# Script written by Julie A. Hart
# last edited 8/20/2026



##------ set paths ------

# set working directory
dir_prj <- ".."

# path to expected breeding dates
dir_data <- file.path(dir_prj, "..", "data")

# path to eBird data
dir_ebird <- file.path(dir_prj, "..", "..", "eBird")
stopifnot(dir.exists(dir_ebird))

# path to breeding and year-round ranges from ebirdst
dir_ebird_ranges <- file.path(
  dir_ebird,
  "eBird_ranges",
  "breeding_ranges_clipped_ny",
  "raw"
)
stopifnot(dir.exists(ebirdst::ebirdst_data_dir()))

# path to other ranges
dir_spatial <- file.path(dir_prj, "..", "..", "Spatial_data", "ecoregions")
stopifnot(dir.exists(dir_spatial))

# review output directory
dir_review <- file.path("..", paste0(2026, "_review"))
dir_out <- file.path(dir_review, "output")

# shiny app directory
dir_shiny <- file.path(dir_prj, "..", "shiny_app_for_data_review")



##------ set file names ------

fname_inputs <- list(

  breedingspecies = file.path(dir_data, "breeding_species_plus_hybrids.csv"),

  rarebreeders = file.path(dir_data, "rare_breeding_species.csv"),

  dates = file.path(dir_data, "expected_dates_2026-06-01.csv"),

  uncoded = file.path(dir_out, "uncoded_obs_for_review_yr2026_v20260813.rds"),

  coded = file.path(dir_out, "coded_obs_on_good_checklists_yr2026_v20260614.rds"),

  reviewed = file.path(dir_out, "final_obs_data_combined_2026-08-14.rds")
)

stopifnot(file.exists(unlist(fname_inputs)))



##------ read in data ------

#--breeding season cutoffs
dates <- read.csv(fname_inputs[["dates"]], na.strings = c("", "NULL"))
#remove NA columns
dates <- dates[, colSums(is.na(dates)) < nrow(dates)]

#--get w species list
w_spp <- dates$common_name[dates$w_species %in% "w"]

#--rare species list
brc_rare <- read.csv(fname_inputs[["rarebreeders"]])

#--breeding species list
breeding_sp <- read.csv(fname_inputs[["breedingspecies"]])



##------ process uncoded data ------

###-- read in originally uncoded observations ----

uncoded <- readRDS(fname_inputs[["uncoded"]])
nrow(uncoded) #8,065,141 observations
#already restricted to good checklists


#--clean up breeding codes and categories
unique(uncoded$breeding_code)
unique(uncoded$breeding_category)
uncoded$breeding_category <- "observed"


#--change behavior_code column name
uncoded <- dplyr::rename(
  uncoded,
  orig_breeding_code = behavior_code
)



###-- remove non-breeding species ----

ids <- uncoded$common_name %in% breeding_sp$Species
uncoded <- uncoded[ids, ]

if (FALSE) {
  nrow(uncoded) #7,685,336
  table(uncoded$breeding_code)
  #      F     NC
  # 133622    696
  table(uncoded$breeding_category)
  # observed
  #  7685336
}



###-- restrict to wide-ranging species ----

#only assess range and date for W species

ids <- uncoded$common_name %in% w_spp

wdat <- uncoded[ids, , drop = FALSE]

species <- unique(wdat[, "common_name"])


if (FALSE) {
  nrow(wdat) #779,607
  table(wdat$breeding_code)
  #     F    NC
  # 48501     8
  table(wdat$breeding_category)
  # observed
  #   779607
}



###--- create review tracker ----

# st = spatiotemporal
review_vars <- c(
  "user_change",
  "review_status",
  "has_support",
  "rarity_status",

  # Phase 1: code usage review
  "code_review_cat",
  "code_review_new_code",

  # Phase 2: spatiotemporal review
  "range_status",
  "distance_to_range_km",
  "resident_status",
  "breeding_season",
  "st_review_status",
  "st_review_cat",

  # Phase 3: review category = code review cat + st review cat
  "review_cat",

  # Phase 4: automatic changes
  "st_review_new_code",

  # Final review outcome
  "breeding_code",
  "breeding_category",
  "change_reason",
  "review_date"
)


revtrack <- data.frame(
  global_unique_identifier = wdat[, "global_unique_identifier"],
  common_name = wdat[, "common_name"],
  orig_breeding_code = wdat[, "breeding_code"],
  orig_breeding_category = wdat[, "breeding_category"],
  observation_date = wdat[, "observation_date"],
  sampling_event_identifier = wdat[, "sampling_event_identifier"],
  atlas_block = wdat[, "atlas_block"],
  block_county = wdat[, "block_county"],
  latitude = wdat[, "latitude"],
  longitude = wdat[, "longitude"],
  matrix(
    nrow = nrow(wdat),
    ncol = length(review_vars),
    dimnames = list(NULL, review_vars)
  )
)



###----- flag rare breeders -----

ids <- match(
  revtrack[, "common_name"],
  brc_rare[, "Species", drop = TRUE],
  nomatch = 0
)

revtrack["rarity_status"] <- ifelse(
  ids > 0,
  "rare_breeder",
  "other"
)



###----- determine range -----

wdat_spat <- sf::st_as_sf(
  wdat,
  coords = c("longitude", "latitude"),
  crs = 4326
)

pb <- utils::txtProgressBar(max = length(species), style = 3)

for (k in seq_along(species)) {
  is_sp <- wdat[, "common_name"] %in% species[k]

  sp_range <- NULL
  #if species has range_note in breeding date table, use what is directed
  #otherwise use ebird status and trends map
  if (!is.na(dates$range_notes[dates$common_name %in% species[k]])) {
    fname_range <- file.path(
      dir_spatial,
      paste0(dates$range_notes[dates$common_name %in% species[k]], ".gpkg")
    )

    sp_range <- sf::st_read(
      fname_range,
      quiet = TRUE
    ) |>
      sf::st_transform(crs = 4326)

  } else {
    fname_range <- file.path(
      dir_ebird_ranges,
      paste0(sub("/", "_", species[k]), "-breeding-range-raw-9k-2024-NY.gpkg")
    )

    if (file.exists(fname_range)) {
      sp_range <- sf::st_read(
        fname_range,
        layer = "range",
        quiet = TRUE
      )
    }
  }

  if (!is.null(sp_range)) {

    stopifnot(nrow(sp_range) == 1L) # assume 1 multipolygon

    obs_dist_to_range <- units::set_units(
      sf::st_distance(wdat_spat[is_sp, ], sp_range)[, 1L, drop = TRUE],
      "km"
    ) |>
      units::drop_units()

    revtrack[is_sp, "range_status"] <- ifelse(
      obs_dist_to_range < sqrt(.Machine[["double.eps"]]),
      "rangein",
      "rangeout"
    )

    revtrack[is_sp, "distance_to_range_km"] <- obs_dist_to_range

  } else {
    revtrack[is_sp, "range_status"] <- "nd"
    warning("No breeding range map for ", species[k])
  }

  utils::setTxtProgressBar(pb, value = k)
}

close(pb)


stopifnot(!anyNA(revtrack[, "range_status"]))


if (FALSE) {
  table(revtrack$range_status)
  #--full dataset
  # rangein rangeout
  # 7132096   647263

  #--subset to w species
  # rangein rangeout
  #  737420    42187
}



###----- determine season -----


var_seasons <- c(
  "pre_breeding_start",
  "core_breeding_start",
  "core_breeding_end",
  "post_breeding_end"
)
def_seasons <- c(
  "early_season",
  "prebreeding",
  "core_breeding",
  "postbreeding",
  "late_season"
)


pb <- utils::txtProgressBar(max = length(species), style = 3)


for (k in seq_along(species)) {
  is_sp <- revtrack[, "common_name"] %in% species[k]
  is_dates <- dates[, "common_name"] == species[k]

  if (any(is_sp) && any(is_dates)) {

    #--- Identify resident/migrant status
    revtrack[is_sp, "resident_status"] <- switch(
      EXPR = dates[is_dates, "season_name"],
      breeding = "migrant",
      year_round = "resident",
      NA
    )

    #--- Identify time of observation relative to breeding season
    # calculate day of year
    # start of year = Jan 1 except:
    # Nov 1 for BAEA, GOEA, and GHOW; Dec 1 for CORA
    offset <- switch(
      EXPR = species[k],
      "Great Horned Owl" = 61,
      "Bald Eagle" = 61,
      "Golden Eagle" = 61,
      "Common Raven" = 31,
      0
    )
    obs_doy <- 1 + as.POSIXlt(revtrack[is_sp, "observation_date"])$yday
    obs_doy <- (obs_doy + offset) %% 365L
    sp_doy <- 1 + as.POSIXlt(
      unlist(
        dates[is_dates, var_seasons]
      ),
      format = "%m/%d/%Y"
    )$yday
    sp_doy <- (sp_doy + offset) %% 365L

    if (any(diff(sp_doy) < 0)) {
      stop(
        "Species ", shQuote(species[k]),
        ": breeding season dates are not increasing: ",
        paste0(dates[is_dates, var_seasons], collapse = ", ")
      )
    }

    tmp <- 1 + findInterval(obs_doy, vec = sp_doy)
    revtrack[is_sp, "breeding_season"] <- def_seasons[tmp]

  } else {
    warning("Breeding season not defined for ", species[k])
  }

  utils::setTxtProgressBar(pb, value = k)
}

close(pb)


stopifnot(
  !anyNA(revtrack[, "resident_status"]),
  !anyNA(revtrack[, "breeding_season"])
)


if (FALSE) {
  table(revtrack$breeding_season)
  #--full datset
  # core_breeding  early_season   late_season  postbreeding   prebreeding
  #       3075340       1864696       1772888        378785        687650

  #--subset
  # core_breeding  early_season   late_season  postbreeding   prebreeding
  #        258040        191399        215806         51688         62674
}



###----- add W codes -----

# if in core breeding season and in range:
#   breeding code <- "W"
#   breeding category <- "ranging"
#   change reason <- "expctbrdr"

uncoded_core_inrange <-
  revtrack$range_status %in% "rangein" &
  revtrack$breeding_season %in% "core_breeding" &
  revtrack$common_name %in% species

revtrack[uncoded_core_inrange, "code_review_cat"] <- 4
revtrack[uncoded_core_inrange, "code_review_new_code"] <- NA
revtrack[uncoded_core_inrange, "st_review_status"] <- "uncoded_core_w"
revtrack[uncoded_core_inrange, "st_review_cat"] <- 4
revtrack[uncoded_core_inrange, "st_review_new_code"] <- "W"
revtrack[uncoded_core_inrange, "review_cat"] <- 4
revtrack[uncoded_core_inrange, "change_reason"] <- "expctbrdr"
revtrack[uncoded_core_inrange, "breeding_code"] <- "W"
revtrack[uncoded_core_inrange, "breeding_category"] <- "ranging"
revtrack[uncoded_core_inrange, "review_status"] <- "changed"
revtrack[uncoded_core_inrange, "review_date"] <- format(Sys.Date())


if (FALSE) {
  table(revtrack[, "change_reason"], useNA = "always")
  #--full dataset
  # expctbrdr      <NA>
  #   2928742   4850617

  #--subset
  # expctbrdr      <NA>
  #    245783    533824

  table(revtrack$breeding_code %in% "W")
  #  FALSE   TRUE
  # 533824 245783
  table(revtrack$breeding_category)
  # ranging
  # 245783
}



###----- merge with all uncoded obs -----

#--subset revtrack to only rows with W codes
#otherwise overwrite F codes and observed categories in next step
w_revtrack <- revtrack[revtrack$breeding_code %in% "W", ]


#--update existing data in uncoded with values from revtrack
uncoded <- uncoded |>
  dplyr::rows_update(
    w_revtrack |>
      dplyr::select(
        "global_unique_identifier",
        "breeding_code", "breeding_category"
      ),
    by = "global_unique_identifier"
  )


#--check column names
setdiff(names(uncoded), names(w_revtrack))
setdiff(names(w_revtrack), names(uncoded))

#drop duplicate columns from w_revtrack
cols2drop <- c("common_name", "observation_date", "atlas_block", "block_county",
  "latitude", "longitude", "sampling_event_identifier", "breeding_code",
  "breeding_category", "orig_breeding_code")
w_revtrack <- w_revtrack[, !names(w_revtrack) %in% cols2drop]


#--merge them
alluncoded <- dplyr::left_join(
  uncoded, w_revtrack, by = "global_unique_identifier"
)


if (FALSE) {
  #check that everything looks correct
  dim(alluncoded)
  # 7685336      75

  table(alluncoded$breeding_code, useNA = "always")
  #        F      NC       W    <NA>
  #   110271     695  245783 7328587

  table(alluncoded$breeding_category, useNA = "always")
  #  observed  ranging     <NA>
  #   7439553   245783        0
}



##----- process reviewed data -----

###--read in reviewed data originally coded ----
reviewed <- readRDS(fname_inputs[["reviewed"]])


###--add W codes ----

reviewed_w_data <-
  reviewed$range_status %in% "rangein" &
  reviewed$breeding_season %in% "core_breeding" &
  reviewed$breeding_code %in% "NC" &
  reviewed$common_name %in% species

reviewed[reviewed_w_data, "breeding_code"] <- "W"
reviewed[reviewed_w_data, "breeding_category"] <- "ranging"


if (FALSE) {
  table(reviewed$breeding_code %in% "W")
  #   FALSE    TRUE
  # 3309947     179

  table(reviewed$breeding_code)
  #     NC       F       W       H       S      S7       M       P       T       C       N
  # 447953       0     179  646643 1070216  360304   50723  197181   29087   45158   29161
  #      A       B      PE      CN      NB      DD      UN      ON      FL      CF      FY
  #  32650    3652    1211   25020   14184    1815    2628   68094  151125   62348   38262
  #     FS      NE      NY
  #   2113    6753   23666

  table(reviewed$breeding_category)
  # confirmed  observed  possible  probable   ranging
  #    397219    447953   1716859    747916       179
}



###-- merge with all coded obs ----

#--read in all coded data (pre-review)
coded <- readRDS(fname_inputs[["coded"]])
nrow(coded) #3,314,913


#--update common names
coded$common_name[coded$common_name %in% "Warbling Vireo"] <- "Eastern Warbling Vireo"
coded$common_name[coded$common_name %in% "Yellow Warbler"] <- "Northern Yellow Warbler"


#--remove non-breeding species
ids <- coded$common_name %in% breeding_sp$Species
coded <- coded[ids, ]
nrow(coded) #3,310,126


#--convert breeding category to category names
coded$breeding_category[coded$breeding_category %in% "C2"] <- "possible"
coded$breeding_category[coded$breeding_category %in% "C3"] <- "probable"
coded$breeding_category[coded$breeding_category %in% "C4"] <- "confirmed"


#--update col names to match
setdiff(names(reviewed), names(coded))
reviewed <- dplyr::rename(
  reviewed,
  change_reason = review_reason
)
setdiff(names(coded), names(reviewed))
coded <- dplyr::rename(
  coded,
  orig_breeding_code = behavior_code
)


#--update existing columns in coded with new values from reviewed
coded <- coded |>
  dplyr::rows_update(
    reviewed |>
      dplyr::select(
        "global_unique_identifier",
        "breeding_code", "breeding_category"
      ),
    by = "global_unique_identifier"
  )


#--remove dupe columns from reviewed
cols2drop <- c("common_name", "observation_date", "atlas_block", "block_county",
  "latitude", "longitude", "sampling_event_identifier", "orig_breeding_code",
  "breeding_code", "breeding_category", "max_block_code", "checklist_link"
)
reviewed <- reviewed[, !names(reviewed) %in% cols2drop]


#--merge reviewed data with coded
allcoded <- dplyr::left_join(
  x = coded,
  y = reviewed,
  by = "global_unique_identifier",
  copy = TRUE
)


if (FALSE) {
  dim(allcoded)
  # 3310126      75

  table(allcoded$breeding_code)
  #     A       B       C      CF      CN      DD      FL      FS      FY       H       M
  # 32650    3652   45158   62348   25020    1815  151125    2113   38262  646643   50723
  #     N      NB      NC      NE      NY      ON       P      PE       S      S7       T
  # 29161   14184  447953    6753   23666   68094  197181    1211 1070216  360304   29087
  #    UN       W
  #  2628     179

  table(allcoded$breeding_category)
  #  confirmed  observed  possible  probable   ranging
  #     397219    447953   1716859    747916       179
}



#----- merge uncoded and coded -----

#--check col names
setdiff(names(alluncoded), names(allcoded))
setdiff(names(allcoded), names(alluncoded))
#all good


#--check col types
common_cols <- intersect(names(allcoded), names(alluncoded))
type_check <- data.frame(
  column = common_cols,
  df1_type = sapply(allcoded[common_cols], class),
  df2_type = sapply(alluncoded[common_cols], class)
)
type_check |> dplyr::filter(df1_type != df2_type)

# #--update col types to match
# alluncoded$code_review_cat <- as.character(alluncoded$code_review_cat)
# alluncoded$st_review_cat <- as.character(alluncoded$st_review_cat)
# alluncoded$review_cat <- as.character(alluncoded$review_cat)
# alluncoded$review_date <- as.Date(alluncoded$review_date, format = "%Y-%m-%d")


#--merge them
alldat <- dplyr::bind_rows(alluncoded, allcoded)

stopifnot(nrow(alldat) == nrow(coded) + nrow(uncoded))


if (FALSE) {
  dim(alldat)
  # 10,995,462       75

  table(alldat$breeding_code, useNA = "always")
  #     A       B       C      CF      CN      DD       F      FL      FS      FY       H
  # 32650    3652   45158   62348   25020    1815  110271  151125    2113   38262  646643
  #     M       N      NB      NC      NE      NY      ON       P      PE       S      S7
  # 50723   29161   14184  448648    6753   23666   68094  197181    1211 1070216  360304
  #     T      UN       W    <NA>
  # 29087    2628  245962 7328587

  table(alldat$breeding_category, useNA = "always")
  # confirmed  observed  possible  probable   ranging      <NA>
  #    397219   7887506   1716859    747916    245962         0
}



#----- get max code and category per block -----

#--order codes
code_order <- c(
  NA, "NC", "F", "W", "H", "S", "S7", "M", "P", "T", "C", "N", "A", "B", "PE",
  "CN", "NB", "DD", "UN", "ON", "FL", "CF", "FY", "FS", "NE", "NY"
)

alldat$breeding_code <- factor(
  alldat$breeding_code,
  levels = code_order,
  ordered = TRUE
)


#--order categories
cat_order <- c("observed", "ranging", "possible", "probable", "confirmed")

alldat$breeding_category <- factor(
  alldat$breeding_category,
  levels = cat_order,
  ordered = TRUE
)


#--get highest category per spp per block
cat_by_block <- alldat |>
  dplyr::group_by(common_name, atlas_block) |>
  dplyr::summarize(max_block_cat = max(breeding_category))

alldat <- dplyr::left_join(
  x = alldat,
  y = cat_by_block,
  by = dplyr::join_by(common_name, atlas_block)
)


#--get highest code per spp per block
code_by_block <- alldat |>
  dplyr::group_by(common_name, atlas_block) |>
  dplyr::summarize(max_block_code = max(breeding_code))

alldat <- dplyr::left_join(
  x = alldat,
  y = code_by_block,
  by = dplyr::join_by(common_name, atlas_block)
)



#----- export results -----

#--save all records

saveRDS(
  alldat,
  file.path(dir_out, paste0("final_atlas_dataset_allobs_", Sys.Date(), ".rds"))
)


#--save only records with codes

#subset to obs with a breeding category
cats2keep <- c("ranging", "possible", "probable", "confirmed")
alldat_coded <- alldat[alldat$breeding_category %in% cats2keep, ]

saveRDS(
  alldat_coded,
  file.path(dir_out, paste0("final_atlas_dataset_codedobs_", Sys.Date(), ".rds"))
)


#--save frequency table of codes x species

frq_sp_code <- table(alldat_coded$common_name, alldat_coded$breeding_code)

write.csv(
  frq_sp_code,
  file.path(
    dir_out,
    paste0("freq_species_by_brcode_usage_", Sys.Date(), ".csv")
  )
)


#--save frequency table of categories x species

frq_sp_cat <- table(alldat_coded$common_name, alldat_coded$breeding_category)

write.csv(
  frq_sp_cat,
  file.path(
    dir_out,
    paste0("freq_species_by_brcat_usage_", Sys.Date(), ".csv")
  )
)


#
# #----- create Shiny files -----
#
# #--create breeding species lookup table (common_name x block_county x breeding)
#
# filter_spp_list <- alldat |>
#   dplyr::mutate(breeding = dplyr::case_when(
#     breeding_category %in% c("ranging", "possible", "probable", "confirmed") ~ TRUE,
#     TRUE ~ FALSE
#   )) |>
#   dplyr::select(common_name, block_county, breeding) |>
#   dplyr::distinct()
#
# species_list <- data.frame(common_name = unique(alldat$common_name),
#   block_county = "All Regions",
#   breeding = TRUE)
#
# filter_spp_list <- dplyr::bind_rows(filter_spp_list, species_list)
#
# saveRDS(filter_spp_list, file.path(dir_shiny, "data", "bba3_species_list.rds"))
#
#
# #--adjust some columns for shiny app
#
# #round distance
# alldat$distance_to_range_km <- round(alldat$distance_to_range_km, digits = 2)
#
# #change review_cat to descriptions
# alldat$review_cat[alldat$review_cat %in% "1"] <- "confident"
# alldat$review_cat[alldat$review_cat %in% c("2a", "2b_3", "2b_b")] <- "uncertain"
# alldat$review_cat[alldat$review_cat %in% "3"] <- "unlikely"
# alldat$review_cat[alldat$review_cat %in% "4"] <- "ranging"
#
# #remove underscore in breeding season column
# alldat$breeding_season <- gsub("_", " ", alldat$breeding_season)
#
# #fix ebird url
# alldat$checklist_link <- paste0("https://ebird.org/checklist/", alldat$sampling_event_identifier)
#
#
# #--split review tracker into species-specific files for smaller loading
# alldat |>
#   dplyr::group_by(common_name) |>
#   dplyr::group_walk(
#     ~ saveRDS(
#       .x,
#       file.path(
#         dir_shiny,
#         "data",
#         "species_data",
#         paste0("bba3_", gsub("/", "_", .y$common_name), ".rds")
#       )
#     ),
#     .keep = TRUE
#   )

