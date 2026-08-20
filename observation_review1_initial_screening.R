# Script to run observation-level data review
#
# Includes the following phases:
# Phase 1: review code usage
# Phase 2: assess range and date (spatiotemporal review)
# Phase 3: assign review category (combine code and st review steps)
# Phase 4: make automatic changes
# Phase 5: conduct manual review
# Phase 6: finalize dataset
#
# NOTE: nomadic double brooders (Crossbills, Sedge Wren) are treated as
# one long breeding season that encompasses both breeding periods
#
# NOTE: wide-ranging species (vultures, terns) handled in a later step
#
# Notes on specific observations:
# Scaup pair at Tupper Lake 5/4/24: S171912372 -- species unconfirmed by eBird review
# Leach's Storm-Petrel 6/4/24: S179597326 -- handled in uncoded observations
# Pelicans in Buffalo Apr 2024 -- too early to say if prospecting or moving through
#
# Script written by Julie A. Hart
# last edited 8/20/2026



##----TODO-----

# -- Phase 6: final review
# TODO: if highest species code in a block is an S, see if can get to S7 or M
#   * to test S7 codes: if same observer, species, location, stationary or
#     incidental, species, more than 7 days apart, in range, and in core season
#   * to test M7 codes: if same observer, species, block, count of 7 or more on
#     same doy, in range, and in core season



##------ Set Variables ------

# review year and file tags
review_year <- 2026
today_tag <- format(Sys.Date(), "%Y%m%d")
review_status <- 20260813
review_tag <- paste0("yr", review_year, "_v", review_status)

# range map buffers
ecoregion_buffer <- 3
ebirdst_buffer <- 5



##-----Set Paths-----

# set working directory
dir_prj <- ".."

# path to expected breeding dates
dir_data <- file.path(dir_prj, "..", "data")
stopifnot(dir.exists(dir_data))

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
dir_review <- file.path("..", paste0(review_year, "_review"))
dir_out <- file.path(dir_review, "output")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)



##------ Set Filenames: Input ------

fname_inputs <- list(

  ebirdny_BBA = file.path(
    dir_ebird,
    "EBD_public_data",
    "ebd_US-NY_202001_202412_unv_smp_relAug-2025_AllPortals_AllData_AllSpp.rds"
  ),

  goodChecklists = file.path(
    dir_prj,
    "..",
    "checklist_validation/output",
    "SEIs that pass checklist review.csv"
  ),

  breedingspecies = file.path(dir_data, "breeding_species_plus_hybrids.csv"),

  rarebreeders = file.path(dir_data, "rare_breeding_species.csv"),

  brc_usage = file.path(dir_data, "expected_codes_long_2026-01-12.csv"),

  dates = file.path(dir_data, "expected_dates_2026-06-01.csv"),

  records_manually_reviewed = file.path(
    dir_out,
    "records_manually_reviewed_2026-03-25.xlsx"
  )
)

stopifnot(file.exists(unlist(fname_inputs)))



##------ Set Filenames: Output ------

fname_out <- list(

  #--- Data files
  obs_on_good_checklists = file.path(
    dir_out,
    paste0("all_obs_on_good_checklists_", review_tag, ".rds")
  ),

  coded_obs = file.path(
    dir_out,
    paste0("coded_obs_on_good_checklists_", review_tag, ".rds")
  ),

  uncoded_obs = file.path(
    dir_out,
    paste0("uncoded_obs_for_review_", review_tag, ".rds")
  ),

  #--- Review files

  # Finalized review tracker (after manual review was incorporated)
  reviewtrack_final = file.path(
    dir_out,
    paste0("review_tracking_final_", review_tag, ".rds")
  ),

  # Review tracker (up to just before manual review)
  reviewtrack_tmp = file.path(
    dir_out,
    paste0("review_tracking_tmp_", review_tag, ".rds")
  ),

  records_for_manual_review = file.path(
    dir_out,
    paste0("records_for_manual_review_", review_tag, ".csv")
  ),

  #--- Miscellaneous files
  records_nonbreeding = file.path(
    dir_out,
    paste0("nonbreeding_records_", review_tag, ".csv")
  ),

  brcusage_freq = file.path(
    dir_out,
    paste0("freq_species_by_brcode_cat_", review_tag, ".csv")
  ),

  brcusage_cat2b_or_3 = file.path(
    dir_out,
    paste0("freq_species_by_brcode_usage_cat_2b_or_3_", review_tag, ".csv")
  ),

  brcusage_cat3 = file.path(
    dir_out,
    paste0("freq_species_by_brcode_usage_cat3_", review_tag, ".csv")
  ),

  rangeinout_freq = file.path(
    dir_out,
    paste0("freq_species_by_range_inout_", review_tag, ".csv")
  ),

  breedingseason_freq = file.path(
    dir_out,
    paste0("freq_species_by_breeding_season_", review_tag, ".csv")
  ),

  st_errors = file.path(
    dir_out,
    paste0("freq_code_by_st_errors_", review_tag, ".csv")
  )
)




##-----Define Breeding Codes -----

observed <- c("NC", "F", NA)
possible <- c("H", "S")
probable <- c("S7", "M", "P", "T", "C", "N", "A", "B")
confirmed <- c(
  "PE", "CN", "NB", "DD", "UN", "ON", "FL", "CF", "FY",   "FS", "NE", "NY"
)

C1 <- observed
C2 <- possible
C3 <- probable
C4 <- confirmed

remove_codes <- c("NC", "F", NA)

solid_codes <- c("NE", "NY", "FS", "ON", "NB")

code_order <- c(
  "H", "S", "S7", "M", "P", "T", "C", "N", "A", "B", "PE", "CN",
  "NB", "DD", "UN", "ON", "FL", "CF", "FY", "FS", "NE", "NY"
)



##-----Load Data-----

#--* good checklist data----
checklists2keep <- read.csv(fname_inputs[["goodChecklists"]])[, "x", drop = TRUE]


#---* observation data----
cdat0 <- readRDS(fname_inputs[["ebirdny_BBA"]])

#subset to "good checklists" (passed checklist review)
cdat <- cdat0[cdat0$sampling_event_identifier %in% checklists2keep, ]

if (FALSE) {
  nrow(cdat0)  # 35,183,871 observations total
  length(unique(cdat0$sampling_event_identifier)) # 2,797,811 checklists

  nrow(cdat) # 11,380,054 observations on good checklists
  length(unique(cdat$sampling_event_identifier)) # 842,582 good checklists

  length(checklists2keep) #847,855
  #some good checklists not in the dataset

  #export observations on good checklists as RDS for posterity
  saveRDS(cdat, file = fname_out[["obs_on_good_checklists"]])
}

#save uncoded data for later processing
uncoded <- cdat[(cdat[, "breeding_code"] %in% remove_codes), , drop = FALSE]
nrow(uncoded) # 8,065,141 observations
saveRDS(uncoded, file = fname_out[["uncoded_obs"]])

#remove uncoded obs from further steps
cdat <- cdat[!(cdat[, "breeding_code"] %in% remove_codes), , drop = FALSE]
stopifnot(!anyNA(cdat[, "breeding_code"]))

if (FALSE) {
  nrow(cdat)  # 3,314,913
  length(unique(cdat$sampling_event_identifier)) #485,816

  #export coded observations on good checklists as RDS for posterity
  saveRDS(cdat, file = fname_out[["coded_obs"]])
}

#get unique list of blocks in dataset
blocks <- unique(cdat$atlas_block)

#update common names
cdat$common_name[cdat$common_name %in% "Warbling Vireo"] <- "Eastern Warbling Vireo"
cdat$common_name[cdat$common_name %in% "Yellow Warbler"] <- "Northern Yellow Warbler"


#---* breeding species----
breeding_sp <- read.csv(fname_inputs[["breedingspecies"]])

ids <- cdat$common_name %in% breeding_sp$Species

if (FALSE){
  nrow(cdat[!ids, ])
  #4787 records of nonbreeders with codes

  table(cdat[!ids, "common_name"])
  table(cdat[!ids, "breeding_code"], useNA = "ifany")

  #check for non-breeders with higher codes
  ids2 <- !ids & cdat[, "breeding_code"] %in% c(probable, confirmed)
  table(cdat[ids2, "common_name"], cdat[ids2, "breeding_code"])
}

# export non-breeders to csv for cursory review
if (file.exists(fname_out[["records_nonbreeding"]])) {
  warning(
    "File ", shQuote(basename(fname_out[["records_nonbreeding"]])),
    " already exists."
  )
} else {
  write.csv(
    cdat[!ids, , drop = FALSE],
    file = fname_out[["records_nonbreeding"]],
    row.names = FALSE
  )
}

# remove non-breeders for future steps
print("Removing data for these nonbreeding species")
print(table(cdat$common_name[!ids]))
cdat <- cdat[ids, ]
#nrow(cdat) # 3,310,126
species <- unique(cdat[, "common_name"])



#---* rare breeders----

brc_rare <- read.csv(fname_inputs[["rarebreeders"]])

#check that everything looks correct and up-to-date
if (FALSE) {
  setdiff(species, brc_rare$Species)
  setdiff(brc_rare$Species, species)
}



#---* breeding code usage chart----

brc_usage <- read.csv(fname_inputs[["brc_usage"]])
tmp <- setdiff(species, brc_usage$common_name)
if (length(tmp) > 0) {
  stop("brc_usage doesn't contain these species: ", toString(tmp))
}



#---* breeding season dates----

dates <- read.csv(fname_inputs[["dates"]], na.strings = c("", "NULL"))
#remove NA columns
dates <- dates[, colSums(is.na(dates)) < nrow(dates)]
#remove NA rows
dates <- dates[rowSums(is.na(dates)) != ncol(dates), ]

#ensure all species have breeding dates
tmp <- setdiff(species, dates$common_name)
if (length(tmp) > 0) {
  stop("dates doesn't contain these species: ", toString(tmp))
}



#------ Create Review Tracking spreadsheet ------

##-- Track all changes in a tracking sheet with these columns:
# global_unique_identifier = observation identifier used by eBird
# common_name = species common name
# orig_breeding_code = user input breeding code
# observation_date = date of observation
# sampling_event_identifier = checklist identifier used by eBird
# atlas_block = atlas block identifier
# latitude = latitude at which the observation was reported
# longitude = longitude at which the observation was reported
# block_county = county block is assigned to, based on block centroid

# checklist_link = URL of eBird checklist
# record_status = new or old (was record reviewed previously)
# user_change = changed or not (if record_status is old, has the user changed
#   any of the breeding values since last reviewed)
# review_status	= approved, changed, pending, reviewed-approved, reviewed-changed
# has_support = yes/no
# rarity_status = rare_breeder or other
# code_review_cat =	1, 2a, 2b, or 3
# code_review_new_code = new code after automatic changes arising from
#   code_review_cat 2b and 3

# range_status =	rangein or rangeout
# distance_to_range_km = shortest distance to breeding range [km]
# resident_status = migrant, resident
# breeding_season = early_season, pre_breeding, core_breeding, post_breeding,
#   late_season
# st_review_status = combined "residency_range_season_breeding-cat"
# st_review_cat	= 1, 2a, 2b, or 3

# review_cat = 1, 2a (manual review), 2b_b, 2b_3, or 3 determined from
#   `code_review_cat` x `st_review_cat`
# max_block_code = highest breeding code in the block from approved observations

# st_review_new_code = new code after automatic changes arising from
#   st_review_cat 2b and 3

# manual_new_code = new code from manual review arising from review_cat 2a records

# final_code = breeding code
# final_category = same or new category code
# change_reason =	na or change reason code
# review_date	= sys.time or from manual review sheet

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

  # Phase 5: manual review
  "manual_new_code",

  # Final review outcome
  "final_code",
  "final_category",
  "change_reason",
  "review_date"
)



#------ xxxxxx ------
#------ Conduct obs review ------

if (file.exists(fname_out[["reviewtrack_final"]])) {
  #--- If final review tracking spreadsheet exists, load, but don't do anything
  revtrack <- readRDS(fname_out[["reviewtrack_final"]])

  stopifnot(
    revtrack[, "global_unique_identifier"] == cdat[, "global_unique_identifier"]
  )

  stop(
    "Final review tracking spreadsheet exists and was loaded. ",
    "Review preparation code stops here."
  )

} else {
  #--- Final review tracking spreadsheet does not yet exist
  # Check whether interim review tracking spreadsheet exists, if not, create

  if (file.exists(fname_out[["reviewtrack_tmp"]])) {
    #--- Interim review tracking spreadsheet exists
    # --> phases 1-4 have previously been completed
    # ==> skip to phase 5 (manual review)

    revtrack <- readRDS(fname_out[["reviewtrack_tmp"]])

    stopifnot(
      revtrack[, "global_unique_identifier"] == cdat[, "global_unique_identifier"]
    )


  } else {
    #--- Interim review tracking spreadsheet does not yet exist
    # ==> execute code for phases 1-4

    revtrack <- data.frame(
      global_unique_identifier = cdat[, "global_unique_identifier"],
      common_name = cdat[, "common_name"],
      orig_breeding_code = cdat[, "breeding_code"],
      orig_breeding_category = cdat[, "breeding_category"],
      observation_date = cdat[, "observation_date"],
      sampling_event_identifier = cdat[, "sampling_event_identifier"],
      atlas_block = cdat[, "atlas_block"],
      block_county = cdat[, "block_county"],
      latitude = cdat[, "latitude"],
      longitude = cdat[, "longitude"],
      matrix(
        nrow = nrow(cdat),
        ncol = length(review_vars),
        dimnames = list(NULL, review_vars)
      )
    )

    #create URL
    revtrack$checklist_link <- paste0(
      "https://ebird.org/checklist/",
      cdat$sampling_event_identifier
    )



    ##-----* Identify supporting documentation -----

    revtrack[, "has_support"] <-
      !cdat[, "species_comments"] %in% c("", NA) |
      !cdat[, "has_media"] %in% c(FALSE, NA)

    stopifnot(!anyNA(revtrack[, "has_support"]))

    if (FALSE) {
      table(revtrack[, "has_support"], useNA = "always")
      #   FALSE    TRUE    <NA>
      # 2880521  429584       0
    }


    ##-----* Flag Rare Breeder Records-----

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

    if (FALSE) {
      nrow(revtrack[revtrack$rarity_status == "rare_breeder", ])
      #39591 records of rare breeders

      nrow(revtrack[revtrack$rarity_status == "rare_breeder", ])/nrow(cdat)
      #1.2% of coded obs
    }



    ##------ Phase1: Code Usage Review ------

    #---* Determine breeding code usage category 1, 2a, 2b, or 3----

    #look up common_name and breeding_code in brc_usage and return likelihood_score
    brc_vars <- c("common_name", "breeding_code")

    tmp <- merge(
      cdat[, c("global_unique_identifier", brc_vars)],
      brc_usage[, c(brc_vars, "likelihood_score")],
      all.x = TRUE,
      all.y = FALSE
    )

    stopifnot(nrow(tmp) == nrow(cdat), !anyNA(tmp[, "likelihood_score"]))

    ids <- match(
      revtrack[, "global_unique_identifier"],
      tmp[, "global_unique_identifier"],
      nomatch = 0
    )

    revtrack[ids > 0, "code_review_cat"] <- as.integer(tmp[ids, "likelihood_score"])

    #make an exception for PE score on banding checklists
    revtrack$code_review_cat[
      revtrack$orig_breeding_code %in% "PE" &
      revtrack$protocol_code %in% "P33"
    ] <- 1

    # Separate cat2 into 2a and 2b
    is_cat2 <- revtrack[, "code_review_cat"] %in% 2
    revtrack[is_cat2 & revtrack[, "has_support"], "code_review_cat"] <- "2a"
    revtrack[is_cat2 & !revtrack[, "has_support"], "code_review_cat"] <- "2b"

    # check all rows have a value
    stopifnot(!anyNA(revtrack[, "code_review_cat"]))

    # write freq table of which species are miscoded - by code_review_cat
    if (file.exists(fname_out[["brcusage_freq"]])) {
      warning(
        "File ", shQuote(basename(fname_out[["brcusage_freq"]])),
        " already exists."
      )
    } else {
      write.csv(
        table(revtrack[, "common_name"], revtrack[, "code_review_cat"]),
        file = fname_out[["brcusage_freq"]],
        row.names = TRUE
      )
    }

    # write freq table of which species are miscoded - code_review_cat = 2b or 3
    is_cat2_3 <- revtrack[, "code_review_cat"] %in% c("2b", "3")
    #print(sum(is_cat2_3)) # 48056
    if (file.exists(fname_out[["brcusage_cat2b_or_3"]])) {
      warning(
        "File ", shQuote(basename(fname_out[["brcusage_cat2b_or_3"]])),
        " already exists."
      )
    } else {
      write.csv(
        table(
          revtrack[is_cat2_3, "common_name"],
          revtrack[is_cat2_3, "orig_breeding_code"]
        ),
        file = fname_out[["brcusage_cat2b_or_3"]],
        row.names = TRUE
      )
    }


    #------* Change Code Usage cat3 records ------

    is_cat3 <- revtrack[, "code_review_cat"] %in% 3
    #print(sum(is_cat3)) # 31394

    # write freq table of which species are miscoded - code_review_cat = 3
    if (file.exists(fname_out[["brcusage_cat3"]])) {
      warning(
        "File ", shQuote(basename(fname_out[["brcusage_cat3"]])),
        " already exists."
      )
    } else {
      write.csv(
        table(
          revtrack[is_cat3, "common_name"],
          revtrack[is_cat3, "orig_breeding_code"]
        ),
        file = fname_out[["brcusage_cat3"]],
        row.names = TRUE
      )
    }


    #---* Recode errors----

    tmp <- merge(
      x = cdat[, c("global_unique_identifier", brc_vars)],
      y = brc_usage,
      all.x = TRUE,
      all.y = FALSE
    )

    ids <- match(
      revtrack[is_cat3, "global_unique_identifier"],
      tmp[, "global_unique_identifier"],
      nomatch = 0
    )

    ids2 <- is_cat3[ids > 0]
    revtrack[ids2, c("code_review_new_code", "change_reason")] <-
      tmp[ids, c("new_code", "change_reason")]
    revtrack[ids2, "review_status"] <- "changed"
    revtrack[ids2, "review_date"] <- format(Sys.Date())



    #------ Phase 2: Spatiotemporal Review ------

    ##-----* Check Location-----

    # categorize records as within or outside range using eBird range maps
    # add a column to say if that record is in or out of range
    # range_status <- c("rangein", "rangeout", "nd"), where nd = non-determined

    cdat_spat <- sf::st_as_sf(cdat, coords = c("longitude", "latitude"), crs = 4326)

    pb <- utils::txtProgressBar(max = length(species), style = 3)

    for (k in seq_along(species)) {
      is_sp <- cdat[, "common_name"] %in% species[k]

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

        range_buffer <- ecoregion_buffer

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

        range_buffer <- ebirdst_buffer

      }

      if (!is.null(sp_range)) {

        stopifnot(nrow(sp_range) == 1L) # assume 1 multipolygon

        obs_dist_to_range <- units::set_units(
          sf::st_distance(cdat_spat[is_sp, ], sp_range)[, 1L, drop = TRUE],
          "km"
        ) |>
          units::drop_units()

        revtrack[is_sp, "range_status"] <- ifelse(
          obs_dist_to_range < range_buffer,
          #obs_dist_to_range < sqrt(162), #diagonal of EST 9-km grid cell
          #obs_dist_to_range < sqrt(.Machine[["double.eps"]]),
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


    # write freq table with # species records in and out of range
    if (file.exists(fname_out[["rangeinout_freq"]])) {
      warning(
        "File ", shQuote(basename(fname_out[["rangeinout_freq"]])),
        " already exists."
      )
    } else {
      write.csv(
        table(revtrack[, "common_name"], revtrack[, "range_status"]),
        #table(revtrack[, "common_name"], revtrack[, c("range_status", "orig_breeding_category")]),
        file = fname_out[["rangeinout_freq"]],
        row.names = TRUE
      )
    }


    #----*--plot single species map----
    if (FALSE) {
      stopifnot(
        requireNamespace("ggplot2"),
        requireNamespace("maps")
      )

      # plot records out of range
      sp_to_plot <- "Blackpoll Warbler"

      ids <-
        revtrack[, "common_name"] %in% sp_to_plot &
        revtrack[, "range_status"] %in% "rangeout"

      tmp_cdat <- sf::st_as_sf(
        cdat[ids, ],
        coords = c("longitude", "latitude"),
        crs = 4326
      )

      tmp <- sf::st_bbox(tmp_cdat)
      sp_lims <- list(xlim = tmp[c("xmin", "xmax")], ylim = tmp[c("ymin", "ymax")])

      sp_code <- dates[dates[, "common_name"] == sp_to_plot, "species_code"]
      fname_range <- file.path(
        dir_ebird_ranges,
        paste0(sub("/", "_", sp_to_plot), "-breeding-range-smooth-9k-2024-NY.gpkg")
      )

      sp_range <- sf::st_read(
        fname_range,
        layer = "range",
        quiet = TRUE
      )

      ggplot2::ggplot() +
        ggplot2::geom_sf(
          data = tmp_cdat,
          pch = 4,
          ggplot2::aes(color = breeding_category)
        ) +
        ggplot2::guides(color = ggplot2::guide_legend(title = "")) +
        ggplot2::geom_sf(data = sp_range, fill = NA) +
        ggplot2::borders("state", fill = NA, show.legend = FALSE) + # requires "maps"
        ggplot2::coord_sf(
          xlim = sp_lims[["xlim"]],
          ylim = sp_lims[["ylim"]],
          expand = TRUE
        ) +
        ggplot2::labs(title = paste0(sp_to_plot, ": out of range")) +
        ggplot2::theme_bw()
    }



    ##-----* Check Time of Breeding Seasons-----

    #early_season = Jan 1 until Pre-breeding start date
    #prebreeding = Pre-breeding start date until Core breeding start date
    #breeding = Core breeding start date until Core breeding end date
    #postbreeding = Core breeding end date until Post-breeding end date
    #late_season = Post-breeding end date until 31 Dec

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
      is_sp <- cdat[, "common_name"] %in% species[k]
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
        obs_doy <- 1 + as.POSIXlt(cdat[is_sp, "observation_date"])$yday
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

    # write freq table with # species records in each breeding season
    if (file.exists(fname_out[["breedingseason_freq"]])) {
      warning(
        "File ", shQuote(basename(fname_out[["breedingseason_freq"]])),
        " already exists."
      )
    } else {
      write.csv(
        table(revtrack[, "common_name"], revtrack[, "breeding_season"]),
        #table(revtrack[, "common_name"], revtrack[, c("breeding_season", "orig_breeding_category")]),
        file = fname_out[["breedingseason_freq"]],
        row.names = TRUE
      )
    }


    ##------* Flag Spatiotemporal Issues ------

    #--- update breeding codes with code changes arising from code usage review cat3
    current_breeding_code <- revtrack[, "orig_breeding_code"]
    ids <- revtrack[, "code_review_cat"] %in% "3"
    current_breeding_code[ids] <- revtrack[ids, "code_review_new_code"]


    ##-----* Flag Migratory Species-----

    is_migrant <- revtrack[, "resident_status"] %in% "migrant"

    #------classify records into 3 review categories

    #--- cat1 = Confident is breeding, not essential to review
    #prebreeding, in range, all evidence categories
    #breeding, in range, all evidence categories
    #postbreeding, in range, all evidence categories

    is_cat1_migrant <-
      is_migrant &
      revtrack[, "range_status"] %in% "rangein" &
      revtrack[, "breeding_season"] %in% c("prebreeding", "core_breeding", "postbreeding")

    revtrack[is_cat1_migrant, "st_review_cat"] <- 1
    revtrack[is_cat1_migrant, "st_review_status"] <- "migrant_inrange_inseason"


    #--- cat2 = Uncertain, review needed

    # early_season, any range, confirmed
    is_cat2_early_migrant <-
      is_migrant &
      current_breeding_code %in% confirmed &
      revtrack[, "breeding_season"] %in% "early_season"

    revtrack[is_cat2_early_migrant, "st_review_cat"] <- 2
    revtrack[is_cat2_early_migrant, "st_review_status"] <- "migrant_early_co"

    # out of range, all evidence categories, prebreeding, core, or postbreeding
    is_cat2_out_migrant <-
      is_migrant &
      revtrack[, "range_status"] %in% "rangeout" &
      revtrack[, "breeding_season"] %in% c("prebreeding", "core_breeding", "postbreeding")

    revtrack[is_cat2_out_migrant, "st_review_cat"] <- 2
    revtrack[is_cat2_out_migrant, "st_review_status"] <- "migrant_outrange_inseason"

    # late_season, any range, confirmed
    is_cat2_late_migrant <-
      is_migrant &
      current_breeding_code %in% confirmed &
      revtrack[, "breeding_season"] %in% "late_season"

    revtrack[is_cat2_late_migrant, "st_review_cat"] <- 2
    revtrack[is_cat2_late_migrant, "st_review_status"] <- "migrant_late_co"


    #--- cat3 = Likely not breeding, not essential to review

    # early_season, any range, possible & probable
    is_cat3_tooearly_migrant <-
      is_migrant &
      current_breeding_code %in% c(possible, probable) &
      revtrack[, "breeding_season"] %in% "early_season"

    revtrack[is_cat3_tooearly_migrant, "st_review_cat"] <- 3
    revtrack[is_cat3_tooearly_migrant, "st_review_status"] <- "migrant_early_popr"

    # late_season, any range, possible & probable
    is_cat3_toolate_migrant <-
      is_migrant &
      current_breeding_code %in% c(possible, probable) &
      revtrack[, "breeding_season"] %in% "late_season"

    revtrack[is_cat3_toolate_migrant, "st_review_cat"] <- 3
    revtrack[is_cat3_toolate_migrant, "st_review_status"] <- "migrant_late_popr"

    #--- make exceptions for solid confirmed codes

    #migrants in range - early and late
    is_migrant_inrange_solid <-
      is_migrant &
      current_breeding_code %in% solid_codes &
      revtrack[, "range_status"] %in% "rangein" &
      revtrack[, "breeding_season"] %in% c("early_season", "late_season")

    revtrack[is_migrant_inrange_solid, "st_review_cat"] <- 1
    revtrack[is_migrant_inrange_solid, "st_review_status"] <-
      "migrant_inrange_earlylate_solidco"

    #migrants out range - pre, core, post
    is_migrant_outrange_solid <-
      is_migrant &
      current_breeding_code %in% solid_codes &
      revtrack[, "range_status"] %in% "rangeout" &
      revtrack[, "breeding_season"] %in% c("prebreeding", "core_breeding", "postbreeding")

    revtrack[is_migrant_outrange_solid, "st_review_cat"] <- 1
    revtrack[is_migrant_outrange_solid, "st_review_status"] <-
      "migrant_outrange_inseason_solidco"

    #--- make exception for UN codes
    #UN codes are acceptable in all seasons
    is_migrant_inrange_UN <- is_migrant &
      current_breeding_code %in% "UN" &
      revtrack[, "range_status"] %in% "rangein"

    revtrack[is_migrant_inrange_UN, "st_review_cat"] <- 1
    revtrack[is_migrant_inrange_UN, "st_review_status"] <- "migrant_inrange_UN"

    #--- check that all migrants assigned to a category
    if (FALSE) {
      table(
        revtrack[is_migrant, "st_review_cat"],
        revtrack[is_migrant, "st_review_status"],
        useNA = "always"
      )
    # migrant_inrange_earlylate_solidco = 5213
    # migrant_outrange_inseason_solidco = 140
    # migrant_inrange_UN = 2870
    }



    ##-----* Flag Resident Species-----

    is_resident <- revtrack[, "resident_status"] %in% "resident"

    #------classify records into 3 review categories

    #--- cat1 = Confident is breeding, not essential to review

    # In range, all evidence categories
    is_cat1_resident <-
      is_resident &
      revtrack[, "range_status"] %in% "rangein"

    revtrack[is_cat1_resident, "st_review_cat"] <- 1
    revtrack[is_cat1_resident, "st_review_status"] <- "resident_inrange"


    #--- cat2 = Uncertain, review needed

    # Out of range, probable & confirmed
    is_cat2_out_resident <-
      is_resident &
      revtrack[, "range_status"] %in% "rangeout" &
      current_breeding_code %in% c(probable, confirmed)

    revtrack[is_cat2_out_resident, "st_review_cat"] <- 2
    revtrack[is_cat2_out_resident, "st_review_status"] <-
      "resident_outrange_prco"

    # Out of range, possible, core season
    is_resident_outrange_po_core <-
      is_resident &
      revtrack[, "range_status"] %in% "rangeout" &
      revtrack[, "breeding_season"] %in% "core_breeding" &
      current_breeding_code %in% possible

    revtrack[is_resident_outrange_po_core, "st_review_cat"] <- 2
    revtrack[is_resident_outrange_po_core, "st_review_status"] <-
      "resident_outrange_po_core"

    #--- cat3 = Likely not breeding, not essential to review

    # Out of range, possible, all but core season
    is_resident_outrange_po_notcore <-
      is_resident &
      revtrack[, "range_status"] %in% "rangeout" &
      revtrack[, "breeding_season"] %in% c(
        "prebreeding", "postbreeding", "early_season", "late_season"
      ) &
      current_breeding_code %in% possible

    revtrack[is_resident_outrange_po_notcore, "st_review_cat"] <- 3
    revtrack[is_resident_outrange_po_notcore, "st_review_status"] <-
      "resident_outrange_po_notcore"


    #--- make exceptions for solid confirmed codes

    #residents out range - pre, core, post
    is_resident_outrange_solid <-
      is_resident &
      current_breeding_code %in% solid_codes &
      revtrack[, "range_status"] %in% "rangeout" &
      revtrack[, "breeding_season"] %in% c("prebreeding", "core_breeding", "postbreeding")

    revtrack[is_resident_outrange_solid, "st_review_cat"] <- 1
    revtrack[is_resident_outrange_solid, "st_review_status"] <-
      "resident_outrange_inseason_solidco"


    #--- check that all residents assigned to a category
    if (FALSE) {
      table(
        revtrack[is_resident, "st_review_cat"],
        revtrack[is_resident, "st_review_status"],
        useNA = "always"
      )
    #resident_outrange_inseason_solidco = 0
    #resident_outrange_prco = 58
    }



    #------* Handle nd range without assigned st category------
    is_uncat_nd <-
      revtrack[, "range_status"] %in% "nd" &
      is.na(revtrack[, "st_review_cat"])

    revtrack[is_uncat_nd, "st_review_cat"] <- 2
    revtrack[is_uncat_nd, "st_review_status"] <- "unknownrange"



    #------* Separate cat2 into 2a and 2b------
    is_cat2 <- revtrack[, "st_review_cat"] %in% 2
    revtrack[is_cat2 & revtrack[, "has_support"], "st_review_cat"] <- "2a"
    revtrack[is_cat2 & !revtrack[, "has_support"], "st_review_cat"] <- "2b"



    #------* Check all records assigned to a review category----

    # there should not be any NAs in these columns
    stopifnot(!anyNA(revtrack[, "st_review_cat"]))
    stopifnot(!anyNA(revtrack[, "st_review_status"]))

    if (FALSE) {
      # residents
      table(
        revtrack[is_resident, "st_review_cat"],
        revtrack[is_resident, "st_review_status"],
        useNA = "always"
      )

      # migrants
      table(
        revtrack[is_migrant, "st_review_cat"],
        revtrack[is_migrant, "st_review_status"],
        useNA = "always"
      )

      # unknown ranges
      table(
        revtrack[is_uncat_nd, "st_review_cat"],
        revtrack[is_uncat_nd, "st_review_status"],
        useNA = "always"
      )

      # all by review category and date flag
      table(
        revtrack[, "st_review_cat"],
        revtrack[, "st_review_status"],
        useNA = "always"
      )
    }

    # look at all flags for code usage and spatiotemporal issues combined
    if (file.exists(fname_out[["st_errors"]])) {
      warning(
        "File ", shQuote(basename(fname_out[["st_errors"]])),
        " already exists."
      )

    } else {
      write.csv(
        addmargins(table(
          st = revtrack[, "st_review_cat"],
          code = revtrack[, "code_review_cat"],
          useNA = "always"
        )),
        file = fname_out[["st_errors"]],
        row.names = TRUE
      )
    }



    ##-----Phase 3: Process Records-----

    #Process all records based on review category

    # first triple check that no NAs in key fields
    tmp_vars_nona <- c("rarity_status", "code_review_cat", "st_review_cat")
    stopifnot(!anyNA(revtrack[, tmp_vars_nona]))


    #----* no action needed----
    # cat1 (Confident is breeding) -> no further review action needed
    # note: may include code usage cat3 that were automatically changed!
    is_no_action <-
      #revtrack[, "rarity_status"] %in% "other" &
      revtrack[, "code_review_cat"] %in% 1 &
      revtrack[, "st_review_cat"] %in% 1

    revtrack[is_no_action, "review_cat"] <- 1
    revtrack[is_no_action, "review_status"] <- "approved"
    revtrack[is_no_action, "review_date"] <- format(Sys.Date())

    is_no_action_but_already_changed <-
      #revtrack[, "rarity_status"] %in% "other" &
      revtrack[, "code_review_cat"] %in% 3 &
      revtrack[, "st_review_cat"] %in% 1

    revtrack[is_no_action_but_already_changed, "review_cat"] <- 1
    stopifnot(
      revtrack[is_no_action_but_already_changed, "review_status"] == "changed"
    )


    #----* flag records for manual review----

    # cat2a (Uncertain) OR (cat1 & rare_breeder) -> export for manual review
    is_manual_review <-
      (
      #   revtrack[, "rarity_status"] %in% "rare_breeder" &
      #   revtrack[, "code_review_cat"] %in% c(1, 3) &
      #   revtrack[, "st_review_cat"] %in% 1
      # ) | (
        revtrack[, "code_review_cat"] %in% "2a" &
        revtrack[, "st_review_cat"] %in% c("1", "2a")
      ) | (
        revtrack[, "code_review_cat"] %in% c("1", "2a", 3) &
        revtrack[, "st_review_cat"] %in% "2a"
      )

    revtrack[is_manual_review, "review_cat"] <- "2a"
    revtrack[is_manual_review, "review_status"] <- "pending"


    #----* change breeding code----
    # cat2b_as_b (Uncertain in code but no supporting documentation) --> change breeding code
    is_cat2bb <-
      revtrack[, "code_review_cat"] %in% "2b" &
      revtrack[, "st_review_cat"] %in% 1

    revtrack[is_cat2bb, "review_cat"] <- "2b_b"


    # cat2b_as_3 (Uncertain in space and/or time)
    is_cat2b3 <-
      revtrack[, "st_review_cat"] %in% "2b"

    revtrack[is_cat2b3, "review_cat"] <- "2b_3"


    # cat3 (Likely not breeding)
    is_cat3 <-
      revtrack[, "st_review_cat"] %in% 3

    revtrack[is_cat3, "review_cat"] <- 3

    # overview of review categories
    if (FALSE) {
      table(revtrack[, "review_cat"], useNA = "always")
      is_manual <- revtrack[, "review_cat"] %in% "2a"
      table(revtrack[is_manual, "common_name"])
    }

    # inspect a single species
    if (FALSE) {
      is_tmp <- is_manual & revtrack[, "common_name"] %in% "Purple Martin"
      table(revtrack[is_tmp, c("st_review_status", "range_status")])
    }



    #------ Phase 4: Automatic Changes ------
    # code changes for review_cat 2b_b, 2b_3, and 3

    #--- Automatically change review category 2b_b (following brc_changes2cat)
    # Use same R code as for brc_changes3cat

    is_cat2bb <- revtrack[, "review_cat"] %in% "2b_b"

    tmp <- merge(
      x = cdat[, c("global_unique_identifier", brc_vars)],
      y = brc_usage,
      all.x = TRUE,
      all.y = FALSE
    )

    ids <- match(
      revtrack[is_cat2bb, "global_unique_identifier"],
      tmp[, "global_unique_identifier"],
      nomatch = 0
    )

    ids2 <- is_cat2bb[ids > 0]
    revtrack[ids2, c("st_review_new_code", "change_reason")] <-
      tmp[ids, c("new_code", "change_reason")]
    revtrack[ids2, "review_status"] <- "changed"
    revtrack[ids2, "review_date"] <- format(Sys.Date())



    #--- Automatically change review categories 2b_3 and 3 (Likely not breeding)

    # --> Change breeding code to "NC" (for no code) and use reason codes as follows:
    # Migrants - early_season -> "tooearly
    # Migrants - late_season -> "toolate"
    # Residents -> "notlikely"
    is_cat3 <- revtrack[, "review_cat"] %in% c("2b_3", "3")

    revtrack[is_cat3, "st_review_new_code"] <- "NC"
    revtrack[is_cat3, "review_status"] <- "changed"
    revtrack[is_cat3, "review_date"] <- format(Sys.Date())

    is_migrant <- revtrack[, "resident_status"] %in% "migrant"

    is_cat3_tooearly_migrant <-
      is_cat3 &
      is_migrant &
      revtrack[, "st_review_status"] %in% c("migrant_early_co", "migrant_early_popr")
    revtrack[is_cat3_tooearly_migrant, "change_reason"] <- "tooearly"

    is_cat3_toolate_migrant <-
      is_cat3 &
      is_migrant &
      revtrack[, "st_review_status"] %in% c("migrant_late_co", "migrant_late_popr")
    revtrack[is_cat3_toolate_migrant, "change_reason"] <- "toolate"

    is_cat3_outrange_migrant <-
      is_cat3 &
      is_migrant &
      revtrack[, "st_review_status"] %in% "migrant_outrange_inseason"
    revtrack[is_cat3_outrange_migrant, "change_reason"] <- "notlikely"

    is_resident <- revtrack[, "resident_status"] %in% "resident"

    is_cat3_outrange_resident <-
      is_cat3 &
      is_resident &
      revtrack[, "st_review_status"] %in% c(
        "resident_outrange_prco", "resident_outrange_po_notcore",
        "resident_outrange_po_core", "unknownrange"
      )
    revtrack[is_cat3_outrange_resident, "change_reason"] <- "notlikely"


    stopifnot(!anyNA(revtrack[is_cat3, "change_reason"]))

    if (FALSE) {
      table(revtrack$common_name[is_cat3 & is.na(revtrack$change_reason)])
      #none
    }


    ## see plot_observations_for_review.R for code to add breeding category


    #---* Get highest code per species by block----

    #--situations where original breeding code is good
    revtrack$orig_breeding_code <- factor(
      revtrack$orig_breeding_code,
      levels = code_order,
      ordered = TRUE
    )

    is_good_code1 <- revtrack$review_cat %in% 1 &
      revtrack$code_review_cat %in% 1

    sp_by_block <- revtrack[is_good_code1, , drop = FALSE] |>
      dplyr::group_by(common_name, atlas_block) |>
      dplyr::summarize(max_block_code = max(orig_breeding_code))

    revtrack <- dplyr::left_join(
      x = revtrack,
      y = sp_by_block,
      by = dplyr::join_by(common_name, atlas_block)
    )


    #---* Check revtrack for completeness----
    tmp_vars_nona <- c(
      "code_review_cat", "st_review_cat", "review_cat",
      "has_support", "rarity_status",
      "range_status", "resident_status", "breeding_season", "st_review_status"
    )

    for (var in tmp_vars_nona) {
      if (anyNA(revtrack[, var])) {
        warning("Missing values in revtrack for ", shQuote(var))
      }
    }

    stopifnot(!anyNA(revtrack[, tmp_vars_nona]))


    is_changed <- revtrack[, "review_status"] %in% "changed"
    tmp_vars_nona2 <- c(
      "review_status", "change_reason", "review_date"
    )

    for (var in tmp_vars_nona2) {
      if (anyNA(revtrack[is_changed, var])) {
        warning("Missing values in revtrack for ", shQuote(var))
      }
    }

    stopifnot(!anyNA(revtrack[is_changed, tmp_vars_nona2]))

    if (FALSE) {
      table(revtrack[, "review_cat"], revtrack[, "review_status"], useNA = "always")
      #      approved changed pending    <NA>
      # 1     2789048   26824       0       0
      # 2a          0       0   13285       0
      # 2b_3        0   24971       0       0
      # 2b_b        0   14796       0       0
      # 3           0  441181       0       0
      # <NA>        0       0       0       0
      table(revtrack[, "review_cat"], revtrack[, "change_reason"], useNA = "always")
      #      bcinadqt codenotapp ddnotapp nbnotapp notlikely samecat tooearly toolate vocnotapp    <NA>
      # 1           0       4457       27     1105         0       0        0       0     21235 2789048
      # 2a          0          3        0        2         0       0        0       0        16   13264
      # 2b_3        0          0        0        0     15794       0     2707    6470         0       0
      # 2b_b    14616          0        0        0         0     180        0       0         0       0
      # 3           0          0        0        0        56       0   388095   53030         0       0
      # <NA>        0          0        0        0         0       0        0       0         0       0
      table(revtrack[, "review_cat"], revtrack[, "review_date"], useNA = "always")
      #      2026-03-25    <NA>
      # 1       2815872       0
      # 2a           21   13264
      # 2b_3      24971       0
      # 2b_b      14796       0
      # 3        441181       0
      # <NA>          0       0
    }


  #--- save review tracker
  saveRDS(revtrack, file = fname_out[["reviewtrack_tmp"]])
  # and as csv for sharing
  tmp <- sub(".rds$", ".csv", fname_out[["reviewtrack_tmp"]])
  if (!file.exists(tmp)) {
    dat_for_sharing <- merge(
      x = revtrack,
      y = cdat,
      by = c("global_unique_identifier", "common_name"),
      all = FALSE,
      sort = FALSE
    )
    stopifnot(nrow(dat_for_sharing) == nrow(revtrack))

    write.csv(
      dat_for_sharing,
      file = tmp,
      row.names = FALSE
    )
  }



  #------ end of phases 1-4
  }


  #------ Phase 5: Manual Review ------

  # ---* Write out manual review portion----
  #(review_cat 2a, i.e., includes rare 1s)
  is_manual_review <- revtrack[, "review_cat"] %in% "2a"
  sum(is_manual_review)
  #13285 (more than halved from the 35,838 with rare breeders)

  #avoid duplicated columns
  exclude_cols <- c("observation_date", "sampling_event_identifier",
    "atlas_block", "block_county", "latitude", "longitude")

  dat_for_review <- merge(
    revtrack[is_manual_review, ],
    cdat[is_manual_review, !(names(cdat) %in% exclude_cols)],
    by = c("global_unique_identifier", "common_name"),
    all = FALSE,
    sort = FALSE
  )
  stopifnot(
    nrow(dat_for_review) == sum(is_manual_review),
    revtrack[is_manual_review, "global_unique_identifier"] ==
      dat_for_review[, "global_unique_identifier"]
  )

  if (!file.exists(fname_out[["records_for_manual_review"]])) {
    write.csv(
      dat_for_review,
      file = fname_out[["records_for_manual_review"]],
      row.names = FALSE
    )

  } else {
    # Check that version on disk is identical to current version
    dat_for_review_ondisk <- read.csv(fname_out[["records_for_manual_review"]])

    if (!isTRUE(all.equal(dat_for_review, dat_for_review_ondisk))) {
      stop("Records for manual review differs from version stored on disk.")
    }
  }
}


#------ xxxxxx ------


#---* Conduct manual review----
if (!file.exists(fname_inputs[["records_manually_reviewed"]])) {

  stop("No file for manually reviewed records")

} else {

  #---* Read in revtrack and add manual review outcome----
  revtrack <- readRDS(fname_out[["reviewtrack_tmp"]])
  revtrack$review_date <- as.Date(revtrack$review_date, format = "%Y-%m-%d")

  data_was_manually_reviewed <- as.data.frame(
    readxl::read_excel(
      fname_inputs[["records_manually_reviewed"]],
      sheet = "man_review",
      guess_max = 5000
    )
  )
  stopifnot(!anyNA(data_was_manually_reviewed[, "manual_new_code"]))
  #drop hyperlink column (was for excel only) so df's match
  data_was_manually_reviewed <-
    data_was_manually_reviewed[, !(names(data_was_manually_reviewed) %in% "hyperlink")]
  data_was_manually_reviewed$review_date <-
    as.Date(data_was_manually_reviewed$review_date)

  #--fix GWWA records that should be slashes
  sei_slashes <- c("S177444882", "S220709034", "S220717224", "S175967503")
  data_was_manually_reviewed$common_name[
    data_was_manually_reviewed$common_name == "Golden-winged Warbler" &
      data_was_manually_reviewed$sampling_event_identifier %in% sei_slashes
  ] <- "Golden-winged/Blue-winged Warbler"

  # #--check that special observations retained in review tracker
  # # Tom Salo's WEWA in Oneida County 5/17/24: S175117725
  # revtrack[
  #   revtrack$common_name %in% "Worm-eating Warbler" &
  #   revtrack$sampling_event_identifier %in% "S175117725", ] #retained
  # # Shai Mitra's Royal Terns
  # revtrack[revtrack$common_name %in% "Royal Tern", ] #some have codes

  # are there any records that were flagged for review that haven't been reviewed?
  #i.e., are there obs in latest man review list not in data man reviewed?
  stopifnot(
    length(
      setdiff(
        dat_for_review$global_unique_identifier,
        data_was_manually_reviewed$global_unique_identifier)
    ) == 0
  )

  #merge with review tracker
  ids <- base::match(
    x = revtrack[["global_unique_identifier"]],
    table = data_was_manually_reviewed[["global_unique_identifier"]],
    nomatch = 0
  )
  ids2 <- ids > 0

  tmp_mvar <- c("manual_new_code", "change_reason", "review_date")
  revtrack[ids2, tmp_mvar] <- data_was_manually_reviewed[ids, tmp_mvar]

  #check if new code
  has_new <- !is.na(revtrack[, "manual_new_code"]) &
    revtrack[, "manual_new_code"] != revtrack[, "orig_breeding_code"]
  revtrack[has_new, "review_status"] <- "reviewed-changed"

  #check if original code approved
  has_old <- !is.na(revtrack[, "manual_new_code"]) &
    revtrack[, "manual_new_code"] == revtrack[, "orig_breeding_code"]
  revtrack[has_old, "review_status"] <- "reviewed-approved"



  #------ Phase 6: Final Review Outcome ------

  #accommodate NC = no code and W = wide-ranging
  code_order2 <- c(
    NA, "NC", "F", "W", "H", "S", "S7", "M", "P", "T", "C", "N", "A", "B", "PE",
    "CN", "NB", "DD", "UN", "ON", "FL", "CF", "FY", "FS", "NE", "NY"
  )
  revtrack$orig_breeding_code <- factor(
    revtrack$orig_breeding_code,
    levels = code_order2,
    ordered = TRUE
  )
  current_breeding_code <- revtrack[, "orig_breeding_code"]


  #---* Changes from phase 1 ----
  # (code usage review): automatic changes from cat3
  ids1 <- revtrack[, "code_review_cat"] %in% "3"
  current_breeding_code[ids1] <- revtrack[ids1, "code_review_new_code"]


  #---* Changes from phases 2-3 ----
  # (review processing): automatic changes from cat2b_b, cat2b_3, and cat3
  ids3 <- revtrack[, "review_cat"] %in% c("2b_b", "2b_3", "3")
  current_breeding_code[ids3] <- revtrack[ids3, "st_review_new_code"]

  # Tabulate twice changed codes (overlap of phase 1 and 2/3)
  table(
    usage = revtrack[ids1 & ids3, "code_review_new_code"],
    st = revtrack[ids1 & ids3, "st_review_new_code"],
    useNA = "always"
  )


  #---* Changes from phase 4 ----
  # (manual review): cat2a
  ids4 <- revtrack[, "review_cat"] %in% "2a"
  current_breeding_code[ids4] <- revtrack[ids4, "manual_new_code"]


  #---* Assign final code / category----
  stopifnot(!anyNA(current_breeding_code))
  revtrack[, "final_code"] <- current_breeding_code

  revtrack$final_category[revtrack$final_code %in% observed] <- "observed"
  revtrack$final_category[revtrack$final_code %in% possible] <- "possible"
  revtrack$final_category[revtrack$final_code %in% probable] <- "probable"
  revtrack$final_category[revtrack$final_code %in% confirmed] <- "confirmed"
  stopifnot(!anyNA(revtrack$final_category))



  #---* Save final revtrack----
  saveRDS(revtrack, file = fname_out[["reviewtrack_final"]])
}


