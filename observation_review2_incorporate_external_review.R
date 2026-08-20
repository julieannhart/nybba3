# Process records that were reviewed with Shiny tool
#
# PURPOSE:
# Read review log from external review
# Identify records with multiple reviews
# Merge with observation data
# Export data for second manual review
# Read back in second manually reviewed data
# Create and export final dataset
#
# Can process species in batches or all at once
# If process in batches, need to add batch number to species/dates list
#
# Script written by Julie A. Hart
# last edited 8/20/2026



#---- Settings----

#--process type
#run in batches or all data combined
#c("combined", "batch")
process_type <- "combined"

#if batch, which batch to process
batch_set <- 6



#---- Set directories----

dir_prj <- ".."
dir_dat <- file.path(dir_prj, "..", "data")
dir_spat <- file.path(dir_prj, "..", "..", "Spatial_data")
dir_out <- file.path(dir_prj, "2026_review", "output")
dir_shiny <- file.path(dir_prj, "..", "shiny_app_for_data_review")



#---- Read in data----

#--species to focus on

#get list of species from batch column in most recent expected dates file
dates <- read.csv(file.path(dir_dat, "expected_dates_2026-06-01.csv"))

if (process_type == "combined") {

  spp <- unique(dates$common_name)

} else if (process_type == "batch") {

  batch <- dates[dates$batch %in% batch_set, ]
  spp <- unique(batch$common_name)
}



#--review log

if (process_type == "combined") {

  log <- read.csv(file.path(dir_shiny, "data/combined_log.csv"))

} else if (process_type == "batch") {

  #--read in multiple logs and combine
  log1 <- read.csv(file.path(dir_out, "review_log_2026-06-15.csv"))
  log2 <- read.csv(file.path(dir_out, "review_log_2026-07-02.csv"))
  log3 <- read.csv(file.path(dir_shiny, "data/review_log.csv"))
  log <- dplyr::bind_rows(log1, log2, log3)
  log <- log[order(log$obs_id), ]

  #export combined log for local shiny app
  write.csv(
    log_unique,
    file.path(dir_prj, "dir_shiny", "data", "combined_log.csv"),
    row.names = FALSE
  )
}

#remove dupe log entries if all cols identical
log_unique <- log |> dplyr::distinct()
nrow(log_unique) #30201



#--data after internal manual review
#data after running through updated filters and first manual review
alldat <- readRDS(
  file.path(dir_out, "review_tracking_final_yr2026_v20260813.rds")
)
nrow(alldat) #3310126

#fix the dates
alldat <- alldat |>
  dplyr::mutate(
    #convert text values to numbers
    numeric_days = as.numeric(review_date),
    #convert numbers to dates using 1970 Unix origin
    converted_dates = as.Date(numeric_days, origin = "1970-01-01"),
    #parse the existing date strings into dates
    existing_dates = as.Date(review_date, format = "%Y-%m-%d"),
    #merge:: use existing date if available, otherwise the converted one
    clean_date = dplyr::coalesce(existing_dates, converted_dates)
  ) |>
  #replace old column with the clean date and remove tmp columns
  dplyr::mutate(review_date = clean_date) |>
  dplyr::select(-numeric_days, -converted_dates, -existing_dates, -clean_date)


if (FALSE) {
  #exploration
  alldat[
    alldat$common_name %in% "Lesser Scaup" &
    alldat$block_county %in% c("Jefferson") &
    alldat$breeding_season %in% c("core_breeding"),
    #&
    #alldat$orig_breeding_code %in% "S",
  ]
}


# drop unneeded columns
dropcols <- c("final_category", "manual_new_code", #"orig_breeding_code",
  "max_block_cat_factor", "max_block_code_char",
  "max_block_cat_char")
alldat <- alldat[, !names(alldat) %in% dropcols]

# subset to this batch of species
alldat <- alldat[alldat$common_name %in% spp, ]



#--------- SKIP: if processing second review -----------


#---- Merge review log with obs data ----

# append alldat fields to log file
log_plus <- dplyr::left_join(
  x = log_unique,
  y = alldat,
  by = dplyr::join_by(obs_id == global_unique_identifier),
  keep = FALSE
)
log_plus <- log_plus[log_plus$common_name %in% spp, ]


if (FALSE) {
  #testing
  alldat[alldat$common_name %in% "Olive-sided Flycatcher" & alldat$block_county %in% "Monroe", ]
  log_plus[log_plus$common_name %in% "Olive-sided Flycatcher" & log_plus$block_county %in% "Monroe", ]
  dat[dat$common_name %in% "Olive-sided Flycatcher" & dat$block_county %in% "Monroe", ]
  log_plus[log_plus$sampling_event_identifier %in% "S176260289", ]
}



#---- Identify repeat reviews ----

# have any records been reviewed multiple times?
if (sum(duplicated(log_plus$obs_id)) > 0) {

  # which records have been reviewed multiple times?
  dupe_ids <- which(
    duplicated(log_plus$obs_id) |
    duplicated(log_plus$obs_id, fromLast = TRUE)
  )

  # initialize results column
  log_plus$multi_flag <- NA

  # assess if they are reviewed the same way
  lid <- unique(log_plus$obs_id[dupe_ids])

  #for each duplicated id
  for (l in lid) {

    #check if decision and reason are the same
    codes <- log_plus$decision[log_plus$obs_id == l]
    reasons <- log_plus$reason[log_plus$obs_id == l]

    if (length(unique(codes)) > 1 | length(unique(reasons)) > 1) {
      log_plus$multi_flag[log_plus$obs_id %in% l] <- "multiple_differ"
    } else {
      log_plus$multi_flag[log_plus$obs_id %in% l] <- "multiple_same"
    }
  }
}



#---- Identify recommended changes ----

#which obs have differing final_code vs external review decision
diff_ids <- log_plus$decision != log_plus$final_code
log_plus$second_review_needed <- FALSE
log_plus$second_review_needed[diff_ids] <- TRUE



#---- Add columns ----
codedobs <- readRDS(
  file.path(
    dir_out,
    "coded_obs_on_good_checklists_yr2026_v20260614.rds"
  )
)
cols_codedobs <- c("global_unique_identifier", "observation_count",
  "species_comments", "checklist_comments", "has_media", "group_identifier",
  "source")
log_plus <- dplyr::left_join(
  x = log_plus,
  y = codedobs[, cols_codedobs],
  by = dplyr::join_by(obs_id == global_unique_identifier),
  keep = FALSE
)



#---- Export for 2nd review -----

#subset to records needing review
#where multi_flag = "multi_differ" or second_review_needed = T

export_ids <-
  log_plus$multi_flag %in% "multiple_differ" |
  log_plus$second_review_needed

second_review <- log_plus[export_ids, ]

write.csv(
  second_review,
  file.path(dir_out, paste0("second_manual_review_", Sys.Date(), ".csv")),
  row.names = FALSE
)



#---------STOP: Work in Excel-----------

# insert columns for final_final_code and final_final_reason

# if an obs is reviewed multiple times, mark those that should be deleted
#    with "remove" in the final_final_code column



#---------START: Read in second review -----------


#---- Read in 2nd manual review -----

if (process_type == "batch") {

  #get file name
  if (batch_set == 1) {
    fsheet <- "second_manual_review_2026-06-01"
  } else if (batch_set == 2) {
    fsheet <- "second_manual_review_2026-06-15"
  } else if (batch_set == 3) {
    fsheet <- "second_manual_review_2026-06-12"
  } else if (batch_set == 4) {
    fsheet <- "second_manual_review_2026-07-02"
  } else if (batch_set == 5) {
    fsheet <- "second_manual_review_2026-07-27"
  } else if (batch_set == 6) {
    fsheet <- "second_manual_review_2026-08-13"
  }

  df <- as.data.frame(
    readxl::read_excel(
      file.path(dir_out, paste0(fsheet, ".xlsx")),
      sheet = fsheet,
      guess_max = 5000
    )
  )

  #drop species that still have issues
  if (batch_set == 3) {
    sp2drop <- c("Turkey Vulture", "Great Blue Heron",
      "Double-crested Cormorant", "Black Tern", "Black-crowned Night Heron",
      "American Herring Gull")
    #have Irene review BLTE and DCCO
    #have Mike M review BCNH
    #have Matt and Kathy review TUVU and GBHE
  } else if (batch_set == 2) {
    sp2drop <- c("Ring-billed Gull", "Common Nighthawk")
    # RBGU -- too many
    # CONI -- too many in west?
  } else if (batch_set == 4) {
    sp2drop <- c("Magnolia Warbler", "Field Sparrow", "Evening Grosbeak")
    # MAWA -- L Ontario coast
    # FISP -- NYC
    # EVGR -- ask Matt Young to review
  } else if (batch_set == 5){
    sp2drop <- c("Yellow-bellied Flycatcher", "Ring-billed Gull",
      "Red-breasted Merganser", "Lesser Scaup", "Double-crested Cormorant",
      "Bobolink", "American Herring Gull")
    # YBFL -- out of range obs
    # RBGU -- restrict to prob and conf or use W code
    # RBME -- too many records
    # LESC -- should be no records??
    # DCCO -- restrict to prob and conf or use W code
    # BOBO -- NYC & LI records?
    # AHGU -- restrict to prob and conf or use W code
  }
  df <- df[!df$common_name %in% sp2drop, ]

} else if (process_type == "combined") {

  #set file names
  f <- c(
    "second_manual_review_2026-06-01",
    "second_manual_review_2026-06-15",
    "second_manual_review_2026-06-12",
    "second_manual_review_2026-07-02",
    "second_manual_review_2026-07-27",
    "second_manual_review_2026-08-13"
  )

  #read in and combine files
  df <- data.frame()

  for (i in f) {
    tmp <- as.data.frame(
      readxl::read_excel(
        file.path(dir_out, paste0(i, ".xlsx")),
        sheet = i,
        guess_max = 5000
      )
    )

    #drop species that were later dropped b/c of issues
    if (i == f[1]) {
      sp2drop <- c("Eastern Whip-poor-will", "Killdeer")
    } else if (i == f[2]) {
      sp2drop <- c("Ring-billed Gull", "Common Nighthawk")
    } else if (i == f[3]) {
      sp2drop <- c("Turkey Vulture", "Great Blue Heron",
        "Double-crested Cormorant", "Black Tern", "Black-crowned Night Heron",
        "American Herring Gull")
    } else if (i == f[4]) {
      sp2drop <- c("Magnolia Warbler", "Field Sparrow", "Evening Grosbeak")
    } else if (i == f[5]){
      sp2drop <- c("Yellow-bellied Flycatcher", "Ring-billed Gull",
        "Red-breasted Merganser", "Lesser Scaup", "Double-crested Cormorant",
        "Bobolink", "American Herring Gull", "Field Sparrow")
    } else {
      sp2drop <- NA
    }
    tmp <- tmp[!tmp$common_name %in% sp2drop, ]

    #drop "timestamp" col
    tmp <- tmp[, !names(tmp) %in% "timestamp"]

    #combine with df
    df <- dplyr::bind_rows(df, tmp)

  }

}

#set df to new dataframe
reviewed_twice <- df



#--Clean up data----

#--update species names
reviewed_twice$common_name[
  reviewed_twice$common_name %in% "Warbling Vireo"
] <- "Eastern Warbling Vireo"
reviewed_twice$common_name[
  reviewed_twice$common_name %in% "Yellow Warbler"
] <- "Northern Yellow Warbler"


#--remove dupe review entries labeled "remove"
remove_ids <- reviewed_twice$final_final_code %in% "remove"
reviewed_twice <- reviewed_twice[!remove_ids, ]


#--check that only one row per obs
stopifnot(duplicated(reviewed_twice$obs_id) == 0)

if (FALSE) {
  which(duplicated(reviewed_twice$obs_id))
  dupes <- which(duplicated(reviewed_twice$obs_id))
  reviewed_twice$obs_id[1746]
  reviewed_twice$obs_id[1917]
  reviewed_twice$obs_id[5300]
}


#--set review status
reviewed_twice$review_status_v2 <- NA

for (i in 1:nrow(reviewed_twice)) {
  if (reviewed_twice$behavior_code[i] == reviewed_twice$final_final_code[i]) {
    reviewed_twice$review_status_v2[i] <- "reviewed-approved"
  } else {
    reviewed_twice$review_status_v2[i] <- "reviewed-changed"
  }
}

review2_cols <- c(
  "obs_id", "final_final_code", "final_final_reason", "review_status_v2"
)
review2 <- reviewed_twice[, review2_cols]



#---- Create final dataset ----

#--merge in final_final_code and final_final_reason from second man review
dat <- dplyr::left_join(
  x = alldat,
  y = review2,
  by = dplyr::join_by(global_unique_identifier == obs_id),
  keep = FALSE
)


#--pre-populate breeding_code and change_reason with
#final_code and change_reason from interim dataset
dat$breeding_code <- dat$final_code #initialize breeding code column
dat$change_reason_v1 <- dat$change_reason #backup results from round 1 review
dat$review_status_v1 <- dat$review_status #backup results from round 1 review


#--update breeding code, reason, and status if reviewed twice
reviewed2x <- dat$global_unique_identifier %in% reviewed_twice$obs_id
dat$breeding_code[reviewed2x] <- dat$final_final_code[reviewed2x]
dat$change_reason[reviewed2x] <- dat$final_final_reason[reviewed2x]
dat$review_status[reviewed2x] <- dat$review_status_v2[reviewed2x]

# check that it looks right
if (FALSE) {
  cols2view <- c("common_name", "behavior_code", "final_code", "final_final_code",
    "breeding_code", "change_reason_v1", "final_final_reason", "change_reason",
    "review_status_v1", "review_status_v2", "review_status")
  head(dat[dat$breeding_code != dat$behavior_code, cols2view], 50)
}


#--drop columns we no longer need
dropcols2 <- c("final_code", "final_final_code", "final_final_reason",
  "review_status_v1", "review_status_v2", "change_reason_v1")
dat <- dat[, !names(dat) %in% dropcols2]


#--change name of change_reason to review_reason
dat <- dplyr::rename(dat, review_reason = change_reason)


#--assign breeding category
observed <- c("NC", "F", NA, "W")
possible <- c("H", "S")
probable <- c("S7", "M", "P", "T", "C", "N", "A", "B")
confirmed <- c(
  "PE", "CN", "NB", "DD", "UN", "ON", "FL", "CF", "FY",   "FS", "NE", "NY"
)

dat$breeding_category[dat$breeding_code %in% observed] <- "observed"
dat$breeding_category[dat$breeding_code %in% possible] <- "possible"
dat$breeding_category[dat$breeding_code %in% probable] <- "probable"
dat$breeding_category[dat$breeding_code %in% confirmed] <- "confirmed"



# #--drop observed codes
# dat <- dat[!dat$breeding_category %in% "observed", ]


#--test that we have data for all expected species
breeders <- dates[dates$batch %in% 1:6, ]
stopifnot(nrow(breeders) == length(unique(dat$common_name)))
stopifnot(nrow(dat) == nrow(alldat))



#--Export data----

#set file name
if (process_type == "combined") {
  fname <- paste0("final_obs_data_combined_", Sys.Date(), ".rds")
} else {
  fname <- paste0("final_obs_data_batch", batch_set, "_", Sys.Date(), ".rds")
}

saveRDS(object = dat, file = file.path(dir_out, fname))

