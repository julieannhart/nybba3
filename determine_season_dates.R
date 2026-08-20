# Determine season dates for data review
#
# Base date cutoffs on cumulative distribution function (ecdf)
#
# Assess using:
#   * only confirmed codes
#   * attempted codes (probable and confirmed)
#
# Seasons are: early, pre-breeding, core breeding, post-breeding, late
#
# Script written by Julie A. Hart
# last edited 8/20/2026



##------Settings------

#date tag
review_year <- 2025
today_tag <- format(Sys.Date(), "%Y%m%d")

#codes to base dates on
#options = all, attempted, confirmed
use_cats <- "attempted"



##-----Set Paths-----

# set working directory
dir_prj <- ".."

# path to expected breeding dates
dir_data <- file.path(dir_prj, "..", "data")
stopifnot(dir.exists(dir_data))

# path to eBird data
dir_ebird <- file.path(dir_prj, "..", "..", "eBird")
stopifnot(dir.exists(dir_ebird))

# output directory
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
    dir_prj, "..", "checklist_validation/output",
    "SEIs that pass checklist review.csv"),

  breedingspecies = file.path(dir_data, "breeding_species_plus_hybrids.csv"),

  dates = file.path(dir_data, "expected_dates_2025-09-04.csv")
)

stopifnot(file.exists(unlist(fname_inputs)))



##-----Load Data-----

#--* good checklist data----
checklists2keep <- read.csv(fname_inputs[["goodChecklists"]])[, "x", drop = TRUE]


#---* observation data----
cdat0 <- readRDS(fname_inputs[["ebirdny_BBA"]])

#subset to "good checklists" (passed checklist review)
cdat <- cdat0[cdat0$sampling_event_identifier %in% checklists2keep, ]


#---* breeding species----
breeding_sp <- read.csv(fname_inputs[["breedingspecies"]])

#subset obs data to breeders
ids <- cdat$common_name %in% breeding_sp[, 1]
cdat <- cdat[ids, ]

#get list of species
species <- unique(cdat[, "common_name"])


#---* breeding season dates----

dates <- read.csv(fname_inputs[["dates"]])
#remove NA columns
dates <- dates[, colSums(is.na(dates)) < nrow(dates)]

#ensure all species have breeding dates
tmp <- setdiff(species, dates$common_name)
if (length(tmp) > 0) {
  stop("dates doesn't contain these species: ", toString(tmp))
}


##-----Calculate quantiles-----

#subset data to selected breeding code categories
if (use_cats == "confirmed") {
  cdat <- cdat[cdat$breeding_category %in% c("C4"), ]
}
if (use_cats == "attempted") {
  cdat <- cdat[cdat$breeding_category %in% c("C3", "C4"), ]
}
# if (use_cats == "all") {
#   cdat <- cdat[cdat$breeding_category %in% c("C2", "C3", "C4"), ]
# }


#get doy of each obs
cdat$obs_doy <- 1 + as.POSIXlt(cdat[, "observation_date"])$yday


#set variable names
var_season_dates <- c(
  "pre_breeding_start", "core_breeding_start",  "core_breeding_end", "post_breeding_end"
)
var_season_doys <- paste0(var_season_dates, "_doy")
var_season_quantiles <- paste0(var_season_dates, "_quant")


#new date suggestions based on quantiles
harmonized_quantiles <- c(0.1, 0.2, 0.8, 0.9)
var_season_newquantiles <- paste0(var_season_quantiles, "-new")
var_season_newdates <- paste0(var_season_dates, "-new")
var_season_newdoys <- paste0(var_season_doys, "-new")


#initialize results container
ecdf_dates <- array(
  data = NA,
  dim = c(length(species), 2L + 6L * length(var_season_dates)),
  dimnames = list(
    NULL,
    c(
      "common_name",
      "nObs",
      var_season_dates, var_season_doys, var_season_quantiles,
      var_season_newdates, var_season_newdoys, var_season_newquantiles
    )
  )
) |>
  as.data.frame()

ecdf_dates[, "common_name"] <- species
ids <- match(species, dates[, "common_name"], nomatch = 0L)


#convert dates
ecdf_dates[ids > 0L, var_season_dates] <- dates[ids, var_season_dates]

for (k in seq_along(var_season_dates)) {
  tmp <- as.POSIXlt(dates[ids, var_season_dates[[k]]], format = "%m/%d/%y")

  ecdf_dates[ids > 0L, var_season_dates[[k]]] <- as.character(tmp)
  ecdf_dates[ids > 0L, var_season_doys[[k]]] <- 1L + tmp$yday
}


#loop through species
for (k in seq_along(species)) {
  sp <- species[[k]]

  #subset data to species
  sp_dat <- cdat$obs_doy[cdat$common_name %in% sp]
  #sp_dat <- cdat$obs_doy[cdat$common_name %in% sp & cdat$breeding_category %in% "C4"]
  ecdf_dates[k, "nObs"] <- length(sp_dat)

  #only calculate ecdf if there are data
  if (ecdf_dates[k, "nObs"] > 0L) {

    #calc ecdf values
    tmp <- unlist(ecdf_dates[k, var_season_doys])
    ecdf_dates[k, var_season_quantiles] <- ecdf(sp_dat)(tmp)

    #calculate dates at 10, 20, 80, and 90 quantiles
    ecdf_dates[k, var_season_newquantiles] <- harmonized_quantiles
    ecdf_dates[k, var_season_newdoys] <- quantile(
      sp_dat, probs = harmonized_quantiles
    ) |>
      round()

    ecdf_dates[k, var_season_newdates] <- as.Date(
      paste0("2020-", ecdf_dates[k, var_season_newdoys]),
      format = "%Y-%j"
    ) |>
      as.character()
  }
}


#save output
utils::write.csv(
  ecdf_dates,
  file = file.path(dir_out, paste0("new_quantile_season_dates_based_on_", use_cats, "_codes_", today_tag, ".csv")),
  row.names = TRUE
)


