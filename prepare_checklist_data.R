#----Create one dataframe for all checklist data-------
#
# these data will be used to perform checklist-level review
#
# Main steps:
# 1. prepare obs-based sampling df for any checklists not in eBird sampling df
# 2. combine ebird and obs-based sampling df's
# 3. restrict dates, standardize data, add block info
# 4. calculate breeding code stats
# 5. add a checklist type column (moving or point)
#
# Script written by Julie A. Hart
# last edited 8/20/2026



#-----Set directories-----

# set working directory
dir_prj <- ".."

# spatial data
dir_spatial <- file.path(dir_prj, "..", "..", "Spatial_data")
stopifnot(dir.exists(dir_spatial))

# output directory
dir_out <- file.path(dir_prj, "output")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)



#-----Read in data-----

##--* observation data----

alldat <- readRDS(
  file.path(
    dir_prj,
    "..",
    "EBD_public_data",
    "ebd_US-NY_202001_202412_unv_smp_relAug-2025_AllPortals_AllData_AllSpp.rds"
  )
)


##--* sampling data----

#use unique = F to keep each sampling_event_identifier separate
sampling <- auk::read_sampling(
  file.path(
    dir_prj,
    "..",
    "EBD_public_data",
    "ebd_US-NY_202001_202412_unv_smp_relAug-2025_sampling.txt"
  ),
  unique = FALSE
)


##--* blocks----

fname_blocks <- file.path(
  dir_spatial,
  "atlas_blocks",
  "spatial_atlas_blocks",
  "results",
  "block-vector_with_attributes.gpkg"
)

blocks <- sf::read_sf(fname_blocks) |>
  sf::st_transform(crs = 4326) |>
  sf::st_make_valid()


##--* rare breeders

rarespp <- read.csv(
  file.path(
    dir_prj, "..", "..", "DataReview", "data", "rare_breeding_species.csv"
  )
) |> as.vector()



#-----Explore sampling data from eBird-----

# sampling_ebird includes:
#   - all of: regular ebird, zero_species_checklists
#   - some of: hidden, sensitive, zero_count_obs
# sampling_ebird does not include:
#   - all of: external
#   - some of: hidden, sensitive, zero_count_obs
if (FALSE) {
  tt <- alldat$sampling_event_identifier %in% sampling$sampling_event_identifier
  cat("Sources in ebird-sampling df:", fill = TRUE)
  table(alldat$source[tt], alldat$locality_type[tt])

  cat("Sources *not* in ebird-sampling df:", fill = TRUE)
  table(alldat$source[!tt], alldat$locality_type[!tt])
}



#-----Prepare sampling_ebird-----

##-----* Checklist-based sampling df-----

#define columns for final sampling df
#don't need all the columns
cols2keep <- c(
  "sampling_event_identifier", "group_identifier",
  "observer_id", "number_observers",
  "locality", "locality_id", "locality_type",
  "latitude", "longitude",
  "observation_date", "time_observations_started",
  "observation_type", "protocol_name", "protocol_code", "project_names",
  "duration_minutes", "effort_distance_km", "effort_area_ha",
  "all_species_reported", "checklist_comments", "atlas_block"
)

# keep atlas_block from sampling but blockID from hidden and external
sampling_ebird <- sampling[, cols2keep]

#add source column so we can track where data came from
sampling_ebird$source <- "ebird_sampling"

#convert the text string "NA" to actual NA values in checklist_comments
istxtna <- sampling_ebird$checklist_comments %in% "NA"
sampling_ebird$checklist_comments[istxtna] <- NA

stopifnot(!anyDuplicated(sampling_ebird$sampling_event_identifier))



##-----* Obs-based sampling df-----

#subset to unique SEI
samp_from_obs <- dplyr::distinct(
  alldat[, cols2keep], sampling_event_identifier, .keep_all = TRUE
)

samp_from_obs$source <- "alldat_sampling"

# mulitple checklist_comments -- they can differ within SEI, e.g., due to nybba_rollup()
tmp <- dplyr::distinct(alldat[, cols2keep])
idsMultipleCC <- which(
  duplicated(tmp[, c("sampling_event_identifier", "checklist_comments")]) &
    nzchar(tmp$checklist_comments)
)
if (length(idsMultipleCC)) {
  warning(
    "There are multiple, non-unique checklist_comments on n = ",
    length(idsMultipleCC), " checklists!"
  )
}
#147 checklists have comment issues

#clean up
samp_from_obs$group_identifier[!nzchar(samp_from_obs$group_identifier)] <- NA

#convert the text string "NA" to actual NA values in checklist_comments
istxtna <- samp_from_obs$checklist_comments %in% "NA"
samp_from_obs$checklist_comments[istxtna] <- NA

stopifnot(!anyDuplicated(samp_from_obs$sampling_event_identifier))

if (FALSE) {
  sampling_all[sampling_all$sampling_event_identifier %in% "S125959935", ] |> as.data.frame()
  alldat[alldat$sampling_event_identifier %in% "S125959935", ] |> as.data.frame()
}



##--- * Compare sampling_ebird against obs-based sampling df ------

ids <- match(
  samp_from_obs$sampling_event_identifier,
  sampling_ebird$sampling_event_identifier,
  nomatch = 0L
)

all.equal(
  samp_from_obs[ids > 0L, ], current = as.data.frame(sampling_ebird[ids, ])
)
# [1] "Attributes: < Component “row.names”: Mean relative difference: 0.002377663 >"
# [2] "Component “locality”: 1 string mismatch"
# [3] "Component “time_observations_started”: 1 string mismatch"
# [4] "Component “project_names”: 'is.NA' value mismatch: 2016058 in current 2016059 in target"
# [5] "Component “effort_distance_km”: Mean relative difference: 0.1255121"
# [6] "Component “checklist_comments”: 'is.NA' value mismatch: 2194884 in current 7046 in target"
# [7] "Component “atlas_block”: 'is.NA' value mismatch: 2019584 in current 6097 in target"
# [8] "Component “source”: 2752809 string mismatches"



#---DECISIONS

# row.names
# ok to have differences in
#   * row.names
#   * checklist_comments: may arise from  nybba_rollup or due to subsampling
#   * atlas_block: will fix missing blocks later if possible
#   * source

# locality difference
#waldo::compare(samp_from_obs$locality[ids > 0L], sampling_ebird$locality[ids])
#just an issue with an extra space, ok to ignore

# time obs started
#waldo::compare(samp_from_obs$time_observations_started[ids > 0L], sampling_ebird$time_observations_started[ids])
#difference of 3 minutes, ok to ignore

# project names
#waldo::compare(samp_from_obs$project_names[ids > 0L], sampling_ebird$project_names[ids])
#should be in atlas portal
#will be kept in atlas portal with way we make the join


# effort_distance_km
#   --> there are discrepancies without clear reason
#   --> some checklist comments suggests that the observer manually "fixed"
#       effort_distance_km but not the track; then, we are guessing,
#       the new ebird track calculation feature was run and automatically updated those values
#   --> one checklist also has a mix between distance and NA
#       Guess: observer edited checklist from traveling to incidental & associated
#       zero_count_obs on the checklist retained effort_distance_km (for weird reason, ebird db bug?)
if (FALSE) {
  innm <- which(
    abs(samp_from_obs[ids > 0L, "effort_distance_km"] -
        sampling_ebird[ids, "effort_distance_km", drop = TRUE]) > sqrt(.Machine$double.eps) |
      is.na(samp_from_obs[ids > 0L, "effort_distance_km"]) !=
      is.na(sampling_ebird[ids, "effort_distance_km", drop = TRUE])
  )
  cbind(
    alldat = samp_from_obs[ids > 0L, c("sampling_event_identifier", "effort_distance_km")][innm,],
    sampling = sampling_ebird[ids, "effort_distance_km", drop = TRUE][innm]
  )

  table(alldat[alldat$sampling_event_identifier %in% "S73933164", c("source", "effort_distance_km")])
  table(alldat[alldat$sampling_event_identifier %in% "S74133281", c("source", "effort_distance_km")])

  table(alldat[alldat$sampling_event_identifier %in% "S75048278", c("source", "effort_distance_km")])

  table(alldat[alldat$sampling_event_identifier %in% "S92704559", c("source", "effort_distance_km")])
  table(alldat[alldat$sampling_event_identifier %in% "S92714496", c("source", "effort_distance_km")])

  table(alldat[alldat$sampling_event_identifier %in% "S98006532", c("source", "effort_distance_km")])

  # One entry doesn't match for effort_distance_km
  # Guess: observer edited checklist from traveling to incidental & associated
  # zero_count_obs on the checklist retained effort_distance_km (for weird reason, ebird db bug?)
  alldat[alldat$sampling_event_identifier %in% "S75153578", ]
}



##---* Create combined sampling df for data not in sampling_ebird -----

idsFromObs <- which(
  !samp_from_obs$sampling_event_identifier %in%
    sampling_ebird$sampling_event_identifier
)

sampling_all <- rbind(
  sampling_ebird,
  samp_from_obs[idsFromObs, ]
)

stopifnot(!anyDuplicated(sampling_all$sampling_event_identifier))


#-----Restrict dates-----

#Restrict dates to atlas period
isAtlasPeriod <- sampling_all$observation_date >= "2020-01-01" &
  sampling_all$observation_date <= "2024-12-31"
stopifnot(isAtlasPeriod)
sampling_all <- sampling_all[isAtlasPeriod, ]



#-----Add block info-----

# quick check that all atlas_block in the data exist in our blocks file
stopifnot(sampling_all$atlas_block %in% c(NA, blocks$atlas_block))


##-----* spatially look up missing ------

idsNoBlock <- which(is.na(sampling_all$atlas_block))

#add geometry to data file
nb_sf <- sf::st_as_sf(
  sampling_all[idsNoBlock, ], coords = c("longitude", "latitude"), crs = 4326
)

#intersect geometry with blocks
tmpBlockID <- rep(NA_integer_, length(idsNoBlock))
resIntersects <- sf::st_intersects(nb_sf, blocks)

lri <- lengths(resIntersects)
table(lri)
# lri
#    0       1       4
# 6282 2028354       1

ids <- which(lri == 1L)
tmpBlockID[ids] <- unlist(resIntersects[ids])
ids2 <- which(lri > 1L)
if (length(ids2) > 0L) {
  warning("Checklists (n = ", length(ids2), ") intersect more than one block!")
  # Checklists (n = 1) intersect more than one block!
}
tmpBlockID[ids2] <- unlist(lapply(ids2, function(k) resIntersects[[k]][[1L]]))


sampling_all[idsNoBlock, "atlas_block"] <-
  blocks[tmpBlockID, "atlas_block", drop = TRUE]


##--* check for still missing values----

if (anyNA(sampling_all$atlas_block)) {
  idsNABlock <- which(is.na(sampling_all$atlas_block))
  message(length(idsNABlock), " entries have no assigned block.")
  # 6282 entries have no assigned block.

  # map checklists without assigned atlas block
  if (FALSE) {
    nb_sf2 <- sf::st_as_sf(
      sampling_all[idsNABlock, ],
      coords = c("longitude", "latitude"),
      crs = 4326
    )

    #export to look at locations in QGIS
    sf::st_write(
      nb_sf2,
      dsn = file.path(dir_prj, "..", "output", "missing block name.gpkg")
    )

    ggplot2::ggplot() +
      ggplot2::geom_sf(data = blocks[, 0]) +
      ggplot2::geom_sf(data = nb_sf2[, 0]) +
      ggplot2::geom_sf(data = nb_sf[ids2, 0], col = "red")
  }
}
#6282 entries have no assigned block.



##-----* look up additional info -----

#lookup block name, county, region, priority status
var_blocks <- c(
  block_name = "block_name",
  block_county = "county",
  block_region = "region",
  priority_status = "priority_status"
)
stopifnot(!names(var_blocks) %in% colnames(sampling_all))

ids <- match(sampling_all$atlas_block, blocks$atlas_block, nomatch = 0L)

sampling_all[ids > 0L, names(var_blocks)] <- blocks[ids, var_blocks, drop = TRUE]



#------Standardize data-----

#--where are there NAs?
summary(sampling_all)
#duration_minutes, effort_distance_km, effort_area_ha, number_observers
#minutes and distance ok depending on observation_type


##--* duration_minutes----
idsNADuration <- which(
  is.na(sampling_all$duration_minutes) &
    sampling_all$observation_type %in% c("Traveling", "Stationary", "Area")
)

if (length(idsNADuration) > 0L) {
  warning("Traveling, Stationary, or Area checklists without duration!")
}

#what is source of observation data?
table(
  alldat$source[
    alldat$sampling_event_identifier %in%
      sampling_all$sampling_event_identifier[idsNADuration]
  ]
)
# ebd_reviewed
# 1463

#   --> DECISION: leave alone for now and capture in checklist review


##--* effort_distance_km----
idsNADistanceTraveling <- which(
  is.na(sampling_all$effort_distance_km) &
  sampling_all$observation_type == "Traveling"
)

if (length(idsNADistanceTraveling) > 0L) {
  warning("Traveling checklists without effort_distance_km!")
}

#what is source of observation data?
table(
  alldat$source[
    alldat$sampling_event_identifier %in%
      sampling_all$sampling_event_identifier[idsNADistanceTraveling]
  ]
)
# ebd_reviewed    sensitive
#        10442            1

#   --> DECISION: leave alone for now and capture in checklist review


##--* number_observers----

# can be NA for "Incidental" "Historical"

idsNACountObserver <- is.na(sampling_all$number_observers)

if (any(idsNACountObserver)) {

  stopifnot(
    sampling_all$observation_type[idsNACountObserver] %in%
      c("Incidental", "Historical")
  )

  #what is source of data?
  table(
    alldat$source[alldat$sampling_event_identifier %in% sampling_all$sampling_event_identifier[idsNACountObserver]]
  )
  # ebd_reviewed         hidden      sensitive zero_count_obs
  #       111156            370             38             22
}


##--* all_species_reported----

if (!all(sampling_all$all_species_reported %in% c(FALSE, TRUE))) {

  #standardize column
  table(sampling_all$all_species_reported)
  #currently 0, 1, False, True

  #match to how eBird does it in sampling dataframe
  #want 1, 0, NA
  sampling_all$all_species_reported[sampling_all$all_species_reported %in% "TRUE"] <- 1
  sampling_all$all_species_reported[sampling_all$all_species_reported %in% "FALSE"] <- 0

  #check values again
  sort(unique(sampling_all$all_species_reported))
}



##--* review other columns----

sort(unique(sampling_all$locality_type))
head(sort(unique(sampling_all$observer_id)))
head(sort(unique(sampling_all$sampling_event_identifier)))
sort(unique(sampling_all$observation_type))
anyNA(sampling_all$observation_type)
sort(unique(sampling_all$protocol_name))
sort(unique(sampling_all$protocol_code))
sort(unique(sampling_all$project_names))
head(sort(unique(sampling_all$atlas_block)))
range(sampling_all$latitude)
range(sampling_all$longitude)
#if any of these are true, checklist is outside NY and captured in data review
anyNA(sampling_all$atlas_block)



#-----Calc breeding stats-----

#Add columns for number of species, codes, and attempted codes (PR + CO)
#calculate per checklist using sampling_event_identifier
#this info can help us gauge how useful a checklist is when doing manual review

#calculate values from alldat then add to sampling_all

##--* get unique number of species on a checklist----

countSpecies <- aggregate(
  alldat[, "common_name"],
  by = list(sampling_event_identifier = alldat[, "sampling_event_identifier"]),
  length
)

indices <- match(
  x = sampling_all$sampling_event_identifier,
  table = countSpecies$sampling_event_identifier,
  nomatch = 0L
)
sampling_all$countSpecies <- NA_integer_
sampling_all[indices > 0L, "countSpecies"] <- countSpecies$x[indices]


##--* get unique number of coded species on a checklist----

idsCoded <- which(alldat$breeding_category %in% c("C1", "C2", "C3", "C4"))
countCodedSpecies <- aggregate(
  alldat[idsCoded, "common_name"],
  by = list(sampling_event_identifier = alldat[idsCoded, "sampling_event_identifier"]),
  length
)

indices <- match(
  x = sampling_all$sampling_event_identifier,
  table = countCodedSpecies$sampling_event_identifier,
  nomatch = 0L
)
sampling_all$countCodedSpecies <- NA_integer_
sampling_all[indices > 0L, "countCodedSpecies"] <- countCodedSpecies$x[indices]


##--* get unique number of attempted species on a checklist----
idsAttempted <- which(alldat$breeding_category %in% c("C3", "C4"))
countAttemptedSpecies <- aggregate(
  alldat[idsAttempted, "common_name"],
  by = list(sampling_event_identifier = alldat[idsAttempted, "sampling_event_identifier"]),
  length
)

indices <- match(
  x = sampling_all$sampling_event_identifier,
  table = countAttemptedSpecies$sampling_event_identifier,
  nomatch = 0L
)
sampling_all$countAttemptedSpecies <- NA_integer_
sampling_all[indices > 0L, "countAttemptedSpecies"] <- countAttemptedSpecies$x[indices]


##--* get unique number of rare species on a checklist----

idsRare <- which(alldat$common_name %in% rarespp$Species)

countRareSpecies <- aggregate(
  alldat[idsRare, "common_name"],
  by = list(sampling_event_identifier = alldat[idsRare, "sampling_event_identifier"]),
  length
)

indices <- match(
  x = sampling_all$sampling_event_identifier,
  table = countRareSpecies$sampling_event_identifier,
  nomatch = 0L
)

sampling_all$countRareSpecies <- NA_integer_
sampling_all[indices > 0L, "countRareSpecies"] <- countRareSpecies$x[indices]



#-----Add checklist_type based on protocols for point or moving-----

# traveling checklists with distance < 30 m
# Decision: take observer's word that these are traveling checklists
if (FALSE) {
  table(sampling_all$source[sampling_all$observation_type %in% "Traveling" & sampling_all$effort_distance_km < 0.03])
  indeterminate <- unique(sampling_all$sampling_event_identifier[sampling_all$observation_type %in% "Traveling" & sampling_all$effort_distance_km < 0.03])
  table(alldat$source[alldat$sampling_event_identifier %in% indeterminate])
  #from multiple sources, mostly from ebd_reviewed
}

#create empty column
sampling_all$checklist_type <- NA
has_dist <- !sampling_all$effort_distance_km %in% c(0, NA)
has_area <- !sampling_all$effort_area_ha %in% c(0, NA)


##--* conditions for moving checklists----
idsMoving <- c(
  # All checklist that are one of: Traveling, Area
  which(sampling_all$observation_type %in% c("Traveling", "Area")),

  # eBird Pelagic Protocol with a distance
  which(
    sampling_all$observation_type %in% "eBird Pelagic Protocol" & has_dist
  ),

  # Banding if either with a distance or an area
  which(
    sampling_all$observation_type %in% "Banding" & (has_dist | has_area)
  ),

  # Historical if either with a distance or an area
  which(
    sampling_all$observation_type %in% "Historical" & (has_dist | has_area)
  )
)

sampling_all$checklist_type[idsMoving] <- "moving" # 1439108


##--* conditions for point checklists----
idsPoint <- c(
  # All checklist that are one of: Stationary, Incidental, Nocturnal
  which(
    sampling_all$observation_type %in% c(
      "Stationary", "Incidental", "Nocturnal Flight Call Count"
    )
  ),

  # eBird Pelagic Protocol without distance
  which(
    sampling_all$observation_type %in% "eBird Pelagic Protocol" & !has_dist
  ),

  # Banding without distance and without area
  which(
    sampling_all$observation_type %in% "Banding" & !has_dist & !has_area
  ),

  # Historical if missing distance or area
  which(
    sampling_all$observation_type %in% "Historical" & !(has_dist | has_area)
  )

)

stopifnot(!idsPoint %in% idsMoving)

sampling_all$checklist_type[idsPoint] <- "point" # 1379113

#-ensure all data have an assignment
stopifnot(!anyNA(sampling_all$checklist_type))



#-----Save results-----

#save RDS object
saveRDS(
  sampling_all,
  file.path(dir_out, paste0("checklist_data_", Sys.Date(), ".rds"))
)
