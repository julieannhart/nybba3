#--Review checklist effort----
#
# Data: results of checklist1_moving_check.R and checklist2_point_check.R
#
# Main steps:
# 1. review moving checklists = checklist1_moving_check.R
# 2. review point checklists = checklist2_point_check.R
# 3. finally run all the additional checklist-level effort checks below
#
# Script written by Julie A. Hart
# last edited 8/20/2026



#---------------------------Set paths---------------------------

# set project directory
dir_prj <- ".."

#set output directory
dir_out <- file.path(dir_prj, "output")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

#path to point checklist data
fname_pt_sampling <- list.files(
  path = file.path(dir_out),
  pattern = "point_checklists_reviewed_",
  full.names = TRUE
)
stopifnot(length(fname_pt_sampling) == 1)

#path to moving checklist data
fname_mov_sampling <- list.files(
  path = file.path(dir_out),
  pattern = "moving_checklists_reviewed_",
  full.names = TRUE
)
stopifnot(length(fname_mov_sampling) == 1)



#---------------------------Load data---------------------------

#read in RDS files
sampling_pt <- readRDS(fname_pt_sampling)
sampling_mov <- readRDS(fname_mov_sampling)

#combine
cldat <- dplyr::bind_rows(sampling_pt, sampling_mov)



#---------------------------Protocol---------------------------

cldat$protocol_flag <- FALSE


#--* check for NAs----

stopifnot(!anyNA(cldat$protocol_name))
stopifnot(!anyNA(cldat$protocol_code))
stopifnot(!anyNA(cldat$observation_type))

#there are no NAs since we cleaned up in prepare_observation_data.R


#--* check protocols used----
# --> keep all protocols at this stage because likely data entry mistakes
# --> potentially subset later for specific analysis if needed

if (FALSE) {
  table(cldat$observation_type)
  #       Area                      Banding  eBird Pelagic Protocol  Historical
  #       2727                         1372                    4392       10820
  # Incidental  Nocturnal Flight Call Count              Stationary   Traveling
  #     514149                         1197                  853127     1430575
}

#verify that the protocols match the data, such that:
#observation_type == "Traveling" has date, starttime, duration, distance
#observation_type == "Area" has date, starttime, duration, area
#observation_type %in% c("Incidental", "Historical) has date
#observation_type %in% c("Stationary", "Banding", "NFC", "Pelagic") has date, starttime, duration
#else should be flagged

has_date <- !is.na(cldat$observation_date)
has_start <- !is.na(cldat$time_observations_started)
has_dur <- !is.na(cldat$duration_minutes)
has_dist <- !is.na(cldat$effort_distance_km)
has_area <- !is.na(cldat$effort_area_ha)

traveling <- cldat$observation_type == "Traveling"
ids <- traveling & !(has_date & has_start & has_dur & has_dist)
cldat$protocol_flag[ids] <- TRUE

area <- cldat$observation_type == "Area"
ids2 <- area & !(has_date & has_start & has_dur & has_area)
cldat$protocol_flag[ids2] <- TRUE

statplus <- cldat$observation_type %in% c(
  "Stationary", "Banding", "Nocturnal Flight Call Count", "eBird Pelagic Protocol"
)
ids3 <- statplus & !(has_date & has_start & has_dur)
cldat$protocol_flag[ids3] <- TRUE

ih <- cldat$observation_type %in% c("Incidental", "Historical")
ids4 <- ih & !has_date
cldat$protocol_flag[ids4] <- TRUE



#---------------------------Observation date---------------------------

#--* check for NAs----
stopifnot(!anyNA(cldat$observation_date))
#all good


#--* check within atlas period----
stopifnot(
  cldat$observation_date >= "2020-01-01" &
  cldat$observation_date <= "2024-12-31"
)



#---------------------------Start time---------------------------

#--* check for NAs----
table(cldat$observation_type[is.na(cldat$time_observations_started) & !cldat$checklist_flag])
# Historical Incidental Stationary  Traveling
#       3060      13766         46          1
#historical and incidental == OK

#stationary and traveling must have a start time, flag if missing
# -> taken care of via protocol_flag logic



#---------------------------Duration---------------------------

#--* check for NAs----
table(cldat$observation_type[is.na(cldat$duration_minutes)])
# Historical Incidental Stationary
#       8159     512511        229
#"Incidental" & "Historical" == OK

# for now, keep historical without duration;
# may remove those depending on specific analysis

if (FALSE) {
  ids <- is.na(cldat$duration_minutes) &
    cldat$observation_type %in% "Historical"

  ids <- is.na(cldat$duration_minutes) &
    cldat$observation_type %in% "Incidental"

  tmp <- cldat[ids, "checklist_comments", drop = TRUE]
  tmp2 <- tmp[!is.na(tmp)]
  head(tmp2[order(nchar(tmp2), decreasing = TRUE)])
}

# flag stationary checklists without duration
cldat$duration_flag <- is.na(cldat$duration_minutes) &
  (!cldat$observation_type %in% c("Historical", "Incidental"))


#--* check long durations----

#ebird doesn't use checklists longer than 1 day
min_in_day <- 24 * 60

if (FALSE) {
  max(cldat$duration_minutes, na.rm = T)/min_in_day
  # longest checklist has a duration of 21.56944 days!

  # how many checklists longer than 1 day?
  sum(cldat$duration_minutes > min_in_day, na.rm = T)
  # 95 checklists with duration longer than 1 day
}


#--* flag long durations----

cldat$duration_flag <- !is.na(cldat$duration_minutes) &
  (cldat$duration_minutes > min_in_day)

if (FALSE) {
  # #--flag anything longer than 8 hrs
  #
  # ids <- !is.na(cldat$duration_minutes) & (cldat$duration_minutes > 8 * 60)
  # cldat$duration_flag <- FALSE
  # cldat$duration_flag[ids] <- TRUE

  table(cldat$duration_flag)
  #    FALSE       TRUE
  #  2818264         95
}



#---------------------------Locality type---------------------------

# locality_type options are:
#P = personal
#H = hotspot
#T = town
#PC = postal code
#C = county
#S = state

table(cldat$locality_type)
#       H        P      PC       S       T
# 1326635  1484543     462       1    6718

#flag locality_type at zip/town level or higher
cldat$locality_flag <- !cldat$locality_type %in% c("P", "H")

if (FALSE) {
  table(cldat$locality_flag)
  #   FALSE     TRUE
  # 2811178     7181
}



#---------------------------Block info---------------------------

#save original atlas block to a new column
cldat$orig_atlas_block <- cldat$atlas_block

#update atlas block ID with updatedBlockID column
updateIDs <- !is.na(cldat$updatedBlockID) & (cldat$updatedBlockID != cldat$atlas_block)
cldat$atlas_block[updateIDs] <- cldat$updatedBlockID[updateIDs]


# checklist_flag is already set for missing atlas_block
stopifnot(cldat$checklist_flag[is.na(cldat$atlas_block)])

# make sure the only checklists missing county and region are the same ones
# missing a block assignment
stopifnot(identical(is.na(cldat$atlas_block), is.na(cldat$block_county)))
stopifnot(identical(is.na(cldat$atlas_block), is.na(cldat$block_region)))



#---------------------------Number observers---------------------------

#--* check for NAs----

table(cldat$observation_type[is.na(cldat$number_observers)])
# Historical Incidental
#       5121      14664
#"Incidental" & "Historical" == OK


#--* flag checklists with tons of observers----

#ebird doesn't recommend using checklists with > 10 observers
# --> atlas has different use cases and more observers are ok
# --> subset later for specific analyses if needed, e.g., occupancy

if (FALSE) {
  #--check for NAs
  sum(is.na(cldat$number_observers)) #19785
  table(cldat$observation_type[is.na(cldat$number_observers)])
  # Historical Incidental
  #       5121      14664
  #NAs for Historical and Incidental == OK

  tabulate(cldat$number_observers, nbins = 20)
  range(cldat$number_observers, na.rm = T)
  hist(cldat$number_observers, xlim = c(0, 10), breaks = 10000)
  sum(cldat$number_observers <= 10, na.rm = T) #2752257
  sum(cldat$number_observers > 10, na.rm = T)  #  43545
  sum(cldat$number_observers > 20, na.rm = T)  #  12599
  sum(cldat$number_observers > 30, na.rm = T)  #   6549
  sum(cldat$number_observers > 40, na.rm = T)  #   4949
  sum(cldat$number_observers > 50, na.rm = T)  #   2719
  sum(cldat$number_observers > 100, na.rm = T) #     92

  #50 is arbitrary
  cldat$nobs_flag <- cldat$number_observers > 50

  table(cldat$nobs_flag)
  #   FALSE      TRUE
  # 2795855      2719
}



#---------------------------Combine flags---------------------------

#set passesChecklistChecks to FALSE if any of the checks fail

#get ids for different flags
protocol_flag <- cldat$protocol_flag
duration_flag <- cldat$duration_flag
locality_flag <- cldat$locality_flag
checklist_flag <- cldat$checklist_flag # contains outsideNY, block_flag, etc.

cldat$passesChecklistChecks <- !(
  protocol_flag | duration_flag | locality_flag | checklist_flag
)

if (FALSE) {
  table(cldat$passesChecklistChecks)
  #  FALSE     TRUE
  # 894291  1924068

  isBBA <- cldat$project_names %in% "New York Breeding Bird Atlas"
  table(pass = cldat$passesChecklistChecks, isBBA = isBBA)
  #            isBBA
  # pass      FALSE    TRUE
  #   FALSE  865270   29021
  #   TRUE  1171049  753019

  table(
    protocol_flag = protocol_flag,
    duration_flag = duration_flag,
    locality_flag = locality_flag,
    checklist_flag = checklist_flag,
    isBBA = isBBA,
    useNA = "ifany"
  ) |>
    as.data.frame()
  #    protocol_flag duration_flag locality_flag checklist_flag isBBA    Freq
  # 1          FALSE         FALSE         FALSE          FALSE FALSE 1171049
  # 2           TRUE         FALSE         FALSE          FALSE FALSE     147
  # 3          FALSE          TRUE         FALSE          FALSE FALSE      45
  # 4           TRUE          TRUE         FALSE          FALSE FALSE       0
  # 5          FALSE         FALSE          TRUE          FALSE FALSE     230
  # 6           TRUE         FALSE          TRUE          FALSE FALSE       0
  # 7          FALSE          TRUE          TRUE          FALSE FALSE       0
  # 8           TRUE          TRUE          TRUE          FALSE FALSE       0
  # 9          FALSE         FALSE         FALSE           TRUE FALSE  857155
  # 10          TRUE         FALSE         FALSE           TRUE FALSE     946
  # 11         FALSE          TRUE         FALSE           TRUE FALSE      35
  # 12          TRUE          TRUE         FALSE           TRUE FALSE       0
  # 13         FALSE         FALSE          TRUE           TRUE FALSE    6702
  # 14          TRUE         FALSE          TRUE           TRUE FALSE      10
  # 15         FALSE          TRUE          TRUE           TRUE FALSE       0
  # 16          TRUE          TRUE          TRUE           TRUE FALSE       0
  # 17         FALSE         FALSE         FALSE          FALSE  TRUE  753019
  # 18          TRUE         FALSE         FALSE          FALSE  TRUE      81
  # 19         FALSE          TRUE         FALSE          FALSE  TRUE      15
  # 20          TRUE          TRUE         FALSE          FALSE  TRUE       0
  # 21         FALSE         FALSE          TRUE          FALSE  TRUE     227
  # 22          TRUE         FALSE          TRUE          FALSE  TRUE       0
  # 23         FALSE          TRUE          TRUE          FALSE  TRUE       0
  # 24          TRUE          TRUE          TRUE          FALSE  TRUE       0
  # 25         FALSE         FALSE         FALSE           TRUE  TRUE   28588
  # 26          TRUE         FALSE         FALSE           TRUE  TRUE      98
  # 27         FALSE          TRUE         FALSE           TRUE  TRUE       0
  # 28          TRUE          TRUE         FALSE           TRUE  TRUE       0
  # 29         FALSE         FALSE          TRUE           TRUE  TRUE      12
  # 30          TRUE         FALSE          TRUE           TRUE  TRUE       0
  # 31         FALSE          TRUE          TRUE           TRUE  TRUE       0
  # 32          TRUE          TRUE          TRUE           TRUE  TRUE       0
}

#check exception for S64860994
#reviewed by Kyle Bardwell, only record for RUGR in Putnam County
stopifnot(cldat$passesChecklistChecks[cldat$sampling_event_identifier %in% "S64860994"])



#---------------------------Explore portal options---------------------------

if (FALSE) {

  #can only move in now if user already has data in portal
  #if user doesn't have data in portal already, need to give them chance to opt out
  atlaserIDs <- unique(cldat$observer_id[cldat$project_names %in% "New York Breeding Bird Atlas"])

  #what protocols are the checklists to move into portal?
  table(
    cldat$observation_type[
      cldat$portalAction %in% "moveIn" &
        !cldat$observer_id %in% atlaserIDs
    ]
  )
  #       Area                       Banding      eBird Pelagic Protocol        Historical
  #        544                            61                           5              1886
  # Incidental   Nocturnal Flight Call Count                  Stationary         Traveling
  #     135839                           345                      214666            175820

  #what is the data source?
  table(
    cldat$source[
      cldat$portalAction %in% "moveIn" &
        !cldat$observer_id %in% atlaserIDs
    ]
  )
  # alldat_sampling  ebird_sampling
  #               1          529165


  moveInNow <- unique(
    cldat$observer_id[
      cldat$portalAction %in% "moveIn" &
        cldat$observer_id %in% atlaserIDs
    ]
  )
  length(moveInNow)
  # 3645 observers' checklists

  askBeforeMoveIn <- unique(
    cldat$observer_id[
      cldat$portalAction %in% "moveIn" &
        !cldat$observer_id %in% atlaserIDs
    ]
  )
  length(askBeforeMoveIn)
  # 33851 observers
  # this is way too many users to email
  # need a way to filter to only useful data


  #---what if we restrict to core breeding season

  # checklists from June, July, and August
  cldat$month <- format(cldat$observation_date, "%m")

  table(
    cldat$month[
      cldat$portalAction %in% "moveIn" &
        !cldat$observer_id %in% atlaserIDs
    ]
  )
  #    01    02    03    04    05    06    07    08    09    10    11    12
  # 38198 53449 46398 57853 81808 43306 40432 36269 35084 37982 28799 29588

  length(
    unique(
      cldat$observer_id[
        cldat$portalAction %in% "moveIn" &
          !cldat$observer_id %in% atlaserIDs &
          cldat$month %in% c("06", "07", "08")
      ]
    )
  )
  # 14095 = halved the number, but still a lot


  #---number of species with any code

  # what if we restrict to checklists with codes in the summer?
  length(
    unique(
      cldat$observer_id[
        cldat$portalAction %in% "moveIn" &
          !cldat$observer_id %in% atlaserIDs &
          cldat$countCodedSpecies > 0 & !is.na(cldat$countCodedSpecies) &
          cldat$month %in% c("06", "07", "08")
      ]
    )
  )
  # 2148 observers with any code in summer


  #---number of attempted codes

  # what if we restrict to checklists with attempted codes in the summer?
  length(
    unique(
      cldat$observer_id[
        cldat$portalAction %in% "moveIn" &
          !cldat$observer_id %in% atlaserIDs &
          cldat$countAttemptedSpecies > 0 & !is.na(cldat$countAttemptedSpecies) &
          cldat$month %in% c("06", "07", "08")
      ]
    )
  )
  # 1803 observers with attempted code in summer


  #---checklists with rarer species

  # what if we restrict to checklists with rare breeders?
  length(
    unique(
      cldat$observer_id[
        cldat$portalAction %in% "moveIn" &
          !cldat$observer_id %in% atlaserIDs &
          cldat$countRareSpecies > 0 & !is.na(cldat$countRareSpecies)
      ]
    )
  )
  # 15451 with rare breeder

  #---checklists with attempted codes or rare species

  # what if we restrict to checklists with ATTEMPTED codes in the summer PLUS
  # checklists with RARE breeding species?
  length(
    unique(
      cldat$observer_id[
        cldat$portalAction %in% "moveIn" &
          !cldat$observer_id %in% atlaserIDs &
          ((cldat$countAttemptedSpecies > 0 & cldat$month %in% c("06", "07", "08") & !is.na(cldat$countAttemptedSpecies)) |
              (cldat$countRareSpecies > 0 & !is.na(cldat$countRareSpecies)))
      ]
    )
  )
  # 15742 - ack, too many!

  #---checklists with attempted codes and rare species

  # what about checklists with a rare species and at least one attempted code?
  length(
    unique(
      cldat$observer_id[
        cldat$portalAction %in% "moveIn" &
          !cldat$observer_id %in% atlaserIDs &
          cldat$month %in% c("06", "07", "08") &
          !is.na(cldat$countAttemptedSpecies) &
          cldat$countRareSpecies > 0 & !is.na(cldat$countRareSpecies)
      ]
    )
  )
  # 929 observers

  #---checklists with rare birds in summer

  # what about checklists with a rare species and at least one attempted code?
  length(
    unique(
      cldat$observer_id[
        cldat$portalAction %in% "moveIn" &
          !cldat$observer_id %in% atlaserIDs &
          cldat$month %in% c("06", "07", "08") &
          cldat$countRareSpecies > 0 &
          !is.na(cldat$countRareSpecies)
      ]
    )
  )
  # 6880 observers

  # what if we restrict to checklists with any code?
  length(
    unique(
      cldat$observer_id[
        cldat$portalAction %in% "moveIn" &
          !cldat$observer_id %in% atlaserIDs &
          cldat$countCodedSpecies > 0 & !is.na(cldat$countCodedSpecies)
      ]
    )
  )
  # 4412 with any code

  # what if we restrict to checklists with attempted code?
  length(
    unique(
      cldat$observer_id[
        cldat$portalAction %in% "moveIn" &
          !cldat$observer_id %in% atlaserIDs &
          cldat$countAttemptedSpecies > 0 & !is.na(cldat$countAttemptedSpecies)
      ]
    )
  )
  # 3413 with an attempted code
}

#DECISION: only move into portal if it has a breeding code



#---------------------------Assign portal action---------------------------

#indicate if checklist should be moved out of or into portal
#only move into portal if it has a breeding code

#initialize column
cldat$portalAction <- NA

#get indices
passesChecks <- cldat$passesChecklistChecks
inPortal <- cldat$project_names %in% "New York Breeding Bird Atlas"
coded <- cldat$countCodedSpecies > 0 & !is.na(cldat$countCodedSpecies)

# if passChecks & in portal, portalAction = "noChange"
cldat$portalAction[passesChecks & inPortal] <- "noChange"

# if passChecks & not in portal & has a code, portalAction = "moveIn"
cldat$portalAction[passesChecks & !inPortal & coded] <- "moveIn"

# if passChecks & not in portal & no code, portalAction = "noChange"
cldat$portalAction[passesChecks & !inPortal & !coded] <- "noChange"

# if does not passChecks & in portal, portalAction = "moveOut"
cldat$portalAction[!passesChecks & inPortal] <- "moveOut"

# if does not passChecks & not in portal, portalAction = "noChange"
cldat$portalAction[!passesChecks & !inPortal] <- "noChange"

# ensure all data have an assignment
stopifnot(!anyNA(cldat$portalAction))

if (FALSE) {
  table(cldat$portalAction)
  #  moveIn  moveOut noChange
  #   94814    29021  2694545
}



#---------------------------Save results---------------------------

saveRDS(
  cldat,
  file.path(
    dir_out,
    paste0("all_checklists_with_review_decision_", Sys.Date(), ".rds")
  )
)

moveOut <- cldat$sampling_event_identifier[cldat$portalAction %in% "moveOut"]
utils::write.csv(
  moveOut, file.path(dir_out, "move out of NYBBAIII portal.csv"),
  row.names = FALSE
)
#29201 checklists removed (move out of NYBBAIII portal.csv)

moveIn <- cldat$sampling_event_identifier[cldat$portalAction %in% "moveIn"]
utils::write.csv(
  moveIn, file.path(dir_out, "move into NYBBAIII portal.csv"),
  row.names = FALSE
)
#94814 checklists added (move into NYBBAIII portal.csv)

goodChecklists <- unique(
  cldat$sampling_event_identifier[
    (passesChecks & inPortal) |
    (passesChecks & !inPortal & coded)
  ]
)
utils::write.csv(
  goodChecklists,
  file.path(dir_out, "SEIs that pass checklist review.csv"),
  row.names = FALSE
)
#847855 checklists retained


