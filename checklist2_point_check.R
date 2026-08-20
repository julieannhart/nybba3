#------ Process point checklists ------
#
# Purpose: identify which point checklists should be in the Atlas portal
#   * based on checklist qualities
#
# for data in the Atlas portal:
#   * checklist location within US-NY
#
# for data outside the Atlas portal:
#   * checklist location within US-NY
#   * location type personal
#
# flag extra long distances or large areas
#
# Script written by Julie A. Hart
# last edited 8/20/2026



#-------------------Settings-------------------

# Distance to buffer state boundary to determine if within NY
bufferDistanceNY <- units::set_units(50, "m")

# non-trivial distance > 30 m (per eBird protocol for stationary count)
nonTrivialDistance <- 0.03

# non-trivial area >1 ha
# 1 ha is arbitrary but a good round number and middle of range
nonTrivialArea <- 1

# use the s2 package for spherical geometry
sf::sf_use_s2(TRUE)



#-------------------Set directories-------------------

# set working directory
dir_prj <- ".."

# path to eBird data
dir_ebird <- file.path(dir_prj, "..", "..", "eBird")
stopifnot(dir.exists(dir_ebird))

# path to spatial data
dir_spatial <- file.path(dir_prj, "..", "..", "Spatial_data")
stopifnot(dir.exists(dir_spatial))

# output folder
dir_out <- file.path(dir_prj, "output")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)



#-------------------Load data-------------------

#--- * get sampling data ------

#sampling = checklist-level data

#path to checklist data
fname_sampling <- list.files(
  path = file.path(
    dir_ebird,
    "analysis",
    "output"
  ),
  pattern = "checklist_data",
  full.names = TRUE
)
stopifnot(length(fname_sampling) == 1)

#read in data
sampling_all <- readRDS(fname_sampling)

#subset to only data with checklist_type = point
sampling_pt <- sampling_all[sampling_all$checklist_type %in% "point", ]

sampling_pt$checklist_flag <- FALSE


#--* load NY boundary-----

ny_boundary <- sf::st_read(
  file.path(
    dir_spatial,
    "Boundaries",
    "NYS_Civil_Boundaries_state.gpkg"
  )
) |>
  sf::st_transform(crs = 4326) |>
  sf::st_make_valid()

ny_boundary_buff <- terra::buffer(
  terra::vect(ny_boundary), width = bufferDistanceNY
) |>
  sf::st_as_sf()



#-------------------Check within NY-------------------

#add geom to checklists
sampling_pt_sf <- sf::st_as_sf(
  sampling_pt,
  coords =  c("longitude", "latitude"),
  crs = 4326
)

#determine if checklist locations within NY boundary (+ buffer)
ids1 <- sf::st_within(sampling_pt_sf, ny_boundary_buff)

#if they intersect, set withinNY = TRUE, otherwise FALSE
sampling_pt$withinNY <- lengths(ids1) > 0


# bad if outside NY
ids1 <- !sampling_pt$withinNY
sampling_pt$checklist_flag[ids1] <- TRUE


if (FALSE) {
  table(sampling_pt$withinNY)
  # FALSE    TRUE
  #   180 1378984

  table(sampling_pt$observation_type[!sampling_pt$withinNY])
  # eBird Pelagic Protocol             Incidental             Stationary
  #                      4                    116                     60

  table(sampling_pt$project_names[!sampling_pt$withinNY])
  # New York Breeding Bird Atlas
  #             41
}



#-------------------Check block assignment-------------------

# No need to check because blocks should be assigned either
#    * automatically by eBird, or
#    * determined by script prepare_checklist_data.R
# based on checklist location (if they are within NY).

# It is unknown what happens if someone moves a checklist location into another
# atlas block (either atlas block gets updated or not), but we don't know
# which one would be the correct atlas block.

# bad if not atlas block
ids2 <- is.na(sampling_pt$atlas_block)
sampling_pt$checklist_flag[ids2] <- TRUE



#-------------------Check portal & locality -------------------

# bad if outside portal and not personal location
#unless at Plum Island or Great Gull Island hotspots
island_hotspots <- c("L24945036", "L976281")

#correct portal for Leach's obs
sampling_pt$project_names[sampling_pt$sampling_event_identifier %in% "S179597326"] <- "New York Breeding Bird Atlas"

ids3 <- !sampling_pt$project_names %in% "New York Breeding Bird Atlas" &
  !sampling_pt$locality_type %in% "P" &
  !sampling_pt$locality_id %in% island_hotspots
sampling_pt$checklist_flag[ids3] <- TRUE

#check that Leach's Storm-Petrel retained
#sampling_pt[sampling_pt$sampling_event_identifier %in% "S179597326", ] |> as.data.frame()



#-------------------Distance & area checks-------------------

#--* distance----

# distance should be NA since they are point locations

# inspect records with a distance
if (FALSE) {
  #distance should be NA since they are point locations
  table(sampling_pt$observation_type[!is.na(sampling_pt$effort_distance_km)])
  # Historical  Incidental Nocturnal Flight Call Count  Stationary
  #          1        256                           3        1074

  fivenum(sampling_pt$effort_distance_km, na.rm = T)
  #0.000  0.123  0.304  1.141 64.874

  #look at stationary obs in more detail
  range(
    sampling_pt$effort_distance_km[sampling_pt$observation_type %in% "Stationary"],
    na.rm = T
  )
  #0.016 64.874

  boxplot(sampling_pt$effort_distance_km ~ sampling_pt$observation_type)

  sum(!is.na(sampling_pt$effort_distance_km)) #1334
}

# flag nontrivial distances > 30 m (per eBird protocol for stationary count)
ids4 <- sampling_pt$effort_distance_km >= nonTrivialDistance &
  !is.na(sampling_pt$effort_distance_km)

sampling_pt$checklist_flag[ids4] <- TRUE


#--* area-----

# area should be NA since they are point locations

# inspect records with area
if (FALSE) {
  table(sampling_pt$observation_type[!is.na(sampling_pt$effort_area_ha)])
  #    Banding Historical Stationary
  #          1          1         29

  range(sampling_pt$effort_area_ha, na.rm = T)
  #0.0000 8.9031

  sum(!is.na(sampling_pt$effort_area_ha)) #31

  sampling_pt[ids5, ] |> as.data.frame()
}


# flag non-trivial areas > 1 ha
#1 ha is arbitrary but a good round number and middle of range
#see: fivenum(sampling_pt$effort_area_ha)
ids5 <- sampling_pt$effort_area_ha >= nonTrivialArea &
  !is.na(sampling_pt$effort_area_ha)

sampling_pt$checklist_flag[ids5] <- TRUE


#--* distance * area-----

#NOTE: don't need to worry about distance * area like we do in checklist1



#-------------------Check results-------------------

if (FALSE) {
  table(sampling_pt$checklist_flag, useNA = "always")
  #  FALSE    TRUE    <NA>
  # 1104466  274698      0

  # 1104466 / 1379164 * 100 = 80% pass
  # 274698 / 1379164 * 100 = 20% fail

  head(sampling_pt) |> as.data.frame()
  summary(sampling_pt)

  table(sampling_pt$checklist_flag, sampling_pt$project_names, useNA = "always")

  isBBA <- sampling_pt$project_names %in% "New York Breeding Bird Atlas"
  table(
    r1 = ids1, r2 = ids2, r3 = ids3, r4 = ids4, r5 = ids5, isBBA = isBBA,
    useNA = "ifany"
  ) |>
    as.data.frame()
  #       r1    r2    r3    r4    r5 isBBA   Freq
  # 1  FALSE FALSE FALSE FALSE FALSE FALSE 720063
  # 2   TRUE FALSE FALSE FALSE FALSE FALSE     23
  # 3  FALSE  TRUE FALSE FALSE FALSE FALSE     67
  # 4   TRUE  TRUE FALSE FALSE FALSE FALSE    103
  # 5  FALSE FALSE  TRUE FALSE FALSE FALSE 273094
  # 6   TRUE FALSE  TRUE FALSE FALSE FALSE      0
  # 7  FALSE  TRUE  TRUE FALSE FALSE FALSE      0
  # 8   TRUE  TRUE  TRUE FALSE FALSE FALSE     13
  # 9  FALSE FALSE FALSE  TRUE FALSE FALSE    417
  # 10  TRUE FALSE FALSE  TRUE FALSE FALSE      0
  # 11 FALSE  TRUE FALSE  TRUE FALSE FALSE      0
  # 12  TRUE  TRUE FALSE  TRUE FALSE FALSE      0
  # 13 FALSE FALSE  TRUE  TRUE FALSE FALSE    532
  # 14  TRUE FALSE  TRUE  TRUE FALSE FALSE      0
  # 15 FALSE  TRUE  TRUE  TRUE FALSE FALSE      0
  # 16  TRUE  TRUE  TRUE  TRUE FALSE FALSE      0
  # 17 FALSE FALSE FALSE FALSE  TRUE FALSE      0
  # 18  TRUE FALSE FALSE FALSE  TRUE FALSE      0
  # 19 FALSE  TRUE FALSE FALSE  TRUE FALSE      0
  # 20  TRUE  TRUE FALSE FALSE  TRUE FALSE      0
  # 21 FALSE FALSE  TRUE FALSE  TRUE FALSE     15
  # 22  TRUE FALSE  TRUE FALSE  TRUE FALSE      0
  # 23 FALSE  TRUE  TRUE FALSE  TRUE FALSE      0
  # 24  TRUE  TRUE  TRUE FALSE  TRUE FALSE      0
  # 25 FALSE FALSE FALSE  TRUE  TRUE FALSE      0
  # 26  TRUE FALSE FALSE  TRUE  TRUE FALSE      0
  # 27 FALSE  TRUE FALSE  TRUE  TRUE FALSE      0
  # 28  TRUE  TRUE FALSE  TRUE  TRUE FALSE      0
  # 29 FALSE FALSE  TRUE  TRUE  TRUE FALSE      0
  # 30  TRUE FALSE  TRUE  TRUE  TRUE FALSE      0
  # 31 FALSE  TRUE  TRUE  TRUE  TRUE FALSE      0
  # 32  TRUE  TRUE  TRUE  TRUE  TRUE FALSE      0
  # 33 FALSE FALSE FALSE FALSE FALSE  TRUE 384403
  # 34  TRUE FALSE FALSE FALSE FALSE  TRUE     23
  # 35 FALSE  TRUE FALSE FALSE FALSE  TRUE     13
  # 36  TRUE  TRUE FALSE FALSE FALSE  TRUE     18
  # 37 FALSE FALSE  TRUE FALSE FALSE  TRUE      0
  # 38  TRUE FALSE  TRUE FALSE FALSE  TRUE      0
  # 39 FALSE  TRUE  TRUE FALSE FALSE  TRUE      0
  # 40  TRUE  TRUE  TRUE FALSE FALSE  TRUE      0
  # 41 FALSE FALSE FALSE  TRUE FALSE  TRUE    380
  # 42  TRUE FALSE FALSE  TRUE FALSE  TRUE      0
  # 43 FALSE  TRUE FALSE  TRUE FALSE  TRUE      0
  # 44  TRUE  TRUE FALSE  TRUE FALSE  TRUE      0
  # 45 FALSE FALSE  TRUE  TRUE FALSE  TRUE      0
  # 46  TRUE FALSE  TRUE  TRUE FALSE  TRUE      0
  # 47 FALSE  TRUE  TRUE  TRUE FALSE  TRUE      0
  # 48  TRUE  TRUE  TRUE  TRUE FALSE  TRUE      0
  # 49 FALSE FALSE FALSE FALSE  TRUE  TRUE      0
  # 50  TRUE FALSE FALSE FALSE  TRUE  TRUE      0
  # 51 FALSE  TRUE FALSE FALSE  TRUE  TRUE      0
  # 52  TRUE  TRUE FALSE FALSE  TRUE  TRUE      0
  # 53 FALSE FALSE  TRUE FALSE  TRUE  TRUE      0
  # 54  TRUE FALSE  TRUE FALSE  TRUE  TRUE      0
  # 55 FALSE  TRUE  TRUE FALSE  TRUE  TRUE      0
  # 56  TRUE  TRUE  TRUE FALSE  TRUE  TRUE      0
  # 57 FALSE FALSE FALSE  TRUE  TRUE  TRUE      0
  # 58  TRUE FALSE FALSE  TRUE  TRUE  TRUE      0
  # 59 FALSE  TRUE FALSE  TRUE  TRUE  TRUE      0
  # 60  TRUE  TRUE FALSE  TRUE  TRUE  TRUE      0
  # 61 FALSE FALSE  TRUE  TRUE  TRUE  TRUE      0
  # 62  TRUE FALSE  TRUE  TRUE  TRUE  TRUE      0
  # 63 FALSE  TRUE  TRUE  TRUE  TRUE  TRUE      0
  # 64  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE      0

}



#-------------------Save results--------------------

#save file with checklist_id and sampling_event_identifier and:
# withinNY (T/F)
# checklist_flag (T/F)

saveRDS(
  sampling_pt,
  file.path(
    dir_out,
    paste0("point_checklists_reviewed_", Sys.Date(), ".rds")
  )
)


