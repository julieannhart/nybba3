# Script to read in eBird breeding range boundaries and clip to NY
# for downstream use in data review
#
# Download data here: https://ebird.org/science/status-and-trends/download-data
# Note: need an API key
#
# Cite the eBird Status Data Products using:
# Fink, D., T. Auer, A. Johnston, M. Strimas-Mackey, S. Ligocki, O. Robinson,
# W. Hochachka, L. Jaromczyk, C. Crowley, K. Dunham, A. Stillman, C. Davis,
# M. Stokowski, P. Sharma, V. Pantoja, D. Burgin, P. Crowe, M. Bell, S. Ray,
# I. Davies, V. Ruiz-Gutierrez, C. Wood, A. Rodewald. 2024. eBird Status and
# Trends, Data Version: 2023; Released: 2025. Cornell Lab of Ornithology, Ithaca,
# New York. https://doi.org/10.2173/WZTW8903
#
# This version of the ebirdst package provides access to the 2023 version of the
# eBird Status Data Products and the 2022 version of the eBird Trends Data Products.
#
# Two types of range maps are provided with each download: Range Map (raw) and
# Range Map (smoothed). The smoothed versions are provided at both medium
# resolution ("mr" or 9km) and low resolution ("lr" or 27km).
#
# DECISION: use raw maps because the smoothed maps drop a lot of the smaller
# occupied patches
#
# Each GeoPackage file includes vector data of each species' range and has two
# layers: "range" shows the range boundary and "prediction_area" shows the area
# within which predictions were made. Each layer has up to four multipolygon
# features, one for each season. The "season_name" field has values: breeding,
# nonbreeding, postbreeding_migration, prebreeding_migration, or resident
#
# DECISION: use "range" boundary layer, i.e., use layer = "range" with st_read
# DECISION: use "breeding" polygon for migrants and "resident" polygon for residents
#
# NOTE: currently uses common_name, but that becomes an issue for YBSA which is
# used as an example by eBird and the example only has 27 km resolution data,
# need to use species_code instead for this species
#
# Script written by Julie A. Hart
# last edited 8/20/2026



##----------Load libraries----------

if (FALSE) {
  install.packages("sf", "ebirdst", "terra", "fields", "rnaturalearth")

  library(ebirdst)
  library(sf)
  library(fields)
  library(rnaturalearth)
  library(terra)
  library(dplyr)
}



##----------Settings

#ebirdst::set_ebirdst_access_key("apikeycodehere", overwrite = TRUE)
#after setting the first time, restart R

#distance to buffer the ranges (to capture range edges)
bufferDistanceNY <- units::set_units(5000, "m")

#whether to use the "smoothed" or "raw" ranges
range_type <- "raw"
smooth <- ifelse (range_type == "raw", FALSE, TRUE)



##----------Set paths----------

dir_prj <- ".."

# path to all ranges
dir_rawdata <- ebirdst::ebirdst_data_dir()
stopifnot(dir.exists(dir_rawdata))

# path to filtered and clipped files
dir_output <- file.path(dir_prj, "breeding_ranges_clipped_ny", range_type)
dir.create(dir_output, showWarnings = FALSE, recursive = TRUE)

# path to NY state border
dir_ny <- file.path(dir_prj, "..", "..", "Spatial_data/Boundaries")

# path to NY breeders
dir_spp <- file.path(dir_prj, "..", "..", "DataReview/data")



##----------Export breeding season dates----------

if (FALSE) {
  dplyr::glimpse(ebirdst::ebirdst_runs) #shows dates for all species
  View(ebirdst::ebirdst_runs)

  write.csv(
    x = as.data.frame(ebirdst::ebirdst_runs),
    file = file.path(dir_output, "eBirdst_runs_table_v2024.csv"),
    row.names = FALSE
  )
}



##----------Load NY boundary----------

# Load NY border
ny_boundary <- sf::st_read(
  file.path(dir_ny, "NYS_Civil_Boundaries_state.gpkg")
) |>
  sf::st_transform(crs = 4326) |>
  sf::st_make_valid()

# Add buffer
# use terra to buffer because buffering via s2 does not create smooth buffers
ny_boundary_buff <- terra::buffer(
  terra::vect(ny_boundary), width = bufferDistanceNY
) |>
  sf::st_as_sf()



##----------Load species----------

# Load list of breeders in NY
spp <- read.csv(
  file.path(dir_spp, "breeding_species_plus_hybrids.csv")
)[, "Species", drop = TRUE]

# check species list against eBird taxonomy v2024
modeled_species_common <- ebirdst::ebirdst_runs$common_name
ids <- which(!spp %in% modeled_species_common)

# species without modeled ranges
spp[ids]

# remove unmodeled species
spp2 <- spp[-ids]
stopifnot(length(which(!spp2 %in% modeled_species_common)) == 0)

# add irregular names
spp2 <- c(spp2, "Northern House Wren")



##----------Clip species ranges----------

for (sp in spp2) {

  # Translate common names to EST codes and handle special cases
  estCode <- switch(
    EXPR = sp,
    # Northern and Southern House Wren are currently combined for EST product
    "Northern House Wren" = "y01309", # "Northern/Southern House Wren",
    # Yellow-bellied Sapsucker has two entries (one as a low-res example)
    "Yellow-bellied Sapsucker" = "yebsap",
    ebirdst::ebirdst_runs$species_code[ebirdst::ebirdst_runs$common_name %in% sp]
  )

  stopifnot(length(estCode) == 1L)

  #download range maps
  suppressMessages(
    ebirdst::ebirdst_download_status(
      species = estCode,
      download_ranges = TRUE,
      download_abundance = FALSE
    )
  )

  #use 9 km version (raw or smoothed)
  ranges <- ebirdst::load_ranges(
    species = estCode,
    resolution = "9km",
    smoothed = smooth
  )

  #get relevant season (breeding, resident)
  breed <- ifelse(
    ebirdst::ebirdst_runs$is_resident[ebirdst::ebirdst_runs$species_code %in% estCode],
    "resident",
    "breeding"
  )
  range_breeding <- dplyr::filter(ranges, season == breed)

  # check that each species has one and only one feature
  stopifnot(isTRUE(nrow(range_breeding) == 1))

  # ensure geometry is valid
  ny_breeding <- sf::st_make_valid(range_breeding)

  # set out path and file name
  fname <- file.path(
    dir_output,
    paste0(sub("/", "_", sp), "-breeding-range-", range_type, "-9k-2024-NY.gpkg")
  )

  # clip ranges to NY and save
  if (!file.exists(fname)) {
    # Suppress messages about assuming spatial uniformity of features
    # Suppress warnings about using planar calculations despite geographic data
    tmp <- suppressMessages(suppressWarnings(
      sf::st_intersection(ny_breeding, ny_boundary_buff[, 0])
    ))

    if (nrow(tmp) > 0) {
      if (!sf::st_is_valid(tmp)) {
        tmp <- sf::st_make_valid(tmp)
        stopifnot(sf::st_is_valid(tmp))
      }
      sf::st_write(tmp, dsn = fname, layer = "range", quiet = TRUE, append = FALSE)
    } else {
      cat(
        "Species", shQuote(sp),
        "has no eBird breeding/resident area in NY",
        fill = TRUE
      )
    }
 }
}



##----------Handle hybrid ranges----------

#---* GWWA x BWWA hybrids----
gw_range <- sf::st_read(
  file.path(
    dir_output,
    paste0("Golden-winged Warbler-breeding-range-", range_type, "-9k-2024-NY.gpkg")
  ),
  layer = "range",
  quiet = TRUE
) |>
  sf::st_make_valid()

bw_range <- sf::st_read(
  file.path(
    dir_output,
    paste0("Blue-winged Warbler-breeding-range-", range_type, "-9k-2024-NY.gpkg")
  ),
  layer = "range",
  quiet = TRUE
) |>
  sf::st_make_valid()

gwbw_hybrid_range <- sf::st_intersection(gw_range, bw_range) |>
  sf::st_make_valid()

sf::st_write(
  gwbw_hybrid_range,
  dsn = file.path(dir_output,
    paste0("Brewster's Warbler (hybrid)-breeding-range-", range_type, "-9k-2024-NY.gpkg")
  ),
  layer = "range",
  quiet = TRUE,
  append = FALSE
)
sf::st_write(
  gwbw_hybrid_range,
  dsn = file.path(dir_output,
    paste0("Lawrence's Warbler (hybrid)-breeding-range-", range_type, "-9k-2024-NY.gpkg")
  ),
  layer = "range",
  quiet = TRUE,
  append = FALSE
)
sf::st_write(
  gwbw_hybrid_range,
  dsn = file.path(dir_output,
    paste0("Golden-winged x Blue-winged Warbler (hybrid)-breeding-range-", range_type, "-9k-2024-NY.gpkg")
  ),
  layer = "range",
  quiet = TRUE,
  append = FALSE
)
sf::st_write(
  gwbw_hybrid_range,
  dsn = file.path(dir_output,
    paste0("Golden-winged_Blue-winged Warbler-breeding-range-", range_type, "-9k-2024-NY.gpkg")
  ),
  layer = "range",
  quiet = TRUE,
  append = FALSE
)


#---* MALL x ABDU hybrid----
mall_range <- sf::st_read(
  file.path(
    dir_output,
    paste0("Mallard-breeding-range-", range_type, "-9k-2024-NY.gpkg")
  ),
  layer = "range",
  quiet = TRUE
) |>
  sf::st_make_valid()

abdu_range <- sf::st_read(
  file.path(
    dir_output,
    paste0("American Black Duck-breeding-range-", range_type, "-9k-2024-NY.gpkg")
  ),
  layer = "range",
  quiet = TRUE
) |>
  sf::st_make_valid()

mall_abdu_hybrid_range <- sf::st_intersection(mall_range, abdu_range) |>
  sf::st_make_valid()

sf::st_write(
  mall_abdu_hybrid_range,
  dsn = file.path(
    dir_output,
    paste0("Mallard x American Black Duck (hybrid)-breeding-range-", range_type, "-9k-2024-NY.gpkg")
  ),
  layer = "range",
  quiet = TRUE,
  append = FALSE
)



##----------Species without ranges----------

#--Species without ranges in NY

#list for smoothed ranges
# Species 'Canvasback' has no eBird breeding/resident area in NY
# Species 'King Rail' has no eBird breeding/resident area in NY
# Species 'Black Rail' has no eBird breeding/resident area in NY
# Species 'Golden Eagle' has no eBird breeding/resident area in NY
# Species 'Long-eared Owl' has no eBird breeding/resident area in NY
# Species 'Short-eared Owl' has no eBird breeding/resident area in NY
# Species 'American Three-toed Woodpecker' has no eBird breeding/resident area in NY
# Species 'Philadelphia Vireo' has no eBird breeding/resident area in NY
# Species 'Loggerhead Shrike' has no eBird breeding/resident area in NY
# Species 'Cape May Warbler' has no eBird breeding/resident area in NY
# Species 'Bay-breasted Warbler' has no eBird breeding/resident area in NY
# Species "Wilson's Warbler" has no eBird breeding/resident area in NY

#list for raw ranges
# Species 'Canvasback' has no eBird breeding/resident area in NY
# Species 'King Rail' has no eBird breeding/resident area in NY
# Species 'Black Rail' has no eBird breeding/resident area in NY
# Species 'Golden Eagle' has no eBird breeding/resident area in NY
# Species 'Long-eared Owl' has no eBird breeding/resident area in NY
# Species 'Short-eared Owl' has no eBird breeding/resident area in NY
# Species 'American Three-toed Woodpecker' has no eBird breeding/resident area in NY
# Species 'Loggerhead Shrike' has no eBird breeding/resident area in NY
# Species 'Cape May Warbler' has no eBird breeding/resident area in NY
# Species 'Bay-breasted Warbler' has no eBird breeding/resident area in NY
# Species "Wilson's Warbler" has no eBird breeding/resident area in NY
# Species 'Mottled Duck' has no eBird breeding/resident area in NY
# Species 'Swallow-tailed Kite' has no eBird breeding/resident area in NY


#--Species without any modeled ranges

spp$Species[ids] # (minus House Wren)
# "European Goldfinch"
# "Brewster's Warbler (hybrid)"
# "Lawrence's Warbler (hybrid)"
# "Golden-winged/Blue-winged Warbler"
# "Golden-winged x Blue-winged Warbler (hybrid)"
# "Mallard x American Black Duck (hybrid)"
