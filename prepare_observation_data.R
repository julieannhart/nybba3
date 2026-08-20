##----------------------------README-------------------------------
# Purpose:
# Prepares combined ebd and sensitive data for downstream applications, e.g.,
# assessing progress, data review, sharing data with Heritage, etc.
#
# Steps:
# 1. request and download latest EBD from eBird every 15th of the month (1 day)
# 2. request and get sensitive data from Jenna (up to 1 week)
# 3. download files from eBird on first of each month from nybba3@gmail.com
# 4. move files to eBird/MonthlyDataDownloads/data and archive older data files
# 5. get latest list of atlas users from Ian and update file path (1-3 days)
# 6. download latest eBird taxonomy
# 7. ensure that common names of Heritage tracked species match current taxonomy
# 8. select desired settings
# 9. run script
#
# NOTE: When auk_unique() is run, a new field is created (checklist_id),
# which is populated with group_identifier for group checklists and
# sampling_event_identifier otherwise; this is now a unique identifier for checklists.
# Specifically, for each species, it retains only the first observation of that
# species, which is typically the one submitted by the primary observer (i.e.
# the person who submit the checklist to eBird). Note that the resulting
# “checklist” will be a combination of all the species seen across all copies
# of the group checklist. This function is CALLED AUTOMATICALLY by read_ebd()
# and read_sampling().
#
# NOTE: need to review each checklist from each observer
# but if multiple observers, refer to other shared checklists for context
# case1: only one shared checklist has codes --> treat separately
# case2: one observer used incorrect codes, others didn't --> treat separately
# case3: only one observer added comments or media --> approve for all shared
# case4: different locations --> treat separately (does auk recognize this?)
# case5: different portals --> treat separately
#
# NOTE:
# "obsr6794908" is point count/abundance data
# "obsr8116219" is NY Audubon point counts
# "obsr96857", "obsr538427" are J&D
#
# Script last updated:
# 2025-07-21 by Julie Hart



##---------------------------TODO---------------------------

# TODO: refine nybba_rollup for sharing with eBird



##---------------------------Load libraries---------------------------

if (FALSE) {
  remotes::install_github("CornellLabofOrnithology/auk")
}


##---------------------------Settings---------------------------

include_all_obs <- TRUE # FALSE = subset to obs with breeding codes
include_outside_portal <- TRUE # FALSE = subset to Atlas portal only
include_all_spp <- TRUE # FALSE = subset to rare breeders only
do_atlaser_perspective <- FALSE # TRUE = duplicate shared observations to entries for each atlaser

hybrids_to_retain <- c(
  "Brewster's Warbler (hybrid)",
  "Golden-winged x Blue-winged Warbler (hybrid)",
  "Lawrence's Warbler (hybrid)",
  "Mallard x American Black Duck (hybrid)"
)

slashes_to_retain <- "Golden-winged/Blue-winged Warbler"

domestic_to_retain <- "Rock Pigeon" # Rock Pigeon (Feral Pigeon)



##---------------------------Set Paths---------------------------

# Set working directory to location of this script
dir_prj <- "."

#path to eBird data
fname_ebirdny <- list.files(
  path = file.path(
    dir_prj,
    "..",
    "..",
    "EBD_public_data"
  ),
  pattern = "ebd_US-NY_[[:digit:]]{6}_[[:digit:]]{6}_unv_smp_rel[[:alpha:]]{3}-[[:digit:]]{4}.txt",
  full.names = TRUE
)
stopifnot(length(fname_ebirdny) == 1)

#path to sensitive eBird data
fname_sensitive <- list.files(
  path = file.path(
    dir_prj,
    "..",
    "..",
    "EBD_sensitive_data"
  ),
  pattern = "ebd_sensitive",
  full.names = TRUE
)
stopifnot(length(fname_sensitive) == 1)

#path to hidden records
fname_hidden <- list.files(
  path = file.path(dir_prj, "..", "..", "monthly_data_exports", "data"),
  pattern = "User-hidden",
  full.names = TRUE
)
stopifnot(length(fname_hidden) == 1)

#path to zero species checklists
fname_nospecies <- list.files(
  path = file.path(dir_prj, "..", "..", "monthly_data_exports", "data"),
  pattern = "Zero-species",
  full.names = TRUE
)
stopifnot(length(fname_nospecies) == 1)

#path to zero count observations
fname_zerocount <- list.files(
  path = file.path(dir_prj, "..", "..", "monthly_data_exports", "data"),
  pattern = "Zero count",
  full.names = TRUE
)
stopifnot(length(fname_zerocount) == 1)

#path to external data
fname_external <- list.files(
  path = file.path(
    dir_prj,
    "..",
    "..",
    "..",
    "external data",
    "Prepare external data for EBD"
  ),
  pattern = "external_data_for_EBD_",
  full.names = TRUE
)
stopifnot(length(fname_external) == 1)

#path to atlaser user info
fname_atlasers <- list.files(
  path = file.path(dir_prj, "..", "..", "atlasers", "data"),
  pattern = "NY_atlasers",
  full.names = TRUE
)
stopifnot(length(fname_atlasers) == 1)

#path to breeding code translation
fname_codes <- file.path(
  dir_prj,
  "..",
  "..",
  "ebird_internal_codes.csv"
)

#path to ebird taxonomy
auk::auk_ebd_version(fname_ebirdny) #gets tax version used in ebd file
taxonomy_version <- 2024
fname_taxonomy <- file.path(
  dir_prj,
  "..",
  "..",
  "taxonomy_eBird_Clements",
  paste0("eBird_taxonomy_v", taxonomy_version, ".csv")
)

#path to ebird protocol lookup table
fname_protocols <- file.path(
  dir_prj,
  "..",
  "..",
  "ebird_protocols_2025-05.txt"
)

#path to rare breeders list
fname_tracked <- file.path(
  dir_prj,
  "..",
  "..",
  "..",
  "DataReview",
  "data",
  "heritage_tracked_species.csv"
)

#path to blocks with attributes
fname_blocks <- file.path(
  dir_prj,
  "..",
  "..",
  "..",
  "Spatial_data",
  "atlas_blocks",
  "spatial_atlas_blocks",
  "results",
  "block-vector_with_attributes.gpkg"
)

#path to sampling data (for nospecies)
fname_sampling <- list.files(
  path = file.path(
    dir_prj,
    "..",
    "..",
    "EBD_public_data"
  ),
  pattern = "[[:alnum:]]+_sampling.txt",
  full.names = TRUE
)
stopifnot(length(fname_ebirdny) == 1)

#ebd outpaths
output_file_tag <- paste0(
  if (include_outside_portal) "_AllPortals" else "_AtlasPortal",
  if (include_all_obs) "_AllData" else "_CodesOnly",
  if (include_all_spp) "_AllSpp" else "_TrackedSpp"
)

output_file_tag2 <- if (do_atlaser_perspective) {
  paste0(output_file_tag, "_AtlaserPerspective")
} else {
  output_file_tag
}

fname_ebirdny_data <- sub(
  pattern = ".txt",
  replacement = paste0(output_file_tag2, ".rds"),
  x = fname_ebirdny,
  fixed = TRUE
)

fname_ebirdny_auk_filtered <- sub(
  pattern = ".txt",
  replacement = paste0(output_file_tag, "_auk_filtered.txt"),
  x = fname_ebirdny,
  fixed = TRUE
)



##---------------------------Functions---------------------------

get_taxonomy <- function(common_name, tax, stop_on_NA = TRUE) {
  res_vars <- c("scientific_name", "taxonomic_order")
  res <- as.data.frame(array(
    dim = c(length(common_name), 2),
    dimnames = list(NULL, res_vars)
  ))

  tmp <- match(common_name, tax$common_name, nomatch = 0)
  res[tmp > 0, ] <- tax[tmp, c("scientific_name", "taxon_order")]

  if (any(tmp == 0)) {
    cat(
      "No matches of `common_name` in `tax$common_name` for: ",
      paste(shQuote(unique(common_name[tmp == 0])), collapse = ", ")
    )
  }

  if (stop_on_NA) stopifnot(!anyNA(res))

  res
}

#' @examples
#' translate_codes("P20", "protocol_code", "protocol_name", protocols)
#' translate_codes("Banding", "protocol_name", "protocol_code", protocols)
#' translate_codes("P20", "protocol_code", "observation_type", protocols)
#' translate_codes("NY", "INTERNAL", "PUBLIC", codes)
#' translate_codes("NY", "INTERNAL", "CATEGORY", codes)
#' translate_codes("B", "PUBLIC", "CATEGORY", codes)
#' translate_codes(c("B", "", "ON"), "PUBLIC", "CATEGORY", codes)
#' translate_codes("", "PUBLIC", "CATEGORY", codes)
#' translate_codes(NA, "PUBLIC", "CATEGORY", codes)
translate_codes <- function(x, fromName, toName, lookupTable) {
  indices <- match(x, table = lookupTable[, fromName], nomatch = 0L)
  res <- rep(NA, length(x))
  res[indices > 0L] <- lookupTable[indices, toName]
  res
}

#' @examples
#' selectHighestCode(c("S", "B"), publicBreedingCodes = codes$PUBLIC)
#' selectHighestCode(c("S", "FL"), publicBreedingCodes = codes$PUBLIC)
#' selectHighestCode(c("", "FL"), publicBreedingCodes = codes$PUBLIC)
#' selectHighestCode(c(NA, "FL"), publicBreedingCodes = codes$PUBLIC)
#' selectHighestCode(c(NA, NA), publicBreedingCodes = codes$PUBLIC)
selectHighestCode <- function(x, publicBreedingCodes) {
  if (all(x %in% c("", NA))) {
    NA
  } else {
    ids <- match(x, publicBreedingCodes)
    publicBreedingCodes[min(ids, na.rm = TRUE)]
  }
}

#' @examples
#' mergeComments(NA)
#' mergeComments(c(NA, NA))
#' mergeComments(c(NA, "NA", "", NA))
#' mergeComments("My favorite bird")
#' mergeComments(c("My favorite bird", "My other favorite bird"))
#' mergeComments(c("My favorite bird", "My favorite bird"))
#' mergeComments(c(NA, "My favorite bird", "", "My other favorite bird"))
mergeComments <- function(x) {
  x <- unique(x[!x %in% c("", "NA", NA)])
  n <- length(x)
  switch(
    EXPR = 1L + min(2L, n),
    "",
    x,
    paste(
      x[[1L]],
      paste0("Comment", seq_len(n)[-1L], ": ", x[-1L], collapse = " | "),
      sep = " | "
    )
  )
}


#' Rollup
#'
#' This rollup version accumulates unique checklist comments,
#' species comments, the media flag, and selects the highest
#' breeding code/category.
#'
#' @examples
#' x <- data.frame(
#'   global_unique_identifier = c("OBS1091519927", "OBS1091519928", "OBS1091519929"),
#'   sampling_event_identifier = c("S83112315", "S83112315", "S83112315"),
#'   observation_count = c("2", "1", "10"),
#'   checklist_comments = c("", "", ""),
#'   species_comments = c("", "has species comments", ""),
#'   breeding_code = c("S", "FL", NA),
#'   breeding_category = c("C2", "C4", NA),
#'   has_media = c(NA, TRUE, FALSE),
#'   scientific_name = c("Buteo jamaicensis", "Buteo jamaicensis", "Columba livia"),
#'   category = c("species", "species", "domestic")
#' )
#'
#' auk::auk_rollup(x)
#' nybba_rollup(x, breedingCodes = codes)
nybba_rollup <- function(
  x,
  breedingCodes,
  taxonomy_version,
  taxonomy,
  combineSharedChecklists = TRUE,
  drop_higher = TRUE
) {
  assertthat::assert_that(is.data.frame(x), "scientific_name" %in%
      names(x))
  if (isTRUE(attr(x, "rollup"))) {
    return(x)
  }
  cid <- if (combineSharedChecklists) {
    "checklist_id"
  } else {
    "sampling_event_identifier"
  }
  stopifnot(cid %in% names(x))
  if (!missing(taxonomy)) {
    stopifnot(
      utils::hasName(
        taxonomy,
        c("scientific_name", "taxon_order", "category", "report_as")
      )
    )
    tax_full <- taxonomy
  } else if (missing(taxonomy_version) || taxonomy_version == auk::auk_version()$taxonomy_version) {
    tax_full <- auk::ebird_taxonomy
  } else {
    stopifnot(is_integer(taxonomy_version), length(taxonomy_version) == 1)
    tax_full <- auk::get_ebird_taxonomy(version = taxonomy_version)
  }
  if (drop_higher) {
    include <- "species"
  } else {
    include <- c("species", "slash", "spuh", "hybrid")
  }
  undesc <- dplyr::filter(
    tax_full, .data$category == "form", is.na(.data$report_as)
  )
  tax <- dplyr::filter(tax_full, .data$category %in% include)
  tax <- rbind(tax, undesc)
  tax <- dplyr::select(tax, "scientific_name", "taxon_order")
  species_prefilter <- unique(x$scientific_name)
  x <- dplyr::inner_join(x, tax, by = "scientific_name")
  species_after <- unique(x$scientific_name)
  removed_species <- setdiff(species_prefilter, species_after)
  tax_dropped <- dplyr::filter(tax_full, !.data$category %in% include)
  removed_species <- setdiff(removed_species, tax_dropped$scientific_name)
  if (length(removed_species) > 0) {
    warning_message <- paste("Removed the following species due to invalid taxonomy:\n",
      paste(removed_species, collapse = ", "), "\n\nIf taxonomy was recently updated, try updating the package:",
      "\n- Run this command in R: install.packages('auk')",
      "\n or install the latest version from GitHub: remotes::install_github('CornellLabofOrnithology/auk')")
    warning(warning_message, call. = FALSE)
  }
  if (nrow(x) == 0) {
    if ("subspecies_common_name" %in% names(x)) {
      x$subspecies_common_name <- NULL
    }
    if ("subspecies_scientific_name" %in% names(x)) {
      x$subspecies_scientific_name <- NULL
    }
    attr(x, "rollup") <- TRUE
    return(dplyr::as_tibble(x))
  }

  x$breedingCode <- match(x$breeding_code, breedingCodes$PUBLIC, nomatch = NA)

  vars_select <- c(cid, "scientific_name", "observation_count")
  vars_select <- c(vars_select, "breedingCode")
  vars_select <- c(vars_select, "species_comments", "checklist_comments")
  vars_select <- c(vars_select, "has_media")
  sp <- dplyr::select(x, dplyr::all_of(vars_select))
  suppressWarnings({
    sp$count <- as.integer(sp$observation_count)
  })
  sp <- dplyr::group_by(
    sp, dplyr::across(dplyr::all_of(c(cid, "scientific_name")))
  )
  sp <- dplyr::summarise(
    sp,
    count = sum(.data$count),
    minBreedingCode = suppressWarnings(min(.data$breedingCode, na.rm = TRUE)),
    checklistComments = mergeComments(.data$checklist_comments),
    speciesComments = mergeComments(.data$species_comments),
    hasMedia = any(.data$has_media, na.rm = TRUE),
    .groups = "drop"
  )
  sp$minBreedingCode[!is.finite(sp$minBreedingCode)] <- NA
  sp <- dplyr::mutate(
    sp,
    count = as.character(.data$count),
    count = dplyr::coalesce(.data$count, "X")
  )
  x <- dplyr::group_by(
    x, dplyr::across(dplyr::all_of(c(cid, "scientific_name")))
  )
  x <- dplyr::slice_min(
    x, n = 1, order_by = .data$taxon_order, with_ties = FALSE
  )
  x <- dplyr::ungroup(x)
  x <- dplyr::inner_join(x, sp, by = c(cid, "scientific_name"))
  x <- dplyr::mutate(
    x,
    observation_count = .data$count,
    breeding_code = breedingCodes$PUBLIC[.data$minBreedingCode],
    breeding_category = translate_codes(
      breeding_code, "PUBLIC", "CATEGORY", breedingCodes
    ),
    checklist_comments = .data$checklistComments,
    species_comments = .data$speciesComments,
    has_media = .data$hasMedia
  )
  x <- dplyr::select(
    x, -"count", -"taxon_order",
    -"breedingCode", -"minBreedingCode",
    -"speciesComments", -"checklistComments", -"hasMedia"
  )
  if ("category" %in% names(x)) {
    x$category <- ifelse(x$category %in% include, x$category, "species")
  }
  if ("subspecies_common_name" %in% names(x)) {
    x$subspecies_common_name <- NULL
  }
  if ("subspecies_scientific_name" %in% names(x)) {
    x$subspecies_scientific_name <- NULL
  }
  attr(x, "rollup") <- TRUE
  dplyr::as_tibble(x)
}



##---------------------------Load & Prep Data---------------------------

#--* atlaser info-----
atlasers <- if (do_atlaser_perspective) {
  utils::read.csv(fname_atlasers)
}

#--* breeding codes-----
codes <- utils::read.csv(fname_codes)

#--* blocks----
blocks <- sf::read_sf(fname_blocks) |>
  sf::st_transform(crs = 4326) |>
  sf::st_make_valid()
#   #--Block info
# block_info <- utils::read.csv(fname_blocks)

#--* protocols----
protocols <- utils::read.delim(fname_protocols, sep = "\t", quote = "")
colnames(protocols) <- tolower(colnames(protocols))
colnames(protocols) <- gsub(
  pattern = "\\.",
  replacement = "_",
  x = colnames(protocols)
)

#--* sampling data----
#do not use unique = FALSE for this script, need checklist_id for no species
sampling <- auk::read_sampling(fname_sampling)

#--* taxonomy----
#if subsetting by species, check that species input list is in eBird taxonomy
#if names don't match or aren't unique, auk will produce an error

if (file.exists(fname_taxonomy)) {
  taxonomy <- utils::read.csv(fname_taxonomy)

  # Update names in manually downloaded file to match names as obtained by
  # `auk::get_ebird_taxonomy()`
  cns <- colnames(taxonomy)
  colnames(taxonomy)[cns %in% "SCI_NAME"] <- "scientific_name"
  colnames(taxonomy)[cns %in% "PRIMARY_COM_NAME"] <- "common_name"
  colnames(taxonomy)[cns %in% "SPECIES_CODE"] <- "species_code"
  colnames(taxonomy)[cns %in% "CATEGORY"] <- "category"
  colnames(taxonomy)[cns %in% "TAXON_ORDER"] <- "taxon_order"
  colnames(taxonomy)[cns %in% "ORDER"] <- "order"
  colnames(taxonomy)[cns %in% "FAMILY"] <- "family"
  colnames(taxonomy)[cns %in% "REPORT_AS"] <- "report_as"

  if (!include_all_spp) {
    spp <- as.data.frame(taxonomy[taxonomy$category %in% "species", ])
    tracked <- as.vector(read.delim(fname_tracked, sep = ",", header = TRUE))
    stopifnot(tracked$common_name %in% spp$common_name)
  }
  #which(!tracked$common_name %in% spp$common_name)


} else {
  # 2026-May-27: The 2024 taxonomy was manually downloaded from the eBird
  # website differs. The file differs in common_name (but not scientific_name)
  # with the taxonomy obtained by `auk::get_ebird_taxonomy(version = 2024)`
  taxonomy <- auk::get_ebird_taxonomy(version = taxonomy_version)

  utils::write.csv(taxonomy, file = fname_taxonomy, row.names = FALSE)
}



#----- Run big import and filtering ------
if (!file.exists(fname_ebirdny_data)) {

  ##-----* EBD data-----
  if (!file.exists(fname_ebirdny_auk_filtered)) {
    # 1. define filters
    ebd_filters <- auk::auk_ebd(fname_ebirdny) |>
      auk::auk_state("US-NY")

    if (!include_outside_portal) {
      ebd_filters <- auk::auk_project(ebd_filters, "New York Breeding Bird Atlas")
    }

    if (!include_all_obs) {
      ebd_filters <- auk::auk_breeding(ebd_filters)
    }

    if (!include_all_spp) {
      ebd_filters <- auk::auk_species(
        x = ebd_filters,
        species = tracked$common_name,
        taxonomy_version = 2024
      )
    }

    # 2. run filtering
    ebd_filtered <- auk::auk_filter(
      ebd_filters,
      file = fname_ebirdny_auk_filtered
    )
  }


  # 3. read filtered ebd data file
  #no rollup to retain desired hybrids/domestics/slashes
  #rollup = F retains hybrids
  #unique = F retains each observer's data even on shared checklists,
  #i.e., not lumped by group_id, means you end up with dupes
  # unique = F -> does not create a checklist_id column
  ebd_df <- auk::read_ebd(
    fname_ebirdny_auk_filtered, rollup = FALSE, unique = FALSE
  ) |>
    as.data.frame()

  #check that common names used in data are available in the taxonomy
  stopifnot(
    ebd_df[["common_name"]] %in% taxonomy[["common_name"]]
  )

  #add data source column
  ebd_df$source <- "ebd_reviewed"

  if (FALSE) {
    # Example of shared checklist with different location, effort, species, codes
    # (which would get lost with auk unique)
    ebd_df[ebd_df$group_identifier %in% "G10389967", ]
    ebd_df[ebd_df$sampling_event_identifier %in% "S140708189", ]

    #check that Leach's Storm-Petrel record is kept
    ebd_df[ebd_df$sampling_event_identifier %in% "S179597326", ]
  }



  ##-----* Sensitive data-----

  #--read in data
  # unique = F -> does not create a checklist_id column
  sensitive <- auk::read_ebd(fname_sensitive, unique = FALSE, rollup = FALSE) |>
    as.data.frame()

  #--add empty columns for subspecies
  #since we don't need the subspp info for any sensitive species
  tmp <- c("subspecies_common_name", "subspecies_scientific_name")
  sensitive[, tmp] <- NA

  #--add data source column
  sensitive$source <- "sensitive"



  ##-----* Hidden data-----

  #--read in data
  hidden <- utils::read.delim(fname_hidden, sep = "\t", quote = "")

  # checklist_id of hidden data does not mean anything -> delete
  stopifnot(
    !hidden$checklist_id %in% sampling$checklist_id
  )

  # There is something wrong with hidden data, see:
  #hidden[hidden$checklist_id == "CL23935", ]
  #multiple locations, dates, etc for the same checklist_id
  #but https://ebird.org/atlasny/checklist/S115346889 looks fine
  unique(hidden$sampling_event_identifier[hidden$checklist_id == "CL23935"])
  #WHAT IS GOING ON?!!!!
  #Gabriel has the same issue, something wrong with the hidden data
  #DECISION: replace checklist_id with sampling_event_identifier?
  #can also have dupe records with different loc_types
  #DECISION: remove dupes ignoring loc_types

  #remove dupes because of loc_type
  tmp <- unique(hidden[, !colnames(hidden) %in% "loc_type"])
  ids <- match(tmp$global_unique_identifier, hidden$global_unique_identifier, nomatch = 0L)
  hidden <- cbind(tmp, locality_type = hidden$loc_type[ids])

  #check there are no remaining dupe species per sei
  #if use common_name, this will stop because diff subspecies/forms are separate
  #need to do auk_rollup at the end
  #BUT auk_rollup only adds count of individuals doesn't merge comments or has_media
  stopifnot(
    !anyDuplicated(hidden[, c("sampling_event_identifier", "orig_species_code")])
  )

  stopifnot(hidden$proj_period_id %in% "EBIRD_ATL_NY_2020")

  #--reformat data
  hidden$project_names <- "New York Breeding Bird Atlas"
  hidden$observer_id <- gsub(
    pattern = "USER",
    replacement = "obsr",
    x = hidden$observer_id
  )
  hidden$observation_date <- format(
    as.POSIXlt(hidden$to_char), format = "%Y-%m-%d"
  )
  hidden$time_observations_started <- format(
    as.POSIXlt(hidden$to_char), format = "%H:%M:%S"
  )
  hidden$breeding_code <- translate_codes(
    hidden$breeding_code, "INTERNAL", "PUBLIC", codes
  )
  hidden$behavior_code <- translate_codes(
    hidden$behavior_code, "INTERNAL", "PUBLIC", codes
  )
  hidden$all_species_reported <- as.logical(hidden$all_species_reported)

  #--cases when how_many_atmost differ from how_many_atleast
  if (FALSE) {
    ids <- hidden$how_many_atleast != hidden$how_many_atmost
    table(hidden$how_many_atmost[ids], hidden$how_many_atleast[ids])
    # --> if they differ, then
    # how_many_atleast is 1 and how_many_atmost is 999,999,999
    # ==> this was most likely orginally an "X" entry
  }

  hidden$observation_count <- hidden$how_many_atleast
  ids <- which(hidden$how_many_atmost == "999,999,999")
  hidden$observation_count[ids] <- "X"

  #--change column names
  hidden <- dplyr::rename(
    hidden,
    category = taxon_category,
    last_edited_date = last_edited_dt,
    checklist_comments = trip_comments
  )

  #--add defined or calculated columns
  hidden$country <- "United States"
  hidden$country_code <- "US"
  hidden$duration_minutes <- round(hidden$duration_hrs * 60)
  hidden$state <- "New York"
  hidden$has_media <- NA
  hidden <- cbind(hidden, get_taxonomy(hidden$common_name, tax = taxonomy))
  hidden$protocol_name <- translate_codes(
    hidden$protocol_code, "protocol_code", "protocol_name", protocols
  )
  hidden$observation_type <- translate_codes(
    hidden$protocol_code, "protocol_code", "observation_type", protocols
  )
  hidden$project_identifiers <- "1006"
  hidden$source <- "hidden"

  #--add NA columns
  tmp <- c(
    "age_sex", "bcr_code", "county", "county_code", "effort_area_ha",
    "iba_code", "locality", "usfws_code", "taxon_concept_id", "exotic_code",
    "subspecies_common_name", "subspecies_scientific_name", "observer_orcid_id"
  )
  stopifnot(!tmp %in% colnames(hidden))
  hidden[, tmp] <- NA

  #--drop columns from hidden
  dropcols <- c("orig_species_code", "sub_reviewstatus", "how_many_atleast",
    "how_many_atmost", "last_name", "first_name", "duration_hrs",
    "obs_time_valid", "checklist_id", "proj_period_id", "to_char")
  hidden <- hidden[, !(names(hidden) %in% dropcols)]



  ##-----* No species checklists-----
  # Do not include in alldat because rollup does not work without species
  # These checklists are included in sampling_ebird

  if (FALSE) {
    #--read in data
    nospecies <- utils::read.delim(fname_nospecies, sep = "\t", quote = "")

    #--add checklist_id column: we need to create it to join in sampling data frame
    nospecies$checklist_id <- nospecies$sampling_event_identifier
    ids <- !nospecies$group_identifier %in% c(NA, "")
    nospecies$checklist_id[ids] <- nospecies$group_identifier[ids]

    # #explore data--check for overlap
    # nospecies_sei <- unique(nospecies$sampling_event_identifier)
    # sampling_sei <- unique(sampling$sampling_event_identifier)
    # setdiff(nospecies_sei, sampling_sei) #no matches = 28
    # #these are either hidden or flagged for checklist-level issues
    # #can safely remove checklists not in sampling

    #grab info from sampling table b/c it contains more info
    ids <- sampling$checklist_id %in% nospecies$checklist_id
    nospecies2 <- sampling[ids, ]

    #--add defined or calculated columns
    nospecies2$category <- "species"
    nospecies2$common_name <- "NoSpecies"
    nospecies2$global_unique_identifier <- paste0(
      "nospecies", nospecies2$sampling_event_identifier
    )
    nospecies2$has_media <- FALSE
    nospecies2$observation_count <- 0
    nospecies2$reviewed <- FALSE
    nospecies2$source <- "zero_species_checklists"

    #--add NA columns
    tmp <- c(
      "age_sex", "approved", "behavior_code", "breeding_category",
      "breeding_code", "reason", "scientific_name", "species_comments",
      "taxonomic_order", "taxon_concept_id", "exotic_code",
      "subspecies_common_name", "subspecies_scientific_name"
    )
    stopifnot(!tmp %in% colnames(nospecies2))
    nospecies2[, tmp] <- NA

    #--drop columns from nospecies2
    dropcols <- c("checklist_id")
    nospecies2 <- nospecies2[, !(names(nospecies2) %in% dropcols)]
  }


  ##-----* Zero count data-----

  #--read in data
  zerocount <- utils::read.delim(fname_zerocount, sep = "\t", quote = "")

  #checklist_id column is incorrect
  #create correct checklist_ids to join in sampling data frame
  zerocount$checklist_id2 <- zerocount$sampling_event_identifier
  ids <- !zerocount$group_identifier %in% c(NA, "")
  zerocount$checklist_id2[ids] <- zerocount$group_identifier[ids]

  #remove dupes because of loc_type
  tmp <- unique(zerocount[, !colnames(zerocount) %in% "loc_type"])
  ids <- match(tmp$global_unique_identifier, zerocount$global_unique_identifier, nomatch = 0L)
  zerocount <- cbind(tmp, locality_type = zerocount$loc_type[ids])

  #merge with sampling data to fill in missing info
  sampcols <- c(
    "project_names", "project_identifiers", "country", "country_code", "state",
    "protocol_name", "observation_type", "bcr_code", "county", "county_code",
    "effort_area_ha", "iba_code", "locality", "usfws_code", "checklist_id"
  )
  zerocount <- merge(
    x = zerocount,
    y = sampling[, sampcols],
    by.x = "checklist_id2",
    by.y = "checklist_id",
    all.x = TRUE,
    all.y = FALSE
  )

  #--remove any zerocount obs on checklists not in sampling
  na_ids <- is.na(zerocount$observation_type)
  zerocount <- zerocount[!na_ids, , drop = FALSE]


  #--reformat data
  zerocount$time_observations_started <- format(
    as.POSIXlt(zerocount$obs_dt, format = "%m/%d/%Y %H:%M:%S"),
    format = "%H:%M:%S"
  )
  # if obs_dt is at "00:00:00", then this means that time should be NA
  zerocount$time_observations_started[grep("00:00:00", zerocount$obs_dt)] <- NA

  zerocount$observation_date <-
    format(
      as.POSIXlt(zerocount$obs_dt, format = "%m/%d/%Y %H:%M:%S"),
      format = "%Y-%m-%d"
    )
  zerocount$observer_id <- gsub(
    pattern = "USER",
    replacement = "obsr",
    x = zerocount$observer_id
  )
  zerocount$breeding_code <- translate_codes(
    zerocount$breeding_code, "INTERNAL", "PUBLIC", codes
  )
  zerocount$behavior_code <- translate_codes(
    zerocount$behavior_code, "INTERNAL", "PUBLIC", codes
  )
  zerocount$all_species_reported <- as.logical(zerocount$all_species_reported)


  #--cases when how_many_atmost differ from how_many_atleast
  if (FALSE) {
    ids <- zerocount$how_many_atleast != zerocount$how_many_atmost
    table(zerocount$how_many_atmost[ids], zerocount$how_many_atleast[ids])
    table(zerocount$how_many_atleast)
    # --> they are always the same and are 0
  }

  #--change column names
  zerocount <- dplyr::rename(
    zerocount,
    category = taxon_category,
    observation_count = how_many_atleast,
    last_edited_date = last_edited_dt,
    checklist_comments = trip_comments
  )

  #--add defined or calculated columns
  zerocount$duration_minutes <- round(zerocount$duration_hrs * 60)
  zerocount$has_media <- NA
  zerocount <- cbind(zerocount, get_taxonomy(zerocount$common_name, tax = taxonomy))
  zerocount$source <- "zero_count_obs"

  #--add NA columns
  tmp <- c(
    "age_sex", "taxon_concept_id", "exotic_code", "subspecies_common_name",
    "subspecies_scientific_name", "reason", "observer_orcid_id"
  )
  stopifnot(!tmp %in% colnames(zerocount))
  zerocount[, tmp] <- NA

  #--drop columns from zerocount
  dropcols <- c(
    "orig_species_code", "is_birding_hotspot", "how_many_atmost", "last_name",
    "first_name", "duration_hrs", "obs_dt", "obs_time_valid", "proj_period_id",
    "checklist_id", "checklist_id2", "loc_type"
  )
  zerocount <- zerocount[, !(names(zerocount) %in% dropcols)]

  zerocount$observation_count <- as.character(zerocount$observation_count)


  ##-----* External data-----

  #--read in data
  external <- utils::read.csv(fname_external, na.strings = c(""," ","NA"))

  #--add defined or calculated columns
  external$category <- "species"
  external$has_media <- FALSE
  external$country <- "United States"
  external$state <- "New York"
  external$state_code <- "US-NY"
  external$project_names <- "New York Breeding Bird Atlas"
  external$project_identifiers <- "1006"
  external$last_edited_date <- Sys.Date()
  external$protocol_code <- translate_codes(
    external$protocol_name, "protocol_name", "protocol_code", protocols
  )
  external$observation_type <- translate_codes(
    external$protocol_code, "protocol_code", "observation_type", protocols
  )
  external$behavior_code <- external$breeding_code
  external$breeding_category <- translate_codes(
    external$breeding_code_internal, "INTERNAL", "CATEGORY", codes
  )
  external$locality_type <- "P"
  #don't need taxon_concept_id or locality_id filled in

  #--add NA columns
  tmp <- c(
    "taxon_concept_id", "subspecies_common_name", "subspecies_scientific_name",
    "exotic_code", "age_sex", "county", "county_code", "iba_code",
    "bcr_code", "usfws_code", "atlas_block", "locality_id", "observer_id",
    "observer_orcid_id", "group_identifier", "approved", "reviewed",
    "reason"
  )
  stopifnot(!tmp %in% colnames(external))
  external[, tmp] <- NA

  #--drop unneeded columns from external
  dropcols <- c("genus", "species", "breeding_code_internal",
                "date", "start_time", "state_province")
  external <- external[, !(names(external) %in% dropcols)]

  external$observation_count <- as.character(external$observation_count)

  #how much data do we add?
  dim(external)
  # 196132


  ##-----Combine all data-----

  alldat <- rbind(ebd_df, sensitive, hidden, zerocount, external)

  #how much data do we add to the EBD?
  dim(alldat)[1] - dim(ebd_df)[1] # 271252

  rm(ebd_df, sensitive, hidden, zerocount, external)


  ##-----* Cleanup-----
  alldat$group_identifier[!nzchar(alldat$group_identifier)] <- NA

  #convert the text string "NA" to actual NA values in checklist_comments
  istxtna <- alldat$checklist_comments %in% "NA"
  alldat$checklist_comments[istxtna] <- NA


  # remove any obs outside Atlas period
  alldat <- alldat[
    alldat$observation_date >= "2020-01-01" &
    alldat$observation_date <= "2024-12-31",
  ]

  # remove white space from evidence codes
  alldat$breeding_code <- trimws(alldat$breeding_code)
  alldat$behavior_code <- trimws(alldat$behavior_code)

  #remove flyovers and observations without breeding codes
  #NOTE: this removes zero species surveys
  if (!include_all_obs) {
    drop <- c(NA, "", "F", "NC") # 368 of sensitive have no breeding code
    ids <- alldat$breeding_code %in% drop
    alldat <- alldat[!ids, ]
    stopifnot(!anyNA(alldat[, "breeding_code"]))
  }

  #restrict to Atlas portal
  if (!include_outside_portal) {
    ids <- alldat$project_names %in% "New York Breeding Bird Atlas"
    table(alldat$source[!ids])
    table(
      alldat$project_names[alldat$source %in% "sensitive"],
      alldat$breeding_code[alldat$source %in% "sensitive"],
      useNA = "always"
    )
    alldat <- alldat[ids, ]
  }

  #restrict to rare/tracked species
  if (!include_all_spp) {
    ids <- alldat$common_name %in% tracked$common_name
    alldat <- alldat[ids, ]
  }

  #remove any obs without lat/long
  stopifnot(
    !anyNA(alldat$latitude) | !anyNA(alldat$longitude)
  )


  #--- * Code Rock Pigeons as category "domestic" ------
  # data sources "hidden" and "zero_count_obs" use "species" instead of "domestic"
  # -- which may be an indication that these two were run through auk::auk_rollup()
  rp <- which(alldat$common_name %in% "Rock Pigeon")
  alldat$category[rp] <- "domestic"


  #--- * Check if SEI appears in multiple sources ------

  # SEI is expected to be shared in ebd and sensitive or ebd and zero_count_obs
  # but not if it is shared for the same species

  # SEI should not be shared between hidden and any of ebd, sensitive, or zero_count_obs
  msei <- intersect(
    unique(alldat$sampling_event_identifier[alldat$source %in% "hidden"]),
    unique(alldat$sampling_event_identifier[!alldat$source %in% "hidden"])
  )

  #SEI in multiple sources: remove them from hidden
  # 2025-10-08:
  # Remove the hidden version of sei =
  # S106990472, S108900259, S111548230, S111549625, S112946382, S141857664,
  # S159956244, S159957025, S159957498, S159958014, S159957198, S159958513,
  # S159958376, S159958132, S159957863, S159958630, S159958280, S165899108,
  # S166000336, S179002247, S180856032, S181280091, S182332421, S182523895,
  # S182524667, S182715702, S182715876, S182715773, S182716055, S182716235,
  # S182716422, S182716028, S182716919, S182716579, S182717029, S182716112,
  # S182715952, S182717132, S182717081, S182716311, S182716864, S182716897,
  # S182716794, S182966892, S183907561, S183908043, S185499778, S187621755,
  # S187953145, S193483918, S195252080, S195252978, S196279904, S66865687,
  # S68101314, S69466395, S69910705, S69913183, S69913576
  if (length(msei) > 0L) {
    warning("SEI in multiple sources")
    cat("Remove the hidden version of sei = ", toString(msei), fill = TRUE)
    idsRemove <- which(
      alldat$sampling_event_identifier %in% msei & alldat$source %in% "hidden"
    )
    alldat <- alldat[-idsRemove, ]
  }


  #--- * Homogenize start time ------
  #   --> there are sei with multiple start times which looks like an ebird bug
  #   --> one hypothetical explanation is that if an observer with an adjusted
  #       start time on a shared checklists "adds" a species entered by someone
  #       who has a different start time (and this process would not correctly
  #       update the start time)
  tmp <- dplyr::distinct(
    alldat[, c("sampling_event_identifier", "time_observations_started")]
  )

  seiMultipleStarts <- tmp$sampling_event_identifier[duplicated(tmp$sampling_event_identifier)]
  if (FALSE) {
    table(alldat[alldat$sampling_event_identifier %in% "S175652929", c("observer_id", "time_observations_started")])
  }

  # Fix start time to the value that the majority of the checklist entries use
  for (k in seq_along(seiMultipleStarts)) {
    idsk <- which(alldat$sampling_event_identifier %in% seiMultipleStarts[[k]])
    tt <- table(alldat$time_observations_started[idsk])
    tmp <- which(tt == max(tt))
    ntmp <- names(tt)[[tmp[[length(tmp)]]]]
    message(
      "sei = ", seiMultipleStarts[[k]], ": time_observations_started ",
      "was fixed to ", ntmp, ", because it contained multiple values: ",
      paste0(names(tt), " (n = ", tt, ")", collapse = ", ")
    )
    alldat$time_observations_started[idsk] <- ntmp
  }

  # sei = S137198708: time_observations_started was fixed to 08:30:00, because it contained multiple values: 08:30:00 (n = 24), 08:53:00 (n = 10)
  # sei = S147606917: time_observations_started was fixed to 07:26:00, because it contained multiple values: 06:55:00 (n = 1), 07:26:00 (n = 48)
  # sei = S137268779: time_observations_started was fixed to 08:30:00, because it contained multiple values: 08:30:00 (n = 24), 08:53:00 (n = 10)
  # sei = S197930267: time_observations_started was fixed to 11:45:00, because it contained multiple values: 11:30:00 (n = 2), 11:45:00 (n = 31)
  # sei = S204470725: time_observations_started was fixed to 07:57:00, because it contained multiple values: 07:40:00 (n = 7), 07:57:00 (n = 20)
  # sei = S188109747: time_observations_started was fixed to 11:17:00, because it contained multiple values: 09:17:00 (n = 11), 11:17:00 (n = 19)
  # sei = S205786610: time_observations_started was fixed to 12:20:00, because it contained multiple values: 12:20:00 (n = 12), 12:23:00 (n = 6)
  # sei = S175652929: time_observations_started was fixed to 06:40:00, because it contained multiple values: 06:40:00 (n = 91), 06:45:00 (n = 1)
  # sei = S194679361: time_observations_started was fixed to 07:50:00, because it contained multiple values: 07:25:00 (n = 4), 07:50:00 (n = 41)
  # sei = S158730794: time_observations_started was fixed to 14:25:00, because it contained multiple values: 14:25:00 (n = 32), 14:54:00 (n = 1)
  # sei = S171395313: time_observations_started was fixed to 10:20:00, because it contained multiple values: 07:41:00 (n = 1), 10:20:00 (n = 55)
  # sei = S205609397: time_observations_started was fixed to 07:45:00, because it contained multiple values: 06:20:00 (n = 1), 07:45:00 (n = 34)
  # sei = S171035259: time_observations_started was fixed to 09:50:00, because it contained multiple values: 09:07:00 (n = 1), 09:50:00 (n = 46)
  # sei = S161395982: time_observations_started was fixed to 16:10:00, because it contained multiple values: 15:54:00 (n = 1), 16:10:00 (n = 23)
  # sei = S204872985: time_observations_started was fixed to 16:23:00, because it contained multiple values: 16:15:00 (n = 1), 16:23:00 (n = 18)
  # sei = S164904507: time_observations_started was fixed to 09:50:00, because it contained multiple values: 09:00:00 (n = 1), 09:50:00 (n = 45)
  # sei = S76802977: time_observations_started was fixed to 07:29:00, because it contained multiple values: 07:29:00 (n = 24), 07:42:00 (n = 1)
  # sei = S73599309: time_observations_started was fixed to 13:20:00, because it contained multiple values: 13:00:00 (n = 1), 13:20:00 (n = 3)
  # sei = S87920901: time_observations_started was fixed to 05:50:00, because it contained multiple values: 05:50:00 (n = 99), 06:23:00 (n = 6)
  # sei = S96621340: time_observations_started was fixed to 07:45:00, because it contained multiple values: 07:45:00 (n = 49), 07:59:00 (n = 3)
  # sei = S86975862: time_observations_started was fixed to 09:00:00, because it contained multiple values: 05:30:00 (n = 3), 09:00:00 (n = 95)
  # sei = S96344775: time_observations_started was fixed to 07:47:00, because it contained multiple values: 07:10:00 (n = 5), 07:47:00 (n = 39)
  # sei = S87411045: time_observations_started was fixed to 08:24:00, because it contained multiple values: 08:24:00 (n = 62), 10:05:00 (n = 1)
  # sei = S83821213: time_observations_started was fixed to 11:50:00, because it contained multiple values: 10:15:00 (n = 8), 11:50:00 (n = 13)
  # sei = S95783675: time_observations_started was fixed to 17:25:00, because it contained multiple values: 17:25:00 (n = 10), 18:11:00 (n = 1)
  # sei = S83043598: time_observations_started was fixed to 15:45:00, because it contained multiple values: 15:15:00 (n = 1), 15:45:00 (n = 34)
  # sei = S88599747: time_observations_started was fixed to 10:49:00, because it contained multiple values: 10:34:00 (n = 2), 10:49:00 (n = 15)
  # sei = S87926335: time_observations_started was fixed to 05:50:00, because it contained multiple values: 05:50:00 (n = 98), 06:23:00 (n = 9)
  # sei = S151303458: time_observations_started was fixed to 17:25:00, because it contained multiple values: 17:25:00 (n = 10), 18:11:00 (n = 5)
  # sei = S83580560: time_observations_started was fixed to 11:01:00, because it contained multiple values: 10:00:00 (n = 2), 11:01:00 (n = 31)
  # sei = S90769176: time_observations_started was fixed to 08:45:00, because it contained multiple values: 08:45:00 (n = 24), 09:15:00 (n = 1)
  # sei = S84186603: time_observations_started was fixed to 07:38:00, because it contained multiple values: 06:20:00 (n = 1), 07:38:00 (n = 36)
  # sei = S98813952: time_observations_started was fixed to 07:06:00, because it contained multiple values: 07:06:00 (n = 12), 07:15:00 (n = 1)
  # sei = S93512085: time_observations_started was fixed to 12:57:00, because it contained multiple values: 12:57:00 (n = 17), 13:11:00 (n = 2)
  # sei = S88053913: time_observations_started was fixed to 05:47:00, because it contained multiple values: 05:45:00 (n = 2), 05:47:00 (n = 74)
  # sei = S98546550: time_observations_started was fixed to 07:52:00, because it contained multiple values: 07:30:00 (n = 1), 07:52:00 (n = 26)
  # sei = S82415921: time_observations_started was fixed to 10:37:00, because it contained multiple values: 09:37:00 (n = 1), 10:37:00 (n = 22)
  # sei = S95352569: time_observations_started was fixed to 08:05:00, because it contained multiple values: 08:05:00 (n = 50), 09:30:00 (n = 1)
  # sei = S110276056: time_observations_started was fixed to 05:36:00, because it contained multiple values: 05:36:00 (n = 71), 06:00:00 (n = 3)
  # sei = S122074325: time_observations_started was fixed to 06:27:00, because it contained multiple values: 06:27:00 (n = 36), 06:35:00 (n = 2)
  # sei = S122080144: time_observations_started was fixed to 06:35:00, because it contained multiple values: 06:27:00 (n = 2), 06:35:00 (n = 36)
  # sei = S122316328: time_observations_started was fixed to 13:54:00, because it contained multiple values: 13:50:00 (n = 3), 13:54:00 (n = 26)
  # sei = S122073711: time_observations_started was fixed to 06:35:00, because it contained multiple values: 06:27:00 (n = 4), 06:35:00 (n = 34)
  # sei = S122385674: time_observations_started was fixed to 06:35:00, because it contained multiple values: 06:27:00 (n = 1), 06:35:00 (n = 33)
  # sei = S120401953: time_observations_started was fixed to 06:51:00, because it contained multiple values: 06:51:00 (n = 69), 07:50:00 (n = 1)
  # sei = S104252986: time_observations_started was fixed to 09:20:00, because it contained multiple values: 06:29:00 (n = 5), 09:20:00 (n = 44)
  # sei = S110230909: time_observations_started was fixed to 09:15:00, because it contained multiple values: 07:35:00 (n = 1), 09:15:00 (n = 47)
  # sei = S106124100: time_observations_started was fixed to 15:14:00, because it contained multiple values: 15:05:00 (n = 2), 15:14:00 (n = 2)
  # sei = S118361906: time_observations_started was fixed to 22:05:00, because it contained multiple values: 21:13:00 (n = 1), 22:05:00 (n = 36)
  # sei = S122360369: time_observations_started was fixed to 13:54:00, because it contained multiple values: 13:50:00 (n = 3), 13:54:00 (n = 26)
  # sei = S122073710: time_observations_started was fixed to 06:35:00, because it contained multiple values: 06:27:00 (n = 1), 06:35:00 (n = 33)
  # sei = S115471086: time_observations_started was fixed to 07:17:00, because it contained multiple values: 06:33:00 (n = 1), 07:17:00 (n = 39)
  # sei = S131028457: time_observations_started was fixed to 09:30:00, because it contained multiple values: 08:51:00 (n = 2), 09:30:00 (n = 16)
  # sei = S78372517: time_observations_started was fixed to 09:26:00, because it contained multiple values: 08:16:00 (n = 1), 09:26:00 (n = 10)
  # sei = S119628397: time_observations_started was fixed to 07:30:00, because it contained multiple values: 07:22:00 (n = 2), 07:30:00 (n = 35)
  # sei = S137698487: time_observations_started was fixed to 08:23:00, because it contained multiple values: 07:49:00 (n = 1), 08:23:00 (n = 45)
  # sei = S139651441: time_observations_started was fixed to 07:40:00, because it contained multiple values: 07:35:00 (n = 1), 07:40:00 (n = 61)
  # sei = S143172523: time_observations_started was fixed to 10:20:00, because it contained multiple values: 10:20:00 (n = 16), 10:22:00 (n = 2)
  # sei = S146146712: time_observations_started was fixed to 05:35:00, because it contained multiple values: 05:20:00 (n = 1), 05:35:00 (n = 43)
  # sei = S129643370: time_observations_started was fixed to 15:33:00, because it contained multiple values: 15:33:00 (n = 14), 15:55:00 (n = 1)

  ##-----Add checklist_id-----
  # auk defines checklist_id as sampling_event_identifier unless there is a group_identifier
  alldat$checklist_id <- alldat$sampling_event_identifier
  ids <- !alldat$group_identifier %in% c(NA, "")
  alldat$checklist_id[ids] <- alldat$group_identifier[ids]

  stopifnot(!anyNA(alldat$checklist_id))


  #----- Rollup ------
  ids <- alldat$common_name %in% c(
    hybrids_to_retain, domestic_to_retain, slashes_to_retain
  )

  stopifnot(
    !anyDuplicated(alldat[ids, c("sampling_event_identifier", "scientific_name")])
  )

  print(nrow(alldat))

  # rollup does two things (but here we only want number 1):
  #   1) combine observations of sub-species into observations at the species level
  #   2) combine observations on shared checklists
  alldat <- dplyr::bind_rows(
    nybba_rollup(
      x = alldat[!ids, ],
      taxonomy = taxonomy,
      combineSharedChecklists = FALSE,
      breedingCodes = codes
    ),
    alldat[ids, ]
  ) |>
    as.data.frame()

  print(nrow(alldat))
  # n (pre-rollup) = 35491937
  # n (post-rollup) = 35183850
  # n (old partial rollup) = 30368043

  stopifnot(
    !anyDuplicated(alldat[, c("sampling_event_identifier", "scientific_name")])
  )


  ##-----Atlas block: spatially look up missing ------

  # quick check that all atlas_block in the data exist in our blocks file
  stopifnot(alldat$atlas_block %in% c(NA, blocks$atlas_block))

  idsNoBlock <- which(is.na(alldat$atlas_block))

  #add geometry to data file
  nb_sf <- sf::st_as_sf(
    alldat[idsNoBlock, ], coords = c("longitude", "latitude"), crs = 4326
  )

  #intersect geometry with blocks
  tmpBlockID <- rep(NA_integer_, length(idsNoBlock))
  resIntersects <- sf::st_intersects(nb_sf, blocks)
  lri <- lengths(resIntersects)
  table(lri %in% 0L:1L)
  ids <- which(lri == 1L)
  tmpBlockID[ids] <- unlist(resIntersects[ids])
  ids2 <- which(lri > 1L)
  if (length(ids2) > 0L) {
    warning("Checklists (n = ", length(ids2), ") intersect more than one block!")
  }
  tmpBlockID[ids2] <- unlist(lapply(ids2, function(k) resIntersects[[k]][[1L]]))

  alldat[idsNoBlock, "atlas_block"] <- blocks[tmpBlockID, "atlas_block", drop = TRUE]

  #--check for still missing values
  if (anyNA(alldat$atlas_block)) {
    idsNABlock <- which(is.na(alldat$atlas_block))
    message(length(idsNABlock), " entries have no assigned block.")

    # map checklists without assigned atlas block
    if (FALSE) {
      nb_sf <- sf::st_as_sf(
        alldat[idsNABlock, ], coords = c("longitude", "latitude"), crs = 4326
      )
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = blocks[, 0]) +
        ggplot2::geom_sf(data = nb_sf[, 0])
    }
  }
  #39279 entries have no assigned block


  ##-----Atlas block: look up additional info -----
  #lookup block name, county, region, priority status
  var_blocks <- c(
    block_name = "block_name",
    block_county = "county",
    block_region = "region",
    priority_status = "priority_status"
  )
  stopifnot(!names(var_blocks) %in% colnames(alldat))

  ids <- match(alldat$atlas_block, blocks$atlas_block, nomatch = 0L)

  alldat[ids > 0L, names(var_blocks)] <- blocks[ids, var_blocks, drop = TRUE]

  # #check that Cornwall-on-Hudson blocks have hypenated names
  if (FALSE) {
    unique(alldat[grep("Cornwall", alldat$block_name), c("atlas_block", "block_name")])
  }


  ##-----Add atlaser info-----

  if (do_atlaser_perspective) {
    ##-----* Split shared checklist by observer------
    alldat[["number_ebirders"]] <- lengths(
      strsplit(alldat$observer_id, split = ",")
    )

    #Checklists with exactly 1 observer
    stopifnot(!anyNA(alldat$number_ebirders))
    tmp_udat1 <- alldat[alldat$number_ebirders == 1, , drop = FALSE]

    # Expand collapsed shared checklists
    idsm <- which(alldat$number_ebirders > 1)
    ids_exp <- rep(idsm, times = alldat$number_ebirders[idsm])
    #complete checklists with multiple observers
    tmp_udatm <- alldat[ids_exp, , drop = FALSE]

    tmp_udatm[, "number_ebirders"] <- 1
    tmp_udatm[, "observer_id"] <- unlist(
      strsplit(alldat$observer_id[idsm], split = ",")
    )
    tmp_udatm[, "sampling_event_identifier"] <- unlist(
      strsplit(alldat$sampling_event_identifier[idsm], split = ",")
    )

    #Combined checklists by observer
    alldat <- rbind(tmp_udat1, tmp_udatm)


    ##-----* Prepare atlaser info-----
    #note this does not work for multiple observers
    atlasers$observer_id <- gsub(
      pattern = "USER",
      replacement = "obsr",
      x = atlasers$user_id
    )
    atlasers$name <- paste(atlasers$first_name, atlasers$last_name)

    # Remove empty email and "Deleted User"
    ids <- c(
      grep("Deleted User", atlasers$name),
      which(nchar(atlasers$email) == 0L)
    ) |> unique()

    if (length(ids) > 0L) {
      atlasers <- atlasers[-ids, , drop = FALSE]
    }

    # Replace empty names with login_name
    ids <- atlasers$name %in% " "
    if (any(ids)) {
      atlasers$name[ids] <- atlasers$login_name[ids]
    }

    # Create unique atlaser id
    atlasers[["atlaser_id"]] <- atlasers[["observer_id"]]

    # Combine observer_id as one atlaser if name and email are the same
    tmp <- atlasers[, c("name", "email"), drop = FALSE]
    dups <- which(duplicated(tmp))

    for (k in dups) {
      tmpa <- tmp[k, ]
      ids <- which(
        atlasers[["name"]] %in% tmpa[["name"]] &
          atlasers[["email"]] %in% tmpa[["email"]]
      )
      atlasers[ids, "atlaser_id"] <- paste(
        atlasers[ids, "atlaser_id"], collapse = "&"
      )
    }


    ##-----* Add atlaser info-----
    varAtlaser <- c("atlaser_id", "name", "email")
    indices <- match(
      alldat[["observer_id"]], table = atlasers[["observer_id"]], nomatch = 0L
    )

    alldat[indices > 0L, varAtlaser] <- atlasers[indices, varAtlaser]
  }


  ##-----Save RDS object-----


  #save RDS object
  saveRDS(alldat, file = fname_ebirdny_data)
}

#dim(alldat) #35,183,871

