#------ Process moving checklists ------
#
# Purpose: get a list of moving checklists that pass checklist-level review
#   * to identify which checklist IDs should be in the portal and used for Atlas analysis
#   * by using checklist bounding boxes to overlay checklist area with atlas block boundaries
#
# Data: bounding box data provided by Tom Auer & Matt Strimas-Mackey at eBird
#
# for all moving checklists (in and outside atlas portal)
#   * record if there is a track available (only about 60% have)
#   * confirm track lies within US-NY
#   * determine how much of the track bbox is within the assigned block
#   * check that block assignment matches bbox overlap
#
# criteria to move a checklist into portal && checks for atlas checklists
#   * checklist location within US-NY
#   * track sufficiently within one atlas block
#   * checklist location falls within same atlas block as track
#
# Script written by Julie A. Hart with help from Daniel R. Schlaepfer
# last edited 8/20/2026



#-------------------Settings-------------------
# use the s2 package for spherical geometry
sf::sf_use_s2(TRUE)

# Tolerance for acceptable rounding errors
tol <- sqrt(.Machine[["double.eps"]])

# Distance to buffer state boundary to determine if within NY
bufferDistanceNY <- units::set_units(50, "m")

# Distance to buffer atlas blocks to determine if within single block
bufferDistanceBlocks <- units::set_units(100, "m")

# Distance (km) limit for checklists without tracks
maxDistanceInPortal <- 10 # limit used by eBird for occupancy modeling

# Area (ha) limit for checklists without tracks
maxAreaInPortal <- 2275.975 / 2 # half of smallest block area

if (FALSE) {
  units::set_units(fivenum(sf::st_area(blocks)), "mile^2")
  # Units: [mile^2]
  # [1]  8.787587 9.002315 9.113774 9.205465 9.456338
  units::set_units(units::set_units(8.787587, "mile^2"), "ha")
  # 2275.975 [ha] area of smallest block
}



#-------------------Load functions-------------------

#calculate the amount of overlap (%) of track bbox with assigned block
overlapArea <- function(track, block) {
  res <- suppressWarnings(
    sf::st_intersection(track, block) |>
      sf::st_area()
  )
  if (length(res) == 1L) res else units::set_units(0, "m2")
}



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

#--- * get checklist data ------

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
stopifnot(length(fname_sampling) == 1L)

#read in data
sampling_all <- readRDS(fname_sampling)

#subset to only data with checklist_type = moving
sampling_mov <- sampling_all[sampling_all$checklist_type %in% "moving", ]

sampling_mov_sf <- sf::st_as_sf(
  sampling_mov,
  coords = c("longitude", "latitude"),
  remove = FALSE,
  crs = 4326
)



#--- * get atlas blocks ------

# need to use the gpkg b/c it has the block code (shapefile does not)
# add a 100 m buffer since we can't do a -100 buffer on tracks b/c that
# results in Nulls
blocks <- sf::st_read(
  file.path(
    dir_spatial,
    "atlas_blocks",
    "spatial_atlas_blocks",
    "results",
    "block-vector_with_attributes.gpkg"
  )
) |>
  sf::st_transform(crs = 4326) |>
  sf::st_make_valid()


# user terra to buffer because buffering via s2 does not create smooth buffers
blocks_buff <- terra::buffer(
  terra::vect(blocks), width = bufferDistanceBlocks
) |>
  sf::st_as_sf()

if (FALSE) {
  blocks_buff1 <- sf::st_buffer(blocks, dist = bufferDistanceBlocks)

  id <- nrow(blocks)
  plot(blocks[nrow(blocks), 0], reset = FALSE)
  plot(blocks_buff[id, 0], border = "blue", add = TRUE)
  plot(blocks_buff1[id, 0], border = "orange", add = TRUE)
}



#--- * get NY boundary ------

ny_boundary <- sf::st_read(
  file.path(
    dir_spatial,
    "Boundaries",
    "NYS_Civil_Boundaries_state.gpkg"
  )
) |>
  sf::st_transform(crs = 4326) |>
  sf::st_make_valid()

# user terra to buffer because buffering via s2 does not create smooth buffers
ny_boundary_buff <- terra::buffer(
  terra::vect(ny_boundary), width = bufferDistanceNY
) |>
  sf::st_as_sf()



#-------------------Prepare track data-------------------

fname_trax_bbox <- file.path(dir_out, "trax_bbox.rds")

if (file.exists(fname_trax_bbox)) {
  trax_bbox <- readRDS(fname_trax_bbox)

} else {

  trax <- utils::read.csv(
    file.path(
      dir_ebird, "eBird_tracks", "ebird-tracks_ny-atlas.csv"
    )
  ) |>
    dplyr::rename(sampling_event_identifier = checklist_id)

  # append checklist data to the trax
  # add protocol type, add block assignment, project code
  # and subset trax data to traveling checklists from 2020-2024
  checklistcols <- c(
    "sampling_event_identifier", "protocol_name", "project_names", "atlas_block"
  )
  trax2 <- merge(
    x = trax,
    y = sampling_mov[, checklistcols],
    by.x = "sampling_event_identifier",
    by.y = "sampling_event_identifier"
  )

  #---* create bounding boxes-----

  # define bbox coordinates and make polygons
  lst <- lapply(
    1:nrow(trax2),
    function(x) {
      res <- matrix(
        c(
          trax2[x, 'track_lon_min'], trax2[x, 'track_lat_min'], # bottom left
          trax2[x, 'track_lon_min'], trax2[x, 'track_lat_max'], # top left
          trax2[x, 'track_lon_max'], trax2[x, 'track_lat_max'], # top right
          trax2[x, 'track_lon_max'], trax2[x, 'track_lat_min'], # bottom right
          trax2[x, 'track_lon_min'], trax2[x, 'track_lat_min']  # bottom left
        ),
        ncol = 2,
        byrow = TRUE
      )
      sf::st_polygon(list(res))
    }
  )

  # create simple features collection
  trax_bbox <- sf::st_sf(
    trax2,
    sf::st_sfc(lst),
    crs = 4326
  ) |>
    sf::st_make_valid()

  sf::st_geometry(trax_bbox) <- "geom"

  # calculate area of each track bbox polygon
  trax_bbox[, "area"] <- sf::st_area(trax_bbox)

  # remove tracks without area
  # (assume that something went wrong and treat checklist as if no track)
  idsRemove <- which(abs(units::drop_units(trax_bbox$area)) < tol)
  print(length(idsRemove)) # 598

  trax_bbox <- trax_bbox[-idsRemove, , drop = FALSE]

  saveRDS(trax_bbox, fname_trax_bbox)
}


#---* create track centroids -----

# Note: it appears that the centroid coordinates represent the centroid
# location of the track (and not the bounding box centroid or the midpoint
# of the track)
fname_trax_ctr <- file.path(dir_out, "trax_centroids.rds")

if (file.exists(fname_trax_ctr)) {
  trax_ctr <- readRDS(fname_trax_ctr)

} else {
  trax_ctr <- sf::st_as_sf(
    as.data.frame(trax_bbox),
    coords =  c("centroid_lon", "centroid_lat"),
    crs = sf::st_crs(trax_bbox)
  )

  saveRDS(trax_ctr, fname_trax_ctr)
}

#How many tracks are we talking about?
if (FALSE) {
  table(sampling_mov$observation_type)
  # Area    Banding   eBird Pelagic Protocol    Historical     Traveling
  # 2727        300                     4388         1205        1430575

  #how many moving checklists have a track? %
  checklistIDs <- unique(sampling_mov$sampling_event_identifier)
  traxIDs <- unique(trax_bbox$sampling_event_identifier)
  mean(checklistIDs %in% traxIDs) #57%

  #how many moving checklists in atlas portal have a track? %
  trav_atlas_IDs <- unique(sampling_mov$sampling_event_identifier[
    sampling_mov$project_names %in% "New York Breeding Bird Atlas"
  ])
  mean(trav_atlas_IDs %in% traxIDs) #67%
}



#---* add track info to results-----

# trackArea (numeric)
#not sure if this is necessary but we calculate it so might as well keep it
ids_tr2r <- match(
  sampling_mov$sampling_event_identifier,
  trax_bbox$sampling_event_identifier,
  nomatch = 0
)

sampling_mov$trackArea_m2 <- NA
sampling_mov$trackArea_m2[ids_tr2r > 0] <-
  units::drop_units(trax_bbox$area[ids_tr2r])

# hasTrack (T/F)
sampling_mov$hasTrack <-
  sampling_mov$trackArea_m2 > 0 &
  !is.na(sampling_mov$trackArea_m2)



#-------------------Manually update records-------------------

#confirmed with observer that this checklist was in a single block
#only RUGR for the county
sampling_mov$project_names[sampling_mov$sampling_event_identifier %in% "S64860994"] <- "New York Breeding Bird Atlas"



#-------------------Check within NY-------------------

# Is checklist location within (buffered) NY
sampling_mov$withinNY <- lengths(
  sf::st_intersects(sampling_mov_sf, ny_boundary_buff)
) > 0L


#--- * Check in NY if has track ------
# If has track, then checklist location and bounding box must intersect NY
ids <- sf::st_intersects(trax_bbox, ny_boundary_buff)
isBboxInNY <- lengths(ids) > 0L
sampling_mov$withinNY[ids_tr2r > 0L] <-
  sampling_mov$withinNY[ids_tr2r > 0L] & isBboxInNY[ids_tr2r]

if (FALSE) {
  ids <- sf::st_intersects(trax_ctr, ny_boundary_buff)
  isCentroidInNY <- lengths(ids) > 0L

  table(isCentroidInNY, isBboxInNY)

  idsDisagree <- isBboxInNY & !isCentroidInNY

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = trax_ctr[idsDisagree, 0]) +
    ggplot2::geom_sf(data = trax_bbox[idsDisagree, 0], fill = NA) +
    ggplot2::geom_sf(data = ny_boundary[, 0], fill = NA, color = "gray") +
    ggplot2::coord_sf() +
    ggplot2::theme_bw()
}


stopifnot(!anyNA(sampling_mov$withinNY))


if (FALSE) {
  table(sampling_mov$withinNY, useNA = "always")
  # FALSE       TRUE    <NA>
  #  5620    1433575       0

  ids <- !sampling_mov$withinNY

  #in which portal are these out-of-state checklists?
  table(sampling_mov$project_names[ids], useNA = "always")
  # Canada KBA Bird Monitoring  Maryland-DC Breeding Bird Atlas  New York Breeding Bird Atlas
  #                         14                                1                           415
  #    <NA>
  #    5190

  #what protocol are these out-of-state checklists?
  table(sampling_mov$observation_type[ids])
  # eBird Pelagic Protocol              Traveling
  #                   4062                   1558
}


#--- * Ignore tracks if too far away from checklist location ------

#--- Calculate distance between track centroid vs. checklist location
dlc <- sf::st_distance(
  sampling_mov_sf[ids_tr2r > 0L, 0], trax_ctr[ids_tr2r, 0], by_element = TRUE
)

# dlc > effort distance
idsFarAway <- which(
  dlc > units::set_units(sampling_mov_sf$effort_distance_km[ids_tr2r > 0L], "km") &
    dlc > units::set_units(maxDistanceInPortal, "km")
)
summary(dlc[idsFarAway])

# update hasTrack
sampling_mov$hasTrack[ids_tr2r > 0L][idsFarAway] <- FALSE


if (FALSE) {
  hist(dlc)

  tmp <- dlc[idsFarAway] > units::set_units(1e3, "km") &
    dlc[idsFarAway] < units::set_units(5e3, "km")
  idsFarAwayM <- idsFarAway[tmp]
  print(sampling_mov_sf$sampling_event_identifier[ids_tr2r > 0L][idsFarAwayM])
  # [1] "S68952652"  "S86434021"  "S104551144" "S106543345" "S126882771" "S104047104" "S122285371" "S121002681" "S142072076"
  # [10] "S150366503" "S129249041" "S145194427"


  idsFarAway2 <- idsFarAway[order(dlc[idsFarAway], decreasing = TRUE)]

  head(sampling_mov_sf$sampling_event_identifier[ids_tr2r > 0L][idsFarAway2])
  #[1] "S76385755" "S76385066" "S76385096" "S76383944" "S77685576" "S77679589"

  # Plotting
  world_sf <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

  # Extract coordinates and combine for arcs
  c1 <- sf::st_coordinates(sampling_mov_sf[ids_tr2r > 0L, 0L])
  c2 <- sf::st_coordinates(trax_ctr[ids_tr2r, 0L])
  arc_data <- data.frame(
    x = c1[, 1L],
    y = c1[, 2L],
    xend = c2[, 1L],
    yend = c2[, 2L],
    dist = units::drop_units(dlc)
  )

  tmp <- sf::st_bbox(world_sf)
  tmp <- sf::st_bbox(ny_boundary)
  lims <- list(xlim = tmp[c("xmin", "xmax")], ylim = tmp[c("ymin", "ymax")])

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = sampling_mov_sf[ids_tr2r > 0L, 0L],
      pch = 3,
      color = "orange"
    ) +
    ggplot2::geom_segment(
      data = arc_data,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, color = dist)
    ) +
    ggplot2::geom_sf(data = world_sf[, 0], fill = NA) +
    ggplot2::geom_sf(data = ny_boundary[, 0], fill = NA, color = "gray") +
    ggplot2::coord_sf(
      xlim = lims[["xlim"]],
      ylim = lims[["ylim"]],
      expand = TRUE
    ) +
    ggplot2::theme_bw()

}



#-------------------Check within blocks-------------------


if (FALSE) {
  # NYBBA checklists with excessive area do not have many codes
  #   --> ok to discard (even if these were due to gps errors)
  ids <- which(
    units::drop_units(units::set_units(trax_bbox$area, "km2")) > 1000 &
      trax_bbox$project_names %in% "New York Breeding Bird Atlas"
  )
  sei_large <- trax_bbox$sampling_event_identifier[ids]
  tv <-
    c("sampling_event_identifier", "project_names", "observation_type",
    grep("count", colnames(sampling_mov), value = TRUE)
    )
  sampling_mov[sampling_mov$sampling_event_identifier %in% sei_large, tv] |>
    as.data.frame()
}


#---* intersect with atlas blocks-----

# calculate the amount of overlap (%) of track bbox with assigned buffered block
fname_trackOverlapWithBlock <- file.path(dir_out, "trackOverlapWithBlock.rds")

if (file.exists(fname_trackOverlapWithBlock)) {
  trackOverlapWithBlock <- readRDS(fname_trackOverlapWithBlock)

} else {

  cl <- parallel::makeCluster(9)
  doParallel::registerDoParallel(cl)

  library("foreach")

  trackOverlapWithBlock <- foreach(
    k = seq_len(nrow(trax_bbox)),
    .combine = "rbind",
    .inorder = TRUE
  ) %dopar% {
    res <- data.frame(
      sampling_event_identifier = trax_ctr$sampling_event_identifier[[k]],
      isTrackWithinBlock = NA,
      overlapWithBlock = NA_real_,
      blockWithTrackCentroid = NA_character_
    )

    # approach
    #   1. locate (unbuffered) block where track centroid is located
    idBlock <- which(lengths(sf::st_intersects(blocks, trax_ctr[k, ])) > 0L)

    if (length(idBlock) == 1L) {
      res[["blockWithTrackCentroid"]] <-
        blocks[idBlock, "atlas_block", drop = TRUE]

      #   2. calculate overlap of that (buffered) block with track
      oa <- overlapArea(trax_bbox[k, 0], block = blocks_buff[idBlock, 0])

      res[["overlapWithBlock"]] <- 100 * units::drop_units(
        oa / trax_bbox[k, "area", drop = TRUE]
      )

      #   3. then determine if completely within and if so assign block to track
      res[["isTrackWithinBlock"]] <- abs(res[["overlapWithBlock"]] - 100) < tol
    }

    res
  }

  parallel::stopCluster(cl)

  trackOverlapWithBlock[["isAssignedBlockCorrect"]] <-
    !is.na(trackOverlapWithBlock[["blockWithTrackCentroid"]]) &
    trackOverlapWithBlock[["blockWithTrackCentroid"]] == trax_bbox[["atlas_block"]]

  if (FALSE) {
    k <- 2; idBlock <- 4253 # within
    k <- 345472; idBlock <- 4316  # across multiple blocks
    k <- 227690; idBlock <- 1084
    plot(blocks[idBlock, 0], reset = FALSE)
    plot(trax_ctr[k, 0], col = "red", add = TRUE)
    plot(trax_bbox[k, 0], border = "purple", add = TRUE)
  }

  saveRDS(
    trackOverlapWithBlock,
    file = fname_trackOverlapWithBlock
  )
}


multipleBuffers <- FALSE
if (multipleBuffers) {
  mBufferDistancesBlocks <- c(1, 1.25, 1.5, 2, 2.5, 3) * bufferDistanceBlocks

  blocks_mBuffs <- lapply(
    mBufferDistancesBlocks,
    function(d) {
      terra::buffer(
        terra::vect(blocks), width = d
      ) |>
        sf::st_as_sf()
    }
  )

  fname_trackOverlapWithBlock2 <- file.path(dir_out, "trackOverlapWithBlock2.rds")

  if (file.exists(fname_trackOverlapWithBlock2)) {
    trackOverlapWithBlock2 <- readRDS(fname_trackOverlapWithBlock2)

  } else {

    cl <- parallel::makeCluster(9)
    doParallel::registerDoParallel(cl)

    library("foreach")

    tmp <- data.frame(
      isTrackWithinBlockBuffer = NA,
      overlapWithBlockBuffer = NA_real_
    )
    template <- lapply(mBufferDistancesBlocks, function(d) tmp) |>
      do.call(cbind, args = _)
    colnames(template) <- paste0(
      rep(colnames(tmp), times = length(mBufferDistancesBlocks)),
      rep(seq_along(mBufferDistancesBlocks), each = ncol(tmp))
    )


    trackOverlapWithBlock2 <- foreach(
      k = seq_len(nrow(trax_bbox)),
      .combine = "rbind",
      .inorder = TRUE
    ) %dopar% {
      res <- data.frame(
        sampling_event_identifier = trax_ctr$sampling_event_identifier[[k]],
        blockWithTrackCentroid = NA_character_,
        template
      )

      # approach
      #   1. locate (unbuffered) block where track centroid is located
      idBlock <- which(lengths(sf::st_intersects(blocks, trax_ctr[k, ])) > 0L)

      if (length(idBlock) == 1L) {
        res[["blockWithTrackCentroid"]] <-
          blocks[idBlock, "atlas_block", drop = TRUE]

        for (kb in seq_along(mBufferDistancesBlocks)) {
          #   2. calculate overlap of that (buffered) block with track
          oa <- overlapArea(trax_bbox[k, 0], block = blocks_mBuffs[[kb]][idBlock, 0])

          varo <- paste0("overlapWithBlockBuffer", kb)
          res[[varo]] <- 100 * units::drop_units(
            oa / trax_bbox[k, "area", drop = TRUE]
          )

          #   3. then determine if completely within and if so assign block to track
          res[[paste0("isTrackWithinBlockBuffer", kb)]] <- abs(res[[varo]] - 100) < tol
        }
      }

      res
    }

    parallel::stopCluster(cl)

    trackOverlapWithBlock2[["isAssignedBlockCorrect"]] <-
      !is.na(trackOverlapWithBlock2[["blockWithTrackCentroid"]]) &
      trackOverlapWithBlock2[["blockWithTrackCentroid"]] == trax_bbox[["atlas_block"]]

    saveRDS(
      trackOverlapWithBlock2,
      file = fname_trackOverlapWithBlock2
    )
  }

}



#-----* explore overlap-----

#overlap using single buffer of 100 m
if (FALSE) {

  hist(
    trackOverlapWithBlock$overlapWithBlock,
    xlim = c(0, 100),
    ylim = c(0, 2000),
    breaks = 1000
  )
  tail(table(trackOverlapWithBlock$overlapWithBlock))

  #look at distribution of percent overlap
  tabulate(trackOverlapWithBlock$overlapWithBlock, nbins = 100)

  #what percentage of tracks are 100% in a block?
  tabulate(
    trackOverlapWithBlock$overlapWithBlock,
    nbins = 100
  )[100]/nrow(trackOverlapWithBlock)
  #0.7364912
  #bumps up to 84% if include >= 99%

  #at what level do we capture 90%?
  mean(
    trackOverlapWithBlock$overlapWithBlock > 83 |
    is.na(trackOverlapWithBlock$overlapWithBlock)
  )
  #0.899923

  # number of tracks with given proportion of area within assigned block
  sum(trackOverlapWithBlock$overlapWithBlock < 15, na.rm = TRUE)   #   2325
  sum(trackOverlapWithBlock$overlapWithBlock < 50, na.rm = TRUE)   #  17592
  sum(trackOverlapWithBlock$overlapWithBlock > 50, na.rm = TRUE)   # 806932
  sum(trackOverlapWithBlock$overlapWithBlock > 75, na.rm = TRUE)   # 762501
  sum(trackOverlapWithBlock$overlapWithBlock > 80, na.rm = TRUE)   # 749946
  sum(trackOverlapWithBlock$overlapWithBlock > 85, na.rm = TRUE)   # 736109
  sum(trackOverlapWithBlock$overlapWithBlock > 90, na.rm = TRUE)   # 721301
  sum(trackOverlapWithBlock$overlapWithBlock > 95, na.rm = TRUE)   # 707847
  sum(trackOverlapWithBlock$overlapWithBlock >= 100, na.rm = TRUE) # 610876

}

#inspect overlap using different block buffers
if (FALSE) {
  #histograms
  hist(
    trackOverlapWithBlock2$overlapWithBlockBuffer1,
    xlim = c(80,100),
    ylim = c(0, 100000),
    breaks = 100
  )
  hist(
    trackOverlapWithBlock2$overlapWithBlockBuffer2,
    xlim = c(80,100), ylim = c(0, 100000), breaks = 100)
  hist(
    trackOverlapWithBlock2$overlapWithBlockBuffer6,
    xlim = c(80,100), ylim = c(0, 5000), breaks = 100)

  #cumulative distributions
  ids <- trackOverlapWithBlock2$overlapWithBlockBuffer1 < 100
  plot(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer1[ids]), col = "black")
  lines(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer2[ids]), col = "blue")
  lines(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer3[ids]), col = "green")
  lines(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer4[ids]), col = "yellow")
  lines(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer5[ids]), col = "orange")
  lines(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer6[ids]), col = "red")
  abline(h = 0.5)

  #tabulated bins
  plot(
    tabulate(trackOverlapWithBlock2$overlapWithBlockBuffer1)[80:100],
    xlim = c(80,100),
    ylim = c(0, 5000),
    main = "Number records with percent overlap using different block buffer distances",
    sub = "How many tracks overlap with block by this amount",
    xlab = "Percent overlap of track with block",
    ylab = "Frequency"
  )
  lines(
    80:100,
    tabulate(trackOverlapWithBlock2$overlapWithBlockBuffer1)[80:100],
    type = "l",
    col = "black"
  )
  lines(
    80:100,
    tabulate(trackOverlapWithBlock2$overlapWithBlockBuffer2)[80:100],
    type = "l",
    col = "blue"
  )
  lines(
    80:100,
    tabulate(trackOverlapWithBlock2$overlapWithBlockBuffer3)[80:100],
    type = "l",
    col = "green"
  )
  lines(
    80:100,
    tabulate(trackOverlapWithBlock2$overlapWithBlockBuffer4)[80:100],
    type = "l",
    col = "yellow"
  )
  lines(
    80:100,
    tabulate(trackOverlapWithBlock2$overlapWithBlockBuffer5)[80:100],
    type = "l",
    col = "orange"
  )
  lines(
    80:100,
    tabulate(trackOverlapWithBlock2$overlapWithBlockBuffer6)[80:100],
    type = "l",
    col = "red"
  )
  legend(
    "bottom",
    legend = c(100, 125, 150, 200, 250, 300),
    title = "Buffer distance (m)",
    fill = c("black", "blue", "green", "yellow", "orange", "red")
  )

  #look at in portal data only
  #add portal info to trackOverlap df
  ids <- match(
    trackOverlapWithBlock2$sampling_event_identifier,
    sampling_mov$sampling_event_identifier,
    nomatch = 0L
  )
  trackOverlapWithBlock2$project_names[ids > 0L] <- sampling_mov$project_names[ids]
  #now plot atlas checklists only
  ids <-
    trackOverlapWithBlock2$overlapWithBlockBuffer1 < 100 &
    trackOverlapWithBlock2$project_names %in% "New York Breeding Bird Atlas"
  plot(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer1[ids]), col = "black")
  lines(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer2[ids]), col = "blue")
  lines(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer3[ids]), col = "green")
  lines(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer4[ids]), col = "yellow")
  lines(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer5[ids]), col = "orange")
  lines(ecdf(trackOverlapWithBlock2$overlapWithBlockBuffer6[ids]), col = "red")
  abline(h = 0.5)
  legend(
    "topleft",
    legend = c(100, 125, 150, 200, 250, 300),
    title = "Buffer distance (m)",
    fill = c("black", "blue", "green", "yellow", "orange", "red")
  )

  #CONCLUSION: data do not indicate any threshold is better than another
  #atlaser behavior is consistent
  #need to base decision on what we are comfortable with
}



#---* add block overlap info to sampling----
# ignore track information where we set hasTrack to FALSE
# because we ignore tracks that are "too far away" from checklist locations

#add overlap to sampling
sampling_mov$isTrackWithinBlock <- NA
sampling_mov$isTrackWithinBlock[ids_tr2r > 0] <-
  trackOverlapWithBlock$isTrackWithinBlock[ids_tr2r]

sampling_mov$isTrackWithinBlock[!sampling_mov$hasTrack] <- NA

#add if block is correct
sampling_mov$isAssignedBlockCorrect <- NA
sampling_mov$isAssignedBlockCorrect[ids_tr2r > 0] <-
  trackOverlapWithBlock$isAssignedBlockCorrect[ids_tr2r]

sampling_mov$isAssignedBlockCorrect[!sampling_mov$hasTrack] <- NA

#add block ID containing track centroid
sampling_mov$blockWithTrackCentroid <- NA
sampling_mov$blockWithTrackCentroid[ids_tr2r > 0] <-
  trackOverlapWithBlock$blockWithTrackCentroid[ids_tr2r]

sampling_mov$blockWithTrackCentroid[!sampling_mov$hasTrack] <- NA

#add percent of block overlap
sampling_mov$overlapWithBlock <- NA
sampling_mov$overlapWithBlock[ids_tr2r > 0] <-
  trackOverlapWithBlock$overlapWithBlock[ids_tr2r]

sampling_mov$overlapWithBlock[!sampling_mov$hasTrack] <- NA




#---* explore block assignment-----

if (FALSE) {
  #
  table(iT = sampling_mov$isTrackWithinBlock, hT = sampling_mov$hasTrack, useNA = "always")
  #         hT
  # iT        FALSE     TRUE   <NA>
  #   FALSE       0   128738      0
  #    TRUE       0   691775      0
  #    <NA>  613926     4756      0
  # 4756 = track centroids that are outside any atlas block

  #how many trax within block?
  table(sampling_mov$isTrackWithinBlock, useNA = "always")
  #  FALSE   TRUE   <NA>
  # 128738 691775 618682

  #how many have correct block assignment?
  table(sampling_mov$isAssignedBlockCorrect, useNA = "always")
  #  FALSE   TRUE   <NA>
  #  73089 752008 614098

  #how many are within block and have correct block assignment?
  table(sampling_mov$isAssignedBlockCorrect, sampling_mov$isTrackWithinBlock)
  #        FALSE   TRUE <-- within block
  # FALSE  45873  22460
  # TRUE   82697 669311
  #  ^ correct assignment

  #what is the source of these block issues?
  table(sampling_mov$isAssignedBlockCorrect, sampling_mov$source)
  #         alldat_sampling     ebird_sampling
  # FALSE               345              72744
  # TRUE               3346             748662

  table(sampling_mov$isTrackWithinBlock, sampling_mov$source)
  #           alldat_sampling    ebird_sampling
  # FALSE                 632            128106
  # TRUE                 3059            688716

  #are most of these in the atlas portal?
  table(sampling_mov$isAssignedBlockCorrect, sampling_mov$project_names)
  #           New York Breeding Bird Atlas
  # FALSE                   15039
  # TRUE                   252111

  table(sampling_mov$isTrackWithinBlock, sampling_mov$project_names)
  #           New York Breeding Bird Atlas
  # FALSE                  26468
  # TRUE                  240290

  #how many checklists are completely within their assigned block
  sum(
    sampling_mov$isAssignedBlockCorrect &
    sampling_mov$isTrackWithinBlock,
    na.rm = T
  )
  #669311

  #how many checklists are within a block but the block is different than
  #that assigned?
  sum(
    sampling_mov$isTrackWithinBlock &
    !sampling_mov$isAssignedBlockCorrect,
    na.rm = T
  )
  #22460

  #what is the source of checklists that are within a different block?
  table(
    sampling_mov$source[
      sampling_mov$isTrackWithinBlock &
      !sampling_mov$isAssignedBlockCorrect
    ]
  )
  # alldat_sampling  ebird_sampling
  #             174           22286

  #what is the portal of checklists that are within a different block?
  table(
    sampling_mov$project_names[
      sampling_mov$isTrackWithinBlock &
      !sampling_mov$isAssignedBlockCorrect
    ]
  )
  # Canada KBA Bird Monitoring
  # 1
  # Maryland-DC Breeding Bird Atlas
  # 10
  # New York Breeding Bird Atlas
  # 5679
  # New York Breeding Bird Atlas|International Shorebird Survey (ISS)
  # 1
  # North Carolina Bird Atlas
  # 11
  # remaining 17701 were out of any portal

}


# look at Julie's and Daniel's tracks
if (FALSE) {

  isMoveOurs <- sampling_mov$observer_id %in% c("obsr96857", "obsr538427")
  idsTrackOurs <- which(
    trax_ctr$sampling_event_identifier %in%
     sampling_mov$sampling_event_identifier[isMoveOurs]
  )

  hist(trackOverlapWithBlock$overlapWithBlock[idsTrackOurs])
  tmp <- hist(
    sampling_mov$overlapWithBlock[
      isMoveOurs &
      sampling_mov$project_names %in% "New York Breeding Bird Atlas"
    ]
  )
  print(tmp)

  ii <- !is.na(sampling_mov$overlapWithBlock) &
    sampling_mov$overlapWithBlock < 99 &
    sampling_mov$overlapWithBlock >= 90 &
    isMoveOurs &
    sampling_mov$project_names %in% "New York Breeding Bird Atlas"
  sampling_mov$sampling_event_identifier[ii]
  # "S67480361"  "S67480362"  "S91196209"  "S91196212"  "S105724454"
  # "S105886982" "S123436068" "S123436067" "S118198626" "S118157827"

  # all off-season with no codes
}


# further inspect atlas checklists
if (FALSE) {

  #how many checklists in portal have a track?
  sum(
    sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
    sampling_mov$hasTrack
  )
  #267228

  #how many checklists in portal have track going outside block?
  sum(
    sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
    sampling_mov$hasTrack &
    !sampling_mov$isTrackWithinBlock,
    na.rm = T
  )
  #26468 out of 267228 total checklists in portal = 10%

  #how many checklists in portal are contained in block?
  sum(
    sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
    sampling_mov$hasTrack &
    sampling_mov$isTrackWithinBlock,
    na.rm = T
  )
  #240290 out of 267227 total checklists in portal = 90%

  table(sampling_mov$hasTrack, sampling_mov$isTrackWithinBlock, useNA = "always")
  #        FALSE   TRUE   <NA>
  # FALSE      0      0 613926
  # TRUE  128738 691775   4756
  # <NA>       0      0      0

  #how many checklists in portal have 100% overlap with block?
  sum(
    sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
    sampling_mov$overlapWithBlock >= 100,
    na.rm = T
  )
  #211226 out of 267228 total checklists in portal = 79%

  #how many checklists in portal have 99% overlap with block?
  sum(
    sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
      sampling_mov$overlapWithBlock >= 99,
    na.rm = T
  )
  #241103 out of 267228 total checklists in portal = 90%

  #how many checklists in portal have 95% overlap with block?
  sum(
    sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
    sampling_mov$overlapWithBlock >= 95,
    na.rm = T
  )
  #243490 out of 267228 total checklists in portal = 91%

  #how many checklists in portal have 80% overlap with block?
  sum(
    sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
    sampling_mov$overlapWithBlock >= 80,
    na.rm = T
  )
  #252802 out of 267228 total checklists in portal = 95%

  #how does the number of overlapping checklists vary by year?
  sampling_mov$year <- format(sampling_mov$observation_date, "%Y")
  table(
      sampling_mov$year[
        !sampling_mov$isTrackWithinBlock &
        sampling_mov$project_names %in% "New York Breeding Bird Atlas"
      ]
  )
  # 2020 2021 2022 2023 2024
  # 7216 8413 5520 4784  535
  #seems the issue is not primarily because of no blocks in app first year

  #what is distribution of overlap for atlas checklists <99% in block?
  hist(
    sampling_mov$overlapWithBlock[
      sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
      sampling_mov$overlapWithBlock < 99
    ]
  )

  tabulate(
    sampling_mov$overlapWithBlock[
      sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
      sampling_mov$overlapWithBlock
    ],
    nbins = 100
  )
  #seems like most checklists overlap at least 99%

  #how many codes would be dropped if we lowered overlap threshold to 95%?
  table(
    sampling_mov$countCodedSpecies[
      sampling_mov$overlapWithBlock < 95 &
        sampling_mov$project_names %in% "New York Breeding Bird Atlas"
    ]
  )

  #how many attempted codes would be dropped if we lowered overlap threshold
  #to 95%?
  table(
    sampling_mov$countAttemptedSpecies[
      sampling_mov$overlapWithBlock < 95 &
      sampling_mov$project_names %in% "New York Breeding Bird Atlas"
    ]
  )
}



#---* udpate block IDs-----

#Add column = updatedBlockID
#get original atlas block by default
sampling_mov$updatedBlockID <- sampling_mov$atlas_block

#update blockID only if has a track and that track is found to be in a
#different block

updateIDs <-
  sampling_mov$hasTrack &
  !is.na(sampling_mov$hasTrack) &
  !sampling_mov$isAssignedBlockCorrect &
  !is.na(sampling_mov$isAssignedBlockCorrect)

sampling_mov$updatedBlockID[updateIDs] <-
  sampling_mov$blockWithTrackCentroid[updateIDs]

# NOTE: there are cases where there was a blockID, but it was incorrectly
# assigned and we can't identify new block (likely because track centroid is
# outside NY), which leads to <NA> in updatedBlockID column


#inspect checklists without block assignment
if (FALSE) {
  #how many checklists still don't have a block ID?
  sum(is.na(sampling_mov$updatedBlockID))
  #6353

  #is that the same number of checklists without a block assignment?
  sum(is.na(sampling_mov$atlas_block))
  #6068
  #no, discrepancy of 285 checklists

  dim(sampling_mov[
    is.na(sampling_mov$updatedBlockID) &
    !is.na(sampling_mov$atlas_block), ]
  ) |> as.data.frame()
  #285

  head(
    sampling_mov[
      is.na(sampling_mov$updatedBlockID) &
      !is.na(sampling_mov$atlas_block),
    ]
  ) |> as.data.frame()
  #looks like these are tracks where they don't overlap with blocks

  head(
    sampling_mov[
      is.na(sampling_mov$updatedBlockID) &
      !is.na(sampling_mov$atlas_block) &
      sampling_mov$withinNY,
    ]
  ) |> as.data.frame()
  #looks like they are mostly coastal
}



#-------------------Distance Checks-------------------


#--* check for NAs in distance----
table(sampling_mov$observation_type[is.na(sampling_mov$effort_distance_km)])
# Area    Banding Historical  Traveling
# 2276        279         84        996

#no distance is ok for all except traveling counts
sum(
  sampling_mov$effort_distance_km < tol &
    sampling_mov$observation_type %in% "Traveling",
  na.rm = TRUE
)
# 2639

#how many move less than 30 m (threshold for stationary counts)
sum(
  sampling_mov$effort_distance_km < 0.03 &
    sampling_mov$observation_type %in% "Traveling",
  na.rm = TRUE
)
# 4770


#--* flag traveling checklists missing distance----

sampling_mov$distance_flag <- FALSE

ids2 <- is.na(sampling_mov$effort_distance_km) &
  sampling_mov$observation_type %in% "Traveling"
sampling_mov$distance_flag[ids2] <- TRUE



#--* determine maxDistanceInPortal----

# for checklists without a track, limit distance for checklists in portal
# if out of portal and no track, don't use

# determine max distance using distribution of checklists inside portal with
# tracks that are contained within a block
if (FALSE) {
  ids <-
    !is.na(sampling_mov$effort_distance_km) &
    sampling_mov$hasTrack &
    sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
    sampling_mov$isTrackWithinBlock &
    !is.na(sampling_mov$isTrackWithinBlock)

  #plot results
  plot(
    ecdf(sampling_mov$effort_distance_km[ids]),
    main = "Frequency of atlas checklist distances where track contained in block",
    col = "red",
    xlim = c(0, 20)
  )
  abline(v = 10) #current limit for data inside portal

  #compare to checklists outside portal with tracks
  ids2 <-
    !is.na(sampling_mov$effort_distance_km) &
    sampling_mov$hasTrack &
    !sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
    sampling_mov$isTrackWithinBlock &
    !is.na(sampling_mov$isTrackWithinBlock)
  lines(ecdf(sampling_mov$effort_distance_km[ids2]), col = "blue")

  #CONCLUSION: very similar curves
  #10 km threshold used by eBird seems to capture 99% of checklists
  #use 10 km
}



#--* flag atlas checklists with large distance and no track----

#--in Atlas portal
#can be longer if in portal b/c more likely they stayed in a block if they knew
#enough about the portal to use it in the first place
#use 10 km recommended by eBird best practices
# 7.07 km = roughly from one corner to opposite corner of a block
# 4.8 km = roughly side of a block
ids3 <-
  !is.na(sampling_mov$effort_distance_km) &
  !sampling_mov$hasTrack &
  sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
  (sampling_mov$effort_distance_km >= maxDistanceInPortal)

sampling_mov$distance_flag[ids3] <- TRUE


#--outside Atlas portal
#don't use anything outside portal if no track
#accounted for in section "combine flags"



#-------------------Area Checks-------------------

#--* check for NAs in area----

if (FALSE) {
  table(sampling_mov$observation_type[is.na(sampling_mov$effort_area_ha)])
  # Area  Banding  eBird Pelagic Protocol  Historical  Traveling
  # 10         19                    4388         641    1430575

  #ok to have NAs for all except Area

  #where are these 10 records from?
  table(
    sampling_mov$source[
      is.na(sampling_mov$effort_area_ha) &
        sampling_mov$observation_type %in% "Area"
    ]
  )
  # ebird_sampling
  # 10
}


#--* flag Area checklists missing area----

sampling_mov$area_flag <- FALSE

ids5 <- is.na(sampling_mov$effort_area_ha) &
  sampling_mov$observation_type %in% "Area"

sampling_mov$area_flag[ids5] <- TRUE


#--* check for large areas relative to atlas block size----

#an atlas block is roughly 9 sq miles

if (FALSE) {
  max(
    sampling_mov$effort_area_ha[!sampling_mov$hasTrack], na.rm = TRUE
  )
  # largest area for a checklist is 40468.57 hectares = 156.25 sq miles!!

  sampling_mov[
    sampling_mov$effort_area_ha > 40000 &
      !is.na(sampling_mov$effort_area_ha),
  ] |> as.data.frame()
  #species from a 90 mile hike

  hist(sampling_mov$effort_area_ha, xlim = c(0, 50000), breaks = 10000)
  tabulate(sampling_mov$effort_area_ha, nbins = 20)

  sum(sampling_mov$effort_area_ha <= 258.999 & !sampling_mov$hasTrack, na.rm = T) #2914 (< 1 sq mile)
  sum(sampling_mov$effort_area_ha > 258.999 & !sampling_mov$hasTrack, na.rm = T)  #33 (> 1 sq mile)
  sum(sampling_mov$effort_area_ha > 517.998 & !sampling_mov$hasTrack, na.rm = T)  #8 (> 2 sq miles)
  sum(sampling_mov$effort_area_ha > 776.996 & !sampling_mov$hasTrack, na.rm = T)  #7 (3 sq miles)
  sum(sampling_mov$effort_area_ha > 1036.00 & !sampling_mov$hasTrack, na.rm = T)  #6 (4 sq miles)
  sum(sampling_mov$effort_area_ha > 1294.99 & !sampling_mov$hasTrack, na.rm = T)  #5 (5 sq miles)
  sum(sampling_mov$effort_area_ha > 1553.99 & !sampling_mov$hasTrack, na.rm = T)  #5 (6 sq miles)
  sum(sampling_mov$effort_area_ha > 1812.99 & !sampling_mov$hasTrack, na.rm = T)  #5 (7 sq miles)
  sum(sampling_mov$effort_area_ha > 2071.99 & !sampling_mov$hasTrack, na.rm = T)  #2 (8 sq miles)
  sum(sampling_mov$effort_area_ha > 2330.99 & !sampling_mov$hasTrack, na.rm = T)  #2 (9 sq miles)
}



#--* determine maxAreaInPortal----

#an atlas block is roughly 9 sq miles or 2330.99 ha

if (FALSE) {
  ids <-
    !is.na(sampling_mov$effort_area_ha) &
    sampling_mov$hasTrack &
    sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
    sampling_mov$isTrackWithinBlock &
    !is.na(sampling_mov$isTrackWithinBlock)

  #plot results
  plot(
    ecdf(sampling_mov$effort_area_ha[ids]),
    main = "Frequency of atlas checklist areas where track contained in block",
    col = "red"
  )
  abline(v = 2275.975) #current limit for data inside portal

  #compare to checklists outside portal with tracks
  ids2 <-
    !is.na(sampling_mov$effort_area_ha) &
    sampling_mov$hasTrack &
    !sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
    sampling_mov$isTrackWithinBlock &
    !is.na(sampling_mov$isTrackWithinBlock)
  lines(ecdf(sampling_mov$effort_area_ha[ids2]), col = "blue")

  #not very helpful b/c not enough data

  #distribution of area for all checklists
  fivenum(sampling_mov$effort_area_ha)
  # 0.0202     1.2141     3.2375    22.2577   40468.5660

  #distribution of area for checklists within a block
  fivenum(sampling_mov$effort_area_ha[sampling_mov$isTrackWithinBlock])
  # 0.2023  1.2141  3.0351  7.2843 32.7795

  #compared to average block size
  fivenum(units::set_units(sf::st_area(blocks$geom), "ha"))
  # 2275.975 2331.589 2360.457 2384.205 2449.180

  #areas of atlas checklists without a track
  sort(unique(sampling_mov$effort_area_ha[
    sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
    !sampling_mov$hasTrack
  ]))

  #if use half of smallest block size, only lose 1 checklist from portal
  #that one checklist has an area of 2330.9900 or about the size of a block
  #next largest area is 768.9030

  #CONCLUSION: use half of smallest block area

}


#--* flag checklists with large area----

#an atlas block is roughly 9 sq miles (2330.99 ha)
#smallest block size is 2275.975 ha

#--in Atlas portal
#add area_flag for anything over half the size of a block
ids6 <-
  !is.na(sampling_mov$effort_area_ha) &
  !sampling_mov$hasTrack &
  sampling_mov$project_names %in% "New York Breeding Bird Atlas" &
  sampling_mov$effort_area_ha >= maxAreaInPortal

sampling_mov$area_flag[ids6] <- TRUE


#--outside Atlas portal
#don't use anything outside portal if no track
#accounted for in section "combine flags"



#-------------------Distance & Area Checks-------------------

#are there moving checklists missing both distance and area?
if (FALSE) {
  table(
    sampling_mov$observation_type[
      is.na(sampling_mov$effort_distance_km) &
        is.na(sampling_mov$effort_area_ha)
    ]
  )
  # Area     Traveling
  # 10         996

  #what portal are they?
  table(
    sampling_mov$project_names[
      is.na(sampling_mov$effort_distance_km) &
        is.na(sampling_mov$effort_area_ha)
    ],
    useNA = "always"
  )
  # New York Breeding Bird Atlas      PROALAS—General           <NA>
  #                           98                    4            904

  #what is data source?
  table(sampling_mov$source[
    is.na(sampling_mov$effort_distance_km) &
    is.na(sampling_mov$effort_area_ha)
  ])
  # ebird_sampling
  # 1006

  # Need to flag these records in following sections
}


#---* flag checklists with no distance or area----

ids8 <-
  is.na(sampling_mov$effort_distance_km) &
  is.na(sampling_mov$effort_area_ha)
sampling_mov$distance_flag[ids8] <- TRUE
sampling_mov$area_flag[ids8] <- TRUE


#what percent of checklists have distance or area flags?
if (FALSE) {
  sum(sampling_mov$distance_flag, na.rm = TRUE) #2206 out of 1439195 = 0.15%
  sum(sampling_mov$area_flag, na.rm = TRUE) #1007 out of 1439195 = 0.07%
}



#---------------------------Combine flags---------------------------

#set checklist_flag to TRUE if any other flags
sampling_mov$checklist_flag <- NA

#get ids for different flags
hasTrack <- sampling_mov$hasTrack
outsideNY <- !sampling_mov$withinNY #shouldn't be NAs here!!
distance_flag <- sampling_mov$distance_flag
area_flag <- sampling_mov$area_flag
atlas_checklist <- sampling_mov$project_names %in% "New York Breeding Bird Atlas"
block_flag <-
  is.na(sampling_mov$isTrackWithinBlock) |
  !sampling_mov$isTrackWithinBlock

#set flag for all checklists with a track
#flag if has a track and any issues with NY, distance, area, or block
tmp <- hasTrack &
  (outsideNY | distance_flag | area_flag | block_flag)

#set flag for atlas checklists without a track
#flag atlas checklists without track if location outsideNY, distance issue,
#or area issue
tmp2 <-
  !hasTrack & atlas_checklist &
  (outsideNY | distance_flag | area_flag)

#set flag for non-atlas checklists without a track
#flag all non-atlas checklists without a track
tmp3 <- !hasTrack & !atlas_checklist

#set flag for checklists outside of atlas blocks
tmp4 <- is.na(sampling_mov$updatedBlockID)

sampling_mov$checklist_flag <- tmp | tmp2 | tmp3 | tmp4

#add exception for S64860994
#reviewed by Kyle Bardwell, only record for RUGR in Putnam County
stopifnot(sampling_mov$checklist_flag[sampling_mov$sampling_event_identifier %in% "S64860994"] == FALSE)

#check results
if (FALSE) {
  #how many pass vs fail?
  table(sampling_mov$checklist_flag, useNA = "always")
  #  FALSE   TRUE   <NA>
  # 820347 618848      0

  # 1439195 total checklists
  # 820347 / 1439195 * 100 = 57% pass
  # 618848 / 1439195 * 100 = 43% fail

  table(sampling_mov$checklist_flag, sampling_mov$project_names, useNA = "always")

  # How many does tmp3 flag that are not flagged by other criteria?
  sum(tmp3[!(tmp | tmp2 | tmp4)]) # 482550
  # 482550 / 618848 * 100 = 78% of failed checklists are flagged because
  # not in portal and no track

  #do all the passing checklists have an updated block ID?
  sum(is.na(sampling_mov$updatedBlockID) & !sampling_mov$checklist_flag) #0

  isBBA <- sampling_mov$project_names %in% "New York Breeding Bird Atlas"
  table(
    r1 = tmp,
    r2 = tmp2,
    r3 = tmp3,
    r4 = tmp4,
    isBBA = isBBA,
    useNA = "ifany"
  ) |>
    as.data.frame()
  #       r1    r2    r3    r4 isBBA   Freq
  # 1  FALSE FALSE FALSE FALSE FALSE 451409
  # 2   TRUE FALSE FALSE FALSE FALSE 102252
  # 3  FALSE  TRUE FALSE FALSE FALSE      0
  # 4   TRUE  TRUE FALSE FALSE FALSE      0
  # 5  FALSE FALSE  TRUE FALSE FALSE 482550
  # 6   TRUE FALSE  TRUE FALSE FALSE      0
  # 7  FALSE  TRUE  TRUE FALSE FALSE      0
  # 8   TRUE  TRUE  TRUE FALSE FALSE      0
  # 9  FALSE FALSE FALSE  TRUE FALSE      3
  # 10  TRUE FALSE FALSE  TRUE FALSE   4377
  # 11 FALSE  TRUE FALSE  TRUE FALSE      0
  # 12  TRUE  TRUE FALSE  TRUE FALSE      0
  # 13 FALSE FALSE  TRUE  TRUE FALSE   1402
  # 14  TRUE FALSE  TRUE  TRUE FALSE      0
  # 15 FALSE  TRUE  TRUE  TRUE FALSE      0
  # 16  TRUE  TRUE  TRUE  TRUE FALSE      0
  # 17 FALSE FALSE FALSE FALSE  TRUE 368938
  # 18  TRUE FALSE FALSE FALSE  TRUE  26402
  # 19 FALSE  TRUE FALSE FALSE  TRUE   1291
  # 20  TRUE  TRUE FALSE FALSE  TRUE      0
  # 21 FALSE FALSE  TRUE FALSE  TRUE      0
  # 22  TRUE FALSE  TRUE FALSE  TRUE      0
  # 23 FALSE  TRUE  TRUE FALSE  TRUE      0
  # 24  TRUE  TRUE  TRUE FALSE  TRUE      0
  # 25 FALSE FALSE FALSE  TRUE  TRUE      9
  # 26  TRUE FALSE FALSE  TRUE  TRUE    548
  # 27 FALSE  TRUE FALSE  TRUE  TRUE     14
  # 28  TRUE  TRUE FALSE  TRUE  TRUE      0
  # 29 FALSE FALSE  TRUE  TRUE  TRUE      0
  # 30  TRUE FALSE  TRUE  TRUE  TRUE      0
  # 31 FALSE  TRUE  TRUE  TRUE  TRUE      0
  # 32  TRUE  TRUE  TRUE  TRUE  TRUE      0

  it <- hasTrack & isBBA
  table(
    outsideNY = outsideNY[it],
    distance_flag = distance_flag[it],
    area_flag = area_flag[it],
    block_flag = block_flag[it],
    useNA = "ifany"
  ) |>
    as.data.frame()
  #    outsideNY distance_flag area_flag block_flag   Freq
  # 1      FALSE         FALSE     FALSE      FALSE 240278
  # 2       TRUE         FALSE     FALSE      FALSE      0
  # 3      FALSE          TRUE     FALSE      FALSE      0
  # 4       TRUE          TRUE     FALSE      FALSE      0
  # 5      FALSE         FALSE      TRUE      FALSE      0
  # 6       TRUE         FALSE      TRUE      FALSE      0
  # 7      FALSE          TRUE      TRUE      FALSE     12
  # 8       TRUE          TRUE      TRUE      FALSE      0
  # 9      FALSE         FALSE     FALSE       TRUE  26549
  # 10      TRUE         FALSE     FALSE       TRUE    384
  # 11     FALSE          TRUE     FALSE       TRUE      0
  # 12      TRUE          TRUE     FALSE       TRUE      0
  # 13     FALSE         FALSE      TRUE       TRUE      0
  # 14      TRUE         FALSE      TRUE       TRUE      0
  # 15     FALSE          TRUE      TRUE       TRUE      5
  # 16      TRUE          TRUE      TRUE       TRUE      0
}



#-----Save output-----

# RETURN: sampling_mov df with additional columns for:

# trackArea_m2 (numeric)
# hasTrack (T/F)
# withinNY (T/F; >95%)
# isTrackWithinBlock (T/F)
# isAssignedBlockCorrect (T/F)
# blockWithTrackCentroid (alphanumeric)
# overlapWithBlock (numeric; percent of block overlap)
# updatedBlockID (alphanumeric)
# distance_flag
# area_flag (T/F)
# checklist_flag (T/F)


saveRDS(
  sampling_mov,
  file.path(
    dir_out,
    paste0("moving_checklists_reviewed_", Sys.Date(), ".rds")
  )
)

