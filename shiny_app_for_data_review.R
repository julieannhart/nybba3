#Purpose: to produce a shiny app like this for data review:
#https://mddcbba3.shinyapps.io/atlas-review-tool/


# Global----

# R packages required for this shiny app
suppressPackageStartupMessages({
  require("shiny", quietly = TRUE)
  require("shinyjs", quietly = TRUE)
  require("shinybusy", quietly = TRUE)
  require("leaflet", quietly = TRUE)
  require("DT", quietly = TRUE)
  require("htmltools", quietly = TRUE)
  require("dplyr", quietly = TRUE)
  require("readr", quietly = TRUE)
  require("sf", quietly = TRUE)
  require("htmlwidgets", quietly = TRUE)
  require("memoise", quietly = TRUE)
  require("cachem", quietly = TRUE)
})


## Load species list from file without full dataset----

species_list_full <- readRDS("data/bba3_species_list.rds")

species_list <- species_list_full |>
  dplyr::filter(block_county == "All Regions") |>
  dplyr::pull(common_name) |>
  sort()



## Load expected dates----

dates <- read.csv("data/expected_dates_2026-06-01.csv")



## Load atlas blocks----

atlas_blocks <- sf::st_read("data/block-vector.gpkg", layer = "block-vector")



## Load county boundaries----

tmp_county <- "data/NYS_Civil_Boundaries_county_shoreline.gpkg"

layer_name <- "Counties_Shoreline"
sql_query <- paste0("SELECT NAME, CALC_SQ_MI, SHAPE FROM \"", layer_name, "\"")

counties <- sf::st_read(dsn = tmp_county, query = sql_query) |>
  sf::st_transform(4326)



## Define variables----

county_list <- c(
  "All Regions",
  "Albany",
  "Allegany",
  "Bronx",
  "Broome",
  "Cattaraugus",
  "Cayuga",
  "Chautauqua",
  "Chemung",
  "Chenango",
  "Clinton",
  "Columbia",
  "Cortland",
  "Delaware",
  "Dutchess",
  "Erie",
  "Essex",
  "Franklin",
  "Fulton",
  "Genesee",
  "Greene",
  "Hamilton",
  "Herkimer",
  "Jefferson",
  "Kings",
  "Lewis",
  "Livingston",
  "Madison",
  "Monroe",
  "Montgomery",
  "Nassau",
  "New York",
  "Niagara",
  "Oneida",
  "Onondaga",
  "Ontario",
  "Orange",
  "Orleans",
  "Oswego",
  "Otsego",
  "Putnam",
  "Queens",
  "Rensselaer",
  "Richmond",
  "Rockland",
  "St Lawrence",
  "Saratoga",
  "Schenectady",
  "Schoharie",
  "Schuyler",
  "Seneca",
  "Steuben",
  "Suffolk",
  "Sullivan",
  "Tioga",
  "Tompkins",
  "Ulster",
  "Warren",
  "Washington",
  "Wayne",
  "Westchester",
  "Wyoming",
  "Yates"
)


season_list <- c(
  "All Seasons" = "All Seasons",
  "Early" = "early season",
  "Pre-breeding" = "prebreeding",
  "Core breeding" = "core breeding",
  "Post-breeding" = "postbreeding",
  "Late" = "late season"
)


br_codes <- c(
  "NC (No code)" = "NC",
  "F (Flyover)" = "F",
  "H (In suitable habitat)" = "H",
  "S (Singing bird)" = "S",
  "S7 (Singing bird present 7+ days)" = "S7",
  "M (Multiple (7+) singing birds)" = "M",
  "P (Pair in suitable habitat)" = "P",
  "T (Territorial defense)" = "T",
  "C (Courtship, display, or copulation)" = "C",
  "N (Visiting probable nest site)" = "N",
  "A (Agitated behavior)" = "A",
  "B (Woodpecker/wren nest building)" = "B",
  "PE (Physiological evidence)" = "PE",
  "CN (Carrying nesting material)" = "CN",
  "NB (Nest building)" = "NB",
  "DD (Distraction display)" = "DD",
  "UN (Used nest)" = "UN",
  "ON (Occupied nest)" = "ON",
  "FL (Recently fledged young)" = "FL",
  "CF (Carrying food)" = "CF",
  "FY (Feeding young)" = "FY",
  "FS (Carrying fecal sac)" = "FS",
  "NE (Nest with eggs)" = "NE",
  "NY (Nest with young)" = "NY"
)


br_reasons <- c(
  "bcawaysite - Displays breeding behavior away from breeding site" = "bcawaysite",
  "bcdataerr - Apparent data entry error" = "bcdataerr",
  "bcinadqt - Insufficient documentation" = "bcinadqt",
  "bcmedia - Changed because of photo/audio/video" = "bcmedia",
  "bcnotes - Comments suggest upgrade or downgrade" = "bcnotes",
  "bcranging - Species known to range far from breeding location" = "bcranging",
  "cfnotapp - Carrying food not necessarily for young" = "cfnotapp",
  "codenotapp - Code not suitable for species" = "codenotapp",
  "ddnotapp - Consider A or T instead of DD" = "ddnotapp",
  "expctbrdr - Expected breeder at this time of year" = "expctbrdr",
  "highercat - Code in higher category more suitable" = "highercat",
  "lowercat - Code in lower category more suitable" = "lowercat",
  "nbnotapp - Nest building not confirmation of breeding" = "nbnotapp",
  "notlikely - Not likely to breed in this region" = "notlikely",
  "notlocal - Fledglings may not have hatched locally" = "notlocal",
  "pnotapp - Pair code is not suitable" = "pnotapp",
  "samecat - Different code within category more suitable" = "samecat",
  "tooearly - Too early in year to safely assume breeding" = "tooearly",
  "toolate - Too late in year to safely assume breeding" = "toolate",
  "vocnotapp - Vocalizations not indicative of breeding" = "vocnotapp"
)



# UI----

ui <- shiny::fluidPage(

  shinyjs::useShinyjs(),

  tags$head(
    # CSS styles
    tags$style(
      htmltools::HTML(
        "
        body {
          padding-bottom: 80px; /* Adds whitespace at the very bottom of the app */
        }
        .action-button {
          margin-bottom: 20px; /* Adds space below the action button */
        }
        .checkbox {
          margin-top: 10px; /* Adds space above the checkbox input */
        }
        #review_table_filter {
          margin-bottom: 0px;
          padding-bottom: 0px;
        }
        #review_table_filter label {
          margin-bottom: 0px;
        }
        .dataTables_wrapper .dataTables_filter {
          margin-bottom: 0px !important;
          padding-bottom: 0px !important;
        }
        .dataTables_wrapper .dataTables_filter label {
          margin-bottom: 0px !important;
          padding-bottom: 0px !important;
          line-height: 1 !important;
        }
        "
      )
    ),
    # JS handlers
    tags$script(
      htmltools::HTML("
        // --- SILENT UPDATE (Used after Saving a Review) ---
        Shiny.addCustomMessageHandler('clear_checkboxes_only', function(message) {
          // Only uncheck the boxes so the user can see the new data through the old filters
          $('.obs_checkbox').prop('checked', false);
          $('#select_all_obs').prop('checked', false);
          $('#select_all_text').text('Select All');
        });

        // --- SCROLL TO REVIEW PANEL ---
        Shiny.addCustomMessageHandler('scroll_to_review', function(message) {
          var panel = document.getElementById('review_panel');
          if (panel) {
            panel.scrollIntoView({ behavior: 'smooth', block: 'start' });
          }
        });
      ")
    )
  ),

  shiny::titlePanel("Atlas Observation Review Tool"),

  shiny::uiOutput("NumObs"),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # write reviewer's name
      shiny::textInput("reviewer_name", "Your Name:"),
      # select species
      shiny::selectInput(
        inputId = "species_select",
        label ="Select Species:",
        choices = c("-- Select a species --" = "", species_list),
        selected = "",
        multiple = FALSE
      ),
      # select county filter
      shiny::selectInput(
        inputId = "county_select",
        label = "Filter by County:",
        choices = county_list,
        selected = character(0),
        multiple = TRUE,
        selectize = TRUE
      ),
      # select season filter
      shiny::selectInput(
        inputId = "season_select",
        label = "Filter by Season:",
        choices = season_list,
        selected = character(0),
        multiple = TRUE,
        selectize = TRUE
      ),
      # show only records that haven't been externally reviewed
      shiny::checkboxInput(
        "unreviewed_only",
        "Hide obs that have been externally reviewed",
        value = FALSE
      ),
      # show only records with breeding codes
      shiny::checkboxInput(
        inputId = "coded_only",
        label = "Show only obs with breeding codes",
        value = FALSE
      ),
      # show all flagged records
      shiny::checkboxInput(
        inputId =  "flagged_only",
        label = "Show only flagged obs",
        value = FALSE
      ),
      # show only records that were changed by Julie
      shiny::checkboxInput(
        inputId = "changed_only",
        label = "Show only obs that Julie changed",
        value = FALSE
      ),

      # show atlas blocks
      shiny::checkboxInput(
        inputId = "show_blocks",
        label = "Display atlas blocks",
        value = FALSE
      ),
      # show breeding range map
      shiny::checkboxInput(
        inputId = "show_range",
        label = "Display breeding range for selected species",
        value = FALSE
      ),

      # load the data onto the map
      shiny::actionButton(
        inputId = "load_data",
        label = htmltools::HTML("<b>Load Observations</b>")
      ),

      # remove filters
      shiny::actionButton(
        inputId = "reset_filters",
        label = "Reset Filters"
      ),

      # show breeding season cutoffs
      shiny::uiOutput(outputId = "species_details_ui"),

      width = 2

  ),

    shiny::mainPanel(
      # view tabs for a map and table of the observations
      shiny::tabsetPanel(
        id = "main_tabs",
        shiny::tabPanel(
          title = "Map View",
          leaflet::leafletOutput("map", height = 600)
        ),

        shiny::tabPanel(
          title = "Table View",
          tags$div(
            style = "margin-bottom: 0px;",
            shiny::checkboxInput("select_all_obs",
              tags$span(
                "Select all records on current page",
                style = "color: #8B008B; font-weight: bold; font-size: 15px;"
              ),
             value = FALSE
            )
          ),
          DT::DTOutput("review_table")
        )
      ),

      # show busy spinner
      shinybusy::add_busy_spinner(
        spin = "fading-circle",
        position = "full-page"
      ),

      # show review fields below map
      shiny::fluidRow(
        shiny::column(
          width = 6,  # 1-12, where 12 = full width
          shiny::wellPanel(
            id = "review_panel",
            style = "max-width: 600px;",
            shiny::uiOutput(outputId = "review_ui")
          )
        )
      )
    )
  )
)




# Server----

fetch_review_log <- function() {

  path <- "data/review_log.csv"

  if (!file.exists(path)) {
    return(
      data.frame(
        obs_id = character(),
        decision = character(),
        reviewer = character(),
        reason = character(),
        comment = character(),
        timestamp = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
}

server <- function(input, output, session) {

  # Define Reactives/Values----

  ##  initialize reactive values and species cache----
  rv <- shiny::reactiveValues(
    filtered = NULL,
    reset_counter = 0
  )
  species_data <- shiny::reactiveVal(NULL)
  current_species <- shiny::reactiveVal(NULL)
  reviewed_ids <- shiny::reactiveVal(NULL)
  bird_cache <- cachem::cache_mem(max_n = 50)



  # Update the UI----

  ## enable Load button only when species selected----

  shiny::observe({
    shinyjs::toggleState(
      "load_data",
      condition =
        !is.null(input$species_select) &&
        nzchar(input$species_select)
    )
  })



  ## load new species from file----

  shiny::observeEvent(
    input$load_data, {
      species <- input$species_select
      if (is.null(species) || species == "") return()

      # Only load if species changed
      if (!identical(current_species(), species)) {

        spp_fname <- file.path("data/species_data", paste0("bba3_",  sub("/", "_", species), ".rds"))
        df <- readRDS(spp_fname)
        #unlink(spp_fname)  #####Deletes the file. This causes problems.

        # Replace reason codes with user-friendly definitions
        df$new_code_reason <-
          names(br_reasons)[match(df$change_reason, br_reasons)]

        # Cache loaded species
        species_data(df)
        current_species(species)
      }

      # Cache reviewed IDs
      reviewed_ids(tryCatch(fetch_review_log(), error = function(e) NULL))
      # reviewed_ids(tryCatch({
      #   log <- googlesheets4::read_sheet(review_sheet_id, col_types = "c")
      #
      #   # Return NULL if the sheet is empty or missing expected columns
      #   if (nrow(log) == 0 || !all(c("obs_id", "decision") %in% names(log))) {
      #     NULL
      #   } else {
      #     as.data.frame(log)
      #   }
      #
      # }, error = function(e) NULL))
    }
  )


  ## update species based on county filter----

  shiny::observeEvent(list(input$county_select, input$coded_only), {

    # start with the full list
    species_list_filtered <- species_list_full

    # apply county filter to the choices
    if (!is.null(input$county_select) && !"All Regions" %in% input$county_select) {
      species_list_filtered <- species_list_filtered |>
        dplyr::filter(block_county %in% input$county_select)
    }

    # get the final unique, sorted list of names
    final_choices <- species_list_filtered |>
      dplyr::distinct(common_name) |>
      dplyr::pull(common_name) |>
      sort()

    # update the UI
    shiny::updateSelectInput(
      session,
      inputId = "species_select",
      choices = final_choices,
      selected = input$species_select
    )

  }, ignoreInit = TRUE)



  # Data Processing----

  ## filter dataset----

  filtered_data <- shiny::reactive({

    df <- species_data()
    shiny::req(df)

    # County
    if (
      !is.null(input$county_select) &&
      !"All Regions" %in% input$county_select
    ) {
      df <- df |>
        dplyr::filter(block_county %in% input$county_select)
    }

    # Season
    if (
      !is.null(input$season_select) &&
      !"All Seasons" %in% input$season_select
    ) {
      df <- df |>
        dplyr::filter(breeding_season %in% input$season_select)
    }

    # Merge review log
    if (!is.null(reviewed_ids())) {
      df <- df |>
        dplyr::left_join(
          reviewed_ids() |>
            dplyr::select(obs_id, decision) |>
            dplyr::group_by(obs_id) |>
            dplyr::summarise(
              decision = paste(unique(decision), collapse = " | "),
              .groups = "drop"
            ),
          by = c("global_unique_identifier" = "obs_id")
        ) |>
        dplyr::mutate(reviewed = !is.na(decision))
    } else {
      df <- df |> dplyr::mutate(reviewed = NA, decision = NA_character_)
    }

    # Records not yet reviewed
    if (isTRUE(input$unreviewed_only)) {
      df <- df |> dplyr::filter(!reviewed | is.na(reviewed))
    }

    # Coded only
    if (isTRUE(input$coded_only)) {
      df <- df |>
        dplyr::filter(final_category %in% c("possible", "probable", "confirmed"))
    }

    # Flagged only
    if (isTRUE(input$flagged_only)) {
      df <- df |> dplyr::filter(review_cat %in% c("uncertain", "unlikely"))
    }

    # Changed records only
    if (isTRUE(input$changed_only)) {
      df <- df |> dplyr::filter(review_status %in% c("changed", "reviewed-changed"))
    }

    # Change NAs to --
    #df$max_block_code <- as.character(df$max_block_code)
    #df <- df[is.na(df) | df == ""] <- "--"
    #str(df)

    # Return filtered dataset
    df
  })



  ## update reactiveValues for DT & Leaflet----

  ### update num obs----

  shiny::observe({
    df <- filtered_data()

    # Check if we hit the limit
    if (!is.null(df) && nrow(df) > 50000) {
      shiny::showNotification(
        paste0("This species has ", nrow(df), " observations -- too many to load. Adjust filters."),
        type = "error",
        duration = 15
      )
      rv$filtered <- NULL # clear the data
    } else {
      rv$filtered <- df   # keep the data
    }
  })

  output$NumObs <- shiny::renderUI({
    df <- rv$filtered
    species <- current_species()

    shiny::req(df, species)

    # Return the tag directly
    htmltools::h4(
      paste("Returning", nrow(df), "observations of", species),
      style = "color: #00688B;"
    )
  })


  ### update range map----

  load_gpkg_raw <- function(species, dates_df) {

    # determine the file path
    tmp <- dates$range_notes[dates$common_name %in% species]
    # if species has range_note in breeding date table, use what is directed
    if (!is.na(tmp) && nzchar(tmp)) {
      fname_range <- file.path("data/ranges", paste0(tmp, ".gpkg"))
    } else {
      # otherwise use ebird status and trends map
      fname_range <- file.path(
        "data/ranges",
        paste0(sub("/", "_", species), "-breeding-range-raw-9k-2024-NY.gpkg")
      )
    }

    # exit if file missing
    if (!file.exists(fname_range)) return(NULL)

    # read in range map
    map_data <- sf::st_read(fname_range, quiet = TRUE)
    # transform CRS only if necessary
    if (sf::st_crs(map_data)$epsg != 4326) {
      map_data <- sf::st_transform(map_data, crs = 4326)
    }

    return(map_data)
  }

  # add range map to cache
  load_gpkg_cached <- memoise::memoise(load_gpkg_raw, cache = bird_cache)

  # load species range on map
  species_range <- shiny::reactive({
    species <- current_species()
    shiny::req(species, species != "")
    #checks cache before loading anew
    load_gpkg_cached(species, dates)
  })


  ### update breeding season cutoffs----

  clean_date <- function(x) {
    if (is.na(x) || x == "") return("N/A")
    d <- as.Date(x, format = "%m/%d/%y")
    format(d, "%b %d")
  }

  output$species_details_ui <- shiny::renderUI({

    species <- current_species()
    shiny::req(species, species != "")

    species_dates <- dates[dates$common_name == species, ]

    htmltools::tagList(
      htmltools::h5(paste0("Breeding season cutoffs for ", species, ":")),
      tags$ul(
        tags$li(paste("Pre-breeding start:", clean_date(species_dates$pre_breeding_start))),
        tags$li(paste("Core breeding start:", clean_date(species_dates$core_breeding_start))),
        tags$li(paste("Core breeding end:", clean_date(species_dates$core_breeding_end))),
        tags$li(paste("Post-breeding end:", clean_date(species_dates$post_breeding_end)))
      )
    )
  })



  # Render Outputs----

  ## map panel----

  ### background layers----
  output$map <- leaflet::renderLeaflet({

    leaflet::leaflet() |>
      # define base maps
      leaflet::addProviderTiles(providers$OpenStreetMap, group = "Street") |>
      leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      leaflet::addLayersControl(
        baseGroups = c("Street", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      leaflet::setView(lng = -75.53, lat = 42.95, zoom = 7) |>

      # define pane levels for layer stacking
      leaflet::addMapPane("ranges_pane", zIndex = 405) |>
      leaflet::addMapPane("county_pane", zIndex = 410) |>
      leaflet::addMapPane("blocks_pane", zIndex = 415) |>
      leaflet::addMapPane("points_pane", zIndex = 420) |>

      # load blocks once at startup
      leaflet::addPolygons(
        data = atlas_blocks,
        group = "blocks",
        color = "gray",
        weight = 2,
        opacity = 0.5,
        fill = FALSE,
        options = pathOptions(pane = "blocks_pane")
      ) |>
      # but keep blocks hidden for now
      leaflet::hideGroup("blocks")
  })

  ### observation points----
  shiny::observe({
    shiny::req(rv$filtered)
    df <- rv$filtered

    proxy <- leaflet::leafletProxy("map") |>
      leaflet::clearMarkers() |>
      leaflet::clearControls()

    if (nrow(df) == 0) {
      proxy |>
        leaflet::addControl(
          "<b style='color:red;'>No observations returned</b>",
          position = "topright"
        )
      return()
    }

    #set legend text and colors
    colors <- c(
      "Observed" = "#ADD8E6",
      "Possible" = "#AA88BF",
      "Probable" = "#70447F",
      "Confirmed" = "#36013F"
    )

    pal <- leaflet::colorFactor(
      c("#ADD8E6", "#AA88BF", "#70447F", "#36013F"),
      levels = c("observed", "possible", "probable", "confirmed")
    )

    grouped <- df |>
      dplyr::group_by(latitude, longitude) |>
      dplyr::mutate(
        final_category = factor(
          final_category,
          levels = c("observed", "possible", "probable", "confirmed"),
          ordered = TRUE
        )
      ) |>
      dplyr::summarise(
        popup_text = paste0(
          "<div style='max-height:200px; overflow-y:auto;'>",
          paste(
            paste0(
              "<b>", common_name, "</b><br>",
              "Date: ", observation_date, " (Season = ", breeding_season, ")", "<br>",
              "Review status: ", review_status, "<br>",
              "Code: internal - ", final_code, " (original - ", orig_breeding_code, ")", "<br>",
              "Review reason: ", change_reason, "<br>",
              "Range status: ", range_status, " (km from range = ", distance_to_range_km, ")", "<br>",
              "Highest passed code in block: ", max_block_code, "<br>",
              "Has support: ", has_support, "<br>",
              "<a href='", checklist_link, "' target='_blank' onclick='event.stopPropagation();'>View checklist</a><br>",
              "<button onclick=\"event.stopPropagation(); Shiny.setInputValue('selected_obs_ids', ['",
              global_unique_identifier,
              "'], {priority: 'event'})\">Review This</button>"
            ),
            collapse="<hr>"
          ),
          "</div>"
        ),
        cat = as.character(max(final_category)),
        reviewed = isTRUE(all(reviewed)),
        .groups = "drop"
      )

    proxy |>
      leaflet::addPolygons(
        data = counties,
        fill = FALSE,
        stroke = TRUE,
        color = "#828282",
        weight = 1.5,
        options = leaflet::pathOptions(pane = "county_pane")
      ) |>
      leaflet::addCircleMarkers(
        data = grouped,
        lat = ~latitude,
        lng = ~longitude,
        radius = 6,
        fill = TRUE,
        fillColor = ~pal(cat),
        fillOpacity = 1,
        stroke = TRUE,
        color = ~ifelse(reviewed, "#CD3278", "#000000"), #stroke color #CD6600
        weight = ~ifelse(reviewed, 3, 0.5), #stroke weight
        opacity = 0.75,
        popup = ~popup_text,
        popupOptions = leaflet::popupOptions(closeOnClick = FALSE),
        options = leaflet::pathOptions(pane = "points_pane")
      ) |>
      leaflet::addLegend(
        position = "bottomright",
        colors = unname(colors),
        labels = names(colors),
        title = "Breeding Category",
        opacity = 1
      )

  })

  ### range map----
  shiny::observe({
    proxy <- leaflet::leafletProxy("map")
    proxy |> leaflet::clearGroup("range_poly")

    shiny::req(input$show_range)

    data <- species_range() # Reactive: triggers on species change

    if (!is.null(data)) {
      proxy |>
        leaflet::addPolygons(
          data = data,
          group = "range_poly",
          color = "orange",
          fillOpacity = 0.4,
          stroke = TRUE,
          options = pathOptions(pane = "ranges_pane")
        )
    }
  })

  ### atlas blocks----
  shiny::observeEvent(input$show_blocks, {
    proxy <- leaflet::leafletProxy("map")

    if (input$show_blocks) {
      proxy |> leaflet::showGroup("blocks")
    } else {
      proxy |> leaflet::hideGroup("blocks")
    }
  }, ignoreInit = TRUE)



  ## data table----

  output$review_table <- DT::renderDT({

    ### get data to display----
    df <- rv$filtered

    if (is.null(df) || nrow(df) == 0) {
      return(
        DT::datatable(
          data.frame(Message = "No observations to display"),
          options = list(dom = 't', paging = FALSE),
          rownames = FALSE
        )
      )
    }

    ### clean up the columns----

    # rename columns so they are more intuitive
    df <- dplyr::rename(
      df,
      "Species" = "common_name",
      "Obs date" = "observation_date",
      "Season" = "breeding_season",
      "County" = "block_county",
      "Review status" = "review_status", # approved, changed, reviewed-changed, reviewed-approved
      "Original code" = "orig_breeding_code",
      "Internal review code" = "final_code",
      "Internal review reason" = "change_reason",
      "Internal review date" = "review_date",
      "Externally reviewed" = "reviewed",
      "External review code" = "decision",
      "Max passed code" = "max_block_code",
      "Distance to range" = "distance_to_range_km",
      "Range status" = "range_status",
      "Resident status" = "resident_status",
      "Has support" = "has_support"
    ) |>
    # order the columns
    dplyr::select(
      "Species", "Obs date", "Season", "County",
      "Review status", "Original code", "Internal review code",
      "Internal review reason", "Internal review date", "Externally reviewed",
      "External review code", "Max passed code", "Distance to range",
      "Range status", "Resident status", "Has support", dplyr::everything()
    )

    # create new columns
    df <- df |>
      dplyr::mutate(
        select = paste0(
          "<input type='checkbox' class='obs_checkbox' data-id='",
          global_unique_identifier,
          "'>"
        ),
        url = paste0("<a href='", checklist_link, "' target='_blank'>View checklist</a>")
      ) |>
      dplyr::select(select, url, dplyr::everything())

    # hide some columns
    cols_to_hide <- c(
      "global_unique_identifier", "orig_breeding_category", "latitude",
      "longitude", "user_change", "st_review_new_code", "checklist_link",
      "review_cat", "code_review_cat", "code_review_new_code", "st_review_status",
      "st_review_cat", "final_category", "manual_new_code", "new_code_reason",
      "rarity_status", "sampling_event_identifier", "atlas_block"
    )

    ### make the table----
    DT::datatable(
      df,
      colnames = c(
        "Select record",
        "Checklist link",
        names(df)[3:ncol(df)]
      ),
      escape = which(!names(df) %in% c("url", "select")),
      rownames = FALSE,
      selection = "none",
      filter = "top",
      caption = paste("Atlas observations for", current_species()),
      options = list(
        # stateSave = TRUE,
        # stateDuration = -1,
        columnDefs = list(
          # disable sorting on the first column
          list(targets = 0, orderable = FALSE),

          # hide some columns from ui
          list(
            targets = which(names(df) %in% cols_to_hide) - 1,
            visible = FALSE
          ),

          # set width for the first column ("Select")
          list(width = '80px', targets = 0)
        ),

        pageLength = 10,
        dom = "ftipl",
        scrollX = TRUE,
        searchHighlight = TRUE,
        autoWidth = TRUE,

        rowCallback = htmlwidgets::JS(
          "function(row, data, index) {
            var checkbox = $('input.obs_checkbox', row);
            checkbox.off('change').on('change', function() {
              var ids = [];
              $('#review_table').find('.obs_checkbox:checked').each(function() {
                ids.push($(this).data('id'));
              });
              Shiny.setInputValue('selected_obs_ids', ids);
            });
          }"
        ),

        # Select all handler - now a proper Shiny input, no JS needed
        shiny::observeEvent(input$select_all_obs, {
          df <- rv$filtered
          shiny::req(df)
          if (input$select_all_obs) {
            shinyjs::runjs(sprintf(
              "var ids = []; $('#review_table').find('.obs_checkbox').prop('checked', true).each(function() { ids.push($(this).data('id')); }); Shiny.setInputValue('selected_obs_ids', ids);"
            ))
          } else {
            shinyjs::runjs(
              "$('#review_table').find('.obs_checkbox').prop('checked', false); Shiny.setInputValue('selected_obs_ids', null);"
            )
          }
        })
      )
    )
  },
  server = TRUE
  )



  ## review panel----

  output$review_ui <- shiny::renderUI({

    rv$reset_counter
    shiny::req(input$selected_obs_ids)
    n_selected <- length(input$selected_obs_ids)

    # pre sort the reasons
    sorted_reasons <- sort(br_reasons)

    # button label logic to handle singular vs plural
    button_label <- if (n_selected == 1) {
      "Save review for 1 observation"
    } else {
      paste("Save review for", n_selected, "observations")
    }

    # scroll to the panel after it renders
    session$sendCustomMessage("scroll_to_review", list())

    # return tag list
    htmltools::tagList(
      htmltools::h4("Submit Review for Selected Data"),
      shiny::selectInput("decision", "Review Decision:", choices = c("", br_codes)),
      shiny::selectInput("reason", "Review Reason:", choices = c("", sorted_reasons)),
      shiny::textAreaInput("feedback", "Reviewer Comments:"),
      shiny::actionButton(
        "save_review",
        label = button_label,
        class = "btn-primary" # Makes the button blue and prominent
      ),
      shiny::actionButton("reset_review", "Reset Review")
    )
  })

  shiny::observeEvent(input$save_review, {

    shiny::req(input$selected_obs_ids, rv$filtered)

    if (is.null(input$reviewer_name) || input$reviewer_name == "") {
      shiny::showModal(
        shiny::modalDialog(
          title = "Missing Reviewer Name",
          "Please enter your name before submitting a review.",
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
      return()
    }

    if (is.null(input$decision) || input$decision == "") {
      shiny::showModal(
        shiny::modalDialog(
          title = "Missing Decision",
          "Please select a review decision before saving.",
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
      return()
    }

    if (is.null(input$reason) || input$reason == "") {
      shiny::showModal(
        shiny::modalDialog(
          title = "Missing Review Reason",
          "Please select the best reason for your review before saving.",
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
      return()
    }

    selected_rows <- rv$filtered |>
      dplyr::filter(global_unique_identifier %in% input$selected_obs_ids)

    # create new df with the review data
    new_review <- data.frame(
      obs_id = selected_rows$global_unique_identifier,
      reviewer = input$reviewer_name,
      decision = input$decision,
      reason = input$reason,
      comment = input$feedback,
      timestamp = Sys.time(),
      stringsAsFactors = FALSE
    )

    tryCatch({
      # googlesheets4::sheet_append(ss = review_sheet_id, data = new_review)
      write.table(
        new_review,
        file = "data/review_log.csv",
        sep = ",",
        append = file.exists("data/review_log.csv"),
        col.names = !file.exists("data/review_log.csv"),
        row.names = FALSE,
        quote = TRUE
      )

      # Only update rv$filtered if the write succeeded
      rv$filtered <- rv$filtered |>
        dplyr::mutate(
          decision = dplyr::if_else(
            global_unique_identifier %in% input$selected_obs_ids,
            input$decision,
            decision
          ),
          reviewed = dplyr::if_else(
            global_unique_identifier %in% input$selected_obs_ids,
            TRUE,
            reviewed
          )
        )

      proxy <- DT::dataTableProxy("review_table")
      DT::replaceData(proxy, rv$filtered, resetPaging = FALSE, clearSelection = FALSE)

      session$sendCustomMessage("clear_checkboxes_only", list())
      shinyjs::runjs("Shiny.setInputValue('selected_obs_ids', null);")
      shiny::showNotification("Review saved. Table updated.", type = "message")

    }, error = function(e) {
      shiny::showNotification(
        paste("Error saving review:", e$message),
        type = "error",
        duration = 10
      )
    })
  })

  # clear the review boxes if reset review button pressed
  shiny::observeEvent(input$reset_review, {
    rv$reset_counter <- rv$reset_counter + 1
  })



  # Reset filters----

  shiny::observeEvent(input$reset_filters, {
    shiny::updateSelectInput(
      session, "species_select",
      selected = "",
      choices = species_list
    )
    shiny::updateSelectInput(session, "county_select", selected = character(0))
    shiny::updateSelectInput(session, "season_select", selected = character(0))
    shiny::updateCheckboxInput(session, "unreviewed_only", value = FALSE)
    shiny::updateCheckboxInput(session, "coded_only", value = FALSE)
    shiny::updateCheckboxInput(session, "flagged_only", value = FALSE)
    shiny::updateCheckboxInput(session, "changed_only", value = FALSE)
    shiny::updateCheckboxInput(session, "show_blocks", value = FALSE)
    shiny::updateCheckboxInput(session, "show_range", value = FALSE)

    rv$filtered <- NULL
    species_data(NULL)
    current_species(NULL)

    leaflet::leafletProxy("map") |>
      leaflet::clearMarkers() |>
      leaflet::clearControls() |>
      leaflet::setView(lng = -75.53, lat = 42.95, zoom = 8)
  })
}


shiny::shinyApp(ui, server)
