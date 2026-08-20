# nybba3
Code to run NY Breeding Bird Atlas III, including coordination, data review, and analysis stages

# Coordination:
  - Block sign-up and monitoring block progress:
  - Atlaser certificates:

# Data review:

1. Prepare the data:
    - prepare_external_data.R
    - prepare_observation_data.R (pulls in EBD, sensitive data, hidden data, zero count data, and external data)
    - prepare_checklist_data.R
    - prepare_ebird_ranges.R
    - more details here: [Data preparation metadata](https://docs.google.com/document/d/127n37vgndwFBinGIvX6l_9k4bBWSJxadpU6HaNpJntY/edit?usp=sharing)

2. Perform checklist-level review:
    - checklist1_moving_check.R (also processes bounding boxes of eBird tracks)
    - checklist2_point_check.R
    - checklist3_effort_time.R
    - more details here: [Pseudocode for checklist review](https://docs.google.com/document/d/1JRLThutdeRqQ0RxNVx0Vke-_Tmd-V3SD6pGIPJ0jf4g/edit?usp=sharing)

3. Perform observation-level review:
    - observation_review1_initial_screening.R (need to have species list, expected codes, expected ranges, and expected dates)
    - observation_review2_incorporate_external_review.R
    - observation_review3_process_uncoded_observations.R
    - more details here: [Pseudocode for observation review](https://docs.google.com/document/d/1SHpsQ80MA7s1o7Gdj4cfjvTcumFNz_HMbKb1EXAoIkU/edit?usp=sharing)

4. Alongside observation-level review, use:
    - determine_season_dates.R
    - make_shiny_review_files.R
    - shiny_app_for_data_review.R
    - see: [Data review tool](https://nynhp.shinyapps.io/atlas-review-tool/)

# Analysis:
  - Block maps
  - Dot maps
  - Phenology bar charts
  - Phenology density charts
  - Topo rollup change maps
  - Get block stats
  - 
