read_front_matter <- function(path) {
  raw_qmd <- readLines(path)
  fm_locations <- which(raw_qmd == "---")
  raw_yaml <- raw_qmd[(fm_locations[1] + 1):(fm_locations[2] - 1)]
  yaml.load(raw_yaml)
}