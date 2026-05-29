library(dplyr)
library(ggplot2)
library(ggtext)
library(readr)
library(sf)


## INPUT
#' Set a `country`, this will automate the download and data processing for that
#' particular country.
#' However, adjustments might be necessary:
#' a) If a country has areas spread across the globe (e.g. Netherlands, France,
#'    also the US) you might want to limit the coordinate system using coord_sf()
#' b) When saving the plot, the aspect ratio is determined from the rectangular
#'    bounding box of the country. Maybe needs some adjustments.
#' c) Too many overlaps among text labels (ggrepel)
#' d) The plot code will fail if there are more categories than avaiable colors
#'    in the color palette (9 colors). Increase the minimum value to built a category
#'    in the call of fct_lump()
#'
#' Note: Not checked for non-Latin-letter names

####################### Input parameters ##########################
# which country >>>>>
country <- "Madagascar"
# Check for ASCII version (i.e. "è" becomes "e") - TRUE/FALSE >>>>>
check_ascii <- FALSE
# where to store the data
data_dir <- here::here("palindrome-places", "data")
###################################################################

# Checks if a string is a palindrome
# Returns a vector of TRUE/FALSE if given a vector
is_palindrome <- function(s) {
  s_lower <- tolower(s)
  s_split <- strsplit(s_lower, split = "")
  s_lower == sapply(s_split, function(x) paste(rev(x), collapse = ""))
}


#' Downloads and unzips country datasets from GeoNames.org
#' http://download.geonames.org/export/dump/
download_and_unzip_geonames <- function(country, data_dir) {
  # get country code from English country name
  country_code <- countrycode::countrycode(
    country,
    origin = "country.name",
    destination = "iso2c"
  )
  geonames_url <- glue::glue(
    "http://download.geonames.org/export/dump/{country_code}.zip"
  )
  geonames_localfile_zip <- here::here(
    data_dir,
    glue::glue("geonames_{tolower(country_code)}.zip")
  )

  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }
  if (!file.exists(geonames_localfile_zip)) {
    download.file(geonames_url, destfile = geonames_localfile_zip, mode = "wb")
  }
  geonames_localfile <- unzip(geonames_localfile_zip, list = TRUE) |>
    filter(Name != "readme.txt")
  unzip(geonames_localfile_zip, exdir = data_dir)

  c("filename" = geonames_localfile$Name[1])
}

filename <- download_and_unzip_geonames(country, data_dir = data_dir)
filename

places <- read_tsv(
  here::here(data_dir, filename),
  col_names = c(
    "geonameid",
    "name",
    "asciiname",
    "alternatenames",
    "latitude",
    "longitude",
    "feature_class",
    "feature_code",
    "country_code",
    "cc2",
    "admin1_code",
    "admin2_code",
    "admin3_code",
    "admin4_code",
    "population",
    "elevation",
    "dem",
    "timezone",
    "modification_date"
  )
)

# Find all palindromes in the place names column
if (check_ascii) {
  places_palindromes <- places |>
    filter(is_palindrome(name) | is_palindrome(asciiname))
} else {
  places_palindromes <- places |>
    filter(is_palindrome(name))
}

places_palindromes <- places_palindromes |>
  filter(feature_class == "P") |> # city, village etc., see http://www.geonames.org/export/codes.html
  mutate(name2 = forcats::fct_lump_min(name, min = 4)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326")

# load country shape
shp <- rnaturalearth::ne_countries(
  scale = 10,
  country = country,
  returnclass = "sf"
)

color_accent <- "#ab5756"
color_white <- "#f8fafb"
color_gray <- "#b8c5cd"
color_black <- "#333333"

# Annotations
plot_titles <- list(
  title = glue::glue(
    "<span style='color: { color_accent };'>Palindromic Places</span> ",
    "in {country}"
  ),
  subtitle = glue::glue(
    "There are **{nrow(places_palindromes)}** ",
    "<span style='color: { color_accent };'>**place names\\***</span> that are ",
    "<span style='color: { color_accent };'>**spelled the same way backward as forward**</span> in Madagascar; ",
    "**5** of them are exactly the same: \"**Anena**\"<br>",
    "<span style='font-size: 12pt;'>(**\\*** Populated places according to GeoNames.org)</span>"
  ),
  caption = "**Source:** GeoNames.org | **base map:** Natural Earth Data<br>**Visualization:** Andriambelo Rajo based on Ansgar Wolsing's #rstats code"
)

st_crs(shp)
st_crs(places_palindromes)

# Add a buffer around the map to palce the labels
map_buffer <- shp |>
  st_buffer(98000) |>
  st_boundary() |>
  st_transform(3857) |>
  st_line_sample(20) |>
  st_transform(4326)

places_palindromes_df <- places_palindromes |>
  mutate(
    x = st_coordinates(geometry)[, "X"],
    y = st_coordinates(geometry)[, "Y"],
  ) |>
  st_drop_geometry()

p1 <-
  ggplot(shp) +
  ggfx::with_shadow(
    geom_sf(
      size = 0.2,
      fill = "#b8c5cd"
    ),
    x_offset = 8,
    y_offset = 8,
    colour = color_black
  ) +
  geom_sf(
    data = places_palindromes,
    shape = 21,
    stroke = 1.12,
    size = 3.2,
    fill = color_white,
    color = color_black
  ) +
  geom_sf(data = map_buffer, color = "#f0f4f9", size = 0) +
  ggpointgrid::geom_segmentgrid(
    data = places_palindromes_df,
    aes(x, y),
    grid_xy = st_coordinates(map_buffer),
    linetype = "1234",
    linewidth = 0.75,
    color = color_black
  ) +
  ggpointgrid::geom_labelgrid(
    data = places_palindromes_df,
    aes(
      x,
      y,
      label = name,
      fontface = if_else(name == "Anena", "bold", "plain")
    ),
    grid_xy = st_coordinates(map_buffer),
    size = 4.56,
    color = color_black,
    fill = color_white,
    family = "Atkinson Hyperlegible Mono"
  ) +
  coord_sf(crs = st_crs(shp), expand = FALSE, clip = "off") +
  labs(
    title = plot_titles$title,
    subtitle = plot_titles$subtitle,
    caption = plot_titles$caption
  ) +
  cowplot::theme_map(
    font_size = 14,
    font_family = "Atkinson Hyperlegible Next"
  ) +
  theme(
    plot.background = element_rect(color = NA, fill = "#f0f4f9"),
    text = element_text(color = color_black),
    plot.title = element_markdown(
      family = "Fraunces",
      size = rel(1.8),
      hjust = 0.5,
      margin = margin(t = 10, b = 6)
    ),
    plot.subtitle = element_textbox_simple(
      width = 1.75,
      halign = 0.5,
      lineheight = 1.1,
      size = rel(1.2),
      margin = margin(t = 6, b = 0)
    ),
    plot.caption = element_markdown(
      colorspace::lighten(color_black, 0.2),
      lineheight = 1.33,
      hjust = 0.5,
      size = rel(0.75),
      margin = margin(t = 18, b = 10)
    )
  )

ggsave(
  here::here(
    "palindrome-places",
    "plots",
    glue::glue("palindrome_places_{country}.png")
  ),
  dpi = 300,
  width = 21,
  height = 25,
  units = "cm"
)
