install.packages("ggplot2")
install.packages("sf")
install.packages("ggspatial")
install.packages("giscoR")
install.packages("magick")
install.packages("ggpattern")
install.packages("dplyr")
install.packages("ggfx")


shp_italy <- giscoR::gisco_get_nuts(
  year = "2021", country = "Italy", resolution = "10", nuts_level = "2"
)
st_crs(shp_italy)

# Set fill colors randomly for the geometries in shp_italy
shp_italy$fill_color <- sample(
  c("#e1d9c7", "#d4c9b1", "#f3efdd", "#f3e3c3", "#e6dec4"), 
  size = nrow(shp_italy), replace = TRUE
)

map_labels <- shp_italy %>%
  select(NUTS_ID, NUTS_NAME, geometry) %>%
  mutate(
    # First, calculate the centroids
    geometry = st_centroid(geometry)
  )

shifting_offsets <- map_labels$geometry

shifting_offsets[map_labels$NUTS_ID %in% c("DEE0", "DEC0")] <- 
  st_sfc(st_point(c(0, 0.2))) + shifting_offsets[map_labels$NUTS_ID %in% c("DEE0", "DEC0")]

shifting_offsets[map_labels$NUTS_ID == "DE40"] <- 
  st_sfc(st_point(c(-0.2, 0.3))) + shifting_offsets[map_labels$NUTS_ID == "DE40"]

map_labels$geometry <- shifting_offsets

st_crs(map_labels) <- 4326  

st_crs(map_labels)
head(map_labels)
map <- shp_italy |> 
  ggplot() +
  ggfx::with_shadow(
    geom_sf_pattern(
      aes(
        fill = fill_color,  
        pattern_type = NUTS_ID
      ),
      colour = "black", pattern_fill = "black", pattern_aspect_ratio = 0.25,
      size = 0.1, pattern = "magick", show.legend = FALSE
    ),
    colour = "#4a3b2b", x_offset = 3, y_offset = 3
  ) +
  scale_fill_manual(values = light_brown_palette) +  
  geom_sf_label(
    data = shp_italy,
    aes(label = shortened_labels, geometry = centroid),  
    angle = 0, hjust = 0.5, vjust = 0.5, size = 2.5,  
    family = "Garamond", 
    label.size = 0.1,  
    fill = "#FAF3E0",  
    color = "black"    
  ) +
  annotation_scale(
    location = "bl", width_hint = 0.2, text_col = "#7b6d58",
    text_family = "Garamond"
  ) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering(
      fill = c("#7b6d58", "#f3efdd"), line_col = "#2d2d2a"
    )
  ) +
  scale_pattern_type_discrete(choices = gridpattern::names_magick) +
  labs(
    title = "Government Regions in Italy",
    caption = "Source: GISCO. Visualization: Taiwo Kayode"
  ) +
  coord_sf(expand = TRUE) +  
  theme_minimal(base_family = "Garamond") + 
  theme(
    plot.background = element_rect(fill = "#F5DEB3", color = NA), 
    panel.background = element_rect(fill = "#F5DEB3", color = NA), 
    panel.grid.major = element_line(color = "#d4c9b1", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 9, color = "#7b6d58"),
    axis.title = element_blank(),
    plot.title = element_text(
      size = 40, color = "#4a3b2b", hjust = 0.5, face = "bold", lineheight = 1.2
    ),
    plot.caption = element_text(hjust = 0.5, size = 14, lineheight = 1.1),  
    plot.subtitle = element_text(hjust = 0.5),
    plot.margin = margin(20, 20, 50, 20)  
  )

# Display the map
print(map)
# Save the map as a PNG file
ggsave("italy_map_vintage.png", plot = map, width = 10, height = 8, dpi = 300)
