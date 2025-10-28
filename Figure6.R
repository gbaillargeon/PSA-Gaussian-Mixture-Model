

library(ggplot2)
library(plotly)
library(dplyr)
library(ellipse)
library(viridis)


set.seed(123)

# Ensure cluster column
PSAdata$cluster <- factor(GMM6$classification)


max_p <- PSAdata %>% filter(p == max(p))                     # Maximum productivity
max_s <- PSAdata %>% filter(s == max(s))                     # Maximum susceptibility
min_v <- PSAdata %>% filter(v == min(v))                     # Minimum vulnerability
max_v_species <- PSAdata %>% filter(Species == "Myrichthys colubrinus")  # Maximum vulnerability (tie between two species)

# Count overlapping species per unique (p, s) coordinate
PSAdata_count <- PSAdata %>%
  group_by(p, s) %>%
  mutate(count = n()) %>%
  ungroup()

# Compute ellipses manually for each cluster
ellipse_df <- do.call(rbind, lapply(levels(PSAdata$cluster), function(cl) {
  sub <- PSAdata[PSAdata$cluster == cl, ]
  if (nrow(sub) < 2) return(NULL)
  e <- ellipse::ellipse(
    cov(sub[, c("p", "s")]),
    centre = colMeans(sub[, c("p", "s")]),
    level = 0.95
  )
  e_df <- data.frame(e)
  colnames(e_df) <- c("p", "s")
  e_df$cluster <- cl
  return(e_df)
}))

# Reverse viridis colors for ellipses
viridis_colors <- viridis(3, direction = -1)
names(viridis_colors) <- levels(PSAdata$cluster)

# Combine highlight points for legend and mapping
highlight_df <- bind_rows(
  max_v_species %>% mutate(type = "Maximum vulnerability"),
  min_v %>% mutate(type = "Minimum vulnerability"),
  max_p %>% mutate(type = "Maximum productivity"),
  max_s %>% mutate(type = "Maximum susceptibility")
)

# Assign shapes and fill colors
highlight_df <- highlight_df %>%
  mutate(
    shape_manual = case_when(
      type == "Maximum vulnerability" ~ 24,   # triangle up
      type == "Minimum vulnerability" ~ 25,   # triangle down
      type == "Maximum productivity" ~ 22,    # square
      type == "Maximum susceptibility" ~ 22   # square
    ),
    fill_manual = case_when(
      type == "Maximum vulnerability" ~ "purple",
      type == "Minimum vulnerability" ~ "blue",
      type == "Maximum productivity" ~ "cyan",
      type == "Maximum susceptibility" ~ "pink"
    )
  )

highlight_df$type <- factor(
  highlight_df$type,
  levels = c("Maximum vulnerability",
             "Minimum vulnerability",
             "Maximum productivity",
             "Maximum susceptibility")
)

# === PLOT ===
gg <- ggplot() +
  
  # Ellipses behind points
  geom_path(
    data = ellipse_df,
    aes(x = p, y = s, group = cluster),
    color = viridis_colors[ellipse_df$cluster],
    linewidth = 1.5,
    alpha = 0.6
  ) +
  
  # Main points: viridis gradient for vulnerability, size = species count
  geom_point(
    data = PSAdata_count,
    aes(x = p, y = s, color = v, size = count, text = Species),
    alpha = 0.6
  ) +
  scale_color_viridis_c(option = "D", direction = -1, name = NULL) +
  scale_size(range = c(2, 7), name = NULL) +
  
  # Highlight key points with shapes/fills
  geom_point(
    data = highlight_df,
    aes(x = p, y = s, shape = type, fill = type),
    size = 5,
    color = "black",
    stroke = 0.7
  ) +
  scale_shape_manual(
    name = NULL,
    values = c(
      "Maximum vulnerability" = 24,
      "Minimum vulnerability" = 25,
      "Maximum productivity" = 22,
      "Maximum susceptibility" = 22
    )
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Maximum vulnerability" = "orange1",
      "Minimum vulnerability" = "blue",
      "Maximum productivity" = "cyan",
      "Maximum susceptibility" = "hotpink"
    )
  ) +
  
  # Labels and theme
  labs(
    title = "Productivity vs. Susceptibility with GMM Clustering",
    x = "Productivity",
    y = "Susceptibility"
  ) +
  scale_x_reverse() +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )

# Convert to interactive plot
interactive_plot <- ggplotly(gg, tooltip = c("text", "x", "y", "color", "size"))
interactive_plot
