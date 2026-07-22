library(tidyverse)
library(png)
library(grid)
library(gridExtra)
library(forcats)
library(ggnewscale)
library(RColorBrewer) 
library(sf)
library(rnaturalearth) 
library(ggspatial)

## ---------------------------------------------------------------------------
## helper: given a PNG that already exists on disk, also write TIFF (LZW) and
## JPEG versions at the SAME pixel dimensions, so every manuscript figure is
## available as .png / .tif / .jpg. `res` only sets the DPI metadata tag; the
## actual pixel dimensions are taken from the source PNG, so all three formats
## are visually identical. (JPEG re-encode is lossy but fine for raster figures.)
## ---------------------------------------------------------------------------
save_alt_formats <- function(png_path, res = 500, quality = 95) {
  if (!file.exists(png_path)) {
    warning("PNG not found, skipping alternate formats: ", png_path)
    return(invisible(NULL))
  }
  
  img <- png::readPNG(png_path)
  h   <- dim(img)[1]   # height in pixels
  w   <- dim(img)[2]   # width  in pixels
  
  tif_path <- sub("\\.png$", ".tif", png_path, ignore.case = TRUE)
  jpg_path <- sub("\\.png$", ".jpg", png_path, ignore.case = TRUE)
  
  ## TIFF (lossless, LZW-compressed)
  grDevices::tiff(tif_path, width = w, height = h, units = "px",
                  res = res, compression = "lzw")
  grid::grid.raster(img, width = grid::unit(1, "npc"),
                    height = grid::unit(1, "npc"), interpolate = FALSE)
  dev.off()
  message("Saved: ", tif_path)
  
  ## JPEG
  grDevices::jpeg(jpg_path, width = w, height = h, units = "px",
                  res = res, quality = quality)
  grid::grid.raster(img, width = grid::unit(1, "npc"),
                    height = grid::unit(1, "npc"), interpolate = FALSE)
  dev.off()
  message("Saved: ", jpg_path)
  
  invisible(c(tif = tif_path, jpg = jpg_path))
}

##parameters
ocv_status <- "all"

## Figure 1

out_path <- "../../notebooks/manuscript_figures/"
fig1_path <- paste0(out_path, "alert_types.png")

png(fig1_path, width=4000, height=2200, res=400)
par(mfrow=c(2,2), mar=c(2.5,2.5,4,2.5))  # allow space for titles/subtitles


remove_y_axis <- function() { axis(2, labels=FALSE, tick=FALSE) }

arrow_down <- function(x, y, frac=0.1) {
  usr <- par("usr")
  y_length <- usr[4] - usr[3]
  y_top <- y + frac * y_length
  arrows(x0=x, y0=y_top, x1=x, y1=y, col="red3", lwd=3, length=0.15)
}

label_uncertain_bar <- function(bar_midpoints, bar_heights){
  n <- length(bar_midpoints)
  usr <- par("usr")
  y_pos <- bar_heights[n] + 0.06*(usr[4]-usr[3])   # just above the final bar
  text(x=bar_midpoints[n], y=y_pos, labels="?",
       col="red3", font=2, cex=1.6)
}

panel_threshold_box <- function(thresholds, widthfrac=0.5, heightfrac=0.15){
  usr <- par("usr")
  x0 <- usr[1] + 0.01*(usr[2]-usr[1])
  x1 <- x0 + widthfrac*(usr[2]-usr[1])  
  y1 <- usr[4] - 0.01*(usr[4]-usr[3])
  y0 <- y1 - heightfrac*(usr[4]-usr[3])
  rect(x0, y0, x1, y1, col=alpha("white",0.8), border="black")
  text(x=x0+0.01*(usr[2]-usr[1]), y=y1-0.05*(usr[4]-usr[3]),
       labels=paste("Thresholds:", paste(thresholds, collapse=", ")), adj=c(0,1), cex=0.85)
}

bar_col <- "grey80"
bar_alert_col <- "steelblue4"
threshold_col <- "darkorange"

compute_arrow_base <- function(offset_frac=0.15){
  usr <- par("usr")
  return(usr[4] - offset_frac * (usr[4]-usr[3]))
}

## trend-based alert
cholera_cases3 <- c(0,0,0,0,5,4)
weeks3 <- 1:length(cholera_cases3)
mean4weeks <- sapply(1:length(cholera_cases3), function(i){ start <- max(1,i-4); mean(cholera_cases3[start:(i-1)]) })
mean4weeks[1] <- 0
alert1 <- cholera_cases3 > mean4weeks
trigger_week3 <- which(alert1)

bar_colors3 <- ifelse(alert1, bar_alert_col, bar_col)
bar_midpoints3 <- barplot(cholera_cases3,
                          names.arg=paste("Week", weeks3),
                          col=bar_colors3,
                          ylim=c(0,max(cholera_cases3)+2),
                          main="Trend-Based Alert",
                          ylab="")
remove_y_axis()
lines(bar_midpoints3, mean4weeks, col=threshold_col, lwd=3)
if(length(trigger_week3)>0){
  arrow_base <- compute_arrow_base()
  for(i in trigger_week3){
    next_week <- i + 1
    if(next_week <= length(bar_midpoints3)){  # avoid going past last week
      arrow_down(bar_midpoints3[next_week], arrow_base)
    }
  }
}
panel_threshold_box(c(1,2,3), widthfrac=0.7, heightfrac=0.15)
mtext("upward trend relative to mean of previous 4 weeks for x weeks", side=3, line=0.5, cex=0.9)
label_uncertain_bar(bar_midpoints3, cholera_cases3)

## weekly case-based alert
cholera_cases1 <- c(6,6,7,6)
weeks1 <- 1:length(cholera_cases1)
threshold <- 5
alert_weeks <- rep(FALSE, length(cholera_cases1))
for(i in 1:(length(cholera_cases1)-3)) if(all(cholera_cases1[i:(i+2)]>=threshold)) alert_weeks[i+3]<-TRUE

bar_colors1 <- ifelse(cholera_cases1>=threshold, bar_alert_col, bar_col)
bar_midpoints1 <- barplot(cholera_cases1,
                          names.arg=paste("Week", weeks1),
                          col=bar_colors1,
                          ylim=c(0,max(cholera_cases1)+2),
                          main="Weekly Case-Based Alert",
                          ylab="")
remove_y_axis()
abline(h=threshold, col=threshold_col, lwd=3)
if(any(alert_weeks)){
  arrow_base <- compute_arrow_base()
  for(i in which(alert_weeks)) arrow_down(bar_midpoints1[i], arrow_base)
}
panel_threshold_box(c(2,5,10,25,50,100,250), widthfrac=0.7, heightfrac=0.15)
mtext("at least x suspected cases for 3 consecutive weeks", side=3, line=0.5, cex=0.9)
label_uncertain_bar(bar_midpoints1, cholera_cases1)

## cumulative case-based alert
cholera_cases2 <- c(1,2,3,1)
weeks2 <- 1:length(cholera_cases2)
threshold2 <- 5
cumulative_sum <- cumsum(cholera_cases2)
trigger_week <- which(cumulative_sum>=threshold2)[1]+1
if(trigger_week>length(cholera_cases2)) trigger_week <- NA

bar_colors2 <- rep(bar_col, length(cholera_cases2))
bar_midpoints2 <- barplot(cholera_cases2,
                          names.arg=paste("Week", weeks2),
                          col=bar_colors2,
                          ylim=c(0,max(cumulative_sum)+2),
                          main="Cumulative Case-Based Alert",
                          ylab="")
remove_y_axis()
abline(h=threshold2, col=threshold_col, lwd=3)
lines(bar_midpoints2, cumulative_sum, col=bar_alert_col, lwd=3, type="b", pch=19)
if(!is.na(trigger_week)){
  arrow_base <- compute_arrow_base()
  arrow_down(bar_midpoints2[trigger_week], arrow_base)
}
panel_threshold_box(c(5,10,25,50,100,500,1000), widthfrac=0.7, heightfrac=0.15)
mtext("at least x cumulative suspected cases for 3 consecutive weeks", side=3, line=0.5, cex=0.9)
label_uncertain_bar(bar_midpoints2, cholera_cases2)

## rate-based alert
rate_cases <- c(2,2,3,2)
pop <- 10000
weeks4 <- 1:length(rate_cases)
threshold_rate <- 1
weekly_incidence <- (rate_cases/pop)*10000
alert_weeks_rate <- rep(FALSE, length(weeks4))
for(i in 1:(length(weekly_incidence)-2)) if(all(weekly_incidence[i:(i+2)]>=threshold_rate)) alert_weeks_rate[i+3]<-TRUE

bar_colors4 <- ifelse(weekly_incidence>=threshold_rate, bar_alert_col, bar_col)
bar_midpoints4 <- barplot(weekly_incidence,
                          names.arg=paste("Week", weeks4),
                          col=bar_colors4,
                          ylim=c(0,max(weekly_incidence)+0.5),
                          main="Rate-Based Alert",
                          ylab="")
remove_y_axis()
abline(h=threshold_rate, col=threshold_col, lwd=3)
if(any(alert_weeks_rate, na.rm=TRUE)){
  arrow_base <- compute_arrow_base()
  for(i in which(alert_weeks_rate)) arrow_down(bar_midpoints4[i], arrow_base)
}
panel_threshold_box(c(0.25,0.5,1,1.5,2.5,5,7.5), widthfrac=0.7, heightfrac=0.15)
mtext("at least x suspected cases per 10,000 people for 3 consecutive weeks", side=3, line=0.5, cex=0.9)
label_uncertain_bar(bar_midpoints4, weekly_incidence)

dev.off()

save_alt_formats(fig1_path, res = 400)

# ## Figure 2

## paths
bottom_path <- "../../notebooks/manuscript_figures/fig2_boxplot"
top_path    <- "../../notebooks/manuscript_figures/fig2_tradeoff_scatterplots"
out_path    <- "../../notebooks/manuscript_figures/Figure2.png"

# add ocv_status suffix
bottom_path <- paste0(bottom_path, "_", ocv_status, ".png")
top_path    <- paste0(top_path, "_", ocv_status, ".png")
#out_path    <- paste0(out_path, "_", ocv_status, ".png")

img_top <- png::readPNG(top_path)
img_bottom <- png::readPNG(bottom_path)

g_top <- grid::rasterGrob(img_top)
g_bottom <- grid::rasterGrob(img_bottom)

g_bottom_labeled <- gridExtra::arrangeGrob(
  g_bottom,
  top = grid::textGrob(
    "C",
    x = 0, hjust = 0,
    gp = grid::gpar(fontsize = 6, fontface = "bold")
  )
)

combined <- gridExtra::arrangeGrob(
  g_top,
  g_bottom_labeled,
  ncol = 1,
  heights = c(1, 1)
)
png(out_path, width = 3, height = 3, units = "in", res = 500)
grid::grid.draw(combined)
dev.off()

message("Saved combined figure 2 to: ", out_path)
save_alt_formats(out_path, res = 500)   # out_path = .../Figure2.png

## Figure 2 version 2

## paths
bottom_path <- "../../notebooks/manuscript_figures/fig2_boxplot"
top_path    <- "../../notebooks/manuscript_figures/fig2_tradeoff_scatterplots"
out_path    <- "../../notebooks/manuscript_figures/fig2_combined_v2"

## add ocv_status suffix
bottom_path <- paste0(bottom_path, "_", ocv_status, ".png")
top_path    <- paste0(top_path, "_", ocv_status, ".png")
out_path    <- paste0(out_path, "_", ocv_status, ".png")

img_top    <- png::readPNG(top_path)
img_bottom <- png::readPNG(bottom_path)

g_top    <- grid::rasterGrob(img_top)
g_bottom <- grid::rasterGrob(img_bottom)

## left-side group labels for the bottom figure
group_labels <- c("trend", "case", "cum case", "rate")
group_sizes  <- c(3, 7, 7, 7)          # rows per group, top -> bottom
n_rows <- sum(group_sizes)             # 24

## fraction of the PNG height occupied by the plotting rows
top_frac <- 0.8   # top edge of the first row
bot_frac <- 0.1   # bottom edge of the last row

## center of each group in row units from the top, then map to PNG y (1 = top)
ends    <- cumsum(group_sizes)
starts  <- ends - group_sizes
centers <- (starts + ends) / 2         # 1.5, 6.5, 13.5, 20.5
ys      <- top_frac - (top_frac - bot_frac) * (centers / n_rows)

left_grob <- grid::grobTree(
  do.call(grid::gList, lapply(seq_along(group_labels), function(i) {
    grid::textGrob(
      group_labels[i],
      x = 0.5, y = ys[i],
      rot = 90,
      gp = grid::gpar(fontsize = 5)
    )
  }))
)

g_bottom_labeled <- gridExtra::arrangeGrob(
  g_bottom,
  top = grid::textGrob(
    "C",
    x = 0, hjust = 0,
    gp = grid::gpar(fontsize = 6, fontface = "bold")
  ),
  left = left_grob
)

combined <- gridExtra::arrangeGrob(
  g_top,
  g_bottom_labeled,
  ncol = 1,
  heights = c(1, 1)
)

png(out_path, width = 3, height = 3, units = "in", res = 500)
grid::grid.draw(combined)
dev.off()

message("Saved combined figure 2 to: ", out_path)
save_alt_formats(out_path, res = 500)   # out_path = .../fig2_combined_v2_all.png

## Figure 3
## Figure 3 is the pre-rendered bubble alert plot. Here we copy it to the
## standard Figure3.png name and emit the .tif / .jpg versions alongside it.

bubble_src <- paste0("../../notebooks/manuscript_figures/bubble_alert_plot_", ocv_status, ".png")
out_path   <- "../../notebooks/manuscript_figures/Figure3.png"

if (file.exists(bubble_src)) {
  file.copy(bubble_src, out_path, overwrite = TRUE)
  message("Saved figure 3 to: ", out_path)
  save_alt_formats(out_path, res = 500)
} else {
  warning("Bubble alert plot not found: ", bubble_src)
}

## Figure 4

## data
csv1_path <- "../../notebooks/manuscript_figures/top_alerts_cutoff_-1"
csv2_path <- "../../notebooks/manuscript_figures/country_utility_top3_ncountries"
out_path <- "../../notebooks/manuscript_figures/Figure4.png"

## ocv_status suffix
csv1_path <- paste0(csv1_path, "_", ocv_status, ".csv")
csv2_path <- paste0(csv2_path, "_", ocv_status, ".csv")

csv1 <- read_csv(csv1_path, show_col_types = FALSE)
csv2 <- read_csv(csv2_path, show_col_types = FALSE)

csv2 <- csv2 %>%
  mutate(
    pop_brk = case_when(
      pop_brk == "< 50k" ~ "less than 50k",
      pop_brk == "[50k, 500k)" ~ "50k to 500k",
      pop_brk == "≥ 500k" ~ "over 500k",
      TRUE ~ NA_character_
    )
  )

## population groups
population_groups <- c("less than 50k", "50k to 500k", "over 500k")

## custom alert order
custom_order <- c(
  "1-week", "2-week", "3-week", 
  "≥ 2 weekly", "≥ 5 weekly", "≥ 10 weekly", "≥ 25 weekly", "≥ 50 weekly", "≥ 100 weekly", "≥ 250 weekly",
  "≥ 5 total", "≥ 10 total", "≥ 25 total", "≥ 50 total", "≥ 100 total", "≥ 500 total", "≥ 1000 total",
  "≥ .5 per 10K", "≥ 1 per 10K", "≥ 1.5 per 10K", "≥ 2.5 per 10K", "≥ 5 per 10K", "≥ 7.5 per 10K"
)

## pooled outbreak-prone
pooled_df <- csv1 %>%
  mutate(
    population_group = case_when(
      pop_brk == "Administrative units with <50,000 people" ~ "less than 50k",
      pop_brk == "Administrative units with 50,000 to 500,000 people" ~ "50k to 500k",
      pop_brk == "Administrative units with ≥500,000 people" ~ "over 500k",
      TRUE ~ NA_character_
    ),
    alert_label = trimws(`Alert Definition`),
    transmission_setting = factor(
      "Pooled",
      levels = c("Pooled", "Country\nStratified")
    )
  ) %>%
  rename(utility = `Utility Score`) %>%
  group_by(population_group) %>%
  arrange(desc(utility), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  filter(rank %in% c(1,2,3)) %>%
  mutate(
    population_group = factor(population_group, levels = population_groups),
    alert_label = factor(alert_label, levels = custom_order),
    rank = factor(rank, levels = c(1,2,3))
  )

## country stratified 
country_df <- csv2 %>%
  rename(
    population_group = pop_brk,
    alert_label = alert_lab,
    n_countries_top3 = n_countries
  ) %>%
  mutate(
    transmission_setting = factor(
      "Country\nStratified",
      levels = c("Pooled", "Country\nStratified")
    ),
    alert_label = trimws(alert_label),
    population_group = factor(population_group, levels = population_groups),
    alert_label = factor(alert_label, levels = custom_order),
    n_countries_bin = case_when(
      n_countries_top3 == 1 ~ "1",
      n_countries_top3 >= 2 & n_countries_top3 <= 4 ~ "2-4",
      n_countries_top3 >= 5 & n_countries_top3 <= 7 ~ "5-7",
      n_countries_top3 >= 8 & n_countries_top3 <= 10 ~ "8-10",
      TRUE ~ NA_character_
    ),
    n_countries_bin = factor(n_countries_bin, levels = c("8-10","5-7","2-4","1")) 
  )

rank_colors <- c(
  "3" = "#d1c4e9",  
  "2" = "#9c27b0",  
  "1" = "#4a148c"   
)

country_colors <- c(
  "1" = "#d9f0d3",
  "2-4" = "#78c679",
  "5-7" = "#238443",
  "8-10" = "#203618"
)

n_rows <- length(custom_order)

p <- ggplot() +
  ## pooled outbreak-prone
  geom_tile(
    data = pooled_df,
    aes(x = population_group, y = alert_label, fill = rank),
    color = "white",
    width = 0.9
  ) +
  scale_fill_manual(
    values = rank_colors,
    na.value = "#D9D9D9",
    name = "Alert\nRank"
  ) +
  ggnewscale::new_scale_fill() +
  ## country stratified
  geom_tile(
    data = country_df,
    aes(x = population_group, y = alert_label, fill = n_countries_bin),
    color = "white",
    width = 0.9
  ) +
  scale_fill_manual(
    values = country_colors,
    name = "N Countries\nin Top 3",
    drop = FALSE,
    na.value = "#D9D9D9"
  ) +
  
  ## horizontal row borders
  geom_hline(
    yintercept = seq(0.5, n_rows + 0.5, by = 1),
    color = "grey85",
    linewidth = 0.4
  ) +
  facet_wrap(~transmission_setting, nrow = 1) +
  scale_y_discrete(limits = rev(custom_order), drop = FALSE) +
  labs(x = "Population Size", y = "Alert Definition") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.key.size = unit(0.5, "cm"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

ggsave(out_path, plot = p, width = 6, height = 4.5, dpi = 500)
message("Saved figure to: ", out_path)
save_alt_formats(out_path, res = 500)   # out_path = .../Figure4.png

## final version, map figure 5
slope_dir <- "../../notebooks/manuscript_figures/"

slope_files <- list.files(
  slope_dir,
  pattern = "slope_summaries_.*\\.rds$",
  full.names = TRUE
)

slopes_df <- purrr::map_dfr(slope_files, function(f) {
  df <- readRDS(f)
  
  pop_group <- basename(f) %>%
    stringr::str_extract("(under_\\d+K|\\d+Kto\\d+K|over_\\d+K)(?=_)")
  
  df %>%
    mutate(pop_group = pop_group)
})

slopes_df <- slopes_df %>%
  filter(level %in% c("global", "country", "country_raw")) %>%
  mutate(
    pop_group = case_when(
      pop_group %in% c("under_50K") ~ "<50k",
      pop_group %in% c("50Kto500K", "50kto500k") ~ "[50k, 500k)",
      pop_group %in% c("over_500K") ~ "≥500k",
      TRUE ~ pop_group
    ),
    pop_group = factor(pop_group, levels = c("<50k", "[50k, 500k)", "≥500k"))
  )

global_medians <- slopes_df %>%
  filter(level == "global") %>%
  select(pop_group, median) %>%
  rename(global_median = median)

country_raw <- slopes_df %>%
  filter(level == "country_raw") %>%
  select(country, pop_group, median, q5, q95) %>%
  mutate(significant = !(q5 <= 0 & q95 >= 0))

country_slopes <- slopes_df %>%
  filter(level == "country") %>%
  left_join(global_medians, by = "pop_group") %>%
  mutate(perc_change = median * 100) %>%
  left_join(
    country_raw %>% select(country, pop_group, significant),
    by = c("country", "pop_group")
  )

africa <- rnaturalearth::ne_countries(
  continent = "Africa",
  scale = "large",
  returnclass = "sf"
) %>%
  st_make_valid()

## fix Somalia + -99 artifact 
somalia <- africa %>%
  filter(iso_a3 == "SOM")

unknown <- africa %>%
  filter(iso_a3 == "-99")

## only keep -99 geometries that touch Somalia
unknown_touching_somalia <- unknown[
  st_intersects(unknown, somalia, sparse = FALSE),
]

somalia_clean <- rbind(somalia, unknown_touching_somalia) %>%
  st_make_valid() %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(iso_a3 = "SOM")

africa <- africa %>%
  filter(!(iso_a3 %in% c("SOM", "-99"))) %>%
  bind_rows(somalia_clean)

africa_bbox <- sf::st_bbox(
  c(xmin = -20, xmax = 55, ymin = -35, ymax = 38),
  crs = sf::st_crs(africa)
)

africa <- sf::st_crop(africa, africa_bbox)

pop_levels <- levels(slopes_df$pop_group)
all_countries <- africa$iso_a3

full_grid <- expand.grid(
  iso_a3 = all_countries,
  pop_group = pop_levels,
  stringsAsFactors = FALSE
)

africa_slopes_full <- full_grid %>%
  left_join(country_slopes, by = c("iso_a3" = "country", "pop_group")) %>%
  left_join(africa, by = "iso_a3") %>%
  st_as_sf()

africa_slopes_full <- africa_slopes_full %>%
  mutate(
    significant = ifelse(!is.na(perc_change) & significant, TRUE, FALSE),
    pop_group = factor(pop_group, levels = c("<50k", "[50k, 500k)", "≥500k"))
  )

midpoint_val <- -20
max_dev <- max(abs(africa_slopes_full$perc_change - midpoint_val), na.rm = TRUE)
fill_limits <- c(midpoint_val - max_dev, midpoint_val + max_dev)

p_map <- ggplot(africa_slopes_full) +
  geom_sf(aes(fill = perc_change), color = "gray70", linewidth = 0.3) +
  
  geom_sf(
    data = subset(africa_slopes_full, significant),
    fill = NA,
    color = "black",
    linewidth = 0.5
  ) +
  
  scale_fill_gradient2(
    low = "darkred",
    mid = "white",
    high = "darkblue",
    midpoint = midpoint_val,
    limits = fill_limits,
    na.value = "lightgray",
    name = "Change per week (%)"
  ) +
  
  annotation_scale(
    data = data.frame(pop_group = factor("≥500k", levels = pop_levels)),
    location = "bl", width_hint = 0.25, text_cex = 0.6
  ) +
  annotation_north_arrow(
    data = data.frame(pop_group = factor("<50k", levels = pop_levels)),
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(0.8, "cm"),
    width = unit(0.8, "cm")
  ) +
  
  facet_wrap(~pop_group) +
  
  theme_classic(base_size = 20) +
  theme(
    legend.position = "bottom",
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    strip.text = element_text(face = "bold")
  )

out_path <- "../../notebooks/manuscript_figures/Figure5.png"

ggsave(
  out_path,
  plot = p_map,
  width = 12,
  height = 7,
  dpi = 500
)

message("Saved figure 5 map to: ", out_path)
save_alt_formats(out_path, res = 500)   