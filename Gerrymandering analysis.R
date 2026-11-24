  # Gerrymandering Analysis: Five-State Comparison and Michigan Case Study
# Data source: ALARM Project
# Election years: 2016 and 2020 | District grouping: cd_2010
#
# Run from the directory containing all_states_combined.rds.
# Packages: tidyverse, sf, lwgeom, scales, knitr
# Outputs: PNG plots and summary tables in the current working directory.
#
# The analysis aggregates presidential two-party votes to districts;
# district winners and seat shares are proxies, not congressional results.
# Descriptive metrics do not establish intentional gerrymandering.
# Source converted from original R Markdown; not executed or validated here.

# ============================================================
# Gerrymandering Metrics Analysis (UPDATED — uses cd_2010)
# Overview: All 5 States | Deep Dive: Michigan
# Metrics: Efficiency Gap, Packed/Cracked, Partisan Bias, Declination
# ============================================================
library(tidyverse)
library(sf)
library(lwgeom)
library(scales)
library(knitr)
all_states <- readRDS("all_states_combined.rds")
df <- st_drop_geometry(all_states)


# ============================================================
# HELPER: Aggregate precinct data up to DISTRICT level
# All metrics operate at the district level, not precinct
# ============================================================
district_level <- df %>%
  group_by(state, cd_2010) %>%
  summarise(
    total_pop     = sum(pop, na.rm = TRUE),
    dem_votes_20  = sum(adv_20, na.rm = TRUE),
    rep_votes_20  = sum(arv_20, na.rm = TRUE),
    dem_votes_16  = sum(adv_16, na.rm = TRUE),
    rep_votes_16  = sum(arv_16, na.rm = TRUE),
    pop_white     = sum(pop_white, na.rm = TRUE),
    pop_black     = sum(pop_black, na.rm = TRUE),
    pop_hisp      = sum(pop_hisp, na.rm = TRUE),
    pop_asian     = sum(pop_asian, na.rm = TRUE),
    area_land     = sum(area_land, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_votes_20  = dem_votes_20 + rep_votes_20,
    dem_share_20    = dem_votes_20 / total_votes_20,
    rep_share_20    = rep_votes_20 / total_votes_20,
    total_votes_16  = dem_votes_16 + rep_votes_16,
    dem_share_16    = dem_votes_16 / total_votes_16,
    swing           = dem_share_20 - dem_share_16,
    pct_white       = pop_white / total_pop,
    pct_black       = pop_black / total_pop,
    pct_hisp        = pop_hisp  / total_pop,
    pct_nonwhite    = 1 - pct_white,
    # Who won each district
    winner_20       = ifelse(dem_share_20 > 0.5, "Democrat", "Republican"),
    # Wasted votes (efficiency gap calculation)
    wasted_dem      = ifelse(winner_20 == "Democrat",
                             dem_votes_20 - (total_votes_20 / 2),  # surplus
                             dem_votes_20),                          # all lost
    wasted_rep      = ifelse(winner_20 == "Republican",
                             rep_votes_20 - (total_votes_20 / 2),  # surplus
                             rep_votes_20),                          # all lost
    # Packed/cracked classification
    district_type   = case_when(
      dem_share_20 > 0.70                        ~ "Packed Dem",
      dem_share_20 < 0.30                        ~ "Packed Rep",
      dem_share_20 >= 0.45 & dem_share_20 <= 0.55 ~ "Competitive",
      dem_share_20 > 0.55 & dem_share_20 <= 0.70  ~ "Lean Dem",
      dem_share_20 >= 0.30 & dem_share_20 < 0.45  ~ "Lean Rep",
      TRUE ~ NA_character_
    )
  )


# ============================================================
# METRIC 1: EFFICIENCY GAP (by state)
# Positive = benefits Republicans, Negative = benefits Democrats
# Threshold of concern: |EG| > 0.08 (8%)
# ============================================================
efficiency_gap <- district_level %>%
  group_by(state) %>%
  summarise(
    total_wasted_dem = sum(wasted_dem, na.rm = TRUE),
    total_wasted_rep = sum(wasted_rep, na.rm = TRUE),
    total_votes      = sum(total_votes_20, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    efficiency_gap = (total_wasted_dem - total_wasted_rep) / total_votes,
    favors         = case_when(
      efficiency_gap >  0.08 ~ "Favors Republicans",
      efficiency_gap < -0.08 ~ "Favors Democrats",
      TRUE                   ~ "Roughly Fair"
    )
  )

# Plot 1: Efficiency Gap by State
p1 <- ggplot(efficiency_gap, aes(x = reorder(state, efficiency_gap),
                                  y = efficiency_gap, fill = favors)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept =  0.08, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = -0.08, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0,     linetype = "solid",  color = "black") +
  annotate("text", x = 0.6, y = 0.10, label = "Republican advantage threshold",
           hjust = 0, size = 3, color = "grey40") +
  annotate("text", x = 0.6, y = -0.10, label = "Democrat advantage threshold",
           hjust = 0, size = 3, color = "grey40") +
  scale_fill_manual(values = c("Favors Republicans" = "#d6604d",
                                "Favors Democrats"   = "#4393c3",
                                "Roughly Fair"       = "#aaaaaa")) +
  scale_y_continuous(labels = percent) +
  labs(
    title    = "Efficiency Gap by State (2020)",
    subtitle = "Measures wasted votes — values beyond ±8% indicate potential gerrymandering",
    x        = "State", y = "Efficiency Gap", fill = NULL,
    caption  = "Source: ALARM Project | Positive = favors Republicans"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

ggsave("plot1_efficiency_gap.png", p1, width = 9, height = 6, dpi = 150)
cat("✓ Plot 1 saved: Efficiency Gap by State\n")


# ============================================================
# METRIC 2: PACKED vs CRACKED DISTRICTS (all states)
# ============================================================
packed_cracked <- district_level %>%
  count(state, district_type) %>%
  filter(!is.na(district_type))

p2 <- ggplot(packed_cracked, aes(x = state, y = n, fill = district_type)) +
  geom_col(position = "fill", width = 0.6) +
  scale_fill_manual(values = c(
    "Packed Dem"  = "#2166ac",
    "Lean Dem"    = "#92c5de",
    "Competitive" = "#f7f7f7",
    "Lean Rep"    = "#f4a582",
    "Packed Rep"  = "#d6604d"
  )) +
  scale_y_continuous(labels = percent) +
  labs(
    title    = "District Classification by State (2020)",
    subtitle = "Vote shares above 70% or below 30% are classified as packed for this analysis",
    x        = "State", y = "Share of Districts", fill = "District Type",
    caption  = "Source: ALARM Project"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "right")

ggsave("plot2_packed_cracked.png", p2, width = 9, height = 6, dpi = 150)
cat("✓ Plot 2 saved: Packed vs Cracked Districts\n")


# ============================================================
# METRIC 3: SEAT–VOTE DIFFERENCE (all states)
# If a party wins 50% of votes, do they win ~50% of seats?
# ============================================================
seats_votes <- district_level %>%
  group_by(state) %>%
  summarise(
    total_districts = n(),
    dem_seats       = sum(winner_20 == "Democrat",   na.rm = TRUE),
    rep_seats       = sum(winner_20 == "Republican", na.rm = TRUE),
    dem_vote_share  = sum(dem_votes_20, na.rm = TRUE) /
                      sum(total_votes_20, na.rm = TRUE),
    dem_seat_share  = dem_seats / total_districts,
    .groups = "drop"
  ) %>%
  mutate(
    partisan_bias = dem_seat_share - dem_vote_share,
    favors        = case_when(
      partisan_bias >  0.05 ~ "Favors Democrats",
      partisan_bias < -0.05 ~ "Favors Republicans",
      TRUE                  ~ "Roughly Proportional"
    )
  )

p3 <- ggplot(seats_votes, aes(x = dem_vote_share, y = dem_seat_share, color = favors)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 5) +
  geom_text(aes(label = state), vjust = -1, size = 4, fontface = "bold") +
  scale_x_continuous(labels = percent, limits = c(0, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_color_manual(values = c("Favors Republicans"     = "#d6604d",
                                 "Favors Democrats"       = "#4393c3",
                                 "Roughly Proportional"   = "#aaaaaa")) +
  annotate("text", x = 0.63, y = 0.61, label = "Proportional line",
           color = "grey50", size = 3.5, angle = 42) +
  labs(
    title    = "Seats vs Votes: Seat–Vote Difference (2020)",
    subtitle = "Points above the line indicate a larger district share than vote share",
    x        = "Democratic Vote Share", y = "Democratic Seat Share", color = NULL,
    caption  = "Source: ALARM Project"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

ggsave("plot3_seats_vs_votes.png", p3, width = 9, height = 6, dpi = 150)
cat("✓ Plot 3 saved: Seats vs Votes\n")


# ============================================================
# MICHIGAN DEEP DIVE
# ============================================================
mi_districts <- district_level %>% filter(state == "MI")

# --- MI Plot 1: Vote Share Distribution (Declination visual) ---
# Declination: asymmetry between how Dem/Rep wins are distributed
# Packed party wins by huge margins; cracked party wins narrowly

p4 <- ggplot(mi_districts %>% filter(!is.na(dem_share_20)),
             aes(x = reorder(as.factor(cd_2010), dem_share_20),
                 y = dem_share_20, fill = winner_20)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("Democrat" = "#4393c3", "Republican" = "#d6604d")) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title    = "Michigan Districts: Democratic Vote Share by District (2020)",
    subtitle = "Sorted by two-party vote share",
    x        = "District (sorted by Dem vote share)",
    y        = "Democratic Vote Share", fill = "Winner",
    caption  = "Source: ALARM Project"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.x = element_blank())

ggsave("plot4_MI_vote_distribution.png", p4, width = 10, height = 6, dpi = 150)
cat("✓ Plot 4 saved: Michigan Vote Distribution\n")


# --- MI Plot 2: % Black Population vs Vote Share (packing test) ---
p5 <- ggplot(mi_districts %>% filter(!is.na(dem_share_20), !is.na(pct_black)),
             aes(x = pct_black, y = dem_share_20, color = winner_20)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  geom_vline(xintercept = 0.50, linetype = "dotted", color = "grey50") +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c("Democrat" = "#4393c3", "Republican" = "#d6604d")) +
  labs(
    title    = "Michigan: % Black Population vs Democratic Vote Share (2020)",
    subtitle = "District-level association; not evidence of intent on its own",
    x        = "% Black Population", y = "Democratic Vote Share", color = "Winner",
    caption  = "Source: ALARM Project"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("plot5_MI_black_packing.png", p5, width = 9, height = 6, dpi = 150)
cat("✓ Plot 5 saved: Michigan Black Population Packing\n")


# --- MI Plot 3: 2016 vs 2020 Swing by District ---
p6 <- ggplot(mi_districts %>% filter(!is.na(dem_share_16), !is.na(dem_share_20)),
             aes(x = dem_share_16, y = dem_share_20, color = winner_20)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_text(aes(label = cd_2010), vjust = -0.8, size = 3) +
  scale_x_continuous(labels = percent, limits = c(0, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_color_manual(values = c("Democrat" = "#4393c3", "Republican" = "#d6604d")) +
  annotate("text", x = 0.75, y = 0.72, label = "No change",
           color = "grey50", size = 3.5, angle = 42) +
  labs(
    title    = "Michigan: District-Level Swing 2016 → 2020",
    subtitle = "Points above the line shifted more Democratic; labels = district number",
    x        = "Dem Vote Share 2016", y = "Dem Vote Share 2020", color = "2020 Winner",
    caption  = "Source: ALARM Project"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("plot6_MI_swing.png", p6, width = 9, height = 6, dpi = 150)
cat("✓ Plot 6 saved: Michigan 2016 vs 2020 Swing\n")


# ============================================================
# PRINT SUMMARY TABLES
# ============================================================
cat("\n--- EFFICIENCY GAP SUMMARY ---\n")
print(efficiency_gap %>% select(state, efficiency_gap, favors))

cat("\n--- SEATS VS VOTES SUMMARY ---\n")
print(seats_votes %>% select(state, dem_vote_share, dem_seat_share, partisan_bias, favors))

cat("\n--- MICHIGAN DISTRICT CLASSIFICATIONS ---\n")
print(mi_districts %>% select(cd_2010, dem_share_20, winner_20, district_type) %>%
arrange(dem_share_20))
# PLOT A: COMPACTNESS

sf_use_s2(FALSE)
district_geo <- all_states %>%
  st_make_valid() %>%
  group_by(state, cd_2010) %>%
  summarise(.groups = "drop") %>%
  st_make_valid() %>%
  st_transform(5070) %>%
  mutate(
    area      = as.numeric(st_area(geometry)),
    perimeter = as.numeric(st_perimeter(geometry)),
    pp_score  = (4 * pi * area) / perimeter^2
  ) %>%
  st_drop_geometry()

state_order <- district_geo %>%
  group_by(state) %>%
  summarise(mean_pp = mean(pp_score, na.rm = TRUE)) %>%
  arrange(mean_pp) %>%
  pull(state)

pA <- ggplot(district_geo,
             aes(x = factor(state, levels = state_order), y = pp_score)) +
  geom_jitter(aes(color = pp_score), width = 0.15, size = 2.5, alpha = 0.7) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5,
               color = "grey20", linewidth = 0.6) +
  scale_color_gradient(low = "#d6604d", high = "#4393c3",
                       name = "Compactness") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title    = "District Compactness by State (Polsby-Popper)",
    subtitle = "Score near 1 = circle-like; near 0 = irregular shape. Crossbar = state mean.",
    x        = "State (sorted by mean compactness)",
    y        = "Polsby-Popper Score",
    caption  = "Source: ALARM Project"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("plotA_compactness.png", pA, width = 9, height = 6, dpi = 150)
cat("✓ Plot A saved\n")


# PLOT B: EXPLORATORY DECLINATION
# Original formula retained; verify against the standard definition before
# interpreting these values as a validated declination metric.

decl_input <- district_level %>%
  filter(!is.na(dem_share_20)) %>%
  group_by(state) %>%
  arrange(dem_share_20) %>%
  mutate(x_pos = row_number() / n()) %>%
  ungroup()

decl_summary <- decl_input %>%
  group_by(state) %>%
  summarise(
    yD = mean(dem_share_20[winner_20 == "Democrat"],   na.rm = TRUE),
    xD = mean(x_pos[winner_20 == "Democrat"],          na.rm = TRUE),
    yR = mean(dem_share_20[winner_20 == "Republican"], na.rm = TRUE),
    xR = mean(x_pos[winner_20 == "Republican"],        na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    delta  = (2 / pi) * (atan((0.5 - yR) / (1 - xR)) - atan((yD - 0.5) / xD)),
    label  = paste0("δ = ", round(delta, 3))
  )

pB <- ggplot(decl_input,
             aes(x = x_pos, y = dem_share_20, color = winner_20)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey40") +
  geom_segment(data = decl_summary,
               aes(x = xR, xend = 0.5, y = yR, yend = 0.5),
               color = "#d6604d", linewidth = 1, inherit.aes = FALSE) +
  geom_segment(data = decl_summary,
               aes(x = xD, xend = 0.5, y = yD, yend = 0.5),
               color = "#4393c3", linewidth = 1, inherit.aes = FALSE) +
  geom_text(data = decl_summary,
            aes(x = 0.5, y = 0.95, label = label),
            inherit.aes = FALSE, size = 3.5, fontface = "bold",
            color = "grey20") +
  facet_wrap(~ state, nrow = 2) +
  scale_color_manual(values = c("Democrat" = "#4393c3",
                                "Republican" = "#d6604d")) +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title    = "Declination by State (2020)",
    subtitle = "Lines show average win angle per party — gap between them indicates asymmetry\nPositive δ favors Republicans, negative δ favors Democrats",
    x        = "Districts sorted by Dem vote share →",
    y        = "Democratic Vote Share",
    color    = NULL,
    caption  = "Source: ALARM Project"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top",
        strip.text = element_text(face = "bold"))

ggsave("plotB_declination.png", pB, width = 12, height = 7, dpi = 150)
cat("✓ Plot B saved\n")


# PLOT C: SWING VS OUTCOME

swing_data <- district_level %>%
  filter(!is.na(swing)) %>%
  mutate(
    margin   = abs(dem_share_20 - 0.5),
    category = case_when(
      dem_share_16 < 0.5 & dem_share_20 >= 0.5 ~ "Flipped Dem",
      dem_share_16 > 0.5 & dem_share_20 <= 0.5 ~ "Flipped Rep",
      winner_20 == "Democrat"                   ~ "Held Dem",
      TRUE                                      ~ "Held Rep"
    )
  )

pC <- ggplot(swing_data,
             aes(x = swing, y = margin, color = category)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.5, alpha = 0.75) +
  scale_color_manual(
    values = c("Flipped Dem" = "#1a6faf", "Flipped Rep" = "#b2182b",
               "Held Dem"    = "#92c5de", "Held Rep"    = "#f4a582"),
    name = NULL
  ) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  facet_wrap(~ state, nrow = 2) +
  labs(
    title    = "Swing vs Margin by State (2016 → 2020)",
    subtitle = "x = how much a district shifted Democratic | y = how safe the seat was in 2020\nDescriptive comparison of vote-share changes and margins",
    x        = "Swing 2016 → 2020",
    y        = "Distance from 50% (2020)",
    caption  = "Source: ALARM Project"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top",
        strip.text = element_text(face = "bold"))

ggsave("plotC_swing.png", pC, width = 12, height = 7, dpi = 150)
cat("✓ Plot C saved\n")
# ============================================================
# TABLE 1: COMPETITIVENESS
# ============================================================
competitiveness_table <- district_level %>%
  mutate(margin = abs(dem_share_20 - 0.5)) %>%
  group_by(state) %>%
  summarise(
    total_districts = n(),
    competitive_n   = sum(dem_share_20 >= 0.45 & dem_share_20 <= 0.55, na.rm = TRUE),
    pct_competitive = round(competitive_n / total_districts, 3),
    avg_margin      = round(mean(margin, na.rm = TRUE), 3),
    .groups = "drop"
  )

kable(competitiveness_table, caption = "District Competitiveness by State")


# ============================================================
# TABLE 2: VOTE EFFICIENCY
# ============================================================
vote_efficiency_table <- district_level %>%
  group_by(state) %>%
  summarise(
    dem_seats = sum(winner_20 == "Democrat", na.rm = TRUE),
    rep_seats = sum(winner_20 == "Republican", na.rm = TRUE),
    total_dem_votes = sum(dem_votes_20, na.rm = TRUE),
    total_rep_votes = sum(rep_votes_20, na.rm = TRUE),
    votes_per_dem_seat = round(total_dem_votes / dem_seats, 0),
    votes_per_rep_seat = round(total_rep_votes / rep_seats, 0),
    efficiency_ratio   = round(votes_per_dem_seat / votes_per_rep_seat, 2),
    .groups = "drop"
  )

kable(vote_efficiency_table, caption = "Vote Efficiency by State")


# ============================================================
# TABLE 3: SEAT BONUS
# ============================================================
seat_bonus_table <- district_level %>%
  group_by(state) %>%
  summarise(
    total_districts = n(),
    dem_vote_share  = sum(dem_votes_20, na.rm = TRUE) /
                      sum(total_votes_20, na.rm = TRUE),
    expected_dem_seats = round(dem_vote_share * total_districts, 1),
    actual_dem_seats   = sum(winner_20 == "Democrat", na.rm = TRUE),
    seat_bonus         = round(actual_dem_seats - expected_dem_seats, 1),
    .groups = "drop"
  )

kable(seat_bonus_table, caption = "Seat Bonus by State")
# Individual Efficiency Gap plots per state — clean presentation style

for (s in unique(district_level$state)) {

  state_eg <- district_level %>%
    filter(state == s) %>%
    mutate(
      wasted_dem = ifelse(winner_20 == "Democrat",
                          dem_votes_20 - (total_votes_20 / 2),
                          dem_votes_20),
      wasted_rep = ifelse(winner_20 == "Republican",
                          rep_votes_20 - (total_votes_20 / 2),
                          rep_votes_20)
    ) %>%
    summarise(
      total_wasted_dem = sum(wasted_dem, na.rm = TRUE),
      total_wasted_rep = sum(wasted_rep, na.rm = TRUE),
      total_votes      = sum(total_votes_20, na.rm = TRUE)
    ) %>%
    mutate(
      efficiency_gap = (total_wasted_dem - total_wasted_rep) / total_votes,
      favors = case_when(
        efficiency_gap >  0.08 ~ "Favors Republicans",
        efficiency_gap < -0.08 ~ "Favors Democrats",
        TRUE                   ~ "Roughly Fair"
      ),
      bar_color = case_when(
        efficiency_gap >  0.08 ~ "#d6604d",
        efficiency_gap < -0.08 ~ "#4393c3",
        TRUE                   ~ "#aaaaaa"
      )
    )

  eg_val  <- state_eg$efficiency_gap
  eg_pct  <- paste0(ifelse(eg_val > 0, "+", ""), round(eg_val * 100, 1), "%")
  favors  <- state_eg$favors
  bar_col <- state_eg$bar_color

  p <- ggplot(state_eg, aes(x = "", y = eg_val)) +
    # shaded neutral zone
    annotate("rect", xmin = 0.5, xmax = 1.5, ymin = -0.08, ymax = 0.08,
             fill = "#f5f5f5", alpha = 0.8) +
    # bar
    geom_col(width = 0.35, fill = bar_col) +
    # threshold lines
    geom_hline(yintercept =  0.08, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_hline(yintercept = -0.08, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_hline(yintercept =  0,    linetype = "solid",  color = "grey20", linewidth = 0.4) +
    # value label on bar
    geom_text(aes(y = eg_val, label = eg_pct),
              vjust = ifelse(eg_val >= 0, -0.6, 1.6),
              size = 5.5, fontface = "bold", color = bar_col) +
    # threshold labels
    annotate("text", x = 1.27, y =  0.085, label = "+8%", size = 2.8, color = "grey50") +
    annotate("text", x = 1.27, y = -0.085, label = "−8%", size = 2.8, color = "grey50") +
    scale_y_continuous(
      limits = c(min(-0.25, eg_val * 1.2), max(0.25, eg_val * 1.2)),
      labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x * 100, 0), "%")
    ) +
    labs(
      title    = s,
      subtitle = favors,
      x = NULL, y = NULL,
      caption  = "Efficiency Gap | ±8% = exploratory reference threshold"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title        = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle     = element_text(size = 10, hjust = 0.5, color = bar_col),
      plot.caption      = element_text(size = 8, color = "grey60", hjust = 0.5),
      axis.text.x       = element_blank(),
      axis.text.y       = element_text(size = 9, color = "grey50"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "grey90"),
      plot.background    = element_rect(fill = "white", color = NA),
      plot.margin        = margin(16, 20, 12, 20)
    )
  ggsave(paste0("plot_eg_", s, ".png"), p, width = 3.5, height = 5, dpi = 180)
  print(p)
  cat("✓ Saved:", s, "| EG:", eg_pct, "|", favors, "\n")
}
