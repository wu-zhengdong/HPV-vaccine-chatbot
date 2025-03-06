# Fig. 2 | Stratified generalized estimating equation (GEE) to compare HPV vaccine receipt 
# or scheduled appointment of two arms.

library(forestplot)
library(forestploter)
library(grid)
library(dplyr)
library(readxl)
library(ggplot2)

# Load and prepare data
dt <- read_excel("./data/Fig2_hpv_vaccine_uptake_forestplot_data.xlsx")

# Format subgroup labels with proper indentation
dt$Subgroup <- ifelse(is.na(dt$intervention), 
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# Data preprocessing
dt$intervention <- ifelse(is.na(dt$intervention), "", dt$intervention)
dt$control <- ifelse(is.na(dt$control), "", dt$control)
dt$`p-val` <- ifelse(is.na(dt$`p-val`), "", dt$`p-val`)
dt$se <- (log(dt$hi) - log(dt$est))/1.96

# Format display columns for forest plot
dt$` ` <- paste(rep(" ", 20), collapse = " ")
dt$`Adjusted RR (95% CI)` <- ifelse(is.na(dt$se), "",
                                    sprintf("%.2f (%.2f-%.2f)",
                                            dt$est, dt$low, dt$hi))
dt$"P value" <- dt$`p-val`

# Handle special cases
dt$`Adjusted RR (95% CI)`[7] <- "           -"
dt$"P value"[7] <- "     -"

# Set column headers
names(dt)[names(dt) == "intervention"] <- "Chatbot\n n/N (%)"
names(dt)[names(dt) == "control"] <- "Usual care\n  n/N (%)"
names(dt)[names(dt) == "Subgroup"] <- ""

# Define forest plot theme
tm <- forest_theme(base_size = 10,
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 1,
                   ci_Theight = 0.2,
                   refline_gp = gpar(col = "red", lty = 2, fontface = "italic"),
                   arrow_type = "open",
                   footnote_gp = gpar(col = "blue", cex = 0.6, fontface = "italic")
)

# Create initial forest plot
p <- forest(dt[,c(1, 5, 6, 9, 10, 11)],
            est = dt$est,
            lower = dt$low,
            upper = dt$hi,
            ci_column = 4,
            ref_line = 1,
            xlim = c(0, 12),
            ticks_at = c(0, 1, 5, 10),
            theme = tm)

# Apply formatting
g <- edit_plot(p, row = c(2,6,9,12,15,19,22), gp = gpar(fontface = "bold"))
g <- edit_plot(g, row = c(1:26), which = "background", gp = gpar(fill = "white"))
g <- edit_plot(g, part = "header", row = 1, 
               gp = gpar(fontface=4, hjust = 0.5, wjust=0.5))

# Add header text and formatting
g <- insert_text(g,
                 text = "Number of participants",
                 col = 2:3,
                 row = 1,
                 just = "center",
                 part = "header",
                 gp = gpar(fontface = "bold"))

# Add header borders
g <- add_border(g, part = "header", row = 1, where = "top")
g <- add_border(g, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
g <- add_border(g, part = "header", row = 1, col = 2:3, gp = gpar(lwd = 2))

# Create Figures directory if it doesn't exist
if (!dir.exists("./Figures")) {
  dir.create("./Figures")
}

# Save plots in high resolution
ggsave("./Figures/Fig2.pdf", plot = g, device = "pdf", width = 9, height = 7, units = "in")
ggsave("./Figures/Fig2.png", plot = g, width = 8.5, height = 7, units = "in", dpi = 600)

plot(g)
