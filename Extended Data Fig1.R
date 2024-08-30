library(forestplot)
library(forestploter)
library(grid)
library(dplyr)
library(readxl)
library(ggplot2)

dt <- read_excel("./healthcare_consultation_subgroup_forestplot_data_aRR.xlsx")

dt$Subgroup <- ifelse(is.na(dt$intervention), 
                      dt$Subgroup,
                      paste0("   ", dt$Subgroup))

# NA to blank
dt$intervention <- ifelse(is.na(dt$intervention), "", dt$intervention)
dt$control <- ifelse(is.na(dt$control), "", dt$control)
dt$`p-val` <- ifelse(is.na(dt$`p-val`), "", dt$`p-val`)
dt$se <- (log(dt$hi) - log(dt$est))/1.96

# Add a blank column for the forest plot to display CI.
# Adjust the column width with space. 
dt$` ` <- paste(rep(" ", 20), collapse = " ")

# Create confidence interval column to display
dt$`Adjusted RR (95% CI)` <- ifelse(is.na(dt$se), "",
                                    sprintf("%.2f (%.2f-%.2f)",
                                            dt$est, dt$low, dt$hi))

dt$"P value" <- dt$`p-val`

#dt$`Adjusted RR (95% CI)`[7] <- "-"
#dt$"P value"[7] <- "-"

names(dt)[names(dt) == "intervention"] <- "Chatbot\n n/N (%)"
names(dt)[names(dt) == "control"] <- "Usual care\n   n/N (%)"
names(dt)[names(dt) == "Subgroup"] <- ""
#names(dt)[names(dt) == "intervention"] <- ""
#names(dt)[names(dt) == "control"] <- ""
names(dt)[names(dt) == "Adjusted RR (95% CI)"] <- "Adjusted RR (95% CI)"


# Define theme
tm <- forest_theme(base_size = 10,
                   ci_alpha = 1,
                   ci_lty = 1,
                   ci_lwd = 1,
                   ci_Theight = 0.2,
                   refline_gp = gpar(col = "red", lty = 2, fontface = "italic"),
                   arrow_type = "open",
                   footnote_gp = gpar(col = "blue", cex = 0.6, fontface = "italic"),
)

p <- forest(dt[,c(1, 5, 6, 9, 10, 11)],
            est = dt$est,
            lower = dt$low,
            upper = dt$hi,
            #sizes = dt$se,
            ci_column = 4,
            #ref_line = dt[1, 2]$est,
            ref_line = 1,
            #arrow_lab = c("Usual care better", "Chatbot better"),
            xlim = c(0.5, 5.5),
            ticks_at = c(1,2,3,4,5),
            #footnote = "This is the demo data. Please feel free to change\nanything you want.",
            theme = tm)

plot(p)

# Bold grouping text
g <- edit_plot(p,
               row = c(2,6,9,12,15,19,22),
               gp = gpar(fontface = "bold"))

# g <- edit_plot(g,
#                row = c(1),
#                col = c(1),
#                gp = gpar(fontface = "bold"))

# Edit the background
g <- edit_plot(g, row = c(1:26), which = "background",
               gp = gpar(fill = "white"))

# Edit the fontface of header
g <- edit_plot(g,
               part = "header",
               row = 1,
               gp = gpar(fontface=4, hjust = 0.5, wjust=0.5),
)

# Insert text at the top
g <- insert_text(g,
                 text = "Number of participants",
                 col = 2:3,
                 row = 1,
                 just = "center",
                 part = "header",
                 gp = gpar(fontface = "bold"))

# g <- insert_text(g,
#                  text = "Fig. 2 | Multivariable logistic regression to compare HBV and HCV test uptake rates of two arms.",
#                  col = 1:2,
#                  row = c(28),
#                  part = "body",
#                  just = "left",
#                  gp = gpar(cex = 0.5)
#                  )

# Add underline at the bottom of the header
g <- add_border(g, part = "header", row = 1, where = "top")
g <- add_border(g, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
g <- add_border(g, part = "header", row = 1, col = 2:3, 
                gp = gpar(lwd = 2))

#g <- add_border(g, part = "header", row = 29, where = "bottom")

#g <- add_border(g, part = "body", row = 1, where = "bottom")

# Assuming 'g' is your ggplot object
ggsave("./healthcare_consultation_subgroup_forestplot_data_aRR.pdf", plot = g, device = "pdf", width = 8, height = 7, units = "in")

# Assuming 'g' is your ggplot object
ggsave("./healthcare_consultation_subgroup_forestplot_data_aRR.png", plot = g, width = 9, height = 7, units = "in", dpi = 600)

# Print plot
plot(g)