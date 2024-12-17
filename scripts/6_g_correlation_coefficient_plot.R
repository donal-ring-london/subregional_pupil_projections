
## making that dotplot for correlation coefficients

## 0. libraries and functions
source("scripts/0_a_inputs.R")

library(data.table)
library(extrafont)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)



## 1. reading in data

births_r <- fread("data/processed_data/combined/births_reception_lag4_2023.csv")

births_yr1 <- fread("data/processed_data/combined/births_year_1_lag5_2023.csv")


## 2. calculating the correlation coefficients

births_r_cors <-  births_r[, .(cor_coef = cor(annual_births_lag4, headcount)),
                          by = list(itl_cd, itl_name)]

births_yr1_cors <- births_yr1[, .(cor_coef = cor(annual_births_lag5, headcount)),
                            by = list(itl_cd, itl_name)]

births_r_cors[, mean(cor_coef)]

births_yr1_cors[, mean(cor_coef)]


## 3. making the plot

  ### 3.1. setting the colours and the axis limits
r_col <- rgb(0, 87, 125, alpha = 100, maxColorValue = 255)
yr1_col <- rgb(158, 0, 89, alpha = 100, maxColorValue = 255)

xmin <- min(c(births_r_cors[, cor_coef], births_yr1_cors[, cor_coef]))

xmax <- max(c(births_r_cors[, cor_coef], births_yr1_cors[, cor_coef]))


  ### 3.2. setting the par parameters

svg(filename = "plots/adhoc_plots/correlation_coefficients.svg", 
    height = 6, width = 10)

par(mar = c(5, 5, 4, 2))
par(family = "Arial")
par(fg = "grey")

  ### 3.3. making the final plot
plot(x = 1, y = 1, type = "n", las = 1, bty = "n", yaxt = "n",
     xlim = c(xmin, xmax), ylim = c(0.5, 2.5), xlab = "", ylab = "")

grid(nx = NULL, ny = NA, col = "lightgray", lty = "solid")

points(x = births_r_cors[, cor_coef], y = jitter(rep(1, 43), amount = 0.25),
       pch = 16, cex = 5, col = r_col) # plotting reception dots

points(x = births_yr1_cors[, cor_coef], y = jitter(rep(2, 43), amount = 0.25),
       pch = 16,  cex = 5, col = yr1_col) # plotting year 1 dots

axis(side = 2, at = c(0.75, 1, 2, 2.25), labels = c("" , "Reception", "Year 1", ""), 
     tick = FALSE, las = 1)

mtext(side = 3, line = 2, adj = 0, cex = 1.3, font = 2, 
      text = "Correlation coefficients between births and Reception/Year 1 headcount", col = "black")

mtext(side = 3, line = 1, adj = 0, cex = 1, 
      text = "All ITL 2 Subregions, ITL 1 Regions, and Total England", col = "black")

dev.off()

