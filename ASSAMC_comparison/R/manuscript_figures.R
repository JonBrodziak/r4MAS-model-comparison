library(vioplot)
library(scales)
# Set up working directory ------------------------------------------------

project_dir <- "C:/Users/bai.li/Documents/Github/r4MAS-model-comparison/ASSAMC_comparison/snapper_grouper/"
figure_folder_name <- "manuscript_figures"
figure_path <- file.path(project_dir, figure_folder_name)
dir.create(figure_path)

# Find Flow and Fhigh and plot yield over F figure -----------------------------------------------------
case_id <- "C1"
load(file.path(project_dir, case_id, "output", "OM", paste("OM", 1, ".RData", sep = "")))

ratio <- 0.8
msy_ratio <- ratio * max(om_output$msy$L_eq)
msy_points <- sort(abs(om_output$msy$L_eq - msy_ratio))[1:5]
f_points <- om_output$msy$f_seq[which(abs(om_output$msy$L_eq - msy_ratio) %in% msy_points)]
flow <- min(f_points)
fhigh <- max(f_points)

jpeg(file = file.path(figure_path, paste("yield_over_f.jpg", sep = "")), width = 105, height = 75, units = "mm", res = 1200)
par(mar = c(4, 5, 1, 1))
plot(om_output$msy$f_seq, om_output$msy$L_eq,
  xlab = "", ylab = "",
  type = "l",
  xlim = c(0, 1), ylim = c(0, max(om_output$msy$L_eq) * 1.1),
  xaxs = "i", yaxs = "i",
  axe = F
)
axis(
  side = 1,
  at = c(0, max(om_output$msy$f_seq)),
  labels = rep(NA, 2)
)
axis(
  side = 2,
  at = c(0, max(om_output$msy$L_eq)),
  labels = rep(NA, 2)
)
lines(x = c(0, om_output$msy$Fmsy), y = c(max(om_output$msy$L_eq), max(om_output$msy$L_eq)))
lines(x = c(0, fhigh), y = c(msy_ratio, msy_ratio), lty = 2, col = "gray30")
lines(x = c(om_output$msy$Fmsy, om_output$msy$Fmsy), y = c(0, max(om_output$msy$L_eq)))
lines(x = c(flow, flow), y = c(0, msy_ratio), lty = 2, col = "gray30")
lines(x = c(fhigh, fhigh), y = c(0, msy_ratio), lty = 2, col = "gray30")
axis(1, at = c(flow, om_output$msy$Fmsy, fhigh), labels = c(expression(italic(F)[low]), expression(italic(F)[MSY]), expression(italic(F)[high])), las = 2)
axis(2, at = c(om_output$msy$msy, msy_ratio), labels = c(expression(italic(MSY)), expression(italic(0.8 * MSY))), las = 1)
dev.off()

# Plot relative error in SSB, R, F, SSB/SSBMSY, and F/FMSY vioplot -------------------------------------------------------
case_id <- paste("C", 1:7, sep = "")
maindir_list <- paste(project_dir, case_id, sep = "")

em_num <- 1

ssb_re_list <- r_re_list <- f_re_list <- ssbratio_re_list <- fratio_re_list <- list()

for (j in 1:length(maindir_list)) {
  load(file.path(maindir_list[j], "output", "performance_measure.RData"))
  if (j == 1) {
    ssb_re <- r_re <- f_re <-
      ssbratio_re <- fratio_re <- matrix(NA, nrow = length(re_list), ncol = nrow(re_list[[1]]$ssb))
    ssb <- r <- f <- ssbratio <- fratio <- matrix(NA, nrow = length(re_list) * nrow(re_list[[1]]$ssb), ncol = em_num)
  }
  for (k in 1:em_num) {
    for (i in 1:length(re_list)) {
      ssb_re[i, ] <- as.matrix(re_list[[i]]$ssb[, k])
      r_re[i, ] <- as.matrix(re_list[[i]]$recruit[, k])
      f_re[i, ] <- as.matrix(re_list[[i]]$Ftot[, k])
      ssbratio_re[i, ] <- as.matrix(re_list[[i]]$ssbratio[, k])
      fratio_re[i, ] <- as.matrix(re_list[[i]]$fratio[, k])
    }
    ssb[, k] <- as.vector(ssb_re)
    r[, k] <- as.vector(r_re)
    f[, k] <- as.vector(f_re)
    ssbratio[, k] <- as.vector(ssbratio_re)
    fratio[, k] <- as.vector(fratio_re)
  }
  ssb_re_list[[j]] <- ssb
  r_re_list[[j]] <- r
  f_re_list[[j]] <- f
  ssbratio_re_list[[j]] <- ssbratio
  fratio_re_list[[j]] <- fratio
}

jpeg(file = file.path(figure_path, "violinSSB_R_F_RE.jpg"), width = 130, height = 150, units = "mm", res = 1200)

title_var <- c(
  expression(italic("SSB")),
  expression(italic("R")),
  expression(italic("F")),
  expression(italic("SSB/SSB")[MSY]),
  expression(italic("F/F")[MSY])
)

op <- par(no.readonly = TRUE)
par(op)
par(oma = c(5, 5, 0, 5), mar = c(0, 0, 0, 0.2), mfrow = c(length(case_id) + 1, length(title_var)), pch = 16, las = 1)

ylim <- c(-0.8, 0.8)
yaxis <- c(-0.4, 0, 0.4)
yaxis_label <- c(expression(paste("\u2013", "0.4")), expression("0.0"), expression("0.4"))
legend_x <- -0.2
legend_y <- 0.8

for (i in 1:length(title_var)) {
  plot(NA,
    type = "n",
    xlim = range(0:(em_num + 1)),
    ylim = ylim,
    axes = FALSE, ann = FALSE
  )
  legend("bottomleft",
    title_var[i],
    bty = "n",
    cex = 0.8
  )
}

for (i in 1:length(maindir_list)) {
  plot(NA,
    type = "n",
    xlim = range(0:(em_num + 1)),
    ylim = ylim,
    axes = FALSE, ann = FALSE
  )
  axis(2, at = yaxis, labels = yaxis_label, las = 2)
  vioplot(ssb_re_list[[i]][, 1], add = TRUE)
  abline(h = 0, col = "gray30", lty = 2)
  if (i == length(maindir_list)) axis(1, at = 1:em_num, labels = c("MAS"), las = 2, cex = 0.6)

  legend(x = legend_x, y = legend_y, paste("C", i, sep = ""), bty = "n", cex = 0.9)

  plot(NA,
    type = "n",
    xlim = range(0:(em_num + 1)),
    ylim = ylim,
    axes = FALSE, ann = FALSE
  )
  vioplot(r_re_list[[i]][, 1], add = TRUE)
  abline(h = 0, col = "gray30", lty = 2)
  if (i == length(maindir_list)) axis(1, at = 1:em_num, labels = c("MAS"), las = 2, cex = 0.6)

  legend(x = legend_x, y = legend_y, paste("C", i, sep = ""), bty = "n", cex = 0.9)

  plot(NA,
    type = "n",
    xlim = range(0:(em_num + 1)),
    ylim = ylim,
    axes = FALSE, ann = FALSE
  )
  vioplot(f_re_list[[i]][, 1], add = TRUE)
  abline(h = 0, col = "gray30", lty = 2)
  if (i == length(maindir_list)) axis(1, at = 1:em_num, labels = c("MAS"), las = 2, cex = 0.6)
  legend(x = legend_x, y = legend_y, paste("C", i, sep = ""), bty = "n", cex = 0.9)

  plot(NA,
    type = "n",
    xlim = range(0:(em_num + 1)),
    ylim = ylim,
    axes = FALSE, ann = FALSE
  )
  vioplot(ssbratio_re_list[[i]][, 1], add = TRUE)
  abline(h = 0, col = "gray30", lty = 2)
  if (i == length(maindir_list)) axis(1, at = 1:em_num, labels = c("MAS"), las = 2, cex = 0.6)
  legend(x = legend_x, y = legend_y, paste("C", i, sep = ""), bty = "n", cex = 0.9)

  plot(NA,
    type = "n",
    xlim = range(0:(em_num + 1)),
    ylim = ylim,
    axes = FALSE, ann = FALSE
  )
  vioplot(fratio_re_list[[i]][, 1], add = TRUE)
  abline(h = 0, col = "gray30", lty = 2)
  if (i == length(maindir_list)) axis(1, at = 1:em_num, labels = c("MAS"), las = 2, cex = 0.6)
  legend(x = legend_x, y = legend_y, paste("C", i, sep = ""), bty = "n", cex = 0.9)
}

par(las = 2)
mtext(text = "RE", side = 2, line = 2.5, outer = TRUE, cex = 0.7)

dev.off()
# Plot SSB, R, F, SSB/SSBMSY and F/FMSY over time ------------------

## Aggregate output data
case_id <- paste("C", 1:7, sep = "")
maindir_list <- paste(project_dir, case_id, sep = "")

em_num <- 1

load(file.path(maindir_list[1], "output", "om_output.RData"))
ssb_median <- ssb_low <- ssb_high <-
  r_median <- r_low <- r_high <-
  f_median <- f_low <- f_high <-
  ssbratio_median <- ssbratio_low <- ssbratio_high <-
  fratio_median <- fratio_low <- fratio_high <-
  matrix(NA, nrow = nrow(om_list$ssb), ncol = (em_num + 1) * length(maindir_list))

for (j in 1:length(maindir_list)) {
  load(file.path(maindir_list[j], "output", "om_output.RData"))
  load(file.path(maindir_list[j], "output", "mas_output.RData"))
  data_list <- list(om_list, mas_list)
  for (k in 1:(em_num + 1)) {
    ssb_median[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$ssb), function(x) boxplot.stats(data_list[[k]]$ssb[x, ])$`stats`[3])
    ssb_low[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$ssb), function(x) boxplot.stats(data_list[[k]]$ssb[x, ])$`stats`[1])
    ssb_high[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$ssb), function(x) boxplot.stats(data_list[[k]]$ssb[x, ])$`stats`[5])

    r_median[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$recruit), function(x) boxplot.stats(data_list[[k]]$recruit[x, ])$`stats`[3])
    r_low[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$recruit), function(x) boxplot.stats(data_list[[k]]$recruit[x, ])$`stats`[1])
    r_high[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$recruit), function(x) boxplot.stats(data_list[[k]]$recruit[x, ])$`stats`[5])

    f_median[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$Ftot), function(x) boxplot.stats(data_list[[k]]$Ftot[x, ])$`stats`[3])
    f_low[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$Ftot), function(x) boxplot.stats(data_list[[k]]$Ftot[x, ])$`stats`[1])
    f_high[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$Ftot), function(x) boxplot.stats(data_list[[k]]$Ftot[x, ])$`stats`[5])

    ssbratio_median[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$ssbratio), function(x) boxplot.stats(data_list[[k]]$ssbratio[x, ])$`stats`[3])
    ssbratio_low[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$ssbratio), function(x) boxplot.stats(data_list[[k]]$ssbratio[x, ])$`stats`[1])
    ssbratio_high[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$ssbratio), function(x) boxplot.stats(data_list[[k]]$ssbratio[x, ])$`stats`[5])

    fratio_median[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$fratio), function(x) boxplot.stats(data_list[[k]]$fratio[x, ])$`stats`[3])
    fratio_low[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$fratio), function(x) boxplot.stats(data_list[[k]]$fratio[x, ])$`stats`[1])
    fratio_high[, ((j - 1) * (em_num + 1) + k)] <- sapply(1:nrow(data_list[[k]]$fratio), function(x) boxplot.stats(data_list[[k]]$fratio[x, ])$`stats`[5])
  }
}

## Plot SSB
jpeg(file = file.path(figure_path, "SSB.jpg"), width = 130, height = 150, units = "mm", res = 1200)
op <- par(no.readonly = TRUE)
par(op)
par(oma = c(5, 5, 0, 5), mar = c(0, 0, 0, 0.2), mfrow = c(length(case_id), em_num), pch = 16)

for (j in 1:length(maindir_list)) {
  load(file.path(maindir_list[j], "output", "om_output.RData"))
  x_val <- om_input$year
  plot(x_val, rep(0, times = length(x_val)), ylim = range(ssb_low, ssb_high), type = "l", lty = 1, col = "gray30", xlab = "", ylab = "", axes = F)
  if (j == length(maindir_list)) axis(1, at = c(5, 15, 25), labels = c(5, 15, 25))
  box()
  axis(2, at = c(3000, 7000, 11000), labels = c(3000, 7000, 11000), las = 2)
  legend("topright", case_id[j], bty = "n", cex = 0.8)
  polygon(c(x_val, rev(x_val)), c(ssb_low[, (j - 1) * (em_num + 1) + 1], rev(ssb_high[, (j - 1) * (em_num + 1) + 1])), border = NA, col = alpha("gray30", 0.3))
  polygon(c(x_val, rev(x_val)), c(ssb_low[, (j - 1) * (em_num + 1) + 2], rev(ssb_high[, (j - 1) * (em_num + 1) + 2])), border = NA, col = alpha("deepskyblue3", 0.3))
  lines(x_val, ssb_median[, (j - 1) * (em_num + 1) + 2], type = "o", lty = 1, col = "deepskyblue3", pch = 19, cex = 0.2)
  lines(x_val, ssb_median[, (j - 1) * (em_num + 1) + 1], type = "o", lty = 1, col = "gray30", pch = 19, cex = 0.2)
}
mtext(text = "Year", side = 1, line = 2.5, outer = TRUE)
mtext(text = expression(paste(italic(SSB), " (mt)")), side = 2, line = 3.5, outer = TRUE)

legend("top",
  legend = c("OM", "MAS"),
  col = c("gray30", "deepskyblue3"),
  fill = c("gray30", "deepskyblue3"),
  cex = 0.9,
  bty = "n",
  xpd = NA,
  ncol = 2
)
dev.off()

## R
jpeg(file = file.path(figure_path, "R.jpg"), width = 130, height = 150, units = "mm", res = 1200)
op <- par(no.readonly = TRUE)
par(op)
par(oma = c(5, 5, 0, 5), mar = c(0, 0, 0, 0.2), mfrow = c(length(case_id), em_num), pch = 16)

for (j in 1:length(maindir_list)) {
  load(file.path(maindir_list[j], "output", "om_output.RData"))
  x_val <- om_input$year
  plot(x_val, rep(0, times = length(x_val)), ylim = range(r_low, r_high), type = "l", lty = 1, col = "gray30", xlab = "", ylab = "", axes = F)
  if (j == length(maindir_list)) axis(1, at = c(5, 15, 25), labels = c(5, 15, 25))
  box()
  axis(2, at = c(800, 1600, 2400), labels = c(800, 1600, 2400), las = 2)
  legend("topright", case_id[j], bty = "n", cex = 0.8)
  polygon(c(x_val, rev(x_val)), c(r_low[, (j - 1) * (em_num + 1) + 1], rev(r_high[, (j - 1) * (em_num + 1) + 1])), border = NA, col = alpha("gray30", 0.3))
  polygon(c(x_val, rev(x_val)), c(r_low[, (j - 1) * (em_num + 1) + 2], rev(r_high[, (j - 1) * (em_num + 1) + 2])), border = NA, col = alpha("deepskyblue3", 0.3))
  lines(x_val, r_median[, (j - 1) * (em_num + 1) + 2], type = "o", lty = 1, col = "deepskyblue3", pch = 19, cex = 0.2)
  lines(x_val, r_median[, (j - 1) * (em_num + 1) + 1], type = "o", lty = 1, col = "gray30", pch = 19, cex = 0.2)
}
mtext(text = "Year", side = 1, line = 2.5, outer = TRUE)
mtext(text = expression(paste(italic(R), " (×1000 fish)")), side = 2, line = 3.5, outer = TRUE)

legend("top",
  legend = c("OM", "MAS"),
  col = c("gray30", "deepskyblue3"),
  fill = c("gray30", "deepskyblue3"),
  cex = 0.9,
  bty = "n",
  xpd = NA,
  ncol = 2
)
dev.off()

## F
jpeg(file = file.path(figure_path, "F.jpg"), width = 130, height = 150, units = "mm", res = 1200)
op <- par(no.readonly = TRUE)
par(op)
par(oma = c(5, 5, 0, 5), mar = c(0, 0, 0, 0.2), mfrow = c(length(case_id), em_num), pch = 16)

for (j in 1:length(maindir_list)) {
  load(file.path(maindir_list[j], "output", "om_output.RData"))
  x_val <- om_input$year

  plot(x_val, rep(0, times = length(x_val)), ylim = range(f_low, f_high), type = "l", lty = 1, col = "gray30", xlab = "", ylab = "", axes = F)
  if (j == length(maindir_list)) axis(1, at = c(5, 15, 25), labels = c(5, 15, 25))
  box()
  axis(2, at = c(0.2, 0.4, 0.6), labels = c(0.2, 0.4, 0.6), las = 2)
  legend("topright", case_id[j], bty = "n", cex = 0.8)
  polygon(c(x_val, rev(x_val)), c(f_low[, (j - 1) * (em_num + 1) + 1], rev(f_high[, (j - 1) * (em_num + 1) + 1])), border = NA, col = alpha("gray30", 0.3))
  polygon(c(x_val, rev(x_val)), c(f_low[, (j - 1) * (em_num + 1) + 2], rev(f_high[, (j - 1) * (em_num + 1) + 2])), border = NA, col = alpha("deepskyblue3", 0.3))
  lines(x_val, f_median[, (j - 1) * (em_num + 1) + 2], type = "o", lty = 1, col = "deepskyblue3", pch = 19, cex = 0.2)
  lines(x_val, f_median[, (j - 1) * (em_num + 1) + 1], type = "o", lty = 1, col = "gray30", pch = 19, cex = 0.2)
}
mtext(text = "Year", side = 1, line = 2.5, outer = TRUE)
mtext(text = expression(italic("F")), side = 2, line = 3.5, outer = TRUE)

legend("top",
  legend = c("OM", "MAS"),
  col = c("gray30", "deepskyblue3"),
  fill = c("gray30", "deepskyblue3"),
  cex = 0.9,
  bty = "n",
  xpd = NA,
  ncol = 2
)
dev.off()

## SSB/SSBMSY
jpeg(file = file.path(figure_path, "relativeSSB.jpg"), width = 130, height = 150, units = "mm", res = 1200)
op <- par(no.readonly = TRUE)
par(op)
par(oma = c(5, 5, 0, 5), mar = c(0, 0, 0, 0.2), mfrow = c(length(case_id), em_num), pch = 16)

for (j in 1:length(maindir_list)) {
  load(file.path(maindir_list[j], "output", "om_output.RData"))
  x_val <- om_input$year

  plot(x_val, rep(0, times = length(x_val)), ylim = range(ssbratio_low, ssbratio_high), type = "l", lty = 1, col = "gray30", xlab = "", ylab = "", axes = F)
  if (j == length(maindir_list)) axis(1, at = c(5, 15, 25), labels = c(5, 15, 25))
  box()
  axis(2, at = c(1.5, 3, 4.5), labels = c(1.5, 3, 4.5), las = 2)
  legend("topright", case_id[j], bty = "n", cex = 0.8)
  polygon(c(x_val, rev(x_val)), c(ssbratio_low[, (j - 1) * (em_num + 1) + 1], rev(ssbratio_high[, (j - 1) * (em_num + 1) + 1])), border = NA, col = alpha("gray30", 0.3))
  polygon(c(x_val, rev(x_val)), c(ssbratio_low[, (j - 1) * (em_num + 1) + 2], rev(ssbratio_high[, (j - 1) * (em_num + 1) + 2])), border = NA, col = alpha("deepskyblue3", 0.3))
  lines(x_val, ssbratio_median[, (j - 1) * (em_num + 1) + 2], type = "o", lty = 1, col = "deepskyblue3", pch = 19, cex = 0.2)
  lines(x_val, ssbratio_median[, (j - 1) * (em_num + 1) + 1], type = "o", lty = 1, col = "gray30", pch = 19, cex = 0.2)
  abline(h = 1, col = "gray", lty = 2)
}
mtext(text = "Year", side = 1, line = 2.5, outer = TRUE)
mtext(text = expression(italic(SSB / SSB)[MSY]), side = 2, line = 3.5, outer = TRUE)

legend("top",
  legend = c("OM", "MAS"),
  col = c("gray30", "deepskyblue3"),
  fill = c("gray30", "deepskyblue3"),
  cex = 0.9,
  bty = "n",
  xpd = NA,
  ncol = 2
)
dev.off()
## F/FMSY
jpeg(file = file.path(figure_path, "relativeF.jpg"), width = 130, height = 150, units = "mm", res = 1200)
op <- par(no.readonly = TRUE)
par(op)
par(oma = c(5, 5, 0, 5), mar = c(0, 0, 0, 0.2), mfrow = c(length(case_id), em_num), pch = 16)

for (j in 1:length(maindir_list)) {
  load(file.path(maindir_list[j], "output", "om_output.RData"))
  x_val <- om_input$year

  plot(x_val, rep(0, times = length(x_val)), ylim = range(fratio_low, fratio_high), type = "l", lty = 1, col = "gray30", xlab = "", ylab = "", axes = F)
  if (j == length(maindir_list)) axis(1, at = c(5, 15, 25), labels = c(5, 15, 25))
  box()
  axis(2, at = c(0.5, 2, 3.5), labels = c(0.5, 2, 3.5), las = 2)
  legend("topright", case_id[j], bty = "n", cex = 0.8)
  polygon(c(x_val, rev(x_val)), c(fratio_low[, (j - 1) * (em_num + 1) + 1], rev(fratio_high[, (j - 1) * (em_num + 1) + 1])), border = NA, col = alpha("gray30", 0.3))
  polygon(c(x_val, rev(x_val)), c(fratio_low[, (j - 1) * (em_num + 1) + 2], rev(fratio_high[, (j - 1) * (em_num + 1) + 2])), border = NA, col = alpha("deepskyblue3", 0.3))
  lines(x_val, fratio_median[, (j - 1) * (em_num + 1) + 2], type = "o", lty = 1, col = "deepskyblue3", pch = 19, cex = 0.2)
  lines(x_val, fratio_median[, (j - 1) * (em_num + 1) + 1], type = "o", lty = 1, col = "gray30", pch = 19, cex = 0.2)
  abline(h = 1, col = "gray", lty = 2)
}
mtext(text = "Year", side = 1, line = 2.5, outer = TRUE)
mtext(text = expression(italic("F/F")[MSY]), side = 2, line = 3.5, outer = TRUE)

legend("top",
  legend = c("OM", "MAS"),
  col = c("gray30", "deepskyblue3"),
  fill = c("gray30", "deepskyblue3"),
  cex = 0.9,
  bty = "n",
  ncol = 2
)
dev.off()

# Plot relative error in MSY, FMSY, and SSBMSY  --------------------------

## Aggregate output data
case_id <- paste("C", 1:7, sep = "")
maindir_list <- paste(project_dir, case_id, sep = "")

em_num <- 1

for (j in 1:length(maindir_list)) {
  load(file.path(maindir_list[j], "output", "performance_measure.RData"))
  if (j == 1) {
    msy_re <- matrix(NA, nrow = length(re_list), ncol = em_num * length(maindir_list))
    fmsy_re <- matrix(NA, nrow = length(re_list), ncol = em_num * length(maindir_list))
    ssbmsy_re <- matrix(NA, nrow = length(re_list), ncol = em_num * length(maindir_list))
  }
  for (i in 1:length(re_list)) {
    msy_re[i, ((j - 1) * em_num + 1):((j - 1) * em_num + em_num)] <- as.matrix(re_list[[i]]$msy$MAS)
    fmsy_re[i, ((j - 1) * em_num + 1):((j - 1) * em_num + em_num)] <- as.matrix(re_list[[i]]$fmsy$MAS)
    ssbmsy_re[i, ((j - 1) * em_num + 1):((j - 1) * em_num + em_num)] <- as.matrix(re_list[[i]]$ssbmsy$MAS)
  }
}

msy_stat <- matrix(NA, nrow = 3, ncol = em_num * length(maindir_list))
fmsy_stat <- matrix(NA, nrow = 3, ncol = em_num * length(maindir_list))
ssbmsy_stat <- matrix(NA, nrow = 3, ncol = em_num * length(maindir_list))

sapply(1:ncol(msy_stat), function(x) {
  msy_stat[, x] <<- boxplot.stats(msy_re[, x])$`stats`[c(1, 3, 5)]
  fmsy_stat[, x] <<- boxplot.stats(fmsy_re[, x])$`stats`[c(1, 3, 5)]
  ssbmsy_stat[, x] <<- boxplot.stats(ssbmsy_re[, x])$`stats`[c(1, 3, 5)]
})

jpeg(file = file.path(figure_path, "MSY_RE.jpg"), width = 150, height = 150, units = "mm", res = 1200)
op <- par(no.readonly = TRUE)
par(op)
par(oma = c(1, 1, 1, 1), mar = c(6, 0, 0, 0.5), mfrow = c(1, 3), pch = 16)
color <- rep("black", times = em_num)
xlim <- c(-10, 10)

plot(msy_stat[2, ], rev(1:ncol(msy_stat)),
  pch = rep(((1:em_num) + 1), times = length(maindir_list)),
  col = color,
  xlim = xlim,
  xlab = expression(paste("RE in ", italic("MSY"))), ylab = "",
  axes = F
)
segments(msy_stat[1, ], rev(1:ncol(msy_stat)), msy_stat[3, ], rev(1:ncol(msy_stat)), col = rep(color, times = length(maindir_list)))
abline(v = 0, col = "gray", lty = 2)
abline(h = seq(em_num + 0.5, ncol(msy_stat), by = em_num), col = "gray30", lty = 2)
box()
axis(1, at = seq(-10, 10, by = 5), labels = c(expression(paste("\u2013", "10")), expression(paste("\u2013", "5")), expression("0.0"), expression("5"), expression("10")))
text(
  x = rep(min(xlim) * 0.8, times = length(maindir_list)),
  y = seq(em_num, ncol(msy_stat), by = em_num),
  rev(case_id)
)

plot(fmsy_stat[2, ], rev(1:ncol(fmsy_stat)),
  pch = rep(((1:em_num) + 1), times = length(maindir_list)),
  col = color,
  xlim = xlim,
  xlab = expression(paste("RE in ", italic(F)[MSY])), ylab = "",
  axes = F
)
segments(fmsy_stat[1, ], rev(1:ncol(fmsy_stat)), fmsy_stat[3, ], rev(1:ncol(fmsy_stat)), col = rep(color, times = length(maindir_list)))
abline(v = 0, col = "gray", lty = 2)
abline(h = seq(em_num + 0.5, ncol(fmsy_stat), by = em_num), col = "gray30", lty = 2)
box()
axis(1, at = seq(-10, 10, by = 5), labels = c(expression(paste("\u2013", "10")), expression(paste("\u2013", "5")), expression("0.0"), expression("5"), expression("10")))
text(
  x = rep(min(xlim) * 0.8, times = length(maindir_list)),
  y = seq(em_num, ncol(fmsy_stat), by = em_num) ,
  rev(case_id)
)

legend(
  x = "bottom",
  inset = c(-0.15),
  legend = c("MAS"),
  pch = c(2),
  lty = c(1),
  cex = 1,
  bty = "n",
  xpd = TRUE,
  ncol = 2
)

plot(ssbmsy_stat[2, ], rev(1:ncol(ssbmsy_stat)),
  pch = rep(((1:em_num) + 1), times = length(maindir_list)),
  col = color,
  xlim = xlim,
  xlab = expression(paste("RE in ", italic(SSB)[MSY])), ylab = "",
  axes = F
)
segments(ssbmsy_stat[1, ], rev(1:ncol(ssbmsy_stat)), ssbmsy_stat[3, ], rev(1:ncol(ssbmsy_stat)), col = rep(color, times = length(maindir_list)))
abline(v = 0, col = "gray", lty = 2)
abline(h = seq(em_num + 0.5, ncol(ssbmsy_stat), by = em_num), col = "gray30", lty = 2)
box()
axis(1, at = seq(-10, 10, by = 5), labels = c(expression(paste("\u2013", "10")), expression(paste("\u2013", "5")), expression("0.0"), expression("5"), expression("10")))
text(
  x = rep(min(xlim) * 0.8, times = length(maindir_list)),
  y = seq(em_num, ncol(ssbmsy_stat), by = em_num),
  rev(case_id)
)

dev.off()
