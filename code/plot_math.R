# Plots purely based on functions
library(dplyr);library(ggplot2);library(logitnorm);library(cowplot)

# Plots for comparison of beta and logit-normal
rbeta2 <- function(n, mu, disp)rbeta(n, mu/disp, (1-mu)/disp)
dbeta2 <- function(x, mu, disp)dbeta(x , mu/disp, (1-mu)/disp)
plotit <- function(p, rho, sd){
  ggplot(data.frame(x=c(0.01,0.99)), aes(x)) +
  stat_function(fun=function(z)dbeta2(z, p, rho),
                geom="line", aes(colour="Beta"), size = 1.5, alpha = 0.7) +
  stat_function(fun=function(z)dlogitnorm(z, logit(p), sd),
                geom="line", aes(colour="Logit-normal"), size = 1.5, alpha = 0.6) +
  scale_x_continuous(limits = c(0, 1)) + theme_bw() +
  theme(axis.title = element_blank(), legend.position = "none", axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), plot.margin = unit(c(0,0.1,0,0.1), "cm"),
        axis.title.x=element_blank())
}

annotations <- data.frame(
        x = rep(c(-Inf,Inf), 4),
        y =  Inf,
        l = c("p = 0.5","sigma = 0.5\nphi = 0.05",
              "p = 0.5","sigma = 1\nphi = 0.2",
              "p = 0.2","sigma = 0.5\nphi = 0.05",
              "p = 0.2","sigma = 1\nphi = 0.2"),
        hj = rep(c(-0.2,1.2),4),
        vj = c(2,1.3))

plot_grid(
plotit(0.5, 0.05, 0.5) +
  geom_text(data = annotations[1:2,], aes(x=x, y=y, hjust=hj, vjust=vj, label = l), size = 3) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.margin = unit(c(0.3,0,0.1,0), "cm")),
plotit(0.5, 0.2, 1) +
  geom_text(data = annotations[3:4,], aes(x=x, y=y, hjust=hj, vjust=vj, label = l), size = 3) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        plot.margin = unit(c(0.3,0,0.1,0), "cm")),
plotit(0.2, 0.05, 0.5) +
  geom_text(data = annotations[5:6,], aes(x=x, y=y, hjust=hj, vjust=vj, label = l), size = 3),
plotit(0.2, 0.2, 1) +
  geom_text(data = annotations[7:8,], aes(x=x, y=y, hjust=hj, vjust=vj, label = l), size = 3),
align = "v"
) -> compare_plot

# Plot of the latent normal link
norm_plot <- ggplot(data.frame(x=c(-5,5)), aes(x)) +
  stat_function(fun=function(z)pnorm(z, -0.6, 1),
                geom="line", size = 2) +
  stat_function(fun=function(z)dnorm(z, -0.6, 1),
                geom="line", size = 1.5, alpha = 0.4) +
  geom_segment(x = -1, xend = -1, y = 0, yend = pnorm(-1, -0.6, 1),
               size = 1, alpha = 0.5, linetype = "dotted") +
  geom_segment(x = 1, xend = 1, y = 0, yend = pnorm(1, -0.6, 1),
               size = 1, alpha = 0.5, linetype = "dotted") +
  geom_text(data =   data.frame(x = -Inf, y =  Inf, hj = -0.2, vj = 1.2),
            aes(x=x, y=y, hjust=hj, vjust=vj), size = 4,
            label = "p0 = 0.344\np1 = 0.601\np2 = 0.054") +
  geom_text(data =   data.frame(x = -Inf, y =  Inf, hj = -0.2, vj = 4),
            aes(x=x, y=y, hjust=hj, vjust=vj), size = 4,
            label = "mean = -0.6\nsigma = 1", alpha = 0.5) +
  scale_x_continuous(breaks = c(-1,0,1)) + theme_bw() +
  coord_cartesian(xlim = c(-4, 4), ylim = c(0, 1), expand = 0) +
  theme(axis.title = element_blank(), legend.position = "none", axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), plot.margin = unit(c(0,0.1,0,0.1), "cm"),
        axis.title.x=element_blank())

# SAVE
saveRDS(compare_plot, "data/compare_plot.rds")
saveRDS(norm_plot, "data/norm_plot.rds")