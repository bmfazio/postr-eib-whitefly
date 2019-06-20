# Process fits into shareable results
library(dplyr)
library(ggplot2)
library(cowplot)

looList <- readRDS("data/source/looList.rds")[c(1:7,9:10)]
mar_eff <- readRDS("data/source/marginal_effs.rds")
ppc_gfx <- readRDS("data/source/PPC_plots.rds")
wf <- readRDS("data/source/wf.rds")

## Plots for Data section
wf_layout <- wf %>% select(rep, trt) %>% unique %>% arrange(rep) %>%
  ggplot(aes(y = as.numeric(rep))) +
  geom_point(aes(x = rep(1:6,3), y = as.numeric(rep)+0.07), shape = 8, size = 3.7, color = "#33a532") +
  geom_point(aes(color = trt, fill = trt, x = rep(1:6,3)), shape = 22, size = 3.1) +
  geom_point(aes(color = trt, fill = trt, x = rep(1:6,3)+0.07), shape = 25, size = 3) +
  geom_point(aes(color = trt, fill = trt, x = rep(1:6,3)-0.07), shape = 25, size = 3) +
  geom_tile(data = data.frame(y=1:3,x=3.5), mapping = aes(y=y, x=x),
            width = 5.85, height = 0.6, fill = NA, color = 1, size = 1) +
  geom_text(data = data.frame(x=1.3,y=1:3+0.42,l=c("Block 1", "Block 2", "Block 3")), aes(x=x,y=y,label=l),
            size = 3) +
  geom_text(data = data.frame(x=3.5,y=0.55), aes(x=x,y=y), label = "×12 weeks") +
  coord_cartesian(xlim = c(0.5, 6.5), ylim = c(0.45, 3.5), expand = 0) +
  theme_void() + theme(legend.position = "none")
# Show distribution
wf_dist <- wf %>%
  mutate(frac = nlive/bindenom) %>% group_by(frac) %>% summarise(tot = n()/nrow(wf)) %>%
  ggplot(aes(x = frac, xend = frac, y = 0, yend = tot)) +
  geom_segment(size = 1.2, color = "#333333") + scale_alpha(range = c(1, 0.65)) +
  theme_bw() + ggtitle("Proportion of surviving flies") + scale_y_continuous(breaks = c(0, 0.25, 0.5)) +
  theme(legend.position = "none", axis.title = element_blank(),
        plot.title = element_text(size = 10))

## Plots for Results section
tab_dist <- c("Binomial",
              "ZI Binomial", "ZI Binomial", "ZI Binomial",
              "EI Binomial","EI Binomial","EI Binomial","EI Binomial","EI Binomial")
tab_infl <- c("-", "constant", "treatment", "all", "constant", "treatment", "all", "treatment", "all")
tab_link <- c("-", "-", "logit", "logit", "-", "softmax", "softmax", "Norm. CDF", "Norm. CDF")

loo_table <- looList %>%
  sapply(function(x)x$estimates[3,]) %>% t %>%
  data.frame %>%
  cbind(Distribution = tab_dist, `Inflation` = tab_infl, Link = tab_link, .) %>%
  mutate_if(is.numeric, round, 1)

colnames(loo_table)[4] <- "LOOIC"

# Marginal effects
mar_mod <- lapply(unique(mar_eff$model), function(x) mar_eff %>%
                    filter(model == x)) %>% lapply(
                      function(x) x %>%
                        mutate(Treatment =
                                 factor(Treatment,
                                        levels = levels(Treatment)[c(1,6,2:5)])))
marg_table <- mar_mod %>%
  lapply(function(x)setNames(x[c(1,6,2:5),c(1,9,7,10)],
                             c("Treatment", "Q 2.5", "Mean", "Q 97.5")) %>%
           mutate_if(is.numeric, round, digits = 2))

marg_plot <- mar_mod %>%
  lapply(function(z) z %>%
  ggplot(aes(y = 1, x = estimate__, xmin = lower__, xmax = upper__)) +
  geom_point(size = 0.25) + geom_errorbarh() + theme_bw() + facet_grid(Treatment~., switch = "y") +  ylab("") +
    theme(strip.text.y = element_text(angle = 180, margin = margin(0,50*0.3,0,25*0.3)), panel.spacing = unit(0, "lines"),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(), plot.margin = unit(c(0.2,0.5,0.2,0), "cm"), axis.title.x=element_blank()) +
    scale_x_continuous(breaks = c(0, 5, 10), minor_breaks = c(2.5, 7.5)) +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 2), expand = 0))
# PPC plot
ppch_plot <- ppc_gfx %>%
  lapply(function(x)x + coord_cartesian(xlim = c(0, 15), ylim = c(0, 0.45), expand = 0) + theme_bw() +
           theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank()))

# SAVE
saveRDS(plot_grid(wf_layout, wf_dist, align = "h"), "data/data_plot.rds")
saveRDS(loo_table, "data/loo_table.rds")
saveRDS(marg_plot, "data/marg_plot.rds")
saveRDS(marg_table, "data/marg_table.rds")
saveRDS(ppch_plot, "data/ppch_plot.rds")