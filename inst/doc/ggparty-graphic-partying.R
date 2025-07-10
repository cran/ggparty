## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
fig.width = 7
)

## -----------------------------------------------------------------------------
library(ggparty)

## ----fig.asp = 1, eval = T, echo = FALSE--------------------------------------
data("TeachingRatings", package = "AER")
tr <- subset(TeachingRatings, credits == "more")

tr_tree <- lmtree(eval ~ beauty | minority + age + gender + division + native +
                    tenure, data = tr, weights = students, caseweights = FALSE)


# create dataframe with densities
dens_df <- data.frame(x_dens = numeric(), y_dens = numeric(), id = numeric(), breaks = character())
for (id in c(2, 5)) {
  x_dens <- density(tr_tree[id]$data$age)$x
  y_dens <- density(tr_tree[id]$data$age)$y
  breaks <- rep("left", length(x_dens))
  if (id == 2) breaks[x_dens > 50] <- "right"
  if (id == 5) breaks[x_dens > 40] <- "right"
  dens_df <- rbind(dens_df, data.frame(x_dens, y_dens, id, breaks))
  }

# get the party started
ggparty(tr_tree, terminal_space = 0.4,
        layout = data.frame(id = c(1, 2, 5, 7),
                            x = c(0.35, 0.15, 0.7, 0.8),
                            y = c(0.95, 0.6, 0.8, 0.55))) +
  geom_edge(aes(col = factor(birth_order)),
            size = 1.2,
            alpha = 1,
            ids = -1) +
  geom_node_plot(ids = c(2,5),
                 gglist = list(
                   geom_line(data = dens_df,
                             aes(x = x_dens,
                                 y = y_dens),
                             show.legend = FALSE,
                             alpha = 0.8),
                   geom_ribbon(data = dens_df,
                               aes(x = x_dens,
                                   ymin = 0,
                                   ymax = y_dens,
                                   fill = breaks),
                               show.legend = FALSE,
                               alpha = 0.8),
                   xlab("age"),
                   theme_bw(),
                   theme(axis.title.y = element_blank())),
                 size = 1.5,
                 height = 0.5
  ) +
  geom_node_plot(ids = 1,
                 gglist = list(geom_bar(aes(x = gender, fill = gender),
                                                   show.legend = FALSE,
                                                   alpha = .8),
                               theme_bw(),
                               theme(axis.title.y = element_blank())),
                 size = 1.5,
                 height = 0.5
  ) +
  geom_node_plot(ids = 7,
                 gglist = list(geom_bar(aes(x = division, fill = division),
                                        show.legend = FALSE,
                                        alpha = .8),
                               theme_bw(),
                               theme(axis.title.y = element_blank())),
                 size = 1.5,
                 height = 0.5
  ) +
  geom_node_plot(gglist = list(geom_point(aes(x = beauty,
                                              y = eval,
                                              col = tenure,
                                              shape = minority),
                                          alpha = 0.8),
                               theme_bw(base_size = 10),
                               scale_color_discrete(h.start = 100)),
                 scales = "fixed",
                 ids = "terminal",
                 shared_axis_labels = T,
                 shared_legend = T,
                 predict = "beauty",
                 predict_gpar = list(col = "blue",
                                    size = 1.1)) +
  theme(legend.position = "none")

## -----------------------------------------------------------------------------
data("WeatherPlay", package = "partykit")
sp_o <- partysplit(1L, index = 1:3)
sp_h <- partysplit(3L, breaks = 75)
sp_w <- partysplit(4L, index = 1:2)
pn <- partynode(1L, split = sp_o, kids = list(
  partynode(2L, split = sp_h, kids = list(
    partynode(3L, info = "yes"),
    partynode(4L, info = "no"))),
  partynode(5L, info = "yes"),
  partynode(6L, split = sp_w, kids = list(
    partynode(7L, info = "yes"),
    partynode(8L, info = "no")))))
py <- party(pn, WeatherPlay)

## ----results = "asis"---------------------------------------------------------
is.ggplot(ggparty(py))

pander::pandoc.table(ggparty(py)$data[,1:16])

## ----Weatherplay, fig.width = 7-----------------------------------------------
ggparty(py) +
  geom_edge() +
  geom_edge_label() +
  geom_node_label(aes(label = splitvar), ids = "inner") +
  # identical to  geom_node_splitvar() +
  geom_node_label(aes(label = info), ids = "terminal")
  # identical to geom_node_info()
 

## ----fig.width = 7------------------------------------------------------------
ggparty(py) +
  geom_edge() +
  geom_edge_label() +
  # map color to level and size to nodesize for all nodes
  geom_node_splitvar(aes(col = factor(level),
                         size = nodesize)) +
  geom_node_info(aes(col = factor(level),
                     size = nodesize))


## ----fig.width = 7, eval = T--------------------------------------------------
ggparty(py, horizontal = TRUE) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_info()

## ----eval = T-----------------------------------------------------------------
gg <- ggparty(py, add_vars = list(right = "$node$split$right"))
gg$data$right

## ----eval = T-----------------------------------------------------------------
gg <- ggparty(py, add_vars = list(right =
                                    function(data, node) {
                                      node$node$split$right
                                      }
                                  )
              )
gg$data$right

## ----eval = T-----------------------------------------------------------------
gg <- ggparty(py, add_vars = list(nodedata_x_dens =
                                    function(data, node) {
                                      list(density(node$data$temperature,
                                                   n = data$nodesize)$x)
                                    }
                                  )
              )
gg$data$nodedata_x_dens

## ----eval = T-----------------------------------------------------------------
n1 <- partynode(id = 1L, split = sp_o, kids = lapply(2L:4L, partynode))
t2 <- party(n1,
            data = WeatherPlay,
            fitted = data.frame(
              "(fitted)" = fitted_node(n1, data = WeatherPlay),
              "(response)" = WeatherPlay$play,
              check.names = FALSE),
            terms = terms(play ~ ., data = WeatherPlay)
)
t2 <- as.constparty(t2)

## ----fig.width = 3, fig.asp = 0.8, eval = T-----------------------------------
ggplot(t2[2]$data) +
  geom_bar(aes(x = "", fill = play),
           position = position_fill()) +
  xlab("play")

## ----fig.asp=1, fig.width = 7, eval = T---------------------------------------
ggparty(t2) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  # pass list to gglist containing all ggplot components we want to plot for each
  # (default: terminal) node
  geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = play),
                                        position = position_fill()),
                               xlab("play")))

## ----fig.asp=1, fig.width = 7, eval = T---------------------------------------
ggparty(t2) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = play),
                                        position = position_fill()),
                               xlab("play")),
                 # draw only one label for each axis
                 shared_axis_labels = TRUE,
                 # draw line between tree and legend
                 legend_separator = TRUE
                 )

## ----fig.asp=1, fig.width = 7, eval = T---------------------------------------
ggparty(t2) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = play),
                                        position = position_fill()),
                               xlab("play")),
                 # draw individual legend for each plot
                 shared_legend = FALSE
  )

## ----fig.width = 7, eval = T--------------------------------------------------
ggparty(t2) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  # draw pie charts with their size relative to nodesize
  geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = play),
                                        position = position_fill()),
                               coord_polar("y"),
                               theme_void()),
                 size = "nodesize")

## ----eval = T-----------------------------------------------------------------
data("TeachingRatings", package = "AER")
tr <- subset(TeachingRatings, credits == "more")

tr_tree <- lmtree(eval ~ beauty | minority + age + gender + division + native +
                    tenure, data = tr, weights = students, caseweights = FALSE)

## -----------------------------------------------------------------------------
ggparty(tr_tree) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_plot(gglist = list(geom_point(aes(x = beauty,
                                             y = eval,
                                             col = tenure,
                                             shape = minority),
                                         alpha = 0.8),
                              theme_bw(base_size = 10)),
                shared_axis_labels = TRUE,
                legend_separator = TRUE,
                # predict based on variable
                predict = "beauty",
                # graphical parameters for geom_line of predictions
                predict_gpar = list(col = "blue",
                                   size = 1.2)
                )

## ----eval = T-----------------------------------------------------------------
data("GBSG2", package = "TH.data")
GBSG2$time <- GBSG2$time/365

library("survival")
wbreg <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  survreg(y ~ 0 + x, weights = weights, dist = "weibull", ...)
}


logLik.survreg <- function(object, ...)
  structure(object$loglik[2], df = sum(object$df), class = "logLik")

gbsg2_tree <- mob(Surv(time, cens) ~ horTh + pnodes | age + tsize +
                    tgrade + progrec + estrec + menostat, data = GBSG2,
                  fit = wbreg, control = mob_control(minsize = 80))

## -----------------------------------------------------------------------------
# function to generate newdata for predictions
generate_newdata <- function(data) {
  z <- data.frame(horTh = factor(rep(c("yes", "no"),
                                     each = length(data$pnodes))),
                  pnodes = rep(seq(from = min(data$pnodes),
                                   to = max(data$pnodes),
                                   length.out = length(data$pnodes)),
                               2))
  z$x <- model.matrix(~ ., data = z)
  z}

# convenience function to create dataframe for predictions
pred_df <- get_predictions(gbsg2_tree,
                           # IMPORTANT to set same ids as in geom_node_plot
                           # later used for plotting
                           ids = "terminal",
                           newdata_fun = generate_newdata,
                           predict_arg = list(type = "quantile",
                                              p = 0.5)
)

## ----fig.asp = 0.8, fig.width=7, eval = T-------------------------------------
ggparty(gbsg2_tree, terminal_space = 0.8, horizontal = TRUE) +
  geom_edge() +
  geom_node_splitvar() +
  geom_edge_label() +
  geom_node_plot(
    gglist = list(geom_point(aes(y = `Surv(time, cens).time`,
                                 x = pnodes,
                                 col = horTh),
                             alpha = 0.6),
                  # supply pred_df as data argument of geom_line
                  geom_line(data = pred_df,
                            aes(x = pnodes,
                                y = prediction,
                                col = horTh),
                            size = 1.2),
                  theme_bw(),
                  ylab("Survival Time")
                  ),
    ids = "terminal", # not necessary since default
    shared_axis_labels = TRUE
  )

## ----fig.width= 7, fig.asp= 0.6, eval = T-------------------------------------
ggparty(tr_tree,
        terminal_space = 0,
        add_vars = list(intercept = "$node$info$coefficients[1]",
                        beta = "$node$info$coefficients[2]")) +
  geom_edge(size = 1.5) +
  geom_edge_label(colour = "grey", size = 4) +
  # first label inner nodes
  geom_node_label(# map color of complete label to splitvar
                  mapping = aes(col = splitvar),
                  # map content to label for each line
                  line_list = list(aes(label = splitvar),
                                   aes(label = paste("p =",
                                                     formatC(p.value,
                                                             format = "e",
                                                             digits = 2))),
                                   aes(label = ""),
                                   aes(label = id)
                  ),
                  # set graphical parameters for each line in same order
                  line_gpar = list(list(size = 12),
                                   list(size = 8),
                                   list(size = 6),
                                   list(size = 7,
                                        col = "black",
                                        fontface = "bold",
                                        alignment = "left")
                  ),
                  # only inner nodes
                  ids = "inner") +
  # next label terminal nodes
  geom_node_label(# map content to label for each line
                  line_list = list(
                    aes(label = paste("beta[0] == ", round(intercept, 2))),
                    aes(label = paste("beta[1] == ",round(beta, 2))),
                    aes(label = ""),
                    aes(label = id)
                  ),
                  # set graphical parameters for each line in same order
                  line_gpar = list(list(size = 12, parse = T),
                                   list(size = 12, parse = T),
                                   list(size = 6),
                                   list(size = 7,
                                        col = "black",
                                        fontface = "bold",
                                        alignment = "left")),
                  ids = "terminal",
                  # nudge labels towards bottom so that edge labels have enough space
                  # alternatively use shift argument of edge_label
                  nudge_y = -.05) +
  # don't show legend for splitvar mapping to color since self-explanatory
  theme(legend.position = "none") +
  # html_documents seem to cut off a bit too much at the edges so set limits manually
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.1, 1.1))

## ----eval = T-----------------------------------------------------------------
## Boston housing data
data("BostonHousing", package = "mlbench")
BostonHousing <- transform(BostonHousing,
                           chas = factor(chas, levels = 0:1, labels = c("no", "yes")),
                           rad = factor(rad, ordered = TRUE))

## linear model tree
bh_tree <- lmtree(medv ~ log(lstat) + I(rm^2) | zn +
                    indus + chas + nox + age + dis + rad + tax + crim + b + ptratio,
                  data = BostonHousing, minsize = 40)

## ----fig.width= 7, fig.asp=1, eval = T----------------------------------------
# terminal space specifies at which value of y the terminal plots begin
bh_plot <- ggparty(bh_tree, terminal_space = 0.5) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  # plot first row
  geom_node_plot(gglist = list(
    geom_point(aes(y = medv, x = `log(lstat)`, col = chas),
               alpha = 0.6)),
    # halving the height shrinks plots towards the top
    height = 0.5) +
  # plot second row
  geom_node_plot(gglist = list(
    geom_point(aes(y = medv, x = `I(rm^2)`, col = chas),
               alpha = 0.6)),
    height = 0.5,
    # move -0.25 y to use the bottom half of the terminal space
    nudge_y = -0.25)

bh_plot

## ----fig.width=7, fig.asp = 1, eval = T---------------------------------------
bh_plot + theme_bw()

## ----fig.width= 7, fig.asp=1, eval = T----------------------------------------
ggparty(bh_tree, terminal_space = 0.5,
        # id specifies node; x and y values need to be between 0 and 1
        layout = data.frame(id = c(1, 2),
                            x = c(0.7, 0.3),
                            y = c(1, 0.9))
        ) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_plot(gglist = list(
    geom_point(aes(y = medv, x = `log(lstat)`, col = chas),
               alpha = 0.6)),
    height = 0.5) +
  geom_node_plot(gglist = list(
    geom_point(aes(y = medv, x = `I(rm^2)`, col = chas),
               alpha = 0.6)),
    height = 0.5,
    nudge_y = -0.25) +
  theme_bw()

## ----eval = T-----------------------------------------------------------------
autoplot(py)

## ----eval = T-----------------------------------------------------------------
autoplot(t2)

## ----fig.asp = 1, eval = T----------------------------------------------------
autoplot(bh_tree, plot_var = "log(lstat)", show_fit = FALSE)
autoplot(bh_tree, plot_var = "I(rm^2)", show_fit = TRUE)

## ----eval = T-----------------------------------------------------------------
autoplot(gbsg2_tree, plot_var = "pnodes")

## ----fig.asp = 1, eval = T----------------------------------------------------
autoplot(tr_tree)

## ----fig.width= 7, fig.asp= 1, eval = T---------------------------------------

asterisk_sign <- function(p_value) {
  if (p_value < 0.001) return(c("***"))
  if (p_value < 0.01) return(c("**"))
  if (p_value < 0.05) return(c("*"))
  else return("")
}


ggparty(tr_tree,
        terminal_space = 0.5) +
  geom_edge(size = 1.5) +
  geom_edge_label(colour = "grey", size = 4) +
  # plot fitted values against residuals for each terminal model
  geom_node_plot(gglist = list(geom_point(aes(x = fitted_values,
                                             y = residuals,
                                             col = tenure,
                                             shape = minority),
                                         alpha = 0.8),
                               geom_hline(yintercept = 0),
                               theme_bw(base_size = 10)),
                 # y scale is fixed for better comparability,
                 # x scale is free for effecient use of space
                 scales = "free_x",
                 ids = "terminal",
                 shared_axis_labels = TRUE
  ) +
  # label inner nodes
  geom_node_label(aes(col = splitvar),
                  # label nodes with ID, split variable and p value
                  line_list = list(aes(label = paste("Node", id)),
                                   aes(label = splitvar),
                                   aes(label = asterisk_sign(p.value))
                                   ),
                  # set graphical parameters for each line
                  line_gpar = list(list(size = 8, col = "black", fontface = "bold"),
                                   list(size = 12),
                                   list(size = 8)
                                   ),
                  ids = "inner") +
  # add labels for terminal nodes
  geom_node_label(aes(label = paste0("Node ", id, ", N = ", nodesize)),
                  fontface = "bold",
                  ids = "terminal",
                  size = 3,
                  # 0.01 nudge_y is enough to be above the node plot since a terminal
                  # nodeplot's top (not center) is at the node's coordinates.
                  nudge_y = 0.01) +
  theme(legend.position = "none")

## ----fig.asp = 1, eval = T----------------------------------------------------
# create dataframe with ids, densities and breaks
# since we are going to supply the data.frame directly to a geom inside gglist,
# we don't need to worry about the number of observations per id and only data for the ids
# used by the respective geom_node_plot() needs to be generated (2 and 5 in this case)
dens_df <- data.frame(x_dens = numeric(), y_dens = numeric(), id = numeric(), breaks = character())
for (id in c(2, 5)) {
  x_dens <- density(tr_tree[id]$data$age)$x
  y_dens <- density(tr_tree[id]$data$age)$y
  breaks <- rep("left", length(x_dens))
  if (id == 2) breaks[x_dens > 50] <- "right"
  if (id == 5) breaks[x_dens > 40] <- "right"
  dens_df <- rbind(dens_df, data.frame(x_dens, y_dens, id, breaks))
  }

# adjust layout so that each node plot has enough space
ggparty(tr_tree, terminal_space = 0.4,
        layout = data.frame(id = c(1, 2, 5, 7),
                            x = c(0.35, 0.15, 0.7, 0.8),
                            y = c(0.95, 0.6, 0.8, 0.55))) +
  # map color of edges to birth_order (order from left to right)
  geom_edge(aes(col = factor(birth_order)),
            size = 1.2,
            alpha = 1,
            # exclude root so it doesn't count as it's own colour
            ids = -1) +
  # density plots for age splits
  geom_node_plot(ids = c(2, 5),
                 gglist = list( # supply dens_df and plot line
                   geom_line(data = dens_df,
                             aes(x = x_dens,
                                 y = y_dens),
                             show.legend = FALSE,
                             alpha = 0.8),
                   # supply dens_df and plot ribbon, map color to breaks
                   geom_ribbon(data = dens_df,
                               aes(x = x_dens,
                                   ymin = 0,
                                   ymax = y_dens,
                                   fill = breaks),
                               show.legend = FALSE,
                               alpha = 0.8),
                   xlab("age"),
                   theme_bw(),
                   theme(axis.title.y = element_blank())),
                 size = 1.5,
                 height = 0.5
  ) +
  # plot bar plot of gender at root
  geom_node_plot(ids = 1,
                 gglist = list(geom_bar(aes(x = gender, fill = gender),
                                                   show.legend = FALSE,
                                                   alpha = .8),
                               theme_bw(),
                               theme(axis.title.y = element_blank())),
                 size = 1.5,
                 height = 0.5
  ) +
  # plot bar plot of division for node 7
  geom_node_plot(ids = 7,
                 gglist = list(geom_bar(aes(x = division, fill = division),
                                        show.legend = FALSE,
                                        alpha = .8),
                               theme_bw(),
                               theme(axis.title.y = element_blank())),
                 size = 1.5,
                 height = 0.5
  ) +
  # plot terminal nodes with predictions
  geom_node_plot(gglist = list(geom_point(aes(x = beauty,
                                              y = eval,
                                              col = tenure,
                                              shape = minority),
                                          alpha = 0.8),
                               theme_bw(base_size = 10),
                               scale_color_discrete(h.start = 100)),
                 shared_axis_labels = TRUE,
                 legend_separator = TRUE,
                 predict = "beauty",
                 predict_gpar = list(col = "blue",
                                    size = 1.1)) +
  # remove all legends from top level since self explanatory
  theme(legend.position = "none")

