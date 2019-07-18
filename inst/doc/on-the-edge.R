## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
fig.width = 7
)

## ---- echo = T, message= FALSE-------------------------------------------
library(ggparty) 
data("WeatherPlay", package = "partykit")
levels(WeatherPlay$outlook)[1] <- c("beta")
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

## ------------------------------------------------------------------------
ggparty(py) +
  geom_edge() +
  geom_edge_label() +
  geom_node_splitvar() +
  geom_node_info()

## ------------------------------------------------------------------------
ggparty(py) +
  geom_edge() +
  geom_edge_label(parse = FALSE) +
  geom_node_splitvar() +
  geom_node_info()

## ------------------------------------------------------------------------
ggparty(py) +
  geom_edge() +
  geom_edge_label(parse_all = TRUE) +
  geom_node_splitvar() +
  geom_node_info()

## ------------------------------------------------------------------------
ggparty(py) +
  geom_edge() +
  geom_edge_label(mapping = aes(label = paste(breaks_label)),
                  parse_all = FALSE #has no effect
                  ) +
  geom_node_splitvar() +
  geom_node_info()

## ------------------------------------------------------------------------
ggparty(py) +
  geom_edge() +
  geom_edge_label(mapping = aes(label = paste(breaks_label)),
                  ids = 2,
                  parse = FALSE
                  ) +
  geom_edge_label(mapping = aes(label = paste(breaks_label)),
                  ids = -2,
                  parse = TRUE
                  ) +
  geom_node_splitvar() +
  geom_node_info()

## ------------------------------------------------------------------------
ggparty(py) +
  geom_edge() +
  geom_edge_label(mapping = aes(label = paste(breaks_label, "*NA^", id))) +
  geom_node_splitvar() +
  geom_node_info()

## ------------------------------------------------------------------------
ggparty(py) +
  geom_edge() +
  geom_edge_label(mapping = aes(label = paste0(breaks_label, "*\"NA^\"*", 1:8))) +
  geom_node_splitvar() +
  geom_node_info()

## ------------------------------------------------------------------------
library(MASS)
SexTest <- ctree(sex ~ ., data = Aids2)
ggparty(SexTest) +
  geom_edge() + 
  geom_edge_label(splitlevels = 1:2, nudge_y = 0.025) +
  geom_edge_label(splitlevels = 3:4, nudge_y = -0.025) +
  geom_node_splitvar() +
  geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = sex),
                                        position = position_fill())),
                 shared_axis_labels = TRUE)

## ------------------------------------------------------------------------
library(MASS)
SexTest <- ctree(sex ~ ., data = Aids2)
ggparty(SexTest) +
  geom_edge() + 
  geom_edge_label(max_length = 3) +
  geom_node_splitvar() +
  geom_node_plot(gglist = list(geom_bar(aes(x = "", fill = sex),
                                        position = position_fill())),
                 shared_axis_labels = TRUE)

