load("Rennetworks.RData")

library(ggraph)

# graphs - BOY
g1graph <- ggraph(g1) +
  geom_edge_link(arrow=arrow(length=unit(2,'mm')), end_cap = circle(5, 'mm')) +
  geom_node_point(aes(color=Level, size=degree2, shape=`Took Survey`), show.legend=c(color=TRUE, size=FALSE)) +
  geom_node_text(aes(label=degree2),color="white") +
  scale_size(range = c(3, 7),name="Times Chosen") +
  facet_edges(~questiontype) +
  theme_graph() +
  theme(text=element_text(family="Poppins"),
        plot.margin= unit(c(0,0,0,0),"cm")) +
  scale_color_manual(values=c("#c7c7c7","#211651","#97daeb"),name="School Level Taught") +
  scale_shape_manual(values = c(15, 19))
g2graph <- ggraph(g2) +
  geom_edge_link(arrow=arrow(length=unit(2,'mm')), end_cap=circle(5,'mm')) + 
  geom_node_point(aes(color=Level, size=degree2, shape=`Took Survey`),show.legend=c(color=TRUE, size=TRUE)) +
  geom_node_text(aes(label=degree2),color="white")+
  scale_size(range = c(3, 7),name="Times Chosen") +
  facet_edges(~questiontype) +
  theme_graph() +
  theme(text=element_text(family="Poppins"),
        plot.margin= unit(c(0,0,0,0),"cm")) +
  scale_color_manual(values=c("#c7c7c7","#211651","#97daeb"),name="School Level Taught") +
  scale_shape_manual(values = c(15, 19))
combined <- g1graph + g2graph & theme(legend.position="bottom")
combined + 
  plot_layout(guides="collect") + 
  plot_annotation(title="Renaissance Beginning of Year Staff Survey",
                  subtitle="Number denotes number of times someone named them",
                  caption=str_wrap("Staff who did not take the survey and were not named have been removed from the graph",50))

g3graph <- ggraph(g3) +
  geom_edge_link(arrow=arrow(length=unit(2,'mm')), end_cap = circle(5, 'mm')) +
  geom_node_point(aes(color=Level, size=degree2, shape=`Took Survey`), show.legend=c(color=TRUE, size=FALSE)) +
  geom_node_text(aes(label=degree2),color="white") +
  scale_size(range = c(3, 7),name="Times Chosen") +
  facet_edges(~questiontype) +
  theme_graph() +
  theme(text=element_text(family="Poppins"),
        plot.margin= unit(c(0,0,0,0),"cm")) +
  scale_color_manual(values=c("#c7c7c7","#211651","#97daeb"),name="School Level Taught") +
  scale_shape_manual(values = c(15, 19))
g4graph <- ggraph(g4) +
  geom_edge_link(arrow=arrow(length=unit(2,'mm')), end_cap=circle(5,'mm')) + 
  geom_node_point(aes(color=Level, size=degree2, shape=`Took Survey`),show.legend=c(color=TRUE, size=TRUE)) +
  geom_node_text(aes(label=degree2),color="white")+
  scale_size(range = c(3, 7),name="Times Chosen") +
  facet_edges(~questiontype) +
  theme_graph() +
  theme(text=element_text(family="Poppins"),
        plot.margin= unit(c(0,0,0,0),"cm")) +
  scale_color_manual(values=c("#c7c7c7","#211651","#97daeb"),name="School Level Taught") +
  scale_shape_manual(values = c(15, 19))
combined2 <- g3graph + g4graph & theme(legend.position="bottom")
combined2 + 
  plot_layout(guides="collect") + 
  plot_annotation(title="Renaissance End of Year Staff Survey",
                  caption=str_wrap("Staff who did not take the survey and were not named have been removed from the graph",50))

combined / combined2 + 
  plot_layout(guides="collect") + 
  plot_annotation(title="Renaissance Staff Survey")

##ERGM time
library(tidyverse)
library(intergraph)
library(ergm)

g1network <- asNetwork(g1)
g2network <- asNetwork(g2)
g3network <- asNetwork(g3)
g4network <- asNetwork(g4)

g1ergm <- ergm(g1network~edges+nodematch("Level"))
summary(g1ergm)

g2ergm <- ergm(g2network~edges+nodematch("Level"))
summary(g2ergm)

g3ergm <- ergm(g3network~edges+nodematch("Level"))
summary(g3ergm)

g4ergm <- ergm(g4network~edges+nodematch("Level"))
summary(g4ergm)

##interpreting the coefficients
inv.logit <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

theta1 <- coef(g1ergm)
theta3 <- coef(g3ergm)

inv.logit(theta1)
inv.logit(theta3)

## Assortativity
igraph::assortativity_nominal(g1, 
                              as.integer(as.factor(V(g1)$Level)),
                              directed=TRUE)

igraph::assortativity_nominal(g3, 
                              as.integer(as.factor(V(g3)$Level)),
                              directed=TRUE)
## assortativity went up, as ergm shows, which is interesting

igraph::assortativity_nominal(g2, 
                              as.integer(as.factor(V(g2)$Level)),
                              directed=TRUE)

igraph::assortativity_nominal(g4, 
                              as.integer(as.factor(V(g4)$Level)),
                              directed=TRUE)
## assortativity went down, as ergm shows, which is interesting

## average degree
degree_distribution(g1, cumulative = FALSE, mode="in", loops=FALSE, normalized=FALSE)

g1degree <- tibble(degree=g1 |> degree(
  v = V(g1),
  mode = c("in"),
  loops = FALSE,
  normalized = FALSE
))

mean(g1degree$degree)
#3.33

g3degree <- tibble(degree=g3 |> degree(
  v = V(g3),
  mode = c("in"),
  loops = FALSE,
  normalized = FALSE
))

mean(g3degree$degree)
#4.05

g2degree <- tibble(degree=g2 |> degree(
  v = V(g2),
  mode = c("in"),
  loops = FALSE,
  normalized = FALSE
))
#2.6

mean(g2degree$degree)

g4degree <- tibble(degree=g4 |> degree(
  v = V(g4),
  mode = c("in"),
  loops = FALSE,
  normalized = FALSE
))

mean(g4degree$degree)
#3.08

g2degree |> 
  ggplot(aes(x=degree)) +
  geom_histogram(binwidth=1,color="red",fill=NA) +
  geom_histogram(data=g4degree,aes(x=degree),binwidth=1,color="blue",fill=NA,linetype="dashed") +
  theme_minimal()