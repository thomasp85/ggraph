# We need this so that edge geoms is consider 'points' by gganimate. This is
# safe as all edges have the same number of points. We don't want to depend on
# gganimate so we register it on load
layer_type.GeomEdgePath <- function(x) 'point'
