# Generated by cpp11: do not edit by hand

cactusTree <- function(parent, order, weight, scale, overlap, upright) {
  .Call(`_ggraph_cactusTree`, parent, order, weight, scale, overlap, upright)
}

pack <- function(areas) {
  .Call(`_ggraph_pack`, areas)
}

circlePackLayout <- function(parent, weight) {
  .Call(`_ggraph_circlePackLayout`, parent, weight)
}

dendrogram_spread <- function(graph, starts, y, leaf, repel, pad, ratio) {
  .Call(`_ggraph_dendrogram_spread`, graph, starts, y, leaf, repel, pad, ratio)
}

force_bundle_iter <- function(edges_xy, K, C, P, P_rate, S, I, I_rate, compatibility_threshold, eps) {
  .Call(`_ggraph_force_bundle_iter`, edges_xy, K, C, P, P_rate, S, I, I_rate, compatibility_threshold, eps)
}

hTree <- function(parent, order) {
  .Call(`_ggraph_hTree`, parent, order)
}

partitionTree <- function(parent, order, weight, height) {
  .Call(`_ggraph_partitionTree`, parent, order, weight, height)
}

cut_lines <- function(x, y, id, start_width, start_height, end_width, end_height, start_type, end_type) {
  .Call(`_ggraph_cut_lines`, x, y, id, start_width, start_height, end_width, end_height, start_type, end_type)
}

pathAttr <- function(group, alpha, width, lty, colour, ngroups) {
  .Call(`_ggraph_pathAttr`, group, alpha, width, lty, colour, ngroups)
}

splitTreemap <- function(parent, order, weight, width, height) {
  .Call(`_ggraph_splitTreemap`, parent, order, weight, width, height)
}

unrooted <- function(parent, order, length, daylight, tol, rotation_mod, maxiter) {
  .Call(`_ggraph_unrooted`, parent, order, length, daylight, tol, rotation_mod, maxiter)
}
