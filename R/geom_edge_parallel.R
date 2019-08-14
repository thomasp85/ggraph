#' Draw multi edges as parallel lines
#'
#' This geom draws edges as parallel lines in non-simple graphs. 
#'
#' @inheritSection geom_edge_link Edge variants
#' @inheritSection geom_edge_link Edge aesthetic name expansion
#'
#' @section Aesthetics:
#' `geom_edge_parallel` and `geom_edge_parallel0` understand the following
#' aesthetics. Bold aesthetics are automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **xend**
#' - **yend**
#' - **from**
#' - **to**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_parallel2` understand the following aesthetics. Bold aesthetics are
#' automatically set, but can be overridden.
#'
#' - **x**
#' - **y**
#' - **group**
#' - **from**
#' - **to**
#' - edge_colour
#' - edge_width
#' - edge_linetype
#' - edge_alpha
#' - filter
#'
#' `geom_edge_parallel` and `geom_edge_parallel2` furthermore takes the following
#' aesthetics.
#'
#' - start_cap
#' - end_cap
#' - label
#' - label_pos
#' - label_size
#' - angle
#' - hjust
#' - vjust
#' - family
#' - fontface
#' - lineheight
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{index}{The position along the path (not computed for the *0 version)}
#' }
#'
#' @inheritParams geom_edge_link
#' @inheritParams ggplot2::geom_path
#'
#' @param spread Modify the distance between parallel lines 
#'
#' @author David Schoch
#'
#' @family geom_edge_*
#'
#' @examples
#' require(tidygraph)
#' gr <- as_tbl_graph(data.frame(
#'   from = c(1, 1, 1, 1, 1, 2, 2, 2),
#'   to = c(2, 2, 2, 2, 2, 1, 1, 1),
#'   class = sample(letters[1:3], 8, TRUE)
#' )) %>%
#'   mutate(class = c('a', 'b'))
#'
#' ggraph(gr, 'nicely') +
#'   geom_edge_parallel(aes(alpha = ..index..))
#'
#' ggraph(gr, 'nicely') +
#'   geom_edge_parallel2(aes(colour = node.class))
#'
#' ggraph(gr, 'nicely') +
#'   geom_edge_parallel0(aes(colour = class))
#' @rdname geom_edge_parallel
#' @name geom_edge_parallel
#'
NULL

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @importFrom ggforce StatLink
#' @export

StatEdgeParallel <- ggproto('StatEdgeParallel', StatLink,
  setup_data = function(data, params) {
    if (any(names(data) == 'filter')) {
      if (!is.logical(data$filter)) {
        stop('filter must be logical')
      }
      data <- data[data$filter, names(data) != 'filter']
    }
    data <- remove_loop(data)
    if (nrow(data) == 0) return(NULL)
    data <- create_parallels(data,params)
    StatLink$setup_data(data, params)
  },
  default_aes = aes(filter = TRUE),
  extra_params = c("na.rm","n",'spread')
)

#' @rdname geom_edge_parallel
#'
#' @importFrom ggforce StatLink
#' @export
geom_edge_parallel <- function(mapping = NULL, data = get_edges(),
                               position = "identity", arrow = NULL, spread = 0.02,
                               n = 100,
                               lineend = "butt", linejoin = "round", linemitre = 1,
                               label_colour = 'black',  label_alpha = 1,
                               label_parse = FALSE, check_overlap = FALSE,
                               angle_calc = 'rot', force_flip = TRUE,
                               label_dodge = NULL, label_push = NULL,
                               show.legend = NA, ...) {
  mapping <- complete_edge_aes(mapping)
  mapping <- aes_intersect(mapping, aes_(x=~x, y=~y, xend=~xend, yend=~yend,
                                         from=~from, to=~to))
  layer(data = data, mapping = mapping, stat = StatEdgeParallel,
        geom = GeomEdgePath, position = position, show.legend = show.legend,
        inherit.aes = FALSE,
        params = expand_edge_aes(
          list(arrow = arrow, lineend = lineend, linejoin = linejoin,
               linemitre = linemitre, na.rm = FALSE, spread = spread, n = n,
               interpolate = FALSE,
               label_colour = label_colour, label_alpha = label_alpha,
               label_parse = label_parse, check_overlap = check_overlap,
               angle_calc = angle_calc, force_flip = force_flip,
               label_dodge = label_dodge, label_push = label_push, ...)
        )
  )
}

#' @importFrom dplyr %>% group_by_ arrange_ summarise_ n ungroup transmute_
create_parallels <- function(data,params) {
  data$origID <- 1:nrow(data)
  data$.id <- paste(pmin(data$from, data$to), pmax(data$from, data$to), sep = '-')
  data <- data %>% arrange(.id)
  medge <- data %>% 
    group_by_(~PANEL, ~.id) %>% 
    summarise_(medge = ~n()) %>%
    ungroup() %>% 
    transmute_(medge = ~medge)
  
  medge <- medge$medge  
  medge_shift <- unlist(sapply(medge,dseq,spread=params$spread))
  data$shift <- medge_shift*ifelse(data$from<data$to,1,-1)
  shifted <- t(apply(data[,c("x","y","xend","yend","shift")],1,
                     function(x) edge_shift(x[1],x[2],x[3],x[4],x[5])))
  
  data[,c("x","y","xend","yend")] <- shifted 
  data <- data %>% arrange(origID)
  data[["shift"]] <- NULL
  data[["origID"]] <- NULL
  data[[".id"]] <- NULL
  data[["from"]] <- NULL
  data[["to"]] <- NULL
  data
}

dseq <- function(n,spread){
  seq(-(n-1)*spread/2,(n-1)*spread/2,by=spread)
}
edge_shift <- function(x1,y1,x2,y2,shift){
  
  v <- c(x2 - x1,y2 - y1)
  v <- v/sqrt((v[1]^2 + v[2]^2))
  v_perp <- c( -v[2], v[1] )
  
  return(c(x1 + shift*v_perp[1],y1 + shift*v_perp[2],
           x2 + shift*v_perp[1],y2 + shift*v_perp[2]))
  
}

