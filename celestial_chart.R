library(magrittr)


exopl <- readr::read_csv("exopl.csv")

#define celestial coordinates to cartesian
celtocart <- function(right_asc, decl, dist) {
  x <- dist * cos(pi * decl / 180) * cos(pi * right_asc / 180) 
  y <- dist * cos(pi * decl / 180) * sin(pi * right_asc / 180) 
  z <- dist * sin(pi * decl / 180)
  
  return(list(x,y,z))
}

exopl <- exopl %>% 
  dplyr::rowwise() %>%
  dplyr::mutate_at(c(x,y,z), celtocart(ra, dec, st_dist))


p <- plotly::plot_ly(
  exopl, x = ~x, y = ~y, z = ~z
) %>%
  plotly::add_markers() %>%
  plotly::layout(
    scene = list(xaxis = list(title = 'Vernal equinox'),
                 yaxis = list(title = ''),
                 zaxis = list(title = 'North'))
  )

p
