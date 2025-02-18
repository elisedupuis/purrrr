## Code from Maelle Salmon's blog
## Reducing my for loop usage with purrr::reduce()
## https://masalmon.eu/2023/07/26/reduce/

library(purrr)
library(ggplot2)
library(dplyr)

#  Silly example-----

# Some basic movie information
movies <- tibble::tribble(
  ~title, ~color, ~elements,
  "Barbie", "pink", "shoes",
  "Oppenheimer", "red", "history"
)

# More information to add to movies
info_list <- list(
  list(title = "Barbie", info = list(element = "sparkles")),
  list(title = "Barbie", info = list(element = "feminism")),
  list(title = "Oppenheimer", info = list(element = "fire"))
)

# Don't tell me this is weirdly formatted data,
# who never obtains weirdly formatted data?!
info_list


add_element <- function(movies, info) {
  movies[movies[["title"]] == info[["title"]],][["elements"]] <-
    toString(c(
      movies[movies[["title"]] == info[["title"]],][["elements"]],
      info[["info"]][[1]]
    ))
  movies
}

add_element(movies, info_list[[3]])



purrr::reduce(info_list, add_element, .init = movies)


add_element <- function(movies, info, separator) {
  movies[movies[["title"]] == info[["title"]],][["elements"]] <-
    paste(c(
      movies[movies[["title"]] == info[["title"]],][["elements"]],
      info[["info"]][[1]]
    ), collapse = separator)
  movies
}

purrr::reduce(
  info_list, 
  \(movies, x) add_element(movies, x, separator = " - "), 
  .init = movies
)

# From June Choe blog---------

# Collapse repetitive piping with reduce()

reduce(
  c(8, 4, 2),
  ~ .x + geom_point(size = .y, alpha = .5),
  .init = ggplot(mtcars, aes(hp, mpg))
)

viridis_colors <- viridis::viridis(10)
 
reduce(
  10L:1L,
  ~ .x + geom_point(size = .y * 2, color = viridis_colors[.y]),
  
  .init = mtcars %>% 
    ggplot(aes(hp, mpg)) +
    scale_x_discrete(expand = expansion(.2)) +
    scale_y_continuous(expand = expansion(.2)) +
    theme_void() +
    theme(panel.background = element_rect(fill = "grey20"))
  
)


(mtcars %>% 
  ggplot(aes(hp, mpg)) +
  scale_x_discrete(expand = expansion(.2)) +
  scale_y_continuous(expand = expansion(.2)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "grey20"))) %>% 
  
  reduce(
    10L:1L,
    ~ .x + geom_point(size = .y * 2, color = viridis_colors[.y]),
    .init = . #<- right here!
  )

  new_cols <- c("a", "b", "c")

  mtcars %>% 
    head() %>% 
    select(mpg) %>% 
    mutate(!!new_cols[1] := NA) %>% 
    mutate(!!new_cols[2] := NA) %>% 
    mutate(!!new_cols[3] := NA)
  