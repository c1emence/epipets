#Playspace


# Make a Dataset
set.seed(0905)

n <- 100

store <- data.frame(
  id = 1:n,
  produce_monday = sample(c("apple", "banana", "kiwi", "brocolli", "potato", "lettuce"), n, replace = TRUE),
  produce_tuesday = sample(c("apple", "banana", "kiwi", "brocolli", "potato", "lettuce"), n, replace = TRUE),
  produce_wednesday = sample(c("apple", "banana", "kiwi", "brocolli", "potato", "lettuce"), n, replace = TRUE)
)

head(store)

# Define a group

fruits <- c("apple", "banana", "kiwi")
veggies <- c("brocolli", "potato", "lettuce")

shopping_list <- list(
  fruits = fruits,
  veggies = veggies
  )

# Use normal functions

sum(store $ produce_monday %in% fruits)


store |>
  dplyr::mutate(
    hit = dplyr::if_any(
      dplyr::starts_with("produce_"),
      ~ .x %in% fruits
    )
  )
store |>
  dplyr::summarise(
    n = sum(
      dplyr::if_any(
        dplyr::starts_with("produce_"),
        ~ .x %in% fruits
      )
    )
  )


