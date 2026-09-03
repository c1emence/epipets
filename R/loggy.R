#' Loggy (Simple Logistic Regression) from EpiPets
#'
#' I expect you to know how logistic regression works :P
#' (jk thats why i made this function)
#' Function to Produce ORs, 95\% CI, and P-Values from logistic regression.
#'
#'@param dataframe A data frame.
#'@param y A binary dependent variable.
#'@param x An independent variable.
#'
#'@return A data frame containing the odds ratio, 95\% confidence interval, and p-value for each model term. For categorical predictors, ensure the desired reference level is set before fitting the model.
#'
#'@examples
#' # Binary outcome with continuous predictor
#' loggy(mtcars, am, wt)
#'
#' # Binary outcome with binary predictor
#' loggy(mtcars, am, vs)
#'
#'@export

# 09/02/2026 by Clemence Fichet
# \u2013 is ASCII for "-"

loggy <- function(dataframe, y, x) {

  formula <- substitute(y ~ x)

  model <- stats::glm(
    formula,
    data = dataframe,
    family = "binomial"
  )

  broom::tidy(model,
       exponentiate = TRUE,
       conf.int = TRUE
  )|>
    dplyr::mutate(
      `OR (95% CI)` = paste0(
        round(estimate, 2),
        " (",
        round(conf.low, 2),
        "\u2013",
        round(conf.high, 2),
        ")"
      ),
      `p-value` = dplyr::if_else(
        p.value < 0.001,
        "<0.001",
        sprintf("%.3f", p.value)
      )
    ) |>
    dplyr::select(
      Variable = term,
      `OR (95% CI)`,
      `p-value`
    )
}
