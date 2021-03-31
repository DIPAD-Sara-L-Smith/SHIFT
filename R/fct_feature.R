# Functions for feature engineering with recipes package

#' Bake a recipe
#' @param data dataframe containing the data (r$data)
#' @param dep_var the dependant variable
#' @param ind_vars the independant variables
#' @param vars_center list of variables for center transformation
#'
#' @return a baked data frame
#'
#' @import recipes
#'
#' @export
bake_recipe <- function(data, dep_var, ind_vars, vars_center) {
  # get subset of data based on dep_var and ind_vars
  pre_bake_data <-
    data %>%
    tidyr::drop_na() %>%
    dplyr::select(dep_var, ind_vars)

  # model string from vars
  model <- paste(dep_var, " ~ " , paste0(ind_vars, collapse = " + "))

  # make a recipe (need a model)
  recipe <- recipe(as.formula(model), data = pre_bake_data)

  # add all the step to the recipe
  # box cox
  if(!is.null(vars_center)){
    recipe <-
      recipe %>%
      step_center(all_of(vars_center))
  }

  # prep the recipe
  #recipe_prep <- prep(recipe)

  # bake the recipe
  baked_data <-
    recipe %>%
    prep(retain = TRUE) %>%
    bake(new_data = NULL)

  # return the baked data
  return(baked_data)

}

# TODO add the graphing stuff here

# function to produce graphs or the grid.arrange
