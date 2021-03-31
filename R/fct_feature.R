# Functions for feature engineering with recipes package

#' Bake a recipe
#' @param data dataframe containing the data (r$data)
#' @param dep_var the dependant variable
#' @param ind_vars the independant variables
#' @param box_cox_list list of variables for box cox transformation
#' @import recipes
bake_recipe <- function(data, dep_var, ind_vars, box_cox_list) {
  # get subset of data based on dep_var and ind_vars

  # make a recipe (need a model)

  # add all the step to the recipe

  # box cox

  # prep the recipe

  # bake the recipe

  # return the baked data


}
