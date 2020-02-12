fit_models <- function(xf, ind_var, dep_var, start, end, forecasts){
  # Not tested
  fits <- lapply(forecasts, function(){
  switch(forecast,
         'holtwinters' = get_holtwinters(),
         'decomp' = get_decomp(),
         'naive' = get_naive(),
         'linear' = get_linear(),
         # Default case
         {warning('Forecast type not recogniese')
           NULL})

  })

}

fit_holtwinters <- function(df, ind_var, start, end){
  # Not tested
  stats::HoltWinters(xf[start:end,ind_var])
}

fit_decomp <- function(df, ind_var, start, end){

}

fit_naive <- function(df, ind_var, start, end) {

}

fit_linear <- function(df, ind_var, dep_var, start, end){

}

predict_models <- function(fits, )
