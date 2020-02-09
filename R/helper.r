#'@export

.switchFormula <- function(formulaDegree){
    formula <- switch(formulaDegree,
                      {"covariateValue ~ time"},
                      {"covariateValue ~ time + I(time^2)"},
                      {"covariateValue ~ time + I(time^2) + I(time^3)"},
                      {"covariateValue ~ time + I(time^2) + I(time^3) + I(time^4)"},
                      {"covariateValue ~ time + I(time^2) + I(time^3) + I(time^4) + I(time^5)"}, 
                      {"covariateValue ~ time + I(time^2) + I(time^3) + I(time^4) + I(time^5) + I(time^6)"},
                      {"covariateValue ~ time + I(time^2) + I(time^3) + I(time^4) + I(time^5) + I(time^6) + I(time^7)"})
    
    mixture <-  switch(formulaDegree,
                       {"~ time"},
                       {"~ time + I(time^2)"},
                       {"~ time + I(time^2) + I(time^3)"},
                       {"~ time + I(time^2) + I(time^3) + I(time^4)" },
                       {"~ time + I(time^2) + I(time^3) + I(time^4) + I(time^5)"}, 
                       {"~ time + I(time^2) + I(time^3) + I(time^4) + I(time^5) + I(time^6)"},
                       {"~ time + I(time^2) + I(time^3) + I(time^4) + I(time^5) + I(time^6) + I(time^7)"})
    
    return(list(formula = formula,
                mixture = mixture))
}
