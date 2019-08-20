# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of LcmmOhdsi
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' set arguments for lcmm
#' @param fixedArg          should be one integer
#' @param mixtureArg        should be vector of integer
#' @param randomArg         should be vector of integer
#' @param classNum          should be vector of integer. Default is 2 to 7 (2:7)
#' @param randomSeed        should be one integer
#' @export
#' 
setLcmm <- function(fixedArg= c(2),
                                       mixtureArg= c(1,2),
                                       randomArg= c(1,2),
                                       classNum= 2:7,
                                       randomSeed = 1234){
    if(sum(mixtureArg>fixedArg)) stop ('Every element in mixtureArg should be equal to or less than the fixedArg')
    if(sum(randomArg>fixedArg)) stop ('Every element in mixtureArg should be equal to or less than the fixedArg')
    if(length(randomSeed)>1)
        stop ('random seed must be one')
    result <-split(expand.grid(
        fixedArg = fixedArg,
        mixtureArg = mixtureArg,
        randomArg = randomArg,
        classNum=classNum,
        randomSeed=randomSeed
    ),1:(length(fixedArg)*length(mixtureArg)*length(randomArg)*length(classNum)))
    
    class(result) <- "lcmmModelSettings"
    return(result)
}

#' run lcmm
#' @param modelSettings          resultant list from the setLcmm function
#' @param data        should be vector of integer
#' @param randomArg         should be vector of integer
#' @param classNum          should be vector of integer. Default is 2 to 7 (2:7)
#' @param randomSeed        should be one integer
#' @export
#' 
runLcmm<- function(modelSettings,data){
    #if(class(modelSettings)!="lcmmModelSettings") stop ('modelSettings should be a resultant list from the setLcmm function')
    
    switch(modelSettings$fixedArg,
           {fixedFormula = "covariateValue ~ timeId"},
           {fixedFormula = "covariateValue ~ timeId + I(timeId^2)"},
           {fixedFormula = "covariateValue ~ timeId + I(timeId^2) + I(timeId^3)"})
    
    switch(modelSettings$mixtureArg,
           {mixtureFormula = "~ timeId"},
           {mixtureFormula = "~ timeId + I(timeId^2)"},
           {mixtureFormula = "~ timeId + I(timeId^2) + I(timeId^3)"})
    
    switch(modelSettings$randomArg,
           {randomFormula = "~ timeId"},
           {randomFormula = "~ timeId + I(timeId^2)"},
           {randomFormula = "~ timeId + I(timeId^2) + I(timeId^3)"})
    
    classNum = modelSettings$classNum
    set.seed(modelSettings$randomSeed)
    result<-lcmm::hlme(fixed = as.formula(fixedFormula),
                       mixture =as.formula(mixtureFormula),
                       random = as.formula(randomFormula),
                       subject = "rowId",
                       ng = classNum,
                       data = data)
    
    return (list (fixedFormula = fixedFormula,
                  mixtureFormula = mixtureFormula,
                  randomFormula = randomFormula,
                  classNum = classNum,
                  result = result)
    )
}
