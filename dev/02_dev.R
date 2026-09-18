# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module
golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module
golem::add_module(name = "chargement", with_test = FALSE) # Name of the module
golem::add_module(name = "Accueil", with_test = FALSE) # Name of the module

golem::add_module(name = "Descriptifs", with_test = FALSE) # Name of the module
golem::add_module(name = "Croisements", with_test = FALSE) # Name of the module
golem::add_module(name = "Survie", with_test = FALSE) # Name of the module
golem::add_module(name = "Tests", with_test = FALSE) # Name of the module
golem::add_module(name = "Concordance", with_test = FALSE) # Name of the module
golem::add_module(name = "SaisieManuelle", with_test = FALSE) # Name of the module
golem::add_module(name = "Redaction", with_test = FALSE) # Name of the module


golem::add_fct( "fonctions" ) 
golem::add_fct( "code_sans_dep" ) 
## 2.2 Add dependencies

usethis::use_package( "shiny" ) 
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinyFiles" )
usethis::use_package( "irr" )
# gdata removed (archived from CRAN): the former gdata::reorder.factor() call
# is now replaced by the internal reorder_factor_levels() helper.
usethis::use_package( "boot" )
usethis::use_package( "xtable" )
usethis::use_package( "dplyr" )
usethis::use_package( "ggplot2" )
#usethis::use_package( "DataExplorer" )
usethis::use_package( "ggthemes" )
usethis::use_package( "pROC" )
usethis::use_package( "dashboardthemes" )
usethis::use_package( "shinydashboardPlus")
usethis::use_package("utils" )
usethis::use_package("desctable" )
usethis::use_package("moments" )




## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("gmrc")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
