
# Install if needed:

install.packages("devtools")
install.packages("shiny")
install.packages("shinyWidgets")
install.packages("bslib")
install.packages("bsplus")
install.packages("shinyBS")
install.packages("DT")
install.packages("tidyr")
install.packages("dplyr")


# Install and start doris

devtools::install_github("bayer-group/BIC-doris")
library(doris)

run_doris()
