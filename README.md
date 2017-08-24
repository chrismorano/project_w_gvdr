# Reoperation predictions
## Detecting the insurgence of post-operatory critical clinical conditions through drain values.

This repository contains the sanitised data and the code of a research project that aims to provide Medical Doctors a framework for the early detection of post-operatory clinical conditions using seven days of medical test results. In order to keep the data fully anonymized, the type of medical condition that is being evaluated will not be mentioned, nor will be the location, name and identifying personal details of the patients.  

The goal of this project is to create an R Shiny dashboard for medical professionals so that they can input the test results they have collected and assess the probability that the patient will need further treatment. Our research also offers recommendations for how to better collect the necessary data, resulting in better treatment of these types of patients.

The models and Shiny app are currently being developed by Dr. Chris Morano under the supervision and guidance of Dr. Giulio Dalla Riva (https://www.gvdallariva.net).

The dependencies for this project are: `R`, `tidyverse`, `caret`, `randomForest`, `lime`, and `Shiny`.
