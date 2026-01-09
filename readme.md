# Plotarium


**Create ggplot2 figures — without writing code.**

Plotarium is a personal **R Shiny app** that turns ggplot2 plotting into an intuitive, point-and-click experience.
Upload your dataset, choose variables, customize your plot, and export both the figure and reproducible R code.



## ✨ Features
* Multiple plot types (Scatter, Boxplot, Histogram, Bar, and Line plots)
* Flexible mappings (X, Y, Color, Facet Rows, and Facet Columns)
* Live plot preview  
* Auto-generated ggplot2 code  
* Theme and styling controls  
* Export functionality  
* Dark & Light mode support  

<img src="man/figures/preview_light_darkmode.png" width="80%" style="display: block; margin: auto;" />


## Installation

### Install the development version from GitHub:

``` install
# Install devtools if not already installed
install.packages("devtools")

# Install plotarium from GitHub
devtools::install_github("oli208/plotarium")
```

### Run the App
``` run app
library(plotarium)
run_plotarium()
```


## Architecture
Plotarium uses a modular architecture:
* mod_data – data import & preprocessing
* mod_mapping – aesthetic mappings
* mod_plot – ggplot rendering
* mod_style – themes, labels, advanced styling
* mod_code – code generation
* mod_export – export logic

