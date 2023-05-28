# qacMiss

<img src="image.jpg" width="300"/>

The goal of qacMiss is to identify, visualize, and impute missing data in R data frames. Imputation methods include kth nearest neighbor, random forest, and missing value imputation via chained equations (mice).

## Installation

You can install the development version of qacMiss like so:

``` r
if (!require("remotes")){
  install.packages("remotes")
}
remotes::install_github("rkabacoff/qacMiss")
```

## Example

The following code demonstrates the major functions:

``` r
## basic example code
library(qacMiss)
col_miss(mtcars2)
row_miss(mtcars2)
df_imputed <- impute(mtcars2)
```
