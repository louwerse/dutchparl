
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dutchparl

<!-- badges: start -->
<!-- badges: end -->

dutchparl - An R package for the Dutch Parliamentary Behaviour Dataset

This package allows users to work easily with the Dutch Parliamentary
Behaviour Dataset. There are multiple datasets: one on votes and one on
parliamentary (written) questions.

### Installation

To install this package in R:

``` r
install.packages("devtools")
devtools::install_github("louwerse/dutchparl")
```

### Download data

The data can be downloaded from Harvard Dataverse:
<https://dataverse.harvard.edu/dataverse/dutchparl>

You can download the voting data in R and save it to a file in your
current working directory (default = “votes.rds”).

``` r
downloadVotes()
```

You can download the questions data in R and save it to a file in your
current working directory (default = “questions.rds”).

``` r
downloadQuestions()
```

### Work with the data

Load the package dutchparl. This includes two example objects
`examplevotes` and `examplequestions`, which can be used to demonstrate
how the package works. The fulldataset is not included with the package,
due to its size (see above for downloading).

``` r
library("dutchparl")
data = examplevotes
summary(data)
#> Number of votes in dataset:  61 
#> Category count: 
#> 
#>        Cultuur en recreatie                    Economie 
#>                           5                          19 
#>                   Financiën              Internationaal 
#>                           5                           2 
#>            Natuur en milieu     Onderwijs en wetenschap 
#>                          21                          47 
#> Openbare orde en veiligheid                       Recht 
#>                           1                          13 
#>           Sociale zekerheid                     Verkeer 
#>                           5                          10 
#>                        Werk          Zorg en gezondheid 
#>                          16                          12 
#> Parties appearing at least  61  times in the dataset:
#>  [1] "CDA"     "CU"      "D66"     "GL"      "PvdA"    "PvdD"    "PVV"    
#>  [8] "SGP"     "SP"      "Verdonk" "VVD"
```

You can open the full version of the data using:

``` r
votes = loadRDS("votes.rds") # Use name of relevant datafile
```

Available functions can be obtained via:

``` r
help(package="dutchparl")
```
