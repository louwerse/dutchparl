# dutchparl
dutchparl - An R package for the Dutch Parliamentary Behaviour Dataset

This package allows users to work easily with the Dutch Parliamentary Behaviour Dataset. 


### Installation
To install this package in R:
```r 
install.packages("devtools")
devtools::install_github("louwerse/dutchparl")
``` 

### Download data

The data can be downloaded from Harvard Dataverse: http://dx.doi.org/10.7910/DVN/UXIBNO

Alternatively, you can download the voting data in R and save it to a file in your current working directory (default = "votes.RData").
```r
downloadVotes()
```


### Work with the data
To start working with the data:
```r
library("dutchparl")
load("dpbd_v2.RData")  # Use name of relevant datafile
```

Available functions can be obtained via:
```r
help(package="dutchparl")
```



