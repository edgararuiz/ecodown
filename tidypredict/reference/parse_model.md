# parse_model


Converts an R model object into a table.




## Description

It parses a fitted R model's structure and extracts the components
needed to create a dplyr formula for prediction. The function also
creates a data frame using a specific format so that other
functions in the future can also pass parsed tables to a given
formula creating function.





## Usage
```r
parse_model(model)
```




## Arguments


Argument      |Description
------------- |----------------
model | An R model object.






## Examples
```r
library(dplyr)
df <- mutate(mtcars, cyl = paste0("cyl", cyl))
model <- lm(mpg ~ wt + cyl * disp, offset = am, data = df)
parse_model(model)
```



