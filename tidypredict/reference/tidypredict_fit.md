# tidypredict_fit


Returns a Tidy Eval formula to calculate fitted values




## Description

It parses a model or uses an already parsed model to return a
Tidy Eval formula that can then be used inside a dplyr command.





## Usage
```r
tidypredict_fit(model)
```




## Arguments


Argument      |Description
------------- |----------------
model | An R model or a list with a parsed model.






## Examples
```r

model <- lm(mpg ~ wt + cyl * disp, offset = am, data = mtcars)
tidypredict_fit(model)
```



