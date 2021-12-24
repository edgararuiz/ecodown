# tidypredict_interval


Returns a Tidy Eval formula to calculate prediction interval.




## Description

It parses a model or uses an already parsed model to return a
Tidy Eval formula that can then be used inside a dplyr command.





## Usage
```r
tidypredict_interval(model, interval = 0.95)
```




## Arguments


Argument      |Description
------------- |----------------
model | An R model or a list with a parsed model
interval | The prediction interval, defaults to 0.95




## Details

The result still has to be added to and subtracted from the fit to obtain the upper and
lower bound respectively.






## Examples
```r

model <- lm(mpg ~ wt + cyl * disp, offset = am, data = mtcars)
tidypredict_interval(model)
```



