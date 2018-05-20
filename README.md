# hugo

[![Build Status](https://api.travis-ci.org/hugo4r/hugo.png)](https://travis-ci.org/hugo4r/hugo)
[![Github Issues](http://githubbadges.herokuapp.com/hugo4r/hugo/issues.svg)](https://github.com/hugo4r/hugo/issues)
[![Coverage
Status](https://img.shields.io/codecov/c/github/hugo4r/hugo/master.svg)](https://codecov.io/github/hugo4r/hugo?branch=master)


I'm your personal assistant

## About

Hugo helps keeping results from data analysis in the right order.

## Example

```
library("hugo")
# hugo: I'm Hugo v. 1.0. Ready to work.

hugo_start_investigation("iris_again")
# hugo: I've created a directory iris_again. 
# hugo: Will store there important data

daneO <- hugo_read_data("http://biecek.pl/R/dane/daneO.csv")
# hugo: I've read a data.frame with 97 rows and 9 columns
# hugo: copy of it is stored in iris_again/data/daneO

model <- lm(y ~ ., data = daneO)
hugo_memorise(model)
# hugo: copy of the model object is stored in iris_again/data/model


hugo_train_model(daneO, y ~ .)
# hugo: Following variables are related with y
# hugo: x1 (0.95), x5 (0.55), x3 (0.15), ...


hugo_continue_investigation("iris_again")
# hugo: following objects are restored: daneO, model

```

