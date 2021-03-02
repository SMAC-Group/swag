## swag 0.1.0

- 01.03.2021 Add method `summary` that returns the number and proportion of appearance of each variables on a subset of selected model. The selection procedure proceed in two steps, first we select a dimension in which the `mean`, `min` or `median` is the lowest. Then we compute the selected percentile of the CV error on this dimension. We then select all models in all explored dimensions that have a lower CV error than the CV value set.

- 29.02.2021 Added support for regression problem. Changed the function swag to accept "Accuracy" as training loss function and specify the workflow to latter accept other loss function by specifying a procedure based on this loss function (see code of `swag`). Changed  the function `auto_swagControl` to have different behavior depending on the type of procedure.

- First version for submission to CRAN. The package is made available on GitHub ([github.com/SMAC-Group/SWAG-R-Package/](https://github.com/SMAC-Group/SWAG-R-Package/))
