# Create Model Matrices from Formula List

Creates design matrices for each parameter from the parsed formula list.

## Usage

``` r
create_model_matrices(formula_list, data, response_name = NULL)
```

## Arguments

- formula_list:

  Named list of formulas (from `parse_extended_formula`).

- data:

  Data frame containing the variables.

- response_name:

  Name of the response variable (to exclude from predictors).

## Value

Named list of model matrices.
