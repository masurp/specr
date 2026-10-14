# Return tibble from specr.infer

Long format with one row per bootstrap sample and specification.

## Usage

``` r
# S3 method for class 'specr.infer'
as_tibble(x, ...)
```

## Arguments

- x:

  An object of class "specr.infer".

- ...:

  further arguments (currently ignored).

## Value

A tibble with the estimates and p-values under the null (one row per
bootstrap sample and specification).
