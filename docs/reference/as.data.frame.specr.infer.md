# Return data.frame from specr.infer

Return data.frame from specr.infer

## Usage

``` r
# S3 method for class 'specr.infer'
as.data.frame(x, ...)
```

## Arguments

- x:

  An object of class "specr.infer".

- ...:

  further arguments passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Value

A data frame with the estimates and p-values under the null (one row per
bootstrap sample and specification).
