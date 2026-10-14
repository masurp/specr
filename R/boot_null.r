#' Inference with specification curve analysis
#'
#' @description `r lifecycle::badge("deprecated")`
#'    This function was renamed to [infer_curve()], which also corrects how the
#'    data are resampled under the null. `boot_null()` now simply calls
#'    [infer_curve()] and returns an object of class `specr.infer`.
#'
#' @param x A fitted `specr.object`, resulting from running [specr()].
#' @param y A `specr.setup` object, resulting from running [setup()].
#' @param n_samples Number of bootstrap samples to be drawn. Defaults to 500.
#' @param ... Further arguments passed to [infer_curve()].
#'
#' @return An object of class `specr.infer` (see [infer_curve()]).
#'
#' @keywords internal
#' @export
boot_null <- function(x, y, n_samples = 500, ...) {

  lifecycle::deprecate_warn("1.0.1", "boot_null()", "infer_curve()")

  infer_curve(x, data = y, n_samples = n_samples, ...)

}
