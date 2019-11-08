
# choices <- function(independent,
#                     dependent,
#                     results,
#                     control = NA,
#                     subset = NA,
#                     transform = NA) {
#
#   if (!is.na(control)) {
#     control <- list(control %>% paste(collapse=" + "),
#                     map(1:length(control), ~ control[[.x]]), "") %>%
#       unlist
#
#   }
#
#   results_frame <- expand.grid(independent = independent,
#                                dependent = dependent,
#                                control = control,
#                                subset = subset,
#                                transform = transform) %>%
#     tbl_df
#   results_frame[, results] <- NA
#
#   return(results_frame)
# }
#
#
# results_frame <- results_frame[1:100,]
#
#
# for (i in 1:nrow(results_frame)) {
#   if((i/50)%%1 == 0){
#     # Show progress every 50 models
#     print(i)
#   } else {}
#   results_frame[i, results_frame_colnames] <-
#     get_fit(
#       get_model(independent = results_frame$independent[i],
#                 dependent = results_frame$dependent[i],
#                 control = results_frame$control[i]),
#       gender = results_frame$gender[i],
#       data_transform = results_frame$data_transform[i])
# }
#
# library(furrr)
# plan(multiprocess)
#
# a = future_map(1:nrow(results_frame), ~
#   get_fit(
#     get_model(independent = results_frame$independent[.x],
#               dependent = results_frame$dependent[.x],
#               control = results_frame$control[.x]),
#     gender = results_frame$gender[.x],
#     data_transform = results_frame$data_transform[.x]))
#
# a %>% map(~data.frame(x = .x)) %>%
#   map(rownames_to_column) %>%
#   map_df(~spread(.x, rowname, x)) %>%
#   as_tibble()
#
# choices("x", "y", "b") %>% map(1:nrow(.), ~
#                                  get_fit(
#                                    get_model(independent = results_frame$independent[.x],
#                                              dependent = results_frame$dependent[.x],
#                                              control = results_frame$control[.x]),
#                                    gender = results_frame$gender[.x],
#                                    data_transform = results_frame$data_transform[.x]))
#
# log_trans = function(df) {
#   df %>% mutate(log_y = log(y))
# }
#
# mytransforms = list(logt = log_trans)
