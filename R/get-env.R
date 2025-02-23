#' Describe the contents of an environment
#'
#' @param environment An environment. Optional, defaults to global environment.
#' @param items Optional. A character vector of objects in the environment to
#' describe.
#'
#' @returns
#' A string describing the environment contents with `#>` prefixing
#' each object's printed representation.
#'
#' @inheritSection get_installed_packages See Also
#'
#' @export
get_environment <- function(environment = global_env(), items = NULL) {
  if (!is.environment(environment)) {
    # TODO: does the env name live in the global env?
    # is it in `search_envs`?
    cli::cli_abort("Not implemented yet.")
  }

  res <- character()
  env_item_names <- ls(environment)
  if (!is.null(items)) {
    env_item_names <- env_item_names[env_item_names %in% items]
  }

  for (item_name in env_item_names) {
    item <- env_get(environment, item_name)

    if (inherits(item, "Chat")) next

    if (inherits(item, c("data.frame", "tbl"))) {
      item_desc <- strsplit(get_data_frame(item), "\n")[[1]]
    } else if (inherits(item, "function")) {
      # TODO: this should be a `get_function()` or something
      package_topic <- strsplit(item_name, "::", fixed = TRUE)[[1]]
      item_desc <- tryCatch(
        get_help_page(package_topic[1], package_topic[2]),
        error = function(e) capture.output(item)
      )
    } else {
      item_desc <- capture_print(item)
    }

    item_res <- c(item_name, paste0("#> ", item_desc), "\n")
    res <- c(res, item_res)
  }

  paste0(res, collapse = "\n")
}

tool_get_environment <- function() {
  ellmer::tool(
    get_environment,
    "Retrieve specified items from a given environment.",
    items = ellmer::type_array(
      "The names of items to retrieve from the environment. Defaults to `NULL`, indicating all
  items.",
      items = ellmer::type_string()
    )
  )
}

capture_print <- function(item) {
  out <- capture.output(print(item))
  if (length(out) && nzchar(out)) return(out)

  capture.output(print(item), type = "message")
}
