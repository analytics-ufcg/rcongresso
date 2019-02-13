.HTTP_CACHE <- new.env(parent = emptyenv())
.cache_dir_path <- file.path(system.file(package="rcongresso"), "extdata")
.cache_file_path <- file.path(.cache_dir_path, "test_cache.rds")

#' Setup cache
#' @param name Name of the cache to use. NULL for none.
#' @export
setup_cache <- function (name) {
    options("rcongresso.cache"=name)
}

#' Save the cache to a file
.save_cache <- function() {
    dir.create(.cache_dir_path, showWarnings = FALSE)
    saveRDS(.HTTP_CACHE, .cache_file_path)
}

#' Stores a value in the cache.
#' @param key Key to store
#' @param value Value to store
.put_in_cache <- function(key, value) {
    if (is.null(getOption("rcongresso.cache"))) return(NULL)

    # Sometimes (after package installation) it's a locked list
    if (typeof(.HTTP_CACHE) == "environment") {
      assign(key, value, envir=.HTTP_CACHE)
      .save_cache()
    }
}

#' Gets a value from the cache.
#' @param key Key to get
.get_from_cache <- function(key) {
    if (is.null(getOption("rcongresso.cache"))) return(NULL)

    if (length(.HTTP_CACHE) == 0) {
        tryCatch({
            cache <- readRDS(.cache_file_path)
            for( k in names(cache) ) assign(k, cache[[k]], envir=.HTTP_CACHE)
        }, warning = function(w) {
        }, error = function(error_condition) {
        })
    }
    if (typeof(.HTTP_CACHE) == "environment") {
        # Sometimes it's an environment
        value <- tryCatch({
            get(key, envir=.HTTP_CACHE)
        }, error = function(e) {
            NULL
        })
    } else {
        # Sometimes it's a list (when checking package)
        value <- .HTTP_CACHE[[key]]
    }

    if (is.null(value)) {
        print(c("Not in cache", .cache_file_path))
    } else {
        print(c("In cache", .cache_file_path))
    }
    return(value)
}
