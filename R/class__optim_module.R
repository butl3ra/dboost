#' @export
optim_module<- function(classname = NULL, inherit = optim_Module, ...,
                        private = NULL, active = NULL,
                        parent_env = parent.frame()) {
  e <- new.env(parent = parent_env)
  e$inherit <- inherit

  inherit_class <- get_inherited_classes(inherit)
  classes <- c(classname, inherit_class, "optim_module")

  Module <- R6::R6Class(
    classname = classname,
    inherit = inherit,
    lock_objects = FALSE,
    public = list(
      .classes = classes,
      ...
    ),
    private = private,
    active = active,
    parent_env = e
  )

  init <- get_init(Module)

  fun <- rlang::new_function(
    args = rlang::fn_fmls(init),
    body = rlang::expr({
      instance <- Module$new(!!!rlang::fn_fmls_syms(init))
      create_optim_module_callable(instance)
    })
  )
  attr(fun, "class") <- c(classes, "optim_module_generator")
  attr(fun, "module") <- Module
  fun
}


#' @export
print_optim_module <- function(self, private) {
  cat('An `optim_module` \n')
}

#' @export
get_init<-function (x)
{
  if (!is.null(x$public_methods$initialize))
    return(x$public_methods$initialize)
  else return(get_init(x$get_inherit()))
}

#' @export
create_optim_module_callable<-function (instance)
{
  f <- instance$solve
  attr(f, "class") <- instance$.classes
  attr(f, "module") <- instance
  f
}

#' @export
get_inherited_classes <- function(inherit) {
  inherit_class <- inherit$public_fields$.classes
  # Filter out classes that we eventually add in our normal flow.
  inherit_class <- inherit_class[inherit_class != "R6ClassGenerator"]
  inherit_class <- inherit_class[!duplicated(inherit_class, fromLast = TRUE)]
  inherit_class
}

#' @export
`$.optim_module` <- function(x, y) {
  module <- attr(x, "module")
  do.call("$", args = list(module, y))
}

#' @export
`[[.optim_module` <- function(x, y) {
  module <- attr(x, "module")
  do.call("[[", args = list(module, y))
}

#' @export
`$.optim_module` <- function(x, y) {
  x[[y]]
}

#' @export
`[[.optim_Module` <- function(x, y) {
  if (y == ".__enclos_env__") {
    return(NextMethod())
  }

  if (is.numeric(y)) {
    return(x[[".__enclos_env__"]][["private"]][["modules_"]][[y]])
  }

  pars <- x[[".__enclos_env__"]][["private"]][["parameters_"]]
  if (!is.null(pars)) {
    o <- pars[[y]]
    if (!is.null(o)) {
      return(o)
    }
  }

  bufs <- x[[".__enclos_env__"]][["private"]][["buffers_"]]
  if (!is.null(bufs)) {
    o <- bufs[[y]]
    if (!is.null(o)) {
      return(o)
    }
  }

  mods <- x[[".__enclos_env__"]][["private"]][["modules_"]]
  if (!is.null(mods)) {
    o <- mods[[y]]
    if (!is.null(o)) {
      return(o)
    }
  }

  find_method <- x[[".__enclos_env__"]][["private"]][["find_method"]]
  if (!is.null(find_method)) {
    o <- find_method(y)
    if (!is.null(o)) {
      return(o)
    }
  }

  NextMethod("[[", x)
}

#' @export
is_optim_module <- function(x) {
  inherits(x, "optim_module") && !inherits(x, "optim_module_generator")
}

#' @export
`[[<-.optim_Module` <- function(x, name, value) {
  if (inherits(value, "nn_parameter")) {
    x$register_parameter(name, value)
  } else if (inherits(value, "nn_buffer")) {
    x$register_buffer(name, value, attr(value, "persistent"))
  } else if (is_optim_module(value)) {
    x$add_module(name, value)
  } else {
    NextMethod("$<-", x)
  }

  invisible(x)
}

#' @export
`$<-.optim_Module` <- function(x, name, value) {
  x[[name]] <- value
  invisible(x)
}

#' @export
`$<-.optim_module` <- function(x, name, value) {
  attr(x, "module")[[name]] <- value
  invisible(x)
}

#' @export
`[[<-.optim_module` <- `$<-.optim_module`

#' @export
names.optim_module <- function(x, ...) {
  x <- attr(x, "module")
  NextMethod("names", x)
}

#' @export
print.optim_module <- function(x, ...) {
  x <- attr(x, "module")
  print(x)
}




