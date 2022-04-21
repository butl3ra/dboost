optim_Module <- R6::R6Class(
  classname = "optim_Module",
  lock_objects = FALSE,
  public = list(
    solve = function(...) {
      stop("solve method is not implemented")
    },
    print = function() {
      print_optim_module(self, private)
    }
  ),
  private = list(),
  active = list()
)
