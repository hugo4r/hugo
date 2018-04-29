.hugoEnv <- new.env()

setDefaultHugoEnv <- function() {
}

.onAttach <- function(...) {
  setDefaultHugoEnv()
  packageStartupMessage("Hi, I'm Hugo v.", utils::packageVersion("hugo"), ".\nReady to work.")
}

.onLoad <- function(...) {
  setDefaultHugoEnv()
}


## no S4 methodology here; speedup :
.noGenerics <- TRUE
