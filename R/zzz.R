.hugoEnv <- new.env()

setDefaultHugoEnv <- function() {
}

.onAttach <- function(...) {
  setDefaultHugoEnv()
  packageStartupMessage("Hi, I'm Hugo v.", utils::packageVersion("hugo"), ".\nHow can I help you?")
}

.onLoad <- function(...) {
  setDefaultHugoEnv()
}


## no S4 methodology here; speedup :
.noGenerics <- TRUE
