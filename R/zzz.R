.hugoEnv <- new.env()

setDefaultHugoEnv <- function() {
  .hugoEnv$path = paste0("hugo_investigation_",gsub(Sys.Date(), pattern = "-", replacement = "_"))
}

.onAttach <- function(...) {
  setDefaultHugoEnv()
  packageStartupMessage("Hi, I'm Hugo v.", utils::packageVersion("hugo"), ".\nReady to work.")
}

.onLoad <- function(...) {
  setDefaultHugoEnv()
}

.hugoEnv$history<-"empty"

## no S4 methodology here; speedup :
.noGenerics <- TRUE
