.hugoEnv <- new.env()


setDefaultHugoEnv <- function() {
  .hugoEnv$path = paste0("hugo_investigation_",gsub(Sys.Date(), pattern = "-", replacement = "_"))
  options(hugo.connection_in = stdin())
  options(hugo.connection_out = stdout())
  options(hugo.know_credentials = F)
  options(hugo.user_name = '')
  options(hugo.repo_name = '')
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
