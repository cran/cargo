#' Run the Cargo Command
#'
#' This function invokes this package's installation of Cargo (Rust's package
#' manager) with \code{...} arguments passed to [system2()].
#'
#' If not already available, this function first tries to installs Cargo using
#' \code{rustup} (Rust's installer) in a subdirectory of
#' \code{tools::R_user_dir("cargo", "cache")}.  If Cargo is not available and
#' cannot be installed, \code{NULL} is returned.  Likewise, for testing
#' purposes, \code{NULL} is returned if the \code{R_CARGO_FORCE_FAIL}
#' environment variable equals \code{"TRUE"}.
#'
#' By default, all writes to disk are temporarily and removed after each call.
#' To avoid this, set the \code{R_CARGO_SAVE_CACHE} environment variable to
#' \code{TRUE}, which will then allow caching between calls to this function. If
#' defined, the \code{R_CARGO_HOME} environment variable will be used as the
#' cache location. Otherwise, the \code{R_PACKAGE_NAME} environment variable
#' (which is set by R while installing a source package) will be used to set the
#' cache path as follows:
#' \code{file.path(tools::R_user_dir(Sys.getenv("R_PACKAGE_NAME"), "cache"),
#' "cargo")}.  Lacking either of those, this package's own cache is used.
#' Regardless of the location, the user is responsible to maintaining and
#' clearing the cache when using the \code{R_CARGO_SAVE_CACHE} environment
#' variable.
#'
#' @param ... Arguments passed to \code{\link{system2}}, although the
#'   \code{command} argument is set to \code{cargo} by this function and cannot
#'   be set by the user.
#' @param minimum_version A character string representing the minimum Rust
#'   version that is needed.  The function runs \code{rustup update} if the
#'   currently installed Cargo version does not meet the supplied minimum
#'   version.
#' @param verbose Should the configuration information be written to the console
#'   before running Cargo?
#'
#' @return The result of the underlying call to the \code{system2} function
#'   used to run Cargo is returned, or \code{NULL} if Cargo is not available.
#'
#' @seealso [base::system2()], [base::Sys.setenv()], [tools::R_user_dir()]
#'
#' @export
#'
#' @examples
#' \donttest{
#' run("--version")
#' run("--help")
#' }
#'
run <- function(..., minimum_version, verbose) {
  config <- config()
  if ( ! config$available ) {
    cat("\nCargo is not available of this system.\n")
    return(NULL)
  }
  if ( Sys.getenv("R_CARGO_FORCE_FAIL") == "TRUE" ) {
    cat("\nFail forced by R_CARGO_FORCE_FAIL environment variable.\n")
    return(NULL)
  }
  if ( ! missing(minimum_version) ) {
    version_string_long <- shell_out(config, command=config$cargo_cmd, args="--version", stdout=TRUE)
    version_string <- strsplit(version_string_long,' ',fixed=TRUE)[[1]][2]
    if ( utils::compareVersion(version_string, minimum_version) < 0 ) {
      shell_out(config, command=config$rustup_cmd, args="update")
    }
  }
  config$cargo_home <- if ( Sys.getenv("R_CARGO_SAVE_CACHE") == "TRUE" ) {
    if ( Sys.getenv("R_CARGO_HOME") != "" ) {
      Sys.getenv("R_CARGO_HOME")
    } else if ( Sys.getenv("R_PACKAGE_NAME") != "" ) {
      file.path(tools::R_user_dir(Sys.getenv("R_PACKAGE_NAME"), "cache"), "cargo")
    } else {
      config$cargo_home
    }
  } else {
    cargo_home_tmp <- file.path(tempdir(check=TRUE), "cargo")
    on.exit(unlink(cargo_home_tmp))
    cargo_home_tmp
  }
  if ( ! missing(verbose) && isTRUE(verbose) ) {
    cat("\nRunning Cargo with the following settings:\n\n")
    print(config)
  }
  shell_out(config, command=config$cargo_cmd, ...)
}

## Private

mkdir <- function(dir) {
  if ( ! dir.exists(dir) ) {
    if ( ! dir.create(dir, showWarnings=FALSE, recursive=TRUE) ) {
      stop(sprintf("Could not create directory '%s'.", dir))
    }
  }
  dir
}

config <- function(verbose) {
  windows <- .Platform$OS.type=="windows"
  config <- list()
  config$specification <- 2L  # Increment to force re-installation attempt.
  config$config_home <- mkdir(tools::R_user_dir("cargo", "config"))
  config$config_file <- file.path(config$config_home, "config.rds")
  config$cache_home  <- mkdir(tools::R_user_dir("cargo", "cache"))
  config$cargo_home  <- file.path(config$cache_home, "cargo")
  config$cargo_cmd   <- file.path(config$cargo_home, "bin", paste0("cargo",ifelse(windows,".exe","")))
  config$rustup_home <- file.path(config$cache_home, "rustup")
  config$rustup_cmd  <- file.path(config$cargo_home, "bin", paste0("rustup",ifelse(windows,".exe","")))
  do_install <- FALSE
  do_install <- do_install || ! file.exists(config$config_file)
  if ( ! do_install ) {
    config_extra <- readRDS(config$config_file)
    do_install <- do_install || is.null(config_extra$specification) || ( config_extra$specification < config$specification )
    config_extra$specification <- NULL
    config <- c(config, config_extra)
    # Retry failed installation attempts every 7 days.
    if ( ! do_install && ! is.null(config$installation_date) && isFALSE(config$available) ) {
      do_install <- ( config$installation_date <= Sys.Date() - 7 )
    }
  }
  if ( do_install ) config <- install(config)
  if ( ! missing(verbose) && isTRUE(verbose) ) {
    cat("\nCargo configuration:\n\n")
    print(config)
    invisible(config)
  } else {
    config
  }
}

shell_out <- function(config, ...) {
  CARGO_BIN_ <- Sys.getenv("CARGO_BIN","<unset>")
  Sys.setenv(CARGO_BIN=normalizePath(file.path(config$cargo_home, "bin"), mustWork=FALSE))
  RUSTUP_HOME_ <- Sys.getenv("RUSTUP_HOME","<unset>")
  Sys.setenv(RUSTUP_HOME=normalizePath(config$rustup_home, mustWork=FALSE))
  CARGO_HOME_ <- Sys.getenv("CARGO_HOME","<unset>")
  Sys.setenv(CARGO_HOME=normalizePath(config$cargo_home, mustWork=FALSE))
  on.exit({
    if ( CARGO_BIN_ == "<unset>" ) Sys.unsetenv("CARGO_BIN") else Sys.setenv(CARGO_BIN=CARGO_BIN_)
    if ( RUSTUP_HOME_ == "<unset>" ) Sys.unsetenv("RUSTUP_HOME") else Sys.setenv(RUSTUP_HOME=RUSTUP_HOME_)
    if ( CARGO_HOME_ == "<unset>" ) Sys.unsetenv("CARGO_HOME") else Sys.setenv(CARGO_HOME=CARGO_HOME_)
  })
  system2(...)
}

install <- function(config) {
  cat("\n### Installing Cargo. ###\n\n")
  config_extra <- list(specification=config$specification, installation_date=Sys.Date(), available=FALSE)
  saveRDS(config_extra, config$config_file)
  windows <- .Platform$OS.type=="windows"
  unlink(c(config$rustup_home, config$cargo_home), recursive=TRUE, force=TRUE, expand=TRUE)
  rustup_init <- file.path(config$cache_home, sprintf("rustup-init.%s",ifelse(windows,"exe","sh")))
  URL <- ifelse(windows,"https://win.rustup.rs/x86_64","https://sh.rustup.rs")
  if ( tryCatch(utils::download.file(URL, rustup_init, mode="wb"), warning=function(e) 1, error=function(e) 1) != 0 ) {
    cat(sprintf("Could not download '%s' to '%s'.\n", URL, rustup_init))
    return(config)
  }
  if ( windows ) {
    lines <- c(
      paste0(shQuote(normalizePath(rustup_init,mustWork=FALSE))," -q -y --no-modify-path --default-host x86_64-pc-windows-gnu"),
      paste0(shQuote(normalizePath(config$rustup_cmd,mustWork=FALSE))," target add i686-pc-windows-gnu"))
    rustup_init_bat <- file.path(config$cache_home, "rustup-init.bat")
    writeLines(lines, rustup_init_bat)
    # Suppress output to avoid spurious output that can trip up automatic checks on old platforms.
    if ( shell_out(config, rustup_init_bat, stdout=FALSE, stderr=FALSE) != 0 ) {
      cat(sprintf("There was a problem running the rustup installer at '%s'.\n", rustup_init_bat))
      return(config)
    }
    file.remove(rustup_init_bat)
  } else {
    # Suppress output to avoid spurious output that can trip up automatic checks on old platforms.
    if ( shell_out(config, "sh", c(shQuote(rustup_init),"-q","-y","--no-modify-path"), stdout=FALSE, stderr=FALSE) != 0 ) {
      cat(sprintf("There was a problem running the rustup installer at '%s'.\n", rustup_init))
      return(config)
    }
  }
  file.remove(rustup_init)
  config_extra$available <- TRUE
  saveRDS(config_extra, config$config_file)
  cat("\n### Cargo installation was successful. ###\n\n")
  c(config, config_extra)
}

download_static_library <- function(target, mkURL=function(osName,pkgName,pkgVersion) {}, package_source_home="..", verbose=TRUE) {
  if ( ! ( as.character(version['arch']) %in% c("i386","x86_64") ) ) {
    stop(paste0("Unsupported architecture: ",as.character(version['arch'])))
  }
  osName <- if ( .Platform$OS.type == "windows" ) "windows"
  else if ( .Platform$OS.type == "unix" ) {
    sysname <- Sys.info()['sysname']
    if ( sysname == "Darwin" ) "macosx"
    else if ( sysname == "Linux" ) "linux"
    else stop(paste0("Unsupported OS: ",sysname))
  } else stop(paste0("Unsupported OS: ",sysname))
  if ( verbose ) cat("\nDownloading static library.\n\n")
  desc <- read.dcf(file.path(package_source_home,"DESCRIPTION"))
  pkgName    <- as.character(desc[,"Package"])
  pkgVersion <- as.character(desc[,"Version"])
  utils::download.file(mkURL(osName,pkgName,pkgVersion), "staticlib.tar.gz", quiet=TRUE)
  utils::untar("staticlib.tar.gz", exdir="..")
  unlink("staticlib.tar.gz")
  if ( osName == "windows" ) {
    destDir <- mkdir(sprintf("rustlib/target/%s/release", target))
    headDir <- if ( substr(target,1,3) == "x86" ) "x64" else "i386"
    invisible(file.rename(sprintf("../src-%s/%s/librustlib.a", headDir, destDir),
                          sprintf(          "%s/librustlib.a",          destDir)))
  }
}
