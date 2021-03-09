#' Run Cargo
#'
#' This function finds and invokes Cargo (Rust's package manager) with
#' \code{...} arguments passed to [system2()] but, by default, does not write to
#' the user's file system (e.g., \code{~/.cargo}) to comply with CRAN policies.
#'
#' To enable caching, set the \code{R_CARGO_SAVE_CACHE} environment variable to
#' \code{TRUE}. Then, if defined, the \code{R_CARGO_HOME} environment variable
#' will be used as the cache location. Otherwise, Cargo uses its default
#' behavior (usually writing to \code{~/.cargo} unless the \code{CARGO_HOME}
#' environment variable is set). Regardless of the location, the user is
#' responsible to maintaining and clearing the cache when using the
#' \code{R_CARGO_SAVE_CACHE} environment variable.
#'
#' @param ... Arguments passed to \code{\link{system2}}, although the
#'   \code{command} argument is set to \code{cargo} by this function and cannot
#'   be set by the user.
#' @param verbose Should debugging information be written to the console before
#'   running Cargo?
#'
#' @return The result of the underlying call to the \code{system2} function used
#'   to run Cargo is returned or, if Cargo is not found, an error is thrown.
#'
#' @seealso [base::system2()], [base::Sys.setenv()]
#'
#' @export
#'
#' @examples
#' if ( is_available() ) {
#'   run("--version")
#'   run("--help")
#' }
#'
run <- function(..., verbose=TRUE) {
  if ( Sys.getenv("R_CARGO_FORCE_FAIL") == "TRUE" ) {
    stop("Cargo failed because of R_CARGO_FORCE_FAIL environment variable.")
  }
  cargo_cmd <- find_cargo()
  if ( is.null(cargo_cmd) ) {
    stop("Cargo is not installed.  Please run 'cargo::install()'.")
  }
  cargo_home <- if ( Sys.getenv("R_CARGO_SAVE_CACHE") == "TRUE" ) {
    if ( Sys.getenv("R_CARGO_HOME") != "" ) {
      Sys.getenv("R_CARGO_HOME")
    } else {
      NULL
    }
  } else {
    cargo_home_tmp <- file.path(tempdir(check=TRUE), "cargo")
    on.exit(unlink(cargo_home_tmp))
    cargo_home_tmp
  }
  n <- function(x) normalizePath(x, mustWork=FALSE)
  if ( verbose ) {
    cat(sprintf("Cargo executable: %s\n",n(cargo_cmd)))
    if ( ! is.null(cargo_home) ) {
      cat(sprintf("Cargo home: %s\n",n(cargo_home)))
    } else {
      cat("Cargo home is not set.\n")
    }
  }
  if ( ! is.null(cargo_home) ) {
    system3(command=n(cargo_cmd), ..., env=c(CARGO_HOME=n(cargo_home)))
  } else {
    system3(command=n(cargo_cmd), ...)
  }
}

#' Determine Rust Target
#'
#' This function determines the appropriate Rust target for this instance of R.
#'
#' @return A string giving a Rust target
#'
#' @export
#'
#' @examples
#' tryCatch(target(), error=function(e) cat(e$message, "\n", sep=""))
#'
target <- function() {
  info <- Sys.info()
  sysname <- info['sysname']
  machine <- info['machine']
  arch <- if ( machine == "x86" ) "i686"
  else if ( grepl("x86[_-]64",machine) ) "x86_64"
  else if ( grepl("aarch64",machine) ) "aarch64"
  else machine
  fail <- function() stop(sprintf("Unsupported sysname, machine, architecture: %s, %s, %s", sysname, machine, arch))
  if ( .Platform$OS.type == "windows" ) {
    if ( arch %in% c("i686","x86_64") ) paste0(arch,"-pc-windows-gnu")
    else fail()
  } else {
    if ( sysname == "Darwin" && arch == "x86_64" ) "x86_64-apple-darwin"
    else if ( sysname == "Darwin-arm64" && arch == "aarch64" ) "aarch64-apple-darwin"
    else if ( sysname == "Linux" ) {
      if ( arch %in% c("aarch64","i686","x86_64") ) paste0(arch,"-unknown-linux-gnu")
      else fail()
    }
    else fail()
  }
}

#' Test Availability of Cargo
#'
#' This function checks if Cargo is available on the system.
#'
#' @param minimum_version A character string representing the minimum Rust
#'   version that is needed.
#' @param verbose Print information about the installation?
#'
#' @return Logical indicating whether (at least the desired version of) Cargo is
#'   available.
#'
#' @export
#'
#' @examples
#' if ( ! is_available("1.50") ) {
#'   cat("Please run 'cargo::install()'.\n")
#' }
#'
is_available <- function(minimum_version, verbose=TRUE) {
  if ( Sys.getenv("R_CARGO_FORCE_FAIL") == "TRUE" ) {
    if ( verbose ) cat("Cargo failed because of R_CARGO_FORCE_FAIL environment variable.\n")
    return(FALSE)
  }
  if ( is.null(find_cargo()) ) {
    if ( verbose ) cat("Cargo is not installed.  Please run 'cargo::install()'.\n")
    return(FALSE)
  }
  if ( missing(minimum_version) && ! verbose ) return(TRUE)
  v <- strsplit(run(args="--version", stdout=TRUE),' ',fixed=TRUE)[[1]][2]
  if ( ! missing(minimum_version) && utils::compareVersion(v, minimum_version) < 0 ) {
    if ( verbose ) cat(sprintf("Cargo version %s is installed, but %s is needed.  Please run 'cargo::install()'.\n",v,minimum_version))
    return(FALSE)
  }
  if ( verbose ) cat(sprintf("Cargo version: %s\n",v))
  TRUE
}

#' Install or Update Cargo
#'
#' This function downloads and runs the \code{rustup} installer.  If already
#' installed, the installer simply updates the installation.  Note that,
#' contrary to the default behavior of the installer, the system is NOT
#' configured to modify the \code{PATH} environment variable.
#'
#' @param force Install without asking for user confirmation?
#'
#' @return \code{TRUE} if only only if the installation was successful,
#'   invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install()
#' }
#'
install <- function(force=FALSE) {
  windows <- .Platform$OS.type=="windows"
  if ( ! force ) {
    if ( ! interactive() ) stop("This function requires either user confirmation or 'force=TRUE'.")
      while ( TRUE ) {
        response <- toupper(trimws(readline(prompt=paste0("Do you want to install Cargo? [y/N] "))))
        if ( response %in% c("N","") ) return(invisible(FALSE))
        if ( response %in% c("Y") ) break
      }
  }
  temp_install_home <- mkdir(tempfile("cargo"))
  rustup_init <- file.path(temp_install_home, sprintf("rustup-init.%s",ifelse(windows,"exe","sh")))
  URL <- ifelse(windows,"https://win.rustup.rs/x86_64","https://sh.rustup.rs")
  if ( tryCatch(utils::download.file(URL, rustup_init, mode="wb"), warning=function(e) 1, error=function(e) 1) != 0 ) {
    cat(sprintf("Could not download '%s' to '%s'.\n", URL, rustup_init))
    return(invisible(FALSE))
  }
  if ( windows ) {
    rustup_cmd <- file.path("~/.cargo","bin","rustup.exe")
    lines <- c(
      paste0(shQuote(normalizePath(rustup_init,mustWork=FALSE))," -q -y --no-modify-path --default-host x86_64-pc-windows-gnu"),
      paste0(shQuote(normalizePath(rustup_cmd,mustWork=FALSE))," target add i686-pc-windows-gnu"))
    rustup_init_bat <- file.path(temp_install_home, "rustup-init.bat")
    writeLines(lines, rustup_init_bat)
    if ( system2(rustup_init_bat) != 0 ) {
      cat(sprintf("There was a problem running the rustup installer at '%s'.\n", rustup_init_bat))
      return(invisible(FALSE))
    }
  } else {
    # Suppress output to avoid spurious output that can trip up automatic checks on old platforms.
    if ( system2("sh", c(shQuote(rustup_init),"-q","-y","--no-modify-path")) != 0 ) {
      cat(sprintf("There was a problem running the rustup installer at '%s'.\n", rustup_init))
      return(invisible(FALSE))
    }
  }
  unlink(temp_install_home, recursive=TRUE, force=TRUE)
  cat("\n### Cargo installation was successful. ###\n\n")
  invisible(TRUE)
}

## Private

find_cargo <- function() {
  add_exe <- function(x) if ( .Platform$OS.type=="windows" ) paste0(x,".exe") else x
  if ( Sys.getenv("R_CARGO_HOME","<unset>") != "<unset>" ) {
    candidate <- file.path(Sys.getenv("R_CARGO_HOME"),"bin",add_exe("cargo"))
    if ( file.exists(candidate) ) return(candidate)
  }
  if ( Sys.getenv("CARGO_HOME","<unset>") != "<unset>" ) {
    candidate <- file.path(Sys.getenv("CARGO_HOME"),"bin",add_exe("cargo"))
    if ( file.exists(candidate) ) return(candidate)
  }
  candidate <- Sys.which("cargo")
  if ( candidate != "" && file.exists(candidate) ) return(candidate)
  candidate <- file.path("~/.cargo","bin",add_exe("cargo"))
  if ( file.exists(candidate) ) return(candidate)
  NULL
}

mkdir <- function(dir) {
  if ( ! dir.exists(dir) ) {
    if ( ! dir.create(dir, showWarnings=FALSE, recursive=TRUE) ) {
      stop(sprintf("Could not create directory '%s'.", dir))
    }
  }
  dir
}

# A fix for the system2 function which sets environment variables better.
system3 <- function(..., env=character()) {
  if ( length(env) > 0 ) {
    names <- names(env)
    original_env <- sapply(names, function(x) Sys.getenv(x,"<unset>"))
    set <- original_env != "<unset>"
    to_restore <- original_env[set]
    to_unset <- names(original_env[!set])
    do.call(Sys.setenv, as.list(env))
    on.exit({
      if ( length(to_restore) > 0 ) do.call(Sys.setenv, as.list(to_restore))
      Sys.unsetenv(to_unset)
    })
  }
  system2(...)
}

download_static_library <- function(target,
                                    mkURL1=function(pkgName,pkgVersion,osName,target) {},
                                    mkURL2=function(pkgName,pkgVersion,osName,target) {},
                                    package_source_home="..", verbose=TRUE) {
  osName <- if ( .Platform$OS.type == "windows" ) "windows"
  else {
    info <- Sys.info()
    sysname <- info['sysname']
    if ( sysname == "Darwin" ) "macosx"
    else if ( sysname == "Darwin-arm64" ) "macosx-arch64"
    else if ( sysname == "Linux" ) "linux"
    else stop(sprintf("Unsupported OS: %s",sysname))
  }
  desc <- read.dcf(file.path(package_source_home,"DESCRIPTION"))
  pkgName    <- as.character(desc[,"Package"])
  pkgVersion <- as.character(desc[,"Version"])
  staticlib_filename <- "staticlib.tar.gz"
  url <- mkURL1(pkgName,pkgVersion,osName,target)
  cat(paste0("Downloading static libraries from: ", url, "\n"))
  if ( tryCatch(utils::download.file(url, staticlib_filename, mode="wb"), warning=function(e) 1, error=function(e) 1) != 0 ) {
    cat(sprintf("Could not download '%s' to '%s'.\n", url, staticlib_filename))
    url <- mkURL2(pkgName,pkgVersion,osName,target)
    cat(paste0("Downloading static libraries from: ", url, "\n"))
    if ( tryCatch(utils::download.file(url, staticlib_filename, mode="wb"), warning=function(e) 1, error=function(e) 1) != 0 ) {
      cat(sprintf("Could not download '%s' to '%s'.\n", url, staticlib_filename))
      stop("Giving up trying to download the static library!")
    }
  }
  utils::untar(staticlib_filename, exdir="..")
  unlink(staticlib_filename)
}
