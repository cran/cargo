#' Run Cargo
#'
#' This function finds and runs Cargo (Rust's package manager) with the
#' \code{...} arguments passed as command line arguments but, by default, runs
#' according to CRAN policies.  First, it does not write to the user's file
#' system (e.g., \code{~/.cargo}). Second, it only uses at most two parallel
#' jobs when building (i.e., compiling).
#'
#' To enable caching, set the \code{R_CARGO_SAVE_CACHE} environment variable to
#' \code{TRUE}. Then, if defined, the \code{R_CARGO_HOME} environment variable
#' will be used as the cache location. Otherwise, Cargo uses its default
#' behavior (usually writing to \code{~/.cargo} unless the \code{CARGO_HOME}
#' environment variable is set). Regardless of the location, the user is
#' responsible to maintaining and clearing the cache when using the
#' \code{R_CARGO_SAVE_CACHE} environment variable.
#'
#' To enable a specific number of parallel jobs, set the
#' \code{R_CARGO_BUILD_JOBS} environment variable to the desired integer. If
#' \code{R_CARGO_BUILD_JOBS} is \code{0}, Cargo will use its default behavior
#' (usually using all the cores unless the \code{CARGO_BUILD_JOBS} environment
#' variable is set or the \code{--jobs} argument is provided).
#'
#' @param ... Character vector of command line arguments passed to the
#'   \code{cargo} command.
#' @param minimum_version A character string representing the minimum version of
#'   Rust that is needed.
#'
#' @return A logical equally \code{TRUE} if and only if the minimum version is
#'   available and the exit status of the command is zero (indicating success).
#'   The function should never throw a warning or error.
#'
#' @seealso [base::Sys.setenv()]
#'
#' @export
#' @importFrom utils packageVersion
#'
#' @examples
#' run(minimum_version="1.50")
#'
run <- function(..., minimum_version) {
  if ( require("cargo", character.only=TRUE) ) {
    cat(sprintf("This is version %s of the cargo package.\n",packageVersion("cargo")))
  }
  if ( Sys.getenv("R_CARGO_FORCE_FAIL") == "TRUE" ) {
    cat("Cargo failed because of R_CARGO_FORCE_FAIL environment variable.\n")
    return(FALSE)
  }
  n <- function(x) normalizePath(x, mustWork=FALSE)
  cargo_cmd <- find_cmd("cargo")
  if ( is.null(cargo_cmd) ) {
    cat("Cargo is not found. Please see the package's INSTALL instructions.\n")
    return(FALSE)
  }
  cargo_cmd <- n(cargo_cmd)
  cat(sprintf("Cargo executable: %s\n",cargo_cmd))
  output <- suppressWarnings(system2(cargo_cmd, "--version", stdout=TRUE))
  if ( ! is.null(attr(output,"status")) ) {
    cat("Cargo is installed, but broken. Please see the package's INSTALL instructions.\n")
    return(FALSE)
  }
  if ( ! missing(minimum_version) ) {
    version <- tryCatch({
      version <- strsplit(output," ",fixed=TRUE)[[1]][2]
      if ( is.na(version) ) {
        cat(sprintf("Problem parsing Cargo version string: '%s'. Please see the package's INSTALL instructions.\n",paste(output,collapse=",")))
        return(FALSE)
      }
      if ( utils::compareVersion(version, minimum_version) < 0 ) {
        cat(sprintf("Cargo version '%s' is installed, but '%s' is needed. Please see the package's INSTALL instructions.\n",version,minimum_version))
        return(FALSE)
      }
      version
    }, warning=function(e) e, error=function(e) e)
    if ( inherits(version,"warning") || inherits(version,"error") ) {
      cat(sprintf("Problem parsing Cargo version string '%s' and comparing it against '%s'. Please see the package's INSTALL instructions.\n",paste(output,collapse=","),minimum_version))
      return(FALSE)
    }
    cat(sprintf("Cargo version: %s\n",version))
  } else {
    cat("The 'minimum_version' argument will be required in the future.\n")
  }
  env <- character()
  if ( Sys.getenv("R_CARGO_SAVE_CACHE") == "TRUE" ) {
    if ( Sys.getenv("R_CARGO_HOME") != "" ) {
      env <- c(env, CARGO_HOME=n(Sys.getenv("R_CARGO_HOME")))
    }
  } else {
    cargo_home <- file.path(tempdir(check=TRUE), "cargo")
    on.exit(unlink(cargo_home))
    env <- c(env, CARGO_HOME=n(cargo_home))
  }
  if ( Sys.getenv("R_RUSTC","<unset>") != "<unset>" ) {
    env <- c(env, RUSTC=n(Sys.getenv("R_RUSTC")))
  } else if ( Sys.getenv("RUSTC","<unset>") == "<unset>" ) {
    rustc_cmd <- find_cmd("rustc")
    if ( is.null(rustc_cmd) ) {
      cat("The Rust compiler (rustc) is not found. Please see the package's INSTALL instructions.\n")
      return(FALSE)
    }
    env <- c(env, RUSTC=n(rustc_cmd))
  }
  nCores <- Sys.getenv("R_CARGO_BUILD_JOBS","2")
  if ( nCores != "0" ) env <- c(env, CARGO_BUILD_JOBS=nCores)
  if ( length(env) > 0 ) {
    cat(sprintf("Cargo environment variables explicitly set:\n   %s\n",paste(names(env), env, sep="=", collapse="\n   ")))
  }
  args <- c(...)
  if ( length(args) == 0 ) return(TRUE)
  status <- tryCatch(system3(command=cargo_cmd, args=args, env=env),
                     warning=function(e) e, error=function(e) e)
  if ( status != 0 || inherits(status,"warning") || inherits(status,"error") ) {
    cat(sprintf("There was a problem in running Cargo. Please see the package's INSTALL instructions.\n"))
    FALSE
  } else TRUE
}

#' Determine Rust Target
#'
#' This function tries to determine the appropriate Rust target for this
#' instance of R.
#'
#' @return A string giving a Rust target, or \code{""} if this cannot be
#'   determined.
#'
#' @export
#'
#' @examples
#' target()
#'
target <- function() {
  info <- Sys.info()
  sysname <- info['sysname']
  machine <- info['machine']
  arch <- if ( machine == "x86" ) "i686"
  else if ( grepl("x86[_-]64",machine) ) "x86_64"
  else if ( machine %in% c("aarch64","arm64") ) "aarch64"
  else machine
  result <- if ( .Platform$OS.type == "windows" ) {
    if ( arch %in% c("i686","x86_64") ) paste0(arch,"-pc-windows-gnu")
    else ""
  } else {
    if ( sysname == "Darwin" ) {
      if ( arch %in% c("aarch64","x86_64") ) paste0(arch,"-apple-darwin")
      else ""
    } else if ( sysname == "Linux" ) {
      if ( arch %in% c("aarch64","i686","x86_64") ) paste0(arch,"-unknown-linux-gnu")
      else ""
    } else ""
  }
  if ( result == "" ) {
    cat(sprintf("Unrecognized sysname, machine, architecture, platform, os: %s, %s, %s, %s, %s\n", sysname, machine, arch, R.Version()$platform, R.Version()$os))
  } else {
    cat("Target is: ", result, "\n", sep="")
  }
  result
}


## Private

download_staticlib <- function(...) {
  templates <- list(...)
  info <- as.data.frame(read.dcf(file.path("..","DESCRIPTION")))
  staticlib_filename <- "staticlib.tar.gz"
  success <- FALSE
  for ( url in templates ) {
    url <- sub("${name}"   ,info$Package,url,fixed=TRUE)
    url <- sub("${version}",info$Version,url,fixed=TRUE)
    url <- sub("${target}" ,target(),    url,fixed=TRUE)
    cat(paste0("Downloading static libraries from: ", url, "\n"))
    if ( tryCatch(utils::download.file(url, staticlib_filename, mode="wb"), warning=function(e) 1, error=function(e) 1) == 0 ) {
      success <- TRUE
      break
    }
  }
  if ( ! success ) stop("Giving up trying to download the static library!")
  utils::untar(staticlib_filename)
  unlink(staticlib_filename)
}

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

find_cmd <- function(what) {
  if ( .Platform$OS.type=="windows" ) what <- paste0(what,".exe")
  if ( Sys.getenv("R_CARGO_HOME","<unset>") != "<unset>" ) {
    candidate <- file.path(Sys.getenv("R_CARGO_HOME"),"bin",what)
    if ( file.exists(candidate) ) return(candidate)
  }
  if ( Sys.getenv("CARGO_HOME","<unset>") != "<unset>" ) {
    candidate <- file.path(Sys.getenv("CARGO_HOME"),"bin",what)
    if ( file.exists(candidate) ) return(candidate)
  }
  candidate <- Sys.which(what)
  if ( candidate != "" && file.exists(candidate) ) return(candidate)
  candidate <- file.path("~",".cargo","bin",what)
  if ( file.exists(candidate) ) return(candidate)
  candidate <- file.path(Sys.getenv(ifelse(.Platform$OS.type=="windows","USERPROFILE","HOME")),".cargo","bin",what)
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

