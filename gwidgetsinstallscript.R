# Created: 30 Jul 2020
# Updated: 18 Nov 2020

##################################################################
#  gwdg-arch.R: Installation of current and archived packages    #
#               upon which the RQDA package depends. The key     #
#               packages mentioned are RGtk2, gWidgets,          #
#                                                                #
#  HOW TO USE THIS SCRIPT:                                       #
#  This file should be run at the command line or in an          #
#  interactive R session i.e. in the R console, as follows:      #
#   * Windows command line (CMD or PowerShell)                   #
#       Rscript.exe gwdg-arch.R [--args verbose]                 #
#                                                                #
#   * R console                                                  #
#       source('gwdg-arch.R')                                    #
#                                                                #
#                   IMPORTANT NOTICE!!!                          #
#  Please BE aware that if Gtk+ is not properly installed, this  #
#  script will stop and require the user to open an R session    #
#  to install it. Once this is done, this script should be run   #
#  again to complete the installation of the packages that       #
#  directly or indirectly depend on it.                          #
#                                                                #
##################################################################

local({
  # ---
  cArg <- commandArgs(trailingOnly = TRUE)
  cArg <- cArg[length(cArg)]
  # ---
  
  ## Provides the index to CRAN directory
  cran.index <- function() {
    c("https://cran.r-project.org")
  }
  
  ## Returns the address to RStudio's CRAN mirror
  rstudio <- function() {
    c('https://cran.rstudio.com')
  }
  
  local_gtk_bin_path <- function(pkg = 'RGtk2') {
    gtkdir <- file.path(.libPaths()[1], pkg, 'gtk')
    file.path(gtkdir, .Platform$r_arch)
  }
  
  shellQuiet <- TRUE
  if (length(cArg) != 0) {
    if (cArg == "verbose") 
      shellQuiet <- FALSE
  }
  
  ## Installs initial packages required by the script.
  ## What makes these ones special is that they are
  ## current package versions from CRAN and they are
  ## downloaded as binaries.
  ## @param cranry A character vector of packages.
  .install_init <- function(cranbry) {
    stopifnot(is.character(cranbry))
    tryCatch({
      notInstalled <-
        cranbry[!cranbry %in% .packages(all.available = TRUE)]
      install.packages(notInstalled, repos = rstudio(), quiet = !shellQuiet)
    }, error = function(e) {
      stop(sprintf(
        "Initialization failed. Install %s",
        paste(cranbry, collapse = ', ')
      ))
    })
  }
  
  
  ## Checks the availability of Rtools on Windows (v35)
  .check_buildtools <- function() {
    if (!devtools::has_devel()) {
      if (.Platform$OS.type == 'windows') {
        toolsUrl <-
          file.path(cran.index(),
                    "bin",
                    .Platform$OS.type,
                    "Rtools/history.html")
        errBuildtools <-
          sprintf("Build tools were not found. Please visit %s to install.",
                  toolsUrl)
        stop(errBuildtools, call. = TRUE)
      }
    }
  }
  
  .install_init(c('devtools', 'cairoDevice'))
  
  .check_buildtools()
  
  ## Installs a given CRAN archive
  ## @param name Name of the package
  ## @param ver The package version
  inst <- function(name, ver) {
    rgtk2 <- "RGtk2"
    archOpts <- "--no-multiarch"
    isRGtk2 <- name == rgtk2
    pkgExists <- quote(name %in% .packages(all.available = TRUE))
    
    if (isRGtk2) {
      msgRGtk2 <- 
        list(
          line1 = "Installing 'RGtk2'. If it fails, use `install.packages` in R console ... ",
          line2 = "Run `library(RGtk2)` in R to install Gtk+. Then, rerun this script."
        )
      
      # Custom error condition
      abortRgtk2 <- function() {
        msg <- 
          sprintf("Could not install %s. Try doing so in R console", rgtk2)
        stop(msg, call. = FALSE)
      }
      
      ## Install RGtk2
      if (!eval(pkgExists)) {
        message(msgRGtk2$line1, appendLF = !shellQuiet)
        
        tryCatch({
          install.packages(
            rgtk2,
            repos = rstudio(),
            INSTALL_opts = archOpts,
            quiet = shellQuiet,
            verbose = shellQuiet
          )
          message("Done")     # Per RGtk2/R/zzz.R, Gtk+ can only be installed interactively.
        },
        error = function(e) {
          message("Failed")
          abortRgtk2()
        },
        warning = function(w) {
          wrnmsg <- "cannot remove prior installation of package 'RGtk2'"
          if (conditionMessage(w) == wrnmsg)
            abortRgtk2()
        }, 
        finally = return(message(msgRGtk2$line2)))
      }
      
    }
    
    ## Avoid repeat installations via an early return
    ## If we're dealing with RGtk2, just stop the script
    ## and install Gtk+ interactively, if it is required.
    if (eval(pkgExists)) {
      message(sQuote(name), " is already installed")
      if (isRGtk2) {
        if (!dir.exists(local_gtk_bin_path(rgtk2))) {    # Consider: Directory may be a dud.
          message(msgRGtk2$line2)
          stop('Execution stopped.', call. = FALSE)
        }
      }
      return()
    }
    
    ## Grab the package archives as desired.
    ## When installing them, the following are considered:
    ##   => asking for upgrade interrupts installation
    ##   => install only one of 32- or 64-bit, not both
    ## But first, if RGtk2 is not present, there's no 
    ## point trying to install packages that depend on it.
    isRgtkDep <- (name == 'gWidgetsRGtk2' || name == 'RQDA')
    rgtkNotReady <- !(rgtk2 %in% .packages(all.available = TRUE) &&
                        dir.exists(local_gtk_bin_path(rgtk2)))
    if (isRgtkDep && rgtkNotReady) {
      message(sQuote(name), " was not installed because RGtk2 is not ready")
      return()
    }
    
    tryCatch({
      msg <- sprintf("Installing '%s' ... ", name)
      message(msg, appendLF = !shellQuiet)
      
      if (!isRGtk2)
        devtools::install_version(
          name,
          ver,
          repos = cran.index(),
          quiet = shellQuiet,
          upgrade = "never",
          INSTALL_opts = archOpts
        )
      message("Done")
    },
    error = function(e) {
      message("Failed")
    })
    
  } # end inst()
  
  pkgversions <- c(RGtk2 = "2.20.36",
                   gWidgets = '0.0-54.2',
                   gWidgetsRGtk2 = '0.0-86',
                   RQDA = '0.3-1')
  
  invisible(Map(inst, names(pkgversions), pkgversions))
  
})

# Used successfully with R 4.0.3