detect_unused_packages <- function(script_path = NULL, packages = NULL) {
  # Automatically detect the script file if not provided
  if (is.null(script_path)) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      script_path <- rstudioapi::getActiveDocumentContext()$path
    } else {
      script_path <- sys.frame(1)$ofile
    }
  }
  
  # Validate script path
  if (is.null(script_path) || script_path == "" || !file.exists(script_path)) {
    stop("❌ Error: Could not detect or find the script file. Please provide a valid path.")
  }
  
  # Read the script content
  script_text <- readLines(script_path)
  
  # Get currently loaded packages if none specified
  if (is.null(packages)) {
    packages <- names(sessionInfo()$otherPkgs)
  }
  
  # Function to check if any function from a package is used in the script
  is_package_used <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) return(FALSE)
    
    # Get all functions in the package
    pkg_functions <- ls(paste0("package:", pkg), all.names = TRUE)
    
    # Escape special characters for regex
    pkg_functions_escaped <- gsub("([][{}()^$.|*+?\\])", "\\\\\\1", pkg_functions)
    
    # Check if any function from the package is used in the script
    any(sapply(pkg_functions_escaped, function(fn) {
      any(grepl(paste0("\\b", fn, "\\s*\\("), script_text))  # Look for function calls
    }))
  }
  
  # Identify unused packages
  unused_packages <- packages[!sapply(packages, is_package_used)]
  
  # Print results
  cat("\n📂 Checking script:", script_path, "\n")
  
  if (length(unused_packages) > 0) {
    cat("\n🚀 These packages are not used and can be removed:\n")
    for (pkg in unused_packages) {
      cat(" -", pkg, "\n")
    }
    cat("\n🔹 Some packages listed here might be auto-loaded dependencies of other libraries.\n")
  } else {
    cat("\n✅ All specified packages are being used.\n")
  }
}
