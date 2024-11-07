# run this script to download the required packages

# List of required packages
packages <- c("data.table", "forecast")

# Function to check and install missing packages
install_if_missing <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        message("Installing ", pkg)
        install.packages(pkg, dependencies = TRUE)
    } else {
        message(pkg, " is already installed.")
    }
}

# Install required packages
invisible(lapply(packages, install_if_missing))

message("All required packages are installed and ready.")
