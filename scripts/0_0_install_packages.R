
# very short script to check if required packages are installed, and to install them if not. 
# I think it's very unlikely that it will matter what version of these packages are used, because they're quite robust with respect to the main functions used. 
# for that reason, the versions aren't built into the script. In case it is important....the versions are 8.23.0 for forecast and 1.16.4 for data.table. 

## 1. put the names of the packages to be installed into a list
packages_to_install <- list("data.table", "forecast")

package_ins <- "data.table"

## 2. define the function that will check if the package has been installed, and will install the package if it has not been
check_and_install <- function(package_ins){
  
  if(!(nzchar(system.file(package = package_ins)))){
    
    install.packages(package_ins)
    
  }
  
}


## 3. apply the function over the list of package names

lapply(
  X = packages_to_install,
  FUN = check_and_install
)


