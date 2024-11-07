## a small and simple function. The input is the path for a new directory. The function checks whether the directory already exists, and if it doesn't, it creates the new directory.

check_and_create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}
