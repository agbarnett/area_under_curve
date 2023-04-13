# run all

r_scripts <- list.files()[grep("[0-9]+.*\\.R$", list.files())]



for (f in rev(r_scripts)) {
  renv::run(f)
}
