# run all

r_scripts <- list.files()[grep("[0-9]+.*\\.R$", list.files())]

do_not_run <- c(
  "99_sample_size.R" # 
)

r_scripts <- r_scripts[!r_scripts %in% d_not_run]

for (f in r_scripts) {
  print(f)
  source(f)
}
