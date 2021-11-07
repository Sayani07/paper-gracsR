sample_seed.all <- list(seq(10, 100, 10))

for (sample_seed in sample_seed.all) {
  rmarkdown::render('index.Rmd', params = list(sample_seed = sample_seed))
}
