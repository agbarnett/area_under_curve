# 99_play_numbers.R
# playing with numbers of false positives, etc
# July 2020

# basic numbers
n_patients = 1000
pre_test = 0.05 # pre-test probability
sensitivity = 0.80
specificity = 0.80
with_disease = n_patients*pre_test
without_disease = n_patients*(1-pre_test)

# 2x2 table
table = matrix(c(with_disease * sensitivity,
          with_disease * (1-sensitivity),
          without_disease * (1-specificity),
          without_disease * specificity), nrow=2)
table

# make a likely AUC from numbers
points = list(specificities = c(1, specificity, 0),
                    sensitivities = c(0, sensitivity, 1))
# simple linear interpolation
gap = 0.001 # smaller gap is more accurate
est = with(points, approx(x = 1 - specificities, 
                    y = sensitivities, 
                    xout = seq(0, 1, gap), 
                    method="linear", n=50))
# check on plot
with(points, plot(1 - specificities, sensitivities, type='b'))
with(est, points(x, y, type='p', col='red'))

# 
(AUC = sum(est$y*gap))
