library(tidyverse)
library(lubridate)
library(modelr)
library(fda)

url <- 'https://raw.githubusercontent.com/pstat197/pstat197a/main/materials/labs/lab7-curvefitting/data/soiltemp.csv'

soil <- read_csv(url)
soil %>% head()

# scatterplot of temperatures against day of year
temp_pts <- soil %>%
  ggplot(aes(x = day, y = temp)) +
  geom_point(alpha = 0.1)

temp_pts

# LOcally Estimated Scatterplot Smoothing (LOESS) curve

temp_pts + 
  geom_smooth(formula = 'y ~ x',
              method = 'loess',
              span = 0.5,
              se = F,
              col = 'red') +
  geom_smooth(formula = 'y ~ x',
              method = 'loess',
              span = 0.1,
              se = F,
              col = 'blue') +
  geom_smooth(formula = 'y ~ x',
              method = 'loess',
              span = 1,
              se = F,
              col = 'orange')

  #' we see that the red curve with span = 0.5 
  #' captures the trend the best

# Polynomial Regression

# quadratic fit
temp_pts + 
  geom_smooth(formula = 'y ~ poly(x, 2)',
              method = 'lm')

poly(1:5, degree = 3, raw = T, simple = T)

# fit a polynomial model
fit_poly <- lm(temp ~ poly(day, degree = 50, raw = T),
               data = soil)

    #' when we use a large polynomial order we tend to get overfitiing and our
    #' model becomes very flexible which will lower the basis but
    #' the curve will fit the trend better than lower order polynomials

# compute predictions
pred_df <- tibble(day = 1:365) %>%
  add_predictions(fit_poly)

# visualize
temp_pts + 
  geom_path(data = pred_df, 
            aes(y = pred),
            color = 'blue')

# Spline Regeression

# linear spline with a knot at day 200
fit_spline <- lm(temp ~ day + I((day - 200)*(day > 200)) - 1,
                 data = soil)

# compute predictions
pred_df <- tibble(day = 1:365) %>%
  add_predictions(fit_spline)

# plot it
temp_pts + 
  geom_path(data = pred_df, 
            aes(y = pred),
            color = 'blue')

### Spline basis (aka "bspline")

# define knot points
knotpts <- c(100, 150, 200, 300)

# fit an order 3 regression spline with three internal knots
fit_bs <- lm(temp ~ bs(day, degree = 3, knots = knotpts),
             data = soil) 

# compute predictions
pred_df <- tibble(day = 1:365) %>%
  add_predictions(fit_bs)

# plot it
temp_pts + 
  geom_path(data = pred_df, 
            aes(y = pred),
            color = 'blue') +
  geom_vline(xintercept = knotpts, 
             linetype = 'dashed')

###

# define knots
knotpts <- c(100, 200, 300)

# input variable
x <- 1:365

# calculate basis expansion and plot it
bs(x, knots = knotpts, degree = 3) %>%
  as_tibble() %>%
  bind_cols(x = x) %>%
  pivot_longer(-x, names_to = 'basis') %>%
  ggplot(aes(x = x, y = value)) +
  geom_path(aes(group = basis, color = basis)) +
  geom_vline(xintercept = knotpts, linetype = 'dashed')

### Fourier Basis

  # assumptions that the data is a reflection over a short period of time
    # ex: shows seasonal trends in temperature but won't show annual trends in temperature

# fit the model with the fourier basis expansion
fit_fbs <- lm(temp ~ fourier(day, nbasis = 4, period = 365) - 1,
              data = soil)

# compute predictions
pred_df <- tibble(day = 1:365) %>%
  add_predictions(fit_fbs)

# plot it
temp_pts + 
  geom_path(data = pred_df, 
            aes(y = pred),
            color = 'blue')