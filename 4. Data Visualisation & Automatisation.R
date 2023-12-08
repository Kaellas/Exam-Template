#
# ──────────────────────────────────────────────────────────────────────────────
#
# SECTION 4: DATA VISUALISATION & AUTOMATISATION
#
# ──────────────────────────────────────────────────────────────────────────────
#
# Libraries used:

library(ggplot2)
library(recipes)

# SECTION TABLE OF CONTENTS
#
# 
# 4. Data Visualisation & Automatisation
#     -   ggplot2 Package
#     -   recipes Package
#     -   shiny Package
#     -   cronR Package
#     -   httr2 Package (API calls)
# 
# ──────────────────────────────────────────────────────────────────────────────
# GGPLOT2 PACKAGE
# ──────────────────────────────────────────────────────────────────────────────

# ggplot2 is a great alternative to functions like plot() and hist() if you
# want to get fancy. Here are all of its basic functions:

# declaring a plot - does nothing on its own

   ggplot(pumpkin) + ...

# declaring aesthetic mappings. It's used to specify how variables in the data
# are mapped to visual properties (aesthetics) of the plot.

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + ...

# These functions specify the type of plot you want to draw

# scatter plot

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_point()

# line plot

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_line()

# bar plot (requires a factor)

pumpkin$Repack <- factor(pumpkin$Repack)

ggplot(pumpkin, aes(x = Repack)) + 
  geom_bar()


# make multi panel plots depending on a factor

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_point() +
  facet_wrap(~ Repack)

# Customize the non-data parts of your plot (like axes, text, colors, etc.)

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_point() + 
  theme_minimal()

# Add titles and modify axis labels

ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_point() + 
  labs(title = "Scatterplot", x = "X Axis", y = "Y Axis")

# Save plots as a file

gg_plot <- ggplot(pumpkin, aes(x = High.Price, y = Low.Price)) + 
  geom_point()

ggsave("myplot.png", plot = gg_plot, device = "png")
# (will save to working directory unless otherwise specified)


# ──────────────────────────────────────────────────────────────────────────────
# RECIPES PACKAGE
# ──────────────────────────────────────────────────────────────────────────────

# recipes can take care of most of the preprocessing and modelling functions 
# that were discussed in the earlier section. It can handle everything 
# much faster and simpler

# Here are its key functions:

# initialize a recipe object. Specify a formula and a dataset
# does nothing on its own
rec <- recipe(High.Price ~ Low.Price + Mostly.Low, data = pumpkin) %>%
  ...

# add different preprocessing steps to the recipe:
# (can be used on all or only specified predictors)

# Factorisation
step_factor()

# Outliers
step_range(High.Price, range = c(lower_limit, upper_limit))

# Transformations
step_log(Variable)
step_BoxCox()
step_YeoJohnson()

# Centering
step_center(all_predictors())

# Scaling
step_scale(all_predictors())
   
# For creating dummy variables from categorical predictors
step_dummy(all_nominal())
   
# Normalizing numeric predictors
step_normalize(all_numeric())

# Missing values (NAs)
step_naomit() # delete rows with Nas
step_impute_mean(all_predictors()) # impute with mean (or other options)

# Lumping
step_other(Variable, threshold = 0.1)

# Dummy encoding
step_dummy()

# For principal component analysis (PCA)
step_pca(all_numeric(), threshold = 0.95)


# For other all_ functions you can use:
?has_role


# After creating the recipe, you have to first prepare it

prep <- prep(rec, training = train)

# Then bake it to obtain the preprocessed dataset

train <- bake(prep, new_data = train)
test <- bake(prep, new_data = test)

# ──────────────────────────────────────────────────────────────────────────────
# SHINY PACKAGE
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# CRONR PACKAGE
# ──────────────────────────────────────────────────────────────────────────────

# ──────────────────────────────────────────────────────────────────────────────
# HTTR2 PACKAGE
# ──────────────────────────────────────────────────────────────────────────────