% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_vimps.R
\name{plot_vimps}
\alias{plot_vimps}
\title{Box plot of VIMPs and corresponding p-values}
\usage{
plot_vimps(
  shadow_vimp_out,
  pooled = TRUE,
  filter_vars = NULL,
  helper_legend = TRUE,
  p_val_labels = TRUE,
  text_size = 4,
  legend.position = c("right", "left", "top", "bottom", "none"),
  category_colors = c(`FWER conf.` = "#DD5129FF", `FDR conf.` = "#0F7BA2FF",
    `Unadjusted conf.` = "#43B284FF", `Not significant` = "#898E9FFF"),
  ...
)
}
\arguments{
\item{shadow_vimp_out}{Object of the class "shadow_vimp", the output of the
function \code{shadow_vimp()}.}

\item{pooled}{Boolean
\itemize{
\item \code{TRUE} - passed \code{shadow_vimp_out} contains p-values obtained using the "
pooled" approach. Default.
\item \code{FALSE} - passed \code{shadow_vimp_out} contains p-values obtained using the
"per variable"  approach.
}}

\item{filter_vars}{Numeric, the number of variables to plot. The default is
\code{NULL}, which means that all variables considered in the last step of the
procedure (and included in the \code{ shadow_vimp_out}) will be plotted.}

\item{helper_legend}{Boolean. Indicates whether the circle subplot displaying
the relationship between the FWER, FDR, and unadjusted p-values should be
shown alongside the legend. The default is \code{TRUE}.}

\item{p_val_labels}{Boolean, controls whether the p-value labels should be
printed on the plot, default \code{TRUE}.}

\item{text_size}{Numeric, parameter that controls the size of the printed
p-values on the plot, default is 4.}

\item{legend.position}{Character, one of "right", "left", "top", "bottom" or
"none". Argument specifying the position of the legend.}

\item{category_colors}{Character of length 4, containing color assignment for
each of four possible outcomes: variable not significant, confirmed by
unadjusted, FDR and FWER adjusted p-values. The default colors are color
blind friendly.}

\item{...}{Other options used to control the appearance of the output plot.}
}
\value{
ggplot object
}
\description{
Box plot displaying variable importance measures along with unadjusted,
FDR-adjusted, and FWER-adjusted p-values obtained from the \code{shadow_vimp()}
function. Colors indicate whether each covariate is informative and specify
under which multiple testing adjustment (FWER, FDR, or none) it is deemed
informative.
}
\examples{
data(mtcars)

# When working with real data, increase the value of the `niters` and
# `num.trees` parameters to obtain trustworthy results.
\donttest{
# Function to make sure proper number of cores is specified for multithreading
safe_num_threads <- function(n) {
  available <- parallel::detectCores()
  if (n > available) available else n
}

# Pooled p-values
set.seed(789)
out_pooled <- shadow_vimp(
  data = mtcars, outcome_var = "vs",
  niters = c(10, 20, 30), num.trees = 30,
  num.threads = safe_num_threads(1)
)

# The following 3 lines of code produce identical plots
plot_vimps(shadow_vimp_out = out_pooled, pooled = TRUE, text_size = 4)
plot_vimps(shadow_vimp_out = out_pooled, text_size = 4)
plot_vimps(shadow_vimp_out = out_pooled)

# Plot only top 3 covariates with the lowest p-values
plot_vimps(shadow_vimp_out = out_pooled, filter_vars = 3)

#' # Do not display p-values on the plot
plot_vimps(shadow_vimp_out = out_pooled, p_val_labels = FALSE)

# Change the size of displayed p-values
plot_vimps(shadow_vimp_out = out_pooled, text_size = 6)

# Change the position of the legend, available options: "right", "left",
# "top","bottom", "none"
plot_vimps(shadow_vimp_out = out_pooled, legend.position = "bottom")
plot_vimps(shadow_vimp_out = out_pooled, legend.position = "left")

# Remove the legend
plot_vimps(shadow_vimp_out = out_pooled, legend.position = "none")

# Remove the subplot that displays the relationship between FWER, FDR, and
# unadjusted p-values
plot_vimps(shadow_vimp_out = out_pooled, helper_legend = FALSE)

# Change colours of the boxes
plot_vimps(shadow_vimp_out = out_pooled, category_colors = c(
  "FWER conf." = "#EE2617FF",
  "FDR conf." = "#F2A241FF",
  "Unadjusted conf." = "#558934FF",
  "Not significant" = "#0E54B6FF"
))

# Per variable p-values plot
out_per_var <- shadow_vimp(
  data = mtcars, outcome_var = "vs",
  niters = c(10, 20, 30), num.trees = 30,
  method = "per_variable", num.threads = safe_num_threads(1)
)

# Set pooled to `FALSE`, otherwise the function will throw an error.
plot_vimps(shadow_vimp_out = out_per_var, pooled = FALSE)
}
}
