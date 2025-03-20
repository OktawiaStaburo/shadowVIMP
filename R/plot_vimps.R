#' Box plot of VIMPs and corresponding p-values
#'
#' Box plot displaying variable importance measures along with unadjusted,
#' FDR-adjusted, and FWER-adjusted p-values obtained from the `shadow_vimp()`
#' function. Colors indicate whether each covariate is informative and specify
#' under which multiple testing adjustment (FWER, FDR, or none) it is deemed
#' informative.
#'
#' @param shadow_vimp_out Object of the class "shadow_vimp", the output of the
#'   function `shadow_vimp()`.
#' @param pooled Boolean
#'  * `TRUE` - passed `shadow_vimp_out` contains p-values obtained using the "
#'  pooled" approach. Default.
#'  * `FALSE` - passed `shadow_vimp_out` contains p-values obtained using the
#'  "per variable"  approach.
#' @param filter_vars Numeric, the number of variables to plot. The default is
#'   `NULL`, which means that all variables considered in the last step of the
#'   procedure (and included in the ` shadow_vimp_out`) will be plotted.
#' @param  p_val_labels Boolean, controls whether the p-value labels should be
#'   printed on the plot, default `TRUE`.
#' @param text_size Numeric, parameter that controls the size of the printed
#'   p-values on the plot, default is 4.
#' @param legend.position Character, one of "right", "left", "top", "bottom" or
#'   "none". Argument specifying the position of the legend.
#' @param category_colors Character of length 4, contains color assignment for
#'   each of four possible outcomes: variable not significant, confirmed by
#'   unadjusted, FDR and FWER adjusted p-values. The default colors are color
#'   blind friendly.
#' @param helper.legend Boolean. Indicates whether the circle subplot displaying
#'   the relationship between the FWER, FDR, and unadjusted p-values should be
#'   shown alongside the legend. The default is `TRUE`.
#' @param ... Other options used to control the appearance of the output plot.
#' @return ggplot object
#' @export
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom ggpubr get_legend as_ggplot
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_wrap
#' @import patchwork
#' @importFrom ggforce geom_circle
#' @examples
#' data(mtcars)
#'
#' # When working with real data, increase the value of the `niters` and
#' # `num.trees` parameters to obtain trustworthy results.
#' \donttest{
#' # Pooled p-values
#' set.seed(789)
#' out_pooled <- shadow_vimp(
#'   data = mtcars, outcome_var = "vs",
#'   niters = c(10, 20, 30), num.trees = 30
#' )
#'
#' # The following 3 lines of code produce identical plots
#' plot_vimps(shadow_vimp_out = out_pooled, pooled = TRUE, text_size = 4)
#' plot_vimps(shadow_vimp_out = out_pooled, text_size = 4)
#' plot_vimps(shadow_vimp_out = out_pooled)
#'
#' # Plot only top 3 covariates with the lowest p-values
#' plot_vimps(shadow_vimp_out = out_pooled, filter_vars = 3)
#'
#' #' # Do not display p-values on the plot
#' plot_vimps(shadow_vimp_out = out_pooled, p_val_labels = FALSE)
#'
#' # Change the size of displayed p-values
#' plot_vimps(shadow_vimp_out = out_pooled, text_size = 6)
#'
#' # Change the position of the legend, available options: "right", "left",
#' # "top","bottom", "none"
#' plot_vimps(shadow_vimp_out = out_pooled, legend.position = "bottom")
#' plot_vimps(shadow_vimp_out = out_pooled, legend.position = "left")
#'
#' # Remove the legend
#' plot_vimps(shadow_vimp_out = out_pooled, legend.position = "none")
#'
#' # Remove the subplot that displays the relationship between FWER, FDR, and
#' # unadjusted p-values
#' plot_vimps(shadow_vimp_out = out_pooled, helper.legend = FALSE)
#'
#' # Change colours of the boxes
#' plot_vimps(shadow_vimp_out = out_pooled, category_colors = c(
#'   "FWER conf." = "#EE2617FF",
#'   "FDR conf." = "#F2A241FF",
#'   "Unadjusted conf." = "#558934FF",
#'   "Not significant" = "#0E54B6FF"
#' ))
#'
#' # Per variable p-values plot
#' out_per_var <- shadow_vimp(
#'   data = mtcars, outcome_var = "vs",
#'   niters = c(10, 20, 30), num.trees = 30, method = "per_variable"
#' )
#'
#' # Set pooled to `FALSE`, otherwise the function will throw an error.
#' plot_vimps(shadow_vimp_out = out_per_var, pooled = FALSE)
#' }
plot_vimps <- function(shadow_vimp_out,
                       pooled = TRUE,
                       filter_vars = NULL,
                       p_val_labels = TRUE,
                       text_size = 4,
                       legend.position = c("right", "left", "top", "bottom", "none"),
                       category_colors = c(
                         "FWER conf." = "#DD5129FF",
                         "FDR conf." = "#0F7BA2FF",
                         "Unadjusted conf." = "#43B284FF",
                         "Not significant" = "#898E9FFF"
                       ),
                       helper.legend = TRUE,
                       ...) {
  # Parameters check
  legend.position <- match.arg(legend.position)

  # Checks of category_colors parameter done by the helper function
  .check_colors(category_colors)

  if (is.numeric(text_size) == FALSE) {
    stop("Parameter `text_size` must be numeric.")
  }

  if (is.logical(pooled) == FALSE || is.logical(p_val_labels) == FALSE || is.logical(helper.legend) == FALSE) {
    stop("Parameter `pooled`, `p_val_labels` and `helper.legend` must be logical.")
  }

  if (is.null(filter_vars) == FALSE && (is.numeric(filter_vars) == FALSE || filter_vars <= 0)) {
    stop("Parameter `filter_vars` must be integer greater than 0. If you don't want to filter any variables, set filter_vars to `NULL` (the default).")
  } else if (is.null(filter_vars) == FALSE && filter_vars != as.integer(filter_vars)) {
    filter_vars <- as.integer(filter_vars)
    warning("Parameter `filter_vars` will be converted to the nearest integer.")
  }

  # Select appropriate results
  if (pooled == TRUE) {
    data <- shadow_vimp_out$final_dec_pooled
  } else {
    data <- shadow_vimp_out$final_dec_per_variable
  }

  if (is.null(data)) {
    stop("Invalid data provided. Consider changing the value of the 'pooled' parameter.")
  }

  # Check if data contains all necessary columns
  required_cols <- c("p_unadj", "p_adj_FDR", "p_adj_FWER", "Type1_confirmed", "FDR_confirmed", "FWER_confirmed")
  available_cols <- colnames(data)

  if (sum(required_cols %in% available_cols) != length(required_cols)) {
    stop("Not all of the required columns are present in the `shadow_vimp_out`.\n Consider changing  the `to_show` parameter when running `shadow_vimp()` function.")
  }

  # Check if vimp_history for the last step is available
  if (is.null(shadow_vimp_out$vimp_history) == TRUE) {
    stop("`vimp_history` for the last step is not available. Consider running the wrapper object with the `save_vimp_history` argument set to `all` or `last`.")
  }

  decisions <- data %>%
    select("varname", starts_with("p_"), ends_with("confirmed")) %>%
    mutate(
      p_val_unadj = ifelse(.data[["p_unadj"]] < 0.001, "<0.001", format(round(.data[["p_unadj"]], 3), nsmall = 3)),
      p_val_FDR = ifelse(.data[["p_adj_FDR"]] < 0.001, "<0.001", format(round(.data[["p_adj_FDR"]], 3), nsmall = 3)),
      p_val_FWER = ifelse(.data[["p_adj_FWER"]] < 0.001, "<0.001", format(round(.data[["p_adj_FWER"]], 3), nsmall = 3)),
      decision = factor(
        x = case_when(
          (FWER_confirmed == 1) & (FDR_confirmed == 1) & (Type1_confirmed == 1) ~ "FWER conf.",
          (FWER_confirmed == 0) & (FDR_confirmed == 1) & (Type1_confirmed == 1) ~ "FDR conf.",
          (FWER_confirmed == 0) & (FDR_confirmed == 0) & (Type1_confirmed == 1) ~ "Unadjusted conf.",
          (FWER_confirmed == 0) & (FDR_confirmed == 0) & (Type1_confirmed == 0) ~ "Not significant"
        ),
        levels = c("FWER conf.", "FDR conf.", "Unadjusted conf.", "Not significant"),
        ordered = T
      )
    ) %>%
    select(-c("p_unadj":"FWER_confirmed"))

  # Append VIMPs from simulations
  vimps <- shadow_vimp_out$vimp_history %>%
    pivot_longer(cols = everything(), names_to = "varname", values_to = "VIMP") %>%
    filter(!grepl("_permuted", .data[["varname"]])) %>%
    left_join(decisions, by = "varname")

  # Select variables to be plotted
  if (is.null(filter_vars) == TRUE) {
    # No filtering of variables, warning when more than 40 variables should be plotted
    vimps_subset <- vimps

    num_vars <- vimps_subset %>%
      summarise(unique_elem = n_distinct(.data[["varname"]]))

    if (num_vars > 40) {
      warning("Plotting more than 40 variables may affect readability. Consider filtering out some variables.")
    }
  } else {
    # If number of selected variables is larger than the number of available variables, give a warning
    available_vars <- vimps %>%
      summarise(n_distinct(.data[["varname"]])) %>%
      pull()

    if (available_vars < filter_vars) {
      warning("The number of available variables is less than the specified `filter_vars'. All available variables are plotted.")
    }

    # Plot only filter_vars variables
    vimps_subset <- vimps %>%
      inner_join(
        vimps %>%
          group_by(.data[["varname"]]) %>%
          summarise(median = median(.data[["VIMP"]])) %>%
          arrange(desc(median)) %>%
          top_n(filter_vars) %>%
          select("varname"),
        by = "varname"
      )
  }

  # Ensure the varname is ordered consistently across both data sets needed to create a box plot
  ordered_vars <- with(vimps_subset, reorder(varname, VIMP, FUN = median))

  # Update vimps_subset to include this ordered varname
  vimps_subset$ordered_varname <- ordered_vars

  # Extract unique p-values into a new data frame with the same ordering
  p_values_df <- vimps_subset %>%
    select(c("ordered_varname", "p_val_unadj", "p_val_FDR", "p_val_FWER")) %>%
    distinct()

  # Calculate x-axis range and minimum
  x_range <- diff(range(vimps_subset$VIMP))
  min_vimp <- min(vimps_subset$VIMP)

  # Default theme specification
  default_theme <- theme(
    legend.position = legend.position,
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.box.margin = margin(5, 5, 5, 5)
    )

  # User theme - to allow the modification of the plot without multiple argument matching error
  user_theme <- theme(...)

  # Merge the lists so that user-supplied settings override the default theme
  # merged_theme <- modifyList(default_theme, user_theme)
  # final_theme <- do.call(theme, merged_theme)

  # Box plot
  box_plot <- ggplot(
    vimps_subset,
    aes(
      y = .data[["ordered_varname"]],
      x = .data[["VIMP"]],
      fill = .data[["decision"]]
    )
  ) +
    geom_boxplot(outlier.size = 0.5) +
    scale_fill_manual(
      values = category_colors,
      guide = guide_legend(keywidth = unit(2, "cm")),
      labels = function(x) str_wrap(x, width = 8) # wrap text of the legend labels
    ) +
    # add extra space on the left and right side of the x-axis (in percentage)
    scale_x_continuous(expand = expansion(mult = c(0.15, 0.05))) +
    coord_cartesian(clip = "off") +
    theme_bw() +
    default_theme +
    user_theme

  if (p_val_labels == TRUE) {
    # Transform p-values data to long format
    p_values_long <- p_values_df %>%
      pivot_longer(
        cols = c("p_val_unadj", "p_val_FDR", "p_val_FWER"),
        names_to = "p_type",
        values_to = "p_value"
      )

    # Define fixed offsets for each p-value type (as a fraction of the x_range)
    offsets <- c(p_val_unadj = 0.15, p_val_FDR = 0.08, p_val_FWER = 0.01)

    # Compute the actual x position for each label and set color based on p-value type
    p_values_long <- p_values_long %>%
      mutate(
        offset_fraction = offsets[.data$p_type],
        x_pos = min_vimp - (x_range * .data$offset_fraction),
        color_val = case_when(
          .data$p_type == "p_val_unadj" ~ category_colors["Unadjusted conf."][[1]],
          .data$p_type == "p_val_FDR" ~ category_colors["FDR conf."][[1]],
          .data$p_type == "p_val_FWER" ~ category_colors["FWER conf."][[1]]
        ),
        # For all but the last label, add a comma between the p-values
        label_text = ifelse(.data$p_type == "p_val_FWER", .data$p_value, paste0(.data$p_value, ","))
      )

    # Add a single geom_text layer with p-values
    box_plot <- box_plot +
      geom_text(
        data = p_values_long,
        aes(
          x = .data$x_pos,
          y = .data[["ordered_varname"]],
          label = .data$label_text,
          color = .data$color_val
        ),
        hjust = 1,
        size = text_size,
        inherit.aes = FALSE,
        show.legend = FALSE
      ) +
      scale_color_identity()
  }

  # If user selected plot without the legend:
  if (legend.position == "none") {
    box_plot
  } else {
    # Customize the legend and place it in the correct place
    # Extracting legend from the box plot
    legend <- get_legend(box_plot, position = legend.position) %>%
      as_ggplot()

    # Box plot without legend
    bp_no_legend <- box_plot + theme(legend.position = "none")

    # Should the plot showing the dependency between Type-1, FDR and FWER be displayed?
    if (helper.legend == FALSE) {
      legend_helper <- legend
    } else {
      # Creating subplot displayed next to main legend - the dependency between Type-1, FDR and FWER
      fdr_fwer_type1_plot <- .helper_plot(category_colors = category_colors)

      # Merged legend and circle subplots
      if (legend.position %in% c("left", "right")) {
        legend_helper <- (legend / fdr_fwer_type1_plot) +
          plot_layout(heights = c(4, 1)) # previously: 2, 1
      } else if (legend.position %in% c("bottom", "top")) {
        legend_helper <- (legend + fdr_fwer_type1_plot) +
          plot_layout(widths = c(4, 1)) # previously: 2, 1
      }
    }

    # Arrange the box plot and the legend_helper based on legend.position
    if (legend.position %in% c("left", "right")) {
      # Horizontal layout
      if (legend.position == "left") {
        final_plot <- (wrap_plots(legend_helper) + bp_no_legend) +
          plot_layout(widths = c(1, 7))
      } else {
        final_plot <- (bp_no_legend + legend_helper) +
          plot_layout(widths = c(7, 1))
      }
    } else if (legend.position %in% c("top", "bottom")) {
      # Vertical layout
      if (legend.position == "top") {
        final_plot <- (legend_helper / bp_no_legend) +
          plot_layout(heights = c(1, 7))
      } else {
        final_plot <- (bp_no_legend / legend_helper) +
          plot_layout(heights = c(7, 1))
      }
    }

    final_plot
  }
}

# Internal function creating a circular plot of dependencies between FWER, FDR
# and Type-1 error (right bottom corner of main plot)
.helper_plot <- function(category_colors) {
  data_fr <- data.frame(
    x_coord = c(1, 1, 1),
    y_coord = c(1, 1, 1),
    radius = c(1.5, 1.1, 0.7),
    group = c("Type-1", "FDR", "FWER")
  ) %>%
    mutate(group = factor(.data[["group"]], levels = c("Type-1", "FDR", "FWER")))

  # Create the plot
  helper_plot <- ggplot(
    data_fr,
    aes(
      x0 = .data[["x_coord"]],
      y0 = .data[["y_coord"]],
      r = .data[["radius"]],
      fill = .data[["group"]]
    )
  ) +
    geom_circle(
      alpha = 0.9,
      color = "black",
      show.legend = F
    ) +
    coord_fixed() +
    theme_void() +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    scale_fill_manual(values = c(
      "Type-1" = category_colors["Unadjusted conf."][[1]],
      "FWER" = category_colors["FWER conf."][[1]],
      "FDR" = category_colors["FDR conf."][[1]]
    )) +
    geom_text(
      aes(
        x = .data[["x_coord"]],
        y = 0.8 * .data[["y_coord"]] + .data[["radius"]],
        label = .data[["group"]],
        fontface = "bold"
      ),
      size = 4
    ) +
    theme(
      plot.margin = margin(
        t = 0, # Top margin
        r = 0, # Right margin
        b = 0, # Bottom margin
        l = 0 # Left margin
      )
    )

  helper_plot
}

.check_colors <- function(category_colors) {
  if (is.character(category_colors) == FALSE) {
    stop("Parameter `category_colors` must be a character vector.")
  }

  if (length(category_colors) != 4) {
    stop("Parameter `category_colors` must have exactly 4 elements.")
  }

  expected_names <- c("FWER conf.", "FDR conf.", "Unadjusted conf.", "Not significant")
  actual_names <- names(category_colors)

  if (is.null(actual_names) || !all(expected_names %in% actual_names)) {
    stop("Parameter `category_colors` must have names: ", paste(expected_names, collapse = ", "))
  }
}
