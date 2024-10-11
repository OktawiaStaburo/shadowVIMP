#' Box plot of VIMPs and corresponding p-values
#'
#' Box plot of variable importance measures obtained from the simulation, along
#' with unadjusted, FDR, and FWER adjusted p-values indicating whether a given
#' variable is informative.
#'
#' @param wrapper_object List, the output of the function
#'   `vim_perm_sim_wrapper()`.
#' @param pooled Boolean
#'  * `TRUE` - passed `wrapper_object` contains pooled p-values.
#'  * `FALSE` - passed `wrapper_object` contains per variable p-values.
#' @param filter_vars Numeric, the number of variables to plot. The default is
#'   `NULL`, which means that all variables considered in the last step of the
#'   procedure (and included in the ` wrapper_object`) will be plotted.
#' @param text_size Numeric, parameter that controls the size of the printed
#'   p-values on the plot, default is 3.
#' @param ... Other options used to control the appearance of the output plot.
#' @return ggplot object
#' @export
#' @import dplyr ggplot2
#' @importFrom magrittr %>%
#' @importFrom ggpubr get_legend as_ggplot
#' @importFrom tidyr pivot_longer
#' @importFrom cowplot plot_grid
#' @importFrom ggforce geom_circle
#' @examples
#' data(mtcars)
#'
#' # When working with real data, increase the value of the nsims and num.trees
#' # parameters to obtain trustworthy results.
#' # Pooled p-values
#' out_pooled <- vim_perm_sim_wrapper(entire_data = mtcars, outcome_var = "vs",
#'  nsims = c(10, 20, 30), num.trees = 30)
#'
#' # The following 2 lines of code produce identical plots
#' plot_vimps(wrapper_object = out_pooled, pooled = TRUE, text_size = 4)
#' plot_vimps(wrapper_object = out_pooled, text_size = 4)
#'
#' # Plot only 3 covariates
#' plot_vimps(wrapper_object = out_pooled, filter_vars = 3)
#'
#' # Per variable p-values
#' out_per_var <- vim_perm_sim_wrapper(entire_data = mtcars, outcome_var = "vs",
#'  nsims = c(10, 20, 30), num.trees = 30, method = "per_variable")
#'
#' # Set pooled to `FALSE`, otherwise the function will throw an error.
#' plot_vimps(wrapper_object = out_per_var, pooled = FALSE, text_size = 4)
plot_vimps <- function(wrapper_object, pooled = TRUE, filter_vars = NULL, text_size = 3, ...){

  # Parameters check
  if(is.logical(pooled) == FALSE){
    stop("Parameter `pooled` must be logical.")
  }

  if(is.null(filter_vars) == FALSE && (is.numeric(filter_vars) == FALSE || filter_vars <= 0)){
    stop("Parameter `filter_vars` must be integer greater than 0. If you don't want to filter any variables, set filter_vars to `NULL` (the default).")
  } else if(is.null(filter_vars) == FALSE && filter_vars != as.integer(filter_vars)){
    filter_vars <- as.integer(filter_vars)
    warning("Parameter `filter_vars` will be converted to the nearest integer.")
  }

  # Select appropriate results
  if(pooled == TRUE){
    data <- wrapper_object$final_dec_pooled
  } else{
    data <- wrapper_object$final_dec_per_variable
  }

  if(is.null(data)){
    stop("Invalid data provided. Consider changing the value of the 'pooled' parameter.")
  }

  # Check if data contains all necessary columns
  required_cols <- c("p_unadj", "p_adj_FDR", "p_adj_FWER", "Type1_confirmed", "FDR_confirmed", "FWER_confirmed")
  available_cols <- colnames(data)

  if(sum(required_cols %in% available_cols) != length(required_cols)){
    stop("Not all of the required columns are present in the `wrapper_object`.\n Consider changing  the `to_show` parameter when running `vim_perm_sim_wrapper()` function.")
  }

  # Check if vimp_history for the last step is available
  if(is.null(wrapper_object$vimp_history) == TRUE){
    stop("`vimp_history` for the last step is not available. Consider running the wrapper object with the `save_vimp_history` argument set to `all` or `last`." )
  }

  decisions <- data %>%
    select("varname", starts_with("p_"), ends_with("confirmed")) %>%
    mutate(
      p_val_unadj = ifelse(.data[["p_unadj"]] < 0.001, "<0.001", format(round(.data[["p_unadj"]], 3), nsmall =3)),
      p_val_FDR = ifelse(.data[["p_adj_FDR"]] < 0.001, "<0.001", format(round(.data[["p_adj_FDR"]], 3), nsmall =3)),
      p_val_FWER = ifelse(.data[["p_adj_FWER"]] < 0.001, "<0.001", format(round(.data[["p_adj_FWER"]], 3), nsmall =3)),
      decision = factor(x = case_when( (FWER_confirmed == 1)&(FDR_confirmed == 1)&(Type1_confirmed == 1)~ "FWER conf.",
                                       (FWER_confirmed == 0)&(FDR_confirmed == 1)&(Type1_confirmed == 1)~ "FDR conf.",
                                       (FWER_confirmed == 0)&(FDR_confirmed == 0)&(Type1_confirmed == 1)~ "Unadjusted conf.",
                                       (FWER_confirmed == 0)&(FDR_confirmed == 0)&(Type1_confirmed == 0)~ "Not significant"),
                        levels = c("FWER conf.", "FDR conf.", "Unadjusted conf.", "Not significant"),
                        ordered = T)
    ) %>%
    select(-c("p_unadj":"FWER_confirmed"))

  # Append VIMPs from simulations
  vimps <-  wrapper_object$vimp_history %>%
    pivot_longer(cols = everything(), names_to = "varname", values_to = "VIMP") %>%
    filter(!grepl('_permuted', .data[["varname"]])) %>%
    left_join(decisions, by="varname")

  # Select variables to be plotted
  if(is.null(filter_vars) == TRUE){
    # No filtering of variables, warning when more than 40 variables should be plotted
    vimps_subset <- vimps

    num_vars <- vimps_subset %>%
      summarise(unique_elem = n_distinct(.data[["varname"]]))

    if(num_vars > 40){
     warning("Plotting more than 40 variables may affect readability. Consider filtering out some variables.")
    }
  } else {
    # If number of selected variables is larger than the number of available variables, give a warning
    available_vars <- vimps %>%
      summarise(n_distinct(.data[["varname"]])) %>%
      pull()

    if(available_vars < filter_vars){
      warning("The number of available variables is less than the specified `filter_vars'. All available variables are plotted.")}

    # Plot only filter_vars variables
    vimps_subset <- vimps %>%
      inner_join(
        vimps %>%
          group_by(.data[["varname"]]) %>%
          summarise(median = median(.data[["VIMP"]])) %>%
          arrange(desc(median)) %>%
          top_n(filter_vars) %>%
          select("varname"),
        by= "varname"
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

  # Assign color to each type of confirmation
  category_colors <- c("FWER conf." = "#DD5129FF",
                       "FDR conf." = "#0F7BA2FF",
                       "Unadjusted conf." = "#43B284FF",
                       "Not significant" = "#898E9FFF")

  # Create box plot
  box_plot <- ggplot(vimps_subset,
                     aes(y = .data[["ordered_varname"]],
                         x = .data[["VIMP"]],
                         fill = .data[["decision"]]), ...) +
    geom_boxplot(outlier.size = 0.5) +
    scale_fill_manual(values = category_colors,
                      guide = guide_legend(keywidth = unit(3, "cm"))) +
    theme_bw() +
    theme(legend.position = "right",
          axis.title = element_blank(),
          legend.title = element_blank(), ...) +
    geom_text(data = p_values_df,
              aes(x = min(vimps_subset$VIMP)- 3.7,
                  y = .data[["ordered_varname"]],
                  label = paste0(.data[["p_val_unadj"]], ",")),
              color = "#43B284FF",
              hjust = 1,
              size = text_size,
              inherit.aes = FALSE, ...)+
    geom_text(data = p_values_df,
              aes(x = min(vimps_subset$VIMP)-2,
                  y = .data[["ordered_varname"]],
                  label = paste0(.data[["p_val_FDR"]], ",")),
              color = "#0F7BA2FF",
              hjust = 1,
              size = text_size,
              inherit.aes = FALSE, ...) +
    geom_text(data = p_values_df,
              aes(x = min(vimps_subset$VIMP) - 0.3,
                  y = .data[["ordered_varname"]],
                  label = .data[["p_val_FWER"]]),
              color = "#DD5129FF",
              hjust = 1,
              size = text_size,
              inherit.aes = FALSE, ...)

  # Extracting legend from the box plot
  legend <- get_legend(box_plot, position = "right") %>%
    as_ggplot()

  # Box plot without legend
  bp_no_legend <- box_plot + theme(legend.position = "none")

  # Creating subplot displayed in the right corner - the dependency between Type-1, FDR and FWER
  fdr_fwer_type1_plot <- .helper_plot()

  # Merged legend and subplot
  legend_helper <- plot_grid(legend,
                             fdr_fwer_type1_plot,
                             ncol = 1,
                             rel_widths = c(3,1),
                             rel_heights = c(3,1)
  )

  # Box plot with legend and subplot
 plot_grid(bp_no_legend, legend_helper, ncol = 2, rel_widths = c(7,1))
}

# Internal function creating a circular plot of dependencies between FWER, FDR
# and Type-1 error (right bottom corner of main plot)
.helper_plot <- function(){
  data_fr <- data.frame(
    x_coord = c(1, 1, 1),
    y_coord = c(1, 1, 1),
    radius = c(1.5, 1.1, 0.7),
    group = c( "Type-1", "FDR", "FWER")
  ) %>%
    mutate(group = factor(.data[["group"]], levels = c( "Type-1", "FDR", "FWER")))

  # Create the plot
  helper_plot <- ggplot(data_fr,
                        aes(x0 = .data[["x_coord"]],
                            y0 = .data[["y_coord"]],
                            r = .data[["radius"]],
                            fill = .data[["group"]])
                        ) +
    geom_circle(
      alpha = 0.9,
      color = "black",
      show.legend = F) +
    coord_fixed() +
    theme_void() +
    scale_fill_manual(values = c("Type-1" = "#43B284FF",
                                 "FWER" = "#DD5129FF",
                                 "FDR" = "#0F7BA2FF")) +
    geom_text(aes(x = .data[["x_coord"]],
                  y = 0.8*.data[["y_coord"]]+.data[["radius"]],
                  label = .data[["group"]],
                  fontface = "bold"),
              size = 4
    )+
    theme(plot.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = 0) # Left margin
    )

  helper_plot
}
