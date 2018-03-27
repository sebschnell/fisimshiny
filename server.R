#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);
library(ggplot2);
library(fisim);
library(maptools);
library(spatstat);
library(sp);


# Helper function for loading data from http://stackoverflow.com/questions/30951204/load-dataset-from-r-package-using-data-assign-it-directly-to-a-variable
get_data <- function(x) {
  e <- new.env();
  name <- data(list = x, envir = e)[1];
  e[[name]];
}

# Define server logic
shinyServer(function(input, output) {

  pop <- reactive({
    get_data(input$select_pop);
  });

  dt_sim_res <- reactiveValues();
  observeEvent(input$start_sim,{

    withProgress(message = "Running simulation",
                 min = 0, max = 100,
                 expr = {
                   s_loc <- xy_sample(pop()$boundary,
                                      n = input$select_sample_size,
                                      M = input$select_sim_steps,
                                      method = input$select_sampling_design);

                   if (input$select_r_design == "fixed_area") {
                     idx_tree <- fixed_area(tree_pop = pop(),
                                            sample_loc = s_loc,
                                            r = input$select_radius);
                   } else if (input$select_r_design == "angle_count") {
                     idx_tree <- angle_count(tree_pop = pop(),
                                             sample_loc = s_loc,
                                             baf = input$select_baf);
                   } else if (input$select_r_design == "k_tree") {
                     idx_tree <- k_tree(tree_pop = pop(),
                                        sample_loc = s_loc,
                                        k = input$select_k);
                   }

                   s_tree <- extract_data(pop(), idx_tree);

                   if (input$select_edge == "wt") {
                     s_tree <- edge_corr_wt(tree_pop = pop(),
                                            tree_sample = s_tree,
                                            sample_loc = s_loc);
                   }

                   s_plot <- sum_data(s_tree, target_vars = input$select_var);
                 });

    dt_sim_res$a <- est_srs(s_plot);
  });

  output$stand <- renderPlot({
    s_loc <- xy_sample(pop()$boundary,
                       n = input$select_sample_size,
                       M = 1,
                       method = input$select_sampling_design);

    if (input$select_r_design == "fixed_area") {
      idx_tree <- fixed_area(tree_pop = pop(),
                             sample_loc = s_loc,
                             r = input$select_radius);
    } else if (input$select_r_design == "angle_count") {
      idx_tree <- angle_count(tree_pop = pop(),
                              sample_loc = s_loc,
                              baf = input$select_baf);
    } else if (input$select_r_design == "k_tree") {
      idx_tree <- k_tree(tree_pop = pop(),
                         sample_loc = s_loc,
                         k = input$select_k);
    }

    s_tree <- extract_data(tree_pop = pop(), response = idx_tree);

    pop()$data[, plot(x_tree, y_tree, asp = 1, axes = FALSE, ann = FALSE, pch = 1,
                      xlim = range(x_tree), ylim = range(y_tree),
                      mai = c(0, 0, 0, 0))];
    s_tree$data[, points(x_tree, y_tree, pch = 16)];
    s_loc$data[, points(x_s, y_s, pch = 4)];
  });


  output$estimates <- renderTable({
    dt_sim_res$a[,
                 list(y_hat = mean(y_hat),
                      v_emp = var(y_hat),
                      v_hat = mean(v_hat)),
                 by = list(variable, est_appr)];
  });

  output$distribution <- renderPlot({
    A <- sum(extract_area(hberg_beech$boundary))/10000;
    dt_y <- data.table(variable = input$select_var,
                       y = pop()$data[, sapply(.SD, sum), .SDcols = input$select_var]/A);
    ggplot(dt_sim_res$a, aes(y_hat)) +
      geom_freqpoly() +
      facet_grid(est_appr ~ variable, scales = "free") +
      geom_vline(aes(xintercept = y), dt_y) +
      theme_light()
  });
})
#testgit
