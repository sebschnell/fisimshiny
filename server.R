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

# Helper function for loading data from http://stackoverflow.com/questions/30951204/load-dataset-from-r-package-using-data-assign-it-directly-to-a-variable
get_data <- function(x) {
  e <- new.env();
  name <- data(list = x, envir = e)[1];
  e[[name]];
}

# Define server logic
shinyServer(function(input, output) {

  dt_pop <- reactive({
    get_data(input$select_pop);
  });

  pop_border <- reactive({
    get_data(paste(input$select_pop, "_border", sep = ""));
  });

  dt_sim_res <- reactiveValues();
  observeEvent(input$start_sim,{

    dens <- est_density(as.matrix(dt_pop()[, list(x_rel, y_rel)]));
    k <- ceiling(dens$max*pi*input$select_radius^2*2);

    withProgress(message = "Running simulation",
                 min = 0, max = 100,
                 expr = {
                   dt_s_loc <- xy_sample(dt_pop()[, range(x_rel)],
                                         dt_pop()[, range(y_rel)],
                                         n = input$select_sample_size,
                                         M = input$select_sim_steps);

                   s <- fixed_area(as.matrix(dt_pop()[, list(x_rel, y_rel)]),
                                   as.matrix(dt_s_loc[, list(x_pt, y_pt)]),
                                   r = input$select_radius,
                                   k = k,
                                   n = input$select_sample_size,
                                   M = input$select_sim_steps);

                   dt_s_tree <- extract_data(dt_pop(), s);

                   if (input$select_edge == "wt") {
                     idx_wt <- edge_corr_wt(dt_s_tree, dt_s_loc, pop_border());
                     dt_s_tree[idx_wt, f_edge := 2];
                   }

                   dt_s_plot <- sum_data(dt_s_tree, target_vars = input$select_var);
                 });

    dt_sim_res$a <- est_srs(dt_s_plot);
  });

  output$stand <- renderPlot({
    dt_s_loc <- xy_sample(dt_pop()[, range(x_rel)],
                          dt_pop()[, range(y_rel)],
                          n = input$select_sample_size,
                          M = 1);

    s <- fixed_area(as.matrix(dt_pop()[, list(x_rel, y_rel)]),
                    as.matrix(dt_s_loc[, list(x_pt, y_pt)]),
                    r = input$select_radius,
                    n = input$select_sample_size,
                    M = 1);

    dt_s_tree <- extract_data(dt_pop(), s);

    dt_pop()[!dt_s_tree[, stem_id],
             plot(x_rel, y_rel, asp = 1, axes = FALSE, ann = FALSE, pch = 1,
                  xlim = range(x_rel), ylim = range(y_rel),
                  mai = c(0, 0, 0, 0))];
    dt_pop()[dt_s_tree[, stem_id],
             points(x_rel, y_rel, pch = 16)];
    dt_s_loc[, points(x_pt, y_pt, pch = 4)];
  });

  output$estimates <- renderTable({
    dt_sim_res$a[,
                 list(y_hat = mean(y_hat),
                      v_emp = var(y_hat),
                      v_hat = mean(v_hat)),
                 by = variable];
  });

  output$distribution <- renderPlot({
    ggplot(dt_sim_res$a, aes(y_hat)) +
      geom_line(stat = 'density') +
      facet_wrap( ~ variable, scales = "free", ncol = 2) +
      theme_light()
  });
})
#testgit
