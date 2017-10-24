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
    # Collect results
    dt_s_dstrb <- data.table(id = rep(1:input$select_sim_steps,
                                      each = length(input$select_var)),
                             variable = rep(input$select_var,
                                            times = input$select_sim_steps),
                             t_hat = 0.0,
                             v_hat = 0.0);

    dens <- est_density(as.matrix(dt_pop()[, list(x_rel, y_rel)]));
    k <- ceiling(dens$max*pi*input$select_radius^2*2);
    cnt <- 0L;
    withProgress(message = "Running simulation",
                 min = 0, max = 100,
                 expr = {
                   for (m in seq.int(input$select_sim_steps)) {
                     dt_s_loc <- xy_sample(dt_pop()[, range(x_rel)],
                                           dt_pop()[, range(y_rel)],
                                           input$select_sample_size);

                     s <- fixed_area(as.matrix(dt_pop()[, list(x_rel, y_rel)]),
                                     dt_s_loc,
                                     r = input$select_radius,
                                     k = k);

                     dt_s_tree <- sample_data(dt_pop(), s);

                     if (input$select_edge == "wt") {
                       idx_wt <- edge_corr_wt(dt_s_tree, dt_s_loc, pop_border());
                       dt_s_tree[idx_wt, f_edge := 2];
                     }

                     dt_s_plot <- plot_data(dt_s_tree, target_vars = input$select_var);

                     dt_est <- est_srs(dt_s_plot);
                     r_start <- cnt*length(input$select_var) + 1L;
                     r_end <- r_start + length(input$select_var) - 1L;
                     set(dt_s_dstrb,
                         r_start:r_end,
                         3L:4L,
                         dt_est[, .SD, .SDcol = c("y_hat", "v_hat")]);
                     cnt <- cnt + 1;
                     incProgress(1/input$select_sim_steps*100);
                   }
                 });
    dt_sim_res$a <- dt_s_dstrb;
  });

  output$stand <- renderPlot({
    dt_s_loc <- xy_sample(dt_pop()[, range(x_rel)],
                          dt_pop()[, range(y_rel)],
                          input$select_sample_size);

    s <- fixed_area(as.matrix(dt_pop()[, list(x_rel, y_rel)]),
                    dt_s_loc,
                    r = input$select_radius);

    dt_s_tree <- sample_data(dt_pop(), s);

    dt_pop()[!dt_s_tree[, stem_id],
             plot(x_rel, y_rel, asp = 1, axes = FALSE, ann = FALSE, pch = 1,
                  xlim = range(x_rel), ylim = range(y_rel),
                  mai = c(0, 0, 0, 0))];
    dt_pop()[dt_s_tree[, stem_id],
             points(x_rel, y_rel, pch = 16)];
    dt_s_loc[, points(x_u, y_u, pch = 4)];
  });

  output$estimates <- renderTable({
    dt_sim_res$a[,
                 list(t_hat = mean(t_hat),
                      v_emp = var(t_hat),
                      v_hat = mean(v_hat)),
                 by = variable];
  });

  output$distribution <- renderPlot({
    ggplot(dt_sim_res$a, aes(t_hat)) +
      geom_line(stat = 'density') +
      facet_wrap( ~ variable, scales = "free", ncol = 2) +
      theme_light()
  });
})
