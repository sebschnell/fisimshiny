library(shiny);
library(ggplot2);
library(fisim);
library(maptools);
library(spatstat);
library(sp);

# Helper functions --------------------------------------------------------

# Helper function for loading data from http://stackoverflow.com/questions/30951204/load-dataset-from-r-package-using-data-assign-it-directly-to-a-variable
get_data <- function(x) {
  e <- new.env();
  name <- data(list = x, envir = e)[1];
  e[[name]];
}

# Helper function for converting a matrix of coordinates into a svg path
xy_to_svg <- function(xy) {
  n_row <- nrow(xy);
  svg_cmd <- c("M", rep("L", n_row - 2));
  svg_path <- c(paste(svg_cmd,
                      apply(xy[-n_row, ], 1, paste, collapse = " "),
                      collapse = " "),
                "Z");
  return(paste(svg_path, collapse = " "));
}


# Plotly definitions ------------------------------------------------------

updatemenus <- list(
  list(
    x = 0.5, y = -0.01,
    active = -1,
    direction = 'right',
    type = 'buttons',
    buttons = list(
      list(
        label = "Dbh",
        method = "update",
        args = list(list(visible = c(TRUE, FALSE, TRUE, FALSE, TRUE)))),
      list(
        label = "Reset",
        method = "update",
        args = list(list(visible = c(TRUE, TRUE, FALSE, TRUE, FALSE))))) # Last trace comes first, then following the original trace order
  )
)

annot <- list(list(text = "Mark trees by:",
                   x = 0, y = -0.05,
                   xref = 'paper', yref = 'paper',
                   showarrow = FALSE));

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE,
  domain = c(0, 1)
);


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

  output$stand <- renderPlotly({
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

    # pop()$data[, plot(x_tree, y_tree, asp = 1, axes = FALSE, ann = FALSE, pch = 1,
    #                   xlim = range(x_tree), ylim = range(y_tree),
    #                   mai = c(0, 0, 0, 0))];
    # s_tree$data[, points(x_tree, y_tree, pch = 16)];
    # s_loc$data[, points(x_s, y_s, pch = 4)];

    asp_rat <- pop()$data[, diff(range(x_tree))/diff(range(y_tree))];
    plot_ly(data = pop()$data,
                 type = 'scatter',
                 mode = "markers",
                 width = asp_rat*700,
                 height = 700,
                 showlegend = FALSE) %>%
      add_markers(data = pop()$data, x = ~x_tree, y = ~y_tree,
                  visible = TRUE,
                  marker = list(size = 10,
                                color = 'grey',
                                symbol = 'circle',
                                line = list(color = 'white',
                                            width = 1)),
                  text = ~paste("Dbh: ", dbh),
                  name = "Population") %>%
      add_markers(data = pop()$data, x = ~x_tree, y = ~y_tree,
                  visible = FALSE,
                  marker = list(size = hberg_beech$data[, 10*scale(dbh, center = FALSE)],
                                sizemin = 5,
                                color = 'grey',
                                symbol = 'circle',
                                line = list(color = 'white',
                                            width = 1)),
                  text = ~paste("Dbh: ", dbh),
                  name = "Population") %>%
      add_markers(data = s_tree$data, x = ~x_tree, y = ~y_tree,
                  visible = TRUE,
                  marker = list(size = 10,
                                color = 'black',
                                line = list(color = 'white',
                                            width = 1),
                                opacity = 0.5,
                                visible = TRUE),
                  name = "Selected") %>%
      add_markers(data = s_tree$data, x = ~x_tree, y = ~y_tree,
                  visible = FALSE,
                  marker = list(size = s_tree$data[, 10*scale(dbh, center = FALSE)],
                                sizemin = 5,
                                color = 'black',
                                line = list(color = 'white',
                                            width = 1),
                                opacity = 0.5,
                                visible = FALSE),
                  name = "Selected") %>%
      add_markers(data = s_loc$data, x = ~x_s, y = ~y_s,
                  visible = TRUE,
                  marker = list(size = 10,
                                symbol = 'x',
                                color = 'blue',
                                visible = TRUE),
                  text = ~paste("ID: ", id_point),
                  showlegend = FALSE, inherit = FALSE, name = "Sample location") %>%
      layout(xaxis = ax, yaxis = ax,
             updatemenus = updatemenus,
             annotations = annot
             ,shapes = list(
               list(type = "path",
                    fillcolor = "",
                    line = list(color = "grey", opacity = 0.5, dash = "dash"),
                    path = xy_to_svg(pop()$boundary[1]@polygons[[1]]@Polygons[[1]]@coords),
                    xref = "x", yref = "y")));
  });


  output$estimates <- renderDataTable(
    dt_sim_res$a[,
                 list(y_hat = mean(y_hat),
                      v_emp = var(y_hat),
                      v_hat = mean(v_hat)),
                 by = list(variable, est_appr)]
  );

  output$distribution <- renderPlot({
    A <- sum(extract_area(pop()$boundary))/10000;
    dt_y <- data.table(variable = input$select_var,
                       y = pop()$data[, sapply(.SD, sum), .SDcols = input$select_var]/A);
    ggplot(dt_sim_res$a, aes(y_hat)) +
      geom_histogram() +
      facet_grid(est_appr ~ variable, scales = "free") +
      geom_vline(aes(xintercept = y), dt_y) +
      theme_light()
  });
})
