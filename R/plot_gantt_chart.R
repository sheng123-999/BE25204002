#' Plot Gantt chart for scheduling results
#'
#' @param schedule Result$schedule returned by decode_schedule()
#' @param title Plot title
#' @param filename File name only. Default "gantt_chart.png"; NULL means not saved.
#' @param width Image width (inches)
#' @param height Image height (inches)
#' @param dpi Resolution
#'
#' @export
plot_gantt_chart <- function(schedule,
                             title = "Gantt Chart",
                             filename = "gantt_chart.png",
                             width = 14,
                             height = 8,
                             dpi = 300) {
  
  df_list <- list()
  for (task_name in names(schedule)) {
    for (act in schedule[[task_name]]) {
      label <- act$subtask
      if (act$outsourced_amount > 0) {
        label <- paste0(label, "\nOutsourced: ", act$outsourced_amount)
      }
      
      df_list[[length(df_list) + 1]] <- data.frame(
        Task     = task_name,
        Start    = act$start,
        End      = act$end,
        Resource = act$resource,
        Label    = label,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(df_list) == 0) {
    stop("No data found in schedule")
  }
  
  df <- do.call(rbind, df_list)
  
  ordered_res <- sort(unique(df$Resource))
  df$Resource <- factor(df$Resource, levels = ordered_res)
  df$Y <- as.integer(df$Resource)
  
  task_colors <- scales::hue_pal()(length(unique(df$Task)))
  names(task_colors) <- unique(df$Task)
  
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      xmin = Start,
      xmax = End,
      ymin = Y - 0.4,
      ymax = Y + 0.4,
      fill = Task
    )
  ) +
    ggplot2::geom_rect(color = "black") +
    ggplot2::geom_text(
      ggplot2::aes(x = (Start + End) / 2, y = Y, label = Label),
      size = 3
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(ordered_res),
      labels = ordered_res
    ) +
    ggplot2::scale_fill_manual(values = task_colors) +
    ggplot2::labs(
      title = title,
      x = "Time",
      y = "Resource"
    ) +
    ggplot2::theme_minimal()
  
  print(p)
  
  if (!is.null(filename)) {
    example_dir <- system.file("example", package = "BE25204002")
    if (example_dir != "") {
      ggplot2::ggsave(
        filename = file.path(example_dir, filename),
        plot = p,
        width = width,
        height = height,
        dpi = dpi
      )
    }
  }
  
  invisible(p)
}

utils::globalVariables(
  c("Start", "End", "Y", "Task", "Label")
)

