#' Decode Particle and Calculate Multi-Objective Fitness (4PL Logistics Scheduling Optimization)
#'
#' For the multi-objective resource scheduling problem of Fourth-Party Logistics (4PL), this function decodes the particle's position (resource allocation scheme),
#' calculates a fitness vector of 6 objectives including total cost, total completion time, resource load variance, direct cost, connection cost, and delay penalty,
#' while considering constraints such as task priority, resource time windows, dependencies, and switching costs.
#'
#' @param particle List, a single particle object containing attributes such as position (subtask-resource mapping) and velocity.
#' @param tasks List, collection of logistics tasks, each task includes:
#' \itemize{
#' \item priority: Task priority (1-3, 3 is the highest);
#' \item due_date: Task deadline;
#' \item delay_penalty: Delay penalty coefficient;
#' \item activities: List of subtasks, each subtask includes name and execution time/cost/demand of available resources;
#' \item relationships: Dependencies between subtasks (0 for serial, 1 for parallel).
#' }
#' @param self_resource List, information of self-owned resources, each resource includes a 'capacity' attribute.
#' @param outsourced_resource List, information of outsourced resources, each resource includes 'capacity' and 'cost_ratio' attributes.
#' @param resource_windows List, resource time windows, each resource corresponds to a binary vector (start time, end time).
#' @param resource_joint_cost List, resource switching costs, with keys as "ResourceA,ResourceB" and values as binary vectors (switching time, switching cost).
#' @param resource_mapping List, mapping from self-owned resources to outsourced resources (e.g., M1 = "O1").
#'
#' @return List containing:
#' \itemize{
#' \item fitness: Numeric vector (named) of length 6, with names:
#'   "total_cost", "total_time", "util_variance", "total_direct_cost", "total_connection_cost", "total_delay_penalty";
#' \item schedule: List, mapping from task names to subtask details (each subtask includes start, end, resource, cost, demand, outsourced_amount, etc.).
#' }
#'
#' @details This function is the core decoding function for multi-objective optimization of 4PL logistics scheduling,
#' implementing the parsing of particle positions and the calculation of multi-objective fitness.
#' It considers key factors in actual logistics scenarios such as task dependencies, resource conflicts, and priority constraints.
#' A smaller fitness vector indicates a better scheduling scheme.
#'
#' @examples
#' # Build example data
#' resource_mapping <- list(M1 = "O1", M2 = "O2")
#' tasks <- list(
#'   task1 = list(
#'     priority = 2,
#'     due_date = 10,
#'     delay_penalty = 5,
#'     activities = list(
#'       list("subtask1_1", list(M1 = c(2.0, 10.0, 2), O1 = c(1.0, 20.0, 2))),
#'       list("subtask1_2", list(M2 = c(3.0, 15.0, 3), O2 = c(2.0, 25.0, 3)))
#'     ),
#'     relationships = c(0)
#'   )
#' )
#' self_resource <- list(M1 = list(capacity = 4), M2 = list(capacity = 5))
#' outsourced_resource <- list(
#'   O1 = list(capacity = 10, cost_ratio = 1.2),
#'   O2 = list(capacity = 10, cost_ratio = 1.2)
#' )
#' resource_windows <- list(M1 = c(0.0, 20.0), M2 = c(0.0, 20.0), O1 = c(0.0, 20.0), O2 = c(0.0, 20.0))
#' resource_joint_cost <- list("M1,M1" = c(0.0, 0.0), "M1,O1" = c(1.0, 5.0))
#'
#' # Mock initialize_particles function (for example independence)
#' initialize_particles <- function(pop_size, tasks, resource_mapping) {
#'   subtask_names <- unlist(lapply(tasks, function(x) lapply(x$activities, `[[`, 1)))
#'   lapply(1:pop_size, function(i) {
#'     list(
#'       position = setNames(sample(c(names(self_resource), names(outsourced_resource)),
#'                                 length(subtask_names), replace = TRUE),
#'                          subtask_names),
#'       velocity = NULL
#'     )
#'   })
#' }
#'
#' # Initialize particles
#' particles <- initialize_particles(pop_size = 1, tasks = tasks, resource_mapping = resource_mapping)
#'
#' # Decode particle and calculate fitness
#' result <- decode_schedule(
#'   particle = particles[[1]],
#'   tasks = tasks,
#'   self_resource = self_resource,
#'   outsourced_resource = outsourced_resource,
#'   resource_windows = resource_windows,
#'   resource_joint_cost = resource_joint_cost,
#'   resource_mapping = resource_mapping
#' )
#' print(result$fitness) # Now has named columns
#' print(result$schedule)
#'
#' @export
decode_schedule <- function(particle, tasks, self_resource, outsourced_resource,
                            resource_windows, resource_joint_cost, resource_mapping) {
  # Initialize variables
  total_direct_cost <- 0.0
  total_connection_cost <- 0.0
  total_delay_penalty <- 0.0
  total_time <- 0.0
  schedule <- list() # Record schedule
  resource_timeline <- list()
  all_resources <- c(names(self_resource), names(outsourced_resource))
  
  # Initialize resource timeline (capacity and usage)
  for (res in all_resources) {
    resource_timeline[[res]] <- list(
      usage = list(),
      capacity = ifelse(res %in% names(self_resource),
                        self_resource[[res]]$capacity,
                        outsourced_resource[[res]]$capacity)
    )
  }
  
  # Subtask time records
  subtask_end_times <- lapply(names(tasks), function(x) list())
  subtask_start_times <- lapply(names(tasks), function(x) list())
  names(subtask_end_times) <- names(tasks)
  names(subtask_start_times) <- names(tasks)
  schedule <- lapply(names(tasks), function(x) list())
  names(schedule) <- names(tasks)
  
  # Parse dependencies between subtasks
  dependency <- list()
  for (task_name in names(tasks)) {
    task <- tasks[[task_name]]
    activities <- task$activities
    if (length(activities) == 0) next
    dependency[[activities[[1]][[1]]]] <- NULL
    current_dep <- NULL
    for (i in 2:length(activities)) {
      rel_idx <- i - 1
      if (rel_idx <= length(task$relationships) && task$relationships[rel_idx] == 0) {
        current_dep <- activities[[i-1]][[1]]
      }
      dependency[[activities[[i]][[1]]]] <- current_dep
    }
  }
  
  # Sort tasks by priority (descending)
  sorted_tasks <- names(tasks)[order(sapply(tasks, function(x) x$priority), decreasing = TRUE)]
  
  # Process each subtask of each task
  for (task_name in sorted_tasks) {
    task <- tasks[[task_name]]
    for (idx in seq_along(task$activities)) {
      subtask <- task$activities[[idx]]
      subtask_name <- subtask[[1]]
      res <- particle$position[[subtask_name]]
      
      # Skip if resource is not assigned
      if (is.null(res) || !res %in% names(subtask[[2]])) {
        next
      }
      
      # Get resource information for the subtask
      res_info <- subtask[[2]][[res]]
      process_time <- round(res_info[1], 1) # Processing time (1 decimal place)
      base_cost <- res_info[2]
      demand <- as.integer(res_info[3]) # Resource demand (integer)
      
      # Resource time window
      time_window <- resource_windows[[res]]
      S_Mi <- round(time_window[1], 1)
      E_Mi <- round(time_window[2], 1)
      
      # Calculate direct cost (consider priority and outsourcing)
      cost <- base_cost
      if (task$priority == 3) {
        if (grepl("^O", res)) {
          cost <- base_cost * outsourced_resource[[res]]$cost_ratio
        } else if (grepl("^M", res)) {
          mapped_res <- resource_mapping[[res]]
          if (!is.null(mapped_res) && mapped_res %in% names(subtask[[2]])) {
            mapped_info <- subtask[[2]][[mapped_res]]
            cost <- mapped_info[2] * outsourced_resource[[mapped_res]]$cost_ratio
          }
        }
      }
      
      # Handle dependency relationships and switching costs
      start_time <- S_Mi
      dep_subtask <- dependency[[subtask_name]]
      if (!is.null(dep_subtask)) {
        dep_end <- subtask_end_times[[task_name]][[dep_subtask]]
        if (is.null(dep_end)) {
          # Return infinite fitness for dependency errors
          fitness <- setNames(rep(Inf, 6),
                              c("total_cost", "total_time", "util_variance",
                                "total_direct_cost", "total_connection_cost", "total_delay_penalty"))
          return(list(fitness = fitness, schedule = list()))
        }
        # Get previous resource
        prev_res <- particle$position[[dep_subtask]]
        if (is.null(prev_res)) prev_res <- res
        # Calculate switching cost and time
        joint_key <- paste(prev_res, res, sep = ",")
        joint_info <- resource_joint_cost[[joint_key]] %||% c(0.0, 0.0)
        joint_time <- round(joint_info[1], 1)
        joint_cost <- joint_info[2]
        start_time <- max(start_time, round(dep_end + joint_time, 1))
        total_connection_cost <- total_connection_cost + joint_cost
      }
      
      # Handle resource conflicts and outsourcing
      available_capacity <- as.integer(resource_timeline[[res]]$capacity)
      existing_usage <- sum(unlist(resource_timeline[[res]]$usage[as.character(seq(floor(start_time), ceiling(start_time), by = 1))]), na.rm = TRUE)
      total_demand <- existing_usage + demand
      outsourced_amount <- max(0, total_demand - available_capacity)
      
      # Calculate outsourcing cost for self-owned resources
      if (grepl("^M", res) && outsourced_amount > 0) {
        outsourced_res <- resource_mapping[[res]]
        if (!is.null(outsourced_res) && outsourced_res %in% names(outsourced_resource)) {
          outsourcing_cost <- outsourced_amount * outsourced_resource[[outsourced_res]]$cost_ratio
          cost <- cost + outsourcing_cost
        }
      }
      
      # Calculate end time
      end_time <- round(start_time + process_time, 1)
      
      # Update resource timeline (usage)
      for (t in seq(floor(start_time), ceiling(end_time), by = 1)) {
        t_key <- as.character(t)
        resource_timeline[[res]]$usage[[t_key]] <- min(
          sum(resource_timeline[[res]]$usage[[t_key]], existing_usage, demand, na.rm = TRUE),
          available_capacity
        )
      }
      
      # Record subtask details to schedule
      outsourced <- list()
      if (grepl("^M", res) && outsourced_amount > 0) {
        outsourced_res <- resource_mapping[[res]]
        outsourced[[outsourced_res]] <- outsourced_amount
      }
      
      schedule[[task_name]][[idx]] <- list(
        subtask = subtask_name,
        start = start_time,
        end = end_time,
        resource = res,
        cost = cost,
        delay_penalty = task$delay_penalty,
        due_date = task$due_date,
        demand = demand,
        outsourced = outsourced,
        priority = task$priority,
        outsourced_amount = outsourced_amount
      )
      
      # Update subtask time records
      subtask_start_times[[task_name]][[subtask_name]] <- start_time
      subtask_end_times[[task_name]][[subtask_name]] <- end_time
      
      # Accumulate direct cost
      total_direct_cost <- total_direct_cost + cost
    }
    
    # Calculate task delay penalty
    task_end_times <- unlist(subtask_end_times[[task_name]])
    if (length(task_end_times) > 0) {
      task_end <- max(task_end_times)
      total_time <- max(total_time, task_end)
      delay <- max(0, task_end - task$due_date)
      total_delay_penalty <- total_delay_penalty + task$delay_penalty * delay
    }
  }
  
  # Calculate resource utilization variance (for self-owned resources)
  utilization_rates <- c()
  for (res in names(self_resource)) {
    usage <- unlist(resource_timeline[[res]]$usage)
    capacity <- self_resource[[res]]$capacity
    if (capacity == 0 || length(usage) == 0) next
    util <- mean(usage / capacity, na.rm = TRUE)
    utilization_rates <- c(utilization_rates, util)
  }
  util_variance <- if (length(utilization_rates) >= 2) {
    stats::var(utilization_rates)
  } else {
    0.0
  }
  
  # Calculate total cost (sum of all costs)
  total_cost <- total_direct_cost + total_connection_cost + total_delay_penalty
  
  # Handle abnormal values (infinite cost)
  if (total_cost > 99999) {
    fitness <- setNames(rep(Inf, 6),
                        c("total_cost", "total_time", "util_variance",
                          "total_direct_cost", "total_connection_cost", "total_delay_penalty"))
    return(list(fitness = fitness, schedule = list()))
  }
  
  # Return fitness vector WITH NAMES (key fix for the index error)
  fitness <- setNames(
    c(total_cost, total_time, util_variance, total_direct_cost, total_connection_cost, total_delay_penalty),
    c("total_cost", "total_time", "util_variance", "total_direct_cost", "total_connection_cost", "total_delay_penalty")
  )
  
  list(
    fitness = fitness,
    schedule = schedule
  )
}

# Helper function: Safe null check (equivalent to %||% in purrr)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}