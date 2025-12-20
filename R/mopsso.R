#' Multi-Objective Particle Swarm Optimization (MOPSO) for Task Scheduling
#'
#' @title MOPSO for Multi-Objective Task Scheduling
#' @description
#' This function implements a multi-objective particle swarm optimization (MOPSO)
#' algorithm for task scheduling, aiming to optimize multiple objectives (e.g.,
#' cost, time, variance) simultaneously. It maintains an external archive of
#' non-dominated solutions (Pareto front) and returns the final Pareto optimal
#' solutions.
#'
#' @param tasks List. Task definitions including `id`, `priority`, `activities`,
#'   and `precedence`.
#' @param self_resource List. In-house resource definitions.
#' @param outsourced_resource List. Outsourced resource definitions.
#' @param resource_windows List. Resource availability time windows.
#' @param resource_joint_cost List. Joint cost between resources.
#' @param resource_mapping List. Mapping between in-house and outsourced resources.
#' @param pop_size Integer. Number of particles in the swarm.
#' @param max_gen Integer. Maximum number of generations.
#' @param max_archive_size Integer. Maximum size of the external archive.
#'
#' @return
#' A list of particles representing the Pareto front.
#'
#' @seealso
#' \code{\link{dominates}},
#' \code{\link{calculate_crowding_distance}},
#' \code{\link{non_dominated_sort}}
#'
#' @importFrom stats setNames
#'
#' @examples
#' # ==============================================
#' # Step 1: Construct Test Data (Simplified Example)
#' # ==============================================
#' tasks <- list(
#'   list(id = 1, priority = 2, activities = "machining", precedence = c()),
#'   list(id = 2, priority = 1, activities = "assembly", precedence = c(1)),
#'   list(id = 3, priority = 3, activities = "packaging", precedence = c(2))
#' )
#'
#' self_resource <- list(
#'   list(id = "S1", type = "machine", capacity = 1, cost_per_hour = 10),
#'   list(id = "S2", type = "worker", capacity = 2, cost_per_hour = 20)
#' )
#'
#' outsourced_resource <- list(
#'   list(id = "O1", type = "machine", capacity = 1, cost_per_hour = 15),
#'   list(id = "O2", type = "worker", capacity = 2, cost_per_hour = 25)
#' )
#'
#' resource_windows <- list(
#'   list(resource_id = "S1", start_time = 0, end_time = 100),
#'   list(resource_id = "S2", start_time = 0, end_time = 100),
#'   list(resource_id = "O1", start_time = 0, end_time = 100),
#'   list(resource_id = "O2", start_time = 0, end_time = 100)
#' )
#'
#' resource_joint_cost <- list(
#'   list(resource1_id = "S1", resource2_id = "S2", joint_cost = 0),
#'   list(resource1_id = "O1", resource2_id = "O2", joint_cost = 0)
#' )
#'
#' resource_mapping <- list(
#'   list(self_id = "S1", outsourced_id = "O1"),
#'   list(self_id = "S2", outsourced_id = "O2")
#' )
#'
#' # ==============================================
#' # Step 2: Mock Dependencies (Isolated Example)
#' # ==============================================
#' mock_initialize_particles <- function(pop_size, tasks, resource_mapping) {
#'   n_tasks <- length(tasks)
#'   n_resources <- length(resource_mapping)
#'   lapply(seq_len(pop_size), function(i) {
#'     list(
#'       position = runif(n_tasks * n_resources),
#'       fitness = NULL,
#'       schedule = NULL,
#'       best_position = NULL,
#'       best_fitness = NULL
#'     )
#'   })
#' }
#'
#' mock_decode_schedule <- function(particle, tasks, self_resource,
#'                                  outsourced_resource, resource_windows,
#'                                  resource_joint_cost, resource_mapping) {
#'   fitness <- stats::setNames(
#'     c(
#'       cost = runif(1, 100, 500),
#'       time = runif(1, 10, 50),
#'       variance = runif(1, 1, 10)
#'     ),
#'     c("cost", "time", "variance")
#'   )
#'
#'   schedule <- list(
#'     task_assignment = sample(
#'       c("S1", "S2", "O1", "O2"),
#'       length(tasks),
#'       replace = TRUE
#'     ),
#'     start_time = runif(length(tasks), 0, 20),
#'     end_time = runif(length(tasks), 20, 50)
#'   )
#'
#'   list(fitness = fitness, schedule = schedule)
#' }
#'
#' mock_update_particles <- function(particles, external_archive, gen, max_gen,
#'                                  tasks, resource_mapping, self_resource,
#'                                  outsourced_resource, resource_windows,
#'                                  resource_joint_cost) {
#'   lapply(particles, function(p) {
#'     p$position <- p$position + rnorm(length(p$position), 0, 0.1)
#'
#'     decoded <- mock_decode_schedule(
#'       particle = p,
#'       tasks = tasks,
#'       self_resource = self_resource,
#'       outsourced_resource = outsourced_resource,
#'       resource_windows = resource_windows,
#'       resource_joint_cost = resource_joint_cost,
#'       resource_mapping = resource_mapping
#'     )
#'
#'     p$fitness <- decoded$fitness
#'     p$schedule <- decoded$schedule
#'
#'     if (is.null(p$best_fitness) ||
#'         dominates(p$fitness, p$best_fitness)) {
#'       p$best_position <- p$position
#'       p$best_fitness <- p$fitness
#'     }
#'
#'     p
#'   })
#' }
#'
#' mock_manage_archive <- function(archive, max_archive_size) {
#'   if (length(archive) <= max_archive_size) return(archive)
#'
#'   archive <- calculate_crowding_distance(archive)
#'
#'   ord <- order(
#'     vapply(archive, function(x) x$crowding_distance, numeric(1)),
#'     decreasing = TRUE
#'   )
#'
#'   archive[ord][seq_len(max_archive_size)]
#' }
#'
#' initialize_particles <- mock_initialize_particles
#' decode_schedule <- mock_decode_schedule
#' update_particles <- mock_update_particles
#' manage_archive <- mock_manage_archive
#'
#' # ==============================================
#' # Step 3: Run MOPSO
#' # ==============================================
#' set.seed(123)
#'
#' pareto_front <- mopsso(
#'   tasks = tasks,
#'   self_resource = self_resource,
#'   outsourced_resource = outsourced_resource,
#'   resource_windows = resource_windows,
#'   resource_joint_cost = resource_joint_cost,
#'   resource_mapping = resource_mapping,
#'   pop_size = 20,
#'   max_gen = 5,
#'   max_archive_size = 10
#' )
#'
#' # ==============================================
#' # Step 4: Analyze Results (CRAN-safe)
#' # ==============================================
#' cat("Number of Pareto solutions:", length(pareto_front), "\n")
#'
#' if (length(pareto_front) > 0) {
#'   pareto_fitness <- do.call(
#'     rbind,
#'     lapply(pareto_front, function(p) as.numeric(p$fitness))
#'   )
#'
#'   colnames(pareto_fitness) <- names(pareto_front[[1]]$fitness)
#'
#'   if (nrow(pareto_fitness) >= 1 &&
#'       all(c("cost", "time") %in% colnames(pareto_fitness))) {
#'     plot(
#'       pareto_fitness[, "cost"],
#'       pareto_fitness[, "time"],
#'       xlab = "Total Cost",
#'       ylab = "Total Time",
#'       main = "Pareto Front (Cost vs Time)",
#'       pch = 19
#'     )
#'   }
#' }
#'
#' @export
mopsso <- function(tasks,
                   self_resource,
                   outsourced_resource,
                   resource_windows,
                   resource_joint_cost,
                   resource_mapping,
                   pop_size = 100,
                   max_gen = 100,
                   max_archive_size = 20) {
  
  particles <- initialize_particles(pop_size, tasks, resource_mapping)
  
  if (length(particles) == 0) {
    warning("No particles initialized")
    return(list())
  }
  
  for (i in seq_along(particles)) {
    decoded <- decode_schedule(
      particle = particles[[i]],
      tasks = tasks,
      self_resource = self_resource,
      outsourced_resource = outsourced_resource,
      resource_windows = resource_windows,
      resource_joint_cost = resource_joint_cost,
      resource_mapping = resource_mapping
    )
    
    particles[[i]]$fitness <- decoded$fitness
    particles[[i]]$schedule <- decoded$schedule
    particles[[i]]$best_position <- particles[[i]]$position
    particles[[i]]$best_fitness <- decoded$fitness
  }
  
  external_archive <- list()
  
  for (gen in seq_len(max_gen)) {
    
    particles <- update_particles(
      particles = particles,
      external_archive = external_archive,
      gen = gen,
      max_gen = max_gen,
      tasks = tasks,
      resource_mapping = resource_mapping,
      self_resource = self_resource,
      outsourced_resource = outsourced_resource,
      resource_windows = resource_windows,
      resource_joint_cost = resource_joint_cost
    )
    
    for (p in particles) {
      if (is.null(p$fitness) || any(is.infinite(p$fitness))) next
      
      dominated <- FALSE
      if (length(external_archive) > 0) {
        dominated <- any(
          vapply(
            external_archive,
            function(a) dominates(a$fitness, p$fitness),
            logical(1)
          )
        )
      }
      
      if (!dominated) {
        external_archive <- append(external_archive, list(p))
      }
    }
    
    if (length(external_archive) > 0) {
      keep <- rep(TRUE, length(external_archive))
      for (i in seq_along(external_archive)) {
        for (j in seq_along(external_archive)) {
          if (i != j &&
              dominates(
                external_archive[[j]]$fitness,
                external_archive[[i]]$fitness
              )) {
            keep[i] <- FALSE
            break
          }
        }
      }
      external_archive <- external_archive[keep]
      external_archive <- manage_archive(external_archive, max_archive_size)
    }
  }
  
  if (length(external_archive) == 0) return(list())
  
  rounded <- lapply(external_archive, function(p) round(p$fitness, 4))
  keys <- vapply(rounded, function(x) paste(x, collapse = ","), character(1))
  
  external_archive[!duplicated(keys)]
}
