#' Multi-Objective Particle Swarm Optimization (MOPSO) for Task Scheduling
#'
#' @title MOPSO for Multi-Objective Task Scheduling
#' @description
#' This function implements a multi-objective particle swarm optimization (MOPSO) algorithm for task scheduling,
#' aiming to optimize multiple objectives (e.g., cost, time, variance) simultaneously. It maintains an external archive
#' of non-dominated solutions (Pareto front) and returns the final Pareto optimal solutions.
#'
#' @param tasks List. Task definitions including `id`, `priority`, `activities`, and `precedence` (precedence constraints).
#' Each element is a sublist for a single task.
#' @param self_resource List. In-house resource definitions including `id`, `type`, `capacity`, and `cost_per_hour`.
#' @param outsourced_resource List. Outsourced resource definitions including `id`, `type`, `capacity`, and `cost_per_hour`.
#' @param resource_windows List. Resource availability time windows with `resource_id`, `start_time`, and `end_time`.
#' @param resource_joint_cost List. Joint cost between resources with `resource1_id`, `resource2_id`, and `joint_cost`.
#' @param resource_mapping List. Mapping between in-house and outsourced resources with `self_id` and `outsourced_id`.
#' @param pop_size Integer (default = 100). Number of particles in the swarm.
#' @param max_gen Integer (default = 100). Maximum number of generations for optimization.
#' @param max_archive_size Integer (default = 20). Maximum size of the external archive for non-dominated solutions.
#'
#' @return
#' A list of particles representing the Pareto front. Each particle contains:
#' \item{position}{Numeric vector: Encoded position of the particle (e.g., resource assignment and task scheduling).}
#' \item{fitness}{Numeric vector: Multi-objective fitness values (e.g., cost, time, variance).}
#' \item{schedule}{List: Decoded task schedule (resource assignment, start/end time of tasks).}
#' \item{best_position}{Numeric vector: Personal best position of the particle.}
#' \item{best_fitness}{Numeric vector: Personal best fitness values of the particle.}
#'
#' @examples
#' # ==============================================
#' # Step 1: Construct Test Data (Simplified for Example)
#' # ==============================================
#' # 1. Task definitions (3 tasks with precedence constraints)
#' tasks <- list(
#'   list(id = 1, priority = 2, activities = "machining", precedence = c()),
#'   list(id = 2, priority = 1, activities = "assembly", precedence = c(1)),
#'   list(id = 3, priority = 3, activities = "packaging", precedence = c(2))
#' )
#'
#' # 2. In-house resources (2 resources)
#' self_resource <- list(
#'   list(id = "S1", type = "machine", capacity = 1, cost_per_hour = 10),
#'   list(id = "S2", type = "worker", capacity = 2, cost_per_hour = 20)
#' )
#'
#' # 3. Outsourced resources (2 resources)
#' outsourced_resource <- list(
#'   list(id = "O1", type = "machine", capacity = 1, cost_per_hour = 15),
#'   list(id = "O2", type = "worker", capacity = 2, cost_per_hour = 25)
#' )
#'
#' # 4. Resource availability windows
#' resource_windows <- list(
#'   list(resource_id = "S1", start_time = 0, end_time = 100),
#'   list(resource_id = "S2", start_time = 0, end_time = 100),
#'   list(resource_id = "O1", start_time = 0, end_time = 100),
#'   list(resource_id = "O2", start_time = 0, end_time = 100)
#' )
#'
#' # 5. Resource joint cost (simplified: no joint cost)
#' resource_joint_cost <- list(
#'   list(resource1_id = "S1", resource2_id = "S2", joint_cost = 0),
#'   list(resource1_id = "O1", resource2_id = "O2", joint_cost = 0)
#' )
#'
#' # 6. Resource mapping (in-house <-> outsourced)
#' resource_mapping <- list(
#'   list(self_id = "S1", outsourced_id = "O1"),
#'   list(self_id = "S2", outsourced_id = "O2")
#' )
#'
#' # ==============================================
#' # Step 2: Define Mock Dependencies (for Example Run)
#' # Note: In real package, these functions are fully implemented
#' # ==============================================
#' # Initialize particles (random encoded position)
#' initialize_particles <- function(pop_size, tasks, resource_mapping) {
#'   n_tasks <- length(tasks)
#'   n_resources <- length(resource_mapping)
#'   lapply(1:pop_size, function(i) {
#'     list(
#'       position = runif(n_tasks * n_resources),  # Random encoded position
#'       fitness = NULL,
#'       schedule = NULL,
#'       best_position = NULL,
#'       best_fitness = NULL
#'     )
#'   })
#' }
#'
#' # Decode particle to schedule and calculate fitness
#' decode_schedule <- function(particle, tasks, self_resource, outsourced_resource,
#'                             resource_windows, resource_joint_cost, resource_mapping) {
#'   # Simplified: Random fitness (cost, time, variance) for example
#'   fitness <- c(
#'     cost = runif(1, 100, 500),
#'     time = runif(1, 10, 50),
#'     variance = runif(1, 1, 10)
#'   )
#'   # Simplified schedule
#'   schedule <- list(
#'     task_assignment = sample(c("S1", "S2", "O1", "O2"), length(tasks), replace = TRUE),
#'     start_time = runif(length(tasks), 0, 20),
#'     end_time = runif(length(tasks), 20, 50)
#'   )
#'   list(fitness = fitness, schedule = schedule)
#' }
#'
#' # Update particles (simplified: random position update)
#' update_particles <- function(particles, external_archive, gen, max_gen,
#'                             tasks, resource_mapping, self_resource, outsourced_resource,
#'                             resource_windows, resource_joint_cost) {
#'   lapply(particles, function(p) {
#'     # Random position update (simplified for example)
#'     p$position <- p$position + rnorm(length(p$position), 0, 0.1)
#'     # Re-decode to get new fitness
#'     decode_result <- decode_schedule(
#'       particle = p, tasks = tasks, self_resource = self_resource,
#'       outsourced_resource = outsourced_resource, resource_windows = resource_windows,
#'       resource_joint_cost = resource_joint_cost, resource_mapping = resource_mapping
#'     )
#'     p$fitness <- decode_result$fitness
#'     p$schedule <- decode_result$schedule
#'     # Update personal best
#'     if (is.null(p$best_fitness) || dominates(p$fitness, p$best_fitness)) {
#'       p$best_position <- p$position
#'       p$best_fitness <- p$fitness
#'     }
#'     p
#'   })
#' }
#'
#' # Manage archive size (use crowding distance from previous function)
#' manage_archive <- function(archive, max_archive_size) {
#'   if (length(archive) <= max_archive_size) return(archive)
#'   # Calculate crowding distance (reuse previous function)
#'   archive <- calculate_crowding_distance(archive)
#'   # Sort by crowding distance (descending) and keep top N
#'   archive_sorted <- archive[order(sapply(archive, function(x) x$crowding_distance), decreasing = TRUE)]
#'   archive_sorted[1:max_archive_size]
#' }
#'
#' # ==============================================
#' # Step 3: Run MOPSO (Small Parameters for Fast Example)
#' # ==============================================
#' set.seed(123)  # For reproducibility
#' pareto_front <- mopsso(
#'   tasks = tasks,
#'   self_resource = self_resource,
#'   outsourced_resource = outsourced_resource,
#'   resource_windows = resource_windows,
#'   resource_joint_cost = resource_joint_cost,
#'   resource_mapping = resource_mapping,
#'   pop_size = 20,  # Small population for fast run
#'   max_gen = 5,    # Small generations for fast run
#'   max_archive_size = 10
#' )
#'
#' # ==============================================
#' # Step 4: Analyze Results
#' # ==============================================
#' # Number of Pareto optimal solutions
#' cat("Number of Pareto optimal solutions:", length(pareto_front), "\n")
#'
#' # Extract fitness values of Pareto front
#' pareto_fitness <- do.call(rbind, lapply(pareto_front, function(p) p$fitness))
#' head(pareto_fitness)
#'
#' # Plot Pareto front (2D: cost vs time)
#' plot(pareto_fitness[, "cost"], pareto_fitness[, "time"],
#'      xlab = "Total Cost", ylab = "Total Time", main = "Pareto Front (Cost vs Time)",
#'      pch = 19, col = "blue")
#'
#' @seealso
#' \code{\link{dominates}}, \code{\link{calculate_crowding_distance}}, \code{\link{non_dominated_sort}}
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
  
  # Initialize particles
  particles <- initialize_particles(pop_size, tasks, resource_mapping)
  
  # Decode initial particles
  for (i in seq_along(particles)) {
    decode_result <- decode_schedule(
      particle = particles[[i]],
      tasks = tasks,
      self_resource = self_resource,
      outsourced_resource = outsourced_resource,
      resource_windows = resource_windows,
      resource_joint_cost = resource_joint_cost,
      resource_mapping = resource_mapping
    )
    
    particles[[i]]$fitness <- decode_result$fitness
    particles[[i]]$schedule <- decode_result$schedule
    particles[[i]]$best_position <- particles[[i]]$position
    particles[[i]]$best_fitness <- decode_result$fitness
  }
  
  # Initialize external archive
  external_archive <- list()
  
  # Main optimization loop
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
    
    # Update external archive with new particles
    for (particle in particles) {
      if (all(!is.infinite(particle$fitness))) {
        is_dominated <- any(
          vapply(
            external_archive,
            function(arch) dominates(arch$fitness, particle$fitness),
            logical(1)
          )
        )
        
        if (!is_dominated) {
          external_archive <- append(external_archive, list(particle))
        }
      }
    }
    
    # Remove dominated solutions from archive
    to_keep <- rep(TRUE, length(external_archive))
    for (i in seq_along(external_archive)) {
      for (j in seq_along(external_archive)) {
        if (i != j &&
            dominates(external_archive[[j]]$fitness,
                      external_archive[[i]]$fitness)) {
          to_keep[i] <- FALSE
          break
        }
      }
    }
    external_archive <- external_archive[to_keep]
    
    # Control archive size
    external_archive <- manage_archive(external_archive, max_archive_size)
    
    cat(
      sprintf(
        "Generation %d / %d | Archive size: %d\n",
        gen, max_gen, length(external_archive)
      )
    )
  }
  
  # Remove duplicate solutions in the Pareto front
  unique_keys <- character(0)
  pareto_front <- list()
  
  for (p in external_archive) {
    key <- paste(p$fitness, collapse = ",")
    if (!key %in% unique_keys) {
      unique_keys <- c(unique_keys, key)
      pareto_front <- append(pareto_front, list(p))
    }
  }
  
  pareto_front
}