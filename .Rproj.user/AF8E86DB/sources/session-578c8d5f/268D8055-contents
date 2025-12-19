#' Multi-Objective Particle Swarm Optimization (MOPSO)
#'
#' @title MOPSO main optimization routine
#'
#' @description
#' This function implements a multi-objective particle swarm optimization
#' (MOPSO) algorithm for task scheduling. It initializes a particle swarm,
#' iteratively updates particles, maintains an external archive of
#' non-dominated solutions, and returns the final Pareto front.
#'
#' @param tasks List. Task definitions including priorities, activities,
#'   and precedence constraints.
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
#' A list of particles representing the Pareto front. Each particle contains
#' its position, fitness vector, and decoded schedule.
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
