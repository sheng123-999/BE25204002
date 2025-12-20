#' 判断向量a是否支配向量b（多目标最小化，越小越优）
#' @title 多目标支配判断函数
#' @description
#' 检查解 a 是否支配解 b：
#' a 在所有目标上不劣于 b，且至少在一个目标上严格优于 b。
#' @param a 数值向量，第一个解的适应度向量。
#' @param b 数值向量，第二个解的适应度向量。
#' @return 逻辑值，TRUE 表示 a 支配 b，FALSE 表示不支配。
#' @examples
#' # Example 1: a dominates b
#' a <- c(2, 3)
#' b <- c(3, 4)
#' dominates(a, b)  # TRUE
#'
#' # Example 2: a equals b, no domination
#' a <- c(2, 3)
#' b <- c(2, 3)
#' dominates(a, b)  # FALSE
#'
#' # Example 3: a is dominated by b
#' a <- c(3, 4)
#' b <- c(2, 3)
#' dominates(a, b)  # FALSE
#'
#' # Example 4: three-objective domination
#' a <- c(1, 2, 3)
#' b <- c(1, 3, 4)
#' dominates(a, b)  # TRUE
#'
#' # Example 5: different vector lengths (error handling)
#' tryCatch(
#'   dominates(c(1, 2), c(3)),
#'   error = function(e) message(e$message)
#' )
#' @export
dominates <- function(a, b) {
  # Check length consistency
  if (length(a) != length(b)) {
    stop("Fitness vectors must have the same length.")
  }
  
  has_strictly_better <- FALSE
  
  for (i in seq_along(a)) {
    if (a[i] > b[i]) {
      # a is worse in at least one objective
      return(FALSE)
    } else if (a[i] < b[i]) {
      # a is strictly better in at least one objective
      has_strictly_better <- TRUE
    }
  }
  
  # a dominates b only if it is no worse in all objectives
  # and strictly better in at least one objective
  has_strictly_better
}

#' 非支配排序（多目标优化）
#' @title 非支配排序函数
#' @description
#' 对粒子群进行非支配排序，返回每个粒子的前沿等级（rank），
#' rank = 1 表示 Pareto 最优前沿。
#' @param particles 列表，每个元素是包含 fitness 数值向量的粒子，
#'   例如 list(fitness = c(1, 2), position = ...)。
#' @return 整数向量，每个元素对应粒子的前沿等级。
#' @examples
#' # Example 1: general case
#' set.seed(123)
#' particles <- list(
#'   list(fitness = c(2, 3), position = c(0.1, 0.2)),
#'   list(fitness = c(1, 2), position = c(0.3, 0.4)),
#'   list(fitness = c(3, 1), position = c(0.5, 0.6)),
#'   list(fitness = c(4, 5), position = c(0.7, 0.8)),
#'   list(fitness = c(2, 2), position = c(0.9, 0.1)),
#'   list(fitness = c(5, 6), position = c(0.2, 0.3))
#' )
#'
#' ranks <- non_dominated_sort(particles)
#' data.frame(
#'   Particle = seq_along(ranks),
#'   Rank = ranks,
#'   Fitness = sapply(particles, function(x) paste(x$fitness, collapse = ","))
#' )
#'
#' # Example 2: all non-dominated
#' particles2 <- list(
#'   list(fitness = c(1, 4)),
#'   list(fitness = c(2, 3)),
#'   list(fitness = c(3, 2)),
#'   list(fitness = c(4, 1))
#' )
#' non_dominated_sort(particles2)
#'
#' # Example 3: single particle
#' particles3 <- list(list(fitness = c(1, 2)))
#' non_dominated_sort(particles3)
#'
#' # Example 4: three-objective case
#' particles4 <- list(
#'   list(fitness = c(1, 2, 3)),
#'   list(fitness = c(2, 2, 2)),
#'   list(fitness = c(1, 3, 4)),
#'   list(fitness = c(3, 1, 2))
#' )
#' non_dominated_sort(particles4)
#' @export
non_dominated_sort <- function(particles) {
  # Input validation
  if (length(particles) == 0) {
    stop("Particle set must not be empty.")
  }
  
  if (!all(vapply(particles, function(x) "fitness" %in% names(x), logical(1)))) {
    stop("Each particle must contain a 'fitness' element.")
  }
  
  fitness_lengths <- vapply(particles, function(x) length(x$fitness), integer(1))
  if (length(unique(fitness_lengths)) > 1) {
    stop("All fitness vectors must have the same length.")
  }
  
  n <- length(particles)
  
  # dom_set[[i]]: indices of particles dominated by particle i
  dom_set <- vector("list", n)
  # dom_count[i]: number of particles dominating particle i
  dom_count <- integer(n)
  
  # Compute dominance relationships
  for (i in seq_len(n)) {
    f_i <- particles[[i]][["fitness"]]
    for (j in seq_len(n)) {
      if (i == j) next
      f_j <- particles[[j]][["fitness"]]
      
      if (dominates(f_i, f_j)) {
        dom_set[[i]] <- c(dom_set[[i]], j)
      } else if (dominates(f_j, f_i)) {
        dom_count[i] <- dom_count[i] + 1
      }
    }
  }
  
  # Initialize ranks
  rank <- integer(n)
  rank[] <- 1
  
  # First front
  current_front <- which(dom_count == 0)
  front_level <- 1
  
  # Iteratively identify subsequent fronts
  while (length(current_front) > 0) {
    next_front <- integer(0)
    
    for (i in current_front) {
      for (j in dom_set[[i]]) {
        dom_count[j] <- dom_count[j] - 1
        if (dom_count[j] == 0) {
          rank[j] <- front_level + 1
          next_front <- c(next_front, j)
        }
      }
    }
    
    front_level <- front_level + 1
    current_front <- next_front
  }
  
  rank
}
