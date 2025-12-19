#' 更新粒子群的速度和位置（PSO evolution core）
#'
#' @title Particle update function for MOPSO
#' @description
#' Update particle velocity and position based on personal best and
#' global best (sampled from external archive), then re-decode the particle
#' to obtain updated fitness and schedule, and update personal best.
#'
#' @param particles List. Particle swarm; each particle contains
#'   \code{position}, \code{velocity}, \code{best_position},
#'   \code{best_fitness}, \code{fitness}, and \code{schedule}.
#' @param external_archive List. External archive of non-dominated solutions.
#' @param gen Numeric. Current generation index.
#' @param max_gen Numeric. Maximum number of generations.
#' @param tasks List. Task definitions including priority, deadlines, activities, and precedence.
#' @param resource_mapping List. Mapping between in-house and outsourced resources.
#' @param self_resource List. In-house resource definitions.
#' @param outsourced_resource List. Outsourced resource definitions.
#' @param resource_windows List. Resource availability time windows.
#' @param resource_joint_cost List. Joint cost between resources.
#'
#' @return List. Updated particle swarm.
#'
#' @importFrom stats runif
#' @export
update_particles <- function(particles, external_archive, gen, max_gen, tasks, resource_mapping,
                             self_resource, outsourced_resource, resource_windows, resource_joint_cost) {
  # 初始化PSO参数
  w_max <- 0.9  # 最大惯性权重
  w_min <- 0.4  # 最小惯性权重
  w <- w_max - (w_max - w_min) * (gen / max_gen)  # 动态惯性权重（线性递减）
  c1 <- 2.0     # 个体学习因子
  c2 <- 1.0     # 社会学习因子
  random_prob <- 0.1  # 随机切换资源的概率
  
  # 遍历每个粒子进行更新
  for (i in seq_along(particles)) {
    particle <- particles[[i]]
    
    # 从外部存档中采样全局最优粒子（若无存档则使用当前粒子）
    if (length(external_archive) > 0) {
      gbest <- sample(external_archive, 1)[[1]]
    } else {
      gbest <- particle
    }
    
    # 遍历粒子的每个子任务位置（资源分配）进行更新
    for (subtask_name in names(particle$position)) {
      # 获取当前资源、个体最优资源、全局最优资源
      current_res <- particle$position[[subtask_name]]
      pbest_res <- particle$best_position[[subtask_name]]
      gbest_res <- gbest$position[[subtask_name]]
      
      # 计算与个体最优、全局最优的资源差异（0为相同，1为不同）
      d_pbest <- ifelse(pbest_res != current_res, 1, 0)
      d_gbest <- ifelse(gbest_res != current_res, 1, 0)
      
      # 计算速度（离散型PSO速度，代表资源切换的倾向）
      v_i <- w * particle$velocity[[subtask_name]] +
        c1 * runif(1) * d_pbest +
        c2 * runif(1) * d_gbest
      
      # 速度裁剪（限制在[-5,5]范围内，避免速度过大）
      particle$velocity[[subtask_name]] <- max(min(v_i, 5.0), -5.0)
      
      # 获取当前子任务的可用资源
      # 提取任务名（如从"P1_1"中提取"P1"）
      task_name <- sub("_[0-9]+$", "", subtask_name)
      task <- tasks[[task_name]]
      if (is.null(task)) {
        warning(paste("Task", task_name, "not found. Skipping this subtask."))
        next
      }
      
      # 查找子任务的详细信息（增加容错，避免匹配失败）
      subtask_idx <- which(sapply(task$activities, function(x) x[[1]] == subtask_name))
      if (length(subtask_idx) == 0) {
        warning(paste("Subtask", subtask_name, "not found. Skipping this subtask."))
        next
      }
      
      subtask_info <- task$activities[[subtask_idx[1]]]
      available_res <- names(subtask_info[[2]])
      
      # 根据任务优先级筛选可用资源（优先级3优先外包，否则优先自有）
      priority <- task$priority
      if (priority == 3) {
        possible_res <- available_res[grepl("^O", available_res)]
      } else {
        possible_res <- available_res[grepl("^M", available_res)]
      }
      
      # 容错：若筛选后无可用资源，则使用所有可用资源
      if (length(possible_res) == 0) {
        possible_res <- available_res
      }
      
      # 更新粒子位置（资源分配）
      if (runif(1) < random_prob) {
        # 随机选择资源（增加多样性）
        new_res <- sample(possible_res, 1)
      } else {
        # 根据速度计算资源切换概率
        p_switch <- 1 / (1 + exp(-v_i))
        if (runif(1) < p_switch) {
          # 优先选择个体最优，其次全局最优，最后随机
          if (pbest_res %in% possible_res) {
            new_res <- pbest_res
          } else if (gbest_res %in% possible_res) {
            new_res <- gbest_res
          } else {
            new_res <- sample(possible_res, 1)
          }
        } else {
          # 保持当前资源
          new_res <- current_res
        }
      }
      
      # 赋值新资源到粒子位置
      particle$position[[subtask_name]] <- new_res
    }
    
    # 解码新位置，计算适应度和调度方案
    decode_result <- decode_schedule(
      particle = particle,
      tasks = tasks,
      self_resource = self_resource,
      outsourced_resource = outsourced_resource,
      resource_windows = resource_windows,
      resource_joint_cost = resource_joint_cost,
      resource_mapping = resource_mapping
    )
    
    # 更新粒子的适应度和调度方案
    particle$fitness <- decode_result$fitness
    particle$schedule <- decode_result$schedule
    
    # 更新个体最优（若当前适应度支配个体最优，或个体最优为无穷大）
    if (all(is.infinite(particle$best_fitness)) ||
        dominates(particle$fitness, particle$best_fitness)) {
      particle$best_position <- particle$position
      particle$best_fitness <- particle$fitness
    }
    
    # 将更新后的粒子放回粒子群
    particles[[i]] <- particle
  }
  
  # 返回更新后的粒子群
  return(particles)
}
