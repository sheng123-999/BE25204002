#' 解码粒子并计算多目标适应度（4PL物流调度优化）
#'
#' 针对第四方物流（4PL）的多目标资源调度问题，该函数将粒子的位置（资源分配方案）解码，
#' 计算包含总成本、总完成时间、资源负载方差等6个目标的适应度向量，同时考虑任务优先级、
#' 资源时间窗口、依赖关系、切换成本等约束条件。
#'
#' @param particle 列表，单个粒子对象，包含position（子任务-资源映射）、velocity等属性。
#' @param tasks 列表，物流任务集合，每个任务包含：
#' \itemize{
#' \item priority：任务优先级（1-3，3为最高）；
#' \item due_date：任务截止时间；
#' \item delay_penalty：延迟惩罚系数；
#' \item activities：子任务列表，每个子任务包含名称和可用资源的执行时间/成本/需求；
#' \item relationships：子任务间的依赖关系（0表示串行，1表示并行）。
#' }
#' @param self_resource 列表，自有资源信息，每个资源包含capacity（容量）属性。
#' @param outsourced_resource 列表，外包资源信息，每个资源包含capacity（容量）和cost_ratio（成本系数）属性。
#' @param resource_windows 列表，资源时间窗口，每个资源对应一个二元向量（开始时间，结束时间）。
#' @param resource_joint_cost 列表，资源切换成本，键为“资源A,资源B”，值为二元向量（切换时间，切换成本）。
#' @param resource_mapping 列表，自有资源到外包资源的映射（如M1 = "O1"）。
#'
#' @return 列表，包含：
#' \itemize{
#' \item fitness：数值向量，长度为6（总成本、总时间、利用率方差、直接成本、连接成本、延迟惩罚）；
#' \item schedule：列表，任务名称到子任务详情的映射（每个子任务包含start、end、resource、cost、demand、outsourced_amount等）。
#' }
#'
#' @details 该函数是4PL物流调度多目标优化的核心解码函数，实现了对粒子位置的解析和多目标适应度的计算，
#' 考虑了任务依赖、资源冲突、优先级约束等实际物流场景中的关键因素。适应度向量越小，代表调度方案越优。
#'
#' @examples
#' # 构建示例数据
#' resource_mapping <- list(M1 = "O1", M2 = "O2")
#' tasks <- list(
#' task1 = list(
#' priority = 2,
#' due_date = 10,
#' delay_penalty = 5,
#' activities = list(
#' list("subtask1_1", list(M1 = c(2.0, 10.0, 2), O1 = c(1.0, 20.0, 2))),
#' list("subtask1_2", list(M2 = c(3.0, 15.0, 3), O2 = c(2.0, 25.0, 3)))
#' ),
#' relationships = c(0)
#' )
#' )
#' self_resource <- list(M1 = list(capacity = 4), M2 = list(capacity = 5))
#' outsourced_resource <- list(
#' O1 = list(capacity = 10, cost_ratio = 1.2),
#' O2 = list(capacity = 10, cost_ratio = 1.2)
#' )
#' resource_windows <- list(M1 = c(0.0, 20.0), M2 = c(0.0, 20.0), O1 = c(0.0, 20.0), O2 = c(0.0, 20.0))
#' resource_joint_cost <- list("M1,M1" = c(0.0, 0.0), "M1,O1" = c(1.0, 5.0))
#'
#' # 初始化粒子
#' particles <- initialize_particles(pop_size = 1, tasks = tasks, resource_mapping = resource_mapping)
#'
#' # 解码粒子并计算适应度
#' result <- decode_schedule(
#' particle = particles[[1]],
#' tasks = tasks,
#' self_resource = self_resource,
#' outsourced_resource = outsourced_resource,
#' resource_windows = resource_windows,
#' resource_joint_cost = resource_joint_cost,
#' resource_mapping = resource_mapping
#' )
#' print(result$fitness)
#' print(result$schedule)
#'
#' @export
decode_schedule <- function(particle, tasks, self_resource, outsourced_resource,
                            resource_windows, resource_joint_cost, resource_mapping) {
  # 初始化变量
  total_direct_cost <- 0.0
  total_connection_cost <- 0.0
  total_delay_penalty <- 0.0
  total_time <- 0.0
  schedule <- list() # 记录schedule
  resource_timeline <- list()
  all_resources <- c(names(self_resource), names(outsourced_resource))
  for (res in all_resources) {
    resource_timeline[[res]] <- list(
      usage = list(),
      capacity = ifelse(res %in% names(self_resource),
                        self_resource[[res]]$capacity,
                        outsourced_resource[[res]]$capacity)
    )
  }
  # 子任务时间记录
  subtask_end_times <- list()
  subtask_start_times <- list()
  for (task_name in names(tasks)) {
    subtask_end_times[[task_name]] <- list()
    subtask_start_times[[task_name]] <- list()
    schedule[[task_name]] <- list() # 每个任务的子任务列表
  }
  # 依赖关系解析
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
  # 按优先级排序任务
  sorted_tasks <- names(tasks)[order(sapply(tasks, function(x) x$priority), decreasing = TRUE)]
  # 处理每个任务的子任务
  for (task_name in sorted_tasks) {
    task <- tasks[[task_name]]
    for (idx in seq_along(task$activities)) {
      subtask <- task$activities[[idx]]
      subtask_name <- subtask[[1]]
      res <- particle$position[[subtask_name]]
      if (is.null(res)) next
      res_info <- subtask[[2]][[res]]
      process_time <- round(res_info[1], 1) # 处理时间（保留1位小数）
      base_cost <- res_info[2]
      demand <- as.integer(res_info[3]) # 资源需求（整数）
      # 资源时间窗口
      time_window <- resource_windows[[res]]
      S_Mi <- round(time_window[1], 1)
      E_Mi <- round(time_window[2], 1)
      # 计算直接成本
      cost <- base_cost
      if (task$priority == 3 && grepl("^O", res)) {
        cost <- base_cost * outsourced_resource[[res]]$cost_ratio
      } else if (task$priority == 3 && grepl("^M", res)) {
        mapped_res <- resource_mapping[[res]]
        if (!is.null(mapped_res) && mapped_res %in% names(subtask[[2]])) {
          mapped_info <- subtask[[2]][[mapped_res]]
          cost <- mapped_info[2] * outsourced_resource[[mapped_res]]$cost_ratio
        }
      }
      # 处理依赖关系
      start_time <- S_Mi
      dep_subtask <- dependency[[subtask_name]]
      if (!is.null(dep_subtask)) {
        dep_end <- subtask_end_times[[task_name]][[dep_subtask]]
        if (is.null(dep_end)) {
          return(list(fitness = rep(Inf, 6), schedule = list())) # 依赖错误，返回无穷大
        }
        prev_res <- NULL
        for (act in task$activities) {
          if (act[[1]] == dep_subtask) {
            prev_res <- particle$position[[dep_subtask]]
            break
          }
        }
        if (is.null(prev_res)) prev_res <- res
        joint_key <- paste(prev_res, res, sep = ",")
        joint_info <- resource_joint_cost[[joint_key]]
        if (is.null(joint_info)) joint_info <- c(0.0, 0.0)
        joint_time <- round(joint_info[1], 1)
        joint_cost <- joint_info[2]
        start_time <- max(start_time, round(dep_end + joint_time, 1))
        total_connection_cost <- total_connection_cost + joint_cost
      }
      # 资源冲突处理
      available_capacity <- as.integer(resource_timeline[[res]]$capacity)
      existing_usage <- 0
      t_key <- as.character(start_time)
      if (t_key %in% names(resource_timeline[[res]]$usage)) {
        existing_usage <- resource_timeline[[res]]$usage[[t_key]]
      }
      total_demand <- existing_usage + demand
      outsourced_amount <- max(0, total_demand - available_capacity)
      # 计算外包成本
      if (grepl("^M", res) && outsourced_amount > 0) {
        outsourced_res <- resource_mapping[[res]]
        outsourcing_cost <- outsourced_amount * outsourced_resource[[outsourced_res]]$cost_ratio
        cost <- cost + outsourcing_cost
      }
      # 计算结束时间
      end_time <- round(start_time + process_time, 1)
      # 更新资源时间线
      for (t in seq(floor(start_time), ceiling(end_time), by = 1)) {
        t_key <- as.character(t)
        if (t_key %in% names(resource_timeline[[res]]$usage)) {
          resource_timeline[[res]]$usage[[t_key]] <- min(
            resource_timeline[[res]]$usage[[t_key]] + demand,
            available_capacity
          )
        } else {
          resource_timeline[[res]]$usage[[t_key]] <- min(demand, available_capacity)
        }
      }
      # 记录到schedule
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
      subtask_start_times[[task_name]][[subtask_name]] <- start_time
      subtask_end_times[[task_name]][[subtask_name]] <- end_time
      total_direct_cost <- total_direct_cost + cost
    }
    # 计算任务延迟惩罚
    if (length(subtask_end_times[[task_name]]) > 0) {
      task_end <- max(unlist(subtask_end_times[[task_name]]))
      total_time <- max(total_time, task_end)
      delay <- max(0, task_end - task$due_date)
      total_delay_penalty <- total_delay_penalty + task$delay_penalty * delay
    }
  }
  # 计算资源利用率方差
  utilization_rates <- c()
  for (res in names(self_resource)) {
    usage <- unlist(resource_timeline[[res]]$usage)
    capacity <- self_resource[[res]]$capacity
    if (capacity == 0) next
    util <- mean(usage / capacity, na.rm = TRUE)
    utilization_rates <- c(utilization_rates, util)
  }
  util_variance <- if (length(utilization_rates) >= 2) {
    stats::var(utilization_rates)
  } else {
    0.0
  }
  # 总目标计算
  total_cost <- total_direct_cost + total_connection_cost + total_delay_penalty
  # 异常值处理
  if (total_cost > 99999) {
    return(list(fitness = rep(Inf, 6), schedule = list()))
  }
  list(
    fitness = c(total_cost, total_time, util_variance, total_direct_cost, total_connection_cost, total_delay_penalty),
    schedule = schedule
  )
}