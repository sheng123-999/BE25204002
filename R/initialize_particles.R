#' 初始化粒子群（4PL物流调度多目标优化）
#'
#' 针对第四方物流（4PL）的资源调度多目标优化问题，该函数根据任务优先级和资源类型（自有/外包）
#' 初始化粒子群，每个粒子代表一个资源分配方案，包含位置、速度、历史最优位置等属性。
#'
#' @param pop_size 正整数，粒子群规模（建议取值：50-200，根据问题复杂度调整）。
#' @param tasks 列表，物流任务集合（同`decode_schedule`的`tasks`参数）。
#' @param resource_mapping 列表，自有资源到外包资源的映射（如`M1 = "O1"`）。
#'
#' @return 列表，长度为`pop_size`，每个元素为粒子对象，包含：
#' \itemize{
#'   \item `position`：列表，子任务名称到资源名称的映射（核心决策变量）；
#'   \item `velocity`：列表，子任务的资源选择速度（初始为0）；
#'   \item `best_position`：列表，粒子的历史最优位置（初始为当前位置）；
#'   \item `fitness`：数值向量，当前位置的适应度（初始为无穷大）；
#'   \item `best_fitness`：数值向量，历史最优适应度（初始为无穷大）。
#' }
#'
#' @details 该函数是粒子群优化（PSO）算法的初始化模块，根据任务优先级进行资源选择偏向：
#' 高优先级任务（优先级3）优先选择外包资源（O开头），其他优先级任务优先选择自有资源（M开头），
#' 保证初始解的合理性和算法收敛速度。
#'
#' @examples
#' # 构建示例数据
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
#'   ),
#'   task2 = list(
#'     priority = 3,
#'     due_date = 15,
#'     delay_penalty = 3,
#'     activities = list(
#'       list("subtask2_1", list(M1 = c(2.0, 10.0, 2), O1 = c(1.0, 20.0, 2))),
#'       list("subtask2_2", list(M2 = c(3.0, 15.0, 3), O2 = c(2.0, 25.0, 3)))
#'     ),
#'     relationships = c(0)
#'   )
#' )
#'
#' # 初始化粒子群（规模为2）
#' particles <- initialize_particles(pop_size = 2, tasks = tasks, resource_mapping = resource_mapping)
#'
#' # 查看第一个粒子的位置
#' str(particles[[1]]$position)
#'
#' @references
#' 1. Kennedy, J., & Eberhart, R. (1995). Particle swarm optimization. Proceedings of ICNN'95 - International Conference on Neural Networks, 4, 1942-1948.
#' 2. Coello, C. A. C., Lechuga, M. S. (2002). MOPSO: A proposal for multiple objective particle swarm optimization. IEEE Transactions on Evolutionary Computation, 8(3), 256-279.
#'
#' @export
initialize_particles <- function(pop_size, tasks, resource_mapping) {
  particles <- list()
  for (i in 1:pop_size) {
    position <- list()
    velocity <- list()
    # 遍历所有任务
    for (task_name in names(tasks)) {
      task <- tasks[[task_name]]
      task_priority <- task$priority
      # 遍历任务的子任务
      for (subtask in task$activities) {
        subtask_name <- subtask[[1]]
        available_res <- names(subtask[[2]])
        
        # 根据优先级筛选资源
        if (task_priority == 3) {
          # 优先级3优先选择外包资源(O开头)
          possible_res <- available_res[grepl("^O", available_res)]
        } else {
          # 其他优先级优先选择自有资源(M开头)
          possible_res <- available_res[grepl("^M", available_res)]
        }
        
        # 如果没有符合条件的资源，则使用所有可用资源
        if (length(possible_res) == 0) {
          possible_res <- available_res
        }
        
        # 随机选择资源
        selected_res <- sample(possible_res, 1)
        position[[subtask_name]] <- selected_res
        velocity[[subtask_name]] <- 0.0  # 初始化速度
      }
    }
    # 构建粒子对象
    particles[[i]] <- list(
      position = position,
      velocity = velocity,
      best_position = position,
      fitness = c(Inf, Inf, Inf, Inf, Inf, Inf),  # 多目标适应度
      best_fitness = c(Inf, Inf, Inf, Inf, Inf, Inf)
    )
  }
  return(particles)
}