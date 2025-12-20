## ----load-package, include=FALSE----------------------------------------------
library(BE25204002)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5
)
set.seed(123)

## ----eval = FALSE-------------------------------------------------------------
# load("./data/test_data.RData")
# 
# script_path <- system.file("example", "run_mopso.R", package = "BE25204002")
# 
# source(script_path)
# 

## ----eval = FALSE-------------------------------------------------------------
# 
# # 资源映射
# resource_mapping <- list(M1 = "O1", M2 = "O2", M3 = "O3")
# # 任务数据（3 个任务，混合优先级与依赖）
# tasks <- list(
#   P1 = list(
#     priority = 1,
#     due_date = 20,
#     delay_penalty = 4.5,
#     activities = list(
#       list("P1_1", list(M1 = c(4.5, 12.0, 3), O1 = c(3.0, 18.0, 3))),
#       list("P1_2", list(M2 = c(5.2, 14.5, 4), O2 = c(4.0, 20.0, 4))),
#       list("P1_3", list(M3 = c(3.8, 10.0, 2), O3 = c(2.5, 15.0, 2)))
#     ),
#     relationships = c(0, 0) # 全串行
#   ),
#   P2 = list(
#     priority = 2,
#     due_date = 25,
#     delay_penalty = 6.0,
#     activities = list(
#       list("P2_1", list(M1 = c(6.0, 15.0, 5), O1 = c(5.0, 22.0, 5))),
#       list("P2_2", list(M2 = c(4.0, 13.0, 3), O2 = c(3.5, 19.0, 3))),
#       list("P2_3", list(M3 = c(5.5, 16.0, 4), O3 = c(4.5, 21.0, 4))),
#       list("P2_4", list(M1 = c(3.0, 11.0, 2), O1 = c(2.0, 17.0, 2)))
#     ),
#     relationships = c(1, 0, 0) # 第一、二并行，后续串行
#   ),
#   P3 = list(
#     priority = 3,
#     due_date = 15,
#     delay_penalty = 8.0,
#     activities = list(
#       list("P3_1", list(M1 = c(2.5, 9.0, 2), O1 = c(1.8, 14.0, 2))),
#       list("P3_2", list(M2 = c(3.5, 11.0, 3), O2 = c(2.8, 16.0, 3))),
#       list("P3_3", list(M3 = c(4.0, 12.5, 4), O3 = c(3.2, 18.0, 4)))
#     ),
#     relationships = c(0, 1)
#   )
# )
# # 资源参数
# self_resource <- list(
#   M1 = list(capacity = 5),
#   M2 = list(capacity = 6),
#   M3 = list(capacity = 4)
# )
# outsourced_resource <- list(
#   O1 = list(capacity = 20, cost_ratio = 1.5),
#   O2 = list(capacity = 20, cost_ratio = 1.8),
#   O3 = list(capacity = 20, cost_ratio = 2.0)
# )
# resource_windows <- list(
#   M1 = c(0, 30), M2 = c(2, 32), M3 = c(1, 28),
#   O1 = c(0, 40), O2 = c(0, 40), O3 = c(0, 40)
# )
# resource_joint_cost <- list(
#   "M1,M1" = c(0, 0), "M1,M2" = c(1.5, 3), "M1,M3" = c(2, 4),
#   "M2,M1" = c(1, 2.5), "M2,M2" = c(0, 0), "M2,M3" = c(1.8, 3.5),
#   "M3,M1" = c(2.2, 4.5), "M3,M2" = c(1.2, 2.8), "M3,M3" = c(0, 0),
#   "M1,O1" = c(3, 5), "O1,M1" = c(3, 5)
# )

## ----eval=FALSE---------------------------------------------------------------
# #' 判断向量a是否支配向量b（多目标最小化，越小越优）
# #' @title 多目标支配判断函数
# #' @description 检查解a是否支配解b：a的每个目标值不大于b，且至少有一个目标值严格小于b。
# #' @param a 数值向量，第一个解的适应度向量。
# #' @param b 数值向量，第二个解的适应度向量。
# #' @return 逻辑值，TRUE 表示a支配b，FALSE 表示不支配。
# #' @examples
# #' # 示例1：a支配b（常规场景）
# #' a <- c(2, 3)
# #' b <- c(3, 4)
# #' dominates(a, b) # 返回TRUE
# #'
# #' # 示例2：a和b相等，不支配
# #' a <- c(2, 3)
# #' b <- c(2, 3)
# #' dominates(a, b) # 返回FALSE
# #'
# #' # 示例3：a被b支配，返回FALSE
# #' a <- c(3, 4)
# #' b <- c(2, 3)
# #' dominates(a, b) # 返回FALSE
# #'
# #' # 示例4：三维目标的支配判断
# #' a <- c(1, 2, 3)
# #' b <- c(1, 3, 4)
# #' dominates(a, b) # 返回TRUE（第二个目标更优，其余相等）
# #'
# #' # 示例5：向量长度不同，触发错误（用tryCatch捕获）
# #' tryCatch({
# #'   dominates(c(1,2), c(3))
# #' }, error = function(e) {
# #'   cat("Error:", e$message, "\n")
# #' })
# #' @export
# dominates <- function(a, b) {
#   # 检查两个向量长度是否一致
#   if (length(a) != length(b)) {
#     stop("Fitness vectors must have the same length.")
#   }
# 
#   has_better <- FALSE
#   for (i in seq_along(a)) {
#     if (a[i] > b[i]) {
#       # a has at least one objective worse than b
#       return(FALSE)
#     } else if (a[i] < b[i]) {
#       # a is strictly better in at least one objective
#       has_better <- TRUE
#     }
#   }
# 
#   # a dominates b only if it is no worse in all objectives
#   # and strictly better in at least one objective
#   return(has_better)
# }
# 
# #' 非支配排序（多目标优化）
# #' @title 非支配排序函数
# #' @description 对粒子群进行非支配排序，返回每个粒子的前沿等级（rank），等级1为最优前沿。
# #' @param particles 列表，每个元素是包含"fitness"数值向量的粒子（如 list(fitness = c(1,2), position = ...)）。
# #' @return 整数向量，每个元素对应粒子的前沿等级（rank）。
# #' @examples
# #' # 示例1：常规场景（6个粒子的非支配排序）
# #' set.seed(123) # 保证可复现
# #' particles <- list(
# #'   list(fitness = c(2, 3), position = c(0.1, 0.2)),
# #'   list(fitness = c(1, 2), position = c(0.3, 0.4)),
# #'   list(fitness = c(3, 1), position = c(0.5, 0.6)),
# #'   list(fitness = c(4, 5), position = c(0.7, 0.8)),
# #'   list(fitness = c(2, 2), position = c(0.9, 0.1)),
# #'   list(fitness = c(5, 6), position = c(0.2, 0.3))
# #' )
# #' # 计算非支配排序等级
# #' ranks <- non_dominated_sort(particles)
# #' # 查看每个粒子的等级
# #' data.frame(Particle = 1:6, Rank = ranks, Fitness = sapply(particles, function(x) paste(x$fitness, collapse = ",")))
# #'
# #' # 示例2：边界场景1（所有粒子无支配关系，同属等级1）
# #' particles2 <- list(
# #'   list(fitness = c(1, 4)),
# #'   list(fitness = c(2, 3)),
# #'   list(fitness = c(3, 2)),
# #'   list(fitness = c(4, 1))
# #' )
# #' ranks2 <- non_dominated_sort(particles2)
# #' table(ranks2) # 所有粒子等级为1
# #'
# #' # 示例3：边界场景2（只有1个粒子）
# #' particles3 <- list(list(fitness = c(1, 2)))
# #' ranks3 <- non_dominated_sort(particles3)
# #' ranks3 # 等级为1
# #'
# #' # 示例4：三维目标的非支配排序
# #' particles4 <- list(
# #'   list(fitness = c(1, 2, 3)),
# #'   list(fitness = c(2, 2, 2)),
# #'   list(fitness = c(1, 3, 4)),
# #'   list(fitness = c(3, 1, 2))
# #' )
# #' ranks4 <- non_dominated_sort(particles4)
# #' ranks4
# #' @export
# non_dominated_sort <- function(particles) {
#   # 输入校验：检查每个粒子是否包含fitness元素
#   if (!all(sapply(particles, function(x) "fitness" %in% names(x)))) {
#     stop("每个粒子必须包含'fitness'元素")
#   }
#   # 检查所有fitness向量长度一致
#   fitness_lengths <- sapply(particles, function(x) length(x$fitness))
#   if (length(unique(fitness_lengths)) > 1) {
#     stop("所有粒子的'fitness'向量长度必须一致")
#   }
# 
#   # 获取粒子数量
#   n <- length(particles)
#   if (n == 0) {
#     stop("Particle set must not be empty.")
#   }
# 
#   # 初始化支配集合：dom_set[[i]] 存储被粒子i支配的粒子索引
#   dom_set <- vector("list", n)
#   # 初始化被支配计数：dom_count[i] 表示粒子i被多少个粒子支配
#   dom_count <- integer(n)
# 
#   # 计算所有粒子间的支配关系
#   for (i in seq_len(n)) {
#     # 获取粒子i的适应度向量
#     f1 <- particles[[i]][["fitness"]]
#     for (j in seq_len(n)) {
#       if (i == j) next  # 跳过自身比较
#       # 获取粒子j的适应度向量
#       f2 <- particles[[j]][["fitness"]]
# 
#       # 判断i是否支配j
#       if (dominates(f1, f2)) {
#         dom_set[[i]] <- c(dom_set[[i]], j)
#       } else if (dominates(f2, f1)) {
#         # j dominates i
#         dom_count[i] <- dom_count[i] + 1
#       }
#     }
#   }
# 
#   # 初始化前沿等级向量（初始值为1）
#   rank <- integer(n)
#   rank[] <- 1
# 
#   # 找出初始前沿（被支配计数为0的粒子）
#   current_front <- which(dom_count == 0)
# 
#   # 迭代计算后续前沿
#   front <- 1
#   while (length(current_front) > 0) {
#     next_front <- integer(0)
# 
#     # 遍历当前前沿的每个粒子
#     for (i in current_front) {
#       # 遍历被当前粒子支配的粒子
#       for (j in dom_set[[i]]) {
#         dom_count[j] <- dom_count[j] - 1
#         if (dom_count[j] == 0) {
#           rank[j] <- front + 1
#           next_front <- c(next_front, j)
#         }
#       }
#     }
# 
#     front <- front + 1
#     current_front <- next_front
#   }
# 
#   return(rank)
# }

## ----include=TRUE-------------------------------------------------------------
a <- c(10, 5)
b <- c(12, 5)
dominates(a, b)

## ----eval=FALSE---------------------------------------------------------------
# #' 非支配排序（返回Pareto前沿列表，与Python一致）
# #'
# #' @title 非支配排序（返回前沿列表）
# #' @description 对粒子群进行非支配排序，返回按等级划分的Pareto前沿列表，每个元素为对应等级的粒子子列表（等级1为最优前沿）。
# #' @param particles 列表，每个元素是包含"fitness"数值向量的粒子（如 list(fitness = c(1,2), position = ...)）。
# #' @return 列表，每个元素是对应等级的粒子子列表，列表长度为前沿的最大等级数。
# #' @examples
# #' # 加载依赖函数（实际包中需确保dominates和non_dominated_sort已导出）
# #' dominates <- function(a, b) {
# #'   if (length(a) != length(b)) stop("Fitness vectors must have the same length.")
# #'   has_better <- FALSE
# #'   for (i in seq_along(a)) {
# #'     if (a[i] > b[i]) return(FALSE)
# #'     else if (a[i] < b[i]) has_better <- TRUE
# #'   }
# #'   return(has_better)
# #' }
# #'
# #' non_dominated_sort <- function(particles) {
# #'   if (!all(sapply(particles, function(x) "fitness" %in% names(x)))) {
# #'     stop("每个粒子必须包含'fitness'元素")
# #'   }
# #'   fitness_lengths <- sapply(particles, function(x) length(x$fitness))
# #'   if (length(unique(fitness_lengths)) > 1) {
# #'     stop("所有粒子的'fitness'向量长度必须一致")
# #'   }
# #'   n <- length(particles)
# #'   if (n == 0) stop("Particle set must not be empty.")
# #'
# #'   dom_set <- vector("list", n)
# #'   dom_count <- integer(n)
# #'
# #'   for (i in seq_len(n)) {
# #'     f1 <- particles[[i]][["fitness"]]
# #'     for (j in seq_len(n)) {
# #'       if (i == j) next
# #'       f2 <- particles[[j]][["fitness"]]
# #'       if (dominates(f1, f2)) {
# #'         dom_set[[i]] <- c(dom_set[[i]], j)
# #'       } else if (dominates(f2, f1)) {
# #'         dom_count[i] <- dom_count[i] + 1
# #'       }
# #'     }
# #'   }
# #'
# #'   rank <- integer(n)
# #'   rank[] <- 1
# #'   current_front <- which(dom_count == 0)
# #'   front <- 1
# #'
# #'   while (length(current_front) > 0) {
# #'     next_front <- integer(0)
# #'     for (i in current_front) {
# #'       for (j in dom_set[[i]]) {
# #'         dom_count[j] <- dom_count[j] - 1
# #'         if (dom_count[j] == 0) {
# #'           rank[j] <- front + 1
# #'           next_front <- c(next_front, j)
# #'         }
# #'       }
# #'     }
# #'     front <- front + 1
# #'     current_front <- next_front
# #'   }
# #'   return(rank)
# #' }
# #'
# #' # 示例1：常规场景（多层前沿，6个粒子）
# #' set.seed(123)
# #' particles <- list(
# #'   list(fitness = c(2, 3), position = c(0.1, 0.2)),  # 等级2
# #'   list(fitness = c(1, 2), position = c(0.3, 0.4)),  # 等级1
# #'   list(fitness = c(3, 1), position = c(0.5, 0.6)),  # 等级1
# #'   list(fitness = c(4, 5), position = c(0.7, 0.8)),  # 等级3
# #'   list(fitness = c(2, 2), position = c(0.9, 0.1)),  # 等级1
# #'   list(fitness = c(5, 6), position = c(0.2, 0.3))   # 等级4
# #' )
# #'
# #' # 计算Pareto前沿列表
# #' fronts <- non_dominated_sort_fronts(particles)
# #'
# #' # 查看各前沿的粒子数量
# #' sapply(fronts, length)  # 输出：3 1 1 1（等级1有3个粒子，等级2-4各1个）
# #'
# #' # 查看等级1前沿的粒子适应度
# #' lapply(fronts[[1]], function(p) p$fitness)
# #'
# #' # 示例2：边界场景1（单个粒子）
# #' particles_single <- list(list(fitness = c(1, 2), position = c(0.1, 0.2)))
# #' fronts_single <- non_dominated_sort_fronts(particles_single)
# #' length(fronts_single)  # 输出：1（仅1个前沿）
# #' sapply(fronts_single, length)  # 输出：1（前沿1有1个粒子）
# #'
# #' # 示例3：边界场景2（所有粒子无支配关系，单前沿）
# #' particles_no_dom <- list(
# #'   list(fitness = c(1, 4)),
# #'   list(fitness = c(2, 3)),
# #'   list(fitness = c(3, 2)),
# #'   list(fitness = c(4, 1))
# #' )
# #' fronts_no_dom <- non_dominated_sort_fronts(particles_no_dom)
# #' length(fronts_no_dom)  # 输出：1（所有粒子同属等级1）
# #' sapply(fronts_no_dom, length)  # 输出：4（前沿1有4个粒子）
# #'
# #' # 示例4：三维目标的非支配排序（多层前沿）
# #' particles_3d <- list(
# #'   list(fitness = c(1, 2, 3)),  # 等级1
# #'   list(fitness = c(2, 2, 2)),  # 等级1
# #'   list(fitness = c(1, 3, 4)),  # 等级2
# #'   list(fitness = c(3, 1, 2)),  # 等级1
# #'   list(fitness = c(4, 5, 6))   # 等级3
# #' )
# #' fronts_3d <- non_dominated_sort_fronts(particles_3d)
# #' sapply(fronts_3d, length)  # 输出：3 1 1（等级1有3个，等级2-3各1个）
# #' @export
# non_dominated_sort_fronts <- function(particles) {
#   ranks <- non_dominated_sort(particles)  # 调用非支配排序函数获取等级
#   max_rank <- max(ranks)
#   fronts <- vector("list", max_rank)
#   for (i in 1:max_rank) {
#     fronts[[i]] <- particles[ranks == i]
#   }
#   fronts
# }

## ----include=TRUE-------------------------------------------------------------
# particles 是粒子列表
particles <- list(
  list(fitness = c(10, 5)),
  list(fitness = c(12, 6)),
  list(fitness = c(9,  7))
)

ranks <- non_dominated_sort(particles)
ranks

## ----include=TRUE-------------------------------------------------------------
# 构造一个最小粒子群（仅包含 fitness）
particles <- list(
  list(fitness = c(10, 5)),  # 解 1
  list(fitness = c(12, 6)),  # 解 2（被解1支配）
  list(fitness = c(9,  7)),  # 解 3（与解1互不支配）
  list(fitness = c(11, 4))   # 解 4（与解1互不支配）
)

# 非支配排序，得到 Pareto 前沿
fronts <- non_dominated_sort_fronts(particles)

# 前沿数量
length(fronts)

# 第一 Pareto 前沿上的解数量
length(fronts[[1]])

# 查看第一前沿中各解的 fitness
lapply(fronts[[1]], function(p) p$fitness)


## ----eval=FALSE---------------------------------------------------------------
# #' 计算前沿的拥挤距离
# #'
# #' 该函数针对多目标优化中的前沿粒子列表，计算每个粒子的拥挤距离（Crowding Distance），
# #' 用于衡量粒子在前沿中的分散程度。计算时基于前3个目标（cost、time、variance，对应fitness向量的1、2、3索引）进行排序，
# #' 边界粒子的拥挤距离设为无穷大（Inf），内部粒子的距离为相邻粒子在各目标上的差值归一化之和。
# #'
# #' @param front 列表，前沿粒子的子列表。每个子列表必须包含`fitness`元素（数值向量，至少3个元素，
# #' 分别对应cost、time、variance）。
# #' @return 列表，与输入结构一致，但每个粒子子列表新增`crowding_distance`字段，存储计算得到的拥挤距离。
# #' @examples
# #' # 示例1：常规场景（5个粒子的前沿）
# #' # 构造测试前沿粒子列表
# #' set.seed(123) # 设定随机种子保证可复现
# #' front <- list(
# #'   list(fitness = c(2.5, 5.2, 1.8)),
# #'   list(fitness = c(1.2, 7.1, 2.3)),
# #'   list(fitness = c(3.7, 4.0, 1.5)),
# #'   list(fitness = c(0.8, 8.5, 2.9)),
# #'   list(fitness = c(4.1, 3.5, 1.2))
# #' )
# #'
# #' # 计算拥挤距离
# #' front_with_cd <- calculate_crowding_distance(front)
# #'
# #' # 查看每个粒子的拥挤距离
# #' sapply(front_with_cd, function(x) x$crowding_distance)
# #'
# #' # 示例2：边界场景1（前沿只有1个粒子）
# #' front_single <- list(list(fitness = c(1.0, 2.0, 3.0)))
# #' front_single_cd <- calculate_crowding_distance(front_single)
# #' front_single_cd[[1]]$crowding_distance # 结果为0（因无相邻粒子）
# #'
# #' # 示例3：边界场景2（前沿只有2个粒子）
# #' front_two <- list(
# #'   list(fitness = c(1.0, 2.0, 3.0)),
# #'   list(fitness = c(4.0, 5.0, 6.0))
# #' )
# #' front_two_cd <- calculate_crowding_distance(front_two)
# #' sapply(front_two_cd, function(x) x$crowding_distance) # 两个粒子均为Inf
# #'
# #' @export
# calculate_crowding_distance <- function(front) {
#   if (length(front) == 0) return(front)
#   n <- length(front)
#   for (i in seq_along(front)) {
#     front[[i]]$crowding_distance <- 0.0
#   }
#   objectives <- list(c(1, "cost"), c(2, "time"), c(3, "variance"))  # 前3个目标用于排序
#   for (obj in objectives) {
#     obj_idx <- obj[[1]]
#     # 按当前目标排序前沿
#     sorted_front <- front[order(sapply(front, function(x) x$fitness[obj_idx]))]
#     # 边界粒子设为无穷大
#     if (n >= 1) {
#       sorted_front[[1]]$crowding_distance <- Inf
#       if (n >= 2) {
#         sorted_front[[n]]$crowding_distance <- Inf
#       }
#     }
#     # 计算目标的范围
#     obj_min <- sorted_front[[1]]$fitness[obj_idx]
#     obj_max <- sorted_front[[n]]$fitness[obj_idx]
#     obj_range <- ifelse(obj_max == obj_min, 1.0, obj_max - obj_min)
#     # 计算内部粒子的拥挤距离
#     if (n > 2) {
#       for (i in 2:(n-1)) {
#         sorted_front[[i]]$crowding_distance <- sorted_front[[i]]$crowding_distance +
#           (sorted_front[[i+1]]$fitness[obj_idx] - sorted_front[[i-1]]$fitness[obj_idx]) / obj_range
#       }
#     }
#     # 恢复原顺序
#     front <- sorted_front[order(match(sorted_front, front))]
#   }
#   front
# }

## ----include=TRUE-------------------------------------------------------------
# 构造一个至少包含 3 个解的 Pareto 前沿
front <- list(
  list(fitness = c(10, 5)),
  list(fitness = c(9,  7)),
  list(fitness = c(11, 4)),
  list(fitness = c(8,  6))
)

# 计算拥挤度距离
cd <- calculate_crowding_distance(front)

cd


## ----fig.cap="图片描述1", out.width="50%", fig.align="center"---------------------
knitr::include_graphics("../inst/example/AAS-CN-2019-0044-5.jpg")

## ----eval=FALSE---------------------------------------------------------------
# #' 更新粒子群的速度和位置（PSO evolution core）
# #'
# #' @title Particle update function for MOPSO
# #' @description
# #' Update particle velocity and position based on personal best and
# #' global best (sampled from external archive), then re-decode the particle
# #' to obtain updated fitness and schedule, and update personal best.
# #'
# #' @param particles List. Particle swarm; each particle contains
# #'   \code{position}, \code{velocity}, \code{best_position},
# #'   \code{best_fitness}, \code{fitness}, and \code{schedule}.
# #' @param external_archive List. External archive of non-dominated solutions.
# #' @param gen Numeric. Current generation index.
# #' @param max_gen Numeric. Maximum number of generations.
# #' @param tasks List. Task definitions including priority, deadlines, activities, and precedence.
# #' @param resource_mapping List. Mapping between in-house and outsourced resources.
# #' @param self_resource List. In-house resource definitions.
# #' @param outsourced_resource List. Outsourced resource definitions.
# #' @param resource_windows List. Resource availability time windows.
# #' @param resource_joint_cost List. Joint cost between resources.
# #'
# #' @return List. Updated particle swarm.
# #'
# #' @importFrom stats runif
# #' @export
# update_particles <- function(particles, external_archive, gen, max_gen, tasks, resource_mapping,
#                              self_resource, outsourced_resource, resource_windows, resource_joint_cost) {
#   # 初始化PSO参数
#   w_max <- 0.9  # 最大惯性权重
#   w_min <- 0.4  # 最小惯性权重
#   w <- w_max - (w_max - w_min) * (gen / max_gen)  # 动态惯性权重（线性递减）
#   c1 <- 2.0     # 个体学习因子
#   c2 <- 1.0     # 社会学习因子
#   random_prob <- 0.1  # 随机切换资源的概率
# 
#   # 遍历每个粒子进行更新
#   for (i in seq_along(particles)) {
#     particle <- particles[[i]]
# 
#     # 从外部存档中采样全局最优粒子（若无存档则使用当前粒子）
#     if (length(external_archive) > 0) {
#       gbest <- sample(external_archive, 1)[[1]]
#     } else {
#       gbest <- particle
#     }
# 
#     # 遍历粒子的每个子任务位置（资源分配）进行更新
#     for (subtask_name in names(particle$position)) {
#       # 获取当前资源、个体最优资源、全局最优资源
#       current_res <- particle$position[[subtask_name]]
#       pbest_res <- particle$best_position[[subtask_name]]
#       gbest_res <- gbest$position[[subtask_name]]
# 
#       # 计算与个体最优、全局最优的资源差异（0为相同，1为不同）
#       d_pbest <- ifelse(pbest_res != current_res, 1, 0)
#       d_gbest <- ifelse(gbest_res != current_res, 1, 0)
# 
#       # 计算速度（离散型PSO速度，代表资源切换的倾向）
#       v_i <- w * particle$velocity[[subtask_name]] +
#         c1 * runif(1) * d_pbest +
#         c2 * runif(1) * d_gbest
# 
#       # 速度裁剪（限制在[-5,5]范围内，避免速度过大）
#       particle$velocity[[subtask_name]] <- max(min(v_i, 5.0), -5.0)
# 
#       # 获取当前子任务的可用资源
#       # 提取任务名（如从"P1_1"中提取"P1"）
#       task_name <- sub("_[0-9]+$", "", subtask_name)
#       task <- tasks[[task_name]]
#       if (is.null(task)) {
#         warning(paste("Task", task_name, "not found. Skipping this subtask."))
#         next
#       }
# 
#       # 查找子任务的详细信息（增加容错，避免匹配失败）
#       subtask_idx <- which(sapply(task$activities, function(x) x[[1]] == subtask_name))
#       if (length(subtask_idx) == 0) {
#         warning(paste("Subtask", subtask_name, "not found. Skipping this subtask."))
#         next
#       }
# 
#       subtask_info <- task$activities[[subtask_idx[1]]]
#       available_res <- names(subtask_info[[2]])
# 
#       # 根据任务优先级筛选可用资源（优先级3优先外包，否则优先自有）
#       priority <- task$priority
#       if (priority == 3) {
#         possible_res <- available_res[grepl("^O", available_res)]
#       } else {
#         possible_res <- available_res[grepl("^M", available_res)]
#       }
# 
#       # 容错：若筛选后无可用资源，则使用所有可用资源
#       if (length(possible_res) == 0) {
#         possible_res <- available_res
#       }
# 
#       # 更新粒子位置（资源分配）
#       if (runif(1) < random_prob) {
#         # 随机选择资源（增加多样性）
#         new_res <- sample(possible_res, 1)
#       } else {
#         # 根据速度计算资源切换概率
#         p_switch <- 1 / (1 + exp(-v_i))
#         if (runif(1) < p_switch) {
#           # 优先选择个体最优，其次全局最优，最后随机
#           if (pbest_res %in% possible_res) {
#             new_res <- pbest_res
#           } else if (gbest_res %in% possible_res) {
#             new_res <- gbest_res
#           } else {
#             new_res <- sample(possible_res, 1)
#           }
#         } else {
#           # 保持当前资源
#           new_res <- current_res
#         }
#       }
# 
#       # 赋值新资源到粒子位置
#       particle$position[[subtask_name]] <- new_res
#     }
# 
#     # 解码新位置，计算适应度和调度方案
#     decode_result <- decode_schedule(
#       particle = particle,
#       tasks = tasks,
#       self_resource = self_resource,
#       outsourced_resource = outsourced_resource,
#       resource_windows = resource_windows,
#       resource_joint_cost = resource_joint_cost,
#       resource_mapping = resource_mapping
#     )
# 
#     # 更新粒子的适应度和调度方案
#     particle$fitness <- decode_result$fitness
#     particle$schedule <- decode_result$schedule
# 
#     # 更新个体最优（若当前适应度支配个体最优，或个体最优为无穷大）
#     if (all(is.infinite(particle$best_fitness)) ||
#         dominates(particle$fitness, particle$best_fitness)) {
#       particle$best_position <- particle$position
#       particle$best_fitness <- particle$fitness
#     }
# 
#     # 将更新后的粒子放回粒子群
#     particles[[i]] <- particle
#   }
# 
#   # 返回更新后的粒子群
#   return(particles)
# }
# 

## ----eval=FALSE---------------------------------------------------------------
# #' 管理外部存档（限制大小，使用拥挤距离）
# #'
# #' @param archive 列表，外部存档粒子
# #' @param max_size 正整数，最大存档大小
# #' @return 列表，截取后的存档
# #' @export
# manage_archive <- function(archive, max_size) {
#   if (length(archive) <= max_size) return(archive)
#   archive_with_dist <- calculate_crowding_distance(archive)
#   sorted_archive <- archive_with_dist[order(sapply(archive_with_dist, function(x) x$crowding_distance), decreasing = TRUE)]
#   sorted_archive[1:max_size]
# }

## ----eval = FALSE-------------------------------------------------------------
# pareto_front <- mopsso(
#   tasks = tasks,
#   self_resource = self_resource,
#   outsourced_resource = outsourced_resource,
#   resource_windows = resource_windows,
#   resource_joint_cost = resource_joint_cost,
#   resource_mapping = resource_mapping,
#   pop_size = 100,
#   max_gen = 200,
#   max_archive_size = 50
# )

## ----eval = FALSE-------------------------------------------------------------
# plot_gantt_chart(
#   schedule = pareto_front[[1]]$schedule,
#   title = "Pareto 解 1",
#   filename = "gantt_solution_1.png"
# )

## ----fig.cap="图片描述", out.width="50%", fig.align="center"----------------------
knitr::include_graphics("../inst/example/gantt_solution_1.png")

