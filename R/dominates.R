#' 判断向量a是否支配向量b（多目标最小化，越小越优）
#' @title 多目标支配判断函数
#' @description 检查解a是否支配解b：a的每个目标值不大于b，且至少有一个目标值严格小于b。
#' @param a 数值向量，第一个解的适应度向量。
#' @param b 数值向量，第二个解的适应度向量。
#' @return 逻辑值，TRUE 表示a支配b，FALSE 表示不支配。
#' @examples
#' # 示例1：a支配b（常规场景）
#' a <- c(2, 3)
#' b <- c(3, 4)
#' dominates(a, b) # 返回TRUE
#'
#' # 示例2：a和b相等，不支配
#' a <- c(2, 3)
#' b <- c(2, 3)
#' dominates(a, b) # 返回FALSE
#'
#' # 示例3：a被b支配，返回FALSE
#' a <- c(3, 4)
#' b <- c(2, 3)
#' dominates(a, b) # 返回FALSE
#'
#' # 示例4：三维目标的支配判断
#' a <- c(1, 2, 3)
#' b <- c(1, 3, 4)
#' dominates(a, b) # 返回TRUE（第二个目标更优，其余相等）
#'
#' # 示例5：向量长度不同，触发错误（用tryCatch捕获）
#' tryCatch({
#'   dominates(c(1,2), c(3))
#' }, error = function(e) {
#'   cat("Error:", e$message, "\n")
#' })
#' @export
dominates <- function(a, b) {
  # 检查两个向量长度是否一致
  if (length(a) != length(b)) {
    stop("Fitness vectors must have the same length.")
  }
  
  has_better <- FALSE
  for (i in seq_along(a)) {
    if (a[i] > b[i]) {
      # a has at least one objective worse than b
      return(FALSE)
    } else if (a[i] < b[i]) {
      # a is strictly better in at least one objective
      has_better <- TRUE
    }
  }
  
  # a dominates b only if it is no worse in all objectives
  # and strictly better in at least one objective
  return(has_better)
}

#' 非支配排序（多目标优化）
#' @title 非支配排序函数
#' @description 对粒子群进行非支配排序，返回每个粒子的前沿等级（rank），等级1为最优前沿。
#' @param particles 列表，每个元素是包含"fitness"数值向量的粒子（如 list(fitness = c(1,2), position = ...)）。
#' @return 整数向量，每个元素对应粒子的前沿等级（rank）。
#' @examples
#' # 示例1：常规场景（6个粒子的非支配排序）
#' set.seed(123) # 保证可复现
#' particles <- list(
#'   list(fitness = c(2, 3), position = c(0.1, 0.2)),
#'   list(fitness = c(1, 2), position = c(0.3, 0.4)),
#'   list(fitness = c(3, 1), position = c(0.5, 0.6)),
#'   list(fitness = c(4, 5), position = c(0.7, 0.8)),
#'   list(fitness = c(2, 2), position = c(0.9, 0.1)),
#'   list(fitness = c(5, 6), position = c(0.2, 0.3))
#' )
#' # 计算非支配排序等级
#' ranks <- non_dominated_sort(particles)
#' # 查看每个粒子的等级
#' data.frame(Particle = 1:6, Rank = ranks, Fitness = sapply(particles, function(x) paste(x$fitness, collapse = ",")))
#'
#' # 示例2：边界场景1（所有粒子无支配关系，同属等级1）
#' particles2 <- list(
#'   list(fitness = c(1, 4)),
#'   list(fitness = c(2, 3)),
#'   list(fitness = c(3, 2)),
#'   list(fitness = c(4, 1))
#' )
#' ranks2 <- non_dominated_sort(particles2)
#' table(ranks2) # 所有粒子等级为1
#'
#' # 示例3：边界场景2（只有1个粒子）
#' particles3 <- list(list(fitness = c(1, 2)))
#' ranks3 <- non_dominated_sort(particles3)
#' ranks3 # 等级为1
#'
#' # 示例4：三维目标的非支配排序
#' particles4 <- list(
#'   list(fitness = c(1, 2, 3)),
#'   list(fitness = c(2, 2, 2)),
#'   list(fitness = c(1, 3, 4)),
#'   list(fitness = c(3, 1, 2))
#' )
#' ranks4 <- non_dominated_sort(particles4)
#' ranks4
#' @export
non_dominated_sort <- function(particles) {
  # 输入校验：检查每个粒子是否包含fitness元素
  if (!all(sapply(particles, function(x) "fitness" %in% names(x)))) {
    stop("每个粒子必须包含'fitness'元素")
  }
  # 检查所有fitness向量长度一致
  fitness_lengths <- sapply(particles, function(x) length(x$fitness))
  if (length(unique(fitness_lengths)) > 1) {
    stop("所有粒子的'fitness'向量长度必须一致")
  }
  
  # 获取粒子数量
  n <- length(particles)
  if (n == 0) {
    stop("Particle set must not be empty.")
  }
  
  # 初始化支配集合：dom_set[[i]] 存储被粒子i支配的粒子索引
  dom_set <- vector("list", n)
  # 初始化被支配计数：dom_count[i] 表示粒子i被多少个粒子支配
  dom_count <- integer(n)
  
  # 计算所有粒子间的支配关系
  for (i in seq_len(n)) {
    # 获取粒子i的适应度向量
    f1 <- particles[[i]][["fitness"]]
    for (j in seq_len(n)) {
      if (i == j) next  # 跳过自身比较
      # 获取粒子j的适应度向量
      f2 <- particles[[j]][["fitness"]]
      
      # 判断i是否支配j
      if (dominates(f1, f2)) {
        dom_set[[i]] <- c(dom_set[[i]], j)
      } else if (dominates(f2, f1)) {
        # j dominates i
        dom_count[i] <- dom_count[i] + 1
      }
    }
  }
  
  # 初始化前沿等级向量（初始值为1）
  rank <- integer(n)
  rank[] <- 1
  
  # 找出初始前沿（被支配计数为0的粒子）
  current_front <- which(dom_count == 0)
  
  # 迭代计算后续前沿
  front <- 1
  while (length(current_front) > 0) {
    next_front <- integer(0)
    
    # 遍历当前前沿的每个粒子
    for (i in current_front) {
      # 遍历被当前粒子支配的粒子
      for (j in dom_set[[i]]) {
        dom_count[j] <- dom_count[j] - 1
        if (dom_count[j] == 0) {
          rank[j] <- front + 1
          next_front <- c(next_front, j)
        }
      }
    }
    
    front <- front + 1
    current_front <- next_front
  }
  
  return(rank)
}