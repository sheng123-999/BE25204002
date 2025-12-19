#' 判断向量a是否支配向量b（多目标最小化，越小越优）
#' @title 多目标支配判断函数
#' @description 检查解a是否支配解b：a的每个目标值不大于b，且至少有一个目标值严格小于b。
#' @param a 数值向量，第一个解的适应度向量。
#' @param b 数值向量，第二个解的适应度向量。
#' @return 逻辑值，TRUE 表示a支配b，FALSE 表示不支配。
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
#' @export
non_dominated_sort <- function(particles) {
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
