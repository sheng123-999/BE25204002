#' 计算前沿的拥挤距离
#'
#' @param front 列表，前沿粒子子列表
#' @return 列表，已添加"crowding_distance"的前沿粒子
#' @export
calculate_crowding_distance <- function(front) {
  if (length(front) == 0) return(front)
  n <- length(front)
  for (i in seq_along(front)) {
    front[[i]]$crowding_distance <- 0.0
  }
  objectives <- list(c(1, "cost"), c(2, "time"), c(3, "variance"))  # 前3个目标用于排序
  for (obj in objectives) {
    obj_idx <- obj[[1]]
    sorted_front <- front[order(sapply(front, function(x) x$fitness[obj_idx]))]
    sorted_front[[1]]$crowding_distance <- Inf
    sorted_front[[n]]$crowding_distance <- Inf
    obj_min <- sorted_front[[1]]$fitness[obj_idx]
    obj_max <- sorted_front[[n]]$fitness[obj_idx]
    obj_range <- ifelse(obj_max == obj_min, 1.0, obj_max - obj_min)
    for (i in 2:(n-1)) {
      sorted_front[[i]]$crowding_distance <- sorted_front[[i]]$crowding_distance +
        (sorted_front[[i+1]]$fitness[obj_idx] - sorted_front[[i-1]]$fitness[obj_idx]) / obj_range
    }
    # 恢复原顺序（可选，但为一致性）
    front <- sorted_front[order(match(sorted_front, front))]  # 简化为原front更新
  }
  front
}