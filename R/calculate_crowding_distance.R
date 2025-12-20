#' 计算前沿的拥挤距离
#'
#' 该函数针对多目标优化中的前沿粒子列表，计算每个粒子的拥挤距离（Crowding Distance），
#' 用于衡量粒子在前沿中的分散程度。计算时基于前3个目标（cost、time、variance，对应fitness向量的1、2、3索引）进行排序，
#' 边界粒子的拥挤距离设为无穷大（Inf），内部粒子的距离为相邻粒子在各目标上的差值归一化之和。
#'
#' @param front 列表，前沿粒子的子列表。每个子列表必须包含`fitness`元素（数值向量，至少3个元素，
#' 分别对应cost、time、variance）。
#' @return 列表，与输入结构一致，但每个粒子子列表新增`crowding_distance`字段，存储计算得到的拥挤距离。
#' @examples
#' # 示例1：常规场景（5个粒子的前沿）
#' # 构造测试前沿粒子列表
#' set.seed(123) # 设定随机种子保证可复现
#' front <- list(
#'   list(fitness = c(2.5, 5.2, 1.8)),
#'   list(fitness = c(1.2, 7.1, 2.3)),
#'   list(fitness = c(3.7, 4.0, 1.5)),
#'   list(fitness = c(0.8, 8.5, 2.9)),
#'   list(fitness = c(4.1, 3.5, 1.2))
#' )
#'
#' # 计算拥挤距离
#' front_with_cd <- calculate_crowding_distance(front)
#'
#' # 查看每个粒子的拥挤距离
#' sapply(front_with_cd, function(x) x$crowding_distance)
#'
#' # 示例2：边界场景1（前沿只有1个粒子）
#' front_single <- list(list(fitness = c(1.0, 2.0, 3.0)))
#' front_single_cd <- calculate_crowding_distance(front_single)
#' front_single_cd[[1]]$crowding_distance # 结果为0（因无相邻粒子）
#'
#' # 示例3：边界场景2（前沿只有2个粒子）
#' front_two <- list(
#'   list(fitness = c(1.0, 2.0, 3.0)),
#'   list(fitness = c(4.0, 5.0, 6.0))
#' )
#' front_two_cd <- calculate_crowding_distance(front_two)
#' sapply(front_two_cd, function(x) x$crowding_distance) # 两个粒子均为Inf
#'
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
    # 按当前目标排序前沿
    sorted_front <- front[order(sapply(front, function(x) x$fitness[obj_idx]))]
    # 边界粒子设为无穷大
    if (n >= 1) {
      sorted_front[[1]]$crowding_distance <- Inf
      if (n >= 2) {
        sorted_front[[n]]$crowding_distance <- Inf
      }
    }
    # 计算目标的范围
    obj_min <- sorted_front[[1]]$fitness[obj_idx]
    obj_max <- sorted_front[[n]]$fitness[obj_idx]
    obj_range <- ifelse(obj_max == obj_min, 1.0, obj_max - obj_min)
    # 计算内部粒子的拥挤距离
    if (n > 2) {
      for (i in 2:(n-1)) {
        sorted_front[[i]]$crowding_distance <- sorted_front[[i]]$crowding_distance +
          (sorted_front[[i+1]]$fitness[obj_idx] - sorted_front[[i-1]]$fitness[obj_idx]) / obj_range
      }
    }
    # 恢复原顺序
    front <- sorted_front[order(match(sorted_front, front))]
  }
  front
}