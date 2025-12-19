#' 非支配排序（返回Pareto前沿列表，与Python一致）
#'
#' @param particles 列表，粒子群
#' @return 列表，每个元素是前沿的粒子子列表
#' @export
non_dominated_sort_fronts <- function(particles) {
  ranks <- non_dominated_sort(particles)  # 调用C++
  max_rank <- max(ranks)
  fronts <- vector("list", max_rank)
  for (i in 1:max_rank) {
    fronts[[i]] <- particles[ranks == i]
  }
  fronts
}