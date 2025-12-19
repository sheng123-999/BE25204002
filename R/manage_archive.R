#' 管理外部存档（限制大小，使用拥挤距离）
#'
#' @param archive 列表，外部存档粒子
#' @param max_size 正整数，最大存档大小
#' @return 列表，截取后的存档
#' @export
manage_archive <- function(archive, max_size) {
  if (length(archive) <= max_size) return(archive)
  archive_with_dist <- calculate_crowding_distance(archive)
  sorted_archive <- archive_with_dist[order(sapply(archive_with_dist, function(x) x$crowding_distance), decreasing = TRUE)]
  sorted_archive[1:max_size]
}