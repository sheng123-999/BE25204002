# run_mopsso.R —— 一键运行整个实验

cat("=== 加载测试数据 ===\n")
data("test_data", package = "BE25204002")

cat("=== 加载函数 ===\n")
library(BE25204002)


cat("=== 运行 MOPSO ===\n")
set.seed(123)
pareto_front <- mopsso(
  tasks = tasks,
  self_resource = self_resource,
  outsourced_resource = outsourced_resource,
  resource_windows = resource_windows,
  resource_joint_cost = resource_joint_cost,
  resource_mapping = resource_mapping,
  pop_size = 80,
  max_gen = 20,
  max_archive_size = 30
)

cat("\n找到 Pareto 前沿解数量：", length(pareto_front), "\n")

# 交互查看
while (TRUE) {
  cat("\n请输入解编号 (1~", length(pareto_front), ") 或 q 退出：")
  input <- readline()
  if (toupper(input) == "Q") break
  idx <- as.integer(input)
  if (is.na(idx) || idx < 1 || idx > length(pareto_front)) {
    cat("无效输入\n")
    next
  }
  selected <- pareto_front[[idx]]
  cat("\n--- 第", idx, "个解 ---\n")
  print(selected$fitness)
  plot_gantt_chart(selected$schedule,
                   title = paste("Pareto 解", idx),
                   filename = paste0("gantt_solution_", idx, ".png"))
}

