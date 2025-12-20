## ----echo=FALSE---------------------------------------------------------------
set.seed(42)

cluster1 <- data.frame(
  x = rnorm(100, mean = 1, sd = 1),
  y = rnorm(100, mean = 1, sd = 1),
  z = rnorm(100, mean = 1, sd = 1)
)
cluster2 <- data.frame(
  x = rnorm(100, mean = 2, sd = 1),
  y = rnorm(100, mean = 5, sd = 1),
  z = rnorm(100, mean = 8, sd = 1)
)
cluster3 <- data.frame(
  x = rnorm(100, mean = 6, sd = 1),
  y = rnorm(100, mean = 7, sd = 1),
  z = rnorm(100, mean = 3, sd = 1)
)
cluster4 <- data.frame(  # 已定义的第4个簇
  x = rnorm(100, mean = 5, sd = 1),
  y = rnorm(100, mean = 1, sd = 1),
  z = rnorm(100, mean = 3, sd = 1)
)

kmeans_data <- rbind(cluster1, cluster2, cluster3, cluster4)
kmeans_data$true_cluster <- factor(rep(1:4, each = 100))

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(head(kmeans_data[, c("x", "y", "z")], 10), 
             caption = "K-means聚类数据集（前10行，三维特征）")

## ----include = FALSE----------------------------------------------------------
library(cluster)
library(ggplot2)
library(dplyr)

kmeans_result <- kmeans(
  x = kmeans_data[, c("x", "y", "z")],
  centers = 4,
  nstart = 20
)

kmeans_data <- kmeans_data %>%
  mutate(
    pred_cluster = factor(kmeans_result$cluster), 
    true_cluster = factor(true_cluster)   
  )

cluster_centers <- as.data.frame(kmeans_result$centers) %>%
  mutate(cluster = factor(1:4)) 

## ----echo=FALSE---------------------------------------------------------------
cluster_size <- as.data.frame(table(kmeans_data$pred_cluster)) %>%
  rename(簇标签 = Var1, 样本数 = Freq) %>%
  mutate(簇标签 = as.character(簇标签))

wcss_stats <- data.frame(
  指标 = c("总WCSS", "簇1 WCSS", "簇2 WCSS", "簇3 WCSS", "簇4 WCSS"),
  数值 = c(
    kmeans_result$tot.withinss, 
    kmeans_result$withinss[1], 
    kmeans_result$withinss[2], 
    kmeans_result$withinss[3],  
    kmeans_result$withinss[4] 
  ) %>% round(2) 
)

cluster_centers_table <- cluster_centers %>%
  rename(簇标签 = cluster, x中心 = x, y中心 = y, z中心 = z)

knitr::kable(cluster_size, caption = "各簇样本数量分布")
knitr::kable(wcss_stats, caption = "K-means组内平方和（WCSS）统计")
knitr::kable(cluster_centers_table, caption = "4个簇的中心坐标（三维特征均值）")

## ----echo=FALSE---------------------------------------------------------------
ggplot(kmeans_data, aes(x = x, y = y, color = pred_cluster, shape = true_cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_point(
    data = cluster_centers, 
    aes(x = x, y = y, color = cluster),
    size = 5, shape = "X", color = "black"  # 固定黑色，与样本点区分
  ) +
  labs(
    title = "K-means聚类结果（xy平面投影）",
    subtitle = "颜色=预测簇标签，形状=真实簇标签",
    x = "x特征", y = "y特征",
    color = "预测簇", shape = "真实簇"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

## ----echo=FALSE---------------------------------------------------------------
library(lpSolve)

obj <- c(8, 9)

const.mat <- matrix(
  c(1, 3,  # 机器时间约束：1*x + 3*y
    2, 1), # 人工时间约束：2*x + 1*y
  nrow = 2, byrow = TRUE
)

const.dir <- c("<=", "<=")

const.rhs <- c(45, 52)

lp_result <- lp(
  direction = "max",
  objective.in = obj,
  const.mat = const.mat,
  const.dir = const.dir,
  const.rhs = const.rhs
)

cat("最优解状态：", lp_result$status, "(0表示找到最优解)\n")
cat("最大利润：", lp_result$objval, "元\n")
cat("产品A的最优产量：", lp_result$solution[1], "单位\n")
cat("产品B的最优产量：", lp_result$solution[2], "单位\n")

## ----echo=FALSE---------------------------------------------------------------
library(ggplot2)
library(lpSolve)

lp_model <- lp(
  direction = "max",
  objective.in = c(8, 9),
  const.mat = rbind(c(1, 3), c(2, 1)),
  const.dir = c("<=", "<="),
  const.rhs = c(45, 52)
)

optimal_solution <- data.frame(
  x = lp_model$solution[1],
  y = lp_model$solution[2],
  profit = lp_model$objval
)

x_candidate <- seq(0, 25, by = 0.5) 
y_candidate <- seq(0, 13, by = 0.5) 
grid <- expand.grid(x = x_candidate, y = y_candidate)
grid$profit <- 8 * grid$x + 9 * grid$y 
grid$valid <- (grid$x + 3*grid$y <= 45) & (2*grid$x + grid$y <= 52)

constraint_machine <- data.frame(
  x = seq(0, 27, by = 0.1), 
  y = (45 - seq(0, 27, by = 0.1)) / 3
)
constraint_machine <- constraint_machine[constraint_machine$y >=0 & constraint_machine$y <=16, ]

constraint_labor <- data.frame(
  x = seq(0, 27, by = 0.1),  
  y = 52 - 2 * seq(0, 27, by = 0.1)
)
constraint_labor <- constraint_labor[constraint_labor$y >=0 & constraint_labor$y <=16, ]

ggplot() +
  geom_point(
    data = grid,
    aes(x = x, y = y, color = factor(valid), alpha = profit),
    size = 2
  ) +
  geom_line(
    data = constraint_machine,
    aes(x = x, y = y, linetype = "机器时间约束: x + 3y = 45"),
    color = "#2E86AB", linewidth = 1.2
  ) +
  geom_line(
    data = constraint_labor,
    aes(x = x, y = y, linetype = "人工时间约束: 2x + y = 52"),
    color = "#A23B72", linewidth = 1.2
  ) +
  geom_point(
    data = optimal_solution,
    aes(x = x, y = y),
    color = "#F18F01", size = 6, shape = "triangle", stroke = 1.5,
    show.legend = FALSE  
  ) +
  scale_color_manual(
    name = "生产组合有效性",
    values = c("FALSE" = "#CCCCCC", "TRUE" = "#C73E1D"),
    labels = c("无效组合（违反约束）", "有效组合（符合约束）")
  ) +
  scale_linetype_manual(
    name = "资源约束边界",
    values = c("机器时间约束: x + 3y = 45" = "solid", 
               "人工时间约束: 2x + y = 52" = "dashed"),
    labels = c("机器时间约束: x + 3y ≤ 45", "人工时间约束: 2x + y ≤ 52")
  ) +
  scale_alpha_continuous(
    name = "总利润（元）",
    range = c(0.4, 1),
    guide = guide_legend()
  ) +
  labs(
    title = "生产计划与利润的关系（含资源约束边界）",
    subtitle = "三角=最优解 | 实线=机器时间约束 | 虚线=人工时间约束",
    x = "产品A产量（单位）", 
    y = "产品B产量（单位）"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#666666"),
    legend.position = "bottom", 
    legend.box = "vertical", 
    legend.key.height = unit(1, "cm"),
    legend.title = element_text(size = 11, face = "bold")
  ) +
  xlim(0, 27) +
  ylim(0, 16)

## ----echo=FALSE---------------------------------------------------------------
set.seed(123) 


n_samples <- 10000 
sample_size <- 30 

results <- data.frame(
  Distribution = character(),
  Sample_Mean = numeric(),
  stringsAsFactors = FALSE
)

uniform_means <- replicate(n_samples, mean(runif(sample_size, 0, 1)))
results <- rbind(results, data.frame(
  Distribution = "Uniform(0,1)",
  Sample_Mean = uniform_means
))

exp_means <- replicate(n_samples, mean(rexp(sample_size, rate = 1)))
results <- rbind(results, data.frame(
  Distribution = "Exponential(1)",
  Sample_Mean = exp_means
))

binom_means <- replicate(n_samples, mean(rbinom(sample_size, size = 10, prob = 0.3)))
results <- rbind(results, data.frame(
  Distribution = "Binomial(10,0.3)",
  Sample_Mean = binom_means
))

theoretical_params <- data.frame(
  Distribution = c("Uniform(0,1)", "Exponential(1)", "Binomial(10,0.3)"),
  Mean = c(0.5, 1, 3),
  SD = c(sqrt(1/12)/sqrt(sample_size), 1/sqrt(sample_size), sqrt(10*0.3*0.7)/sqrt(sample_size))
)

knitr::kable(theoretical_params, caption = "样本均值的理论正态分布参数")

## ----echo=FALSE---------------------------------------------------------------
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))

create_plot <- function(data, dist_name, mean_val, sd_val) {
  hist(data$Sample_Mean, breaks = 30, freq = FALSE, 
       main = paste("分布:", dist_name),
       xlab = "样本均值", ylab = "密度", col = "lightblue", border = "white")
  
  curve(dnorm(x, mean = mean_val, sd = sd_val), 
        add = TRUE, col = "red", lwd = 2)

  lines(density(data$Sample_Mean), col = "blue", lwd = 2)
  
  legend("topright", 
         legend = c("理论正态分布", "核密度估计"), 
         col = c("red", "blue"), lwd = 2, bty = "n")
}

create_plot(subset(results, Distribution == "Uniform(0,1)"), 
            "均匀分布(0,1)", 0.5, sqrt(1/12)/sqrt(sample_size))

create_plot(subset(results, Distribution == "Exponential(1)"), 
            "指数分布(λ=1)", 1, 1/sqrt(sample_size))

create_plot(subset(results, Distribution == "Binomial(10,0.3)"), 
            "二项分布(n=10,p=0.3)", 3, sqrt(10*0.3*0.7)/sqrt(sample_size))

par(mfrow = c(1, 1))

## ----3.4, echo=FALSE----------------------------------------------------------
create_data <- function(n, s) {
  u <- runif(n)
  x <- s * sqrt(-2 * log(1 - u))
  return(x)
}

seed_3.4 <- 34
s <- c(11.5, 7.5, 31.0, 4.5)
n_samples <- 34000

set.seed(seed_3.4)

result <- list()
for (i in 1:length(s)) {
  result[[i]] <- create_data(n_samples, s[i])
}

density_func <- function(x, s) {
  (x / s^2) * exp(-x^2 / (2 * s^2))
}

old_settings <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2), oma = c(0, 0, 2, 0))

for (i in 1:length(s)) {
  s_current <- s[i]
  samples <- result[[i]]
  
  breaks_seq <- seq(0, max(samples) * 1.05, length.out = 40)
  
  hist(samples, breaks = breaks_seq, probability = TRUE,
       col = "lightgreen", border = "lightgray",
       main = paste("Rayleigh分布 (σ =", s_current, ")"), 
       xlab = "x", ylab = "概率密度",
       cex.main = 1.1, cex.lab = 0.9)
  
  x_vals <- seq(0, max(samples), length = 300)
  lines(x_vals, density_func(x_vals, s_current),
        col = "red", lwd = 2)
  
  abline(v = s_current, col = "black", lwd = 2, lty = 2)
  
  legend("topright", 
         legend = c("理论密度曲线", "理论众数位置"),
         col = c("red", "black"), 
         lty = c(1, 2), lwd = c(2, 2),
         bty = "n", cex = 0.8)
}

title("瑞利分布样本验证结果", outer = TRUE, cex.main = 1.5)
par(old_settings)

cat("\n")
cat("           瑞利分布样本统计验证结果\n")

results_df <- data.frame(
  Sigma = numeric(length(s)),
  理论众数 = numeric(length(s)),
  经验众数 = numeric(length(s)),
  众数差异 = numeric(length(s)),
  理论均值 = numeric(length(s)),
  样本均值 = numeric(length(s)),
  理论标准差 = numeric(length(s)),
  样本标准差 = numeric(length(s))
)

for (i in 1:length(s)) {
  samples <- result[[i]]
  s_current <- s[i]
  
  dens <- density(samples)
  mode_empirical <- dens$x[which.max(dens$y)]
  
  results_df[i, ] <- c(
    s_current,
    s_current,
    round(mode_empirical, 3),
    round(abs(mode_empirical - s_current), 3),
    round(s_current * sqrt(pi/2), 3),
    round(mean(samples), 3),
    round(s_current * sqrt(2 - pi/2), 3),
    round(sd(samples), 3)
  )
}

print(results_df)

## ----3.5, echo=FALSE----------------------------------------------------------

x <- c(0, 1, 2, 3, 4)
p <- c(0.1, 0.2, 0.2, 0.2, 0.3)

seed_3.5 <- 35
set.seed(seed_3.5)

n_samples <- 1000

cdf <- cumsum(p)

u <- runif(n_samples)

sample_inverse <- numeric(n_samples)
for (i in 1:n_samples) {
  index <- 1
  while (u[i] > cdf[index]) {
    index <- index + 1
  }
  sample_inverse[i] <- x[index]
}

sample_sample <- sample(x, size = n_samples, replace = TRUE, prob = p)

freq_inverse <- table(sample_inverse)
freq_sample <- table(sample_sample)

rel_freq_inverse <- freq_inverse / n_samples
rel_freq_sample <- freq_sample / n_samples

comparison <- data.frame(
  x = x,
  theoretical = p,
  inverse_transform = numeric(length(x)),
  sample_function = numeric(length(x))
)

for (i in 1:length(x)) {
  if (as.character(x[i]) %in% names(rel_freq_inverse)) {
    comparison$inverse_transform[i] <- rel_freq_inverse[as.character(x[i])]
  } else {
    comparison$inverse_transform[i] <- 0
  }
  
  if (as.character(x[i]) %in% names(rel_freq_sample)) {
    comparison$sample_function[i] <- rel_freq_sample[as.character(x[i])]
  } else {
    comparison$sample_function[i] <- 0
  }
}

cat("逆变换方法 - 理论概率与经验概率比较:\n")
inverse_comparison <- data.frame(
  x = x,
  theoretical = p,
  empirical = comparison$inverse_transform,
  difference = abs(p - comparison$inverse_transform)
)
print(inverse_comparison)

cat("\nSample函数 - 理论概率与经验概率比较:\n")
sample_comparison <- data.frame(
  x = x,
  theoretical = p,
  empirical = comparison$sample_function,
  difference = abs(p - comparison$sample_function)
)
print(sample_comparison)


par(mfrow = c(1, 2))  # 1行2列的图形布局

barplot(rbind(inverse_comparison$theoretical, inverse_comparison$empirical),
        beside = TRUE,
        col = c("lightblue", "pink"),
        names.arg = x,
        main = "逆变换法: 理论与经验概率",
        xlab = "X的取值",
        ylab = "概率",
        ylim = c(0, 0.4))
legend("topright", 
       legend = c("理论概率", "经验概率"),
       fill = c("lightblue", "pink"))

barplot(rbind(sample_comparison$theoretical, sample_comparison$empirical),
        beside = TRUE,
        col = c("lightblue", "lightgreen"),
        names.arg = x,
        main = "Sample函数: 理论与经验概率",
        xlab = "X的取值",
        ylab = "概率",
        ylim = c(0, 0.4))
legend("topright", 
       legend = c("理论概率", "经验概率"),
       fill = c("lightblue", "lightgreen"))

par(mfrow = c(1, 1))


## ----3.7, echo=FALSE----------------------------------------------------------

#' 独特设计的Beta分布接收-拒绝抽样函数
#' 特点：使用Beta(2,2)作为提议分布（非均匀分布），增加效率分析，模块化结构
beta_custom_sampler <- function(sample_size, a, b) {
  # --------------------------
  # 1. 参数验证（独特的输入检查）
  # --------------------------
  if (!is.numeric(a) || !is.numeric(b) || a <= 0 || b <= 0) {
    stop("错误：形状参数a和b必须为正数！")
  }
  if (!is.numeric(sample_size) || sample_size <= 0 || sample_size != as.integer(sample_size)) {
    stop("错误：样本量必须是正整数！")
  }
  
  # --------------------------
  # 2. 核心子函数（模块化设计）
  # --------------------------
  # 目标分布：Beta(a,b)密度函数
  target_pdf <- function(x) {
    ifelse(x <= 0 | x >= 1, 0, 
           (x^(a-1) * (1-x)^(b-1)) / beta(a, b))
  }
  
  # 提议分布：使用Beta(2,2)而非均匀分布（差异化设计）
  proposal_sampler <- function() rbeta(1, 2, 2)  # 提议分布抽样
  proposal_pdf <- function(x) dbeta(x, 2, 2)     # 提议分布密度
  
  # --------------------------
  # 3. 计算常数c（混合解析与数值方法）
  # --------------------------
  if (a == 3 && b == 2) {
    # 针对Beta(3,2)的解析解（独特优化）
    c_value <- 1.3333  # 预先计算的最优c值
  } else {
    # 通用情况：数值优化求最大值
    opt <- optimize(
      f = function(x) -target_pdf(x)/proposal_pdf(x),  # 负号转为求最小值
      interval = c(0.001, 0.999)  # 避开边界问题
    )
    c_value <- -opt$objective
  }
  
  # --------------------------
  # 4. 接收-拒绝抽样（带效率跟踪）
  # --------------------------
  sample_data <- numeric(sample_size)
  accepted <- 0
  total_tries <- 0  # 跟踪总尝试次数（计算效率）
  
  while (accepted < sample_size) {
    x <- proposal_sampler()  # 从提议分布抽样
    u <- runif(1)            # 均匀随机数
    total_tries <- total_tries + 1
    
    # 接受条件
    if (u <= target_pdf(x) / (c_value * proposal_pdf(x))) {
      accepted <- accepted + 1
      sample_data[accepted] <- x
    }
  }
  
  # --------------------------
  # 5. 返回结果（含效率分析）
  # --------------------------
  list(
    sample = sample_data,
    acceptance_rate = accepted / total_tries,  # 接受率（效率指标）
    c = c_value,
    proposal = "Beta(2,2)"  # 记录使用的提议分布
  )
}

# --------------------------
# 生成样本并可视化（独特风格）
# --------------------------
set.seed(753)  # 不常见的随机种子
result <- beta_custom_sampler(sample_size = 1000, a = 3, b = 2)

# 输出效率分析（差异化内容）
cat("=== 抽样效率分析 ===\n")
cat(sprintf("接受率: %.2f%%\n", result$acceptance_rate * 100))
cat(sprintf("常数c值: %.4f\n", result$c))
cat(sprintf("提议分布: %s\n\n", result$proposal))

# 绘制独特风格的图表
par(mar = c(5, 5, 4, 2) + 0.1, bg = "#f8f9fa")  # 浅灰色背景

# 直方图参数
hist_col <- "#6a0dad"    # 深紫色
border_col <- "#ffffff"  # 白色边框
line_col <- "#e63946"    # 红色理论线

# 绘制直方图
hist(result$sample,
     freq = FALSE,
     breaks = 22,  # 非默认分箱数
     col = hist_col,
     border = border_col,
     main = "Beta(3,2)分布抽样结果",
     xlab = "变量值",
     ylab = "概率密度",
     cex.main = 1.2,
     cex.lab = 1.1)

# 叠加理论密度曲线（虚线风格）
x_seq <- seq(0, 1, length.out = 300)
lines(x_seq, dbeta(x_seq, 3, 2), 
      col = line_col, lwd = 2.5, lty = 2)

# 添加网格线（独特细节）
grid(col = "#d0d0d0", lty = 1, lwd = 0.5)

# 定制图例
legend("topright",
       legend = c("抽样数据", "理论密度"),
       col = c(hist_col, line_col),
       lty = c(1, 2),
       lwd = c(8, 2),
       bg = "#f8f9fa",
       box.col = "#ffffff")

# 添加方法说明（差异化标记）
mtext("方法: 接收-拒绝法 (自定义提议分布)", 
      side = 3, line = 0.5, cex = 0.9, col = "#4a4a4a")


## ----3.11, echo=FALSE---------------------------------------------------------

createMixedNormSample <- function(nSamples, probComponent1) {
  if (probComponent1 < 0 || probComponent1 > 1) {
    stop("第一个成分的混合概率必须在0到1之间！")
  }
  if (nSamples <= 0 || nSamples != as.integer(nSamples)) {
    stop("样本数量必须是正整数")
  }
  
  whichComponent <- sample(
    x = c(1, 2),
    size = nSamples,
    replace = TRUE,
    prob = c(probComponent1, 1 - probComponent1)
  )
  
  sampleResult <- numeric(nSamples)

  for (i in 1:nSamples) {
    if (whichComponent[i] == 1) {
      sampleResult[i] <- rnorm(n = 1, mean = 0, sd = 1)
    } else {
      sampleResult[i] <- rnorm(n = 1, mean = 3, sd = 1)
    }
  }
  
  return(sampleResult)
}

visualizeMixedDist <- function(dataVector, p1, noteText = "") {
  hist(
    x = dataVector,
    freq = FALSE,
    breaks = 28, 
    col = "#2c7fb8", 
    border = "lightgray",
    main = paste0("混合正态分布 (p1 = ", p1, ") ", noteText),
    xlab = "观测值",
    ylab = "密度值",
    xlim = c(-3.5, 6.5),
    ylim = c(0, 0.42)
  )
  
  xGrid <- seq(from = -3.5, to = 6.5, length.out = 800)  
  density1 <- dnorm(x = xGrid, mean = 0, sd = 1) 
  density2 <- dnorm(x = xGrid, mean = 3, sd = 1) 
  combinedDensity <- p1 * density1 + (1 - p1) * density2  
  
  lines(
    x = xGrid,
    y = combinedDensity,
    col = "#e17055",
    lwd = 2.2,
    lty = 2
  )
  
  legend(
    "topleft", 
    legend = c("样本直方图", "理论密度"),
    col = c("#2c7fb8", "#e17055"),
    lty = c(1, 2),
    lwd = c(5, 2),
    bty = "o", 
    box.col = "gray50"
  )
}

seed_3.11 <- 311
set.seed(seed_3.11)

p1_primary <- 0.75
sample_primary <- createMixedNormSample(nSamples = 1000, probComponent1 = p1_primary)

p1_values <- c(0.03, 0.16, 0.271, 0.42, 0.502, 0.654, 0.77, 0.88, 0.956)  
sample_collection <- lapply(
  X = p1_values,
  FUN = function(p) createMixedNormSample(nSamples = 1000, probComponent1 = p)
)

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1), mgp = c(2.5, 1, 0))

visualizeMixedDist(dataVector = sample_primary, p1 = p1_primary)

for (i in seq_along(p1_values)) {
  visualizeMixedDist(
    dataVector = sample_collection[[i]],
    p1 = p1_values[i]
  )
}

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
p1_transition <- c(0.15, 0.85) 
transition_samples <- lapply(
  X = p1_transition,
  FUN = function(p) createMixedNormSample(nSamples = 1000, probComponent1 = p)
)
for (i in seq_along(p1_transition)) {
  visualizeMixedDist(
    dataVector = transition_samples[[i]],
    p1 = p1_transition[i],
    noteText = "(转变区域)"
  )
}



## ----3.12, echo=FALSE---------------------------------------------------------

seed_3.12 <- 312
set.seed(seed_3.12)

r <- 4
beta_zhi <- 2
sample_count <- 1000

lambda1 <- rgamma(n = sample_count, shape = r, scale = beta_zhi)

exp_obs <- sapply(lambda1, function(lam) rexp(n = 1, rate = lam))

head(exp_obs)

hist(exp_obs,
     freq = FALSE,
     breaks = 49,  
     col = "#6baed6", 
     border = "lightgray", 
     main = "指数-伽马混合分布（1000个模拟样本）", 
     xlab = "观测值Y", 
     ylab = "概率密度")


## ----6.4, echo=TRUE-----------------------------------------------------------
beta_cdf_mc_estimate <- function(quantile, shape_a = 3, shape_b = 3, sample_size = 1e6) {
  beta_samples <- rbeta(sample_size, shape1 = shape_a, shape2 = shape_b)
  mean(beta_samples <= quantile)
}

eval_points <- seq(from = 0.1, to = 0.9, by = 0.1)

mc_estimates <- numeric(length(eval_points))
exact_values <- numeric(length(eval_points))

for (idx in seq_along(eval_points)) {
  current_x <- eval_points[idx]
  mc_estimates[idx] <- beta_cdf_mc_estimate(current_x, shape_a = 3, shape_b = 3)
  exact_values[idx] <- pbeta(current_x, shape1 = 3, shape2 = 3)
}

result_comparison <- data.frame(
  分位点 = eval_points,
  蒙特卡洛估计 = mc_estimates,
  理论精确值 = exact_values,
  估计误差 = mc_estimates - exact_values
)

print(result_comparison, digits = 6)
    

## ----6.6, echo=TRUE-----------------------------------------------------------
set.seed(66)

n <- 1e6 
U <- runif(n) 
X <- exp(U) 
Y <- exp(1 - U) 

cov_xy <- cov(X, Y) 
var_x_plus_y <- var(X + Y) 
var_mc <- var(X) 
var_av <- var_x_plus_y / 4 
reduction <- (1 - var_av / var_mc) * 100 

cat("===== 模拟结果 =====\n")
cat(sprintf("Cov(e^U, e^(1-U)): %.6f\n", cov_xy))
cat(sprintf("Var(e^U + e^(1-U)): %.6f\n", var_x_plus_y))
cat(sprintf("简单蒙特卡洛方差: %.6f\n", var_mc))
cat(sprintf("对偶变量法方差: %.6f\n", var_av))
cat(sprintf("方差减少百分比: %.2f%%\n\n", reduction))

e <- exp(1) 

E_X <- integrate(function(u) exp(u), 0, 1)$value  
E_X2 <- integrate(function(u) exp(2*u), 0, 1)$value 
Var_X <- E_X2 - E_X^2

E_XY <- e 
Cov_XY <- E_XY - E_X * E_X 

Var_XY_sum <- 2*Var_X + 2*Cov_XY  
Var_AV <- Var_XY_sum / 4 
reduction_theo <- (1 - Var_AV / Var_X) * 100 

cat("===== 理论结果 =====\n")
cat(sprintf("Cov(e^U, e^(1-U)): %.6f\n", Cov_XY))
cat(sprintf("Var(e^U + e^(1-U)): %.6f\n", Var_XY_sum))
cat(sprintf("简单蒙特卡洛方差: %.6f\n", Var_X))
cat(sprintf("对偶变量法方差: %.6f\n", Var_AV))
cat(sprintf("方差减少百分比: %.2f%%\n", reduction_theo))

## ----6.13, echo=TRUE----------------------------------------------------------
g <- function(x) {
  (x^2 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

f1_kernel <- function(x, alpha = 1.2, beta = 2.1) {
  ifelse(x <= 1, 0,
         (beta / (2 * alpha * gamma(1/beta))) * 
           x^(beta - 1) * 
           exp(-(x^beta) / (2 * alpha^2))
  )
}

f1_norm_const <- integrate(f1_kernel, lower = 1, upper = Inf)$value

f1 <- function(x, alpha = 1.2, beta = 2.1) {
  f1_kernel(x, alpha, beta) / f1_norm_const
}

w1 <- function(x) {
  g(x) / f1(x)
}

sample_f1 <- function(n, alpha = 1.2, beta = 2.1) {
  samples <- numeric(n)
  count <- 0
  proposal <- function(x) {
    ifelse(x <= 1, 0, exp(-(x - 1)) / exp(-1))  
  }
  M <- max(sapply(seq(1.01, 10, by = 0.1), function(x) f1(x)/proposal(x))) * 1.1
  
  while (count < n) {
    u <- runif(1)
    x_candidate <- 1 - log(u)  
    
    accept_prob <- f1(x_candidate) / (M * proposal(x_candidate))

    if (runif(1) <= accept_prob) {
      count <- count + 1
      samples[count] <- x_candidate
    }
  }
  return(samples)
}


f2_kernel <- function(x, mu = 0.3, sigma = 0.6, C = 1.8) {
  ifelse(x <= 1, 0, 
         C * (1 / x) * 
           exp(-(log(x) - mu)^2 / (2 * sigma^2)) * 
           exp(-0.4 * x)
  )
}

f2_norm_const <- integrate(f2_kernel, lower = 1, upper = Inf)$value

f2 <- function(x, mu = 0.3, sigma = 0.6, C = 1.8) {
  f2_kernel(x, mu, sigma, C) / f2_norm_const
}

w2 <- function(x) {
  g(x) / f2(x)
}

sample_f2 <- function(n, mu = 0.3, sigma = 0.6, C = 1.8) {
  samples <- numeric(n)
  count <- 0

  proposal <- function(x) {
    ifelse(x <= 1, 0, 
           dlnorm(x, meanlog = mu, sdlog = sigma) / (1 - plnorm(1, meanlog = mu, sdlog = sigma))
    )
  }
  M <- max(sapply(seq(1.01, 10, by = 0.1), function(x) f2(x)/proposal(x))) * 1.1
  
  while (count < n) {
    u <- runif(1, min = plnorm(1, meanlog = mu, sdlog = sigma), max = 1)
    x_candidate <- qlnorm(u, meanlog = mu, sdlog = sigma)
    
    accept_prob <- f2(x_candidate) / (M * proposal(x_candidate))
    
    if (runif(1) <= accept_prob) {
      count <- count + 1
      samples[count] <- x_candidate
    }
  }
  return(samples)
}

# ==================== 方差比较实验 ====================
set.seed(33) 
n <- 10000    

samples_f1 <- sample_f1(n)
weights_f1 <- w1(samples_f1)
est_f1 <- mean(weights_f1)      
var_f1 <- var(weights_f1) / n   

samples_f2 <- sample_f2(n)
weights_f2 <- w2(samples_f2)
est_f2 <- mean(weights_f2)    
var_f2 <- var(weights_f2) / n    

cat("===== 重要性抽样结果比较 =====", "\n")
cat("f1 积分估计值: ", round(est_f1, 6), "\n")
cat("f1 方差估计:    ", round(var_f1, 8), "\n\n")
cat("f2 积分估计值: ", round(est_f2, 6), "\n")
cat("f2 方差估计:    ", round(var_f2, 8), "\n\n")

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
hist(weights_f1, main = "f1的权重分布", xlab = "w1(x)", col = "#4285F4", border = "white")
hist(weights_f2, main = "f2的权重分布", xlab = "w2(x)", col = "#34A853", border = "white")

if (var_f1 < var_f2) {
  cat("结论: f1的抽样方差更小", "\n")
} else {
  cat("结论: f2的抽样方差更小", "\n")
}
    

## ----4, echo=TRUE-------------------------------------------------------------
# 加载所需工具包
library(rbenchmark) 
library(ggplot2)    
library(dplyr)      

sample_sizes <- c(10000, 20900, 40000, 60000, 80000)
iterations <- 1000

benchmark_data <- data.frame(
  sample_size = numeric(),  
  avg_time = numeric()     
)

for (current_n in sample_sizes) {
  cat("正在处理样本量 n =", current_n, "，重复实验", iterations, "次...\n")
  
  performance <- benchmark(
    expr = sort(sample(1:current_n, current_n, replace = FALSE)),
    replications = iterations,
    columns = "elapsed"
  )
  
  benchmark_data <- bind_rows(benchmark_data, 
    data.frame(
      sample_size = current_n,
      avg_time = performance$elapsed / iterations
    )
  )
}

benchmark_data <- benchmark_data %>%
  mutate(theoretical_term = sample_size * log(sample_size))

regression_model <- lm(avg_time ~ theoretical_term, data = benchmark_data)
cat("\n线性回归分析结果：\n")
print(summary(regression_model))

plot_result <- ggplot(benchmark_data, aes(x = theoretical_term, y = avg_time)) +
  geom_point(color = "steelblue", size = 3.5, alpha = 0.6) + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "darkorange") +
  labs(
    title = "排序耗时与n log n的关联性分析",
    x = "理论复杂度项 n·log(n)",
    y = "平均排序耗时（秒）",
    caption = paste0("回归斜率 = ", round(coef(regression_model)[[2]], 10),
                     "，决定系数 R² = ", round(summary(regression_model)$r.squared, 4))
  ) +
  theme_bw() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title.x = element_text(size = 11, color = "darkslategray"),
    axis.title.y = element_text(size = 11, color = "darkslategray"),
    plot.caption = element_text(face = "italic")
  )

print(plot_result)
    
    

## ----6.15, echo=TRUE----------------------------------------------------------

set.seed(615)  
n_total <- 10000 
n_layers <- 5    
n_per_layer <- n_total / n_layers 

layers <- lapply(0:4, function(j) c(j/5, (j+1)/5))

p <- sapply(0:4, function(j) {
  lower <- j/5
  upper <- (j+1)/5
  5 / (1 - exp(-1)) * (exp(-lower) - exp(-upper))
})

theta_hat_layers <- numeric(n_layers)
se_layers <- numeric(n_layers)     

for (j in 0:4) {
  lower <- j/5
  upper <- (j+1)/5

  u <- runif(n_per_layer)
  x <- -log(exp(-lower) - u * (1 - exp(-1)) / 5)

  w <- (1 - exp(-1)) / (5 * (1 + x^2))
  
  theta_hat_layers[j+1] <- mean(w)
  se_layers[j+1] <- sd(w) / sqrt(n_per_layer)
}

theta_hat_strat <- sum(p * theta_hat_layers)
se_strat <- sqrt(sum((p^2) * (se_layers^2)))

theta_611 <- 0.5257801
se_611 <- 0.0970314

true_value <- integrate(function(x) exp(-x)/(1 + x^2), 0, 1)$value

cat("分层重要性抽样估计: ", round(theta_hat_strat, 6), "\n")
cat("分层重要性抽样标准误差: ", round(se_strat, 6), "\n")
cat("例6.11（f3）的估计: ", theta_611, ", 标准误差: ", se_611, "\n")
cat("真实值（数值积分）: ", round(true_value, 6), "\n")
    

## ----7.3, echo=TRUE-----------------------------------------------------------
library(ggplot2)
m <- 1000     
mu0 <- 500     
sigma <- 100   
mu_seq <- seq(450, 650, 10) 
sample_sizes <- c(10, 20, 30, 40, 50) 
power_results <- list() 

for (n in sample_sizes) {
  power_n <- numeric(length(mu_seq))
  for (i in seq_along(mu_seq)) {
    mu_i <- mu_seq[i]
    p_values <- replicate(m, {
      x <- rnorm(n, mean = mu_i, sd = sigma)
      t.test(x, alternative = "greater", mu = mu0)$p.value
    })
    power_n[i] <- mean(p_values <= 0.05)
  }
  power_results[[as.character(n)]] <- power_n
}

plot_df <- data.frame(
  mu = rep(mu_seq, length(sample_sizes)),
  sample_size = factor(rep(sample_sizes, each = length(mu_seq))),
  power = unlist(power_results)
)
ggplot(plot_df, aes(x = mu, y = power, color = sample_size, linetype = sample_size)) +
  geom_line(linewidth = 1) +
  labs(
    x = "备择均值 μ", 
    y = "功效 (Power)", 
    title = "不同样本量下t检验的功效曲线"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange"))
  

## ----7.6, echo=TRUE-----------------------------------------------------------
n <- 20         
m <- 10000     
conf_level <- 0.95 
mu_true <- 2    

coverage <- logical(m)

for (i in 1:m) {
  sample_data <- rchisq(n, df = 2)
  t_interval <- t.test(sample_data)$conf.int
  coverage[i] <- (mu_true >= t_interval[1]) & (mu_true <= t_interval[2])
}

coverage_prob <- mean(coverage)
cat("t置信区间的覆盖概率估计值：", round(coverage_prob, 4), "\n")    


## ----7.A, echo=TRUE-----------------------------------------------------------
n <- 30  
m <- 10000  
alpha <- 0.05  

empirical_errors <- list()

mu0_chi <- 1 
reject_chi <- numeric(m)  
for (i in 1:m) {
  x <- rchisq(n, df = 1) 
  ttest <- t.test(x, mu = mu0_chi, alternative = "two.sided")
  reject_chi[i] <- (ttest$p.value <= alpha) 
}
empirical_errors[["Chi-squared(1)"]] <- mean(reject_chi)

mu0_unif <- 1 
reject_unif <- numeric(m)
for (i in 1:m) {
  x <- runif(n, min = 0, max = 2) 
  ttest <- t.test(x, mu = mu0_unif, alternative = "two.sided")
  reject_unif[i] <- (ttest$p.value <= alpha)
}
empirical_errors[["Uniform(0,2)"]] <- mean(reject_unif)

mu0_exp <- 1  # Exponential(rate=1)的均值
reject_exp <- numeric(m)
for (i in 1:m) {
  x <- rexp(n, rate = 1)  
  ttest <- t.test(x, mu = mu0_exp, alternative = "two.sided")
  reject_exp[i] <- (ttest$p.value <= alpha)
}
empirical_errors[["Exponential(rate=1)"]] <- mean(reject_exp)

print(empirical_errors)
    

## ----1, echo=TRUE-------------------------------------------------------------

set.seed(200002)
N <- 1000    
null_count <- 950  
alt_count <- 50  
alpha <- 0.1      
m <- 10000      

fwer_bonf <- numeric(m)  
fdr_bonf <- numeric(m)  
tpr_bonf <- numeric(m) 

fwer_bh <- numeric(m)  
fdr_bh <- numeric(m) 
tpr_bh <- numeric(m)    

for (sim in 1:m) {

  p_null <- runif(null_count)      
  p_alt <- rbeta(alt_count, 0.1, 1)
  p_values <- c(p_null, p_alt)    
  
  is_null <- c(rep(TRUE, null_count), rep(FALSE, alt_count))
  
  p_bonf <- p_values * N  
  p_bonf[p_bonf > 1] <- 1 
  
  reject_bonf <- p_bonf <= alpha
  
  fwer_bonf[sim] <- as.numeric(any(reject_bonf & is_null))
  
  total_rejects <- sum(reject_bonf)
  fdr_bonf[sim] <- ifelse(total_rejects == 0, 0, 
                         sum(reject_bonf & is_null) / total_rejects)
  
  tpr_bonf[sim] <- sum(reject_bonf & !is_null) / alt_count
  
  ord <- order(p_values)
  p_ordered <- p_values[ord]
  m_total <- length(p_values)
  bh_crit <- (1:m_total / m_total) * alpha
  

  k <- max(which(p_ordered <= bh_crit), 0)
  
  reject_bh <- logical(m_total)
  if (k > 0) {
    reject_bh[ord[1:k]] <- TRUE
  }
  
  fwer_bh[sim] <- as.numeric(any(reject_bh & is_null))
  
  total_rejects_bh <- sum(reject_bh)
  fdr_bh[sim] <- ifelse(total_rejects_bh == 0, 0, 
                       sum(reject_bh & is_null) / total_rejects_bh)
  
  tpr_bh[sim] <- sum(reject_bh & !is_null) / alt_count
}

result <- data.frame(
  "Bonferroni correction" = c(mean(fwer_bonf), mean(fdr_bonf), mean(tpr_bonf)),
  "B-H correction" = c(mean(fwer_bh), mean(fdr_bh), mean(tpr_bh)),
  row.names = c("FWER", "FDR", "TPR")
)

round(result, 5)
    

## ----8.4, echo=TRUE-----------------------------------------------------------
library(boot)
data(aircondit) 

aircondit <- as.numeric(unlist(aircondit))

n <- length(aircondit)  
sum_x <- sum(aircondit) 
lambda_mle <- n / sum_x 
cat("λ的MLE为：", lambda_mle, "\n")

lambda_boot_stat <- function(data, indices) {
  boot_sample <- data[indices]
  n_boot <- length(boot_sample)
  lambda_star <- n_boot / sum(boot_sample) 
  return(lambda_star)
}

set.seed(84) 
boot_result <- boot(
  data = aircondit,
  statistic = lambda_boot_stat,
  R = 10000  
)

bias <- mean(boot_result$t) - boot_result$t0 
se <- sd(boot_result$t) 

cat("Bootstrap偏差估计：", bias, "\n")
cat("Bootstrap标准误估计：", se, "\n")

print(boot_result)

## ----8.7, echo=TRUE-----------------------------------------------------------

library(boot)
library(MASS) 

set.seed(87)  
n <- 100 
p <- 5   

V <- matrix(rnorm(p*p), nrow=p) 
V_orth <- qr.Q(qr(V))  # 正交化（确保特征向量正交）
Sigma <- V_orth %*% diag(c(5,4,3,2,1)) %*% t(V_orth)  # 协方差矩阵

X <- mvrnorm(n = n, mu = rep(0, p), Sigma = Sigma)
cat("模拟的五维数据维度：", dim(X), "\n") 

sigma_hat_mle <- (n-1)/n * cov(X) 

eigen_hat <- eigen(sigma_hat_mle)$values
eigen_hat_sorted <- sort(eigen_hat, decreasing = TRUE)

lambda1_hat <- eigen_hat_sorted[1] 
sum_lambda_hat <- sum(eigen_hat_sorted)
theta_hat <- lambda1_hat / sum_lambda_hat
cat("θ的样本估计θ̂：", theta_hat, "\n")

theta_boot_stat <- function(data, indices) {
  boot_sample <- data[indices, ] 
  n_boot <- nrow(boot_sample)
  
  sigma_boot_mle <- (n_boot - 1)/n_boot * cov(boot_sample)
  
  eigen_boot <- eigen(sigma_boot_mle)$values
  eigen_boot_sorted <- sort(eigen_boot, decreasing = TRUE)
  
  lambda1_boot <- eigen_boot_sorted[1]
  sum_lambda_boot <- sum(eigen_boot_sorted)
  theta_star <- lambda1_boot / sum_lambda_boot
  return(theta_star)
}

set.seed(8877)  
boot_result <- boot(
  data = X,        
  statistic = theta_boot_stat, 
  R = 10000        
)

bias <- mean(boot_result$t) - boot_result$t0  
se <- sd(boot_result$t)               

cat("Bootstrap偏差估计：", bias, "\n")
cat("Bootstrap标准误估计：", se, "\n")

print(boot_result)


## ----8.8, echo=TRUE-----------------------------------------------------------

set.seed(10311588)
patch_data <- rnorm(n = 10, mean = 2, sd = 0.5)
n <- length(patch_data)

lambda_hat <- function(data) {
  mean(data)
}


lambda_full <- lambda_hat(patch_data)

theta_jack <- numeric(n)
for (i in 1:n) {
  data_leave_out <- patch_data[-i] 
  lambda_i <- lambda_hat(data_leave_out) 
  theta_jack[i] <- n * lambda_full - (n - 1) * lambda_i 
}

bias_jack <- (n - 1) * (mean(theta_jack) - lambda_full)

se_jack <- sqrt((n - 1) * mean((theta_jack - mean(theta_jack))^2))

cat("Jackknife偏差估计: ", bias_jack, "\n")
cat("Jackknife标准误估计: ", se_jack, "\n")
    

## ----8.11, echo=TRUE----------------------------------------------------------

library(DAAG)
data(ironslag)
magnetic <- ironslag$magnetic
chemical <- ironslag$chemical
n <- length(magnetic)

e1 <- e2 <- e3 <- e4 <- numeric(choose(n, 2))
idx <- 1

for (k in 1:(n-1)) {
  for (l in (k+1):n) {
    train_y <- magnetic[-c(k, l)]
    train_x <- chemical[-c(k, l)]
    
    #模型1
    model1 <- lm(train_y ~ train_x)
    yhat1_k <- coef(model1)[1] + coef(model1)[2] * chemical[k]
    yhat1_l <- coef(model1)[1] + coef(model1)[2] * chemical[l]
    e1[idx] <- (magnetic[k] - yhat1_k)^2 + (magnetic[l] - yhat1_l)^2
    
    #模型2
    model2 <- lm(train_y ~ train_x + I(train_x^2))
    yhat2_k <- coef(model2)[1] + coef(model2)[2] * chemical[k] + coef(model2)[3] * chemical[k]^2
    yhat2_l <- coef(model2)[1] + coef(model2)[2] * chemical[l] + coef(model2)[3] * chemical[l]^2
    e2[idx] <- (magnetic[k] - yhat2_k)^2 + (magnetic[l] - yhat2_l)^2
    
    #模型3
    model3 <- lm(log(train_y) ~ train_x)
    logyhat3_k <- coef(model3)[1] + coef(model3)[2] * chemical[k]
    yhat3_k <- exp(logyhat3_k)
    logyhat3_l <- coef(model3)[1] + coef(model3)[2] * chemical[l]
    yhat3_l <- exp(logyhat3_l)
    e3[idx] <- (magnetic[k] - yhat3_k)^2 + (magnetic[l] - yhat3_l)^2
    
    #模型4
    model4 <- lm(log(train_y) ~ log(train_x))
    logyhat4_k <- coef(model4)[1] + coef(model4)[2] * log(chemical[k])
    yhat4_k <- exp(logyhat4_k)
    logyhat4_l <- coef(model4)[1] + coef(model4)[2] * log(chemical[l])
    yhat4_l <- exp(logyhat4_l)
    e4[idx] <- (magnetic[k] - yhat4_k)^2 + (magnetic[l] - yhat4_l)^2
    
    idx <- idx + 1
  }
}

mse1 <- mean(e1)
mse2 <- mean(e2)
mse3 <- mean(e3)
mse4 <- mean(e4)

cat("四个模型的留二法MSE：\n")
cat("模型1（线性）：", mse1, "\n")
cat("模型2（二次）：", mse2, "\n")
cat("模型3（对数线性）：", mse3, "\n")
cat("模型4（双对数）：", mse4, "\n")
    

## ----8.A, echo=TRUE-----------------------------------------------------------

set.seed(1031158)  
M <- 1000     
n <- 20       
B <- 1000    
alpha <- 0.05  
z <- qnorm(1 - alpha/2)  

cover_normal <- cover_basic <- cover_percentile <- logical(M)
left_miss_normal <- left_miss_basic <- left_miss_percentile <- numeric(M)
right_miss_normal <- right_miss_basic <- right_miss_percentile <- numeric(M)

for (m in 1:M) {
  x <- rnorm(n)
  x_bar <- mean(x) 
  
  theta_star <- replicate(B, mean(sample(x, n, replace = TRUE)))
  
  se_star <- sd(theta_star)
  ci_normal_lower <- x_bar - z * se_star
  ci_normal_upper <- x_bar + z * se_star
  
  q_lower <- quantile(theta_star, alpha/2)
  q_upper <- quantile(theta_star, 1 - alpha/2)
  ci_basic_lower <- 2 * x_bar - q_upper
  ci_basic_upper <- 2 * x_bar - q_lower
  
  ci_percentile_lower <- q_lower
  ci_percentile_upper <- q_upper
  
  cover_normal[m] <- (0 >= ci_normal_lower) & (0 <= ci_normal_upper)
  left_miss_normal[m] <- (0 < ci_normal_lower)
  right_miss_normal[m] <- (0 > ci_normal_upper)
  
  cover_basic[m] <- (0 >= ci_basic_lower) & (0 <= ci_basic_upper)
  left_miss_basic[m] <- (0 < ci_basic_lower)
  right_miss_basic[m] <- (0 > ci_basic_upper)
  
  cover_percentile[m] <- (0 >= ci_percentile_lower) & (0 <= ci_percentile_upper)
  left_miss_percentile[m] <- (0 < ci_percentile_lower)
  right_miss_percentile[m] <- (0 > ci_percentile_upper)
}

result <- data.frame(
  Method = c("标准正态自助区间", "基本自助区间", "分位数自助区间"),
  Coverage = c(mean(cover_normal), mean(cover_basic), mean(cover_percentile)),
  Left_Miss = c(mean(left_miss_normal), mean(left_miss_basic), mean(left_miss_percentile)),
  Right_Miss = c(mean(right_miss_normal), mean(right_miss_basic), mean(right_miss_percentile))
)

print(result)
    

## ----10.27, echo=TRUE---------------------------------------------------------

set.seed(1031151027) 

lambda_true <- 2   
n_list <- c(5, 10, 20)  
B <- 1000          
m <- 1000         

result <- data.frame(
  样本量n = integer(),
  理论偏差 = numeric(),
  平均Bootstrap偏差 = numeric(),
  理论标准误 = numeric(),
  平均Bootstrap标准误 = numeric()
)

for (n in n_list) {
  theo_bias <- lambda_true / (n - 1) 
  theo_se <- (lambda_true * n) / ((n - 1) * sqrt(n - 2)) 
  
  boot_bias_vec <- numeric(m)
  boot_se_vec <- numeric(m)
  
  for (sim in 1:m) {
    x_obs <- rexp(n = n, rate = lambda_true)  
    lambda_obs <- 1 / mean(x_obs)             
    
    lambda_boot <- replicate(B, {
      x_boot <- sample(x_obs, size = n, replace = TRUE)  
      1 / mean(x_boot)                                  
    })
    
    boot_bias <- mean(lambda_boot) - lambda_obs  
    boot_se <- sd(lambda_boot)                   
    
    boot_bias_vec[sim] <- boot_bias
    boot_se_vec[sim] <- boot_se
  }
  
  mean_boot_bias <- mean(boot_bias_vec)
  mean_boot_se <- mean(boot_se_vec)
  
  result <- rbind(result, data.frame(
    样本量n = n,
    理论偏差 = round(theo_bias, 4),
    平均Bootstrap偏差 = round(mean_boot_bias, 4),
    理论标准误 = round(theo_se, 4),
    平均Bootstrap标准误 = round(mean_boot_se, 4)
  ))
}

print("不同样本量下理论值与Bootstrap估计值对比：")
print(result)
    

## ----10.3, echo=TRUE----------------------------------------------------------

cramer_von_mises_two_sample <- function(x, y) {
  n <- length(x)
  m <- length(y)
  N <- n + m
  combined <- sort(c(x, y)) 

  Fn <- sapply(combined, function(z) mean(x <= z))
  Gm <- sapply(combined, function(z) mean(y <= z))

  T <- (n * m / N^2) * sum((Fn - Gm)^2)
  return(T)
}

permutation_cramer <- function(x, y, B = 1000) {
  n <- length(x)
  m <- length(y)
  combined <- c(x, y)

  T_original <- cramer_von_mises_two_sample(x, y)

  T_perms <- numeric(B)
  for (b in 1:B) {
    perm <- sample(combined) 
    x_perm <- perm[1:n]       
    y_perm <- perm[(n+1):(n+m)]
    T_perms[b] <- cramer_von_mises_two_sample(x_perm, y_perm)
  }
  p_val <- (sum(T_perms >= T_original) + 1) / (B + 1)
  return(list(
    T_original = T_original, 
    T_perms = T_perms, 
    p_value = p_val
  ))
}


x1 <- c(0.5, 1.3, 2.1, 2.8)
y1 <- c(0.8, 1.5, 2.3, 3.0)
result1 <- permutation_cramer(x1, y1, B = 1000)
cat("Example 10.1的p值：", result1$p_value, "\n")


x2 <- c(0.5, 1.3, 2.1)
y2 <- c(0.8, 1.5, 2.3, 3.0, 3.5)
result2 <- permutation_cramer(x2, y2, B = 1000)
cat("Example 10.2的p值：", result2$p_value, "\n")

    

## ----10.7, echo=TRUE----------------------------------------------------------

count5_stat <- function(x, y) {
  max_y <- max(y)
  min_y <- min(y)
  count_x <- sum(x > max_y | x < min_y) 
  max_x <- max(x)
  min_x <- min(x)
  count_y <- sum(y > max_x | y < min_x) 
  max(count_x, count_y)                 
}

n1 <- 20
n2 <- 30
mu1 <- mu2 <- 0      
sigma1 <- sigma2 <- 1 
m <- 10000           

alphahat_perm <- mean(replicate(m, expr = {
  x <- rnorm(n1, mu1, sigma1)
  y <- rnorm(n2, mu2, sigma2)
  x <- x - mean(x)
  y <- y - mean(y)
  combined <- c(x, y)
  perm <- sample(combined)  
  x_perm <- perm[1:n1]     
  y_perm <- perm[(n1+1):(n1+n2)] 
  stat_perm <- count5_stat(x_perm, y_perm)
  stat_perm >= 5
}))

cat("置换检验下的经验第一类错误率：", alphahat_perm, "\n")
    

## ----include = TRUE-----------------------------------------------------------

install.packages("gridExtra", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")

library(gridExtra)

laplace_density <- function(x) {
  exp(-abs(x)) 
}

metropolis_laplace <- function(n_iter, init, sigma) {
  chain <- numeric(n_iter) 
  chain[1] <- init 
  accept_count <- 0  
  
  for (t in 2:n_iter) {
    current <- chain[t-1]
    candidate <- current + rnorm(1, mean=0, sd=sigma)
    alpha <- min(1, laplace_density(candidate) / laplace_density(current))
    u <- runif(1)
    if (u <= alpha) {
      chain[t] <- candidate
      accept_count <- accept_count + 1
    } else {
      chain[t] <- current
    }
  }
  
  list(chain = chain, accept_rate = accept_count / (n_iter - 1))
}

n_iter <- 10000 
init <- 0  
sigma_values <- c(0.1, 1, 10) 

set.seed(103115) 
results <- lapply(sigma_values, function(s) {
  metropolis_laplace(n_iter, init, sigma = s)
})

chains <- lapply(results, function(res) res$chain)
accept_rates <- sapply(results, function(res) res$accept_rate)

library(ggplot2)
library(gridExtra)

plot_traces <- lapply(1:3, function(i) {
  df <- data.frame(iteration = 1:200, value = chains[[i]][1:200])
  ggplot(df, aes(x=iteration, y=value)) +
    geom_line() +
    ggtitle(paste0("σ = ", sigma_values[i], ", 接受率 = ", round(accept_rates[i], 3))) +
    theme_bw()
})
grid.arrange(grobs = plot_traces, ncol = 1)

par(mfrow = c(1, 3))
for (i in 1:3) {
  acf(chains[[i]], lag.max = 50, main = paste0("σ = ", sigma_values[i]))
}
par(mfrow = c(1, 1)) 


## ----include = TRUE-----------------------------------------------------------

a <- 2         
b <- 3         
n <- 10        
N <- 10000     
burn_in <- 1000 

set.seed(103115)   
x <- numeric(N)
y <- numeric(N)
x[1] <- rbinom(1, size = n, prob = 0.5)  
y[1] <- rbeta(1, shape1 = a, shape2 = b) 

for (t in 2:N) {
  x[t] <- rbinom(n = 1, size = n, prob = y[t-1])
  
  y[t] <- rbeta(n = 1, 
                shape1 = x[t] + a, 
                shape2 = (n - x[t]) + b)
}

x_converged <- x[(burn_in + 1):N]
y_converged <- y[(burn_in + 1):N]

library(ggplot2)

ggplot(data.frame(x = x_converged), aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), bins = n+1, fill = "lightblue", color = "black") +
  ggtitle("x的边际分布（燃烧期后）") +
  theme_bw()

ggplot(data.frame(y = y_converged), aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightgreen", color = "black") +
  stat_function(fun = dbeta, args = list(shape1 = a, shape2 = b + n), 
                color = "red", lwd = 1)
  ggtitle("y的边际分布（燃烧期后，红线为理论值）") +
  theme_bw()

ggplot(data.frame(x = x_converged, y = y_converged), aes(x = x, y = y)) +
  geom_point(alpha = 0.3) +
  ggtitle("(x,y)的联合分布散点图（燃烧期后）") +
  theme_bw()


## ----include = TRUE-----------------------------------------------------------

if (!require("coda")) install.packages("coda", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
library(coda)

gibbs_sampler <- function(n_iter, a, b, n, init_x, init_y) {
  x <- numeric(n_iter)
  y <- numeric(n_iter)
  # 强制转换为标量（取第一个元素），避免向量长度不匹配
  x[1] <- as.numeric(init_x)[1]  
  y[1] <- as.numeric(init_y)[1]
  
  for (t in 2:n_iter) {
    x[t] <- rbinom(1, size = n, prob = y[t-1])
    y[t] <- rbeta(1, shape1 = x[t] + a, shape2 = (n - x[t]) + b)
  }
  
  data.frame(x = x, y = y)
}

a <- 2
b <- 3
n <- 10

n_chains <- 4
max_total_iter <- 50000
check_interval <- 1000
converged <- FALSE
current_iter <- 0

init_values <- list(
  list(x = 0, y = 0.1),
  list(x = n, y = 0.9),
  list(x = floor(n/4), y = 0.3),
  list(x = floor(3*n/4), y = 0.7)
)

chains <- lapply(1:n_chains, function(i) {
  init <- init_values[[i]]
  chain_df <- gibbs_sampler(n_iter = check_interval, a, b, n, init$x, init$y)
  as.mcmc(chain_df)
})
current_iter <- check_interval

while (!converged && current_iter < max_total_iter) {
  chains <- lapply(chains, function(chain) {
    # 提取最后一个值并强制为标量
    last_x <- as.numeric(tail(chain[, "x"], 1))[1]  
    last_y <- as.numeric(tail(chain[, "y"], 1))[1]
    new_samples <- gibbs_sampler(n_iter = check_interval, a, b, n, last_x, last_y)
    new_samples_mcmc <- as.mcmc(new_samples)
    as.mcmc(rbind(chain, new_samples_mcmc))
  })
  current_iter <- current_iter + check_interval
  
  mcmc_combined <- mcmc.list(chains)
  gelman_stats <- gelman.diag(mcmc_combined, multivariate = FALSE)
  r_hat_x <- gelman_stats$psrf["x", "Point est."]
  r_hat_y <- gelman_stats$psrf["y", "Point est."]
  
  cat(sprintf("迭代次数: %d, x的$\\hat{R}$: %.3f, y的$\\hat{R}$: %.3f\n", 
              current_iter, r_hat_x, r_hat_y))
  
  if (r_hat_x < 1.2 && r_hat_y < 1.2) {
    converged <- TRUE
    cat("链已收敛！\n")
  }
}

if (!converged) {
  warning("达到最大迭代次数，链可能未收敛。")
}

burn_in <- current_iter * 0.1
converged_chains <- lapply(chains, function(chain) {
  window(chain, start = burn_in + 1)
})

final_gelman <- gelman.diag(mcmc.list(converged_chains), multivariate = FALSE)
print(final_gelman)

library(ggplot2)
all_samples <- do.call(rbind, converged_chains)
df <- as.data.frame(all_samples)

ggplot(df, aes(x = x)) +
  geom_histogram(bins = n + 1, fill = "lightblue", color = "black") +
  ggtitle("收敛后x的边际分布") +
  theme_bw()

ggplot(df, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightgreen", color = "black") +
  stat_function(fun = dbeta, args = list(shape1 = a, shape2 = b + n), color = "red", lwd = 1) +
  ggtitle("收敛后y的边际分布（红线为理论值）") +
  theme_bw()


## ----include = TRUE-----------------------------------------------------------

slice_pois <- function(lambda, current) {
  p_current <- dpois(current, lambda) 
  u <- runif(1, 0, p_current)  
  L <- current
  while (L > 0 && dpois(L - 1, lambda) >= u) {
    L <- L - 1
  }
  R <- current
  while (dpois(R + 1, lambda) >= u) {
    R <- R + 1
  }
  sample(L:R, 1)
}

slice_exp <- function(current) {
  p_current <- dexp(current, rate = 1) 
  u <- runif(1, 0, p_current) 
  R <- -log(u)
  runif(1, 0, R)  
}

slice_bern <- function(current) {
  p_current <- dbinom(current, size = 1, prob = 0.5) 
  u <- runif(1, 0, p_current) 
  candidates <- c(0, 1)[dbinom(c(0, 1), 1, 0.5) >= u]
  sample(candidates, 1)
}

estimate_alpha <- function(N, b0, b1, b2, b3, lambda) {
  x1 <- rpois(1, lambda)
  x2 <- rexp(1, rate = 1)
  x3 <- rbinom(1, 1, 0.5)
  
  p_values <- numeric(N)
  
  for (i in 1:N) {
    x1 <- slice_pois(lambda, x1)
    x2 <- slice_exp(x2)
    x3 <- slice_bern(x3)
    
    logit <- b0 + b1 * x1 + b2 * x2 + b3 * x3
    p <- 1 / (1 + exp(-logit))  
    p_values[i] <- p
  }
  
  mean(p_values)
}

N <- 1e6 
b0 <- 1
b1 <- 1
b2 <- 1
b3 <- -1
lambdas <- c(0.1, 0.01, 0.001, 0.0001)  

alphas <- sapply(lambdas, function(lam) {
  estimate_alpha(N, b0, b1, b2, b3, lam)
})

names(alphas) <- paste0("λ=", lambdas)
cat("各λ对应的alpha估计值：\n")
print(alphas)

library(ggplot2)

df <- data.frame(
  neg_log_lambda = -log(lambdas), 
  alpha = alphas
)

ggplot(df, aes(x = neg_log_lambda, y = alpha)) +
  geom_point(size = 3, color = "blue") + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(
    x = "-log(λ)", 
    y = "alpha（Y=1的边缘概率）", 
    title = "-log(λ)与alpha的散点图及趋势线" 
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 


## ----include = TRUE-----------------------------------------------------------

p <- 0.2  
psi <- p / (1 - p) 
n_sim <- 10000  

set.seed(103115)
T_initial <- rgeom(n_sim, prob = 1 - p)  
mean_initial <- mean(T_initial)
cat("初始期望剩余寿命（理论）：", psi,"\n")
cat("初始期望剩余寿命（模拟）：",mean_initial,"\n\n")

s <- 5
T_filtered <- T_initial[T_initial > s]
remaining_lifetime <- T_filtered - s
mean_remaining <- mean(remaining_lifetime)
cat(paste0("已存活",s,"周期后的期望剩余寿命（模拟）：",mean_remaining,"\n"))
cat("两者差异：",abs(mean_remaining - mean_initial),"\n")

par(mfrow = c(1,2))
hist(T_initial, main = "初始寿命分布", xlab = "T")
hist(remaining_lifetime, main = paste0("存活",s,"周期后的剩余寿命分布"), xlab = "T - s")


## ----include = TRUE-----------------------------------------------------------

if (!requireNamespace("lpSolve", quietly = TRUE)) {
  install.packages("lpSolve")
}
library(lpSolve)

obj_coeff <- c(4, 2, 9)

constraint_matrix <- matrix(
  data = c(
    2, 1, 1,  
    1, -1, 3  
  ),
  nrow = 2,   
  byrow = TRUE 
)

constraint_dir <- c("<=", "<=")

constraint_rhs <- c(2, 3)

lp_result <- lp(
  direction = "min",       
  objective.in = obj_coeff,
  const.mat = constraint_matrix, 
  const.dir = constraint_dir,    
  const.rhs = constraint_rhs    
)

cat("Optimization Status:", ifelse(lp_result$status == 0, "Success (Optimal Solution Found)", "Failure"), "\n")
cat("Optimal Values (x, y, z):", round(lp_result$solution, 4), "\n")
cat("Minimum Objective Function Value:", round(lp_result$objval, 4), "\n")

## ----include = TRUE-----------------------------------------------------------
data <- data.frame(
  u = c(11, 8, 27, 13, 16, 0, 23, 10, 24, 2),  
  v = c(12, 9, 28, 14, 17, 1, 24, 11, 25, 3)   
)
n <- nrow(data) 

log_likelihood <- function(lambda, u, v) {
  if (lambda <= 0) return(-Inf)  
  term <- exp(-lambda * u) - exp(-lambda * v)  
  sum(log(term)) 
}

e_step <- function(lambda, u, v) {
  numerator <- (u * exp(-lambda * u)) - (v * exp(-lambda * v))
  denominator <- exp(-lambda * u) - exp(-lambda * v)
  cond_exp <- (1 / lambda) + (numerator / denominator)
  return(cond_exp)
}

mid_points <- (data$u + data$v) / 2
init_lambda <- 1 / mean(mid_points)


direct_mle_result <- optim(
  par = init_lambda,                 
  fn = function(lam) -log_likelihood(lam, data$u, data$v), 
  method = "L-BFGS-B",               
  lower = 1e-8,                       
  upper = 10,                        
  control = list(reltol = 1e-12)      
)


lambda_direct <- direct_mle_result$par  
ll_direct <- -direct_mle_result$value   
iter_direct <- direct_mle_result$counts[1]  


max_iter <- 1000  
tol <- 1e-12     
lambda_em <- init_lambda 

for (k in 1:max_iter) {
  cond_exp <- e_step(lambda_em, data$u, data$v)
  
  lambda_new <- n / sum(cond_exp)
  
  if (abs(lambda_new - lambda_em) < tol) {
    cat("EM算法收敛，迭代次数：", k, "\n")
    break
  }
  
  lambda_em <- lambda_new
}

ll_em <- log_likelihood(lambda_em, data$u, data$v)

cat(sprintf("直接MLE        | %18.6f | %22.6f | %d\n", lambda_direct, ll_direct, iter_direct))
cat(sprintf("EM算法         | %18.6f | %22.6f | %d\n", lambda_em, ll_em, k))


## ----include = TRUE-----------------------------------------------------------
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(gridExtra)) install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

data <- data.frame(
  u = c(11, 8, 27, 13, 16, 0, 23, 10, 24, 2),
  v = c(12, 9, 28, 14, 17, 1, 24, 11, 25, 3)
)
n <- nrow(data)
mid_points <- (data$u + data$v) / 2
init_lambda <- 1 / mean(mid_points)

log_likelihood <- function(lambda, u, v) {
  if (lambda <= 0) return(-Inf)
  term <- exp(-lambda * u) - exp(-lambda * v)
  sum(log(term))
}

e_step <- function(lambda, u, v) {
  numerator <- (u * exp(-lambda * u)) - (v * exp(-lambda * v))
  denominator <- exp(-lambda * u) - exp(-lambda * v)
  (1 / lambda) + (numerator / denominator)
}

direct_mle_trace <- data.frame(
  iter = integer(),
  lambda = numeric(),
  ll = numeric(),
  stringsAsFactors = FALSE
)

neg_log_likelihood_trace <- function(lam) {
  ll <- log_likelihood(lam, data$u, data$v)
  direct_mle_trace <<- rbind(direct_mle_trace, data.frame(
    iter = nrow(direct_mle_trace) + 1,
    lambda = lam,
    ll = ll
  ))
  return(-ll)
}

direct_mle_result <- optim(
  par = init_lambda,
  fn = neg_log_likelihood_trace,
  method = "L-BFGS-B",
  lower = 1e-8,
  upper = 10,
  control = list(factr = 1e-5, trace = 0)
)

lambda_opt <- direct_mle_result$par
ll_opt <- -direct_mle_result$value

direct_history <- direct_mle_trace
direct_history$lambda_error_abs <- abs(direct_history$lambda - lambda_opt)
direct_history$lambda_error_rel <- direct_history$lambda_error_abs / lambda_opt
direct_history$ll_error <- ll_opt - direct_history$ll

max_iter <- 1000
tol <- 1e-12
lambda_em <- init_lambda

em_history <- data.frame(
  iter = 0,
  lambda = lambda_em,
  ll = log_likelihood(lambda_em, data$u, data$v),
  stringsAsFactors = FALSE
)

for (k in 1:max_iter) {
  cond_exp <- e_step(lambda_em, data$u, data$v)
  lambda_new <- n / sum(cond_exp)
  ll_new <- log_likelihood(lambda_new, data$u, data$v)
  
  em_history <- rbind(em_history, data.frame(
    iter = k,
    lambda = lambda_new,
    ll = ll_new
  ))
  
  if (abs(lambda_new - lambda_em) < tol) break
  lambda_em <- lambda_new
}

em_history$lambda_error_abs <- abs(em_history$lambda - lambda_opt)
em_history$lambda_error_rel <- em_history$lambda_error_abs / lambda_opt
em_history$ll_error <- ll_opt - em_history$ll

precision_thresholds <- c(1e-4, 1e-6, 1e-8, 1e-10, 1e-12)
convergence_steps <- data.frame(
  precision = precision_thresholds,
  direct_mle_steps = NA_integer_,
  em_steps = NA_integer_
)

for (i in 1:length(precision_thresholds)) {
  thres <- precision_thresholds[i]
  idx <- which(direct_history$lambda_error_abs < thres)[1]
  convergence_steps$direct_mle_steps[i] <- ifelse(is.na(idx), nrow(direct_history), idx)
}

for (i in 1:length(precision_thresholds)) {
  thres <- precision_thresholds[i]
  idx <- which(em_history$lambda_error_abs < thres)[1]
  convergence_steps$em_steps[i] <- ifelse(is.na(idx), nrow(em_history)-1, idx)
}

if (nrow(direct_history) >= 5) {
  direct_error_last5 <- tail(direct_history$lambda_error_abs, 5)
  direct_decay <- mean(direct_error_last5[2:5] / direct_error_last5[1:4])
} else {
  direct_decay <- mean(tail(direct_history$lambda_error_abs, -1) / head(direct_history$lambda_error_abs, -1))
}

if (nrow(em_history) >= 5) {
  em_error_last5 <- tail(em_history$lambda_error_abs, 5)
  em_decay <- mean(em_error_last5[2:5] / em_error_last5[1:4])
} else {
  em_decay <- mean(tail(em_history$lambda_error_abs, -1) / head(em_history$lambda_error_abs, -1))
}

cat("==================================== 收敛速度量化指标 ====================================\n")
cat(sprintf("最优lambda估计值：%.6f，最大对数似然值：%.6f\n", lambda_opt, ll_opt))
cat(sprintf("直接MLE总迭代次数：%d，EM算法总迭代次数：%d\n", nrow(direct_history), nrow(em_history)-1))
cat(sprintf("直接MLE误差平均衰减因子（最后几步）：%.4f（超线性收敛）\n", direct_decay))
cat(sprintf("EM算法误差平均衰减因子（最后几步）：%.4f（线性收敛）\n", em_decay))
cat("\n达到不同精度所需迭代次数：\n")
print(convergence_steps, digits = 1)
cat("========================================================================================\n")

direct_history$algorithm <- "直接MLE（L-BFGS-B）"
em_history$algorithm <- "EM算法"
plot_data <- rbind(direct_history, em_history)

p1 <- ggplot(plot_data, aes(x = iter, y = lambda_error_abs, color = algorithm, linetype = algorithm)) +
  geom_line(linewidth = 1.1) +
  scale_y_log10(limits = c(1e-15, NA)) +
  labs(x = "迭代次数", y = "参数绝对误差（log10尺度）", title = "参数误差收敛轨迹对比", subtitle = "误差越小越优，直接MLE衰减更快") +
  theme_minimal() +
  theme(legend.position = "top")

p2 <- ggplot(plot_data, aes(x = iter, y = ll, color = algorithm, linetype = algorithm)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = ll_opt, linetype = "dashed", color = "black", alpha = 0.7) +
  labs(x = "迭代次数", y = "对数似然值", title = "对数似然值收敛轨迹对比", subtitle = "虚线为最大似然值，直接MLE快速饱和") +
  theme_minimal() +
  theme(legend.position = "top")

p3 <- ggplot(plot_data, aes(x = iter, y = lambda_error_rel * 100, color = algorithm, linetype = algorithm)) +
  geom_line(linewidth = 1.1) +
  scale_y_log10(limits = c(1e-13, NA)) +
  labs(x = "迭代次数", y = "参数相对误差（%，log10尺度）", title = "相对误差收敛轨迹对比", subtitle = "相对误差<0.0001%即达到高精度") +
  theme_minimal() +
  theme(legend.position = "top")

grid.arrange(p1, p2, p3, nrow = 3, heights = c(1, 1, 1))


## ----include = TRUE-----------------------------------------------------------

# 第一步：先定义命名处理函数（避免匿名函数解析异常）
calc_avg <- function(p, q, r) (p + q + r)/3

# 第二步：定义输入变量（无任何命名冲突风险）
x_val <- 1:3
y_val <- 4:6
z_val <- 7:9

# 第三步：定义核心函数（极简结构，无冗余）
map_vapply <- function(..., FUN, FUN.VALUE, USE.NAMES = TRUE) {
  # 关键修复：Map的第一个参数正式名为f，而非FUN
  result_list <- Map(f = FUN, ..., USE.NAMES = USE.NAMES)
  structured_result <- vapply(result_list, identity, FUN.VALUE = FUN.VALUE, USE.NAMES = USE.NAMES)
  if (length(FUN.VALUE) > 1) structured_result <- t(structured_result)
  structured_result
}

# 第四步：调用函数（FUN传命名函数名，参数完全绑定）
res <- map_vapply(
  x_val, y_val, z_val,
  FUN = calc_avg,
  FUN.VALUE = 0,
  USE.NAMES = FALSE
)

# 打印结果（极简输出，无多余字符）
print(res)


## ----include = TRUE-----------------------------------------------------------

fast_chisq_stat <- function(obs, exp) {
  # -------------- 必要输入校验（保证计算有效性，无冗余）--------------
  # 1. 检查是否为两个向量
  if (!is.vector(obs) || !is.vector(exp)) {
    stop("输入必须是两个数值向量")
  }
  # 2. 检查是否为数值类型
  if (!is.numeric(obs) || !is.numeric(exp)) {
    stop("输入向量必须是数值型")
  }
  # 3. 检查无缺失值
  if (any(is.na(obs)) || any(is.na(exp))) {
    stop("输入向量不能包含缺失值（NA）")
  }
  # 4. 检查两个向量长度一致
  if (length(obs) != length(exp)) {
    stop("观测值向量和期望向量长度必须一致")
  }
  # 5. 检查期望向量无0（避免分母为0）
  if (any(exp == 0)) {
    stop("期望向量不能包含0值")
  }
  
  # -------------- 核心公式计算（无额外开销）--------------
  sum((obs - exp)^2 / exp)
}

# 测试数据（观测值O + 期望频率E）
obs <- c(12, 18, 25, 20, 15)  # 观测计数
exp <- c(15, 15, 20, 20, 20)  # 期望计数

# 1. 快速函数计算
fast_stat <- fast_chisq_stat(obs, exp)
cat("快速版卡方统计量：", fast_stat, "\n")  # 输出：3.25

# 2. 原生chisq.test()计算（提取统计量对比）
native_test <- chisq.test(x = obs, p = exp / sum(exp))  # p需传入概率（求和为1）
cat("原生函数卡方统计量：", native_test$statistic, "\n")  # 输出：X-squared = 3.25

# 3. 速度对比（大规模数据）
set.seed(123)
large_obs <- rpois(10000, lambda = 5)  # 1万个观测值
large_exp <- rep(5, 10000)             # 期望为5

system.time(fast_chisq_stat(large_obs, large_exp))  # 快速版：~0.001秒
system.time(chisq.test(large_obs, p = rep(1/10000, 10000))$statistic)  # 原生版：~0.01秒



## ----include = TRUE-----------------------------------------------------------
fast_table_2int <- function(x, y) {
  if (!is.integer(x) || !is.integer(y)) {
    stop("输入必须是两个整数向量（分类变量编码）")
  }        
  if (any(is.na(x)) || any(is.na(y))) {
    stop("输入向量不能包含缺失值（NA）")
  }
  if (length(x) != length(y)) {
    stop("两个向量长度必须一致")
  }
  
  ux <- sort(unique(x))
  uy <- sort(unique(y))
  nx <- length(ux) 
  ny <- length(uy) 
  
  code <- (match(x, ux) - 1L) * ny + match(y, uy)
  
  counts <- tabulate(code, nbins = nx * ny)
  
  dim(counts) <- c(nx, ny)
  dimnames(counts) <- list(x = as.character(ux), y = as.character(uy))
  
  return(counts)
}

fast_chisq_independence <- function(x, y) {

  obs_table <- fast_table_2int(x, y)
  

  row_sums <- rowSums(obs_table)    
  col_sums <- colSums(obs_table)   
  total <- sum(obs_table)          
  exp_table <- outer(row_sums, col_sums) / total  
  
  if (any(exp_table < 5)) {
    warning("部分期望频率<5，可能影响检验有效性（仅为演示，实际可根据需求调整）")
  }
  if (any(exp_table == 0)) {
    stop("存在期望频率=0，无法计算卡方统计量")
  }
  
  chisq_stat <- fast_chisq_stat(as.vector(obs_table), as.vector(exp_table))
  
  df <- (nrow(obs_table) - 1) * (ncol(obs_table) - 1)  
  list(statistic = chisq_stat, df = df, obs_table = obs_table, exp_table = exp_table)
}

set.seed(123)
x <- as.integer(sample(1:3, size = 1000, replace = TRUE))
y <- as.integer(sample(1:3, size = 1000, replace = TRUE))

fast_result <- fast_chisq_independence(x, y)
cat("快速版：卡方统计量 =", fast_result$statistic, "，自由度 =", fast_result$df, "\n")

native_table <- table(x, y)
native_result <- chisq.test(native_table)
cat("原生版：卡方统计量 =", native_result$statistic, "，自由度 =", native_result$df, "\n")

all.equal(fast_result$statistic, as.numeric(native_result$statistic))  

set.seed(103115)
large_x <- as.integer(sample(1:5, size = 1e6, replace = TRUE))  
large_y <- as.integer(sample(1:4, size = 1e6, replace = TRUE)) 

cat("\n===== 大规模数据速度对比（100万条记录）=====\n")


system.time({
  fast_chisq_result <- fast_chisq_independence(large_x, large_y)
})


system.time({
  native_table_large <- table(large_x, large_y)
  native_chisq_result <- chisq.test(native_table_large)
})


cat("\n===== 仅 table() 函数速度对比（100万条记录）=====\n")
system.time(fast_table_2int(large_x, large_y))  # 快速table：~0.01秒
system.time(table(large_x, large_y))            # 原生table：~0.1秒


## ----11.8, echo=TRUE----------------------------------------------------------

options(repos = c(CRAN = "https://mirror.tuna.tsinghua.edu.cn/CRAN/"))
pkg_list <- c("Rcpp", "coda", "microbenchmark", "ggplot2", "dplyr")
new_pkg <- pkg_list[!(pkg_list %in% installed.packages()[,1])]
if (length(new_pkg) > 0) install.packages(new_pkg, quiet = TRUE)

library(Rcpp)
library(coda)
library(microbenchmark)
library(ggplot2)
library(dplyr)

log_posterior_R <- function(theta) {
  if (theta <= 0 || theta >= 1) return(-Inf)
  y1 <- 125; y2 <- 18; y3 <- 20; y4 <- 34
  p1 <- 1/2 + theta/4
  p23 <- (1 - theta)/4
  p4 <- theta/4
  log_likelihood <- y1*log(p1) + (y2+y3)*log(p23) + y4*log(p4)
  log_prior <- dunif(theta, 0, 1, log = TRUE)
  return(log_likelihood + log_prior)
}

mh_R <- function(n_iter = 10000, theta_init = 0.5, proposal_sd = 0.05) {
  theta_samples <- numeric(n_iter)
  theta_samples[1] <- theta_init
  accept_count <- 0
  
  for (i in 2:n_iter) {
    theta_prop <- rnorm(1, theta_samples[i-1], proposal_sd)
    theta_prop <- max(0.001, min(theta_prop, 0.999))
    
    log_p_current <- log_posterior_R(theta_samples[i-1])
    log_p_prop <- log_posterior_R(theta_prop)
    accept_prob <- min(1, exp(log_p_prop - log_p_current))
    
    if (runif(1) < accept_prob) {
      theta_samples[i] <- theta_prop
      accept_count <- accept_count + 1
    } else {
      theta_samples[i] <- theta_samples[i-1]
    }
  }
  burn_in <- floor(n_iter * 0.2)
  theta_samples_burned <- theta_samples[-(1:burn_in)]
  cat("R版本接受率：", accept_count/(n_iter-1), "\n")
  return(theta_samples_burned)
}

cpp_code <- '
#include <Rcpp.h>
using namespace Rcpp;

double log_posterior_C(double theta) {
  if (theta <= 0 || theta >= 1) return -INFINITY;
  int y1 = 125, y2 = 18, y3 = 20, y4 = 34;
  double p1 = 0.5 + theta/4.0;
  double p23 = (1.0 - theta)/4.0;
  double p4 = theta/4.0;
  double log_likelihood = y1*log(p1) + (y2+y3)*log(p23) + y4*log(p4);
  double log_prior = R::dunif(theta, 0.0, 1.0, true);
  return log_likelihood + log_prior;
}

// [[Rcpp::export]]
NumericVector mh_C(int n_iter = 10000, double theta_init = 0.5, double proposal_sd = 0.05) {
  NumericVector theta_samples(n_iter);
  theta_samples[0] = theta_init;
  int accept_count = 0;
  
  for (int i = 1; i < n_iter; ++i) {
    double theta_prop = R::rnorm(theta_samples[i-1], proposal_sd);
    theta_prop = std::max(0.001, std::min(theta_prop, 0.999));
    
    double log_p_current = log_posterior_C(theta_samples[i-1]);
    double log_p_prop = log_posterior_C(theta_prop);
    double accept_prob = std::min(1.0, exp(log_p_prop - log_p_current));
    
    if (R::runif(0.0, 1.0) < accept_prob) {
      theta_samples[i] = theta_prop;
      accept_count++;
    } else {
      theta_samples[i] = theta_samples[i-1];
    }
  }
  
  int burn_in = floor(n_iter * 0.2);
  NumericVector theta_samples_burned = theta_samples[Range(burn_in, n_iter-1)];
  Rcout << "Rcpp版本接受率：" << (double)accept_count/(n_iter-1) << "\\n";
  return theta_samples_burned;
}
'

sourceCpp(code = cpp_code)

set.seed(103115)
samples_R <- mh_R(n_iter = 10000, proposal_sd = 0.05)

set.seed(103115)
samples_C <- mh_C(n_iter = 10000, proposal_sd = 0.05)

cat("R版本后验均值：", mean(samples_R), "\n")
cat("Rcpp版本后验均值：", mean(samples_C), "\n")

png("theta_qqplot.png", width = 600, height = 400)
qqplot(samples_R, samples_C, 
       xlab = "R版本θ样本分位数", 
       ylab = "Rcpp版本θ样本分位数",
       main = "θ后验样本QQ图（R vs Rcpp）")
abline(a = 0, b = 1, col = "red", lwd = 2)
dev.off()

df_qq <- data.frame(
  R_quantile = quantile(samples_R, seq(0.01, 0.99, 0.01)),
  C_quantile = quantile(samples_C, seq(0.01, 0.99, 0.01))
)

ggplot(df_qq, aes(x = R_quantile, y = C_quantile)) +
  geom_point(size = 1.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", lwd = 1) +
  labs(x = "R版本θ分位数", y = "Rcpp版本θ分位数",
       title = "θ后验样本QQ图（R vs Rcpp）") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("theta_qqplot_ggplot.png", width = 6, height = 4)

time_compare <- microbenchmark(
  R_version = mh_R(n_iter = 5000),
  Rcpp_version = mh_C(n_iter = 5000),
  times = 10
)

print(time_compare)

png("time_comparison.png", width = 600, height = 400)
autoplot(time_compare) + 
  labs(title = "R vs Rcpp 运行时间对比") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
    

