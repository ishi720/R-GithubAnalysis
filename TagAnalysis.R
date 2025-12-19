# =============================================================================
# GitHub ユーザー リポジトリタグ（トピック）分析スクリプト
# =============================================================================

# 共通設定ファイルの読み込み
source("config.R")

# 必要なパッケージのインストール（初回のみ）
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cran.r-project.org")
      library(pkg, character.only = TRUE)
    }
  }
}

install_if_missing(c("gh", "ggplot2", "dplyr", "scales", "RColorBrewer", "ggwordcloud"))

# =============================================================================
# ユーザーのリポジトリからタグ（トピック）を取得
# =============================================================================

get_user_repo_topics <- function(username) {
  cat("ユーザーのリポジトリを取得中:", username, "\n")
  
  tryCatch({
    # 認証済みユーザー情報を取得
    auth_user <- tryCatch({
      gh::gh("GET /user")$login
    }, error = function(e) NULL)
    
    # 認証済みユーザーと同じ場合は /user/repos を使用
    if (!is.null(auth_user) && tolower(auth_user) == tolower(username)) {
      cat("認証済みユーザーとして取得（プライベートリポジトリ含む）\n")
      repos <- gh::gh("GET /user/repos",
                      visibility = "all",
                      affiliation = "owner",
                      per_page = 100,
                      .limit = Inf)
    } else {
      cat("公開リポジトリのみ取得\n")
      repos <- gh::gh("GET /users/{username}/repos",
                      username = username,
                      per_page = 100,
                      .limit = Inf)
    }
    
    cat("見つかったリポジトリ数:", length(repos), "\n\n")
    
    # 各リポジトリのトピックを取得
    all_topics <- data.frame()
    repo_topics_list <- list()
    
    for (repo in repos) {
      # フォークしたリポジトリはスキップ
      if (isTRUE(repo$fork)) {
        next
      }
      
      visibility <- if (isTRUE(repo$private)) "[Private]" else "[Public]"
      cat("  -", repo$name, visibility)
      
      # トピックを取得
      topics <- tryCatch({
        gh::gh("GET /repos/{owner}/{repo}/topics",
               owner = username,
               repo = repo$name,
               .send_headers = c("Accept" = "application/vnd.github.mercy-preview+json"))
      }, error = function(e) list(names = list()))
      
      topic_names <- unlist(topics$names)
      
      if (length(topic_names) > 0) {
        cat(" (", length(topic_names), "topics)\n")
        
        # リポジトリごとのトピックを保存
        repo_topics_list[[repo$name]] <- topic_names
        
        # 全トピックのデータフレームに追加
        for (topic in topic_names) {
          topic_data <- data.frame(
            repo = repo$name,
            topic = topic,
            language = ifelse(is.null(repo$language), "Unknown", repo$language),
            stringsAsFactors = FALSE
          )
          all_topics <- rbind(all_topics, topic_data)
        }
      } else {
        cat(" (no topics)\n")
      }
      
      Sys.sleep(0.1)  # API制限対策
    }
    
    return(list(
      topics_df = all_topics,
      repo_topics = repo_topics_list
    ))
    
  }, error = function(e) {
    cat("エラー:", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# 集計関数
# =============================================================================

# トピック別リポジトリ数
summarize_by_topic <- function(topics_df) {
  if (nrow(topics_df) == 0) {
    return(data.frame(topic = character(), repos = integer()))
  }
  
  topics_df %>%
    group_by(topic) %>%
    summarise(repos = n_distinct(repo), .groups = "drop") %>%
    arrange(desc(repos))
}

# =============================================================================
# タグクラウド形式で出力
# =============================================================================

# テキスト形式のタグクラウド出力
print_tag_cloud <- function(topic_summary) {
  if (nrow(topic_summary) == 0) {
    cat("トピックがありません\n")
    return(invisible())
  }
  
  # "トピック名 数" の形式で結合
  tags <- paste0(topic_summary$topic, " ", topic_summary$repos)
  tag_cloud <- paste(tags, collapse = ", ")
  
  cat("\n===== タグクラウド =====\n")
  cat(tag_cloud, "\n")
  
  return(invisible(tag_cloud))
}

# =============================================================================
# ワードクラウド画像を作成（PNG出力）
# =============================================================================

create_wordcloud_png <- function(topic_summary, title) {
  if (nrow(topic_summary) == 0) {
    return(NULL)
  }
  
  # カラーパレット
  n_colors <- nrow(topic_summary)
  if (n_colors <= 8) {
    colors <- RColorBrewer::brewer.pal(max(3, n_colors), "Dark2")
  } else {
    colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_colors)
  }
  
  # ggwordcloudでワードクラウドを作成
  p <- ggplot(topic_summary, aes(label = topic, size = repos, color = topic)) +
    geom_text_wordcloud(
      rm_outside = TRUE,
      eccentricity = 1,
      seed = 42
    ) +
    scale_size_area(max_size = 30) +
    scale_color_manual(values = colors) +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(2, 2, 2, 2)
    )
  
  return(p)
}

# =============================================================================
# カラーパレット生成関数
# =============================================================================

generate_colors <- function(n) {
  if (n <= 12) {
    return(RColorBrewer::brewer.pal(max(3, n), "Set3"))
  } else {
    base_colors <- RColorBrewer::brewer.pal(12, "Set3")
    colorRampPalette(base_colors)(n)
  }
}

# =============================================================================
# グラフ作成関数
# =============================================================================

# トピック別リポジトリ数 横棒グラフ
create_topic_bar <- function(df, title, top_n = 20) {
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  df <- head(df, top_n)
  df$topic <- factor(df$topic, levels = rev(df$topic))
  colors <- generate_colors(nrow(df))
  
  ggplot(df, aes(x = topic, y = repos, fill = topic)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = repos), hjust = -0.2, size = 4) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    scale_fill_manual(values = colors) +
    labs(title = title, x = "", y = "リポジトリ数") +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )
}

# =============================================================================
# メイン処理
# =============================================================================

main <- function() {
  user <- TARGET_USER
  
  if (is.null(user) || user == "") {
    user <- readline(prompt = "GitHubユーザー名を入力: ")
    if (user == "") {
      stop("ユーザー名が指定されていません")
    }
  }
  
  # データ取得
  result <- get_user_repo_topics(user)
  
  if (is.null(result)) {
    stop("データを取得できませんでした")
  }
  
  topics_df <- result$topics_df
  repo_topics <- result$repo_topics
  
  # 集計
  topic_summary <- summarize_by_topic(topics_df)
  
  # タグクラウド形式で出力
  tag_cloud_text <- print_tag_cloud(topic_summary)
  
  # 統計サマリー
  repos_with_topics <- length(repo_topics)
  unique_topics <- length(unique(topics_df$topic))
  
  cat("\n===== サマリー =====\n")
  cat("トピック付きリポジトリ数:", repos_with_topics, "\n")
  cat("ユニークトピック数:", unique_topics, "\n")
  
  # グラフを作成して保存
  cat("\nグラフを作成中...\n")
  
  output_dir <- OUTPUT_DIR
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  saved_files <- c()
  
  if (nrow(topic_summary) > 0) {
    # トピック別リポジトリ数 横棒グラフ
    p1 <- create_topic_bar(topic_summary, paste0(user, " のトピック別リポジトリ数"))
    bar_height <- max(6, min(nrow(topic_summary), 20) * 0.4)
    ggsave(file.path(output_dir, "topics_bar.png"), p1,
           width = 10, height = bar_height, dpi = 150)
    saved_files <- c(saved_files, "topics_bar.png")
    
    # ワードクラウド（PNG）
    p2 <- create_wordcloud_png(topic_summary, paste0(user, " のトピック"))
    ggsave(file.path(output_dir, "topics_wordcloud.png"), p2,
           width = 8, height = 8, dpi = 150)
    saved_files <- c(saved_files, "topics_wordcloud.png")
  }
  
  cat("\n保存完了:\n")
  for (f in saved_files) {
    cat("  -", file.path(output_dir, f), "\n")
  }
  
  if (nrow(topic_summary) > 0) {
    print(p2)
  }
  
  return(list(
    topic_summary = topic_summary,
    tag_cloud_text = tag_cloud_text,
    stats = list(
      repos_with_topics = repos_with_topics,
      unique_topics = unique_topics
    )
  ))
}

# 実行
result <- main()