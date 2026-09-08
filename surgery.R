library(tidyverse)
library(igraph)
library(ggraph)

# list of words related to the topic
word_list <- tolower(c(
  "surgery", "robotic surgery", "minimally invasive surgery",
  "artificial intelligence", "machine learning", "deep learning",
  "computer vision", "algorithms", "data analysis", "predictive models",
  "decision support systems", "diagnosis", "risk assessment", "complication",
  "preoperative", "intraoperative", "postoperative", "robot",
  "electronic health record", "big data", "sensors", "innovation",
  "imaging", "laparoscopy", "ai"
))

# key words to highlight
main_words <- c("ai", "surgery", "robot", "decision support systems")

#set up a matrix to track word pairs
n_words <- length(word_list)
cooc_mat <- matrix(0, n_words, n_words)
rownames(cooc_mat) <- word_list
colnames(cooc_mat) <- word_list

# loop through text files 21 to 30
got_files <- FALSE
for (i in 21:30) {
  fname <- paste0(i, ".txt")
  if (file.exists(fname)) {
    got_files <- TRUE
    # read and clean the text
    text <- readLines(fname) %>% paste(collapse = " ")
    words_in_text <- unlist(strsplit(tolower(text), "\\W+"))
    words_in_text <- words_in_text[words_in_text %in% word_list]
    
    # count pairs if we have enough words
    if (length(words_in_text) > 1) {
      pairs <- combn(words_in_text, 2)
      for (j in 1:ncol(pairs)) {
        w1 <- pairs[1, j]
        w2 <- pairs[2, j]
        cooc_mat[w1, w2] <- cooc_mat[w1, w2] + 1
        cooc_mat[w2, w1] <- cooc_mat[w2, w1] + 1
      }
    }
  }
}

# stop if no files found
if (!got_files) {
  stop("Couldn't find any text files!")
}

# turn matrix into edges for the network
edges <- data.frame(from = character(), to = character(), count = numeric())
for (i in 1:(n_words - 1)) {
  for (j in (i + 1):n_words) {
    if (cooc_mat[i, j] > 0) {
      edges <- rbind(edges, data.frame(
        from = word_list[i],
        to = word_list[j],
        count = cooc_mat[i, j]
      ))
    }
  }
}

# make sure count is a number
edges$count <- as.numeric(edges$count)

# build the network, include all words to start
net <- graph_from_data_frame(edges, directed = FALSE, vertices = data.frame(name = word_list))
net <- add_edges(net, as.matrix(edges[, c("from", "to")]))

V(net)$degree <- degree(net)
V(net)$label <- V(net)$name
V(net)$is_main <- V(net)$name %in% main_words
# color 'ai' red, 'surgery' blue, others light blue
V(net)$color <- ifelse(V(net)$name == "ai", "red",
                       ifelse(V(net)$name == "surgery", "blue", "skyblue"))

# keep nodes with connections, or 'ai' and 'surgery'
V(net)$keep <- V(net)$degree > 0 | V(net)$name %in% c("ai", "surgery")
net <- induced_subgraph(net, V(net)[V(net)$keep])

# check if we have anything to show
if (vcount(net) == 0) {
  message("No nodes to plot, even 'ai' or 'surgery'!")
  return(NULL)
}

# make it repeatable
set.seed(123)

# Network Visualization
ggraph(net, layout = "fr") +  # force-directed to put big nodes in the middle
  geom_edge_link(aes(width = count), alpha = 0.6, colour = "gray50", curvature = 0.1) +
  geom_node_point(aes(size = degree, fill = color), shape = 21, alpha = 0.9, colour = "black") +
  geom_node_text(aes(label = label, fontface = ifelse(is_main, "bold", "plain")),
                 repel = TRUE, size = 4, family = "Arial", max.overlaps = 20) +
  scale_size_continuous(range = c(5, 12), name = "Connections") +
  scale_fill_identity() +
  scale_edge_width_continuous(range = c(0.5, 2)) +
  labs(
    title = "AI in Surgery: Word Network",
    caption = "From text files 21.txt to 30.txt"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = "gray80", size = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.caption = element_text(size = 8, hjust = 0.5, margin = margin(t = 10)),
    plot.margin = margin(15, 15, 15, 15)
  )