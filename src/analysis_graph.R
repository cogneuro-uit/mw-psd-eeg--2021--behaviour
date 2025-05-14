library(ProjectTemplate)
relative_path=""
load.project()
library(DiagrammeR)

# Create the nodes data frame
create_node_df(
  n = 5
  , label = c("PSD", "Positive\nmood", "BV", "Probes", "AE")
  , fontsize = 14
  , style = "filled"
  , fillcolor = "lightblue"
  , shape = "rectangle"
  , width = c(1, 1, 1, 1, 1)
  , height = c(0.6, 0.5, 0.5, 0.5, 0.5)
  , x = c(1, 3.5, 5.5, 6, 6)
  , y = c(2.2, 2.5, 4, 2, .7)
  ) |>
  create_graph(
    # Create the edges data frame
    create_edge_df(
      from = c(1, 1, 1, 2, 2, 3, 3, 5)
      , to = c(3, 2, 4, 3, 4, 4.2, 4, 4)
      , color = c("black", "black", "black", "black", "black", "black", "red", "red")
      , style = c("solid", "dotted", "solid", "dotted", "dotted", "solid", "solid", "dotted")
      , label = c("", "", " MW\nMB ", "", "MW\nS-MW", "MW\nMB ", " S-MW ", " MW ")
      , fontsize = 12
      , rel = c("curved", "curved", "curved", "curved", "curved", "normal", "normal", "normal")
      , penwidth = c(1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5)
      
    )
  ) |> 
  add_node(label = "positive association", node_aes = node_aes(
    shape = "plaintext", x = 3, y = 1, fontsize = 12, width = 3.3) ) |>
  # add_edge(from = 6, to = 6, rel = "self") |># node_aes = node_aes(
    # fillcolor = "solid", color = "black", x = 1.5, y = 3)) |>
  render_graph()

  add_node(label = "negative association", node_aes = node_aes(shape = "plaintext", x = 3, y = 1)) |>
  add_node(label = "not present after NS, only with increasing PSD", node_aes = node_aes(shape = "plaintext", x = 3, y = 1)) |>
  # add_edge(from = 6, to = 6, rel = "self", style = "solid", color = "black", x_coord = 1.5, y_coord = -2) %>%
  # add_edge(from = 7, to = 7, rel = "self", style = "dotted", color = "black", x_coord = 1.5, y_coord = -2.5) %>%
  # add_edge(from = 8, to = 8, rel = "self", color = "red", x_coord = 1.5, y_coord = -3) |>
  render_graph()
  
  
create_graph() |>
  add_node(type = "sleep",label = "PSD") |> 
  add_node(type = "beh", label = "BV") |> 
  add_edge("PSD", "BV") |>
  render_graph()
