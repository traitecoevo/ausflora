library(hexSticker)

imgurl <- file.path("inst/figures/APCalign_v2.png")
sticker(
  imgurl, 
  package=" ", 
  # p_size = 30,
  # p_x = 1, p_y = 0.70,
  s_x=1, s_y=1.1, 
  s_width=0.85, s_height = 0.85,
  # p_color = "goldenrod4",
  h_color = "#B38E21",
  h_fill = "#FBF8C7",
  filename="man/figures/APCalign_hex_2.png"
  )

