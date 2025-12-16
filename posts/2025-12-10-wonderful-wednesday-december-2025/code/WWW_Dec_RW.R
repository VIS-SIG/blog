library(ggplot2)

create_festive_plot <- function(to = "Recipient",
                                from = "Sender",
                                message = "Happy Holidays!",
                                image_type = c("christmas gift", "christmas tree", "snowman")) {
  
  image_type <- match.arg(image_type)
  
  # Base ggplot
  p <- ggplot() + coord_fixed() + theme_void()
  
  # Adjust limits dynamically
  if (image_type == "christmas gift") {
    p <- p + xlim(0, 10) + ylim(0, 12)
  } else if (image_type == "christmas tree") {
    p <- p + xlim(0, 10) + ylim(0, 12)
  } else if (image_type == "snowman") {
    p <- p + xlim(0, 10) + ylim(0, 12)
  }
  
  # Draw selected image
  if (image_type == "christmas gift") {
    p <- p +
      geom_rect(aes(xmin = 2, xmax = 8, ymin = 2, ymax = 6),
                fill = "#D32F2F", color = "black") +
      geom_rect(aes(xmin = 1.5, xmax = 8.5, ymin = 6, ymax = 7),
                fill = "#C62828", color = "black") +
      geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 2, ymax = 7),
                fill = "#FFD700") +
      geom_rect(aes(xmin = 1.5, xmax = 8.5, ymin = 5.5, ymax = 6.5),
                fill = "#FFD700") +
      geom_curve(aes(x = 5, y = 7, xend = 3.5, yend = 8.5),
                 curvature = -0.5, color = "#FFD700", size = 1.5) +
      geom_curve(aes(x = 5, y = 7, xend = 6.5, yend = 8.5),
                 curvature = 0.5, color = "#FFD700", size = 1.5)
    
  } else if (image_type == "christmas tree") {
    p <- p +
      # Tree layers
      geom_polygon(aes(x = c(5, 2, 8), y = c(9, 5, 5)), fill = "#2E7D32") +
      geom_polygon(aes(x = c(5, 2.5, 7.5), y = c(7, 3, 3)), fill = "#388E3C") +
      # Trunk
      geom_rect(aes(xmin = 4.5, xmax = 5.5, ymin = 2, ymax = 3), fill = "#6D4C41") +
      # Star aligned with top of tree
      geom_point(aes(x = 5, y = 9), shape = 8, size = 6, color = "#FFD700")
    
  } else if (image_type == "snowman") {
    p <- p +
      # Snowman body with black border and larger sizes
      geom_point(aes(x = 5, y = 3), size = 40, color = "black", stroke = 1, shape = 21, fill = "white") +
      geom_point(aes(x = 5, y = 5), size = 30, color = "black", stroke = 1, shape = 21, fill = "white") +
      geom_point(aes(x = 5, y = 6.8), size = 20, color = "black", stroke = 1, shape = 21, fill = "white") +
      # Eyes
      geom_point(aes(x = 4.7, y = 7), size = 1.5) +
      geom_point(aes(x = 5.3, y = 7), size = 1.5) +
      # Nose
      geom_segment(aes(x = 5, y = 6.8, xend = 5.4, yend = 6.7), color = "orange", size = 1.5) +
      # Arms
      geom_segment(aes(x = 4, y = 5.5, xend = 3, yend = 6.5), color = "brown", size = 1.5) +
      geom_segment(aes(x = 6, y = 5.5, xend = 7, yend = 6.5), color = "brown", size = 1.5)
  }
  
  # Add message text
  p <- p +
    annotate("text", x = 5, y = 11, label = message,
             size = 6, fontface = "bold", color = "#2E7D32") +
    annotate("text", x = 5, y = 10.3, label = paste("To:", to),
             size = 5, color = "#1565C0") +
    annotate("text", x = 5, y = 9.7, label = paste("From:", from),
             size = 5, color = "#1565C0")
  
  return(p)
}

# Example usage:
create_festive_plot(to = "Attendees",
                    from = "Panellists",
                    message = "Merry Christmas!",
                    image_type = "snowman")


