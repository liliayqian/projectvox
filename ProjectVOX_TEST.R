# Testing 2.0
library(tidyverse)
install.packages("devtools")
devtools::install_github("r-lib/conflicted")
library(readr)
library(plotrix)
library(ggplot2)
library(plotly)
library(igraph)
library(visNetwork)

#Bring data to R
correspondence <- read_csv('C:/Users/Samuel David/Desktop/pv_Allcorrespondence_cleaned_index.csv', show_col_types = FALSE)
show (correspondence)

# Crete barplot 
barplot(correspondence$connection_type, xlab = "location")
par(mar = c(5, 4, 4, 2) + 0.1)
barplot(table(correspondence$correspondent), main="Correspondent Frequencies", xlab="", ylab="Frequency", col="skyblue", las=2)

# Create data.frame for ggplot2
correspondent_freq <- as.data.frame(table(correspondence$correspondent))

# Create plot with ggplot2
ggplot(correspondent_freq, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "white") +
  labs(title = "Correspondent Frequencies", x = "", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = rainbow(length(unique(correspondence$correspondent))))

#2nd attempt swicth X and Y axes
ggplot(correspondent_freq, aes(x = Freq, y = Var1, fill = Var1)) +
  geom_bar(stat = "identity", color = "white") +
  labs(title = "Correspondent Frequencies", x = "Frequency", y = "") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.y = element_text(hjust = 1)) +
  scale_fill_manual(values = rainbow(length(unique(correspondence$correspondent))))

# Create pie chart for 'correspondent'
pie(table(correspondence$correspondent), main="Pie Chart of 'correspondent'", col=rainbow(length(unique(correspondence$correspondent))))
correspondent_freq <- table(correspondence$correspondent)
pie(correspondent_freq, main="Pie Chart of 'correspondent'", col=rainbow(length(unique(correspondence$correspondent))), cex = 0.8)
legend("topright", legend = paste(names(correspondent_freq), "\n", scales::percent(correspondent_freq / sum(correspondent_freq))), cex = 0.8, fill = rainbow(length(unique(correspondence$correspondent))))

# pie chart 3D
pie3D(correspondent_freq, main="3D Pie Chart of 'correspondent'", col=rainbow(length(unique(correspondence$correspondent))), labels = paste(names(correspondent_freq), "\n", scales::percent(correspondent_freq / sum(correspondent_freq))), explode = 0.1)


# With GGplot
correspondent_freq <- as.data.frame(table(correspondence$correspondent))
ggplot(correspondent_freq, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Pie Chart of 'correspondent'", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = rainbow(length(unique(correspondence$correspondent))))

#Other Visualitation
plot_ly(
  labels = correspondent_freq$Var1,
  values = correspondent_freq$Freq,
  type = "pie",
  textinfo = "percent",
  textposition = "inside",
  marker = list(colors = rainbow(length(unique(correspondence$correspondent))), line = list(color = "white", width = 2))
) %>%
  layout(title = "3D Pie Chart of 'correspondent'")


##Other visualizations Network of correspondence 
View (correspondence)
# Create directed Network
correspondence_graph <- graph_from_data_frame(correspondence, directed = TRUE, vertices = NULL)
plot(correspondence_graph, 
     layout = layout_with_fr(correspondence_graph),  
     vertex.color = "lightblue",
     vertex.size = 10,
     vertex.label.dist = 2.5,
     vertex.label.cex = 1.0,
     edge.arrow.size = 0.5,
     main = "Directed Correspondence Network")

# Second attempt with more space
layout <- layout_with_fr(correspondence_graph, niter = 200, repulserad = 0.5)
plot(correspondence_graph, 
     layout = layout,  
     vertex.color = "lightblue",
     vertex.size = 9,
     vertex.label.dist = 2.5,
     vertex.label.cex = 0.8,
     edge.arrow.size = 0.5,
     main = "Directed Correspondence Network")


