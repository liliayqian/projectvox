# Lilia Testing

library(tidyverse)
# ggplot
library(readr)
library(plotrix)

correspondence <- read_csv('/Users/liliaqian/Projects/projectvox/data/pv_correspondence_cleaned_index.csv', show_col_types = FALSE)

# barplot(H,xlab,ylab,main, names.arg,col)

barplot(correspondence.connection_type, xlab = "location")

par(mar = c(5, 4, 4, 2) + 0.1)
barplot(table(correspondence$correspondent), main="Correspondent Frequencies", xlab="", ylab="Frequency", col="skyblue", las=2)

# width and height
pie(table(correspondence$correspondent), main="Pie Chart of 'correspondent'", col=rainbow(length(unique(correspondence$correspondent))))

correspondent_freq <- table(correspondence$correspondent)
pie(correspondent_freq, main="Pie Chart of 'correspondent'", col=rainbow(length(unique(correspondence$correspondent))), cex = 0.8)
legend("topright", legend = paste(names(correspondent_freq), "\n", scales::percent(correspondent_freq / sum(correspondent_freq))), cex = 0.8, fill = rainbow(length(unique(correspondence$correspondent))))

correspondent_freq <- table(correspondence$correspondent)
pie3D(correspondent_freq, main="3D Pie Chart of 'correspondent'", col=rainbow(length(unique(correspondence$correspondent))), labels = paste(names(correspondent_freq), "\n", scales::percent(correspondent_freq / sum(correspondent_freq))), explode = 0.1)
