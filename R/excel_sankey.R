library(ggplot2)
library(ggalluvial)
library(reshape2)

#' Creates Sankey Diagram to show how K groups of a measure change over N intervals
#'
#' @param df KxN dataframe i.e. risk measures for K risk factors over N timepoints
#' @param with_border Include white vertical and horizontal borders between different stratum and alluvium
#' @export
excel_sankey <- function(df, with_border = TRUE) {
    title <- colnames(df)[1]
    first_year <- colnames(df)[2]
    border <- ifelse(with_border, 1, 0)

    # Explode the data from 2 dimensions to 1
    rdf <- reshape2::melt(df, id = title)
    names(rdf) <- c("risk_factor", "year", "risk")
    ggplot2::ggplot(rdf,
                    ggplot2::aes(y = rdf$risk, x = rdf$year, stratum = rdf$risk_factor, alluvium = rdf$risk_factor)) +
        # The alluvium is the ribbon between stratum
        ggalluvial::geom_alluvium(ggplot2::aes(fill = rdf$risk_factor), alpha = 1, colour = "white", size = border, width = 3/4, knot.pos = 0, decreasing = FALSE) +
        # The stratum is the rectangles between alluvium
        ggalluvial::geom_stratum(ggplot2::aes(fill = rdf$risk_factor), alpha = 1, colour = "white", size = border, width = 3/4, decreasing = FALSE) +
        # Risk values labelled on the stratum
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%0.2f", round(rdf$risk, 2))), colour = "white", size = 3, stat = "stratum", decreasing = FALSE) +
        # Risk factor (y-axis) labels
        ggplot2::geom_text(ggplot2::aes(label = ifelse(rdf$year == first_year, as.character(rdf$risk_factor),"")), nudge_x = -1.5, hjust = 0, colour = "black", size = 3, stat = "stratum", decreasing = FALSE) +
        # Select colours
        ggplot2::scale_fill_manual(values = c("#CC6655", "#CC8844", "#88BB88", "#366092", "#6699CC")) +
        # Year (x-axis) label adjusted position
        ggplot2::scale_x_discrete(position = "top") +
        # Set the title of plot
        ggplot2::ggtitle(title) +
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       axis.line = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       plot.background = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.title.y = ggplot2::element_blank(),
                       plot.title = ggplot2::element_text(hjust = 1/2),
                       axis.title.x = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(color="black", size=ggplot2::rel(1)),
                       legend.position = "none")
}
