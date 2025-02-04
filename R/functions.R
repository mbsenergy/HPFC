
#' Multiplot
#'
#' Internal function for plot grid
#'
#' @returns a ggplot
#' @export
#'
#'

multiplot <- function(..., plotlist = NULL, cols) {

    require(grid)

    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # Make the panel
    plotCols = cols
    plotRows = ceiling(numPlots/plotCols)

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }

}




#' Keep Columns
#'
#' data.table wrapper to keep columns
#'
#' @returns A dataframe with selected columns
#' @export
#'

kc_cols = function(DT,cols_vec) {

  set(DT, ,names(DT)[!names(DT) %in% cols_vec],NULL)

}




#' Storage qualcosa
#'
#' something
#'
#' @param type A daily dataframe with price
#' @param folder_name A daily dataframe with price
#' @param rundate A daily dataframe with price
#' @param markets A daily dataframe with price
#' @returns A dataframe with 4 columns date, smp_day, hp_trend and detr_smp_day
#' @export
#'

# File Management ----------------------------------------------------

### define new folder images where to store results (within the main output folder) named by date of computations

storage_pather = function(type = 'input', folder_name, rundate, markets) {

    if(type == 'input') {

    dir.create(file.path(folder_name, rundate), recursive = TRUE) |> suppressWarnings()

    }

    if(type == 'output') {

    dir.create(file.path(folder_name, markets, rundate), recursive = TRUE) |> suppressWarnings()

    }

}


