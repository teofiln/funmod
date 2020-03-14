#' Function to module
#'
#' @description Convert a function into a shiny module.
#'
#' @details The arguments of the function become input widgets. The output of the function is passed onto the appropriate `render*` function: `renderPlot` for plot, `renderTable` for tables and `renderText` for text.
#'
#' @param function The name of an R function to be converted into a module
#'
#' @return Shiny module components, UI and server, as functions in `.GlobalEnv`. The module is also returned as text and can be captured in a variable.
#'
#' @examples
#'
#' # First we define a function
#' sum_ <- function(x, y) x + y
#'
#' # Second we convert to module
#' mod <- fun2mod(fun = "sum_")
#'
#' # Third we launch a shiny app with this module
#' library(shiny)
#'
#'ui <- fluidPage(
#'  sum__ui(id = "A")
#')
#'
#'server <- function(input, output, session) {
#'  callModule(sum__srv, id = "A")
#'}
#'
#'shinyApp(ui, server)
#'
#'@export
#'

fun2mod <- function(fun, assign_to_global_env = TRUE) {
  stopifnot(class(fun) == "character")

  .ui_name <- paste0(fun, "_ui")
  .srv_name <- paste0(fun, "_srv")
  .widget_names <- formalArgs(fun)

  .inputs <-
    glue::glue_collapse(sep = ",\n",
                        x = lapply(.widget_names,
                                   function(x) {
                                     paste("numericInput( ns('",x, "'), '", x, "', 10, 0, 100, 1)", sep = "")
                                   }))

  .ui <- glue::glue(
  "{.ui_name} <- function(id) {{
      ns <- NS(id)
      tagList(\n{.inputs},
              textOutput(ns('Sum'))
        )}}"
  )

  .server <- glue::glue(
  "{.srv_name} <- function(input, output, session){{
    x <- reactiveVal()
    observe({{
      y <- sum_(x=input[['{.widget_names[1]}']], y=input[['{.widget_names[2]}']])
      x(y)
    }})
    output$Sum <- renderText({{ x() }})
    x
      }}"
  )

  .mod <- glue::glue("{.ui}\n\n {.server}", .sep = "\n")

  if(assign_to_global_env) {
    assign(x = .ui_name, value = eval(parse(text = .ui)),envir = .GlobalEnv)
    assign(x = .srv_name, value = eval(parse(text = .server)), envir = .GlobalEnv)
  }

  return(list(
    mod = .mod,
    mod_ui = eval(parse(text = .ui)),
    mod_srv = eval(parse(text = .server))
  ))
}
