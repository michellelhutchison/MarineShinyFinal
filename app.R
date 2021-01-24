library(shiny)
library(leaflet)
library(shiny.semantic)
library(ggplot2)


#####THE MODULARIzE DROPDOWNS AND TESTTHATS FRAMEWORK WAS TAKEN FROM
##### https://gist.github.com/MarkEdmondson1234/7e56ee7ac5caa74224327489b0849e61#file-dynamicselectshinymodule-r
#####

#' Safe subset
#'
#' @param df Dataframe
#' @param column One name of column to subset within
#' @param subset Vector of entries in column to subset to
#'
#' If column not in df, returns back the df
safeSubset <- function(df, column, subset){
    
    testthat::expect_is(df, "data.frame")
    testthat::expect_is(column, "character")
    testthat::expect_equal(length(column), 1)
    
    if(!is.null(subset)){
        testthat::expect_is(subset, "character")
    } else {
        message("Subset is NULL, returning original")
        out <- df
    }
    
    message(" # subsetting # original rows: ",nrow(df) ," column:", column, " by ", paste(subset, collapse = ", "))
    
    col <- df[[column]]
    
    if(!is.null(col)){
        out <- df[col %in% subset,]
        message("Subset rows: ", nrow(out))
    } else {
        message("Column not found:", column)
        out <- df
    }
    
    out
    
}


#' Dynamical Update of a selectInput
#'
#' Shiny Module: useage details at \link{dynamicSelect}
#'
#' @param id shiny id
#'
#' @return dynamicSelectInput
#' @export
dynamicSelectInput <- function(id, label, multiple = FALSE){
    
    ns <- shiny::NS(id)
    
    # shiny::
        selectInput(ns("dynamic_select"), label,
                       choices = '', #selectize = TRUE, 
                    multiple = multiple, width = "100%")
    
}

#' Dynamical Update of a selectInput
#'
#' Shiny Module
#'
#' Use via \code{callModule(dynamicSelect, "name_select", the_data, "cyl")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param the_data data.frame containing column of choices
#' @param column The column to select from
#' @param default_select The choices to select on load
#'
#' @seealso \link{dynamicSelectInput}
#'
#' @return the_data filtered to the choice
#' @export
dynamicSelect <- function(input, output, session, the_data, column, default_select = NULL){
    
    ns <- session$ns
    
    ## update input$dynamic_select
    observe({
        shiny::validate(
            shiny::need(the_data(),"Fetching data")
        )
        dt <- the_data()
        
        testthat::expect_is(dt, "data.frame")
        testthat::expect_is(column, "character")
        
        choice <- sort(unique(dt[[column]]))
        
        updateSelectInput(session, "dynamic_select",
                          choices = choice,
                          selected = default_select)
        
    })
    
    new_data <- reactive({
        shiny::validate(
            shiny::need(input$dynamic_select,"Select data"),
            shiny::need(the_data(), "Waiting for data")
        )
        
        sd <- the_data()
        selected <- input$dynamic_select
        
        ## will return sd even if column is NULL
        safeSubset(sd, column, selected)
        
    })
    
    return(new_data)
    
}

### Useage

shinyApp(
    ui = semanticPage(
        #output leaflet
        title = 'Marine-Shiny',
        
        

        div(class = 'ui raised segment',
            div(
                a(class="ui blue ribbon label", "Note"),
                textOutput("select_text"))
        ),
        div(class = 'ui raised segment',
            div(
                # a(class="ui blue ribbon label", "Leaflet"),
                # a(class="ui green ribbon label", "Leaflet Map"),
                leafletOutput("map")
                )
            ),

        
        
        #output datatable
        div(class = 'ui raised segment',
             div(a(class="ui orange ribbon label", "Make Your Selections"),
        # dataTableOutput('ship_data'),
        dynamicSelectInput("ship_type_select", "Select Ship Type:", multiple = FALSE),
        dynamicSelectInput("ship_name_select", "Select Ship Name", multiple = FALSE)
        )),

        
        div(class = 'ui raised segment',
            a(class="ui blue ribbon label", "Ship"),
            textOutput("select_text2")
        ),
        div(class = 'ui raised segment',
            a(class="ui blue ribbon label", "Total Observations"),
            textOutput("select_text3")
        ),
        div(class = 'ui raised segment',
            a(class="ui blue ribbon label", "Ship Identifier"),
            textOutput("select_text4")
        ),
        div(class = 'ui raised segment',
            a(class="ui blue ribbon label", "Ship Names Available for each Ship Type"),
            plotOutput("plot2")
        ),
    ),
    server = function(input, output, session){
        #ESTABILISH BASE LEAFLET
        output$map<-renderLeaflet(
            leaflet() %>% 
                # addTiles()%>%
                addProviderTiles(providers$OpenStreetMap.Mapnik)%>%
                setView(20.51799,57.45748,zoom = 5) %>% ###CHANGE THIS TO CENTER ON AREA OF INTEREST
                addLegend(position = "bottomright", title = 'Ship Farthest Movement', 
                          bins = 2, colors = c('green','red'),
                          labels = c('Start','End'),opacity=1))

        #READ IN REACTIVE ShIP DATA
        the_data <- reactive({
            ships<-read.csv('./Data/ships_agg_data.csv',stringsAsFactors = FALSE)
        })
        
        
        ####GGPLOT SHIP TYPE TO SHIPNAMES AVAILABLE
        output$plot2<-renderPlot({
            ggplot(the_data(),
                   aes(x=ship_type))+geom_bar(stat='count',color='blue')
            })




        ###CALL FILTER FROM SHIPTYPE SELECTION
        ship_type_filter <- shiny::callModule(dynamicSelect, "ship_type_select", the_data, "ship_type", default_select = '')
        ## CALL FILTER FROM SHIPTYPE FILTER
        ship_name_filter <- shiny::callModule(dynamicSelect, "ship_name_select", ship_type_filter, "SHIPNAME", default_select = '')
        
        
        output$select_text <- renderText({ 
            paste("The maximum travel in between signals is about", 
                  round(ship_name_filter()$distance_meters)," meters.")
        })
        
        output$select_text2 <- renderText({ 
            ship_name_filter()$SHIPNAME
        })
        output$select_text3 <- renderText({ 
            ship_name_filter()$points
        })
        output$select_text4 <- renderText({
            ship_name_filter()$SHIP_ID
        })
        
        #updates map
        observe({
            iconsStart <- awesomeIcons(
                icon = 'ship',
                iconColor = 'black',
                library = 'fa',
                markerColor = 'green'
            )
            iconsEnd <- awesomeIcons(
                icon = 'ship',
                iconColor = 'black',
                library = 'fa',
                markerColor = 'red'
            )
         leafletProxy("map",data=ship_name_filter()) %>%
             clearMarkers() %>%
             addAwesomeMarkers( lng=ship_name_filter()$LON,lat=ship_name_filter()$LAT, icon = iconsStart, 
                                popup = paste(ship_name_filter()$SHIPNAME, ' ', ship_name_filter()$SHIP_ID))%>%
             addAwesomeMarkers(lng=ship_name_filter()$dest_lon,lat=ship_name_filter()$dest_lat, icon = iconsEnd,
                               popup = paste(ship_name_filter()$SHIPNAME, ' ', ship_name_filter()$SHIP_ID))%>%
             fitBounds(max(ship_name_filter()$LON),
                       max(ship_name_filter()$LAT),
                       min(ship_name_filter()$dest_lon),
                       min(ship_name_filter()$dest_lat))
        })
    }
)

