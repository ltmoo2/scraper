library(shiny)
library(rvest)
library(dplyr)
library(zoo)
library(stringr)
library(tidyr)
library(purrr)
library(lubridate)
library(readr)
library(shinycssloaders)
library(writexl)

ui <- fluidPage(
    
    
    titlePanel("Real Commercial Web Scraper"),
    
    
    sidebarLayout(
        sidebarPanel(
            textInput("url",
                      "Paste URL"),
            dateInput("date",
                      "Select Date of Last Scrape",
                      value = Sys.Date()),
            actionButton("create",
                         "Scrape"),
            downloadButton("download", "Download Data")
        ),
        
        
        mainPanel(
            dataTableOutput("test") %>%
                withSpinner(color="#0dc5c1")
        )
    )
)


server <- function(input, output) {
    
    
    pages <- reactive({
        
        req(input$create)
        
        pages <- paste0(input$url, "&page=", c(1:25))
    })
    
    final_table <- reactive({
        
        req(input$create)
        
        final_table <- data.frame()
        
        links_final <- data.frame()
        
        
        for(i in unique(pages())){
            
            webpage <- read_html(i)
            
            links <- html_nodes(webpage, 'a')%>%
                html_attr("href")
            
            links_df <- as_tibble(links) 
            
            links_clean <- links_df %>%
                filter(str_detect(value, "leased/property")) %>%
                distinct() %>%
                mutate(value = paste0("https://www.realcommercial.com.au", value))%>%
                mutate("id" = row_number())
            
            for(i in unique(links_clean$id)){
                
                page <- links_clean %>%
                    filter(id == i)
                
                result <- read_html(page$value)
                
                address <- html_nodes(result, "h1") %>%
                    html_text() %>%
                    as_tibble() %>%
                    rename("Address" = value)
                
                prop_type <- html_nodes(result, ".PrimaryDetailsTop_propertyTypes_1mGFK") %>%
                    html_text() %>%
                    as_tibble() %>%
                    rename("Prop_type" = value)
                
                attr_desc <- html_nodes(result, ".Attribute_label_1bYjg") %>%
                    html_text() %>%
                    as_tibble() %>%
                    rename("attr_desc" = value)
                
                attr_val <- html_nodes(result, ".Attribute_value_i8Dee") %>%
                    html_text() %>%
                    as_tibble()
                
                attr <- attr_desc %>%
                    bind_cols(attr_val) %>%
                    pivot_wider(names_from = attr_desc, values_from = value)
                
                desc <- html_nodes(result, ".DescriptionPanel_description_20faq") %>%
                    html_text() %>%
                    as_tibble() %>%
                    rename("Description" = value)
                
                agency <- html_nodes(result, ".AgencyPanel_agencyNameLink_nCd-h")%>%
                    html_text() %>%
                    as_tibble() %>%
                    top_n(1) %>%
                    rename("Agency" = value)
                
                agent <- html_nodes(result, ".AgentDetails_name_23QWU") %>%
                    html_text() %>%
                    as_tibble() %>%
                    top_n(1) %>%
                    rename("Agent" = value)
                
                property <- bind_cols(address, prop_type, attr, desc, agency, agent) %>%
                    mutate("Source" = paste(page$value)) %>%
                    mutate("Leased on" = dmy(`Leased on`))
                
                if(property$`Leased on` < input$date)
                {
                    breaker <-  TRUE
                    
                    break
                }
                
                else if(property$`Leased on` >= input$date)
                {
                    breaker <- FALSE
                    
                    final_table <- bind_rows(final_table, property)
                }
                
            }
            
            if(breaker == TRUE)
            {
                break
            }
            
        }
        
        final_table <- final_table  %>%
            mutate("Postcode" = substr(Address, nchar(Address)-4, nchar(Address))) %>%
            mutate("Address" = substr(Address, 1, nchar(Address)-10))
        
        final_table <- final_table %>%
            mutate("Address" = str_split(Address, ",\\s*(?=[^,]+$)")) %>%
            mutate("Suburb" = map_chr(Address, 2)) %>%
            mutate("Address" = map_chr(Address, 1))
        
        final_table <- final_table %>%
            separate(Address, c("Address", "Street_Type"), sep = " (?=[^ ]*$)", remove = FALSE) %>%
            separate(Address, c("Address", "Street_Name"), sep = " (?=[^ ]*$)", remove = FALSE) %>%
            select(Address, Street_Name, Street_Type, Suburb, Postcode, Prop_type, `Floor area`, `Leased on`:Source) %>%
            mutate("Floor area" = substr(`Floor area`, 1, nchar(`Floor area`)-3))
        
        
        return(final_table)
    })
    
    
    
    output$test <- renderDataTable({final_table()},
                                   options = list(pageLength = 25, info = FALSE,
                                                  lengthMenu = list(c(25, -1), c("25", "All"))))
    
    output$download <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), " - Scrape.xlsx")
        },
        content = function(file) {
            write_xlsx(final_table(), file)
        }
    )
    
    
    
}


shinyApp(ui = ui, server = server)
