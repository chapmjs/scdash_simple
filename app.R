# Load required libraries
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# Initialize renv (uncomment when setting up the project)
# renv::init()

# Define the data
inventory_data <- data.frame(
  `Part Number` = c("E2081-66555", "E2081-66556", "E2084-66501", "E2084-66508", "E2084-66522", 
                    "E2084-66523", "E2081-61116", "E2081-61120", "E2084-67600", "E2084-61101", 
                    "E2084-64701", "E2084-64401", "E2084-00102", "E2084-64400", "E2084-66203", 
                    "E2084-66200", "E2084-67907", "E2084-67920", "E3065-66504", "E3065-66505", 
                    "E3065-66519", "E3065-66517", "E2084-67960", "E3065-64701", "E3065-67901", 
                    "E2084-04107", "E2084-04108", "E3065-66513", "E2085-66520", "E3066-66514", 
                    "E2084-67927"),
  Description = c("HV Driver", "Waveplate 1\"", "Motorized Flipper", "Mounting pedistal", "Holding Fork", 
                  "90 degree prisms", "Prism Coating", "Prism Mount", "Translation stage", "Picomotor actuator", 
                  "Picomotor driver", "Window (FS coated)", "Bare Vacuum Flange", "Vacuum Pump", "Vacuum Pump Valve", 
                  "Mirror", "Mount claw", "Mounting pedistal", "Holding Fork", "Picomotor Servo", 
                  "Tilting Mirror", "Mirror Driver crate", "Mirror Driver card", "Mirror Readback Card", "Lens", 
                  "Optical Table", "Laser Diode", "Hyphenator Duphenator", "LH Smokeshifter", "Bracket", 
                  "Injection Pump"),
  Supplier = c("McDonalds", "McDonalds", "McDonalds", "McDonalds", "Wendys", 
               "Wendys", "Quiznos", "Quiznos", "Quiznos", "Quiznos", 
               "Taco Time", "KFC", "KFC", "Subway", "IHOP", 
               "IHOP", "Sizzler", "Quiznos", "Wendys", "Wendys", 
               "McDonalds", "Wendys", "Sizzler", "Taco Time", "Panda Express", 
               "BYU-I Food Services", "BYU-I Food Services", "Wendys", "McDonalds", "McDonalds", 
               "Sizzler"),
  Yield = c(99.0, 99.0, 99.7, 99.0, 99.0, 
            99.0, 99.0, 99.0, 99.0, 98.0, 
            100.0, 99.0, 99.0, 100.0, 100.0, 
            100.0, 100.0, 100.0, 99.0, 99.0, 
            100.0, 99.0, 95.0, 100.0, 100.0, 
            100.0, 100.0, 95.0, 100.0, 100.0, 
            100.0),
  `CM #1 Inventory` = c(107, 158, 1379, 168, 95, 
                        66, 161, 279, 34, 1187, 
                        13, 30, 16, 30, 123, 
                        36, 8, 283, 711, 467, 
                        102, 520, 375, 5, 659, 
                        1355, 622, 438, 0, 0, 
                        9),
  `CM #2 Inventory` = c(65, 55, 1694, 88, 51, 
                        12, 40, 73, 11, 320, 
                        4, 23, 10, 8, 34, 
                        11, 8, 372, 440, 81, 
                        16, 241, 72, 0, 196, 
                        233, 100, 183, 0, 0, 
                        0),
  `OEM Inventory` = c(14, 14, 252, NA, 7, 
                      8, 7, 14, 2, 63, 
                      2, 2, 2, 2, 7, 
                      2, 1, NA, 36, 27, 
                      7, 63, NA, NA, 36, 
                      NA, NA, 63, NA, NA, 
                      1),
  Index = 1:31
)

# UI
ui <- fluidPage(
  titlePanel("Parts Inventory Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("part", "Select Part Number:", 
                  choices = paste(inventory_data$Index, inventory_data$`Part Number`, "-", 
                                  inventory_data$Description)),
      hr(),
      h4("Part Details"),
      verbatimTextOutput("details"),
      hr(),
      h4("Inventory Levels"),
      plotOutput("inventoryPlot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("All Parts", 
                 DT::dataTableOutput("table")),
        tabPanel("Supplier Analysis", 
                 h4("Parts by Supplier"),
                 plotOutput("supplierPlot"),
                 h4("Yield by Supplier"),
                 plotOutput("yieldPlot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Get selected part data
  selected_part <- reactive({
    part_index <- as.numeric(strsplit(input$part, " ")[[1]][1])
    inventory_data[inventory_data$Index == part_index, ]
  })
  
  # Part details output
  output$details <- renderPrint({
    part <- selected_part()
    cat("Part Number: ", part$`Part Number`, "\n")
    cat("Description: ", part$Description, "\n")
    cat("Supplier: ", part$Supplier, "\n")
    cat("Yield: ", part$Yield, "%\n")
  })
  
  # Inventory plot
  output$inventoryPlot <- renderPlot({
    part <- selected_part()
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Location = c("CM #1", "CM #2", "OEM"),
      Inventory = c(part$`CM #1 Inventory`, part$`CM #2 Inventory`, part$`OEM Inventory`)
    )
    
    ggplot(plot_data, aes(x = Location, y = Inventory, fill = Location)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Inventory), vjust = -0.5) +
      theme_minimal() +
      labs(title = paste("Inventory Levels for", part$`Part Number`),
           y = "Quantity", x = "") +
      theme(legend.position = "none")
  })
  
  # Complete table
  output$table <- DT::renderDataTable({
    DT::datatable(inventory_data[, c("Index", "Part Number", "Description", "Supplier", 
                                     "Yield", "CM #1 Inventory", "CM #2 Inventory", "OEM Inventory")],
                  options = list(pageLength = 15))
  })
  
  # Supplier Analysis
  output$supplierPlot <- renderPlot({
    supplier_counts <- inventory_data %>%
      group_by(Supplier) %>%
      summarize(Count = n()) %>%
      arrange(desc(Count))
    
    ggplot(supplier_counts, aes(x = reorder(Supplier, -Count), y = Count, fill = Supplier)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Supplier", y = "Number of Parts") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$yieldPlot <- renderPlot({
    yield_by_supplier <- inventory_data %>%
      group_by(Supplier) %>%
      summarize(AvgYield = mean(Yield)) %>%
      arrange(desc(AvgYield))
    
    ggplot(yield_by_supplier, aes(x = reorder(Supplier, -AvgYield), y = AvgYield, fill = Supplier)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Supplier", y = "Average Yield (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(90, 100))
  })
}

# Run the app
shinyApp(ui = ui, server = server)