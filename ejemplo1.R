# *********RECUERDA COMENTAR LA LINEA DE ABAJO CUANDO PUBLIQUES LA APP!!!!!!
setwd("D:/Sergio/1.- Subdirección de Investigación Económica/1.- Proyectos/2023 5.-Shiny/app_2")

library(siebanxicor)
library(ggplot2)
#library(r2excel)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(openxlsx)
library(zoo)
#install.packages("devtools")
#install.packages("tidy")
#library(devtools)
#install_github("kassambara/r2excel")


#Open the information of  2021, 2022  and 2023 of fruit and vegetables the weekly prices
# Load the openxlsx package
# Replace 'your_file.xlsx' with the actual path to your Excel file
excel_file <- "datos_reporte_frutas.xlsx"
# Get the list of sheet names
sheet_names <- getSheetNames(excel_file)

# Create an empty list to store data frames
data_list <- list()

# Loop through each sheet name and read data
for (sheet_name in sheet_names) {
  data <- read.xlsx(excel_file, sheet = sheet_name)
  # Store the data in a variable with a dynamic name
  var_name <- paste0("data_", sheet_name)  # Create variable name like data_Sheet1
  assign(var_name, data)  # Store the data frame in the variable
  data
  data_list[[sheet_name]] <- data
}



# long_data <- pivot_longer(data_2021,
#                           cols = -cadena, # ID is the column that will remain as identifier
#                           names_to = "semana",
#                           values_to = "value"


# List of data frames you want to convert to long format
data_frames <- list(data_2021, data_2022, data_2023)  # Add more if needed

# Create an empty list to store the long data frames
long_data_by_year <- list()
j=2021
# Loop through each data frame, convert to long format, and store in the list
for (i in seq_along(data_frames)) {
  long_data <- pivot_longer(data_frames[[i]],
                            cols = -cadena,  # Replace 'ID' with the identifier column
                            names_to = "semana",
                            values_to = "value")
  # Rename the 'value' column based on the year
  colnames(long_data)[colnames(long_data) == "value"] <- paste0("value_", j)
  long_data$semana <- as.numeric(long_data$semana)
  # Create a variable name dynamically
  var_name <- paste0("long_data_", j )
  # Assign the long data frame to the dynamic variable name
  assign(var_name, long_data)
  j=j+1
  print(j)
  
  long_data_by_year[[i]] <- long_data
}

#Merge 2021, 2022 2023
unique_values <- unique(long_data_2021$cadena)
long_data_all <- merge(long_data_2021, long_data_2022, by = c("cadena", "semana"), all = TRUE)
long_data_all <- merge(long_data_all, long_data_2023, by = c("cadena", "semana"), all = TRUE)
# Print the merged data
long_data_all <- long_data_all %>% arrange(cadena, semana)
print(long_data_all)

#Global Data : Advance of agricultural production
avance_agricola <- read.csv("Agricola_Mensual_Acumulado.csv", fileEncoding = "latin1")
avance_agricola$PerenneDummy <- ifelse(avance_agricola$CICLO == "Perenne", 1, 0)
colnames(avance_agricola)
#Advance of agricultural production data
produccion<-aggregate(cbind(PRODUCCION) ~ CULTIVO +  PerenneDummy +AÑO + MES , data = avance_agricola, FUN = "sum")
# Sort the data by CULTIVO, AÑO, and MES
produccion<- produccion[order(produccion$CULTIVO, produccion$AÑO, produccion$MES),]
produccion$PRODUCCION_diff <- ave(produccion$PRODUCCION, produccion$CULTIVO, FUN = function(x) c(NA, diff(x)))
# Check for NAs in PRODUCCION_diff
nas_indices <- is.na(produccion$PRODUCCION_diff)
produccion <- produccion %>%
  mutate(PRODUCCION_diff = case_when(PRODUCCION_diff < 0 & !is.na(PRODUCCION_diff) ~ PRODUCCION,
                                     TRUE ~ PRODUCCION_diff))
produccion$PRODUCCION_diff[is.na(produccion$PRODUCCION_diff) & produccion$PerenneDummy == 1] <- produccion$PRODUCCION[is.na(produccion$PRODUCCION_diff) & produccion$PerenneDummy == 1]
# Sort the data by CULTIVO, AÑO, and MES
produccion<- produccion[order(produccion$CULTIVO, produccion$AÑO, produccion$MES),]

tomate_rojo <- produccion[produccion$CULTIVO == "TOMATE ROJO (JITOMATE)", ]
aguacate <- produccion[produccion$CULTIVO == "AGUACATE", ]

colnames(produccion)
#Transform production to the graph.
#Merge 2021, 2022 2023
unique_values2 <- as.data.frame(unique(produccion$CULTIVO))

#Include information of chain and the agricultural year
produccion_ciclo<-aggregate(cbind(PRODUCCION) ~ CULTIVO +  PerenneDummy + AÑO +AÑO_CICLO_AGRICOLA + MES , data = avance_agricola, FUN = "sum")
# Sort the data by CULTIVO, AÑO, and MES
produccion_ciclo<- produccion_ciclo[order(produccion_ciclo$CULTIVO, produccion_ciclo$AÑO, produccion_ciclo$MES,  produccion_ciclo$AÑO_CICLO_AGRICOLA ),]
produccion_ciclo$PRODUCCION_diff <- ave(produccion_ciclo$PRODUCCION, produccion_ciclo$CULTIVO, FUN = function(x) c(NA, diff(x)))
produccion_ciclo_inicio<-produccion_ciclo[produccion_ciclo$PerenneDummy == 0, ]
produccion_ciclo_inicio<-produccion_ciclo[produccion_ciclo$MES == 4, ]
names(produccion_ciclo_inicio)[names(produccion_ciclo_inicio) == "PRODUCCION_diff"] <- "PRODUCCION_diff2"
produccion_ciclo_inicio <- produccion_ciclo_inicio[, !(names(produccion_ciclo_inicio) %in% c("AÑO_CICLO_AGRICOLA", "PRODUCCION"))]
#tomate_rojo_ciclo<-produccion_ciclo[produccion_ciclo$CULTIVO == "TOMATE ROJO (JITOMATE)", ]
#tomate_rojo_ciclo_abr<-tomate_rojo_ciclo[tomate_rojo_ciclo$MES == 4, ]
#Merge information of year production and the initializacion of the cycle

produccion_merge <- produccion %>%
  left_join(produccion_ciclo_inicio, by = c("CULTIVO","PerenneDummy", "AÑO", "MES")) %>%
  mutate(PRODUCCION_diff = ifelse(!is.na(PRODUCCION_diff2), PRODUCCION_diff2, PRODUCCION_diff)) %>%
  select(-PRODUCCION_diff2)
print(produccion_merge)

# Function to calculate the mean of the last 5 observations excluding the last year
mean_last5_exclude_last_year <- function(x) {
  non_na_values <- na.omit(tail(x[1:(length(x)-1)], 5))  # Exclude the last year
  if (length(non_na_values) == 0) {
    NA
  } else {
    sum(non_na_values)/5
  }
}


# Calculate the grouped mean of the last 5 observations for PRODUCCION excluding the last year
produccion_merge <- produccion_merge %>%
  group_by(CULTIVO, MES) %>%
  mutate(PRODUCCION_avg_last5_exclude_last_year = mean_last5_exclude_last_year(PRODUCCION_diff))
#write.csv(produccion_merge, "prod_merge.csv", row.names = FALSE)

#create a vector with items of interest
# Create your vector
cultivos <- c(
  "TOMATE ROJO (JITOMATE)", "PAPA", "CHILE VERDE", "CEBOLLA", "CALABACITA", "ZANAHORIA",
  "TOMATE VERDE",  "MELON", "SANDIA","GUAYABA","PLATANO","NARANJA",
  "LIMON", "AGUACATE","PAPAYA", "PIÑA", "MANZANA"
  #Faltan agregar los granos
  
)

max_year<-as.numeric(format(Sys.Date(), "%Y"))
min_year<-max_year-2
# Use the subset function to filter the 'produccion' dataframe
produccion_filtered <- subset(produccion_merge, CULTIVO %in% cultivos & AÑO >= min_year & AÑO <= max_year)

# Print the filtered DataFrame
print(produccion_filtered)
tomate_rojo <- produccion_filtered[produccion_filtered$CULTIVO == "TOMATE ROJO (JITOMATE)", ]
aguacate <- produccion_filtered[produccion_filtered$CULTIVO == "AGUACATE", ]
papaya <- produccion_filtered[produccion_filtered$CULTIVO == "PAPAYA", ]


produccion_wide <- produccion_filtered %>%
  select(CULTIVO, MES, AÑO, PRODUCCION_diff, PRODUCCION_avg_last5_exclude_last_year)
# Pivot the DataFrame to wide format
produccion_wide <- pivot_wider(produccion_wide, names_from = AÑO, values_from = c(PRODUCCION_diff, PRODUCCION_avg_last5_exclude_last_year))
# Remove the last two columns
produccion_wide <- produccion_wide[, -c(ncol(produccion_wide) - 1, ncol(produccion_wide))]

# Print the transformed DataFrame
print(produccion_wide)
print(unique(produccion_wide$CULTIVO))

# Assuming your data frame is named produccion
produccion_wide$CULTIVO <- tools::toTitleCase(tolower(produccion_wide$CULTIVO))

# Convert "mes" to a factor with custom levels
produccion_wide$MES <- factor(produccion_wide$MES, levels = 1:12, labels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"))

# 
# tomate_rojo <- produccion_wide[produccion_wide$CULTIVO == "TOMATE ROJO (JITOMATE)", ]
tomate_rojo <- produccion_wide[produccion_wide$CULTIVO == "Tomate Rojo (Jitomate)", ]
chile_verde <- produccion_wide[produccion_wide$CULTIVO == "Chile verde", ]

# Recode the 'mes' variable to 'mes2'
produccion_wide2 <- produccion_wide %>% mutate(MES2 = recode(MES,
                                                             `1`  = "ene",
                                                             `2`  = "feb",
                                                             `3`  = "mar",
                                                             `4`  = "abr",
                                                             `5`  = "may",
                                                             `6`  = "jun",
                                                             `7`  = "jul",
                                                             `8`  = "ago",
                                                             `9`  = "sep",
                                                             `10` = "oct",
                                                             `11` = "nov",
                                                             `12` = "dic"))


# Specify the path to your Excel file
excel_file_path <- "diccioonario.xlsx"
# Read the Excel file
cadena_cultivo_data <- read.xlsx(excel_file_path, sheet = 1)
# Set column names
#colnames(cadena_cultivo_data) <- c("cadena", "CULTIVO")
# Create a dictionary
cadena_dict <- setNames(cadena_cultivo_data$cadena, cadena_cultivo_data$CULTIVO )
# Print the dictionary
print(cadena_dict)
# # Access the associated cadena value
# first_cadena_value <- cadena_dict[[1]]
# # Access the first value of cultivo
# associated_value <- names(cadena_dict)[1]
# # Print the results
# print(first_cadena_value)
# print(associated_value)
# # Sample data
# cadena_cultivo_data <- data.frame(
#   cadena = c("Tomate rojo saladette", "Papa", "Chile verde"),
#   CULTIVO = c("Tomate Saladette", "Papa Alpha", "Chile Jalapeño")
# )
# 
# # Create the dictionary
# cadena_dict <- setNames(cadena_cultivo_data$cadena, cadena_cultivo_data$CULTIVO)
# 
# # Print the dictionary
# print(cadena_dict)
# 
# # Search for the name of CULTIVO when cadena is "Tomate rojo saladette"
# search_cadena <- "Tomate rojo saladette"
# found_cultivo <- names(cadena_dict)[which(cadena_dict == search_cadena)]
# 
# # Print the result
# print(found_cultivo)


# 
# print(as.character(cadena_dict[[1]]))
# selected_cadena <- as.character(cadena_dict[[1]])
# print(selected_cadena)
# found_cultivo <- names(cadena_dict)[cadena_dict == selected_cadena]
# print(found_cultivo)
# print(as.character(found_cultivo))
# data_2 <- produccion_wide2 %>%
#   filter(str_trim(CULTIVO) == str_trim(found_cultivo))
# print(data_2)  # Add a print statement to check the filtered data

#----------------------------------------------------------------------------------------------------
# SECCCION DE PRODUCCION ESTATAL
#Advance of agricultural production data
produccion_estatal<-aggregate(cbind(PRODUCCION) ~ CULTIVO + ESTADO +  PerenneDummy +AÑO + MES , data = avance_agricola, FUN = "sum")
# Sort the data by CULTIVO, AÑO, and MES
produccion_estatal<- produccion_estatal[order(produccion_estatal$CULTIVO, produccion_estatal$ESTADO, produccion_estatal$AÑO, produccion_estatal$MES),]
produccion_estatal <- produccion_estatal %>%
  group_by(CULTIVO, ESTADO, AÑO) %>%
  mutate(PRODUCCION_diff = (PRODUCCION - lag(PRODUCCION)))

# Check for NAs in PRODUCCION_diff
nas_indices <- is.na(produccion_estatal$PRODUCCION_diff)
produccion_estatal <- produccion_estatal %>%
  mutate(PRODUCCION_diff = case_when(PRODUCCION_diff < 0 & !is.na(PRODUCCION_diff) ~ PRODUCCION,
                                     TRUE ~ PRODUCCION_diff))

produccion_estatal$PRODUCCION_diff[is.na(produccion_estatal$PRODUCCION_diff) & produccion_estatal$PerenneDummy == 1] <- produccion_estatal$PRODUCCION[is.na(produccion_estatal$PRODUCCION_diff) & produccion_estatal$PerenneDummy == 1]
# Sort the data by CULTIVO, AÑO, and MES
produccion_estatal<- produccion_estatal[order(produccion_estatal$CULTIVO, produccion_estatal$ESTADO, produccion_estatal$AÑO, produccion_estatal$MES),]

tomate_rojo2 <- produccion_estatal[produccion_estatal$CULTIVO == "TOMATE ROJO (JITOMATE)", ]
aguacate2 <- produccion_estatal[produccion_estatal$CULTIVO == "AGUACATE", ]

colnames(produccion_estatal)

#Include information of chain and the agricultural year
produccion_ciclo_estatal<-aggregate(cbind(PRODUCCION) ~ CULTIVO + ESTADO +  PerenneDummy + AÑO +AÑO_CICLO_AGRICOLA + MES , data = avance_agricola, FUN = "sum")
# Sort the data by CULTIVO, AÑO, and MES
produccion_ciclo_estatal<- produccion_ciclo_estatal[order(produccion_ciclo_estatal$CULTIVO, produccion_ciclo_estatal$ESTADO, produccion_ciclo_estatal$AÑO, produccion_ciclo_estatal$MES,  produccion_ciclo_estatal$AÑO_CICLO_AGRICOLA ),]

produccion_ciclo_estatal <- produccion_ciclo_estatal %>%
  mutate(PRODUCCION_diff = case_when( PRODUCCION < 0 & !is.na(PRODUCCION) & AÑO==AÑO_CICLO_AGRICOLA  ~ PRODUCCION,
                                      TRUE ~ PRODUCCION
  ))


##HASTA AQUI AQUI EN ADELANTE BUSCO EL PERIODO dEL 4to mes y se lo imputo a la información ESATAL
#produccion_ciclo_estatal$PRODUCCION_diff <- ave(produccion_ciclo_estatal$PRODUCCION, produccion_ciclo_estatal$CULTIVO, produccion_ciclo_estatal$ESTADO, FUN = function(x) c(NA, diff(x)))
produccion_ciclo_inicio_estatal<-produccion_ciclo_estatal[produccion_ciclo_estatal$PerenneDummy == 0, ]
produccion_ciclo_inicio_estatal<-produccion_ciclo_inicio_estatal[produccion_ciclo_inicio_estatal$MES == 4, ]
#Ordenar
produccion_ciclo_inicio_estatal<- produccion_ciclo_inicio_estatal[order(produccion_ciclo_inicio_estatal$CULTIVO, 
                                                                        produccion_ciclo_inicio_estatal$ESTADO, 
                                                                        produccion_ciclo_inicio_estatal$AÑO, 
                                                                        produccion_ciclo_inicio_estatal$MES),]
names(produccion_ciclo_inicio_estatal)[names(produccion_ciclo_inicio_estatal) == "PRODUCCION_diff"] <- "PRODUCCION_diff2"
produccion_ciclo_inicio_estatal <- produccion_ciclo_inicio_estatal[, !(names(produccion_ciclo_inicio_estatal) %in% c("AÑO_CICLO_AGRICOLA", "PRODUCCION"))]
#tomate_rojo_ciclo<-produccion_ciclo[produccion_ciclo$CULTIVO == "TOMATE ROJO (JITOMATE)", ]
#tomate_rojo_ciclo_abr<-tomate_rojo_ciclo[tomate_rojo_ciclo$MES == 4, ]
#Merge information of year production and the initializacion of the cycle

#SE LO PEGO
produccion_merge_estatal <- produccion_estatal %>%
  left_join(produccion_ciclo_inicio_estatal, by = c("CULTIVO","ESTADO","PerenneDummy", "AÑO", "MES")) %>%
  mutate(PRODUCCION_diff = ifelse(!is.na(PRODUCCION_diff2), PRODUCCION_diff2, PRODUCCION_diff)) %>%
  select(-PRODUCCION_diff2)
print(produccion_merge_estatal)

# Use the subset function to filter the 'produccion' dataframe
produccion_filtered_estatal <- subset(produccion_merge_estatal, CULTIVO %in% cultivos & AÑO >= min_year-1 & AÑO <= max_year-1)

#Producción SUM of the last three years
produccion_filtered_estatal_sum <- aggregate(cbind(PRODUCCION_diff) ~ CULTIVO + ESTADO  + MES  , data = produccion_filtered_estatal, FUN = "sum")
produccion_filtered_estatal_sum <- produccion_filtered_estatal_sum[order(produccion_filtered_estatal_sum$CULTIVO, 
                                                                         produccion_filtered_estatal_sum$ESTADO, 
                                                                         produccion_filtered_estatal_sum$MES),]

#Comprobacióncon tomate
tomate_rojo2 <- produccion_filtered_estatal_sum[produccion_filtered_estatal_sum$CULTIVO == "TOMATE ROJO (JITOMATE)", ]
tomate_rojo_compro<-aggregate(cbind(PRODUCCION_diff) ~ CULTIVO +  PerenneDummy + AÑO + MES , data = tomate_rojo2, FUN = "sum")
tomate_rojo_compro <- tomate_rojo_compro[order(tomate_rojo_compro$CULTIVO, 
                                               tomate_rojo_compro$AÑO, 
                                               tomate_rojo_compro$MES),]

aguacate <- produccion_filtered_estatal[produccion_filtered_estatal$CULTIVO == "AGUACATE", ]
papaya <- produccion_filtered_estatal[produccion_filtered_estatal$CULTIVO == "PAPAYA", ]



#AQUI ES DONDE DEBERIA DE TRANSFORMAR LOS DATOS y QUE QUEDEN COMO datos_tercera
produccion_wide_estatal <- produccion_filtered_estatal_sum %>%
  select(CULTIVO, ESTADO, MES,  PRODUCCION_diff)
# Pivot the DataFrame to wide format
produccion_wide_estatal <- pivot_wider(produccion_wide_estatal, names_from = MES, values_from = PRODUCCION_diff,names_prefix = "MES_")

#Make a total column
produccion_wide_estatal <- produccion_wide_estatal %>%
  mutate(Total_MES = rowSums(select(., starts_with("MES_")), na.rm = TRUE))

# Print the transformed DataFrame
print(produccion_wide_estatal)
print(unique(produccion_wide_estatal$CULTIVO))

# Assuming your data frame is named produccion
produccion_wide_estatal$CULTIVO <- tools::toTitleCase(tolower(produccion_wide_estatal$CULTIVO))

# Convert "mes" to a factor with custom levels
#produccion_wide$MES <- factor(produccion_wide$MES, levels = 1:12, labels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"))
#########################################################################################################
#EJEMPLO
library(ggplot2)
library(tidyverse)

#get rid of the 'NA' data in the dataset
#noNuseR2016 <- useR2016 %>% dplyr::filter(Q3 != 'NA'&Q11 != 'NA')


# 
# print(as.character(cadena_dict[[1]]))
# selected_cadena <- as.character(cadena_dict[[1]])
# print(selected_cadena)
# found_cultivo <- names(cadena_dict)[cadena_dict == selected_cadena]
# print(found_cultivo)
# print(as.character(found_cultivo))
# data_2 <- produccion_wide2 %>%
#   filter(str_trim(CULTIVO) == str_trim(found_cultivo))
# print(data_2)  # Add a print statement to check the filtered data

# 
# 
# # Plot the basic frame of the stacked bar chart.
# month_gr<-as.character(month(Sys.Date()))
# month_column<-paste0("MES_",month_gr)
# print(month_column)
# 
# #filter data just two columns
# data_graph<- produccion_wide_estatal%>%
#   select(CULTIVO, ESTADO, month_column, Total_MES)
# 
# #select cultivo
# data_graph <- data_graph %>%
#      filter(str_trim(CULTIVO) == str_trim(found_cultivo))
# 
# data_graph <- data_graph %>% 
#   filter(str_to_upper(str_trim(CULTIVO)) == str_to_upper(str_trim(found_cultivo)))
# data_graph <- data_graph[data_graph$CULTIVO == "Tomate Rojo (Jitomate)", ]
# 
# #data_graph$month_column
# 
# data_graph <- data_graph %>%
#   select(CULTIVO, ESTADO, month_column, Total_MES) %>%
#   mutate(Percentage_month_column = .[[3]] / .[[ncol(.)]] * 100) %>%
#   arrange(desc(Percentage_month_column)) %>%  # Sort in descending order by Percentage_month_column
#   top_n(5, wt = Percentage_month_column)
# 
# 
# library(viridis)  # Make sure to install the 'viridis' package if you haven't already
# # Grafica -----------------------------------------------------------------
# #BAR
# ggplot(data = data_graph, aes(x = "", y = Percentage_month_column, fill = ESTADO)) +
#   geom_bar(stat = "identity") +
#   geom_text(
#     aes(label = sprintf("%.2f%%", Percentage_month_column)),
#     position = position_stack(vjust = 0.5),
#     show.legend = FALSE
#   ) +
#   coord_flip() +
#   ylab('Producción') +
#   xlab('Mes') +
#   ggtitle('Producción acumulada') +
#   scale_fill_viridis(discrete = TRUE) +  # Use viridis color palette
#   theme_minimal()
# 
# #PIE CHART
# 
# ggplot(data = data_graph, aes(x = "", y = Percentage_month_column, fill = ESTADO)) +
#   geom_bar(stat = "identity") +
#   coord_polar(theta = "y") +  # Convert to pie chart
#   geom_text(
#     aes(label = sprintf("%.2f%%", Percentage_month_column)),
#     position = position_stack(vjust = 0.5),
#     show.legend = FALSE
#   ) +
#   ylab('Producción') +
#   xlab('Mes') +
#   ggtitle('Producción acumulada') +
#   scale_fill_viridis(discrete = TRUE) +  # Use viridis color palette
#   theme_minimal()

#########################################################################################################


#----------------------------------------------------------------------------------------------------
##Shiny

#Librerías a utilizar
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(highcharter)
library(xts)
library(tsbox)
library(rsconnect)
library(gridExtra)
library(viridis)
library(patchwork)

# 
# print(as.character(cadena_dict[[1]]))
# selected_cadena <- as.character(cadena_dict[[1]])
# print(selected_cadena)
# found_cultivo <- names(cadena_dict)[cadena_dict == selected_cadena]
# print(found_cultivo)
# print(as.character(found_cultivo))
# data_2 <- produccion_wide2 %>%
#   filter(str_trim(CULTIVO) == str_trim(found_cultivo))
# print(data_2)  # Add a print statement to check the filtered data

# ui <- fluidPage(
#   theme = shinytheme("flatly"),
#   titlePanel("Reporte de precios semanales de frutas y hortalizas en la Central de Abasto de Iztapalapa"),
#   sidebarLayout(
#     sidebarPanel(
#       width = 3,
#       selectInput("cadena", "Select cadena:", choices = unique(cadena_dict)),
#     ),
#     mainPanel(
#       column(6, h3("Precio semanal", style = "text-align: center;")),
#       column(6, h3("Producción nacional", style = "text-align: center;")),
#       column(6, plotOutput("plot", height = "350px", width = "450px")),
#       column(6, plotOutput("plot_right", height = "350px", width = "450px")),
#       # Additional output element for the aggregated plot
#       column(12, h3("Producción 3y", style = "text-align: center;")),
#       column(12, plotOutput("aggregated_plot", height = "350px", width = "900px"))
#       # Additional output elements if needed
#     )
#   ),
#   footer = tags$div(
#     style = "text-align: center;",
#     HTML("This Shiny app was created by Your Name.")
#   )
# )


ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Reporte de precios semanales de frutas y hortalizas en la Central de Abasto de Iztapalapa"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("cadena", "Select cadena:", choices = unique(cadena_dict)),
    ),
    mainPanel(
      fluidRow(
        column(6, h3("Precio semanal", style = "text-align: center;")),
        column(6, h3("Producción nacional", style = "text-align: center;"))
      ),
      fluidRow(
        column(6, plotOutput("plot", height = "350px", width = "450px")),
        column(6, plotOutput("plot_right", height = "350px", width = "450px"))
      ),
      fluidRow(
        column(12, h3("Producción 3y", style = "text-align: center;")),
        column(12, plotOutput("aggregated_plot", height = "350px", width = "900px"))
      ),
      absolutePanel(
        bottom = 0, 
        #left = 0,
        right = 0, 
        style = "text-align: right;",
        HTML("This Shiny app was created by Sergio Serrano.")
      )
    )
  )
)

# Define server
server <- function(input, output) {
  output$plot <- renderPlot({
    selected_cadena <- as.character(input$cadena)
    data <- long_data_all[long_data_all$cadena == selected_cadena, ]
    print(data)  # Add a print statement to check the filtered data
    min <- min(min(data$value_2021), min(data$value_2022), min(data$value_2023))
    max <- max(max(data$value_2021), max(data$value_2022), max(data$value_2023)) * 1.1
    ggplot(data, aes(x = semana)) +
      geom_line(aes(y = value_2021, color = "2021"), linetype = "solid") +
      geom_line(aes(y = value_2022, color = "2022"), linetype = "solid") +
      geom_line(aes(y = value_2023, color = "2023"), linetype = "solid") +
      labs(x = "semana", y = "precio") +
      ylim(min, max) +
      scale_color_manual(values = c("2021" = "black", "2022" = "#238b45", "2023" = "red")) +
      scale_linetype_manual(values = c("solid", "solid", "solid")) +
      theme_minimal() +
      labs(color = "Año") +
      theme(legend.position = "top",
            legend.key.size = unit(2, "lines"),
            legend.text = element_text(size = 12))
  })
  
  output$plot_right <- renderPlot({
    selected_cadena <- as.character(input$cadena)
    found_cultivo <- as.character(names(cadena_dict)[cadena_dict == selected_cadena])
    print(found_cultivo)
    # Filter produccion_wide2 based on found_cultivo
    data_2 <- produccion_wide2 %>% 
      filter(str_trim(CULTIVO) == str_trim(found_cultivo))
    print(data_2)  # Add a print statement to check the filtered data
    data_2 <- produccion_wide2 %>% 
      filter(str_to_upper(str_trim(CULTIVO)) == str_to_upper(str_trim(found_cultivo)))
    # Convert CULTIVO to lowercase with the first letter in uppercase
    #data_2$CULTIVO <- tools::toTitleCase(tolower(data_2$CULTIVO))
    data_2$legend_label <- "Promedio 5y"
    min_avance <- min(min(data_2$PRODUCCION_diff_2021), min(data_2$PRODUCCION_diff_2022), min(data_2$PRODUCCION_diff_2023))
    max_avance <- max(max(data_2$PRODUCCION_diff_2021), max(data_2$PRODUCCION_diff_2022), max(data_2$PRODUCCION_diff_2023)) * 1.1
    ggplot(data_2) +
      geom_col(aes(x = MES, y = PRODUCCION_avg_last5_exclude_last_year_2021, fill = legend_label), size = 1, color = "#BCD2E8") +
      geom_line(aes(x = MES, y = PRODUCCION_diff_2021, color = "2021"), size = 0.8, group = 1) +
      geom_line(aes(x = MES, y = PRODUCCION_diff_2022, color = "2022"), size = 0.8, group = 1) +
      geom_line(aes(x = MES, y = PRODUCCION_diff_2023, color = "2023"), size = 0.8, group = 1) +
      labs(title = "Combined Plot", x = "Mes", y = "Avance") +
      ylim(min_avance, max_avance) +
      scale_color_manual(values = c("2021" = "black", "2022" = "#238b45", "2023" = "red")) +
      scale_fill_manual(values = "#BCD2E8", name = NULL) +  # Remove legend title
      scale_linetype_manual(values = c("solid", "solid", "solid")) +
      scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "k")) +  # Format y-axis labels in thousands
      theme_minimal() +
      labs(color = "Año") +
      theme(legend.position = "top",
            legend.key.size = unit(2, "lines"),
            legend.text = element_text(size = 9))
  })
  
  output$aggregated_plot  <- renderPlot({
    # Plot the basic frame of the stacked bar chart.
    month_gr<-as.character(month(Sys.Date()))
    month_column<-paste0("MES_",month_gr)
    print(month_column)
    selected_cadena <- as.character(input$cadena)
    found_cultivo <- as.character(names(cadena_dict)[cadena_dict == selected_cadena])
    print(found_cultivo)
    # Filter produccion_wide2 based on found_cultivo
    
    #filter data just two columns
    data_graph<- produccion_wide_estatal%>%
      select(CULTIVO, ESTADO, month_column, Total_MES)
    
    data_graph <- data_graph %>% 
      filter(str_to_upper(str_trim(CULTIVO)) == str_to_upper(str_trim(found_cultivo)))
    
    
    # Convert CULTIVO to lowercase with the first letter in uppercase
    #data_2$CULTIVO <- tools::toTitleCase(tolower(data_2$CULTIVO))
    data_graph$legend_label <- "Promedio 5y"
    
    data_graph <- data_graph %>%
      select(CULTIVO, ESTADO, month_column, Total_MES) %>%
      #mutate(Percentage_month_column = .[[3]] / .[[ncol(.)]] * 100) %>%
      mutate(Percentage_month_column = .[[3]] / sum(.[[3]]) * 100) %>%
      arrange(desc(Percentage_month_column)) %>%  # Sort in descending order by Percentage_month_column
      top_n(5, wt = Percentage_month_column)
    
    #min_avance <- min(min(data_2$PRODUCCION_diff_2021), min(data_2$PRODUCCION_diff_2022), min(data_2$PRODUCCION_diff_2023))
    #max_avance <- max(max(data_2$PRODUCCION_diff_2021), max(data_2$PRODUCCION_diff_2022), max(data_2$PRODUCCION_diff_2023)) * 1.1
    # Use a different viridis color palette
    
    ggplot(data = data_graph, aes(x = "", y = Percentage_month_column, fill = ESTADO)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +  # Convert to pie chart
      geom_text(
        aes(label = sprintf("%.2f%%", Percentage_month_column)),
        position = position_stack(vjust = 0.5),
        show.legend = FALSE
      ) +
      ylab('Producción') +
      xlab('Mes') +
      ggtitle('Producción acumulada') +
      scale_fill_viridis(discrete = TRUE, option="plasma", direction=-1) +  # Use viridis color palette
      theme_minimal()
    
    
  })
  
  output$combined_plot <- renderPlot({
    # Combine plots using patchwork
    p <- ggplot() +
      plot_spacer() + 
      ggplotGrob(output$plot) +
      plot_spacer() +
      ggplotGrob(output$plot_right) +
      plot_spacer() +
      ggplotGrob(output$aggregated_plot)
    
    p <- p + plot_layout(ncol = 2)
    
    p
  })
  
  
}


shinyApp(ui = ui, server = server)

