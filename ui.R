#version 1.1
library(shiny);library(DT);library(tidyverse);library(rhandsontable)
library(weathermetrics);library(shinyalert);library(shinyBS)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Methane Coversion Factor (MCF) Calculator"),

    # Sidebar
    fluidRow(
      column(width=3,
        checkboxInput("check_air","Input manure temperature (air temperature is input by default",FALSE,width=200),
        rHandsontableOutput("temp_datatable1",width=400),
        rHandsontableOutput("parameter_datatable",width=200),
        bsTooltip("parameter_datatable",
                "VS is the volatile solid excretion;\\
                VS% indicates the % going to liquid storage;\\
                Tmin and Tdamping are the minimum manure temperature\\
                and damping temperature based on user judgement;\\
                B0 is the max methane producing capacity\\
                emptying is the emptying efficiency"
                ,placement="top"),
        actionButton("plot1","Plot")),
        #Show plot
        column(width=9,
        plotOutput("plot.temp"),
        tableOutput("VSCH4")
        ))
    )
)


