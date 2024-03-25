library(shiny)
library(shinyjs)
library(DT)
library(plotly)

shinyUI(
  bootstrapPage(
    shinyjs::useShinyjs(),
    tags$style(type = "text/css","#Plot img {object-fit: contain; max-width: 100%; max-height: 100%; }"),
    title = "SARS-CoV-2 Analysis",
      tags$style(HTML(
        ".navbar-default .navbar-nav > li > a {color:black;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #337ab7; font-weight: bold;}
        .navbar-default .navbar-nav > li > a:hover {color: white; background-color:#337ab7; font-weight: bold;}
                    "
      )),
    
    navbarPage(
      title=div(img(src ="logo.jpg",height= "99px", width = "87px", align = "left",style = "margin:-10px 0px")),
      id = "SARS-CoV-2_Analysis",
    tabPanel(
      title = "Home",
      value = "introduction",
      fluidRow(column(
        width=10,offset=1,
        h3("A Web-based program for SARS-CoV-2 Analyses",align = "center"),
        h5("Shiny-SMAP provides an interactive and visual platform to visualise the VCF files of SARS-CoV-2 and introce the programs for studying SARS-CoV-2",align = "center"),
        br(),
        p("(1) Data Uploaded is designed for uploading your own VCF files which is decoded as vcf formats"),
        p("(2) Result Presentation is designed for presenting the vcf results, which consists of a table representing the mutations and Changes with DNA"),
        p("(3) Mutation Calling is designed for introducing the method to find the mutations and preparing your own vcf files"),
        p("(4) Program Introducing is designed for introduction the programs which is used for studying SARS-CoV-2"),
        br(),
        br(),
        br(),
        br(),
        br(),
        actionButton("jump_to_data_result_presentation","Result Presentation",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold"),
        p("Result Visulization And Saving"),
        br(),
        actionButton("jump_to_data_upload","Data Upload",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold"),
        p("Data Upload User Interface"),
        br(),
        actionButton("jump_to_mutation_calling","Mutation Calling",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold"),
        p("Introduce the Mutation Calling Method of How to Get A VCF File"),
        br(),
        actionButton("jump_to_program_introduction","Program Introduction",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold"),
        p("Introduce the Program for SARS-CoV-2 Research"),
        br(),
        br(),
        br(),
        br(),
        br(),
        h4("Github:",a("https://github.com/NiklausMikaelson12138/",href = "https://github.com/NiklausMikaelson12138/"),align="center"),
        h4("Contact Us:",a("s210502018@stu.cqupt.edu.cn",href = "s210502018@stu.cqupt.edu.cn"),align="center"),
        p(img(src = "lab_logo.jpg",height = "20%", width = "20%"),align = "center")
      ))
    ),
    tabPanel(
      shinyjs::useShinyjs(),title = "Result Presentation",value = "result_presentation",
      sidebarLayout(
        fluid = TRUE,
        sidebarPanel(
          br(),
          br(),
          br(),
          h3("Menu Bar",align = "center"),
          br(),
          radioButtons("variants_of_SARS_CoV_2",
                      label = "Choose a subtype of SARS-CoV-2 to display",
                      choices = c("Alpha","Beta","Delta","Omicron"),
                      selected = "Alpha",inline = TRUE),
          br(),
          selectInput("object_of_SARS_CoV_2",
                      label = "Choose an object to visualization",
                      choices = c("Changes of Bases","Changes of Proteins","Changes of Positions","Kinds of Mutations"),
                      selected = "Changes of Bases"),
          br(),
          selectInput("protein_of_SARS_CoV_2",
                      label = "Choose a protein to show mutation positions",
                      choices = c("All","ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10"),
                      selected = "ORF1ab"),
          br(),
          actionButton("display_results","Analyse",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold",width = "100%"),
          br(),
          br(),
          br(),
          br(),
          radioButtons("variant_of_SARS_CoV_2",
                       label = "Choose a subtype to show mutations",
                       choices = c("Alpha","Beta","Delta","Omicron"),
                       selected = "Alpha",inline = TRUE),
          br(),
          actionButton("display_mutations","Display",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold",width = "100%"),
          br(),
          br(),
          br(),
          br(),
          #h4("Press to show coinfection pathogens with SARS-CoV-2"),
          radioButtons("pathogens_kinds",
                       label = "Choose a species to show co-pathoges",
                       choices = c("All","Eukaryota",'Bacteria',"Virus"),
                       selected = "All"),
          actionButton("display_coinfection_pathogen","Show",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold",width = "100%"),
          width = 3),
        mainPanel(
          h3("The Analysis Result of VCF Belonging to Variants of SARS-CoV-2 as Below",align = "center"),
          br(),
          h4("The photo of analysing SARS-CoV-2",align= 'center'),
          downloadButton("download_plot","Download Plot"),
          plotOutput("mutation"),
          br(),
          h4("The table of SARS-CoV-2 mutation",align = 'center'),
          downloadButton("download_table","Download Table"),
          DTOutput("table_mutation"),
          br(),
          h4("The photo of coinfection pathogens",align = 'center'),
          plotlyOutput("coinfection_pathogen"),
          width = 9
        )
      ),
    ),
    tabPanel(
      title = "Data Upload", value = "process",
      sidebarLayout(
        fluid = TRUE,
        sidebarPanel(
          br(),
          br(),
          br(),
          h3("Data Uploading",align = "center"),
          br(),
          p("uploading your vcf files"),
          fileInput(
            "vcf_file","Choose a file",accept = ".vcf"
          ),
          br(),
          selectInput("analysis_object_of_SARS_CoV_2",
                      label = "Choose an object to visualization",
                      choices = c("Changes of Bases","Changes of Proteins","Changes of Positions","Kinds of Mutations"),
                      selected = "Changes of Bases"),
          br(),
          selectInput("analysis_protein_of_SARS_CoV_2",
                      label = "Choose a protein to show mutation positions",
                      choices = c("All","ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10"),
                      selected = "ORF1ab"),
          actionButton("analyses_results","Analyse",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold",width = "100%"),
          br(),
          br(),
          br(),
          actionButton("analyses_table_results","Show Mutation",style="color: #fff; background-color: #337ab7; border-color: #2e6da4; font-size:100%; font-weight: bold",width = "100%"),
          width = 3),
        mainPanel(
          h3("The Analysis Result of Your VCF File",align = "center"),
          br(),
          h4("The photo results",align = "center"),
          plotOutput("vcf_mutation"),
          br(),
          h4("The table of mutation of SARS-CoV-2",align = "center"),
          DTOutput("vcf_table_mutation"),
          br(),
          width = 9
        )
      )
    ),
    tabPanel(
      title = "Mutation Calling",value = "mutation_calling",
      sidebarLayout(
        fluid = TRUE,
        sidebarPanel(
          h2("A Pipeline for Users to Get A VCF File",align = "center"),
          br(),
          h3("Database Building"),
          h4("Step1 Database for Bwa"),
          p("  Code: bwa index refseq"),
          br(),
          h4("Step2 Database for snpEff"),
          p("  Folling the guidence:",a("https://pcingola.github.io/SnpEff/se_buildingdb/",href = "https://pcingola.github.io/SnpEff/se_buildingdb/")),
          br(),
          br(),
          h3("Sample Processing"),
          h4("Step1 Quality Control"),
          p("Trim_galore is used for WGS data quality control."),
          p("  Code: trim_galore --paired --quality 30 --length 20 fastq1 fastq2 -o fastq_filter"),
          br(),
          h4("Step2 Alignment"),
          p("BWA is used for WGS sequence alignment."),
          p("  Code: bwa aln refseq fastq_filter_1 > fastq_1_sai"),
          p("  Code: bwa aln refseq fastq_filter_2 > fastq_2_sai"),
          p("  Code: bwa sampe refseq fastq_1_sai fastq_2_sai fastq_filter_1 fastq_filter_2 > fastq.sam"),
          br(),
          h4("Step3 Mutation Calling"),
          p("Samtools/BCFTools is used for variant calling."),
          p("  Code: samtools view -S -1 fastq.sam > fastq.bam"),
          p("  Code: samtools sort fastq.bam > fastq.sort.bam"),
          p("  Code: bcftools mpileup fastq.sort.bam --fasta-ref refseq > fastq.vcf"),
          p("  Code: bcftools call fastq.vcf -c -v -o fastq.variant.vcf"),
          br(),
          h4("Step4 Exnorate VCF Flie"),
          p("snpEff is used for annotate vcf files."),
          p("  Code: snpeff refseq fastq.variant.vcf > fastq.snpeff.vcf"),
          width = 5
        ),
        mainPanel(
          h4("The Flowchart of The Pipeline as Below",align = "center"),
          img(src = "refseq_processing.jpg",height = "100%", width = "100%"),
          p("Building the database for alignment and exonerate.",align = "center"),
          br(),
          img(src = "sample_processing.jpg",height = "100%", width = "100%"),
          p("The pipeline to get vcf files.",align = "center"),
          width = 7
        )
      )
    ),
    tabPanel(
      title = "Program Introduction",value = "program_introduction",
      fluidRow(
        column(
          width = 11, offset = 1,
          h2("The program of analysing SARS-CoV-2 are as below."),
          fluidRow(
            column(
              width = 11,h3("1.Sequence Processing")),
            column(
              width = 1,
              actionButton("S1",label = "more",icon = icon("angle-down"),class = "btn btn-info")),
          ),
          fluidRow(column(width = 11, hidden(htmlOutput(outputId = "text_S1")))),
          fluidRow(
            column(
              width = 11,h3("2.Mutation Analysis")),
            column(
              width = 1,
              actionButton("S2",label = "more",icon = icon("angle-down"),class = "btn btn-info")),
          ),
          fluidRow(column(width = 11, hidden(htmlOutput(outputId = "text_S2")))),
          fluidRow(
            column(
              width = 11,h3("3.Role Projections")),
            column(
              width = 1,
              actionButton("S3",label = "more",icon = icon("angle-down"),class = "btn btn-info")),
          ),
          fluidRow(column(width = 11, hidden(htmlOutput(outputId = "text_S3")))),
          fluidRow(
            column(
              width = 11,h3("4.Evolutionary And Transmission")),
            column(
              width = 1,
              actionButton("S4",label = "more",icon = icon("angle-down"),class = "btn btn-info")),
          ),
          fluidRow(column(width = 11, hidden(htmlOutput(outputId = "text_S4")))),
          fluidRow(
            column(
              width = 11,h3("5.Database")),
            column(
              width = 1,
              actionButton("S5",label = "more",icon = icon("angle-down"),class = "btn btn-info")),
          ),
          fluidRow(column(width = 11, hidden(htmlOutput(outputId = "text_S5")))),
          fluidRow(
            column(
              width = 11,h3("6.Others")),
            column(
              width = 1,
              actionButton("S6",label = "more",icon = icon("angle-down"),class = "btn btn-info")),
          ),
          fluidRow(column(width = 11, hidden(htmlOutput(outputId = "text_S6")))),
        )
      )
    ),
    ),
  )
)