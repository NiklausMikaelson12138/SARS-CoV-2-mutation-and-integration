library(shiny)
library(magick)
library(jpeg)
library(shinyjs)
library(trackViewer) #主要画图包
library(RColorBrewer) #引入颜色包
library(networkD3)  #绘制桑葚图
library(plotly)
library(vcfR)
library(ggplot2)

function(input,output,session){
  
  observeEvent(input$jump_to_data_upload,{
    updateTabsetPanel(session,"SARS-CoV-2_Analysis",
                      selected = "process")
  })
  
  observeEvent(input$jump_to_data_result_presentation,{
    updateTabsetPanel(session,"SARS-CoV-2_Analysis",
                      selected = "result_presentation")
  })
  
  observeEvent(input$jump_to_mutation_calling,{
    updateTabsetPanel(session,"SARS-CoV-2_Analysis",
                      selected = "mutation_calling")
  })
  
  observeEvent(input$jump_to_program_introduction,{
    updateTabsetPanel(session,"SARS-CoV-2_Analysis",
                      selected = "program_introduction")
  })
  
  observeEvent(input$S1,{
    toggle("text_S1")
    output$text_S1 <- renderText({
      paste(h4(a("CGT",href=" https://covidgenotyper.app/")),
            "A Web Application for SARS-CoV-2 Genotyping",
            "<br>",
            h4(a("CoVrimer",href = "http://konulabapps.bilkent.edu.tr:3838/CoVrimer/")),
            "A Tool for SARS-CoV-2 Primer Aligning",
            "<br>",
            h4(a("Phytest",href = "https://phytest-devs.github.io/phytest/")),
            "A Tool for Quality Control",
            "<br>",
            h4(a("ViralVar",href = "http://viralvar.org/")),
            "A Web Tool for SARS-CoV-2 Genomes Visualization",
            "<br>",
            h4(a("ReadItAndKeep",href = "https://github.com/GenomePathogenAnalysisService/read-it-and-keep")),
            "A Tool for SARS-CoV-2 Reads Decontamination",
            h4(a("ViralFlow",href = "https://github.com/dezordi/ViralFlow")),
            "A Workflow for SARS-CoV-2 Genome Analysis",
            "<br>",
            h4(a("HAVoC",href = "https://bitbucket.org/auto_cov_pipeline/havoc")),
            "A Pipeline for SARS-CoV-2 Lineage Assignment",
            "<br>",
            h4(a("VirusRecom",href = "https://github.com/ZhijianZhou01/VirusRecom")),
            "A Method for SARS-CoV-2 Genome Recombination Dection",
            "<br>",
            h4(a("Covidex",href = "http://covidex.unlu.edu.ar")),
            "A Tool for SARS-CoV-2 Genome Subtyping",
            "<br>",
            h4(a("SCOPE2",href = "http://124.207.243.56:8081/")),
            "A Workflow for SARS-CoV-2 Genome Analysis",
            "<br>",
            h4(a("gofasta",href = "https://github.com/virus-evolution/gofasta")),
            "A Platform for SARS-CoV-2 Primer Evaluation",
            "</p>"
    )})
  })
  
  observeEvent(input$S2,{
    toggle("text_S2")
    output$text_S2 <- renderText({
      paste(h4(a("UShER_SARS-CoV-2",href="http://hgdownload.soe.ucsc.edu/goldenPath/wuhCor1/UShER_SARS-CoV-2/")),
            "A Tool for SARS-CoV-2 Mutation Annotated Tree",
            "<br>",
            h4(a("VOC-alarm",href="https://github.com/guangxujin/VOC-alarm")),
            "A Tool for SARS-CoV-2 Varitants Prection",
            "<br>",
            h4(a("SPEAR",href="https://github.com/m-crown/SPEAR")),
            "A R Library for SARS-CoV-2 Protein Annotator",
            "<br>",
            h4(a("InterARTIC",href="https://github.com/Psy-Fer/interARTIC/")),
            "A Web Application for SARS-CoV-2 Whole Genome Sequencing Analyis",
            "<br>",
            h4(a("VariantHunter",href="http://gmql.eu/variant_hunter/")),
            "A Tool for SARS-CoV-2 Variant Dection",
            "<br>",
            h4(a("CovidShiny",href="http://www.zhanglabtools.online/shiny/CovidShiny/")),
            "A Web Tool for SARS-CoV-2 Mutation Profiling And Diagnosis Evaluation",
            "<br>",
            h4(a("SpikeSeq",href="https://github.com/kblin/covid-spike-classification")),
            "A Method for SARS-CoV-2 S Gene Variant Identification",
            "<br>",
            h4(a("SpikeScape",href="https://spikescape.streamlitapp.com/")),
            "A Tool for Analyzing Structural Diversity",
            "<br>",
            h4(a("Coronapp",href="https://github.com/federicogiorgi/giorgilab/tree/master/coronannotator")),
            "A Web Application to Annotate And Monitor SARS-CoV-2 Mutation",
            "<br>",
            h4(a("CovRadar",href="https://covradar.net")),
            "A Web Application to Track And Filter SARS-CoV-2 Mutation",
            "<br>",
            h4(a("ViruClust",href="http://gmql.eu/viruclust/")),
            "A Method to Compare SARS-CoV-2 Variant",
            "<br>",
            h4(a("Viral Instant Mutation Viewer",href="https://vimver.afmb.univ-mrs.fr/")),
            "A Tool to Speed Up Identification And Analysis SARS-CoV-2 Variant",
            "<br>",
            h4(a("MixviR",href="https://github.com/mikesovic/MixviR/tree/main/mutation_files")),
            "A R Library for Exploring SARS-CoV-2 Variant",
            "<br>",
            h4(a("COVID-19 CG",href="https://covidcg.org/")),
            "A Web Application to Track SARS-CoV-2 Mutation And Lineage",
            "<br>",
            h4(a("VarEPS",href="http://www.nmdc.cn/ncovn")),
            "A System to Evaluate And Prewarn SARS-CoV-2 Variation",
            "<br>",
            h4(a("CovDif",href="https://github.com/INMEGEN/CovDif")),
            "A Tool to Visualize SARS-CoV-2 Conservation of Variant And Genome",
            "</p>"
      )})
  })
  
  observeEvent(input$S3,{
    toggle("text_S3")
    output$text_S3 <- renderText({
      paste(h4(a("CRESSP",href="https://ahs2202.github.io/3M/")),
            "A Pipeline for Predict SARS-CoV-2 Epitopes",
            "<br>",
            h4(a("SCoV2-MD",href="www.scov2-md.org")),
            "A Database for Predict SARS-CoV-2 Proteme And Variant Impact",
            "<br>",
            h4(a("Mpropred",href="https://share.streamlit.io/nadimfrds/mpropred/Mpropred_app.py")),
            "A Web-APP for Predict SARS-CoV-2 Mpro Antagonists",
            "<br>",
            h4(a("DeepIPs",href="https://github.com/linDing-group/DeepIPs")),
            "A Approach to Identify SARS-CoV-2 Infection Sites",
            "<br>",
            h4(a("EpiSurf",href="http://gmql.eu/episurf/")),
            "A Server for Analyzing SARS-CoV-2 Amino Acid Changes",
            "<br>",
            h4(a("CoronaPep",href="")),
            "A Tool for Anti-Coronavirus Peptide Generation",
            "<br>",
            h4(a("PCN-Miner",href="https://github.com/hguzzi/ProteinContactNetworks")),
            "A Tool for Analysis Protein Contact Networks",
            "<br>",
            h4(a("nCoVDock2",href="https://ncovdock2.schanglab.org.cn")),
            "A Docking Server to Predict the Binding Mode for SARS-CoV-2 Targets And Ligands",
            "<br>",
            h4(a("PEPPI",href="https://zhanggroup.org/PEPPI/")),
            "A Method to Predict Protein-Protein Interation",
            "</p>"
      )})
  })
  
  observeEvent(input$S4,{
    toggle("text_S4")
    output$text_S4 <- renderText({
      paste(h4(a("Coronavirus GenBrowser",href="https://ngdc.cncb.ac.cn/ncov/release_genome")),
            "A Web Application for Monitoring Transmission And Evolution of SARS-CoV-2",
            "<br>",
            h4(a("CSSE",href="thelancet.com/infection")),
            "A Web Dashboard to Track COVID-19",
            "<br>",
            h4(a("CovidPhy",href="http://covidphy.eu")),
            "A Tool for Phylogeographic Analysis of SARS-COV-2",
            "<br>",
            h4(a("A2B-COVID",href="http://shiny.mrc-bsu.cam.ac.uk/apps/a2bcovid/")),
            "A Tool for Evaluation SARS-CoV-2 Transmission Events",
            "<br>",
            h4(a("CoVe-Tracker",href="https://project.iith.ac.in/cove-tracker/")),
            "A Web Application to Track SARS-CoV-2 Pan Proteome",
            "</p>"
      )})
  })
  
    observeEvent(input$S5,{
      toggle("text_S5")
      output$text_S5 <- renderText({
        paste(h4(a("Drug Database",href="")),
              "Anti-SARS-CoV-2 Repurposing Drug Database",
              "<br>",
              h4(a("DockCoV2",href="https://covirus.cc/drugs/")),
              "A Drug Database Against SARS-CoV-2",
              "<br>",
              h4(a("SARS-CoV-2 3D",href="https://sars3d.com/")),
              "A Database for Coronavirus Proteome And Evaluating Drug Targets",
              "<br>",
              h4(a("GESS",href="https://wan-bioinfo.shinyapps.io/GESS/")),
              "A Database of evaluation SARS-CoV-2 Sequences",
              "<br>",
              h4(a("siRNAdb",href="http://www.bioinformatics-brazil.org/siRNAdb/sirnas_cov_db.zip")),
              "A small interfering RNA (siRNA) database for SARS-CoV-2",
              "<br>",
              h4(a("CoV-RDB",href="https://covdb.stanford.edu")),
              "Coronavirus Resistance Database",
              "<br>",
              h4(a("SCoV2-MD",href="www.scov2-md.org")),
              "A Database for the SARS-CoV-2 Proteome And Variant Impact Predictions",
              "</p>"
        )})
  })
    
    observeEvent(input$S6,{
      toggle("text_S6")
      output$text_S6 <- renderText({
        paste(h4(a("e-CoVig",href="https://play.google.com/store/apps/details?id=com.ecovig.app")),
              "A System for Monitoring Symptoms of COVID-19",
              "<br>",
              h4(a("PACIFIC",href="https://github.com/pacific-2020/pacific")),
              "A Classifier of SARS-CoV-2 and Co-intection RNA Virus",
              "<br>",
              h4(a("COWID",href="https://github.com/hendrick0403/COWID")),
              "A Workflow for Identification SARS-CoV-2",
              "</p>"
        )})
    })
    
    observeEvent(input$display_results,{
      selected_variant <- input$variants_of_SARS_CoV_2
      selected_object <- input$object_of_SARS_CoV_2
      selected_protein <- input$protein_of_SARS_CoV_2
      
      if (selected_variant == "Alpha" && selected_object == "Changes of Bases") {
        reactive_mutation <- reactive({
          acgt <- read.csv(".//data//alpha_acgt.csv",row.names = 1)
          
          sums <- colSums(acgt)[1:12]
          base_kinds <- c("A_C","A_T","A_G","C_A","C_T","C_G","G_A","G_C","G_T","T_A","T_C","T_G")
          new_acgt <- data.frame(base_kinds,sums)
          
          ggplot(new_acgt, aes(x = base_kinds, y = sums, fill = base_kinds)) +
            geom_bar(stat = "identity", color = "black") +
            labs(title = "Mutation of Nuclotide", x = "Mutation Type", y = "Number") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
            )
        })
        
        output$mutation <- renderPlot({
          acgt <- read.csv(".//data//alpha_acgt.csv",row.names = 1)
          
          sums <- colSums(acgt)[1:12]
          indicators <- c("A_C","A_T","A_G","C_A","C_T","C_G","G_A","G_C","G_T","T_A","T_C","T_G")
          new_acgt <- data.frame(indicators,sums)
          
          ggplot(new_acgt, aes(x = indicators, y = sums, fill = indicators)) +
            geom_bar(stat = "identity", color = "black") +
            labs(title = "Mutation of Nuclotide", x = "Mutation Type", y = "Number") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
            )
        })
      }
      
      if (selected_variant == "Alpha" && selected_object == "Changes of Proteins") {
        output$mutation <- renderPlot({
          amino <- read.csv(".//data//alpha_amino.csv",row.names = 1)
          
          amino_sums <- colSums(amino)
          proteins <- c("ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10")
          new_amino <- data.frame(proteins,amino_sums)
          new_amino$proteins <- factor(new_amino$proteins,levels = proteins)
          ggplot(new_amino, aes(x = proteins, y = amino_sums, fill = proteins)) +
            geom_bar(stat = "identity", color = "black") +
            labs(title = "Mutation of Protein", x = "Protein", y = "Number") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.text.x = element_text(angle = 45, hjust = 1),
            )
        })
      }

      if (selected_variant == "Alpha" && selected_object == "Changes of Positions") {
        
        if (selected_protein == "All"){
          output$mutation <- renderPlot({
            SNP <- c(241,913,2453,3037,3267,5388,5986,6174,6954,11287,11291,14408,14676,14925,15906,15279,16176,19551,21764,21770,21990,21992,23063,23271,23403,23604,23709,24506,24914,26467,27972,28048,28095,28111,28270,28280,28281,28282,28881,28882,28883,28977)
            variant <- GRanges("NC_045512.2", IRanges(SNP, width=1, names=SNP)) # 设置棒棒图位置
            features <- GRanges("NC_045512.2", IRanges(c(0, 21563, 25393,26245,26523,27202,27394,27756,27894,28274,29558), # 设置block起使位置
                                                       width=c(21563,3821,827,227,668,185,365,131,365,1259,116), # 设置block 的长度
                                                       names=c("ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10"))) # 设置名字
            
            features$fill <- brewer.pal(11,"Spectral") #块的颜色
            variant$color <- sample.int(length(SNP), length(SNP)) #棒子上面的球的颜色
            
            
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE) #棒子的颜色
            variant$label <- as.character(1:length(variant)) #球内的字符
            variant$label.col <- "black" #球内的标签的颜色
            features2 <-features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2, type="pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF1ab"){
          output$mutation <- renderPlot({
            SNP <- c(241,913,2453,3037,3267,5388,5986,6174,6954,11287,11291,14408,14676,14925,15906,15279,16176,19551)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=266,end=21555,
                                                      width = 21290,
                                                      names = "ORF1ab"))
            features$fill <- brewer.pal(11,"Spectral")[1]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "S"){
          output$mutation <- renderPlot({
            SNP <- c(21764,21770,21990,21992,23063,23271,23403,23604,23709,24506,24914)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=21563,end=25383,
                                                      width = 3821,
                                                      names = "S"))
            features$fill <- brewer.pal(11,"Spectral")[2]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF3a"){
          output$mutation <- renderPlot({
            SNP <- c(26467)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=25393,end=26220,
                                                      width = 828,
                                                      names = "ORF3a"))
            features$fill <- brewer.pal(11,"Spectral")[3]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "E"){
          output$mutation <- renderPlot({
            SNP <- c(26467)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=26245,end=26472,
                                                      width = 228,
                                                      names = "E"))
            features$fill <- brewer.pal(11,"Spectral")[4]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "M"){
          output$mutation <- renderPlot({
            SNP <- c(26467)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=26523,end=27191,
                                                      width = 669,
                                                      names = "M"))
            features$fill <- brewer.pal(11,"Spectral")[5]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF6"){
          output$mutation <- renderPlot({
            SNP <- c(26467)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27202,end=27387,
                                                      width = 186,
                                                      names = "ORF6"))
            features$fill <- brewer.pal(11,"Spectral")[6]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF7a"){
          output$mutation <- renderPlot({
            SNP <- c(26467)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27394,end=27759,
                                                      width = 366,
                                                      names = "ORF7a"))
            features$fill <- brewer.pal(11,"Spectral")[7]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF7b"){
          output$mutation <- renderPlot({
            SNP <- c(26467)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27756,end=27887,
                                                      width = 132,
                                                      names = "ORF7b"))
            features$fill <- brewer.pal(11,"Spectral")[8]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF8"){
          output$mutation <- renderPlot({
            SNP <- c(27972,28048,28095,28111)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27894,end=28259,
                                                      width = 366,
                                                      names = "ORF8"))
            features$fill <- brewer.pal(11,"Spectral")[9]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "N"){
          output$mutation <- renderPlot({
            SNP <- c(28270,28280,28281,28282,28881,28882,28883,28977)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=28274,end=29533,
                                                      width = 1260,
                                                      names = "N"))
            features$fill <- brewer.pal(11,"Spectral")[10]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF10"){
          output$mutation <- renderPlot({
            SNP <- c(28270)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=29558,end=29764,
                                                      width = 207,
                                                      names = "ORF10"))
            features$fill <- brewer.pal(11,"Spectral")[11]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
      }
      
      if (selected_variant == "Alpha" && selected_object == "Kinds of Mutations"){
        output$mutation <- renderPlot({
          synony <- read.csv(".//data//alpha_synony.csv",row.names = 1)
          synony_sums <- colSums(synony)
          type <- c("synonymous mutation","missense mutation","stop gained mutation")
          new_synony <- data.frame(type,synony_sums)
          new_synony <- new_synony %>%
            mutate(percent = synony_sums/sum(synony_sums)*100)
          ggplot(new_synony, aes(x = "", y = percent, fill = type)) +
            geom_bar(stat = "identity", color = "black") +
            geom_text(aes(label = sprintf("%.1f%%",percent)),position=position_stack(vjust=0.5))+
            coord_polar(theta = "y")+
            labs(title = "Mutation of Synonymous", x = " ", y = " ") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              legend.position ="bottom"
            )
        })
      }
      
      if (selected_variant == "Beta" && selected_object == "Changes of Bases") {
        
        output$mutation <- renderPlot({
          acgt <- read.csv(".//data//beta_acgt.csv",row.names = 1)
          
          sums <- colSums(acgt)[1:12]
          base_kinds <- c("A_C","A_T","A_G","C_A","C_T","C_G","G_A","G_C","G_T","T_A","T_C","T_G")
          new_acgt <- data.frame(base_kinds,sums)
          
          ggplot(new_acgt, aes(x = base_kinds, y = sums, fill = base_kinds)) +
            geom_bar(stat = "identity", color = "black") +
            labs(title = "Mutation of Nuclotide", x = "Mutation Type", y = "Number") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
            )
        })
      }
      
      if (selected_variant == "Beta" && selected_object == "Changes of Proteins") {
        output$mutation <- renderPlot({
          amino <- read.csv(".//data//beta_amino.csv",row.names = 1)
          
          amino_sums <- colSums(amino)
          proteins <- c("ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10")
          new_amino <- data.frame(proteins,amino_sums)
          new_amino$proteins <- factor(new_amino$proteins,levels = proteins)
          ggplot(new_amino, aes(x = proteins, y = amino_sums, fill = proteins)) +
            geom_bar(stat = "identity", color = "black") +
            labs(title = "Mutation of Protein", x = "Protein", y = "Number") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.text.x = element_text(angle = 45, hjust = 1),
            )
        })
      }
      
      if (selected_variant == "Beta" && selected_object == "Changes of Positions") {
        
        if (selected_protein == "All"){
          output$mutation <- renderPlot({
            SNP <- c(174,241,913,1059,2110,2692,3037,3267,4213,5230,5388,5986,6954,10323,11279,11287,11288,11291,11296,11328,11812,
                     12525,14120,14408,14676,15279,15925,16176,17970,21614,21641,21764,21770,21801,81989,21990,22206,22280,22813,23012,23063,23271,
                     23403,23604,23664,23709,23764,24506,24620,24914,25217,25537,25563,25904,26456,27462,27972,28048,28095,28111,28253,28270,28280,
                     28281,28282,28642,28881,28882,28883,28887,28977)
            variant <- GRanges("NC_045512.2", IRanges(SNP, width=1, names=SNP)) # 设置棒棒图位置
            features <- GRanges("NC_045512.2", IRanges(c(0, 21563, 25393,26245,26523,27202,27394,27756,27894,28274,29558), # 设置block起使位置
                                                       width=c(21563,3821,827,227,668,185,365,131,365,1259,116), # 设置block 的长度
                                                       names=c("ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10"))) # 设置名字
            features$fill <- brewer.pal(11,"Spectral") #块的颜色
            variant$color <- sample.int(length(SNP), length(SNP)) #棒子上面的球的颜色
            
            
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE) #棒子的颜色
            variant$label <- as.character(1:length(variant)) #球内的字符
            variant$label.col <- "black" #球内的标签的颜色
            features2 <-features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2, type="pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF1ab"){
          output$mutation <- renderPlot({
            SNP <- c(174,241,913,1059,2110,2692,3037,3267,4213,5230,5388,5986,6954,10323,11279,11287,11288,11291,11296,11328,11812,
                     12525,14120,14408,14676,15279,15925,16176,17970)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=266,end=21555,
                                                      width = 21290,
                                                      names = "ORF1ab"))
            features$fill <- brewer.pal(11,"Spectral")[1]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "S"){
          output$mutation <- renderPlot({
            SNP <- c(21614,21641,21764,21770,21801,81989,21990,22206,22280,22813,23012,23063,23271,
                     23403,23604,23664,23709,23764,24506,24620,24914,25217)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=21563,end=25383,
                                                      width = 3821,
                                                      names = "S"))
            features$fill <- brewer.pal(11,"Spectral")[2]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF3a"){
          output$mutation <- renderPlot({
            SNP <- c(25537,25563,25904)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=25393,end=26220,
                                                      width = 828,
                                                      names = "ORF3a"))
            features$fill <- brewer.pal(11,"Spectral")[3]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "E"){
          output$mutation <- renderPlot({
            SNP <- c(26456)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=26245,end=26472,
                                                      width = 228,
                                                      names = "E"))
            features$fill <- brewer.pal(11,"Spectral")[4]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "M"){
          output$mutation <- renderPlot({
            SNP <- c(26467)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=26523,end=27191,
                                                      width = 669,
                                                      names = "M"))
            features$fill <- brewer.pal(11,"Spectral")[5]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF6"){
          output$mutation <- renderPlot({
            SNP <- c(26467)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27202,end=27387,
                                                      width = 186,
                                                      names = "ORF6"))
            features$fill <- brewer.pal(11,"Spectral")[6]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF7a"){
          output$mutation <- renderPlot({
            SNP <- c(27462)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27394,end=27759,
                                                      width = 366,
                                                      names = "ORF7a"))
            features$fill <- brewer.pal(11,"Spectral")[7]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF7b"){
          output$mutation <- renderPlot({
            SNP <- c(27972)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27756,end=27887,
                                                      width = 132,
                                                      names = "ORF7b"))
            features$fill <- brewer.pal(11,"Spectral")[8]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF8"){
          output$mutation <- renderPlot({
            SNP <- c(27972,28048,28095,28111,28253)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27894,end=28259,
                                                      width = 366,
                                                      names = "ORF8"))
            features$fill <- brewer.pal(11,"Spectral")[9]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "N"){
          output$mutation <- renderPlot({
            SNP <- c(28270,28280,28281,28282,28642,28881,28882,28883,28887,28977)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=28274,end=29533,
                                                      width = 1260,
                                                      names = "N"))
            features$fill <- brewer.pal(11,"Spectral")[10]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF10"){
          output$mutation <- renderPlot({
            SNP <- c(28270)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=29558,end=29764,
                                                      width = 207,
                                                      names = "ORF10"))
            features$fill <- brewer.pal(11,"Spectral")[11]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
      }
      
      if (selected_variant == "Beta" && selected_object == "Kinds of Mutations"){
        output$mutation <- renderPlot({
          synony <- read.csv(".//data//beta_synony.csv",row.names = 1)
          synony_sums <- colSums(synony)
          type <- c("synonymous mutation","missense mutation","stop gained mutation")
          new_synony <- data.frame(type,synony_sums)
          new_synony <- new_synony %>%
            mutate(percent = synony_sums/sum(synony_sums)*100)
          ggplot(new_synony, aes(x = "", y = percent, fill = type)) +
            geom_bar(stat = "identity", color = "black") +
            geom_text(aes(label = sprintf("%.1f%%",percent)),position=position_stack(vjust=0.5))+
            coord_polar(theta = "y")+
            labs(title = "Mutation of Synonymous", x = " ", y = " ") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              legend.position ="bottom"
            )
        })
      }
      
      if (selected_variant == "Delta" && selected_object == "Changes of Bases") {
        
        output$mutation <- renderPlot({
          acgt <- read.csv(".//data//delta_acgt.csv",row.names = 1)
          
          sums <- colSums(acgt)[1:12]
          base_kinds <- c("A_C","A_T","A_G","C_A","C_T","C_G","G_A","G_C","G_T","T_A","T_C","T_G")
          new_acgt <- data.frame(base_kinds,sums)
          
          ggplot(new_acgt, aes(x = base_kinds, y = sums, fill = base_kinds)) +
            geom_bar(stat = "identity", color = "black") +
            labs(title = "Mutation of Nuclotide", x = "Mutation Type", y = "Number") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
            )
        })
      }
      
      if (selected_variant == "Delta" && selected_object == "Changes of Proteins") {
        output$mutation <- renderPlot({
          amino <- read.csv(".//data//delta_amino.csv",row.names = 1)
          
          amino_sums <- colSums(amino)
          proteins <- c("ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10")
          new_amino <- data.frame(proteins,amino_sums)
          new_amino$proteins <- factor(new_amino$proteins,levels = proteins)
          ggplot(new_amino, aes(x = proteins, y = amino_sums, fill = proteins)) +
            geom_bar(stat = "identity", color = "black") +
            labs(title = "Mutation of Protein", x = "Protein", y = "Number") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.text.x = element_text(angle = 45, hjust = 1),
            )
        })
      }
      
      if (selected_variant == "Delta" && selected_object == "Changes of Positions") {
        
        if (selected_protein == "All"){
          output$mutation <- renderPlot({
            SNP <- c(21,25,241,2095,2832,3037,3241,3544,4886,5386,5393,5924,6512,6515,7615,8393,10029,10449,11282,11291,11471,11537,12670,13195,14004,14408,15714,17410,18163,18402,
                     19955,20055,21618,21632,21987,22200,22578,22674,22679,22686,22688,22775,22786,22792,22813,22882,22992,22995,23013,23040,23055,23063,23075,23403,23525,23599,23604,
                     23694,23700,23706,23854,23948,24424,24469,25000,25584,26060,26270,26577,26709,26858,27259,27382,27383,27384,27807,27954,28271,28311,28361,28367,28881,28882,28883,29510,29733)
            variant <- GRanges("NC_045512.2", IRanges(SNP, width=1, names=SNP)) # 设置棒棒图位置
            features <- GRanges("NC_045512.2", IRanges(c(0, 21563, 25393,26245,26523,27202,27394,27756,27894,28274,29558), # 设置block起使位置
                                                       width=c(21563,3821,827,227,668,185,365,131,365,1259,116), # 设置block 的长度
                                                       names=c("ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10"))) # 设置名字
            features$fill <- brewer.pal(11,"Spectral") #块的颜色
            variant$color <- sample.int(length(SNP), length(SNP)) #棒子上面的球的颜色
            
            
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE) #棒子的颜色
            variant$label <- as.character(1:length(variant)) #球内的字符
            variant$label.col <- "black" #球内的标签的颜色
            features2 <-features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2, type="pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF1ab"){
          output$mutation <- renderPlot({
            SNP <- c(21,25,241,2095,2832,3037,3241,3544,4886,5386,5393,5924,6512,6515,7615,8393,10029,10449,11282,11291,11471,11537,12670,13195,14004,14408,15714,17410,18163,18402,
                     19955,20055)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=266,end=21555,
                                                      width = 21290,
                                                      names = "ORF1ab"))
            features$fill <- brewer.pal(11,"Spectral")[1]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "S"){
          output$mutation <- renderPlot({
            SNP <- c(21618,21632,21987,22200,22578,22674,22679,22686,22688,22775,22786,22792,22813,22882,22992,22995,23013,23040,23055,23063,23075,23403,23525,23599,23604,
                     23694,23700,23706,23854,23948,24424,24469,25000)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=21563,end=25383,
                                                      width = 3821,
                                                      names = "S"))
            features$fill <- brewer.pal(11,"Spectral")[2]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF3a"){
          output$mutation <- renderPlot({
            SNP <- c(25584,26060)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=25393,end=26220,
                                                      width = 828,
                                                      names = "ORF3a"))
            features$fill <- brewer.pal(11,"Spectral")[3]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "E"){
          output$mutation <- renderPlot({
            SNP <- c(26270)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=26245,end=26472,
                                                      width = 228,
                                                      names = "E"))
            features$fill <- brewer.pal(11,"Spectral")[4]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "M"){
          output$mutation <- renderPlot({
            SNP <- c(26577,26709,26858)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=26523,end=27191,
                                                      width = 669,
                                                      names = "M"))
            features$fill <- brewer.pal(11,"Spectral")[5]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF6"){
          output$mutation <- renderPlot({
            SNP <- c(27259,27382,27383,27384)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27202,end=27387,
                                                      width = 186,
                                                      names = "ORF6"))
            features$fill <- brewer.pal(11,"Spectral")[6]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF7a"){
          output$mutation <- renderPlot({
            SNP <- c(27807)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27394,end=27759,
                                                      width = 366,
                                                      names = "ORF7a"))
            features$fill <- brewer.pal(11,"Spectral")[7]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF7b"){
          output$mutation <- renderPlot({
            SNP <- c(27807)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27756,end=27887,
                                                      width = 132,
                                                      names = "ORF7b"))
            features$fill <- brewer.pal(11,"Spectral")[8]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF8"){
          output$mutation <- renderPlot({
            SNP <- c(27954)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27894,end=28259,
                                                      width = 366,
                                                      names = "ORF8"))
            features$fill <- brewer.pal(11,"Spectral")[9]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "N"){
          output$mutation <- renderPlot({
            SNP <- c(28311,28361,28367,28881,28882,28883,29510)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=28274,end=29533,
                                                      width = 1260,
                                                      names = "N"))
            features$fill <- brewer.pal(11,"Spectral")[10]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF10"){
          output$mutation <- renderPlot({
            SNP <- c(29733)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=29558,end=29764,
                                                      width = 207,
                                                      names = "ORF10"))
            features$fill <- brewer.pal(11,"Spectral")[11]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
      }
      
      if (selected_variant == "Delta" && selected_object == "Kinds of Mutations"){
        output$mutation <- renderPlot({
          synony <- read.csv(".//data//delta_synony.csv",row.names = 1)
          synony_sums <- colSums(synony)
          type <- c("synonymous mutation","missense mutation","stop gained mutation")
          new_synony <- data.frame(type,synony_sums)
          new_synony <- new_synony %>%
            mutate(percent = synony_sums/sum(synony_sums)*100)
          ggplot(new_synony, aes(x = "", y = percent, fill = type)) +
            geom_bar(stat = "identity", color = "black") +
            geom_text(aes(label = sprintf("%.1f%%",percent)),position=position_stack(vjust=0.5))+
            coord_polar(theta = "y")+
            labs(title = "Mutation of Synonymous", x = " ", y = " ") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              legend.position ="bottom"
            )
        })
      }
      
      if (selected_variant == "Omicron" && selected_object == "Changes of Bases") {
        
        output$mutation <- renderPlot({
          acgt <- read.csv(".//data//omicron_acgt.csv",row.names = 1)
          
          sums <- colSums(acgt)[1:12]
          base_kinds <- c("A_C","A_T","A_G","C_A","C_T","C_G","G_A","G_C","G_T","T_A","T_C","T_G")
          new_acgt <- data.frame(base_kinds,sums)
          
          ggplot(new_acgt, aes(x = base_kinds, y = sums, fill = base_kinds)) +
            geom_bar(stat = "identity", color = "black") +
            labs(title = "Mutation of Nuclotide", x = "Mutation Type", y = "Number") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
            )
        })
      }
      
      if (selected_variant == "Omicron" && selected_object == "Changes of Proteins") {
        output$mutation <- renderPlot({
          amino <- read.csv(".//data//omicron_amino.csv",row.names = 1)
          
          amino_sums <- colSums(amino)
          proteins <- c("ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10")
          new_amino <- data.frame(proteins,amino_sums)
          new_amino$proteins <- factor(new_amino$proteins,levels = proteins)
          ggplot(new_amino, aes(x = proteins, y = amino_sums, fill = proteins)) +
            geom_bar(stat = "identity", color = "black") +
            labs(title = "Mutation of Protein", x = "Protein", y = "Number") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.text.x = element_text(angle = 45, hjust = 1),
            )
        })
      }
      
      if (selected_variant == "Omicron" && selected_object == "Changes of Positions") {
        
        if (selected_protein == "All"){
          output$mutation <- renderPlot({
            SNP <- c(21,25,241,2095,2832,3037,3241,3544,4886,5386,5393,5924,6512,6515,7615,8393,10029,10449,11282,11291,11471,11537,12670,13195,14004,14408,15714,17410,18163,18402,
                     19955,20055,21618,21632,21987,22200,22578,22674,22679,22686,22688,22775,22786,22792,22813,22882,22992,22995,23013,23040,23055,23063,23075,23403,23525,23599,23604,
                     23694,23700,23706,23854,23948,24424,24469,25000,25584,26060,26270,26577,26709,26858,27259,27382,27383,27384,27807,27954,28271,28311,28361,28367,28881,28882,28883,29510,29733)
            variant <- GRanges("NC_045512.2", IRanges(SNP, width=1, names=SNP)) # 设置棒棒图位置
            features <- GRanges("NC_045512.2", IRanges(c(0, 21563, 25393,26245,26523,27202,27394,27756,27894,28274,29558), # 设置block起使位置
                                                       width=c(21563,3821,827,227,668,185,365,131,365,1259,116), # 设置block 的长度
                                                       names=c("ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10"))) # 设置名字
            features$fill <- brewer.pal(11,"Spectral") #块的颜色
            variant$color <- sample.int(length(SNP), length(SNP)) #棒子上面的球的颜色
            
            
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE) #棒子的颜色
            variant$label <- as.character(1:length(variant)) #球内的字符
            variant$label.col <- "black" #球内的标签的颜色
            features2 <-features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2, type="pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF1ab"){
          output$mutation <- renderPlot({
            SNP <- c(21,25,241,2095,2832,3037,3241,3544,4886,5386,5393,5924,6512,6515,7615,8393,10029,10449,11282,11291,11471,11537,12670,13195,14004,14408,15714,17410,18163,18402,
                     19955,20055)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=266,end=21555,
                                                      width = 21290,
                                                      names = "ORF1ab"))
            features$fill <- brewer.pal(11,"Spectral")[1]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "S"){
          output$mutation <- renderPlot({
            SNP <- c(21618,21632,21987,22200,22578,22674,22679,22686,22688,22775,22786,22792,22813,22882,22992,22995,23013,23040,23055,23063,23075,23403,23525,23599,23604,
                     23694,23700,23706,23854,23948,24424,24469,25000)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=21563,end=25383,
                                                      width = 3821,
                                                      names = "S"))
            features$fill <- brewer.pal(11,"Spectral")[2]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF3a"){
          output$mutation <- renderPlot({
            SNP <- c(25584,26060)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=25393,end=26220,
                                                      width = 828,
                                                      names = "ORF3a"))
            features$fill <- brewer.pal(11,"Spectral")[3]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "E"){
          output$mutation <- renderPlot({
            SNP <- c(26270)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=26245,end=26472,
                                                      width = 228,
                                                      names = "E"))
            features$fill <- brewer.pal(11,"Spectral")[4]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "M"){
          output$mutation <- renderPlot({
            SNP <- c(26577,26709,26858)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=26523,end=27191,
                                                      width = 669,
                                                      names = "M"))
            features$fill <- brewer.pal(11,"Spectral")[5]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF6"){
          output$mutation <- renderPlot({
            SNP <- c(27259,27382,27383,27384)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27202,end=27387,
                                                      width = 186,
                                                      names = "ORF6"))
            features$fill <- brewer.pal(11,"Spectral")[6]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF7a"){
          output$mutation <- renderPlot({
            SNP <- c(27807)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27394,end=27759,
                                                      width = 366,
                                                      names = "ORF7a"))
            features$fill <- brewer.pal(11,"Spectral")[7]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF7b"){
          output$mutation <- renderPlot({
            SNP <- c(27807)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27756,end=27887,
                                                      width = 132,
                                                      names = "ORF7b"))
            features$fill <- brewer.pal(11,"Spectral")[8]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF8"){
          output$mutation <- renderPlot({
            SNP <- c(27954)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27894,end=28259,
                                                      width = 366,
                                                      names = "ORF8"))
            features$fill <- brewer.pal(11,"Spectral")[9]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "N"){
          output$mutation <- renderPlot({
            SNP <- c(28311,28361,28367,28881,28882,28883,29510)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=28274,end=29533,
                                                      width = 1260,
                                                      names = "N"))
            features$fill <- brewer.pal(11,"Spectral")[10]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
        if (selected_protein == "ORF10"){
          output$mutation <- renderPlot({
            SNP <- c(29733)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=29558,end=29764,
                                                      width = 207,
                                                      names = "ORF10"))
            features$fill <- brewer.pal(11,"Spectral")[11]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
              features2 <- features
              variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
              lolliplot(variant,features2,type = "pie",cex = 1)
          })
        }
        
      }
      
      if (selected_variant == "Omicron" && selected_object == "Kinds of Mutations"){
        output$mutation <- renderPlot({
          synony <- read.csv(".//data//omicron_synony.csv",row.names = 1)
          synony_sums <- colSums(synony)
          type <- c("synonymous mutation","missense mutation","stop gained mutation")
          new_synony <- data.frame(type,synony_sums)
          new_synony <- new_synony %>%
            mutate(percent = synony_sums/sum(synony_sums)*100)
          ggplot(new_synony, aes(x = "", y = percent, fill = type)) +
            geom_bar(stat = "identity", color = "black") +
            geom_text(aes(label = sprintf("%.1f%%",percent)),position=position_stack(vjust=0.5))+
            coord_polar(theta = "y")+
            labs(title = "Mutation of Synonymous", x = " ", y = " ") +
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              legend.position ="bottom"
            )
        })
      }
      
      
    })
    
    observeEvent(input$display_mutations,{
      
      select_variant <- input$variant_of_SARS_CoV_2
      
      if (select_variant == "Alpha"){
        output$table_mutation <- renderDT({
          table <- read.csv(".//data//alpha_variant.csv")
          datatable(table,extensions = "Buttons",options = list(pageLength = 5,dom = 'Bfrtip', buttons = c("copy","csv","excel","pdf","print")))
        })
      }
      
      if (select_variant == "Beta"){
        output$table_mutation <- renderDT({
          table <- read.csv(".//data//beta_variant.csv")
          datatable(table,extensions = "Buttons",options = list(pageLength = 5,dom = 'Bfrtip', buttons = c("copy","csv","excel","pdf","print")))
        })
      }
      
      if (select_variant == "Delta"){
        output$table_mutation <- renderDT({
          table <- read.csv(".//data//delta_variant.csv")
          datatable(table,extensions = "Buttons",options = list(pageLength = 5,dom = 'Bfrtip', buttons = c("copy","csv","excel","pdf","print")))
        })
      }
      
      if (select_variant == "Omicron"){
        output$table_mutation <- renderDT({
          table <- read.csv(".//data//omicron_variant.csv")
          datatable(table,extensions = "Buttons",
                    options = list(pageLength = 5,dom = 'Bfrtip',buttons = c("copy","csv","excel","pdf","print")))
        })
      }
      
    })
    
    output$download_table <- downloadHandler(
      filename = function(){
        paste("mutation-",Sys.Date(),".csv",sep="")
      },
      content = function(file){
        write.csv(table,file)
      }
    )
    
    observeEvent(input$display_coinfection_pathogen,{
      
      select_pathogen <- input$pathogens_kinds
      
      if (select_pathogen == "All"){
        output$coinfection_pathogen <- renderPlotly({
          
          genus <- read.csv('.//data//pathogens.csv')
          
          # 整合分类和丰度的嵌套关系，构建桑葚图link列表
          species_genus <- genus[c('Genus','Species','abundance')]
          names(species_genus) <- c('source','target','abundance')
          genus_family <- aggregate(genus$abundance,by = list(genus$Family,genus$Genus),FUN = sum)
          names(genus_family) <- c('source','target','abundance')
          family_order <- aggregate(genus$abundance,by = list(genus$Order,genus$Family),FUN = sum)
          names(family_order) <- c('source','target','abundance')
          order_class <- aggregate(genus$abundance,by = list(genus$Class,genus$Order),FUN = sum)
          names(order_class) <- c('source','target','abundance')
          class_phylum <- aggregate(genus$abundance,by = list(genus$Phylum,genus$Class),FUN = sum)
          names(class_phylum) <- c('source','target','abundance')
          superkingdom_phylum <-aggregate(genus$abundance,by = list(genus$Superkingdom,genus$Phylum),FUN = sum)
          names(superkingdom_phylum) <- c('source','target','abundance')
          link_list <- rbind(superkingdom_phylum,class_phylum,order_class,family_order,genus_family,species_genus)
          
          # 构建node列表，并为link列表中的分类名称指配id指代
          node_list <- reshape2::melt(genus,id = 'abundance')
          node_list <- node_list[!duplicated(node_list$value),]
          #head(node_list)#上一级和下一级分类的对应关系及丰度信息列表
          
          link_list$IDsource <- match(link_list$source,node_list$value) -1
          link_list$IDtarget <- match(link_list$target,node_list$value) -1
          #head(link_list)#层级水平和具体的分类名
          node_colors <- c('#BC80BD','#BC80BD','#BC80BD','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00')
          #node_color_mapping <- setNames(node_colors,unique(node_list$value))
          plot_ly(
            type = "sankey",orientation = "h",
            node = list(
              label = node_list$value,
              pad = 5, thickness = 20,
              line = list(color = "black", width = 0.5),
              color = node_colors
            ),
            
            link = list(
              source = link_list$IDsource, target = link_list$IDtarget,
              value = link_list$abundance
            )
          )
          
        })
      }
      
      if (select_pathogen == "Eukaryota"){
        output$coinfection_pathogen <- renderPlotly({
          
          genus <- read.csv('.//data//eukaryota.csv')
          
          # 整合分类和丰度的嵌套关系，构建桑葚图link列表
          species_genus <- genus[c('Genus','Species','abundance')]
          names(species_genus) <- c('source','target','abundance')
          genus_family <- aggregate(genus$abundance,by = list(genus$Family,genus$Genus),FUN = sum)
          names(genus_family) <- c('source','target','abundance')
          family_order <- aggregate(genus$abundance,by = list(genus$Order,genus$Family),FUN = sum)
          names(family_order) <- c('source','target','abundance')
          order_class <- aggregate(genus$abundance,by = list(genus$Class,genus$Order),FUN = sum)
          names(order_class) <- c('source','target','abundance')
          class_phylum <- aggregate(genus$abundance,by = list(genus$Phylum,genus$Class),FUN = sum)
          names(class_phylum) <- c('source','target','abundance')
          superkingdom_phylum <-aggregate(genus$abundance,by = list(genus$Superkingdom,genus$Phylum),FUN = sum)
          names(superkingdom_phylum) <- c('source','target','abundance')
          link_list <- rbind(superkingdom_phylum,class_phylum,order_class,family_order,genus_family,species_genus)
          
          # 构建node列表，并为link列表中的分类名称指配id指代
          node_list <- reshape2::melt(genus,id = 'abundance')
          node_list <- node_list[!duplicated(node_list$value),]
          #head(node_list)#上一级和下一级分类的对应关系及丰度信息列表
          
          link_list$IDsource <- match(link_list$source,node_list$value) -1
          link_list$IDtarget <- match(link_list$target,node_list$value) -1
          #head(link_list)#层级水平和具体的分类名
          node_colors <- c('#BC80BD','#FDB462','#FDB462','#FDB462','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00')
          #node_color_mapping <- setNames(node_colors,unique(node_list$value))
          plot_ly(
            type = "sankey",orientation = "h",
            node = list(
              label = node_list$value,
              pad = 5, thickness = 20,
              line = list(color = "black", width = 0.5),
              color = node_colors
            ),
            
            link = list(
              source = link_list$IDsource, target = link_list$IDtarget,
              value = link_list$abundance
            )
          )
          
        })
      }
      
      if (select_pathogen == "Bacteria"){
        output$coinfection_pathogen <- renderPlotly({
          
          genus <- read.csv('.//data//bacteria.csv')
          
          # 整合分类和丰度的嵌套关系，构建桑葚图link列表
          species_genus <- genus[c('Genus','Species','abundance')]
          names(species_genus) <- c('source','target','abundance')
          genus_family <- aggregate(genus$abundance,by = list(genus$Family,genus$Genus),FUN = sum)
          names(genus_family) <- c('source','target','abundance')
          family_order <- aggregate(genus$abundance,by = list(genus$Order,genus$Family),FUN = sum)
          names(family_order) <- c('source','target','abundance')
          order_class <- aggregate(genus$abundance,by = list(genus$Class,genus$Order),FUN = sum)
          names(order_class) <- c('source','target','abundance')
          class_phylum <- aggregate(genus$abundance,by = list(genus$Phylum,genus$Class),FUN = sum)
          names(class_phylum) <- c('source','target','abundance')
          superkingdom_phylum <-aggregate(genus$abundance,by = list(genus$Superkingdom,genus$Phylum),FUN = sum)
          names(superkingdom_phylum) <- c('source','target','abundance')
          link_list <- rbind(superkingdom_phylum,class_phylum,order_class,family_order,genus_family,species_genus)
          
          # 构建node列表，并为link列表中的分类名称指配id指代
          node_list <- reshape2::melt(genus,id = 'abundance')
          node_list <- node_list[!duplicated(node_list$value),]
          #head(node_list)#上一级和下一级分类的对应关系及丰度信息列表
          
          link_list$IDsource <- match(link_list$source,node_list$value) -1
          link_list$IDtarget <- match(link_list$target,node_list$value) -1
          #head(link_list)#层级水平和具体的分类名
          node_colors <- c('#BC80BD','#FDB462','#FDB462','#FDB462','#FDB462','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00')
          #node_color_mapping <- setNames(node_colors,unique(node_list$value))
          plot_ly(
            type = "sankey",orientation = "h",
            node = list(
              label = node_list$value,
              pad = 5, thickness = 20,
              line = list(color = "black", width = 0.5),
              color = node_colors
            ),
            
            link = list(
              source = link_list$IDsource, target = link_list$IDtarget,
              value = link_list$abundance
            )
          )
          
        })
      }
      
      if (select_pathogen == "Virus"){
        output$coinfection_pathogen <- renderPlotly({
          
          genus <- read.csv('.//data//virus.csv')
          
          # 整合分类和丰度的嵌套关系，构建桑葚图link列表
          species_genus <- genus[c('Genus','Species','abundance')]
          names(species_genus) <- c('source','target','abundance')
          genus_family <- aggregate(genus$abundance,by = list(genus$Family,genus$Genus),FUN = sum)
          names(genus_family) <- c('source','target','abundance')
          family_order <- aggregate(genus$abundance,by = list(genus$Order,genus$Family),FUN = sum)
          names(family_order) <- c('source','target','abundance')
          order_class <- aggregate(genus$abundance,by = list(genus$Class,genus$Order),FUN = sum)
          names(order_class) <- c('source','target','abundance')
          class_phylum <- aggregate(genus$abundance,by = list(genus$Phylum,genus$Class),FUN = sum)
          names(class_phylum) <- c('source','target','abundance')
          superkingdom_phylum <-aggregate(genus$abundance,by = list(genus$Superkingdom,genus$Phylum),FUN = sum)
          names(superkingdom_phylum) <- c('source','target','abundance')
          link_list <- rbind(superkingdom_phylum,class_phylum,order_class,family_order,genus_family,species_genus)
          
          # 构建node列表，并为link列表中的分类名称指配id指代
          node_list <- reshape2::melt(genus,id = 'abundance')
          node_list <- node_list[!duplicated(node_list$value),]
          #head(node_list)#上一级和下一级分类的对应关系及丰度信息列表
          
          link_list$IDsource <- match(link_list$source,node_list$value) -1
          link_list$IDtarget <- match(link_list$target,node_list$value) -1
          #head(link_list)#层级水平和具体的分类名
          node_colors <- c('#BC80BD','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#FDB462','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#80B1D3','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#FB8072','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#8DD3C7','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#FFFFB3','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00','#458B00')
          #node_color_mapping <- setNames(node_colors,unique(node_list$value))
          plot_ly(
            type = "sankey",orientation = "h",
            node = list(
              label = node_list$value,
              pad = 5, thickness = 20,
              line = list(color = "black", width = 0.5),
              color = node_colors
            ),
            
            link = list(
              source = link_list$IDsource, target = link_list$IDtarget,
              value = link_list$abundance
            )
          )
          
        })
      }
    })
    
    observeEvent(input$vcf_file,{
      vcf <- read.vcfR(input$vcf_file$datapath)
    })
    
    observeEvent(input$analyses_results,{
      
      selected_object <- input$analysis_object_of_SARS_CoV_2
      selected_protein <- input$analysis_protein_of_SARS_CoV_2
      
      vcf <- read.vcfR(input$vcf_file$datapath)
      variants_table <- as.data.frame(vcf@fix)
      num_rows <- nrow(variants_table)
      mutation_table <- data.frame(
        Num = 1:num_rows
      )
      protein_mapping <- c(
        "Ala" = "A", "Phe" = "F", "Cys" = "C", "Asp" = "D", "Asn" = "N",
        "Glu" = "E", "Gln" = "Q", "Gly" = "G", "His" = "H", "Leu" = "L",
        "Ile" = "I", "Lys" = "K", "Met" = "M", "Pro" = "P", "Arg" = "R",
        "Ser" = "S", "Thr" = "T", "Val" = "V", "Trp" = "W", "Tyr" = "Y"
      )
      proteins <- c()
      pro_change <- c()
      synony <- c()
      pro_influ <- c()
      for (i in 1:num_rows){
        pro <- strsplit(variants_table$INFO,"\\|")[[i]][4]
        change <- strsplit(variants_table$INFO,"\\|")[[i]][11]
        for (key in names(protein_mapping)){
          change <- gsub(key,protein_mapping[key],change)
        }
        change <- gsub("p.","",change)
        syno <- strsplit(variants_table$INFO,"\\|")[[i]][2]
        influ <- strsplit(variants_table$INFO,"\\|")[[i]][3]
        proteins <- append(proteins,pro)
        pro_change <- append(pro_change,change)
        synony <- append(synony,syno)
        pro_influ <- append(pro_influ,influ)
      }
      mutation_table$Position <- variants_table$POS
      mutation_table$Protein <- proteins
      mutation_table$Ref_Variant <- paste0(variants_table$REF,sep="-",variants_table$ALT)
      mutation_table$Ref_Variant_Protein <- pro_change
      mutation_table$Synonymous <- synony
      mutation_table$Influence <- pro_influ
      
      if (selected_object == "Changes of Bases"){
        output$vcf_mutation <- renderPlot({
          A_C <- c()
          A_T <- c()
          A_G <- c()
          C_A <- c()
          C_T <- c()
          C_G <- c()
          T_A <- c()
          T_C <- c()
          T_G <- c()
          G_A <- c()
          G_C <- c()
          G_T <- c()
          InDel <- c()
          for (i in mutation_table$Ref_Variant){
            if (i == "A-C"){
              A_C <- append(A_C,i)
            }else if (i =="A-T"){
              A_T <- append(A_T,i)
            }else if (i =="A-G"){
              A_G <- append(A_G,i)
            }else if (i =="C-A"){
              C_A <- append(C_A,i)
            }else if (i =="C-T"){
              C_T <- append(C_T,i)
            }else if (i =="C-G"){
              C_G <- append(C_G,i)
            }else if (i =="T-A"){
              T_A <- append(T_A,i)
            }else if (i =="T-C"){
              T_C <- append(T_C,i)
            }else if (i =="T-G"){
              T_G <- append(T_G,i)
            }else if (i =="G-A"){
              G_A <- append(G_A,i)
            }else if (i =="G_C"){
              G_C <- append(G_C,i)
            }else if (i =="G-T"){
              G_T <- append(G_T,i)
            }else {
              InDel <- append(InDel,i)
            }
          }
          base <- c("A-C","A-T","A-G","C-A","C-T","C-G","T-A","T-C","T-G","G-A","G-C","G-T","InDel")
          ba_nums <- c(length(A_C),length(A_T),length(A_G),length(C_A),length(C_T),length(C_G),
                       length(T_A),length(T_C),length(T_G),length(G_A),length(G_C),length(G_T),length(InDel))
          base_nums <- data.frame(base,ba_nums)
          base_nums$base <- factor(base_nums$base,levels = base)
          ggplot(base_nums, aes(x = base, y = ba_nums, fill = base)) +
            geom_bar(stat = "identity", color = "black") +
            labs(title = "Mutation of Base", x = "Base", y = "Number") +
            geom_text(aes(label = ba_nums),vjust = -0.5)+
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.text.x = element_text(angle = 45, hjust = 1),
            )
        })
        
      }
      
      if (selected_object == "Changes of Proteins"){
        output$vcf_mutation <- renderPlot({
          ORF1ab <- c()
          S <- c()
          ORF3a <- c()
          E <- c()
          M <- c()
          ORF6 <- c()
          ORF7a <- c()
          ORF7b <- c()
          ORF8 <- c()
          N <- c()
          ORF10 <- c()
          for (i in mutation_table$Position){
            if (i < 21555){
              ORF1ab <- append(ORF1ab,i)
            }
            else if( 21563 < i & i< 25384){
              S <- append(S,i)
            }
            else if( 25393 < i & i< 26220){
              ORF3a <- append(ORF3a,i)
            }
            else if( 26245 < i & i< 26472){
              E <- append(E,i)
            }
            else if( 26523 < i & i< 27191){
              M <- append(M,i)
            }
            else if( 27202 < i & i< 27387){
              ORF6 <- append(ORF6,i)
            }
            else if( 27394 < i & i< 27759){
              ORF7a <- append(ORF7a,i)
            }
            else if( 27756 < i & i< 27887){
              ORF7b <- append(ORF7b,i)
            }
            else if( 27894 < i & i< 28259){
              ORF8 <- append(ORF8,i)
            }
            else if( 28274 < i & i< 29533){
              N <- append(N,i)
            }
            else if( 29558 < i & i< 29674){
              ORF10 <- append(ORF10,i)
            }
          }
          pros <- c("ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10")
          nums <- c(length(ORF1ab),length(S),length(ORF3a),length(E),length(M),
                    length(ORF6),length(ORF7a),length(ORF7b),length(ORF8),length(N),length(ORF10))
          pro_nums <- data.frame(pros,nums)
          pro_nums$pros <- factor(pro_nums$pros,levels = pros)
          ggplot(pro_nums, aes(x = pros, y = nums, fill = pros)) +
            geom_bar(stat = "identity", color = "black") +
            labs(title = "Mutation of Protein", x = "Protein", y = "Number") +
            geom_text(aes(label = nums),vjust = -0.5)+
            theme_minimal()+
            theme(
              plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
              axis.text.x = element_text(angle = 45, hjust = 1),
            )
        })
        
      }
      
      if (selected_object == "Changes of Positions"){
        
        ORF1ab = c()
        S = c()
        ORF3a = c()
        E = c()
        M = c()
        ORF6 = c()
        ORF7a = c()
        ORF7b = c()
        ORF8 = c()
        N = c()
        ORF10 = c()
        for (i in mutation_table$Position){
          if (i < 21555){
            ORF1ab <- append(ORF1ab,i)
          }
          else if( 21563 < i & i< 25384){
            S <- append(S,i)
          }
          else if( 25393 < i & i< 26220){
            ORF3a <- append(ORF3a,i)
          }
          else if( 26245 < i & i< 26472){
            E <- append(E,i)
          }
          else if( 26523 < i & i< 27191){
            M <- append(M,i)
          }
          else if( 27202 < i & i< 27387){
            ORF6 <- append(ORF6,i)
          }
          else if( 27394 < i & i< 27759){
            ORF7a <- append(ORF7a,i)
          }
          else if( 27756 < i & i< 27887){
            ORF7b <- append(ORF7b,i)
          }
          else if( 27894 < i & i< 28259){
            ORF8 <- append(ORF8,i)
          }
          else if( 28274 < i & i< 29533){
            N <- append(N,i)
          }
          else if( 29558 < i & i< 29674){
            ORF10 <- append(ORF10,i)
          }
        }
        if (selected_protein == "All"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(mutation_table$Position)
            variant <- GRanges("NC_045512.2", IRanges(SNP, width=1, names=paste0(SNP))) # 设置棒棒图位置
            features <- GRanges("NC_045512.2", IRanges(c(0, 21563, 25393,26245,26523,27202,27394,27756,27894,28274,29558), # 设置block起使位置
                                                       width=c(21563,3821,827,227,668,185,365,131,365,1259,116), # 设置block 的长度
                                                       names=c("ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10"))) # 设置名字
            
            features$fill <- brewer.pal(11,"Spectral") #块的颜色
            variant$color <- sample.int(length(SNP), length(SNP)) #棒子上面的球的颜色
            
            
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE) #棒子的颜色
            variant$label <- as.character(1:length(variant)) #球内的字符
            variant$label.col <- "black" #球内的标签的颜色
            features2 <-features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2, type="pie",cex = 1)
          })
          
        }
        if (selected_protein == "ORF1ab"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(ORF1ab)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=266,end=21555,
                                                      width = 21290,
                                                      names = "ORF1ab"))
            features$fill <- brewer.pal(11,"Spectral")[1]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
          
        }
        if (selected_protein == "S"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(S)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=21563,end=25383,
                                                      width = 3821,
                                                      names = "S"))
            features$fill <- brewer.pal(11,"Spectral")[2]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
          
        }
        if (selected_protein == "ORF3a"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(ORF3a)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=25393,end=26220,
                                                      width = 828,
                                                      names = "ORF3a"))
            features$fill <- brewer.pal(11,"Spectral")[3]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
          
        }
        if (selected_protein == "E"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(E)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=26245,end=26472,
                                                      width = 228,
                                                      names = "E"))
            features$fill <- brewer.pal(11,"Spectral")[4]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
          
        }
        if (selected_protein == "M"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(M)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=26523,end=27191,
                                                      width = 669,
                                                      names = "M"))
            features$fill <- brewer.pal(11,"Spectral")[5]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
          
        }
        if (selected_protein == "ORF6"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(ORF6)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27202,end=27387,
                                                      width = 186,
                                                      names = "ORF6"))
            features$fill <- brewer.pal(11,"Spectral")[6]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
          
        }
        if (selected_protein == "ORF7a"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(ORF7a)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27394,end=27759,
                                                      width = 366,
                                                      names = "ORF7a"))
            features$fill <- brewer.pal(11,"Spectral")[7]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
          
        }
        if (selected_protein == "ORF7b"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(ORF7b)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27756,end=27887,
                                                      width = 132,
                                                      names = "ORF7b"))
            features$fill <- brewer.pal(11,"Spectral")[8]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
          
        }
        if (selected_protein == "ORF8"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(ORF8)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=27894,end=28259,
                                                      width = 366,
                                                      names = "ORF8"))
            features$fill <- brewer.pal(11,"Spectral")[9]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
          
        }
        if (selected_protein == "N"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(N)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=28274,end=29533,
                                                      width = 1260,
                                                      names = "N"))
            features$fill <- brewer.pal(11,"Spectral")[10]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
          
        }
        if (selected_protein == "ORF10"){
          output$vcf_mutation <- renderPlot({
            SNP <- as.numeric(ORF10)
            variant <- GRanges("NC_045512.2",IRanges(SNP,width = 1,names = SNP))
            features <- GRanges("NC_045512.2",IRanges(start=29558,end=29764,
                                                      width = 207,
                                                      names = "ORF10"))
            features$fill <- brewer.pal(11,"Spectral")[11]
            variant$color <- sample.int(length(SNP),length(SNP))
            variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE)
            variant$label <- as.character(1:length(variant))
            variant$label.col <- "black"
            features2 <- features
            variant$SNPsideID <- sample(c("top","bottom"),length(variant),replace=TRUE)
            lolliplot(variant,features2,type = "pie",cex = 1)
          })
          
        }
      }
      
      if (selected_object == "Kinds of Mutations"){
        output$vcf_mutation <- renderPlot({
          counts <- table(mutation_table$Synonymous)
          counts <- data.frame(counts)
          colnames(counts)[1] <- "type"
          counts$percentage <- counts$Freq/sum(counts$Freq)*100
          ggplot(counts, aes(x = "", y = percentage, fill = type)) +
            geom_bar(stat = "identity",color = "black") +
            coord_polar(theta = "y") +
            scale_fill_manual(values = c('#BC80BD', '#FDB462', '#80B1D3', '#FB8072', '#8DD3C7', '#FFFFB3','#458B00','#8B0000'))+
            labs(title = "Kinds of Mutation", x = "" ,y = "") +
            theme_minimal() +
            geom_text(aes(label = sprintf("%.1f%%", percentage)),position = position_stack(vjust = 0.5),size = 3,check_overlap = TRUE)+
            theme(legend.position = "bottom") + # 移动图例位置
            theme_void()
        })
        
      }
    })
    
    observeEvent(input$analyses_table_results,{
      
      vcf <- read.vcfR(input$vcf_file$datapath)
      variants_table <- as.data.frame(vcf@fix)
      num_rows <- nrow(variants_table)
      mutation_table <- data.frame(
        Num = 1:num_rows
      )
      protein_mapping <- c(
        "Ala" = "A", "Phe" = "F", "Cys" = "C", "Asp" = "D", "Asn" = "N",
        "Glu" = "E", "Gln" = "Q", "Gly" = "G", "His" = "H", "Leu" = "L",
        "Ile" = "I", "Lys" = "K", "Met" = "M", "Pro" = "P", "Arg" = "R",
        "Ser" = "S", "Thr" = "T", "Val" = "V", "Trp" = "W", "Tyr" = "Y"
      )
      proteins <- c()
      pro_change <- c()
      synony <- c()
      pro_influ <- c()
      for (i in 1:num_rows){
        pro <- strsplit(variants_table$INFO,"\\|")[[i]][4]
        change <- strsplit(variants_table$INFO,"\\|")[[i]][11]
        for (key in names(protein_mapping)){
          change <- gsub(key,protein_mapping[key],change)
        }
        change <- gsub("p.","",change)
        syno <- strsplit(variants_table$INFO,"\\|")[[i]][2]
        influ <- strsplit(variants_table$INFO,"\\|")[[i]][3]
        proteins <- append(proteins,pro)
        pro_change <- append(pro_change,change)
        synony <- append(synony,syno)
        pro_influ <- append(pro_influ,influ)
      }
      mutation_table$Position <- variants_table$POS
      mutation_table$Protein <- proteins
      mutation_table$Ref_Variant <- paste0(variants_table$REF,sep="-",variants_table$ALT)
      mutation_table$Ref_Variant_Protein <- pro_change
      mutation_table$Synonymous <- synony
      mutation_table$Influence <- pro_influ
      
      output$vcf_table_mutation <- renderDT(mutation_table)
      
      
    })
    
  }