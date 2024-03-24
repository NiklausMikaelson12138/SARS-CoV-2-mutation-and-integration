library(ggplot2)
library(circlize)  #圈图绘制
library(ComplexHeatmap)  #这个包绘制图例
library(grid)  #组合圈图和图例
library(png)
library(pdftools)
library(vcfR)
library(dplyr)
library(Seurat)
library(metacoder)
library(plotly)
library(trackViewer) #主要画图包
library(RColorBrewer)

acgt <- read.csv("F://张二爷//开题//毕业论文//3.nextflow流程研究//2.结果//alpha_acgt.csv",row.names = 1)

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

amino <- read.csv("F://张二爷//开题//毕业论文//3.nextflow流程研究//2.结果//alpha_amino.csv",row.names = 1)

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

position <- read.csv("F://张二爷//开题//毕业论文//3.nextflow流程研究//2.结果//alpha_position_all_variant.csv",row.names = 1)
# 读取数据
seq_stat <- read.delim('F://张二爷//开题//毕业论文//1.变异与多态性研究//2.结果//1.突变位点//circle//genome_stat.txt',stringsAsFactors=FALSE)
alpha <- read.delim('F://张二爷//开题//毕业论文//1.变异与多态性研究//2.结果//1.突变位点//circle//alpha_SNV.txt',stringsAsFactors=FALSE)
#统计变异类型
sample_list <- function(dat) {
  dat <- list(dat[which(dat$change == 'A>T'|dat$change == 'T>A'), ],
              dat[which(dat$change == 'A>G'|dat$change == 'T>C'), ],
              dat[which(dat$change == 'A>C'|dat$change == 'T>G'), ],
              dat[which(dat$change == 'G>A'|dat$change == 'C>T'), ],
              dat[which(dat$change == 'G>T'|dat$change == 'C>A'), ],
              dat[which(dat$change == 'G>C'|dat$change == 'C>G'), ],
              dat[which(dat$change == 'Del'), ])
  #dat[which(dat$change == 'Ins'), ])
  return(dat)
}
alpha_list <- sample_list(alpha)

pdf('position.pdf', width = 8, height = 8)
circle_size = unit(1, 'snpc')
circos.par(gap.degree = 3, start.degree = 90)

#染色体区域
circos.genomicInitialize(seq_stat, plotType = c('axis', 'labels'), major.by = 3000, track.height = 0.05)

circos.genomicTrackPlotRegion(
  seq_stat, track.height = 0.05, stack = TRUE, bg.border = NA,
  panel.fun = function(region, value, ...) {
    circos.genomicRect(region, value, col = '#049a0b', border = NA, ...)
  } )

#sample1 SNV
color_assign <- c('#BC80BD', '#FDB462', '#80B1D3', '#FB8072', '#8DD3C7', '#FFFFB3', 'red', 'blue')

circos.genomicTrackPlotRegion(
  alpha_list, track.height = 0.12, bg.border = 'black', bg.lwd = 0.4,
  panel.fun = function(region, value, ...) {
    circos.genomicPoints(region, value, pch = 16, cex = 0.5, col = color_assign[getI(...)], ...)
    circos.yaxis(labels.cex = 0.2, lwd = 0.1, tick.length = convert_x(0.15, 'mm'))
    xlim = CELL_META$xlim
    ylim = CELL_META$ylim
    circos.text(mean(xlim), mean(ylim), 'alpha', cex = 0.7, col = 'black', facing = 'inside', niceFacing = TRUE)
  } )
#图例
snv_legend <- Legend(
  at = c(1, 2, 3, 4, 5, 6, 7, 8),
  labels = c(' SNP: A>T|T>A', ' SNP: A>G|T>C', ' SNP: A>C|T>G', ' SNP: G>A|C>T', ' SNP: G>T|C>A', ' SNP: G>C|C>G', ' InDel: Ins', ' InDel: Del'),
  labels_gp = gpar(fontsize = 6), title = 'variant type', title_gp = gpar(fontsize = 7),
  grid_height = unit(0.4, 'cm'), grid_width = unit(0.4, 'cm'), type = 'points', background = NA,
  legend_gp = gpar(col = c('#BC80BD', '#FDB462', '#80B1D3', '#FB8072', '#8DD3C7', '#FFFFB3', 'red', 'blue')) )

pushViewport(viewport(x = 0.5, y = 0.5))
grid.draw(snv_legend)
upViewport()
circos.clear()
dev.off()
pdf_path <- "F://张二爷//开题//毕业论文//3.nextflow流程研究//3.shiny//circlize3.pdf"
num_pages <- pdf_info(pdf_path)$pages
page_number <- 1
image <- pdf_render_page(pdf_path, page = page_number)
plot(1:2, type = "n", main = "PDF to Image", xlab = "", ylab = "")
rasterImage(image, 1, 1, 2, 2)

synony <- read.csv("F://张二爷//开题//毕业论文//3.nextflow流程研究//2.结果//alpha_synony.csv",row.names = 1)
synony_sums <- colSums(synony)
synony_indicators <- c("synonymous mutation","missense mutation","stop gained mutation")
new_synony <- data.frame(synony_indicators,synony_sums)
ggplot(new_synony, aes(x = synony_indicators, y = synony_sums, fill = synony_indicators)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Mutation of Synonymous", x = "Type", y = "Number") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
    axis.title.x = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
    axis.title.y = element_text(hjust = 0.5, face = "plain", family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1),
  )


library(trackViewer) #主要画图包
library(RColorBrewer) #引入颜色包

SNP <- c(241,913,2453,3037,3267,5388,5986,6174,6954,11287,11291,14408,14676,14925,15906,15279,16176,19551,21764,21770,21990,21992,23063,23271,23403,23604,23709,24506,24914,26467,27972,28048,28095,28111,28270,28280,28281,28282,28881,28882,28883,28977)
variant <- GRanges("NC_045512.2", IRanges(SNP, width=1, names=paste0("MP ", SNP))) # 设置棒棒图位置
features <- GRanges("NC_045512.2", IRanges(c(0, 21563, 25393,26245,26523,27202,27394,27756,27894,28274,29558), # 设置block起使位置
                                           width=c(21563,3821,827,227,668,185,365,131,365,1259,116), # 设置block 的长度
                                           names=c("ORF1ab","S","ORF3a","E","M","ORF6","ORF7a","ORF7b","ORF8","N","ORF10"))) # 设置名字

features$fill <- brewer.pal(11,"Spectral") #块的颜色
sample.gr$color <- sample.int(length(SNP), length(SNP)) #棒子上面的球的颜色


variant$border <- sample(c("grey60", "grey50"), length(SNP), replace=TRUE) #棒子的颜色
variant$label <- as.character(1:length(sample.gr)) #球内的字符
variant$label.col <- "black" #球内的标签的颜色
features2 <-features
variant$SNPsideID <- sample(c("top","bottom"),length(sample.gr),replace=TRUE)
lolliplot(sample.gr,features2, type="pie",cex = 1)



table <- read.csv("F://张二爷//开题//毕业论文//3.nextflow流程研究//3.shiny//alpha_variant.csv")

library(data.table)
library(dplyr)
library(VariantAnnotation)
vcf <- fread("C://Users//acer//Desktop//SRR13907325.sra.call.vcf", header = FALSE, select = c(1, 2, 3, 4, 5, 6, 7, 8, 10),stringsAsFactors = FALSE)
colnames(vcf) <- c("CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT", "Sample")
variants <- extract.gt(vcf)

vcf <- read.vcfR("F://张二爷//开题//毕业论文//1.突变识别预测//突变位点数量//delta_all.vcf")
variants_table <- as.data.frame(vcf@fix)


synony <- read.csv("F://张二爷//开题//毕业论文//3.nextflow流程研究//2.结果//alpha_synony.csv",row.names = 1)
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


genus <- read.csv('F://张二爷//开题//毕业论文//3.nextflow流程研究//3.shiny//eukaryota.csv')

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

# ORF1ab
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

# S
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

# ORF3a
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

# E
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

# M
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

# ORF6
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

# ORF7a
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

# ORF7b
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

# ORF8
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

# N
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



SNP <- c(241,913,2453,3037,3267,5388,5986,6174,6954,11287,11291,14408,14676,14925,15906,15279,16176,19551,21764,21770,21990,21992,23063,23271,23403,23604,23709,24506,24914,26467,27972,28048,28095,28111,28270,28280,28281,28282,28881,28882,28883,28977)
variant <- GRanges("NC_045512.2", IRanges(SNP, width=1, names=paste0("MP ", SNP))) # 设置棒棒图位置
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
  
  
  
vcf <- read.vcfR("F://张二爷//开题//毕业论文//1.突变识别预测//突变位点数量//SRR13907325.sra.call.vcf.csv")
vcf <- read.vcfR("F://张二爷//开题//毕业论文//1.突变识别预测//突变位点数量//SRR16569827.vcf.snpeff.csv")
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
    ORF1ab = append(ORF1ab,i)
  }
  else if( 21563 < i & i< 25384){
    S = append(S,i)
  }
  else if( 25393 < i & i< 26220){
    ORF3a = append(ORF3a,i)
  }
  else if( 26245 < i & i< 26472){
    E = append(E,i)
  }
  else if( 26523 < i & i< 27191){
    M = append(M,i)
  }
  else if( 27202 < i & i< 27387){
    ORF6 = append(ORF6,i)
  }
  else if( 27394 < i & i< 27759){
    ORF7a = append(ORF7a,i)
  }
  else if( 27756 < i & i< 27887){
    ORF7b = append(ORF7b,i)
  }
  else if( 27894 < i & i< 28259){
    ORF8 = append(ORF8,i)
  }
  else if( 28274 < i & i< 29533){
    N = append(N,i)
  }
  else if( 29558 < i & i< 29674){
    ORF10 = append(i)
  }
}

counts <- table(mutation_table$Synonymous)
counts <- data.frame(counts)
colnames(counts)[1] <- "type"
counts$percentage <- counts$Freq/sum(counts$Freq)*100
ggplot(counts, aes(x = "", y = percentage, fill = type)) +
  geom_bar(stat = "identity",color = "black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c('#BC80BD', '#FDB462', '#80B1D3', '#FB8072', '#8DD3C7', '#FFFFB3'))+
  labs(title = "Kinds of Mutation", x = "" ,y = "") +
  theme_minimal() +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),position = position_stack(vjust = 0.5),size = 3,check_overlap = TRUE)+
  theme(legend.position = "bottom") + # 移动图例位置
  theme_void()

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
