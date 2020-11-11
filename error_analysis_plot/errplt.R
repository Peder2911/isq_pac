#!/usr/bin/env R

library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(readr)
library(itertools)
library(ggrepel)
library(labeling)
library(cowplot)

BASE_WIDTH<-4

args <- commandArgs(trailingOnly=TRUE)
if(length(args) < 5){
   stop("
      USAGE: errplt.R [input file] [output file] [prediction column] [outcome column] [name column]
   ")
}

path_csv_input <-  args[1]
path_out <- args[2]

#path_csv_output_stem <- "$PATH_CSV_OUTPUT_STEM"
#filename <- "$FILENAME"

input_model <- args[3] # Pred.
input_actual <- args[4] # Outcomes
input_label <- args[5] # Names

title <- ""


DiagPlot <- function(f,y,labels,
      worstN=10,
      size_adjust=0,
      right_margin=4.5,
      top_margin=1,
      label_spacing=1.1,
      transp_adjust=5,
      lab_adjust=.5,
      text_size=7,
      bw=FALSE,
      title="Model Diagnostic Plot"
  ) {

  #################
  # Set up data.
  #################

  data <- data.frame(f=f, y=y, labels=labels)
  pdata <- data %>%
    mutate(y_minus_f=y-f) %>%
    arrange(f) %>%
    mutate(forecastOrder = row_number())
  
  # Label worstn.
  pdata <- pdata %>%
    group_by(y) %>%
    arrange(desc(abs(y_minus_f))) %>%
    mutate(label_worst=ifelse(row_number()<=worstN, as.character(labels), " "))

  # Create var for absolute errors.
  pdata<-pdata %>% mutate(abserr=abs(y_minus_f))
  # Create indicator for worst values.
  pdata <- pdata %>%
    group_by(y) %>%
    arrange(desc(abs(y_minus_f))) %>%
    mutate(isworstn=ifelse(row_number()<=worstN, 1, 0))

  # Create coloring factor.
  pdata <- pdata %>% mutate(coloring=
                              ifelse(y==1 & isworstn==1, '1w',
                                     ifelse(y==0 & isworstn==1, '0w',
                                            ifelse(y==1 & isworstn==0, '1',
                                                   '0'))))
  # Arrange data for plotting.
  pdata<-pdata%>%arrange(forecastOrder)
  N=nrow(pdata)
  #labbuffer=(nchar(N)-3)*.3
  # Set up coloring.
  yblue=ifelse(bw==F,'#0862ca','#8b8b8b')
  ybluemarg=ifelse(bw==F,yblue,"#989898")
  ybluelite=ifelse(bw==F,'#cddff4','#d8d8d8')
  ybluelitest=ifelse(bw==F,'#f0f5fb','#f2f2f2')
  yred=ifelse(bw==F,'#fd1205','#000000')
  yredmarg=ifelse(bw==F,yred,yred)
  yredlite=ifelse(bw==F,'#fecfdc','#999999')
  yredlitest=ifelse(bw==F,'#fef0f4','#e5e5e5')
  yredliteR<-ifelse(bw==F,'#fd5950','#4c4c4c')
  #y1_litecol<-ifelse(rare==TRUE,yredliteR,yredlite)
  boolcolors<-c(yred, yblue, yredlite, ybluelite)
  names(boolcolors)<-c('1w', '0w', '1', '0')
  boolscale<-scale_color_manual(name='coloring',values=boolcolors)

  ###################
  #initialize plots.
  #   Object "o2" contains the full plot we care about,
  #       minus the lines & labels.
  #   Object "margx" is the marginal on the x axis of f|y=0 & f|y=1
  ###################

  o1 <- ggplot(pdata, aes(x=f,y=forecastOrder,group=y,
                          color=as.factor(coloring),
                          alpha=as.factor(isworstn))) + boolscale
  o2 <- o1 + geom_point() +
        scale_alpha_discrete(range = c(0.1*transp_adjust, 1)) +
        geom_rug(sides="r") + xlim(c(0,1)) + ylim(c(0,N)) + theme_bw() +
        theme(panel.grid.major=element_line(colour='lightgrey'),
              panel.grid.minor=element_line(colour='grey'),
              panel.grid.major.y=element_blank(),
              panel.grid.minor.y=element_blank(),
              panel.grid.minor.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.x=element_blank(),
              legend.position='none',
              plot.margin=unit(c(top_margin,right_margin,0,1),"lines")) +
        labs(y='Observation (ordered by p)') + boolscale
  margx<-ggplot(pdata,aes(f,fill=factor(y), color=factor(y))) +
         geom_density(alpha=.4) +
         scale_fill_manual(values=c(yblue,yredmarg)) +
         scale_color_manual(values=c(yblue,yredmarg)) +
         xlim(c(0,1)) +
         labs(x='Predicted probability (p)') +
         theme_bw(base_rect_size=1) +
         theme(panel.grid.minor=element_blank(),
               panel.grid.major=element_line(colour='lightgrey'),
               panel.grid.major.y=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               legend.position="none",
               plot.margin=unit(c(-0.2,right_margin,0,1),"lines"))

  ###################
  #Lines and Labels
  ###################

  z<-o2
  count0=0
  count1=0
  trigger=FALSE
  pos_length=nrow(pdata[pdata$coloring=="1w",])
  neg_length=nrow(pdata[pdata$coloring=="0w",])
  label_spacing = label_spacing * (nrow(pdata)/25)

  for (i in length(pdata$label_worst):1) {

    ################################
    #Prepare to position labels
    ################################

    text_spacing<-label_spacing

    labeltext<-pdata$label_worst[i]
    if(labeltext == ' '){
      next
    }
    obsy=pdata$y[i]
    if(obsy==0){
      count0<-count0+text_spacing
    }
    if(obsy==1){
      count1<-count1+text_spacing
    }

    if(obsy==0){
      if(count0==text_spacing){
          if(trigger==FALSE){
            y0init=pdata$forecastOrder[i]
            trigger=TRUE
          }
          else{
            y0init=y1init-pos_length*text_spacing
          }
      }
    }

    if(obsy==1){
      if(count1==text_spacing){
          if(trigger==FALSE){
            y1init=pdata$forecastOrder[i]
            trigger=TRUE
          }
          else{
            y1init=y0init-neg_length*text_spacing
          }
      }
    }

    fpos<-pdata$f[i]

    ##############################
    #Set the parameters for labels
    ##############################

    ycolor<-ifelse(obsy==0,yblue,yred)
    ypos_text<-ifelse(obsy==0,
                      (y0init-(count0-text_spacing)),
                      (y1init-(count1-text_spacing))
    )
    ifelse(pdata$forecastOrder[i]>ypos_text,LineSlope<-c(1,0),LineSlope<-c(0,1))
    labjust_left=1.1
    labjust_right=labjust_left+lab_adjust

    ###############################
    #Create the labels on plot
    ###############################
    current<-
      z+
      annotation_custom(
        grob=linesGrob(
          x=c(fpos,1.01),
          y=0,
          gp=gpar(col=ifelse(obsy==0,ybluelitest,yredlitest))
        ),
        ymin=pdata$forecastOrder[i],
        ymax=pdata$forecastOrder[i],
        xmin=0,
        xmax=1
      ) +
      annotation_custom(
        grob=textGrob(label=labeltext,
                      gp=gpar(fontsize=text_size,col=ycolor)),
        ymin=ypos_text,
        ymax=ypos_text,
        xmin=labjust_left,
        xmax=labjust_right
      )+
      annotation_custom(
        grob=linesGrob(
          x=c(1,labjust_left),
          y=LineSlope,
          gp=gpar(col=ycolor)
        ),
        ymin=
          ifelse(
            pdata$forecastOrder[i]<=ypos_text,
            pdata$forecastOrder[i],
            ypos_text),
        ymax=
          ifelse(
            pdata$forecastOrder[i]>ypos_text,
            pdata$forecastOrder[i],
            ypos_text)
      )
    z<-current
  }

  #Turn off clipping so we can render the plot
  gt <- ggplot_gtable(ggplot_build(z))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  o3 <- plot_grid(gt, margx, ncol=1, align='v', axis='lr', rel_heights = c(4, 1.5))
  o4 <- arrangeGrob(o3, top=textGrob(title,gp=gpar(fontsize=8,font=2),just='top'))
  return(o4)
}

df <- readr::read_csv(path_csv_input)

length_label <- max(nchar(pull(df[input_label])))
width = BASE_WIDTH + (0.14 * length_label)
height = width - 0.3
right_margin = 1.5 * width

ptemp = DiagPlot(f=pull(df[input_model]),y=pull(df[input_actual]),
                 labels=pull(df[input_label]), title=title,
                 right_margin=right_margin)

ggsave(path_out, ptemp,
       height=height, width=width, dpi=400)

