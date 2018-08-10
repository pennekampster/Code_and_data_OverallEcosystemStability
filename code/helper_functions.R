
AICtab <- function(mod1, mod2){
  plyr::ldply(list(mod_linear = mod1, mod_nonlinear = mod2), glance, .id="model") %>%  
    mutate(dAIC=AIC-min(AIC))  %>% 
    select(model,dAIC) %>% 
    ## compute AIC 'weights'
    mutate(weight=exp(-dAIC/2), weight=weight/sum(weight)) %>% 
    arrange(dAIC)  
}

minimal_gtable <- function(x){
  
  #x <- summary_linear
  
  # smaller line below header
  tt <- ttheme_minimal(show.rownames = F)
  g <- tableGrob(x, rows=NULL, theme=tt)
  horizontal_line <- replicate(ncol(g),
                               segmentsGrob(x1 = unit(0, "npc"),             
                                            y0 = unit(0,"npc"),
                                            x0 = unit(1, "npc"),  
                                            y1 = unit(0,"npc"), 
                                            gp=gpar(lwd = 2.0)),
                               simplify=FALSE)
  
  # thick bottom and top line for table
  horizontal_line2 <- replicate(ncol(g),
                                segmentsGrob(x1 = unit(0, "npc"),             
                                             y0 = unit(0,"npc"),
                                             x0 = unit(1, "npc"),  
                                             y1 = unit(0,"npc"), 
                                             gp=gpar(lwd = 4.0)),
                                simplify=FALSE)
  
  # add extra row to have line above header!
  padding <- unit(0.5,"line")
  extra_line <- textGrob("",gp=gpar(fontsize=10))
  footnote <- textGrob("CI = 95% confidence interval, DF = degrees of freedom, Std.Error = Standard error of the estimate", 
                       x=0, hjust = 0, gp=gpar(fontface="italic", fontsize=10))
  
  g <- gtable_add_rows(g, 
                       heights = grobHeight(extra_line) + padding,
                       pos = 0)
  g <- gtable_add_rows(g, 
                       heights = grobHeight(footnote) + padding)
  g <- gtable_add_grob(g, list(extra_line, footnote),
                       t=c(1, nrow(g)), l=c(1,1), 
                       r=ncol(g))
  
  # add other line elements
  g <- gtable::gtable_add_grob(g, grobs = horizontal_line2,
                               t = 1, b = 1, l = 1:ncol(g))
  g <- gtable::gtable_add_grob(g, grobs = horizontal_line,
                               t = 2.1, b = 2.1, l = 1:ncol(g))
  tbl1 <- gtable::gtable_add_grob(g, grobs = horizontal_line2,
                                  t = nrow(g)-1, b = nrow(g)-1, l = 1:ncol(g))
  tbl1
}





minimal_gtable_rma <- function(x){
  
  #x <- summary_linear
  
  # smaller line below header
  tt <- ttheme_minimal(show.rownames = F)
  g <- tableGrob(x, rows=NULL, theme=tt)
  horizontal_line <- replicate(ncol(g),
                               segmentsGrob(x1 = unit(0, "npc"),             
                                            y0 = unit(0,"npc"),
                                            x0 = unit(1, "npc"),  
                                            y1 = unit(0,"npc"), 
                                            gp=gpar(lwd = 2.0)),
                               simplify=FALSE)
  
  # thick bottom and top line for table
  horizontal_line2 <- replicate(ncol(g),
                                segmentsGrob(x1 = unit(0, "npc"),             
                                             y0 = unit(0,"npc"),
                                             x0 = unit(1, "npc"),  
                                             y1 = unit(0,"npc"), 
                                             gp=gpar(lwd = 4.0)),
                                simplify=FALSE)
  
  # add extra row to have line above header!
  padding <- unit(0.5,"line")
  extra_line <- textGrob("",gp=gpar(fontsize=10))
  footnote <- textGrob("P-perm = permutation-based p value", 
                       x=0, hjust = 0, gp=gpar(fontface="italic", fontsize=10))
  
  g <- gtable_add_rows(g, 
                       heights = grobHeight(extra_line) + padding,
                       pos = 0)
  g <- gtable_add_rows(g, 
                       heights = grobHeight(footnote) + padding)
  g <- gtable_add_grob(g, list(extra_line, footnote),
                       t=c(1, nrow(g)), l=c(1,1), 
                       r=ncol(g))
  
  # add other line elements
  g <- gtable::gtable_add_grob(g, grobs = horizontal_line2,
                               t = 1, b = 1, l = 1:ncol(g))
  g <- gtable::gtable_add_grob(g, grobs = horizontal_line,
                               t = 2.1, b = 2.1, l = 1:ncol(g))
  tbl1 <- gtable::gtable_add_grob(g, grobs = horizontal_line2,
                                  t = nrow(g)-1, b = nrow(g)-1, l = 1:ncol(g))
  tbl1
}

minimal_gtable_grob <- function(g){
  
  horizontal_line <- replicate(ncol(g),
                               segmentsGrob(x1 = unit(0, "npc"),             
                                            y0 = unit(0,"npc"),
                                            x0 = unit(1, "npc"),  
                                            y1 = unit(0,"npc"), 
                                            gp=gpar(lwd = 2.0)),
                               simplify=FALSE)
  
  # thick bottom and top line for table
  horizontal_line2 <- replicate(ncol(g),
                                segmentsGrob(x1 = unit(0, "npc"),             
                                             y0 = unit(0,"npc"),
                                             x0 = unit(1, "npc"),  
                                             y1 = unit(0,"npc"), 
                                             gp=gpar(lwd = 4.0)),
                                simplify=FALSE)
  
  # add extra row to have line above header!
  padding <- unit(0.5,"line")
  extra_line <- textGrob("",gp=gpar(fontsize=10))
  g <- gtable_add_rows(g, 
                       heights = grobHeight(extra_line) + padding,
                       pos = 0)
  g <- gtable_add_grob(g, list(extra_line),
                       t=1, l=1, 
                       r=ncol(g))
  # add other line elements
  g <- gtable::gtable_add_grob(g, grobs = horizontal_line2,
                               t = 1, b = 1, l = 1:ncol(g))
  g <- gtable::gtable_add_grob(g, grobs = horizontal_line,
                               t = 2.1, b = 2.1, l = 1:ncol(g))
  tbl1 <- gtable::gtable_add_grob(g, grobs = horizontal_line2,
                                  t = nrow(g), b = nrow(g), l = 1:ncol(g))
  tbl1
}


minimal_gtable2 <- function(x){
  
  tt <- ttheme_minimal(show.rownames = F, base_size=6, padding.h=unit(3, "mm"), padding.v=unit(3, "mm"))
  g <- tableGrob(x, rows=NULL, theme=tt)
  
  horizontal_line <- replicate(ncol(g),
                               segmentsGrob(x1 = unit(0, "npc"),             
                                            y0 = unit(0,"npc"),
                                            x0 = unit(1, "npc"),  
                                            y1 = unit(0,"npc"), 
                                            gp=gpar(lwd = 2.0)),
                               simplify=FALSE)
  
  # thick bottom and top line for table
  horizontal_line2 <- replicate(ncol(g),
                                segmentsGrob(x1 = unit(0, "npc"),             
                                             y0 = unit(0,"npc"),
                                             x0 = unit(1, "npc"),  
                                             y1 = unit(0,"npc"), 
                                             gp=gpar(lwd = 4.0)),
                                simplify=FALSE)
  
  # add extra row to have line above header!
  padding <- unit(0.5,"line")
  extra_line <- textGrob("",gp=gpar(fontsize=10))
  g <- gtable_add_rows(g, 
                       heights = grobHeight(extra_line) + padding,
                       pos = 0)
  g <- gtable_add_grob(g, list(extra_line),
                       t=1, l=1, 
                       r=ncol(g))
  # add other line elements
  g <- gtable::gtable_add_grob(g, grobs = horizontal_line2,
                               t = 1, b = 1, l = 1:ncol(g))
  g <- gtable::gtable_add_grob(g, grobs = horizontal_line,
                               t = 2.1, b = 2.1, l = 1:ncol(g))
  tbl1 <- gtable::gtable_add_grob(g, grobs = horizontal_line2,
                                  t = nrow(g), b = nrow(g), l = 1:ncol(g))
  tbl1
}


# define some functions for convenient outputting
diversity_stability_plot <- function(dsr=NULL, xlab, ylab){
  
  ggplot(data=subset(full_table_clean, diversity_stability_relation == dsr), aes(x=direction1, y=direction2, label=Code))  +
    geom_point(aes(x=0,y=0, color=scale), size=1) +
    geom_rect(xmin = -1.5, xmax = 1.5,   ymin = -.5, ymax = 0.5,   fill = "beige") +  
    geom_rect(ymin = -1.5, ymax = 1.5,   xmin = -.5, xmax = 0.5,   fill = "beige") +  
    geom_text_repel(data=subset(full_table_clean, diversity_stability_relation == dsr), 
                    aes(x=direction1, y=direction2, colour=scale, label=Code), segment.size  = 0, size= 2.0,
                    point.padding = NA,
                    segment.color = NA,
                    direction     = "y",
                    hjust         = 0,
                    vjust = 0,
                    show.legend = F) +
    scale_x_continuous(limits = c(-1.25, 1.25), breaks = c(-1.25,0,1.25)) + 
    scale_y_continuous(limits = c(-1.25, 1.25), breaks = c(-1.25,0,1.25)) +  
    xlab(xlab) + 
    ylab(ylab) + 
    coord_equal() + 
    theme_bw(base_size=7) +
    theme(axis.ticks.x =element_blank(),
          axis.text.x=element_blank(), 
          axis.ticks.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.title=element_text(size=7),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          legend.position = "none") + 
    annotate("text", x = 1., y = 1.25, label = "(+/+)", size = 2) +
    annotate("text", x = 0, y = 1.25, label = "0", size = 2) +
    annotate("text", x = -1., y = 1.25, label = "(-/+)", size = 2) +
    annotate("text", x = 1., y = -1.25, label = "(+/-)", size = 2) +
    annotate("text", x = 0, y = -1.25, label = "0", size = 2) +
    annotate("text", x = -1., y = -1.25, label = "(-/-)", size = 2) +
    annotate("text", y = 0, x = -1.25, label = "0", size = 2) +
    annotate("text", y = 0, x = 1.25, label = "0", size = 2)  +
    guides(colour = guide_legend(title = "Resistance")) +
    scale_color_manual(breaks = c("relative", "absolute"),
                       values=c("darkred", "darkblue"))
}
diversity_stability_plot2 <- function(dsr=NULL, xlab, ylab){
  
  ggplot(data=subset(full_table_clean, diversity_stability_relation == dsr), aes(y=direction1, x=direction2, label=Code))  +
    geom_point(aes(x=0,y=0, color=scale),  size = 1) +
    geom_rect(xmin = -1.5, xmax = 1.5,   ymin = -.5, ymax = 0.5,   fill = "beige") +  
    geom_rect(ymin = -1.5, ymax = 1.5,   xmin = -.5, xmax = 0.5,   fill = "beige") +  
    geom_text_repel(data=subset(full_table_clean, diversity_stability_relation == dsr), 
                    aes(y=direction1, x=direction2, colour=scale, label=Code), segment.size  = 0, size= 2.0,
                    point.padding = NA,
                    segment.color = NA,
                    direction     = "y",
                    hjust         = 0,
                    vjust = 0,
                    show.legend = F) +
    scale_x_continuous(limits = c(-1.25, 1.25), breaks = c(-1,0,1)) + 
    scale_y_continuous(limits = c(-1.25, 1.25), breaks = c(-1,0,1)) +  
    xlab(ylab) + 
    ylab(xlab) + 
    coord_equal() + 
    theme_bw(base_size=7) +
    theme(axis.ticks.x =element_blank(),
          axis.text.x=element_blank(), 
          axis.ticks.y=element_blank(), 
          axis.text.y=element_blank(), 
          axis.title=element_text(size=7),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          legend.position = "none") + 
    annotate("text", x = 1., y = 1.25, label = "(+/+)", size = 2) +
    annotate("text", x = 0, y = 1.25, label = "0", size = 2) +
    annotate("text", x = -1., y = 1.25, label = "(-/+)", size = 2) +
    annotate("text", x = 1., y = -1.25, label = "(+/-)", size = 2) +
    annotate("text", x = 0, y = -1.25, label = "0", size = 2) +
    annotate("text", x = -1., y = -1.25, label = "(-/-)", size = 2) +
    annotate("text", y = 0, x = -1.25, label = "0", size = 2) +
    annotate("text", y = 0, x = 1.25, label = "0", size = 2)  +
    annotate("text", x = 1, y = -1, label = "this study", colour="darkblue", size = 2)+
    guides(colour = guide_legend(title = "Resistance")) +
    scale_color_manual(breaks = c("relative", "absolute"),
                       values=c("darkred", "darkblue"))
}
diversity_stability_plot3 <- function(dsr=NULL, xlab, ylab){
  
  ggplot(data=subset(full_table_clean, diversity_stability_relation == dsr), aes(y=direction1, x=direction2, label=Code))  +
    geom_rect(xmin = -1.5, xmax = 1.5,   ymin = -.5, ymax = 0.5,   fill = "beige") +  
    geom_rect(ymin = -1.5, ymax = 1.5,   xmin = -.5, xmax = 0.5,   fill = "beige") +  
    geom_text_repel(segment.size  = 0, size = 2,
                    point.padding = NA,
                    segment.color = NA,
                    direction     = "y",
                    hjust         = 0,
                    vjust = 0) +
    scale_x_continuous(limits = c(-1.25, 1.25), breaks = c(-1,0,1)) + 
    scale_y_continuous(limits = c(-1.25, 1.25), breaks = c(-1,0,1)) +  
    xlab(ylab) + 
    ylab(xlab) + 
    coord_equal() + 
    theme_bw(base_size=7) +
    theme(axis.ticks.x =element_blank(),
          axis.text.x=element_blank(), 
          axis.ticks.y=element_blank(), 
          axis.text.y=element_blank(),
          axis.title=element_text(size=7),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position = "none") + 
    annotate("text", x = 1., y = 1.25, label = "(+/+)", size = 2) +
    annotate("text", x = 0, y = 1.25, label = "0", size = 2) +
    annotate("text", x = -1., y = 1.25, label = "(-/+)", size = 2) +
    annotate("text", x = 1., y = -1.25, label = "(+/-)", size = 2) +
    annotate("text", x = 0, y = -1.25, label = "0", size = 2) +
    annotate("text", x = -1., y = -1.25, label = "(-/-)", size = 2) +
    annotate("text", y = 0, x = -1.25, label = "0", size = 2) +
    annotate("text", y = 0, x = 1.25, label = "0", size = 2)  +
    scale_color_manual(breaks = c("relative", "absolute"),
                       values=c("darkred", "darkblue"))
}

