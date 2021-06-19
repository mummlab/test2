ParseXML <- function(xmlfile="/
                     29 19_36_41.xml")
{
  xmlline <- readLines(xmlfile);
  # retrieve the xml file name
  exprm.name <- xmlline[grep("<ID>", xmlline)];
  exprm.name <- gsub(" ", "", exprm.name);
  exprm.name <- gsub("<ID>", "", exprm.name);
  exprm.name <- gsub("</ID>", "", exprm.name);
  # retrieve the signal of each well
  all.well.signal <- NULL;
  well.pos <- grep("Well Pos", xmlline);
  for(i in 1:length(well.pos))
  {
    well.signal <- array("", dim=11);
    # 0;2 signal
    well.signal[1] <- xmlline[well.pos[i]+1];
    well.signal[1] <- gsub(" ", "", well.signal[1]);
    well.signal[1] <- gsub("<MultipleMRW_Position=\"0;2\"LabelId=\"1\">", "", well.signal[1]);
    well.signal[1] <- as.character(gsub("</Multiple>", "", well.signal[1]));
    if(well.signal[1]=="OVER" | well.signal[1]=="INVALID"){well.signal[1] <- "66666";}
    # 1;2 signal
    well.signal[2] <- xmlline[well.pos[i]+2];
    well.signal[2] <- gsub(" ", "", well.signal[2]);
    well.signal[2] <- gsub("<MultipleMRW_Position=\"1;2\"LabelId=\"1\">", "", well.signal[2]);
    well.signal[2] <- as.character(gsub("</Multiple>", "", well.signal[2]));
    if(well.signal[2]=="OVER" | well.signal[1]=="INVALID"){well.signal[2] <- "66666";}
    # 2;2 signal
    well.signal[3] <- xmlline[well.pos[i]+3];
    well.signal[3] <- gsub(" ", "", well.signal[3]);
    well.signal[3] <- gsub("<MultipleMRW_Position=\"2;2\"LabelId=\"1\">", "", well.signal[3]);
    well.signal[3] <- as.character(gsub("</Multiple>", "", well.signal[3]));
    if(well.signal[3]=="OVER" | well.signal[3]=="INVALID"){well.signal[3] <- "66666";}
    # 2;1 signal
    well.signal[4] <- xmlline[well.pos[i]+4];
    well.signal[4] <- gsub(" ", "", well.signal[4]);
    well.signal[4] <- gsub("<MultipleMRW_Position=\"2;1\"LabelId=\"1\">", "", well.signal[4]);
    well.signal[4] <- as.character(gsub("</Multiple>", "", well.signal[4]));
    if(well.signal[4]=="OVER" | well.signal[4]=="INVALID"){well.signal[4] <- "66666";}
    # 1;1 signal
    well.signal[5] <- xmlline[well.pos[i]+5];
    well.signal[5] <- gsub(" ", "", well.signal[5]);
    well.signal[5] <- gsub("<MultipleMRW_Position=\"1;1\"LabelId=\"1\">", "", well.signal[5]);
    well.signal[5] <- as.character(gsub("</Multiple>", "", well.signal[5]));
    if(well.signal[5]=="OVER" | well.signal[5]=="INVALID"){well.signal[5] <- "66666";}
    # 0;1 signal
    well.signal[6] <- xmlline[well.pos[i]+6];
    well.signal[6] <- gsub(" ", "", well.signal[6]);
    well.signal[6] <- gsub("<MultipleMRW_Position=\"0;1\"LabelId=\"1\">", "", well.signal[6]);
    well.signal[6] <- as.character(gsub("</Multiple>", "", well.signal[6]));
    if(well.signal[6]=="OVER" | well.signal[6]=="INVALID"){well.signal[6] <- "66666";}
    # 0;0 signal
    well.signal[7] <- xmlline[well.pos[i]+7];
    well.signal[7] <- gsub(" ", "", well.signal[7]);
    well.signal[7] <- gsub("<MultipleMRW_Position=\"0;0\"LabelId=\"1\">", "", well.signal[7]);
    well.signal[7] <- as.character(gsub("</Multiple>", "", well.signal[7]));
    if(well.signal[7]=="OVER" | well.signal[7]=="INVALID"){well.signal[7] <- "66666";}
    # 1;0 signal
    well.signal[8] <- xmlline[well.pos[i]+8];
    well.signal[8] <- gsub(" ", "", well.signal[8]);
    well.signal[8] <- gsub("<MultipleMRW_Position=\"1;0\"LabelId=\"1\">", "", well.signal[8]);
    well.signal[8] <- as.character(gsub("</Multiple>", "", well.signal[8]));
    if(well.signal[8]=="OVER" | well.signal[8]=="INVALID"){well.signal[8] <- "66666";}
    # 2;0 signal
    well.signal[9] <- xmlline[well.pos[i]+9];
    well.signal[9] <- gsub(" ", "", well.signal[9]);
    well.signal[9] <- gsub("<MultipleMRW_Position=\"2;0\"LabelId=\"1\">", "", well.signal[9]);
    well.signal[9] <- as.character(gsub("</Multiple>", "", well.signal[9]));
    if(well.signal[9]=="OVER" | well.signal[9]=="INVALID"){well.signal[9] <- "66666";}
    # Mean signal
    well.signal[10] <- xmlline[well.pos[i]+10];
    well.signal[10] <- gsub(" ", "", well.signal[10]);
    well.signal[10] <- gsub("<MultipleMRW_Position=\"Mean\"LabelId=\"1\">", "", well.signal[10]);
    well.signal[10] <- as.character(gsub("</Multiple>", "", well.signal[10]));
    if(well.signal[10]=="OVER" | well.signal[10]=="INVALID"){well.signal[10] <- "66666";}
    # StDev signal
    well.signal[11] <- xmlline[well.pos[i]+11];
    well.signal[11] <- gsub(" ", "", well.signal[11]);
    well.signal[11] <- gsub("<MultipleMRW_Position=\"StDev\"LabelId=\"1\">", "", well.signal[11]);
    well.signal[11] <- as.character(gsub("</Multiple>", "", well.signal[11]));
    if(well.signal[11]=="OVER" | well.signal[11]=="INVALID"){well.signal[11] <- "66666";}
    well.signal <- as.numeric(well.signal);
    all.well.signal <- rbind(all.well.signal, well.signal);
  }
  rownames(all.well.signal) <- c(paste0("A", 1:12), paste0("B", 12:1), paste0("C", 1:12), paste0("D", 12:1), paste0("E", 1:12), paste0("F", 12:1), paste0("G", 1:12), paste0("H", 12:1));
  all.well.signal <- all.well.signal[c(1:12, 24:13, 25:36, 48:37, 49:60, 72:61, 73:84, 96:85), ];
  colnames(all.well.signal) <- c("S02", "S12", "S22", "S21", "S11", "S01", "S00", "S10", "S20", "Mean", "StDev");
  exprm <- list();
  exprm.name <- gsub("/", "-", exprm.name)
  exprm$name <- exprm.name;
  exprm$signal <- all.well.signal;
  return(exprm);
}

Plot.BoxPlot.SSMD.Heatmap <- function(lable.file=NULL, Roy.file=NULL, Ctl.file=NULL, Drug.file=NULL, dic.output=NULL)
{
  ####################################
  # BoxPlot.SSMD.Heatmap
  ####################################
  well.lable <- read.csv(lable.file);
  well.name <- c(paste0("A", 1:12), paste0("B", 1:12), paste0("C", 1:12), paste0("D", 1:12), paste0("E", 1:12), paste0("F", 1:12), paste0("G", 1:12), paste0("H", 1:12));

  # source("/data/HTS/Liyun/JHDL/Drug.analysis.R");
  # Roy Signal
  Roy <- ParseXML(xmlfile=Roy.file);
  Roy.signal <- Roy$signal[,1:9];
  Roy.mean <- mean(apply(Roy.signal, 1, max));
  Roy.sd <- sd(apply(Roy.signal, 1, max));

  # Background Parameters
  bg.ratio <- Roy.mean+3*Roy.sd;

  # Control Signal
  s.neg <- NULL;
  s.pos <- NULL;
  for (i in 1:length(Ctl.file))
  {
    Ctl <- ParseXML(xmlfile=Ctl.file[i]);
    Ctl.signal <- as.matrix(Ctl$signal[ ,1:9]);
    max.signal <- apply(Ctl.signal, 1, max);
    Ctl.signal[Ctl.signal<=bg.ratio] <- 0;
    well.signal <- apply(Ctl.signal, 1, sum);
    well.signal <- as.numeric(apply(cbind(well.signal, max.signal), 1, max));
    s.neg <- c(s.neg, well.signal[match(well.lable$Well[well.lable$Control=="-"], well.name)]);
    s.pos <- c(s.pos, well.signal[match(well.lable$Well[well.lable$Control=="+"], well.name)]);
  }


  # Drug Signal
  Drug <- ParseXML(xmlfile=Drug.file);
  Drug.signal <- as.matrix(Drug$signal[ ,1:9]);
  max.signal <- apply(Drug.signal, 1, max);
  Drug.signal[Drug.signal<=bg.ratio] <- 0;
  well.signal <- apply(Drug.signal, 1, sum);
  well.signal <- as.numeric(apply(cbind(well.signal, max.signal), 1, max));
  s4 <- well.signal[match(well.lable$Well[well.lable$Concentration=="4"], well.name)];
  s2 <- well.signal[match(well.lable$Well[well.lable$Concentration=="2"], well.name)];
  s1 <- well.signal[match(well.lable$Well[well.lable$Concentration=="1"], well.name)];
  s0.5 <- well.signal[match(well.lable$Well[well.lable$Concentration=="0.5"], well.name)];
  s0.25 <- well.signal[match(well.lable$Well[well.lable$Concentration=="0.25"], well.name)];
  s0.125 <- well.signal[match(well.lable$Well[well.lable$Concentration=="0.125"], well.name)];

  # Different Concentration Signal Boxplot
  png(paste0(dic.output, Drug$name, ".plot.png"), width=1800, height=960);
  par(fig=c(0.02,0.31,0,1), font=2, lwd=3);
  boxplot.signal <- data.frame(rbind(cbind(s4/bg.ratio, rep("A", length(s4))), cbind(s2/bg.ratio, rep("B", length(s2))), cbind(s1/bg.ratio, rep("C", length(s1))), cbind(s0.5/bg.ratio, rep("D", length(s0.5))), cbind(s0.25/bg.ratio, rep("E", length(s0.25))), cbind(s0.125/bg.ratio, rep("F", length(s0.125))), cbind(s.neg/bg.ratio, rep("G", length(s.neg)))));
  boxplot.signal <- data.frame(as.numeric(as.character(boxplot.signal[,1])), boxplot.signal[,2]);
  colnames(boxplot.signal) <- c("signal", "label");
  boxplot.par <- boxplot(signal ~ label, boxplot.signal, notch=TRUE, outline = FALSE, whisklty = 0, staplelty = 0, ylim=c(0,16), ylab="", names=rep("", 7), font.axis=2, font.lab=2, cex.axis=2, cex.lab=2, cex.main=2, lwd=3, yaxt='n', boxwex=0.5, plot=FALSE);
  ymax <- (max(boxplot.par$stats[4,]) %/% 8 +1) * 8;
  boxplot.par <- boxplot(signal ~ label, boxplot.signal, notch=TRUE, outline = FALSE, whisklty = 0, staplelty = 0, ylim=c(0,ymax), ylab="", names=rep("", 7), font.axis=2, font.lab=2, cex.axis=2, cex.lab=2, cex.main=2, lwd=3, yaxt='n', boxwex=0.5);
  axis(1, at=c(1:7), labels=c("4","2","1","0.5","0.25","0.125","(-)"), font=2, cex.axis = 2, lwd=3, las=3);
  axis(2, cex.axis = 2, lwd=3, font=2);
  mtext("YFP", side=2, line=4, cex=2);
  s.median <- c(median(s4/bg.ratio), median(s2/bg.ratio), median(s1/bg.ratio), median(s0.5/bg.ratio), median(s0.25/bg.ratio), median(s0.125/bg.ratio));
  library('pracma');
  rank.polyfit <- polyfit(c(1:6), s.median, 2);
  rank.polyfit.value <- polyval(rank.polyfit, c(0:250)*0.02+1);
  lines(c(0:250)*0.02+1, rank.polyfit.value, lwd=3);

  # Calculate SSMD score
  SSMD.pos.log <- (gamma(length(s.pos)/2-1/2)/gamma(length(s.pos)/2-1))*sqrt(2/(length(s.pos)-1))*mean(log(s.pos, base=2)-median(log(s.neg, base=2)))/sd(log(s.pos, base=2)-median(log(s.neg, base=2)));
  SSMD.neg.log <- (gamma(length(s.neg)/2-1/2)/gamma(length(s.neg)/2-1))*sqrt(2/(length(s.neg)-1))*mean(log(s.neg, base=2)-median(log(s.neg, base=2)))/sd(log(s.neg, base=2)-median(log(s.neg, base=2)));
  SSMD.s4.log <- (gamma(length(s4)/2-1/2)/gamma(length(s4)/2-1))*sqrt(2/(length(s4)-1))*mean(log(s4, base=2)-median(log(s.neg, base=2)))/sd(log(s4, base=2)-median(log(s.neg, base=2)));
  SSMD.s2.log <- (gamma(length(s2)/2-1/2)/gamma(length(s2)/2-1))*sqrt(2/(length(s2)-1))*mean(log(s2, base=2)-median(log(s.neg, base=2)))/sd(log(s2, base=2)-median(log(s.neg, base=2)));
  SSMD.s1.log <- (gamma(length(s1)/2-1/2)/gamma(length(s1)/2-1))*sqrt(2/(length(s1)-1))*mean(log(s1, base=2)-median(log(s.neg, base=2)))/sd(log(s1, base=2)-median(log(s.neg, base=2)));
  SSMD.s0.5.log <- (gamma(length(s0.5)/2-1/2)/gamma(length(s0.5)/2-1))*sqrt(2/(length(s0.5)-1))*mean(log(s0.5, base=2)-median(log(s.neg, base=2)))/sd(log(s0.5, base=2)-median(log(s.neg, base=2)));
  SSMD.s0.25.log <- (gamma(length(s0.25)/2-1/2)/gamma(length(s0.25)/2-1))*sqrt(2/(length(s0.25)-1))*mean(log(s0.25, base=2)-median(log(s.neg, base=2)))/sd(log(s0.25, base=2)-median(log(s.neg, base=2)));
  SSMD.s0.125.log <- (gamma(length(s0.125)/2-1/2)/gamma(length(s0.125)/2-1))*sqrt(2/(length(s0.125)-1))*mean(log(s0.125, base=2)-median(log(s.neg, base=2)))/sd(log(s0.125, base=2)-median(log(s.neg, base=2)));

  SSMD.log <- c(SSMD.s4.log, SSMD.s2.log, SSMD.s1.log, SSMD.s0.5.log, SSMD.s0.25.log, SSMD.s0.125.log, SSMD.neg.log, SSMD.pos.log);

  #  Plot SSMD score
  par(fig=c(0.33,0.64,0,1), font=2, lwd=3, new=TRUE)
  plot(SSMD.log, ylim=c(-2,3), xlim=c(0.5, 8.5), pch='*', cex=4, xaxt='n', yaxt='n', ylab='', xlab='');
  axis(1, at=c(1:8), labels=c("4","2","1","0.5","0.25","0.125","(-)", "(+)"), font=2, cex.axis = 2, lwd=3, las=3);
  axis(2, at=c(-4:6)*0.5, labels=c(-4:6)*0.5 , cex.axis = 2, lwd=3, font=2);
  abline(h=0, col="black", lwd=3, lty=3);
  abline(h=c(-0.25, 0.25), col="blue", lwd=3, lty=3);
  abline(h=c(-0.5, 0.5), col="gold", lwd=3, lty=3);
  abline(h=c(-1, 1), col="pink", lwd=3, lty=3);
  abline(h=c(-1.645, 1.645), col="green", lwd=3, lty=3);
  abline(h=c(-2, 2), col="red", lwd=3, lty=3);
  mtext("SSMD Score", side=2, line=4, cex=2);

  # Heatmap
  Drug.autoflrc <- array(0, dim=96);
  Drug.autoflrc[apply(Drug.signal > bg.ratio, 1, sum)>=5] <- 1;
  s.colorcode <- well.signal[1:96]/(median(well.signal[1:96])+1.5*IQR(well.signal[1:96]));
  s.colorcode[s.colorcode>1] <- 1;
  s.x <- 1:96 %% 12;
  s.x[s.x==0] <- 12;
  s.y <- ceiling(1:96/12);
  autoflrc.x <- s.x[Drug.autoflrc==1];
  autoflrc.y <- s.y[Drug.autoflrc==1];
  # colorRamp produces custom palettes, but needs values between 0 and 1
  colorFunction <- colorRamp(c("black", "tan4", "darkgoldenrod"));

  # Apply colorRamp and switch to hexadecimal representation
  s.colorcode.matrix <- colorFunction(s.colorcode);
  sColors <- rgb(s.colorcode.matrix, maxColorValue=255);

  # Let's plot
  par(fig=c(0.61,0.99,0.3,1), font=1, lwd=2, new=TRUE);
  plot(x=s.x*1.5, y=(9-s.y)*0.94, col=sColors, pch=19, cex=8.3, xlim=c(0.5,18), ylim=c(0.5,10.5), bty="n", xaxt='n', yaxt="n", xlab='', ylab='');
  if (length(autoflrc.x)>0)
  {
    text(x=autoflrc.x*1.5, y=(9-autoflrc.y)*0.94, labels="A", col="red", cex=3, family="Courier New");
  }
  dev.off();
}

Control.SampleSize <- function(lable.file=NULL, Roy.file=NULL, Ctl.file=NULL, dic.output=NULL)
{
  ####################################
  # BoxPlot.SSMD.Heatmap
  ####################################
  well.lable <- read.csv(lable.file);
  well.name <- c(paste0("A", 1:12), paste0("B", 1:12), paste0("C", 1:12), paste0("D", 1:12), paste0("E", 1:12), paste0("F", 1:12), paste0("G", 1:12), paste0("H", 1:12));

  # source("/data/HTS/Liyun/JHDL/Drug.analysis.R");
  # Roy Signal
  Roy <- ParseXML(xmlfile=Roy.file);
  Roy.signal <- Roy$signal[,1:9];
  Roy.mean <- mean(apply(Roy.signal, 1, max));
  Roy.sd <- sd(apply(Roy.signal, 1, max));

  # Background Parameters
  bg.ratio <- Roy.mean+3*Roy.sd;

  # Control Signal
  s.neg <- NULL;
  s.pos <- NULL;
  Ctl.name <- NULL;
  for (i in 1:length(Ctl.file))
  {
    Ctl <- ParseXML(xmlfile=Ctl.file[i]);
    Ctl.signal <- as.matrix(Ctl$signal[ ,1:9]);
    max.signal <- apply(Ctl.signal, 1, max);
    Ctl.signal[Ctl.signal<=bg.ratio] <- 0;
    well.signal <- apply(Ctl.signal, 1, sum);
    well.signal <- as.numeric(apply(cbind(well.signal, max.signal), 1, max));
    s.neg <- c(s.neg, well.signal[match(well.lable$Well[well.lable$Control=="-"], well.name)]);
    s.pos <- c(s.pos, well.signal[match(well.lable$Well[well.lable$Control=="+"], well.name)]);
    Ctl.name <- c(Ctl.name, Ctl$name);
  }

  #################################
  # Sample size estimation
  #################################

  Decrease.ratio <- rep(c("100%", "75%", "50%", "25%"), time=12);
  fold75 <- mean(s.pos)/((mean(s.pos)-mean(s.neg))*0.75+mean(s.neg));
  fold50 <- mean(s.pos)/((mean(s.pos)-mean(s.neg))*0.5+mean(s.neg));
  fold25 <- mean(s.pos)/((mean(s.pos)-mean(s.neg))*0.25+mean(s.neg));
  Decrease.fold <- rep(c(1, fold75, fold50, fold25), time=12);
  TypeI.error <- rep(c(0.05, 0.01, 0.001), c(4*4,4*4,4*4));
  TypeII.error <- rep(rep(c(0.2, 0.05, 0.01, 0.001), c(4,4,4,4)), time=3);
  Z.a <- rep(c(1.96, 2.58, 3.29), c(4*4,4*4,4*4));
  Z.b <- rep(rep(c(0.84, 1.64, 2.33, 3.09), c(4,4,4,4)), time=3);

  smpl.size <- array(0, dim=length(Decrease.ratio));
  for(i in 1:length(Decrease.ratio))
  {
    P.decrs <- s.pos/Decrease.fold[i];
    smpl.size[i] <- 2*max(sd(P.decrs), sd(s.neg))^2*(Z.a[i]+Z.b[i])^2/(mean(P.decrs)-mean(s.neg))^2*1.15
    smpl.size[i] <- round(smpl.size[i],2)
  }
  log.smpl.size <- array(0, dim=length(Decrease.ratio));
  for(i in 1:length(Decrease.ratio))
  {
    P.decrs <- s.pos/Decrease.fold[i];
    log.smpl.size[i] <- 2*max(sd(log(P.decrs,base=2)), sd(log(s.neg, base=2)))^2*(Z.a[i]+Z.b[i])^2/(mean(log(P.decrs, base=2))-mean(log(s.neg, base=2)))^2*1.15
    log.smpl.size[i] <- round(log.smpl.size[i],2)
  }

  sample.size <- data.frame(Decrease.ratio, Decrease.fold, TypeI.error, TypeII.error, Z.a, Z.b, smpl.size, log.smpl.size);
  colnames(sample.size) <- c("Decrease Ratio", "Decrease Fold", "TypeI error", "TypeII error", "Z(Alpha)", "Z(Beta)", "Sample Size", "Sample Size(log)");

  write.csv(sample.size, file=paste0(dic.output, paste(Ctl.name, collapse="_"), ".SampleSize.csv"), row.names=FALSE);
}

SSMD.outputfile <- function(lable.file=NULL, Roy.file=NULL, Ctl.file=NULL, Drug.file=NULL, dic.output=NULL)
{
  ####################################
  # BoxPlot.SSMD.Heatmap
  ####################################
  well.lable <- read.csv(lable.file);
  well.name <- c(paste0("A", 1:12), paste0("B", 1:12), paste0("C", 1:12), paste0("D", 1:12), paste0("E", 1:12), paste0("F", 1:12), paste0("G", 1:12), paste0("H", 1:12));

  # source("/data/HTS/Liyun/JHDL/Drug.analysis.R");
  # Roy Signal
  Roy <- ParseXML(xmlfile=Roy.file);
  Roy.signal <- Roy$signal[,1:9];
  Roy.mean <- mean(apply(Roy.signal, 1, max));
  Roy.sd <- sd(apply(Roy.signal, 1, max));

  # Background Parameters
  bg.ratio <- Roy.mean+3*Roy.sd;

  # Control Signal
  s.neg <- NULL;
  s.pos <- NULL;
  Ctl.name <- NULL;
  for (i in 1:length(Ctl.file))
  {
    Ctl <- ParseXML(xmlfile=Ctl.file[i]);
    Ctl.signal <- as.matrix(Ctl$signal[ ,1:9]);
    max.signal <- apply(Ctl.signal, 1, max);
    Ctl.signal[Ctl.signal<=bg.ratio] <- 0;
    well.signal <- apply(Ctl.signal, 1, sum);
    well.signal <- as.numeric(apply(cbind(well.signal, max.signal), 1, max));
    s.neg <- c(s.neg, well.signal[match(well.lable$Well[well.lable$Control=="-"], well.name)]);
    s.pos <- c(s.pos, well.signal[match(well.lable$Well[well.lable$Control=="+"], well.name)]);
    Ctl.name <- c(Ctl.name, Ctl$name);
  }

  # Drug Signal
  Drug <- ParseXML(xmlfile=Drug.file);
  Drug.signal <- as.matrix(Drug$signal[ ,1:9]);
  max.signal <- apply(Drug.signal, 1, max);
  Drug.signal[Drug.signal<=bg.ratio] <- 0;
  well.signal <- apply(Drug.signal, 1, sum);
  well.signal <- as.numeric(apply(cbind(well.signal, max.signal), 1, max));
  s4 <- well.signal[match(well.lable$Well[well.lable$Concentration=="4"], well.name)];
  s2 <- well.signal[match(well.lable$Well[well.lable$Concentration=="2"], well.name)];
  s1 <- well.signal[match(well.lable$Well[well.lable$Concentration=="1"], well.name)];
  s0.5 <- well.signal[match(well.lable$Well[well.lable$Concentration=="0.5"], well.name)];
  s0.25 <- well.signal[match(well.lable$Well[well.lable$Concentration=="0.25"], well.name)];
  s0.125 <- well.signal[match(well.lable$Well[well.lable$Concentration=="0.125"], well.name)];

  # Calculate SSMD score
  SSMD.pos.log <- (gamma(length(s.pos)/2-1/2)/gamma(length(s.pos)/2-1))*sqrt(2/(length(s.pos)-1))*mean(log(s.pos, base=2)-median(log(s.neg, base=2)))/sd(log(s.pos, base=2)-median(log(s.neg, base=2)));
  SSMD.neg.log <- (gamma(length(s.neg)/2-1/2)/gamma(length(s.neg)/2-1))*sqrt(2/(length(s.neg)-1))*mean(log(s.neg, base=2)-median(log(s.neg, base=2)))/sd(log(s.neg, base=2)-median(log(s.neg, base=2)));
  SSMD.s4.log <- (gamma(length(s4)/2-1/2)/gamma(length(s4)/2-1))*sqrt(2/(length(s4)-1))*mean(log(s4, base=2)-median(log(s.neg, base=2)))/sd(log(s4, base=2)-median(log(s.neg, base=2)));
  SSMD.s2.log <- (gamma(length(s2)/2-1/2)/gamma(length(s2)/2-1))*sqrt(2/(length(s2)-1))*mean(log(s2, base=2)-median(log(s.neg, base=2)))/sd(log(s2, base=2)-median(log(s.neg, base=2)));
  SSMD.s1.log <- (gamma(length(s1)/2-1/2)/gamma(length(s1)/2-1))*sqrt(2/(length(s1)-1))*mean(log(s1, base=2)-median(log(s.neg, base=2)))/sd(log(s1, base=2)-median(log(s.neg, base=2)));
  SSMD.s0.5.log <- (gamma(length(s0.5)/2-1/2)/gamma(length(s0.5)/2-1))*sqrt(2/(length(s0.5)-1))*mean(log(s0.5, base=2)-median(log(s.neg, base=2)))/sd(log(s0.5, base=2)-median(log(s.neg, base=2)));
  SSMD.s0.25.log <- (gamma(length(s0.25)/2-1/2)/gamma(length(s0.25)/2-1))*sqrt(2/(length(s0.25)-1))*mean(log(s0.25, base=2)-median(log(s.neg, base=2)))/sd(log(s0.25, base=2)-median(log(s.neg, base=2)));
  SSMD.s0.125.log <- (gamma(length(s0.125)/2-1/2)/gamma(length(s0.125)/2-1))*sqrt(2/(length(s0.125)-1))*mean(log(s0.125, base=2)-median(log(s.neg, base=2)))/sd(log(s0.125, base=2)-median(log(s.neg, base=2)));
  SSMD.QC.DV <- (mean(log(s.pos, base=2))-mean(log(s.neg, base=2)))/sqrt(sd(log(s.pos, base=2))^2+sd(log(s.neg, base=2))^2);
  SSMD.QC.OTL <- (median(log(s.pos, base=2))-median(log(s.neg, base=2)))/(1.4826*sqrt(mad(log(s.pos, base=2), constant=1)^2+mad(log(s.neg, base=2), constant=1)^2));

  SSMD.log <- c(SSMD.s4.log, SSMD.s2.log, SSMD.s1.log, SSMD.s0.5.log, SSMD.s0.25.log, SSMD.s0.125.log, SSMD.neg.log, SSMD.pos.log, SSMD.QC.DV, SSMD.QC.OTL);
  SSMD.log <- round(SSMD.log, 2);

  SSMD <- list();
  SSMD$name <- paste(Ctl.name, collapse="_");
  SSMD.high <- array("", dim=length(SSMD.log));
  SSMD.high[SSMD.log>=1.3] <- "High";
  SSMD$score <- data.frame(SSMD.log, SSMD.high);
  colnames(SSMD$score) <- c(paste0(Drug$name, ".SSMD"), paste0(Drug$name, ".Lable"));
  SSMD;
}

multi.BoxPlot.SSMD.Heatmap.SampleSize <- function(lable.file=NULL, Roy.file=NULL, Ctl.file=NULL, multi.Drug.file=NULL, dic.output=NULL)
{
  # source("/data/HTS/Liyun/JHDL/Drug.analysis.R");
  for(i in 1:length(multi.Drug.file))
  {
    Plot.BoxPlot.SSMD.Heatmap(lable.file=lable.file, Roy.file=Roy.file, Ctl.file=Ctl.file, Drug.file=multi.Drug.file[i], dic.output=dic.output);
  }
  Control.SampleSize(lable.file=lable.file, Roy.file=Roy.file, Ctl.file=Ctl.file, dic.output=dic.output);
  SSMD.output <- data.frame(c("4", "2", "1", "0.5", "0.25", "0.125", "-", "+", "QC.DV", "QC.OTL"));
  colnames(SSMD.output) <- c("Concentration");
  for(i in 1:length(multi.Drug.file))
  {
    SSMD <- SSMD.outputfile(lable.file=lable.file, Roy.file=Roy.file, Ctl.file=Ctl.file, Drug.file=multi.Drug.file[i], dic.output=dic.output);
    SSMD.output <- cbind(SSMD.output, SSMD$score);
  }
  write.csv(SSMD.output, file=paste0(dic.output, SSMD$name, ".SSMD.score.csv"), row.names=FALSE);
}

library(gWidgets)
library(RGtk2)


GUI <- function(){
lable.file <- "";
Roy.file <- "";
Ctl.file <- "";
multi.Drug.file <- "";
dic.output <- "";


options("guiToolkit"="RGtk2")
win = gwindow("Drug Analysis", width=600, height=300)
tbl = glayout(cont=win)


tbl[1,1, anchor=c(0,0)] <- "Load Roy file"
tbl[1,2] <- gbutton(text = "Upload Roy file", cont=tbl, handler = function(h,...){
  Roy.file <<- gfile()
  svalue(RoyFilename)<- ParseXML(Roy.file)$name })
tbl[1,3, anchor=c(0,0)] <- "Roy Plate Name"
RoyFilename <- gedit(text = "", width=50, height=10, cont=tbl)
tbl[1,4, anchor=c(0,0)] <- RoyFilename


tbl[2,1, anchor=c(0,0)] <- "Load Control file"
tbl[2,2] <- gbutton(text = "Upload Control file", cont=tbl, handler = function(h,...){
  r<- gfile(multi=T)
  r<- unlist(strsplit(r, "\n"))
  Ctl.file <<- r
  for(i in 1:length(r)) {
    r[i] <- ParseXML(r[i])$name
  }
  svalue(ControlFilename)<- paste(r, sep="\n")
  focus(ControlFilename)<- T})
tbl[2,3, anchor=c(0,0)] <- "Control Plate Name"
ControlFilename <- gtext(text = "", width=50, height=40, cont=tbl)
tbl[2,4, anchor=c(0,0)] <- ControlFilename


tbl[3,1, anchor=c(0,0)] <- "Well label"
tbl[3,2] <- gbutton(text = "Well label file", cont=tbl, handler = function(h,...){
  lable.file <<- gfile()
  svalue(WellLabelFilename)<- basename(lable.file) })
tbl[3,3, anchor=c(0,0)] <- "Well Label File Name"
WellLabelFilename <- gedit(text = "", width=50, height=10, cont=tbl)
tbl[3,4, anchor=c(0,0)] <- WellLabelFilename


tbl[4,1, anchor=c(0,0)] <- "Drug XML file"
tbl[4,2] <- gbutton(text = "Drug XML file", cont=tbl, handler = function(h,...){
  r<- gfile(multi=T)
  r<- unlist(strsplit(r, "\n"))
  multi.Drug.file <<- r
  for(i in 1:length(r)) {
    r[i] <- ParseXML(r[i])$name
  }
  svalue(DrugXMLFilenames)<- paste(r, sep="\n")
  focus(DrugXMLFilenames)<- T })
tbl[4,3, anchor=c(0,0)] <- "Drug XML File Names"
DrugXMLFilenames <- gtext(text = "", width=50, height=100, cont=tbl)
tbl[4,4, anchor=c(0,0)] <- DrugXMLFilenames

tbl[5,1:4] <- gseparator(cont=tbl)

tbl[6,1, anchor=c(0,0)] <- "Output Directory"
tbl[6,2] <- gbutton(text = "Choose Directory", cont=tbl, handler = function(h,...){
  dic.output <<- gfile(type="selectdir")
  svalue(DirectoryName)<- dic.output })
tbl[6,3, anchor=c(0,0)] <- "Directory Name"
DirectoryName <- gedit(text = "", width=50, height=10, cont=tbl)
tbl[6,4, anchor=c(0,0)] <- DirectoryName

tbl[7,1:4] <- gseparator(cont=tbl)

tbl[8,1, anchor=c(0,0)] <- gbutton(text = "OK", cont=tbl, handler = function(h,...){
  dic.output <- paste0(dic.output, "\\");
  multi.BoxPlot.SSMD.Heatmap.SampleSize(lable.file=lable.file, Roy.file=Roy.file, Ctl.file=Ctl.file, multi.Drug.file=multi.Drug.file, dic.output=dic.output)
})
tbl[9,1, anchor=c(0,0)] <- gbutton(text = "Cancel", cont=tbl, handler = function(h,...){
  dispose(win) })
}
