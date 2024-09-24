#' Paramounter part1
#'
#' Optimize XCMS parameters by paramounter part1. detect ppm cutoff.
#' @return A ggplot object of missing value.
#' @param directory The data path for mzXML files.
#' @param massSDrange The range of standard deviations for mass differences, default value is 2 (95% confidence interval).
#' @param smooth Chromatographic smoothness, default value is 0. For full scan data, use a positive value; for example, use smooth = 4 for a 4 Hz spectral rate.
#' @param cutoff The ppm percentage for determining the cutoff line of mass accuracy distribution, default value is 0.95 (95%).
#' @param thread thread num
#' @importFrom future plan multisession multicore
#' @importFrom furrr future_map_dfr
#' @importFrom purrr map_dfr
#' @importFrom dplyr filter
#' @import ggplot2
#' @importFrom MSnbase readMSData mz intensity rtime
#' @references see https://github.com/HuanLab/Paramounter
#' @description
#' Research paper: https://pubs.acs.org/doi/10.1021/acs.analchem.1c04758
#' DOI:10.1021/acs.analchem.1c04758
#'
#'
#' @noRd
#' @export
#'

paramounter_part1 = function(
    directory,
    massSDrange = 2,
    smooth = 0,
    cutoff = 0.95,
    thread,
    filenum = c(3,5,"all")
){
 # filenum = match.arg(filenum)
  ## init setting
  filename <- list.files(path = directory,pattern = ".mzXML")
  start_time <- Sys.time()
  mzDiff <- c()
  ppm <- c()
  mzDiff2D <- as.data.frame(matrix(ncol = 3, nrow = 1))
  colnames(mzDiff2D) <- c("mz", "rt", "mzdiff")
  ppm2D <- as.data.frame(matrix(ncol = 3, nrow = 1))
  colnames(ppm2D) <- c("mz", "rt", "ppm")

  file_count = length(filename)

  if(file_count == 1) {
    file_select = 1
  } else if(filenum == 3) {
    file_select = round(c(min(1:file_count),median(1:file_count),max(1:file_count)))
  } else if(filenum == 5) {
    file_select = round(fivenum(1:file_count))
  } else {
    file_select = 1:file_count
  }

  #smoothing function
  peak_smooth <- function(x,level=smooth){
    n <- level
    if(length(x) < 2*n){
      return(x)
    } else if(length(unique(x))==1){
      return(x)
    } else{
      y <- vector(length=length(x))
      for(i in 1:n){
        y[i] <- sum(c((n-i+2):(n+1),n:1)*x[1:(i+n)])/sum(c((n-i+2):(n+1),n:1))
      }
      for(i in (n+1):(length(y)-n)){
        y[i] <-  sum(c(1:(n+1),n:1)*x[(i-n):(i+n)])/sum(c(1:(n+1),n:1))
      }
      for(i in (length(y)-n+1):length(y)){
        y[i] <- sum(c(1:n,(n+1):(n+i-length(x)+1))*x[(i-n):length(x)])/sum(c(1:n,(n+1):(n+i-length(x)+1)))
      }
      return(y)
    }
  }


  if(str_detect(Sys.info()["sysname"],'Linux')){
    plan(multicore, workers = thread)
  } else {
    plan(multisession, workers = thread)
  }

  process_file <- function(q) {
    ms1data <- readMSData(files = paste0(directory,filename[q]), mode = "onDisk", msLevel. = 1)
    mzRange <- c(min(unlist(mz(ms1data))), max(unlist(mz(ms1data))))
    ROI <- seq(mzRange[1], mzRange[2], 0.05)
    mzData <- mz(ms1data)
    intData <- intensity(ms1data)
    rtime <- rtime(ms1data)
    ppm2Ddist <- as.data.frame(matrix(ncol = 3, nrow = 1))
    colnames(ppm2Ddist) <- c("mz", "rt", "ppm")
    mzdiff2Ddist <- as.data.frame(matrix(ncol = 3, nrow = 1))
    colnames(mzdiff2Ddist) <- c("mz", "rt", "mzdiff")

    # 并行处理每个ROI
    split_ppm2Ddist <- furrr::future_map_dfr(.x = c(1:(length(ROI) - 1)),.f = function(.x) {
      blank_ppm2Ddist = data.frame(mz = NA,rt=NA,ppm=NA)
      i = .x
      currmzRange <- c(ROI[i], ROI[i+1])
      tmpMZdata <- mzData
      tmpINTdata <- intData
      for(j in 1:length(mzData)){
        index <- which(tmpMZdata[[j]] >= currmzRange[1] & tmpMZdata[[j]] < currmzRange[2])
        tmpMZdata[[j]] <- tmpMZdata[[j]][index]
        tmpINTdata[[j]] <- tmpINTdata[[j]][index]
      }
      # Extract the intensity vectors from each m/z bin
      eicINTraw <- c()
      eicINT <- c()
      eicRT <- c()
      for(k in 1:length(mzData)){
        if(length(tmpINTdata[[k]]) > 0){
          eicINTraw[k] <- mean(tmpINTdata[[k]])
        }else{
          eicINTraw[k] <- 0
        }
        eicRT[k] <- rtime[k]
      }
      if(sum(eicINTraw != 0) == 0) return(blank_ppm2Ddist)
      # Sort the intensity vectors from each m/z bin, estimate the noise cut off and average
      eicINT <- peak_smooth(eicINTraw)
      eicNon0 <- sort(eicINT[eicINT > 0])
      if(length(eicNon0) > 10){
        for(x in seq(10,length(eicNon0), 10)){
          sd <- sd(eicNon0[1:x])
          blk <- sum(eicNon0[1:x])/x
          thres <- blk + 3*sd
          if(x+1 <= length(eicNon0)){
            if(eicNon0[x+1] >= thres) break()
          }
        }
        cutOFF <- eicNon0[x]
      }else{
        cutOFF <- max(eicNon0)
      }

      aboveTHindex <- which(eicINT > cutOFF)
      if(length(aboveTHindex) == 0) return(blank_ppm2Ddist)
      candidateSegInd <- split(aboveTHindex, cumsum(c(1, diff(aboveTHindex) != 1)))
      peakInd <- c()
      for(x in 1:length(candidateSegInd)){
        peakInd[x] <- which(eicINT[candidateSegInd[[x]]] == max(eicINT[candidateSegInd[[x]]]))[1] + min(candidateSegInd[[x]]) - 1
      }
      refMZvec <- c()
      for(y in 1:length(peakInd)){
        highestINT <- which(tmpINTdata[[peakInd[y]]] == max(tmpINTdata[[peakInd[y]]]))[1]
        refMZvec[y] <- tmpMZdata[[peakInd[y]]][highestINT]
      }

      # Estimate the universal parameters (mass tolerance, peak height, and peak width) for each m/z bin
      ppmDiff <- c()
      for(z in 1:length(peakInd)){
        currPeakInd <- peakInd[z]
        currRefMz <- refMZvec[z]
        currSamePeakMass <- c()
        currSamePeakMass <- c(currSamePeakMass, currRefMz)
        leftInd <- currPeakInd-1
        rightInd <- currPeakInd+1
        if(leftInd > 0){
          while (length(tmpMZdata[[leftInd]]) > 0 & mean(tmpINTdata[[leftInd]]) >= cutOFF) {
            if (length(tmpMZdata[[leftInd]]) == 1){
              currSamePeakMass <- c(currSamePeakMass, tmpMZdata[[leftInd]])
              if(eicINT[leftInd] > eicINT[leftInd+1] & length(currSamePeakMass) > 5){
                Q1 <- as.numeric(summary(currSamePeakMass)[2])
                Q3 <- as.numeric(summary(currSamePeakMass)[5])
                LB <- Q1 - 1.5 *(Q3 - Q1)
                RB <- Q3 + 1.5 *(Q3 - Q1)
                if (currSamePeakMass[length(currSamePeakMass)] < LB || currSamePeakMass[length(currSamePeakMass)] > RB) break()
              }
            } else {
              abvector <- abs(tmpMZdata[[leftInd]] - currRefMz)
              NearInd <- which(abvector == min(abvector))[1]
              currSamePeakMass <- c(currSamePeakMass, tmpMZdata[[leftInd]][NearInd])
              if(eicINT[leftInd] > eicINT[leftInd+1] & length(currSamePeakMass) > 5){
                Q1 <- as.numeric(summary(currSamePeakMass)[2])
                Q3 <- as.numeric(summary(currSamePeakMass)[5])
                LB <- Q1 - 1.5 *(Q3 - Q1)
                RB <- Q3 + 1.5 *(Q3 - Q1)
                if (currSamePeakMass[length(currSamePeakMass)] < LB || currSamePeakMass[length(currSamePeakMass)] > RB) break()
              }
            }
            leftInd <- leftInd-1
            if(leftInd <= 0) break()
          }
        }
        if(rightInd <= length(tmpMZdata)){
          while (length(tmpMZdata[[rightInd]]) > 0 & mean(tmpINTdata[[rightInd]]) >= cutOFF) {
            if (length(tmpMZdata[[rightInd]]) == 1){
              currSamePeakMass <- c(currSamePeakMass, tmpMZdata[[rightInd]])
              if(eicINT[rightInd] > eicINT[rightInd-1] & length(currSamePeakMass) > 5){
                Q1 <- as.numeric(summary(currSamePeakMass)[2])
                Q3 <- as.numeric(summary(currSamePeakMass)[5])
                LB <- Q1 - 1.5 *(Q3 - Q1)
                RB <- Q3 + 1.5 *(Q3 - Q1)
                if (currSamePeakMass[length(currSamePeakMass)] < LB || currSamePeakMass[length(currSamePeakMass)] > RB) break()
              }
            } else {
              abvector <- abs(tmpMZdata[[rightInd]] - currRefMz)
              NearInd <- which(abvector == min(abvector))[1]
              currSamePeakMass <- c(currSamePeakMass, tmpMZdata[[rightInd]][NearInd])
              if(eicINT[rightInd] > eicINT[rightInd-1] & length(currSamePeakMass) > 5){
                Q1 <- as.numeric(summary(currSamePeakMass)[2])
                Q3 <- as.numeric(summary(currSamePeakMass)[5])
                LB <- Q1 - 1.5 *(Q3 - Q1)
                RB <- Q3 + 1.5 *(Q3 - Q1)
                if (currSamePeakMass[length(currSamePeakMass)] < LB || currSamePeakMass[length(currSamePeakMass)] > RB) break()
              }
            }
            rightInd <- rightInd+1
            if(rightInd > length(tmpMZdata)) break()
          }
        }

        if(length(currSamePeakMass) > 1){
          ppmDiff[z] <- (massSDrange*sd(currSamePeakMass))/currRefMz * 1e6
          temp_ppm2Ddist = data.frame(
            mz = currRefMz,rt = rtime[[peakInd[z]]],ppm = ppmDiff[z]
          )
        } else {
          temp_ppm2Ddist = blank_ppm2Ddist
        }
        return(temp_ppm2Ddist)
      }
    },.progress = T) %>% dplyr::filter(!is.na(ppm))

    return(split_ppm2Ddist)
  }

  ppm2D <- purrr::map_dfr(.x = file_select, .f = process_file)

  ppm2D <- ppm2D[complete.cases(ppm2D),]
  ppm2D <- ppm2D[order(ppm2D[,3]),]
  ppm2D <- ppm2D[1:round(nrow(ppm2D)*0.97),]
  ppm2D = as.data.frame(ppm2D)
  ppm2Ddash <- ppm2D[1:round(nrow(ppm2D)*cutoff),]
  dashline <- max(ppm2Ddash[,3])
  p =
  ggplot(ppm2D,aes(x = mz,y = ppm)) +
    geom_point() +
    geom_hline(yintercept = dashline, linetype = "dashed",color = 'red',
               linewidth = 2)+
    ylab("ppm")+
    xlab("m/z")+
    ggtitle(paste0("Mass tolerance"))+
    annotate("text", x = Inf, y = Inf, label = paste0("Recommend ppmCut = ",round(dashline)), hjust = 1.1, vjust = 1.5)+
    theme_bw()
  print(Sys.time() - start_time)
  message("Please find the cutoff line in the generated ppm distribution, and run Paramounter part 2 using the ppm cutoff")
  data_export = list(
    plot = p,
    ppmCut = dashline
  )
  return(data_export)
}
