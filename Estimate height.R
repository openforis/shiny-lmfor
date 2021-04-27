
model_names <- c("naslund",
                 "curtis",
                 "michailoff",
                 "meyer",
                 "power",
                 "naslund2",
                 "naslund3",
                 "naslund4",
                 "micment",
                 "micment2",
                 "wykoff",
                 "prodan",
                 "logistic",
                 "richards",
                 "weibull",
                 "gomperz",
                 "sibbesen",
                 "korf",
                 "ratkowsky",
                 "hossfeldIV")

Run_estimation <- function(tree2, model_name, model_index, model_no, Resultfolder, nrap, useVariance) {
  
  model_iterations  <- c("1","1","1")  # list of successful iterations (im1, im2, im3), 1= ok, 0=no run
  im1<- NULL; im2<-NULL; im3<- NULL
  # print("Rivejä")
  # print(nrow(tree2))
  
  tryCatch(
    expr = {

      Qd   <- quantile(tree2$tree_dbh, probs=c(.02, .98), na.rm=TRUE)
      tree2$H2 <- tree2$H
      tree2$H2[tree2$tree_dbh<Qd[[1]] | tree2$tree_dbh > Qd[[2]] ] <-  NA 
      
      
      # graphic output file
      png(paste0(Resultfolder,"fitted_residual_", as.character(model_index), ".png"), width=300) # ,height=7,pointsize = 12)
      par(mfcol=c(3, 1))
      
      # three options for lmfor:
      # 1. models calibrated for plots
      
      print("im1")
       im1 <- try(ImputeHeights(tree2$tree_dbh, tree2$H, tree2$plot_id,
                                 modelName = model_name, nranp=nrap, varf=useVariance, 
                                 addResidual = FALSE, makeplot = TRUE, level = 1,
                                 control=list(maxIter=100, msMaxIter = 1e6, msVerbose = FALSE), 
                                 start=NA, bh=1.3), 
                  silent = TRUE)
            
      if ( class(im1)=="try-error") {        
        im1                 <- NA
        model_iterations[1] <- "0" }
      
      # 2. models calibrated for clusters
      print("im2")
       im2 <- try(ImputeHeights(tree2$tree_dbh, tree2$H, tree2$cluster,  
                                modelName = model_name, nranp=nrap, varf=useVariance, 
                                addResidual = FALSE, makeplot = TRUE, level = 1,
                                control=list(maxIter=100, msMaxIter = 1e6, msVerbose = FALSE), 
                                start=NA, bh=1.3), 
                  silent = TRUE)
    
       if ( class(im2)=="try-error") {
         print("im2, toka-ajo:")
         im2 <- try(ImputeHeights(tree2$tree_dbh, tree2$H2, tree2$cluster, 
                                  modelName = model_name, nranp=nrap,
                                  level=1, makeplot = TRUE, 
                                  bh=1.3, control=list(maxIter=100, msMaxIter = 1e6, msVerbose = FALSE), varf = FALSE), silent = TRUE)
         #pnlsTol=0.04, 
         
         #  print(str(im3))
       }
       
      # modelName = model_name,
      # addResidual = TRUE, makeplot = TRUE, level = 1,
      # start=NA, bh=1.3, nranp=nrap, varf=useVariance), silent = TRUE)
      
            
      if ( class(im2)=="try-error") {
        im2                 <- NA
        model_iterations[2] <- "0" }
      
      # 3. fixed part of the model only for the whole data
#      print("im3")
      im3 <- try(ImputeHeights(tree2$tree_dbh, tree2$H,tree2$temp, modelName = model_name, 
                 level=0, makeplot = TRUE, bh=1.3), silent = TRUE)
#      print(paste0("eka-ajo:", str(im3)))
      
# *************
      if ( class(im3)=="try-error") {
        st <- c(a=mean(tree2$tree_dbh), b = 1/sd(tree2$tree_dbh), c = 1)

#        print("im3, toka-ajo:")
        im3 <- try(ImputeHeights(tree2$tree_dbh, tree2$H2, tree2$temp, modelName = model_name, 
                   level=0, makeplot = TRUE, 
                   start=st, bh=1.3, control=list(maxIter=100, msMaxIter = 1e6, msVerbose = FALSE), varf = FALSE), silent = TRUE)
        #pnlsTol=0.04, 
        
      #  print(str(im3))
      }
      
      tree2$H2 <- NULL
      
      
# *************      
      if ( class(im3)=="try-error") {
        im3                 <- NA
        model_iterations[3] <- "0" }
      
      graphics.off()

      if ( !is.na(im1) ) {
        hpred  <- im1$h
        tree2  <- cbind(tree2, hpred)
        tree2  <- tree2 %>% dplyr::rename(h1 = hpred)
      } else {
        tree2$h1  <- 0 
      }     
      
      if ( !is.na(im2) ) {
        hpred  <- im2$h
        tree2  <- cbind(tree2, hpred)
        tree2  <- tree2 %>% dplyr::rename(h2 = hpred)
      } else {
        tree2$h2  <- 0 
      }
      
      if ( !is.na(im3) ) {
        hpred  <- im3$h
        tree2  <- cbind(tree2, hpred)
        tree2  <- tree2 %>% dplyr::rename(h3 = hpred)
      } else {
        tree2$h3  <- 0 
      }
      
      heigth_model_fixed  <- 
        ifelse(!is.na(im3) & model_no < 12, c(im3$model$coefficients$fixed[[1]], im3$model$coefficients$fixed[[2]]),
               ifelse(!is.na(im2) & model_no < 12, c(im2$model$coefficients$fixed[[1]], im2$model$coefficients$fixed[[2]]),
                      ifelse(!is.na(im3) & model_no > 11, c(im3$model$coefficients$fixed[[1]], im3$model$coefficients$fixed[[2]], im3$model$coefficients$fixed[[3]]),
                             ifelse(!is.na(im2) & model_no > 11, c(im2$model$coefficients$fixed[[1]], im2$model$coefficients$fixed[[2]], im3$model$coefficients$fixed[[3]]),
                                    c(0,0)))))
      
      heigth_model_c = ifelse( model_no > 11 & length(heigth_model_fixed)>2, heigth_model_fixed[3], 0)
      model_iter     = paste0( model_iterations[1], model_iterations[2], model_iterations[3] )
      
      if (!is.na(im2)) {
        tree2$imputed <- im2$imputed
      } else {
        tree2$imputed <- FALSE
      }
      
      # tree2$h1[tree2$imputed & tree2$tree_height>=1.3 & tree2$tree_height<100] <- tree2$tree_height[tree2$imputed & tree2$tree_height>=1.3 & tree2$tree_height<100]  
      # tree2$h2[tree2$imputed & tree2$tree_height>=1.3 & tree2$tree_height<100] <- tree2$tree_height[tree2$imputed & tree2$tree_height>=1.3 & tree2$tree_height<100]  
      # tree2$h3[tree2$imputed & tree2$tree_height>=1.3 & tree2$tree_height<100] <- tree2$tree_height[tree2$imputed & tree2$tree_height>=1.3 & tree2$tree_height<100]
      

      tree2 <- tree2 %>%
         dplyr::select( cluster, plot, plot_id, tree_dbh, tree_height, h1, h2, h3, imputed)
      
      # print("model_iter")
      # print(model_iter)
      
      if (model_iter != "000") {
        return( list(c(heigth_model_fixed[[1]], heigth_model_fixed[[2]], heigth_model_c, model_iter ), tree2 ))
      } else {
        return( NA )
      }
      
    },
    
    error= function(cond) {
      message("Main run failed")
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      im1<- NULL; im2<-NULL; im3<- NULL
      return( NA )
    }, finally = {
      message('All done, quitting.')
      
    })
  
}




tree_modelling <- function(model_index, model_list, tree, 
                           est_type, random_effect, 
                           Resultfolder, nrap, useVariance) 
  {

  model_no     <- model_list[model_index]

  # create unique temporary plot ID as text variable
  tree$plot_id <- paste( tree$cluster, tree$plot, sep = '_' )
  
  # temporary variables for analysis
  tree$H       <- tree$tree_height

  # SELECT ONLY trees where total h >= 1.35 and <60
  tree$H[tree$H < 1.35 | tree$H >= 60 | (tree$tree_dbh>40 & tree$H<10)] <- NA
    
  # run analysis if at least 20 observations
  if (nrow(tree) >= 20 )  {
    tree$temp <- "1"
#    print(paste0("Malli: ", model_names[model_no]))
    nrap  <- ifelse(model_no < 12 & nrap==3, 2, nrap)    
    
    tulos <- list( Run_estimation(tree2=tree, model_names[model_no], model_index, model_no, Resultfolder, nrap, useVariance))
  } # if

  return( tulos ) 
}