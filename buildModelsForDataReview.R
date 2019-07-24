# buildModelsForDeployment.R
# this script (and similar others?) controls standardized database queries and model training for web deployment

library(dbViewR)
library(incidenceMapR)
library(modelServR)
library(modelTestR)
library(dplyr)
library(tidyr)
library(ggplot2)

SRC <- 'production'
# SRC <- 'simulated_data'

db <- selectFromDB(queryIn= list(SELECT  =c("*")), source = SRC) 

dim(db$observedData %>% nest(-encounter))[1]

db$observedData <- db$observedData  %>% distinct(encounter, .keep_all = TRUE)

dim(db$observedData)


dim(db$observedData)[1]-sum(is.na(db$observedData$residence_census_tract))
dim(db$observedData)[1]-sum(is.na(db$observedData$age_range_fine_lower))

dim(db$observedData)[1]-sum(db$observedData$number_pathogens_tested==0)


db$observedData <- db$observedData[!is.na(db$observedData$residence_census_tract),]
dim(db$observedData)
dim(db$observedData)[1]-sum(db$observedData$number_pathogens_tested==0)


# childrens dupes
sum(db$observedData$encounter[db$observedData$site_type=='childrensHospital'] %in% 
      db$observedData$encounter[db$observedData$site_type=='retrospective'])
sum(is.na(db$observedData$encounter[db$observedData$site_type=='childrensHospital'] %in% 
      db$observedData$encounter[db$observedData$site_type=='retrospective']))


pathogens <- db$observedData %>% distinct(encounter, .keep_all = TRUE) %>% group_by(pathogen) %>% summarize(n = n()) %>% arrange(desc(n))
pathogens$pathogen
pathogens$n
pathogenList<-pathogens
sum(pathogens$n[pathogens$pathogen != 'not_yet_tested'])
pathogens <- c('all',pathogens$pathogen[pathogens$n >= 100 | grepl('Flu',pathogens$pathogen) | grepl('EV',pathogens$pathogen)])

# pathogens<-c('all','Flu_A_H1','Flu_A_H3')

fluVax <- db$observedData %>% group_by(flu_shot) %>% summarize(n=n())
1-fluVax$n[1]/sum(fluVax$n[c(1:2)])


sites <- db$observedData %>% distinct(encounter, .keep_all = TRUE) %>% group_by(site_type) %>% summarize(n = n()) %>% arrange(desc(n))
sites$site_type
sites$n

keptSites <- c("retrospective","childcare","homelessShelter","collegeCampus","childrensHospital","workplace")



allWeeks <- sort(unique(db$observedData$encountered_week))

weekLabels<-allWeeks
weekLabels[seq(2,length(allWeeks),by=2)]=''

##############################
## time-independent maps #####
##############################
factors   <- c('all','site_type')

geoLevels <- list( seattle_geojson = c('residence_neighborhood_district_name','residence_cra_name','residence_census_tract'),
                   king_county_geojson = c('residence_puma')#,
                   # wa_geojson = c('residence_puma') # census tract impossible due to memory limits
)

# geoLevels <- list( # seattle_geojson = c('residence_cra_name')#,
#                    # king_county_geojson = c('residence_puma'),
#                    wa_geojson = c('residence_puma') # census tract impossible due to memory limits
# )

# catchments: number of subjects with pathogen and factor at residence location 
for (SOURCE in names(geoLevels)){
  for (PATHOGEN in pathogens){
    for (FACTOR in factors){
      for (GEO in geoLevels[[SOURCE]]){
        
        if(FACTOR=='all'){
          queryIn <- list(
            SELECT   =list(COLUMN=c('pathogen', GEO)),
            WHERE    =list(COLUMN='pathogen', IN=PATHOGEN),
            GROUP_BY =list(COLUMN=GEO),
            SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
          )
        } else {
          queryIn <- list(
            SELECT   =list(COLUMN=c('pathogen', FACTOR, GEO)),
            WHERE    =list(COLUMN='pathogen', IN=PATHOGEN),
            GROUP_BY =list(COLUMN=c(FACTOR,GEO)),
            SUMMARIZE=list(COLUMN=FACTOR, IN= 'all')
          )
        }
        
        shp <- masterSpatialDB(shape_level = gsub('residence_','',GEO), source = SOURCE)
        
        db <- expandDB( selectFromDB(  queryIn, source=SRC, na.rm=TRUE ), shp=shp )
        
        modelDefinition <- smoothModel(db=db, shp=shp)
        
        # training occassionaly segfaults but it does not appear to be deterministic...
        tryCatch(
          {
            model <- modelTrainR(modelDefinition)
            
            print(summary(model$inla))
            
            saveModel(model)
            
            if(FACTOR == 'all'){
              dir.create('/home/rstudio/seattle_flu/july_22_plots/', showWarnings = FALSE)
              fname <- paste('/home/rstudio/seattle_flu/july_22_plots/',paste('inla_observed',PATHOGEN,SOURCE,GEO,sep='-'),'.png',sep='')
              png(filename = fname,width = 6, height = 5, units = "in", res = 300)
              ggplotSmoothMap(model,shp,title=FACTOR,shape_level = GEO)
              dev.off()
            }else {
              for(k in unique(model$modeledData[[FACTOR]])){
                tmp<-list(modeledData = model$modeledData[model$modeledData[[FACTOR]]==k,])
                dir.create('/home/rstudio/seattle_flu/july_22_plots/', showWarnings = FALSE)
                fname <- paste('/home/rstudio/seattle_flu/july_22_plots/',paste('inla_observed',PATHOGEN,SOURCE,GEO,FACTOR,k,sep='-'),'.png',sep='')
                png(filename = fname,width = 6, height = 5, units = "in", res = 300)
                ggplotSmoothMap(tmp,shp,title=k,shape_level = GEO)
                dev.off()
              }
            }
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
        
      }
    }
  }
}

#####################################
###### timeseries latent field models ############
#####################################

# pathogens<-c('all','Flu_A_H1','Flu_A_H3','RSVA','RSVB')

factors   <- c('site_type')

# geoLevels <- list( seattle_geojson = c('residence_neighborhood_district_name')#,
#                    #king_county_geojson = c('residence_puma')#,
#                    # wa_geojson = c('residence_puma') # census tract impossible due to memory limits
# )

# number of subjects with pathogen and factor at residence location 
for (SOURCE in names(geoLevels)){
  for (PATHOGEN in pathogens){
    for (GEO in geoLevels[[SOURCE]]){
      
      queryIn <- list(
        SELECT   =list(COLUMN=c('pathogen', factors, GEO,'encountered_week')),
        WHERE    =list(COLUMN='pathogen', IN=PATHOGEN),
        GROUP_BY =list(COLUMN=c(factors,GEO,"encountered_week")),
        SUMMARIZE=list(COLUMN='pathogen', IN= PATHOGEN)
      )
      
      shp <- masterSpatialDB(shape_level = gsub('residence_','',GEO), source = SOURCE)
      
      db <- expandDB( selectFromDB(  queryIn, source=SRC, na.rm=TRUE ), shp=shp )
      
      # training occassionaly segfaults on but it does not appear to be deterministic...
      tries <- 0
      success<-0
      while (success==0 & tries<=2){
        tries <- tries+1
        tryCatch(
          {
            
            db <- appendCatchmentModel(db,shp=shp, source=SRC, na.rm=TRUE  )
            
            modelDefinition <- latentFieldModel(db=db, shp=shp)
            model <- modelTrainR(modelDefinition)
            
            print(summary(model$inla))
            
            saveModel(model)
            
            dir.create('/home/rstudio/seattle_flu/july_22_plots/', showWarnings = FALSE)
            fname <- paste('/home/rstudio/seattle_flu/july_22_plots/',paste('inla_latent',PATHOGEN,SOURCE,GEO,'encountered_week',sep='-'),'.png',sep='')
            png(filename = fname,width = 7, height = 5, units = "in", res = 300)
            print(
              ggplot(model$latentField) + 
                    geom_line(aes_string(x='encountered_week',y="modeled_intensity_mode", color=GEO,group =GEO)) + 
                    # geom_ribbon(aes_string(x='encountered_week',ymin="modeled_intensity_lower_95_CI", ymax="modeled_intensity_upper_95_CI", fill=GEO,group =GEO),alpha=0.1) + 
                    # guides(color=FALSE) + 
                    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                    scale_x_discrete(breaks=allWeeks, labels=weekLabels, limits=allWeeks)+
                    xlab('')+ylab('intensity') 
              )
            dev.off()
            
            fname <- paste('/home/rstudio/seattle_flu/july_22_plots/',paste('inla_latent',PATHOGEN,SOURCE,GEO,'median_encountered_week',sep='-'),'.png',sep='')
            png(filename = fname,width = 4, height = 5, units = "in", res = 300)
            
            
            bbox<-sf::st_bbox(shp$geometry)
            
            mapSettings <- ggplot() + #xlim(c(min(122.5, -bbox[1]),max(121.7,-bbox[3]))) + ylim(c(max(47.17,bbox[2]),min(47.76,bbox[4]))) +
              theme_bw() +
              theme(axis.text=element_blank(),axis.ticks=element_blank(),panel.grid.major=element_line(colour="transparent"), panel.border = element_blank())
            p<-mapSettings + geom_sf(data=shp,size=0.1,aes(fill=NaN))
            
            
            plotDat <- right_join(model$latentField %>% group_by_(.dots =GEO) %>% summarise(modeled_intensity_peak = encountered_week[modeled_intensity_median == max(modeled_intensity_median)]),shp, by=GEO)
            colorLabels<-allWeeks[c(2:3,8:17)]
            
            plotDat$modeled_intensity_peak<-factor(plotDat$modeled_intensity_peak,
                                                   levels=colorLabels)
            
            plotDat$integer_peak <- as.integer(plotDat$modeled_intensity_peak)
            
            # colorLimits<-c(min(plotDat$integer_peak,na.rm=TRUE),max(plotDat$integer_peak,na.rm=TRUE))
            colorLimits<-c(1,length(colorLabels))
            colorBreaks<-min(colorLimits):max(colorLimits)
            colorLabels[2]<-''
            p2 <- p + geom_sf(data=plotDat,size=0, aes(fill=integer_peak))  +
              guides(fill=guide_legend(title="peak intensity")) +
              viridis::scale_fill_viridis(na.value="black",breaks=colorBreaks,
                                          limits=colorLimits,
                                          labels=colorLabels) 
            
            
            print(p2)
            # ggplotLatentMap(model,shp,title='',shape_level = GEO)
            dev.off()
            
            success<-1
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
      }
    }
  }
}



#####################################
###### smooth timeseries models that match latent_field (no factors) ############
#####################################

# number of subjects with pathogen and factor at residence location 
for (SOURCE in names(geoLevels)){
  for (PATHOGEN in pathogens){
    for (GEO in geoLevels[[SOURCE]]){
      
      queryIn <- list(
        SELECT   =list(COLUMN=c('pathogen', GEO,'encountered_week')),
        WHERE    =list(COLUMN='pathogen', IN=PATHOGEN),
        GROUP_BY =list(COLUMN=c(GEO,"encountered_week")),
        SUMMARIZE=list(COLUMN='pathogen', IN= PATHOGEN)
      )
      
      shp <- masterSpatialDB(shape_level = gsub('residence_','',GEO), source = SOURCE)
      
      db <- expandDB( selectFromDB(  queryIn, source=SRC, na.rm=TRUE ), shp=shp )
      
      # training occassionaly segfaults on but it does not appear to be deterministic...
      tries <- 0
      success<-0
      while (success==0 & tries<=2){
        tries <- tries+1
        tryCatch(
          {
            
            modelDefinition <- smoothModel(db=db, shp=shp)
            model <- modelTrainR(modelDefinition)
            
            print(summary(model$inla))
            
            saveModel(model)
            
            dir.create('/home/rstudio/seattle_flu/july_22_plots/', showWarnings = FALSE)
            fname <- paste('/home/rstudio/seattle_flu/july_22_plots/',paste('inla_observed',PATHOGEN,SOURCE,GEO,'encountered_week',sep='-'),'.png',sep='')
            png(filename = fname,width = 7, height = 5, units = "in", res = 300)
            print(ggplot(model$modeledData) + 
                    geom_line(aes_string(x='encountered_week',y="modeled_count_mode", color=GEO,group =GEO)) + 
                    # geom_line(aes_string(x='encountered_week',y="positive", color=GEO,group =GEO),size=0.1) + 
                    # geom_ribbon(aes_string(x='encountered_week',ymin="modeled_count_lower_95_CI", ymax="modeled_count_upper_95_CI", fill=GEO,group =GEO),alpha=0.1) + 
                    # guides(color=FALSE) + 
                    theme(axis.text.x = element_text(angle = 90, hjust = 1))) +
                    scale_x_discrete(breaks=allWeeks, labels=weekLabels, limits=allWeeks)+
                    xlab('')
            dev.off()
            
            success<-1
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
      }
    }
  }
}


###########################################
# age-distributions by pathogen and factor#
###########################################
# eventually this should be multinomial models, but indepdendent binomial for now
for (PATHOGEN in pathogens){
  for (FACTOR in factors){
    
    queryIn <- list(
      SELECT   =list(COLUMN=c('pathogen',FACTOR,'age_range_fine_lower')),
      WHERE    =list(COLUMN='pathogen', IN= PATHOGEN),
      GROUP_BY =list(COLUMN=c(FACTOR,'age_range_fine_lower')),
      SUMMARIZE=list(COLUMN='pathogen', IN= 'all')
    )
    
    db<- selectFromDB(  queryIn, source=SRC, na.rm=TRUE  ) 
    
    # get all ages denominator. I'm not sure how to implement this as single query..
    tmp <- selectFromDB(  queryIn[c('SELECT','GROUP_BY','SUMMARIZE')], source=SRC, na.rm=TRUE  )$observedData %>% 
      group_by_(.dots=c(FACTOR,'age_range_fine_lower')) %>% summarize(n=sum(n))
    db$observedData <- db$observedData %>% select(-n) %>% left_join(tmp)
    
    db <- expandDB(db)
    
    modelDefinition <- smoothModel(db=db, shp=shp)
    model <- modelTrainR(modelDefinition)
    print(summary(model$inla))
    
    saveModel(model)
    
    if (model$modelDefinition$family[1]=='poisson'){
      p1<-ggplot(model$modeledData) + geom_line(aes(x=age_range_fine_lower ,y=modeled_count_mode, group=pathogen)) + 
        geom_point(aes(x=age_range_fine_lower, y=positive, group=pathogen)) + 
        geom_ribbon(aes(x=age_range_fine_lower ,ymin=modeled_count_lower_95_CI, ymax=modeled_count_upper_95_CI  , group=pathogen), alpha=0.3) + 
        facet_wrap(FACTOR) +
        ylim(c(0,2*max(model$modeledData$modeled_count_mode)))
    } else if (model$modelDefinition$family[1]=='binomial'){
      p1<-ggplot(model$modeledData) + geom_line(aes(x=age_range_fine_lower ,y=modeled_fraction_mode, group=pathogen)) + 
        geom_point(aes(x=age_range_fine_lower, y=positive/n, group=pathogen)) + 
        geom_ribbon(aes(x=age_range_fine_lower ,ymin=modeled_fraction_lower_95_CI, ymax=modeled_fraction_upper_95_CI  , group=pathogen), alpha=0.3) + 
        facet_wrap(FACTOR) +
        ylim(c(0,1))
    }
    
    dir.create('/home/rstudio/seattle_flu/july_22_plots/', showWarnings = FALSE)
    fname <- paste('/home/rstudio/seattle_flu/july_22_plots/',paste('inla_observed',PATHOGEN,FACTOR,'age_range_fine',sep='-'),'.png',sep='')
    png(filename = fname,width = 6, height = 5, units = "in", res = 300)
    print(p1)
    dev.off()
  }
}


###################################################



#####################################
#### site-type smooth models
#####################################

# number of subjects with pathogen and factor at residence location 
  for (PATHOGEN in pathogens){
      
      queryIn <- list(
        SELECT   =list(COLUMN=c('site_type','pathogen','encountered_week','age_range_fine_lower')),
        WHERE    =list(COLUMN='pathogen', IN=PATHOGEN),
        WHERE    =list(COLUMN='site_type', IN=keptSites),
        GROUP_BY =list(COLUMN=c('site_type',"encountered_week",'age_range_fine_lower')),
        SUMMARIZE=list(COLUMN='pathogen', IN= PATHOGEN)
      )
      
      db <- expandDB( selectFromDB(  queryIn, source=SRC, na.rm=TRUE ) )
      
      # training occassionaly segfaults on but it does not appear to be deterministic...
      tries <- 0
      success<-0
      while (success==0 & tries<=2){
        tries <- tries+1
        tryCatch(
          {
            
            modelDefinition <- smoothModel(db=db, shp=shp)
            model <- modelTrainR(modelDefinition)
            
            print(summary(model$inla))
            
            saveModel(model)
            
            plotDat<-model$modeledData
            plotDat <- plotDat %>% group_by(site_type) %>% 
              mutate(modeled_count_norm = modeled_count_mode/max(modeled_count_mode)) %>%
              mutate(modeled_count_norm_upper_sd = pmin(1,modeled_count_norm + modeled_count_sd/max(modeled_count_mode))) %>%
              mutate(modeled_count_norm_lower_sd = pmax(0,modeled_count_norm - modeled_count_sd/max(modeled_count_mode)))
            
            plotDat$site_type<-factor(plotDat$site_type, levels=keptSites)
            plotDat$height <- c(diff(plotDat$age_range_fine_lower),5)
            plotDat$height[plotDat$height < 0]<-5
            plotDat$modeled_count_norm[plotDat$modeled_count_norm <0.01] <- NaN
            
            weekLabels<-allWeeks
            weekLabels[seq(2,length(allWeeks),by=2)]=''
            
            fname <- paste('/home/rstudio/seattle_flu/july_22_plots/',paste('inla_observed',PATHOGEN,'site_type-encountered_week-age_range_fine_lower',sep='-'),'.png',sep='')
            png(filename = fname,width = 8, height = 5, units = "in", res = 300)
            print(
              ggplot(plotDat, aes(encountered_week,age_range_fine_lower)) + 
                geom_tile(aes(fill=modeled_count_norm, height=height)) + 
                facet_wrap('site_type')+
                theme_bw()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                viridis::scale_fill_viridis(na.value='white', name='count\nsmoothed\n& normed', trans='sqrt', breaks=c(0.1,0.25,0.5,0.75,1))+
                # scale_x_discrete(labels=c(rbind(unique(plotDat$encountered_week)[seq(1,50,by=2)],rep('',50)))) +
                scale_x_discrete(breaks=allWeeks, labels=weekLabels, limits=allWeeks)+
                xlab('')+ylab('age') 
              )
            dev.off()
            
            # dir.create('/home/rstudio/seattle_flu/july_22_plots/', showWarnings = FALSE)
            # fname <- paste('/home/rstudio/seattle_flu/july_22_plots/',paste('inla_observed',PATHOGEN,'site_type-encountered_week',sep='-'),'.png',sep='')
            # png(filename = fname,width = 8, height = 5, units = "in", res = 300)
            # 
            # 
            # 
            # print(
            #   ggplot(plotDat) + 
            #         geom_line(aes_string(x='encountered_week',y="modeled_count_norm", color='site_type',group ='site_type')) + 
            #         # geom_line(aes_string(x='encountered_week',y="positive", color=GEO,group =GEO),size=0.1) + 
            #         geom_ribbon(aes_string(x='encountered_week',ymin="modeled_count_norm_lower_sd", ymax="modeled_count_norm_upper_sd", fill='site_type',group ='site_type'),alpha=0.1) +
            #         # guides(color=FALSE) + 
            #         facet_wrap('site_type')+
            #         theme(axis.text.x = element_text(angle = 90, hjust = 1))
            #   )
            # dev.off()
            # 
            success<-1
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
     
    }
  }


##################
###### test smaller sample sizes for flu ############

geoLevels <- list( seattle_geojson = c('residence_neighborhood_district_name')#,
                   #king_county_geojson = c('residence_puma')#,
                   # wa_geojson = c('residence_puma') # census tract impossible due to memory limits
)

# number of subjects with pathogen and factor at residence location 
for (SOURCE in names(geoLevels)){
  for (PATHOGEN in c('Flu_A_H3')){
    for (GEO in geoLevels[[SOURCE]]){
      
      queryIn <- list(
        SELECT   =list(COLUMN=c('pathogen', factors, GEO,'encountered_week')),
        WHERE    =list(COLUMN='pathogen', IN=PATHOGEN),
        GROUP_BY =list(COLUMN=c(factors,GEO,"encountered_week")),
        SUMMARIZE=list(COLUMN='pathogen', IN= PATHOGEN)
      )
      
      shp <- masterSpatialDB(shape_level = gsub('residence_','',GEO), source = SOURCE)
      
      db <- expandDB( selectFromDB(  queryIn, source=SRC, na.rm=TRUE ), shp=shp )
      
      
      # downsample
      db$observedData$positive <- rpois(length(db$observedData$positive),0.2*db$observedData$positive)
      db$observedData$n <- db$observedData$positive
      
      
      
      # training occassionaly segfaults on but it does not appear to be deterministic...
      tries <- 0
      success<-0
      while (success==0 & tries<=2){
        tries <- tries+1
        tryCatch(
          {
            
            db <- appendCatchmentModel(db,shp=shp, source=SRC, na.rm=TRUE  )
            
            modelDefinition <- latentFieldModel(db=db, shp=shp)
            model <- modelTrainR(modelDefinition)
            
            print(summary(model$inla))
            
            # saveModel(model)
            
            dir.create('/home/rstudio/seattle_flu/july_22_plots/', showWarnings = FALSE)
            fname <- paste('/home/rstudio/seattle_flu/july_22_plots/',paste('inla_latent',PATHOGEN,SOURCE,GEO,'encountered_week-downsampled',sep='-'),'.png',sep='')
            png(filename = fname,width = 7, height = 5, units = "in", res = 300)
            print(
              ggplot(model$latentField) + 
                geom_line(aes_string(x='encountered_week',y="modeled_intensity_mode", color=GEO,group =GEO)) + 
                # geom_ribbon(aes_string(x='encountered_week',ymin="modeled_intensity_lower_95_CI", ymax="modeled_intensity_upper_95_CI", fill=GEO,group =GEO),alpha=0.1) + 
                # guides(color=FALSE) + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1))+
                scale_x_discrete(breaks=allWeeks, labels=weekLabels, limits=allWeeks)+
                xlab('')+ylab('intensity') 
            )
            dev.off()
            
            fname <- paste('/home/rstudio/seattle_flu/july_22_plots/',paste('inla_latent',PATHOGEN,SOURCE,GEO,'median_encountered_week-downsampled',sep='-'),'.png',sep='')
            png(filename = fname,width = 4, height = 5, units = "in", res = 300)
            
            
            bbox<-sf::st_bbox(shp$geometry)
            
            mapSettings <- ggplot() + #xlim(c(min(122.5, -bbox[1]),max(121.7,-bbox[3]))) + ylim(c(max(47.17,bbox[2]),min(47.76,bbox[4]))) +
              theme_bw() +
              theme(axis.text=element_blank(),axis.ticks=element_blank(),panel.grid.major=element_line(colour="transparent"), panel.border = element_blank())
            p<-mapSettings + geom_sf(data=shp,size=0.1,aes(fill=NaN))
            
            
            plotDat <- right_join(model$latentField %>% group_by_(.dots =GEO) %>% summarise(modeled_intensity_peak = encountered_week[modeled_intensity_median == max(modeled_intensity_median)]),shp, by=GEO)
            colorLabels<-allWeeks[c(2:3,8:17)]
            
            plotDat$modeled_intensity_peak<-factor(plotDat$modeled_intensity_peak,
                                                   levels=colorLabels)
            
            plotDat$integer_peak <- as.integer(plotDat$modeled_intensity_peak)
            
            # colorLimits<-c(min(plotDat$integer_peak,na.rm=TRUE),max(plotDat$integer_peak,na.rm=TRUE))
            colorLimits<-c(1,length(colorLabels))
            colorBreaks<-min(colorLimits):max(colorLimits)
            colorLabels[2]<-''
            p2 <- p + geom_sf(data=plotDat,size=0, aes(fill=integer_peak))  +
              guides(fill=guide_legend(title="peak intensity")) +
              viridis::scale_fill_viridis(na.value="black",breaks=colorBreaks,
                                          limits=colorLimits,
                                          labels=colorLabels) 
            
            
            print(p2)
            # ggplotLatentMap(model,shp,title='',shape_level = GEO)
            dev.off()
            
            success<-1
            
          }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
        )
      }
    }
  }
}





