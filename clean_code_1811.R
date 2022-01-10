##New neat fish SDM script ####

#packages from my other script ##
library(sdmpredictors)
library(dismo)
library(reshape2)
library(dplyr)
library(maptools)
library(mapdata)
library(ggmap)
library(sf)
library(rgeos)
library(corrplot)
library(usdm)
library(RStoolbox)
library(sdm)
library(ggmap)
library(ggplot2)
library(raster)
library(lme4)
library(mgcv)
library(MASS)
library(rgdal)
library(officer)
library(sqldf)
library(flextable)

#set wd
setwd("D:/maria_kaust/SDM_kaust")

#setwd("F:/maria_kaust/SDM_kaust")

#area we're predicting into = reef_poly +2km buffer dissolved into one poly
#import in new dissolved reef and sites 
#predict_area<-readOGR('final_site_polydissolve_MAY.shp')


#rasterise to get coverage and use the cells that are partially covered as well as fully 
#plot(env_crop)
#rasterise_poly<- rasterize(predict_area, env_crop[[1]], getCover=TRUE)
#plot(rasterise_poly)

#rasterise_poly[rasterise_poly==0]<-NA

#par(mfrow=c(1,1))
#plot(rasterise_poly)

#mask<-rasterise_poly

#plot(mask)

#writeRaster(mask, 'mask_final_may.tif')

#par(mfrow=c(1,1))

#plot(predict_area)

#crs(predict_area)

#raster mask area
mask<- raster('mask_final_may.tif')

par(mfrow=c(1,1))
plot(mask)


crs(mask)

#ok now read in environmental data 
#list layers from bio-oracle
layers.bio2 <- list_layers( datasets="Bio-ORACLE" ) 

options(sdmpredictors_datadir="D:/maria_kaust/SDM_kaust")

#options(sdmpredictors_datadir="F:/maria_kaust/SDM_kaust")

#checking sst range

#sst_range<-load_layers('BO21_temprange_bdmean')

#plot(sst_range)
#sst_range<-crop(sst_range, extent(mask)) 
#sst_range<-mask(sst_range, mask)



#plot(sst_range)

#sst_range<-mask(sst_range, mask)

#plot(sst_range)

#extract data 
current_preds<-load_layers(c('BO21_tempmean_ss','BO21_temprange_ss','BO21_temprange_bdmean', 'BO2_chlorange_bdmean',  'BO21_salinitymean_bdmean', 'BO21_nitratemean_bdmean', 'BO21_dissoxmean_bdmean',
                             'BO21_phosphatemean_bdmean',
                             'BO_parmean' , 'BO_bathymin','BO_damean'))
res(current_preds)

bathy<- load_layers('BO_bathymin')

bathy<-crop(bathy, extent(mask))

par(mfrow=c(1,1))
plot(bathy)

#deep<-bathy

#deep[deep> -100]<-NA

#plot(deep)

#deeppoly<- rasterToPolygons(deep, na.rm = T)
#plot(deeppoly)

#writeOGR(deeppoly, dsn='deep_poly', layer='poly_100', driver = 'ESRI Shapefile', overwrite_layer = TRUE)

#log bathymetry???? does this make the correlation worse? 

#bathy<-mask(bathy,mask)
#plot(bathy)

#par(mfrow=c(1,1))

#shallow<-bathy
#shallow[shallow< -15]<-NA
#plot(shallow)

#length(which(! is.na(shallow[])) ) #99

#medium1<-bathy

#medium1[ medium1> -15 ]<-NA
#medium1[medium1< -30]<-NA 

#plot(medium1)

#length(which(! is.na(medium1[]))) #171

#medium2<- bathy
#medium2[medium2 > -30]<-NA
#medium2[medium2< -100]<-NA
#length(which(!is.na(medium2[]))) #687


#deep<-bathy

#deep[deep> -100]<-NA

#plot(deep)

#deep<- writeRaster(deep, 'deep_poly/deep_raster100.tif')
#deeppoly<- rasterToPolygons(deep, na.rm = T)
#plot(deeppoly)

#writeOGR(deeppoly, dsn='deep_poly', layer='poly_100', driver = 'ESRI Shapefile')


#ok depth of surrounding cells ? too hard
#vl = lapply(1:length(bathy), function(x){
# extract(bathy, 
#          (c(x,(adjacent(bathy, x, directions=8, pairs=F, sorted=F)))))
#})

# find the mean for each cell
#vm = sapply(1:length(bathy), function(x){
#  as.vector(mean(vl[[x]], na.rm = T))
#})

# create raster template
#templ = bathy/bathy

# multiply into template for new raster
#ras = vm*templ

#par(mfrow=c(1,2))
#plot(ras)
#plot(bathy)

#ras<- mask(ras, mask)
#plot(ras)
#plot(bathy)
#bathy<- mask(bathy, mask)
#near_depth<- ras

#get extent to crop the bio-oracle layer 
extent(mask)
env_crop<- crop(current_preds, extent(mask)) 

writeRaster(env_crop[[1]], 'mask_test2.tif')

par(mfrow=c(3,4))
plot(env_crop)

#mask the environmental data
env_mask<- mask(env_crop, mask)

plot(env_mask)
writeRaster(env_mask[[1]], 'mask_test.tif')

#sort out the depth raster
#distance<-raster('deep_dis1001.tif')
#plot(distance)

#distance<-mask(distance, mask)
#plot(distance)

#add to the raster stack
extent(env_mask)
res(env_mask)


#res(distance)

#env_mask<-stack(env_mask, distance)

par(mfrow=c(3,4))

plot(env_mask)

#check the correlations
#install.packages('ENMTools')
library(ENMTools)

correlations<- raster.cor.plot(env_mask, method='pearson')

correlations

corr.df<-correlations$cor.heatmap$data



#find out which ones are above 
which(corr.df$value > 0.6 )

high_cor<-corr.df[which(corr.df$value > 0.6 ),]

#remove the values where corr=1 as they're the same env var
torm<- which(high_cor$value==1)
high_cor<-high_cor[-torm,]

#temp_range_ss, salinity, nitrate, dissox and phosphate correlated with temp so remove them
#remove bathy anyway as it doesn;t show anything

names(env_mask)

env_mask_filter<- env_mask[[c(1,3, 4, 9, 10,11)]]

names(env_mask_filter)

#writeRaster(env_mask_filter[[5]], 'bathy_pu2.tif')

correlations<- raster.cor.plot(env_mask_filter, method='pearson')
correlations

corr.df_test<- correlations$cor.heatmap$data
which(corr.df_test$value > 0.6 )

#ok we've got rid of the correlated and are left with temp mean,range, chlo range, par and diffuse atten and deep distance
par(mfrow=c(2,3))
plot(env_mask_filter)


#ok now we're ready to load the survey data

raw_fish<- read.csv('final_data/Raw_fish_data_Red_Sea_IDW.csv')
fish_bio<-read.csv('final_data/Red_Sea_Fish_Biodiversity_Compilied_Feb_10_2021.csv')
rls_fish<-read.csv('final_data/Reef_Life_Survey_(RLS)#_Global_reef_fish_dataset-fish_surveys.csv')
perg<- read.csv('final_data/Maher_PERSGA_Data.csv')
tmp<-read.csv('final_data/tmpBioReg.csv')

perg_sites<-perg[,c(2,3,4)]
torm<-which(perg_sites$Site.Common.name=='')
perg_sites<-perg_sites[-c(torm),]
write.csv(perg_sites, 'persga_sites.csv')


unique(raw_fish[raw_fish$Country=='Saudi Arabia', 3 ])
(unique(tmp$DATE))

unique(fish_bio$SITE_NUMBER)


#read in fish bio site data to connect the obs with the lat lon 

bio_sites<- read.csv('final_data/red_sea_biodiversity_sites.csv')


bio_sites<-bio_sites[,c(1,5,6)]

bio_sites$SITE_NUMBER<-as.character(bio_sites$SITE_NUMBER)

site_unique<-unique(fish_bio$SITE_NUMBER)
bio_site_unique<- bio_sites$SITE_NUMBER

site_unique[c(! site_unique %in% bio_site_unique)]

bio_site_unique[c(! bio_site_unique %in% site_unique)]

bio_sites$SITE_NUMBER[bio_sites$SITE_NUMBER=='4a']<-"4A"
bio_sites$SITE_NUMBER[bio_sites$SITE_NUMBER=='4b']<-"4B"
bio_sites$SITE_NUMBER[bio_sites$SITE_NUMBER=='5b']<-"5B"
bio_sites$SITE_NUMBER[bio_sites$SITE_NUMBER=='5b']<-"5B"
bio_sites$SITE_NUMBER[bio_sites$SITE_NUMBER=="Island 1"]<- "ISLAND 1"
bio_sites$SITE_NUMBER[bio_sites$SITE_NUMBER=="Island 2"]<- "ISLAND 2"
bio_sites$SITE_NUMBER[bio_sites$SITE_NUMBER=="Island 3"]<- "ISLAND 3"
bio_sites$SITE_NUMBER[bio_sites$SITE_NUMBER=="Island 4"]<- "ISLAND 4"
bio_sites$SITE_NUMBER[bio_sites$SITE_NUMBER=="Island 5"]<- "ISLAND 5"

fish_bio_site<- left_join(fish_bio, bio_sites, by='SITE_NUMBER')

#now we have the three things with lat lon need to merge
#fish_bio_site
#rls_fish
#raw_fish
#persga
#tmp

#ok first do the abundance models (so just the rls and raw fish)
#sort raw fish

#count total is the abundance of all the fish across the size classes, now work out the abundance across the three transects 
raw_fish$count<-raw_fish$Count.Total

#get the columns you need
raw_fish_sum<- raw_fish[,c(1,8,12,13,25,26,83)]

which(raw_fish_sum$Site=='') #lots of un-named sites but the lat longs are available so hopefully its ok

raw_fish_sum<- raw_fish_sum %>% group_by(TAXONNAME, Site, Latitude, Longitude, Family) %>% summarise(count_av=(sum(count))/3)

#ok now round it up
raw_fish_sum$count_av<-ceiling(raw_fish_sum$count_av)

raw_fish_sum<- raw_fish_sum[,c(1,3,4,5,6)]

#ok now do the same for the rls data
rls_sum<- rls_fish[,c(7,9,10,15,16,17,18)]

rls_sum<- rls_sum %>% group_by(SiteCode,SiteLat,SiteLong,Family, Taxon) %>% summarise(count_av=(sum(Total))/2)

rls_sum$count_av<- ceiling(rls_sum$count_av)

#now sort the pergsa data
perg<- perg[perg$type=='Mean',]

perg$Site.Common.name<- c(1:69)

perg_sites<- perg[,c(2:4)]

perg<-perg[,c(2,6:14)]

library(reshape2)
perg_long<- melt(perg, id="Site.Common.name", variable.name = 'species')

#add the lat lon back 
perg_long<- left_join(perg_long, perg_sites, by="Site.Common.name")

unique(perg_long$species)

perg_long$species<-as.character(perg_long$species)

perg_long$species[perg_long$species=='Stenopus.hispidus'] <-'Stenopus hispidus'
perg_long$species[perg_long$species=='Echinothrix.spp.'] <-'Echinothrix spp.'
perg_long$species[perg_long$species=='H..mammilatus'] <-'H mammilatus'
perg_long$species[perg_long$species=='Tripneustes.spp'] <-'Tripneustes spp'
perg_long$species[perg_long$species=='Acanthaster.plancii'] <-'Acanthaster plancii'
perg_long$species[perg_long$species=='Charonia.tritonis'] <-'Charonia tritonis'
perg_long$species[perg_long$species=='Cheilinus.lunalatus'] <-'Cheilinus lunalatus'
perg_long$species[perg_long$species=='Cheilinus.undulatus'] <-'Cheilinus undulatus'
perg_long$species[perg_long$species=='Bolbometopon.muricatum'] <-'Bolbometopon muricatum'

#remove the three gulf of aden sites sites 67,68,69
torm<- which(perg_long$Site.Common.name==67)
perg_long<- perg_long[-c(torm),]

torm<- which(perg_long$Site.Common.name==68)
perg_long<-perg_long[-c(torm),]

torm<-which(perg_long$Site.Common.name==69)
perg_long<-perg_long[-c(torm),]

write.csv(perg_long, 'final_data/persga_long.csv')

#remove the zeros because there's no zeros in the other dat
torm<-which(perg_long$value==0.00)

perg_abun<- perg_long[-c(torm),]

#add family as 'good' so it doesnt get filtered out 
perg_abun$Family<-'good'
perg_abun$Family[perg_abun$species=='Cheilinus lunalatus']<-"Labridae"
perg_abun$Family[perg_abun$species=='Cheilinus undulatus']<-"Labridae"
perg_abun$Family[perg_abun$species=='Bolbometopon muricatum']<-"Scaridae"

head(perg_abun)

perg_abun<-perg_abun[,c(2,4,5,6,3)]

names(perg_abun)<- c('TAXONNAME', 'Latitude', 'Longitude','Family', 'count_av')

#sort the tmp data 
names(tmp)

tmp<- tmp[,c(3,4,7,11,12,14)]
tmp <- tmp %>% group_by(LATITUDE, LONGITUDE, TAXONNAME, FAMILY) %>% summarise(count=ceiling(mean(COUNT)))

names(tmp)

tmp<- tmp[,c(3,1,2,4,5)]
names(tmp)<- c('TAXONNAME', 'Latitude', 'Longitude','Family', 'count_av')


#combine the two data sets
head(rls_sum)
head(raw_fish_sum)

rls_merge<-rls_sum[,c(5,2,3,4,6)]
names(rls_merge)<- c('TAXONNAME', 'Latitude', 'Longitude','Family', 'count_av')

names(rls_merge)
names(raw_fish_sum)
names(perg_abun)
names(tmp)

abun_dat<- rbind(rls_merge, raw_fish_sum)

abun_dat<-data.frame(abun_dat)
head(abun_dat)
head(perg_abun)
head(tmp)


abun_dat<-rbind(abun_dat, perg_abun)
abun_dat<-data.frame(abun_dat)

tmp<-data.frame(tmp)
abun_dat<-rbind(abun_dat, tmp)

#subset for 'good fish' using MB code, this might not be necessary so can potentially add/ delete this step 
good.fish = c("Carangidae",  "Labridae","Lethrinidae", "Lutjanidae", "Mullidae", 
              "Serranidae","Chaetodontidae", "Fistulariidae", "Acanthuridae", "Kyphosidae",
              "Scaridae", "Siganidae", "Balistidae", "Ephippidae", "Caesionidae", "Zanclidae", "Cheilidactylidae", 
              "Oplegnathidae", "Pomacanthidae", "Pomacentridae", "Cirrhitidae", "Haemulidae", "Nemipteridae", "Microdesmidae", 
              "Diodontidae", "Sphyraenidae", "Aulostomidae", "Ostraciidae",'Monacanthidae', 'Ptereleotridae')

survey<-abun_dat

subset_good <- survey[survey$Family %in% good.fish,] 

subset_good$count_av<- ceiling(subset_good$count_av)

species_list<- unique(subset_good$TAXONNAME)

#alphabetise spp list to check for doubles 
check<-sort(species_list)


#ok now sort these
#Cheilinus lunalatus Cheilinus lunulatus

subset_good$TAXONNAME[subset_good$TAXONNAME=='Cheilinus lunalatus']<-'Cheilinus lunulatus'

#Anampses sp. Anampses spp.
subset_good$TAXONNAME[subset_good$TAXONNAME=='Anampses spp.']<-'Anampses sp.'

#Caesio sp.   Caesionid spp.
subset_good$TAXONNAME[subset_good$TAXONNAME=='Caesionid spp.']<- 'Caesio sp.'

#Chromis dimidata 	Chromis dimidiata
subset_good$TAXONNAME[subset_good$TAXONNAME=='Chromis dimidata']<- 'Chromis dimidiata'

#Lutjanidae   Lutjanus sp.
subset_good$TAXONNAME[subset_good$TAXONNAME=='Lutjanidae']<- 'Lutjanus sp.'

#Oxycheilinus digramma  Oxycheilinus digrammus
subset_good$TAXONNAME[subset_good$TAXONNAME=='Oxycheilinus digrammus']<- 'Oxycheilinus digramma'

#Pomacentrus trichourus  Pomacentrus trichrourus
subset_good$TAXONNAME[subset_good$TAXONNAME=='Pomacentrus trichourus']<- 'Pomacentrus trichrourus'

#Rhinecanthus assai  Rhinecanthus assasi
subset_good$TAXONNAME[subset_good$TAXONNAME=='Rhinecanthus assai']<- 'Rhinecanthus assasi'

#Scarid spp. Scarus sp.
subset_good$TAXONNAME[subset_good$TAXONNAME=='Scarid spp.']<- 'Scarus sp.'

#Sufflamen albicaudatum   Sufflamen albicaudatus
subset_good$TAXONNAME[subset_good$TAXONNAME=='Sufflamen albicaudatus']<- 'Sufflamen albicaudatum'

#get rid of the spp. species
names<-unique(subset_good$TAXONNAME)
#View(names)

sp_torm<- c('Naso spp.' ,  'Caesio sp.' ,  'Anampses sp.' ,  'Cheilinus spp.' , 'Acanthurid spp.' ,	
            'Labrid spp.' , 'Scarus sp.' , 'Halichoeres sp.' , 'Kyphosus sp.' , 'Lethrinus sp.' , 'Lutjanus sp.' , 'Mulloidichthys sp.',
            'Parupeneus sp.' , 'Platax sp.' ,  'Pomacentrus sp.' ,  'Pseudocheilinus sp.', 'Serranidae' , 'Siganus sp.' ,	
            'Stethojulis sp.' , 'Scaridae' , 'Chlorurus sp.' , 'Carangoides sp.', 'Oxycheilinus spp.', 	
            'Caranx sp.', '	Labridae', 'Acanthuridae' , 'Plectropomus sp.', 'Sphyraenidae', 'Labridae')

torm<- which ( subset_good$TAXONNAME %in% sp_torm)

subset_good<-subset_good[-c(torm),]

#View(unique(subset_good$TAXONNAME))

#(subset_good$TAXONNAME == 'Naso spp.' |  'Caesio sp.' |  'Anampses sp.' |  'Cheilinus spp.' | 'Acanthurid spp.' |	
#'Labrid spp.' | 'Scarus sp.' | 'Halichoeres sp.' | 'Kyphosus sp.' | 'Lethrinus sp.' | 'Lutjanus sp.' | 'Mulloidichthys sp.' |
#'Parupeneus sp.' | 'Platax sp.' |  'Pomacentrus sp.' |  'Pseudocheilinus sp.' | 'Serranidae' | 'Siganus sp.' |  	
#'Stethojulis sp.' | 'Scaridae' | 'Chlorurus sp.' | 'Carangoides sp.' )  

### HERE
#ok fixed 
species_list<- unique(subset_good$TAXONNAME)

#ok now get zeros 
subset_good$latlon<- subset_good$Latitude*subset_good$Longitude

latlon_key_abun<- subset_good[,c(2,3,6)]

dups<-which(duplicated(latlon_key_abun))

latlon_key_abun<-latlon_key_abun[-c(dups),]

latlon_key_abun$site_name<-c(1:nrow(latlon_key_abun))

occ_mat_abun<- left_join(subset_good, latlon_key_abun, by='latlon')

unique(occ_mat_abun$site_name)

occ_mat_abun<- occ_mat_abun[,c(1,5,9)]

#now make matrix

occ_mat_abun<-acast(occ_mat_abun, TAXONNAME~site_name, value.var = 'count_av', mean) #ok now all the absences have zero so cast back

zero<-which(is.nan(occ_mat_abun))

occ_mat_zero<-occ_mat_abun

occ_mat_zero[zero]<-0 # ok this worked used this to cast back

#and then make into a pres/abs matrix for number of occurrences in model 

occ_mat_abun_pres<- occ_mat_zero

occ_mat_abun_pres[occ_mat_abun_pres>0]<- 1 #ok can use this for number of occurances 


#cast back
occ_abs_abun<-melt(occ_mat_zero)

names(occ_abs_abun)<- c('TAXONNAME','site_name','count_zero')

occ_abs_abun<- left_join(occ_abs_abun, latlon_key_abun, by='site_name') 

names(subset_good)

names(occ_abs_abun)

occ_abs_abun<- occ_abs_abun[,c(1,4,5,3)]

names(occ_abs_abun)<- c("TAXONNAME",  "Latitude" ,  "Longitude" , "count_av" )

occ_abs_abun$count_av<- ceiling(occ_abs_abun$count_av)

subset_good<- occ_abs_abun

#ok now make the models 
current_preds<- env_mask_filter

plot(current_preds)

#now extract environmental data for every row of subset good
data<-subset_good

write.csv(subset_good, 'abundance_combined_data.csv')

library(raster)

extract_km<-function(data){
  latlon<-data.frame(lon=data$Longitude, lat=data$Latitude)
  extract1<-as.data.frame(raster::extract(current_preds, latlon))
  extract1$lat<-latlon$lat
  extract1$lon<-latlon$lon
  extract1$abundance<-data$count_av
  extract1$species<-data$TAXONNAME
  extract1
  return(extract1)
}

extracted<-extract_km(subset_good) #ok now we have dataframe to model 

torm<- which(is.na(extracted$BO_damean))#remove these lines (for now)  #these are the sites that dont have env data (eg cells do not go close enough to the coast)

missing <-extracted[torm, ]

missing<- missing [,c(7,8)]

missing<- missing[-c(which(duplicated(missing))),]

#write.csv(missing, 'missing_env_data_sitesMAY.csv')

extracted<- extracted[-c(torm),]



lat_lon_abun<- extracted[,c(7,8)]
dups<-which(duplicated(lat_lon_abun)) 
lat_lon_abun<- lat_lon_abun[-c(dups),]
write.csv(lat_lon_abun, 'lat_lon_abun_may.csv')


#come back here ---- 

#
#we have abundance data so can do abundance glm/ gam etc

#ok so lets make planning units from the raster so that each cell has a unique ID 
PuGrid<- rasterToPolygons(current_preds[[1]], n=4, na.rm = TRUE)

# writeOGR(PuGrid, layer='PuGrid', dsn='D:/maria_kaust/SDM_kaust/PuGrid', driver='ESRI Shapefile', overwrite_layer = TRUE )



vals<- getValues(current_preds[[1]])
vals<-na.omit(vals)

predict_df<- data.frame(cell_ID=c(0:(length(vals)-1)), vals=vals) #ok the pu IDs are in the cell_Id here and the FID in the PuGrid poly

#NOW we can model into predict df
#using MB code- build dataframes to fill with results
# build dataframes to put results
fits = data.frame(cbind(predict_df$cell_ID))
colnames(fits)[1] = c("cellID")
fit_ses = data.frame(cbind(predict_df$cell_ID))
colnames(fit_ses) = c("cellID")                  #figure these 4 lines out
rejects = data.frame(Species=as.character(), n = numeric(), stringsAsFactors=FALSE)
models = data.frame(Species=as.character(), Intercept = numeric(),  sst= numeric(), chlo = numeric(), 
                    ph = numeric(), calcite = numeric(), par = numeric(), silicate=numeric(),
                    R2 = numeric(), df = numeric(), logLik = numeric(), AICc = numeric(), 
                    delta = numeric(), weight = numeric(),stringsAsFactors=FALSE)

List_m = list()  # Initial model list
List_avm = list()  # Initial model averaging list

# start building the Word Document for model outputs
#dont have the reporteRS package on my R as it's too new, but using the Officer package
# create new word document
new.word.doc=function(){
  my.doc=read_docx()
  return(my.doc)
}

read_docx()

doc=new.word.doc()


# add a title
add.title=function(doc, my.title){
  my.prop=fp_text(font.size = 14, bold = TRUE, font.family = "Times")
  the.title=fpar(ftext(my.title, prop=my.prop))
  body_add_fpar(doc, the.title)
  body_add_par(doc, " ")
  return("title added")
}

add.title(doc, "Red Sea SDMs - Final Run")

#ok now function for adding a table 
#add a data frame as a table
add.table=function(doc, tbl, col.keys=NULL, col.digits=NULL){
  # create basic flextable
  f.table=qflextable(tbl)
  # set numbers of decimals for numeric variables, if specified
  if(!is.null(col.keys)){
    for(j in 1:length(col.keys)){
      f.table=colformat_num(x=f.table,
                            col_keys=col.keys[j],
                            digits=col.digits[j])
    }
  }
  
  # set table borders
  f.table=border_outer(f.table, part="all",
                       border=fp_border(color="black", width = 1))
  f.table=border_inner_h(f.table, part="all",
                         border=fp_border(color="black", width = 1))
  f.table=border_inner_v(f.table, part="all",
                         border=fp_border(color="black", width = 1))
  
  # set fonts
  f.table=font(f.table,  fontname = "Times", part = "all")
  # also set the table's header font as bold
  f.table=bold(f.table, part = "header")
  
  # add the table to the document
  flextable::body_add_flextable(doc, 
                                value = f.table, 
                                align = "left" )
  return("table added")
}

#non- spatial values table to use predict.gam
names(current_preds)

new_data<- data.frame(cell_ID=predict_df$cell_ID, 'BO21_tempmean_ss' = 1, 'BO21_temprange_bdmean' = 1, 
                      'BO2_chlorange_bdmean' = 1,   'BO_parmean'= 1, 'BO_bathymin'=1, 'BO_damean'= 1)

length(names(current_preds))

#ok env data for every PU in this table
for(i in 1:length(names(current_preds)) ){
  vals<-getValues(current_preds[[i]])
  vals<-na.omit(vals)
  new_data[,i+1]<-vals
}

torm<-which(is.na(extracted$abundance))
extracted<-extracted[-c(torm),]


#NOW MODEL 
library(sdm)

species_list

species_list<-as.character(species_list)

identical(species_list, rownames(occ_mat_abun))

species_list<-sort(species_list)   #HERE#

identical(species_list, rownames(occ_mat_abun))


View(species_list)

#not working
#write.csv(extracted, 'env_abundance_all.csv')
iPAspp=1

rownames(occ_mat_abun_pres)==species_list

nrow(occ_mat_abun_pres)

iPASpp<-1

sum_table<-data.frame(edf=1,Ref.df=1, Chi.sq=1, p.value=1 , species='species', env_var='variable', model_number=1)

for (iPAspp in 1:length(species_list)) {  
  tryCatch({
    
    iSp = extracted[extracted$species == species_list[iPAspp],]  # select the spp we want
    
    if (rowSums(occ_mat_abun_pres,na.rm = TRUE)[iPAspp] > 20) {                                          #im assuming we only select the species with more than 30 records? #lets try 20
      idata =iSp 
      
      imod = gam(abundance ~ s(BO21_tempmean_ss, bs="cr") + s(BO21_temprange_bdmean, bs="cr") + 
                   s(BO2_chlorange_bdmean, bs="cr") + s(BO_parmean,bs="cr") + s(BO_bathymin, bs="cr") + s(BO_damean, bs="cr"), 
                 data = idata, family =nb() , method="REML", na.action = na.fail, select=T)
      
      
      List_m[[length(List_m)+1]] = list(imod)
      names(List_m) = sprintf("m%i", iPAspp)
      
      
      # predict into all cells
      #predict.gam can't predict using spatial object so get the env variables not spatial 
      
      p_model = predict.gam( imod, new_data, type = 'response', se = T)
      
      fits = data.frame(fits, p_model$fit); colnames(fits)[ncol(fits)] = paste0(species_list[iPAspp], "_m", iPAspp)
      fit_ses = data.frame(fit_ses, p_model$se.fit);  colnames(fit_ses)[ncol(fit_ses)] = paste0(species_list[iPAspp], "_m", iPAspp)
      
      #raster version
      raster_mod<- predict( current_preds, imod, type='response')
     # plot(raster_mod)  #checked these are predicting the same
      raster_mod[raster_mod>500]<- 500
      
     # plot(raster_mod)
      # generate plot of the model
      name = paste0("./jpegs/", species_list[iPAspp], ".jpeg")
      
    #   writeRaster(raster_mod, paste0("./rasters/", species_list[iPAspp], ".tif"), overwrite=TRUE)  #TURN THIS BACK ON
      
      #add values to the pred df dataframe
      vals<- getValues(raster_mod)
      vals<-na.omit(vals)
      
      predict_df[,iPAspp+1]<- vals
      
      colnames(predict_df)[iPAspp+1]=paste0(species_list[iPAspp])
      
      
      #add outputs to word doc
      fishname = paste0(species_list[iPAspp], ", n = ",rowSums(occ_mat_abun_pres,na.rm = TRUE)[iPAspp] , " observations") 
      sum.imod = summary(imod)
      add.title(doc, fishname) #HERE 
      table<- data.frame(sum.imod$s.table)
      # add.table(doc, table) #TURN THIS BACK ON IN A MIN
      
      #add outputs to make the figure with p vals 
      table_name<- table
      table_name$species<-fishname
      table_name$env_var<-rownames(table_name)
      table_name$model_number<-iPAspp
      
      sum_table<-rbind(sum_table, table_name)
      
      
    } else {
      # add to a reject table as the spp is too rare to model
      rejects [iPAspp,] =  c(species_list[iPAspp],rowSums(occ_mat_abun_pres,na.rm = TRUE)[iPAspp])
      predict_df[,iPAspp+1]<-NA
      colnames(predict_df)[iPAspp+1]=paste0(species_list[iPAspp])
      
    }
    print(iPAspp)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}



print(doc, target="PA_FishModels_Red_sea_endRUN_sep.docx" )
write.csv(fits, "PA_predictions_Red_sea_endRUN_sep.csv")
write.csv(fit_ses, "PA_half_errors_Red_sea_endRUN_sep.csv")
write.csv(rejects, "PA_rejects_Red_sea_endRUN_sep.csv")

ncol(occ_mat_abun_pres)

#ok now make figure of env predictor significance 

sum_table_export<-sum_table

sum_table_export<-sum_table_export[-1,]

sum_table_export<-sum_table_export[,-c(7)]

write.csv(sum_table_export, 'sum_table_abun.csv')

signif<-which(sum_table$p.value < 0.001)
non_signif<-which(! sum_table$p.value < 0.001)


sum_table$significant<- 1

sum_table$significant[signif]<- 1
sum_table$significant[non_signif]<- 0

length(unique(sum_table$species)) #=127

sum_table_prop <- sum_table %>% group_by (env_var) %>% summarise(prop_signif= (sum(significant)/127)*100 )

sum_table_prop$env_var<- c('Bathy min', 'Da mean', 'Par mean','Chlo a range', 'SST mean', 'SST range' ,'variable')
sum_table_prop<-sum_table_prop[-7,]

#sum_table_prop$env_var<-as.factor(sum_table_prop$env_var)
sum_table_prop$env_var<-factor(sum_table_prop$env_var, levels=c("Chlo a range","Da mean", "Bathy min",    "Par mean" ,  "SST mean" ,  "SST range" ))




library(RColorBrewer)


ggplot(sum_table_prop, aes(x=env_var, y=prop_signif,fill=env_var))+
  geom_bar(stat='identity')+
  #labs(y='Proportion of models containing a predictor significant at P<0.001', x='Environmental Predictors')+
  labs(y='', x='')+
  scale_fill_brewer(palette='Set2')+
  ylim(0,100)+
  theme_bw(base_size=22)+
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off")

#ok now from rasters make a matrix 

#ok so lets make planning units from the raster so that each cell has a unique ID 
PuGrid<- rasterToPolygons(current_preds[[1]], n=4, na.rm = TRUE)

# writeOGR(PuGrid, layer='PuGrid', dsn='D:/maria_kaust/SDM_kaust/PuGrid', driver='ESRI Shapefile', overwrite_layer = TRUE )

PuGrid@data<-predict_df

View(PuGrid@data)

#writeOGR(PuGrid, layer='abund_all', dsn='D:/maria_kaust/SDM_kaust/abun_all', driver='ESRI Shapefile', overwrite_layer = TRUE )

write.csv(predict_df, 'abun_predict_df_sep.csv')

#ok now cluster into bio regions 
abund_mat<-predict_df[colSums(!is.na(predict_df)) > 0]

abundance_sp<-data.frame(colnames(abund_mat))

#write.csv(abundance_sp, 'sp_modelled_abun.csv')


test<-predict_df[1,]
length(which(! is.na(test)))

library(vegan)

#rename the rownames to be PUID 
rownames(abund_mat)<-abund_mat[,1]

abund_mat<-abund_mat[,-1]

abund_log<-sqrt(abund_mat)

bray_log<-vegdist(abund_log, method='bray')

dendro_log<-hclust(bray_log, method='ward.D')

par(mfrow=c(1,1))

plot(dendro_log, labels=FALSE)

plot(as.dendrogram(dendro_log), ylim=c(0,5))

dendro_cut_abun<- cutree(dendro_log, k=23)#can set this to k =23 if need be 


dendro_cut_abun

length(which(dendro_cut_abun==20))

bioregions_abun<-PuGrid

bioregions_df_abun<-data.frame(cell_ID=predict_df[,1], bioregions=dendro_cut_abun)

length(unique ( bioregions_df_abun$bioregions)) #23

bioregions_abun@data<-bioregions_df_abun

dendro_log<-as.dendrogram(dendro_log)

#colour dendro with clusters 
library(RColorBrewer)
library(dendextend)

#test getting colour , doesnt work 
get_palette <- colorRampPalette(c('blue', 'red'))(23)

dendro_log %>%
  set("labels_col", k=23) %>%
  set("branches_k_color", k = 23) %>%
  set("leaves_pch", 19)  %>% 
  set("nodes_cex", 0.7) %>% 
  plot(axes=FALSE)


#original pals colours 
library(pals)
cols<-as.vector(alphabet(23))

#coleen's colours
abun_cols<-read.csv('abun_cols.csv')

cols<-as.vector(abun_cols$abun_cols)  


  
  par(mar=c(2,2,2,2))




#ok lets colour it in the correct order 
group<-as.factor(bioregions_abun$bioregions)

#let's add some color:
colors_to_use <- as.numeric(bioregions_df_abun$bioregions)
colors_to_use
# But sort them based on their order in dend:
colors_to_use <- colors_to_use[order.dendrogram(dendro_log)]
colors_to_use


# Now we can use them
labels_colors(dendro_log) <- colors_to_use
# Now each state has a color
labels_colors(dendro_log) 

#order cols by colors to use
cols_group<-cols[colors_to_use]

#cols_group_F<- factor(cols_group, unique(cols_group))

#plot dendrogram as you would normally do, I did this:

par(mar=c(5.1,4.1,4.1,8.1), xpd=TRUE)

dendro_log%>% set('labels_cex', 0.1) %>% set('branches_lwd',3) %>% 
  set('labels_colors', cols_group)%>%  
  color_branches(clusters=colors_to_use,  col=unique(cols_group)) %>% plot


legend("topright", inset=c(-0.2,0), legend = levels(group), fill = cols)










   # legend("topleft", legend = levels(group), fill = cols, cex = 0.5)
  





writeOGR(bioregions_abun, layer='bioregions_abun_23_sqrt', dsn='D:/maria_kaust/SDM_kaust/bioregion_sept_abun', driver='ESRI Shapefile', overwrite_layer = TRUE )

plot(bioregions_abun, col=)

library(sf)


library(pals)



sf_bioregions_abun<- st_as_sf(bioregions_abun)

sf_bioregions_abun$bioregions<-as.factor(sf_bioregions_abun$bioregions)

levels(sf_bioregions_abun$bioregions)

ggplot()+geom_sf(data=sf_bioregions_abun, aes(fill=bioregions, col=bioregions))+
  scale_fill_manual(values=cols)+
  scale_colour_manual(values=cols)+
  theme_bw()


#ok now probaility of occurance script 
occ<- subset_good


occ$count_av[ occ$count_av >=1]<-1

names(occ)   # ok this data is the same as the abundance dat


#sort out fish bio site
fish_bio<-  fish_bio_site[,c(5,6,14,15)]
fish_bio$pres<-1

subset_fish_bio <-fish_bio[fish_bio$FAMILY %in% good.fish,] 


names(subset_fish_bio)
subset_fish_bio<-subset_fish_bio[,-2]

names(occ)




names(occ)<-c("Species",  "Latitude",  "Longitude","pres"   )
names(subset_fish_bio)<-c("Species", "Latitude",  "Longitude","pres"   )

#ok now the names are the same just need to get the zero abundances from subset fish bio

#subset occ long to wide to get absences 
subset_fish_bio$Latitude<-as.numeric(subset_fish_bio$Latitude)
subset_fish_bio$Longitude<-as.numeric(subset_fish_bio$Longitude)

subset_fish_bio$latlon<- subset_fish_bio$Longitude*subset_fish_bio$Latitude

latlon_key<- subset_fish_bio[,c(2,3,5)]

dups<-which(duplicated(latlon_key)) 
latlon_key<-latlon_key[-c(dups),]

latlon_key$site_name<-c(1:nrow(latlon_key))

fish_bio_mat<- left_join(subset_fish_bio, latlon_key, by='latlon')

unique(fish_bio_mat$site_name)

fish_bio_mat<- fish_bio_mat[,c(1,4,8)]

#ok now make wide

fish_bio_mat<-acast(fish_bio_mat, Species~site_name, value.var = 'pres') #ok now all the absences have zero so cast back

#i think there are still some duplicates so change the twos to 1
fish_bio_mat[fish_bio_mat==2]<-1

#cast back
fish_bio_abs<-melt(fish_bio_mat)

names(fish_bio_abs)<-c('Species','site_name','pres')


#check latlon from occ_abs matches latlon from latlon key because they dont seem to  #issues with rounding 

fish_bio_abs<-left_join(fish_bio_abs, latlon_key, by='site_name')

which(is.na (fish_bio_abs$Latitude)) #ok remove NA

torm<-which(is.na(fish_bio_abs$Latitude)) 

fish_bio_abs<-fish_bio_abs[-c(torm),]


unique(fish_bio_abs$pres) 


#ok now join the two together 
names(occ)
names(fish_bio_abs)

fish_bio_abs<-fish_bio_abs[,c(1,4,5,3)]

occ$Species<-as.character(occ$Species)
fish_bio_abs$Species<-as.character(fish_bio_abs$Species)

occ<-rbind(occ, fish_bio_abs)


#ok 
unique(occ$pres)
torm<-which (is.na(occ$pres))
occ<-occ[-c(torm),]

#now check species

species_occ<- unique(occ$Species)


#alphabetise spp list to check for doubles 
check<-sort(species_occ)

View(check)

#ok now sort these 
which(occ$Species=='Sufflamen albicaudatus')

#subset_occ$Species[subset_occ$Species=='Cheilinus lunalatus']<-'Cheilinus lunulatus'
#subset_occ$Species[subset_occ$Species=='Anampses spp.']<-'Anampses sp.'
#subset_occ$Species[subset_occ$Species=='Caesionid spp.']<- 'Caesio sp.'
occ$Species[occ$Species=='Chromis dimidata']<- 'Chromis dimidiata'
occ$Species[occ$Species=='Rhinecanthus assai']<- 'Rhinecanthus assasi'
occ$Species[occ$Species=='Sufflamen albicaudatus']<- 'Sufflamen albicaudatum'

species_occ<- unique(occ$Species)
check<-sort(species_occ)

View(check)

#remove sp

sp_sp<- c('Anampses sp.',  'Caesionidae',  'Carangoides sp.', 'Caranx sp.',  'Cephalopholis sp.' , 'Cheilinus quinquecinctus/fasciatus', 	
          'Diodontidae',   'Epinephelus sp.',  'Kyphosus bigibbus/vaigiensis',   'Labridae', 'Lethrinus sp.' , 
          'Mulloidichthys sp.' , 'Thalassoma sp.',  'Tripneustes spp', 'Plectropomus sp.', 'Sphyraenidae' ) 


torm<- which (occ$Species %in% sp_sp)

occ<- occ[-c(torm), ]

View(unique(occ$Species))

subset_occ<-occ

#check subset_occ


dups<-duplicated(subset_occ)
length(which(dups))

#remove duplicates

#remove duplicates
subset_occ<- subset_occ[-(which(dups)),]

species_occ<-unique(subset_occ$Species)

unique(subset_occ$pres)


#subset_occ clean and ready to model!

#ok now set up a matrix for the number of observations per species
occ_mat<- subset_occ[,c(1,2,4)]
names(occ_mat)
occ_mat<- acast(occ_mat, Species~Latitude, value.var='pres', sum)


#ok now row sums should equal observations
mat_all<-occ_mat

rowSums(mat_all) #not working too many

mat_all[mat_all >1]<- 1

rowSums(mat_all)


#ok all good 
#ok now make the models 
current_preds<- env_mask_filter

plot(current_preds)

#now extract environmental data for every row of abs all
abs_all<-subset_occ

write.csv(abs_all, 'presence_data_combined.csv')

length(unique(abs_all$Species))

library(raster)

extract_abs<-function(data){
  latlon<-data.frame(lon=data$Longitude, lat=data$Latitude)
  extract1<-as.data.frame(raster::extract(current_preds, latlon))
  extract1$lat<-latlon$lat
  extract1$lon<-latlon$lon
  extract1$pres<-data$pres
  extract1$species<-data$Species
  extract1
  return(extract1)
}

extracted_abs<-extract_abs(abs_all) #ok now we have dataframe to model 

names(current_preds)
torm<- which(is.na(extracted_abs$BO_damean))#remove these lines (for now)  #these are the sites that dont have env data (eg cells do not go close enough to the coast)
extracted_abs<- extracted_abs[-c(torm),]

#
#we have binomial data so can do abundance glm/ gam etc

#ok so lets make planning units from the raster so that each cell has a unique ID 
PuGrid<- rasterToPolygons(current_preds[[1]], n=4, na.rm = TRUE)

# writeOGR(PuGrid, layer='PuGrid', dsn='D:/maria_kaust/SDM_kaust/PuGrid', driver='ESRI Shapefile', overwrite_layer = TRUE )



vals<- getValues(current_preds[[1]])
vals<-na.omit(vals)

predict_df_abs<- data.frame(cell_ID=c(0:(length(vals)-1)), vals=vals) #ok the pu IDs are in the cell_Id here and the FID in the PuGrid poly

#NOW we can model into predict df
#using MB code- build dataframes to fill with results
# build dataframes to put results
fits = data.frame(cbind(predict_df_abs$cell_ID))
colnames(fits)[1] = c("cellID")
fit_ses = data.frame(cbind(predict_df_abs$cell_ID))
colnames(fit_ses) = c("cellID")                  #figure these 4 lines out
rejects = data.frame(Species=as.character(), n = numeric(), stringsAsFactors=FALSE)
models = data.frame(Species=as.character(), Intercept = numeric(),  BO21_tempmean_ss= numeric(), BO21_temprange_bdmean = numeric(), 
                    BO2_chlorange_bdmean = numeric(), BO_parmean = numeric(), BO_bathymin = numeric(), BO_damean=numeric(),
                    R2 = numeric(), df = numeric(), logLik = numeric(), AICc = numeric(), 
                    delta = numeric(), weight = numeric(),stringsAsFactors=FALSE)

List_m = list()  # Initial model list
List_avm = list()  # Initial model averaging list

# start building the Word Document for model outputs
#dont have the reporteRS package on my R as it's too new, but using the Officer package
# create new word document
new.word.doc=function(){
  my.doc=read_docx()
  return(my.doc)
}

doc=new.word.doc()


# add a title
add.title=function(doc, my.title){
  my.prop=fp_text(font.size = 14, bold = TRUE, font.family = "Times")
  the.title=fpar(ftext(my.title, prop=my.prop))
  body_add_fpar(doc, the.title)
  body_add_par(doc, " ")
  return("title added")
}

add.title(doc, "Red Sea SDMs Pres/Abs - Final Run")

#ok now function for adding a table 
#add a data frame as a table
add.table=function(doc, tbl, col.keys=NULL, col.digits=NULL){
  # create basic flextable
  f.table=qflextable(tbl)
  # set numbers of decimals for numeric variables, if specified
  if(!is.null(col.keys)){
    for(j in 1:length(col.keys)){
      f.table=colformat_num(x=f.table,
                            col_keys=col.keys[j],
                            digits=col.digits[j])
    }
  }
  
  # set table borders
  f.table=border_outer(f.table, part="all",
                       border=fp_border(color="black", width = 1))
  f.table=border_inner_h(f.table, part="all",
                         border=fp_border(color="black", width = 1))
  f.table=border_inner_v(f.table, part="all",
                         border=fp_border(color="black", width = 1))
  
  # set fonts
  f.table=font(f.table,  fontname = "Times", part = "all")
  # also set the table's header font as bold
  f.table=bold(f.table, part = "header")
  
  # add the table to the document
  flextable::body_add_flextable(doc, 
                                value = f.table, 
                                align = "left" )
  return("table added")
}

#non- spatial values table to use predict.gam
names(current_preds)

new_data<- data.frame(cell_ID=predict_df_abs$cell_ID,BO21_tempmean_ss = 1, BO21_temprange_bdmean = 1, 
                      BO2_chlorange_bdmean = 1, BO_parmean= 1,  BO_bathymin= 1, BO_damean =1)

length(names(current_preds))

#ok env data for every PU in this table
for(i in 1:length(names(current_preds)) ){
  vals<-getValues(current_preds[[i]])
  vals<-na.omit(vals)
  new_data[,i+1]<-vals
}


#NOW MODEL 
library(sdm)

#species list
species_list<-unique(abs_all$Species)
species_list<-sort(as.character(species_list))

identical(species_list, rownames(mat_all))


#not working
#write.csv(extracted, 'env_abundance_all.csv')

iPAspp=1

which(is.na(extracted_abs$pres)) 

sum_table_pres<-data.frame(edf=1,Ref.df=1, Chi.sq=1, p.value=1 , species='species', env_var='variable', model_number=1)

for (iPAspp in 1:length(species_list)) {  
  tryCatch({
    iSp = extracted_abs[extracted_abs$species == species_list[iPAspp],]  # select the spp we want
    
    
    
    if (rowSums(mat_all,na.rm = TRUE)[iPAspp] > 20) {                                          #im assuming we only select the species with more than 30 records? #lets try 20
      idata =iSp 
      
      imod = gam(pres ~ s(BO21_tempmean_ss, bs="cr") + s(BO21_temprange_bdmean, bs="cr") + 
                   s(BO2_chlorange_bdmean, bs="cr") + s(BO_parmean,bs="cr") + s(BO_bathymin, bs="cr") + s(BO_damean, bs="cr"), 
                 data = idata, family =binomial , method="REML", na.action = na.fail, select=T)
      
      
      List_m[[length(List_m)+1]] = list(imod)
      names(List_m) = sprintf("m%i", iPAspp)
      
      
      # predict into all cells
      #predict.gam can't predict using spatial object so get the env variables not spatial 
      
      p_model = predict.gam( imod, new_data, type = 'response', se = T)
      
      fits = data.frame(fits, p_model$fit); colnames(fits)[ncol(fits)] = paste0(species_list[iPAspp], "_m", iPAspp)
      fit_ses = data.frame(fit_ses, p_model$se.fit);  colnames(fit_ses)[ncol(fit_ses)] = paste0(species_list[iPAspp], "_m", iPAspp)
      
      #raster version
      raster_mod<- predict( current_preds, imod, type='response')
     # plot(raster_mod)  #checked these are predicting the same
      
      # generate plot of the model
      name = paste0("./pres_jpegs/", species_list[iPAspp], ".jpeg")
      
      #writeRaster(raster_mod, paste0("./pres_rasters/", species_list[iPAspp], ".tif"), overwrite=TRUE) #turn back on
      
      #add values to the pred df dataframe
      vals<- getValues(raster_mod)
      vals<-na.omit(vals)
      
      predict_df_abs[,iPAspp+1]<- vals
      
      colnames(predict_df_abs)[iPAspp+1]=paste0(species_list[iPAspp])
      
      
      #add outputs to word doc
      fishname = paste0(species_list[iPAspp], ", n = ", rowSums(mat_all,na.rm = TRUE)[iPAspp], " observations") 
      sum.imod = summary(imod)
      add.title(doc, fishname) #HERE 
      table<- data.frame(sum.imod$s.table)
     # add.table(doc, table) #turn back on
      
      
      #signifcance tables
      #add outputs to make the figure with p vals 
      table_name<- table
      table_name$species<-fishname
      table_name$env_var<-rownames(table_name)
      table_name$model_number<-iPAspp
      
      sum_table_pres<-rbind(sum_table_pres, table_name)
      
    } else {
      # add to a reject table as the spp is too rare to model
      rejects [iPAspp,] =  c(species_list[iPAspp],rowSums(mat_all,na.rm = TRUE)[iPAspp])
      predict_df_abs[,iPAspp+1]<-NA
      colnames(predict_df_abs)[iPAspp+1]=paste0(species_list[iPAspp])
      
    }
    print(iPAspp)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

print(doc, target="pres_PA_FishModels_Red_sea_endRUN_sep.docx" )
write.csv(fits, "pres_PA_predictions_Red_sea_endRUN_sep.csv")
write.csv(fit_ses, "pres_PA_half_errors_Red_sea_endRUN_sep.csv")
write.csv(rejects, "pres_PA_rejects_Red_sea_endRUN_sep.csv")

#ok now make figure of env predictor significance 

sum_table_pres_export<-sum_table_pres


sum_table_pres_export<-sum_table_pres_export[-1,-7]

write.csv(sum_table_pres_export, 'sum_table_pres.csv')

sum_table_pres<-read.csv('sum_table_pres.csv')

signif<-which(sum_table_pres$p.value < 0.001)
non_signif<-which(! sum_table_pres$p.value < 0.001)


sum_table_pres$significant<- 1

sum_table_pres$significant[signif]<- 1
sum_table_pres$significant[non_signif]<- 0

length(unique(sum_table_pres$species)) #=159

sum_table_pres_prop <- sum_table_pres %>% group_by (env_var) %>% summarise(prop_signif= (sum(significant)/159)*100 )

sum_table_pres_prop


sum_table_pres_prop$env_var<- c("Bathy min", 'Da mean', 'Par mean',"Chlo a range", 'SST mean', 'SST range','variable')
sum_table_pres_prop<-sum_table_pres_prop[-7,]


sum_table_pres_prop$env_var<-factor(sum_table_pres_prop$env_var, levels=c("Chlo a range","Da mean", "Bathy min",    "Par mean" ,  "SST mean" ,  "SST range" ))
library(RColorBrewer)


ggplot(sum_table_pres_prop, aes(x=env_var, y=prop_signif,fill=env_var))+
  geom_bar(stat='identity')+
  labs(y='', x='')+
  #labs(y='Proportion of models containing a predictor significant at P<0.001', x='Environmental Predictors')+
  scale_fill_brewer(palette='Set2')+
  ylim(0,100)+
  theme_bw(base_size=22)+
  theme(panel.grid.major.y = element_blank(),
        legend.position = "off")








 #ok now cluster into bio regions 
pu_mat_abs<-predict_df_abs[colSums(!is.na(predict_df_abs)) > 0]

presAbs_sp<- data.frame(colnames(pu_mat_abs))
write.csv(presAbs_sp, 'presence_modelled_sp.csv')


   #rename the rownames to be PUID 
rownames(pu_mat_abs)<-pu_mat_abs[,1]
pu_mat_abs<-pu_mat_abs[,-1]

write.csv(pu_mat_abs, 'pres_pu_mat_probability_sep.csv')



bray<- vegdist(pu_mat_abs, method='bray')

#dendro 
dendro<-hclust(bray, method='ward.D')

par(mfrow=c(1,1))

plot(dendro, labels=FALSE)

dendro<-as.dendrogram(dendro)

dendro_cut<- cutree(dendro, k=23)
dendro_cut

#ok now add the data 
# writeOGR(PuGrid, layer='PuGrid', dsn='D:/maria_kaust/SDM_kaust/PuGrid', driver='ESRI Shapefile', overwrite_layer = TRUE )

bioregions_pres<-PuGrid

bioregions_df<- data.frame(cell_ID=predict_df[,1], bioregions=dendro_cut)

bioregions_pres@data<-bioregions_df

writeOGR(bioregions_pres, layer='bioregions_pres_23', dsn='D:/maria_kaust/SDM_kaust/bioregion_sept', driver='ESRI Shapefile', overwrite_layer = TRUE )


#write.csv(predict_df, 'abun_all/abun_all.csv')

cols2<-as.vector(alphabet2(23))


#ok coleens colours
pres_cols<-read.csv('pres_cols.csv')

library(stringr)

split_pres_cols<- data.frame(str_split_fixed(pres_cols$cols, ',', 3))
split_pres_cols$X1<-as.numeric(split_pres_cols$X1)
split_pres_cols$X2<-as.numeric(split_pres_cols$X2)
split_pres_cols$X3<-as.numeric(split_pres_cols$X3)

pres_cols_hex<- data.frame(hex=as.character('hex'))

i=1
for (i in 1:nrow(split_pres_cols)){
  red<- split_pres_cols[i,1]
  green<- split_pres_cols[i,2]
  blue<- split_pres_cols[i,3]
  pres_cols_hex[i,1]<-rgb(red,green,blue, max=255)
}



cols2<-as.vector(pres_cols_hex$hex)


par(mar=c(4,4,4,4))



group<-as.factor(bioregions_df$bioregions)
group
#let's add some color:
colors_to_use <- as.numeric(bioregions_df$bioregions)
colors_to_use
# But sort them based on their order in dend:
colors_to_use <- colors_to_use[order.dendrogram(dendro)]
colors_to_use


# Now we can use them
labels_colors(dendro) <- colors_to_use
# Now each state has a color
labels_colors(dendro) 

#order cols by colors to use
cols2_group<-cols2[colors_to_use]


#plot dendrogram as you would normally do, I did this:
par(xpd=TRUE)
par(mar=c(5.1,4.1,4.1,8.1), xpd=TRUE)

dendro %>% set('labels_cex',0.1) %>% set('branches_lwd',3) %>% 
  set('labels_colors', cols2_group)%>%
  color_branches(clusters=colors_to_use,  col=unique(cols2_group)) %>% plot
  
legend("topright", inset=c(-0.2,0), legend = levels(group), fill = cols2, title='Bioregion')



sf_bioregions_pres<- st_as_sf(bioregions_pres)

sf_bioregions_pres$bioregions<-as.factor(sf_bioregions_pres$bioregions)

levels(sf_bioregions_pres$bioregions)

ggplot()+geom_sf(data=sf_bioregions_pres, aes(fill=bioregions, col=bioregions))+
  scale_fill_manual(values=cols2)+
  scale_colour_manual(values=cols2)+
  theme_bw()




