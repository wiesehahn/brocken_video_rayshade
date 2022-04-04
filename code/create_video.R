
##___________________________________________________
##
## Script name: rayshade_video.R
##
## Purpose of script:
##
##
## Author: Jens Wiesehahn
## Copyright (c) Jens Wiesehahn, 2021
## Email: wiesehahn.jens@gmail.com
##
## Date Created: 2022-02-01
##
## Notes:
## this script can also be used as template, but for multiple 
## uses its more convenient to implement the snippet in RStudio
##
##___________________________________________________

## use renv for reproducability

## run `renv::init()` at project start inside the project directory to initialze 
## run `renv::snapshot()` to save the project-local library's state (in renv.lock)
## run `renv::history()` to view the git history of the lockfile
## run `renv::revert(commit = "abc123")` to revert the lockfile
## run `renv::restore()` to restore the project-local library's state (download and re-install packages) 

## In short: use renv::init() to initialize your project library, and use
## renv::snapshot() / renv::restore() to save and load the state of your library.

##___________________________________________________

## install and load required packages

## to install packages use: (better than install.packages())
# renv::install("packagename") 

renv::restore()
library(here)
library(dplyr)
library(sf)
library(rayshader)

##___________________________________________________

## load functions into memory
# source("code/functions/some_script.R")

##___________________________________________________




####################################################


# load digital terrain model
dgm <- raster::raster(here("data/external/brocken_elevation_10x10.tif"))

# input images
image_list <- c(here("data/external/brocken2017_10x10.tif"),
                here("data/external/brocken2018_10x10.tif"),
                here("data/external/brocken2019_10x10.tif"),
                here("data/external/brocken2020_10x10.tif"),
                here("data/external/brocken2021_10x10.tif"))

#### video settings ####

videoname = "forest_disturbances_brocken.mp4"
videopath = here("results/video")


# Frames per Second 
fps = 30
# Rotation degrees
rotation = 360
# Rotation time in seconds
duration = 24
# duration of transition between input images
dur_trans = 2

# number of image frames
num_frames = duration * fps
# number of input images
num_images = length(image_list)
# duration of input images in movie
dur_const = (duration - (num_images*dur_trans)) / num_images
# number of morphed images between input images
between_frames <- dur_trans * fps
# number of morphed images between input images
const_frames <- dur_const * fps


#### create one image for each frame ####

# load images
images_in <- magick::image_read(image_list)
# create transition frames between images (also last and first)
images <- c(images_in, images_in[1]) %>%
  magick::image_morph(frames = between_frames)
# remove last image (first which was duplicated)
images <- images[-length(images)]
# merge static and transition images
duptimes <- rep(c(dur_const * fps, rep(1, between_frames)), num_images)
dupindex <- rep(1:length(images), duptimes)
imageframes <- images[dupindex]

# create titles
titletexts <- rep(c("2017", "2018", "2019", "2020", "2021"), each = num_frames / num_images)

#### create terrain and shadows ####

# convert height to a matrix:
elmat = rayshader::raster_to_matrix(dgm)

# compute shadows (not needed with render_highquality)
#ambientshadows = rayshader::ambient_shade(elmat, multicore = TRUE, maxsearch = 100, zscale = 3)
#rayshadows = rayshader::ray_shade(elmat, multicore = TRUE, zscale = 8)


#### create overlay  ####

# load vector damages
disturbance <- st_read(here("data/external/brocken_landtrendr_10x10.geojson"))
# simplify
dist_simp <- disturbance %>% 
  select(label) %>%
  rmapshaper::ms_simplify(keep = 0.2, weighting = 1) %>%
  st_transform(25832) 

# label as factor
dist_simp$label <- as.factor(dist_simp$label)

# colors
my_colors <- viridis::viridis_pal(option = "magma", direction = -1, begin = 0.3, end = 0.8)(5)
pal <- c("2017" = my_colors[1], 
         "2018" = my_colors[2],
         "2019" = my_colors[3],
         "2020" = my_colors[4],
         "2021" = my_colors[5])

#scales::show_col(pal)

dist2017 <- dist_simp %>% filter(label == "2017")
dist2018 <- dist_simp %>% filter(label == "2018")
dist2019 <- dist_simp %>% filter(label == "2019")
dist2020 <- dist_simp %>% filter(label == "2020")
dist2021 <- dist_simp %>% filter(label == "2021")

# overlay
raster_extent <- raster::raster(here("data/external/brocken2017_10x10.tif"))@extent


generate_overlay <- function(polygons, color){
  overlay <- generate_polygon_overlay(polygons, 
                           extent = raster_extent, 
                           palette = color,
                           data_column_fill = "label",
                           linewidth = 0.05,
                           linecolor = color,
                           heightmap = elmat)
  return(overlay)
}


dist_overlay2017 = generate_overlay(dist2017, pal[1])
dist_overlay2018 = generate_overlay(dist2018, pal[2])
dist_overlay2019 = generate_overlay(dist2019, pal[3])
dist_overlay2020 = generate_overlay(dist2020, pal[4])
dist_overlay2021 = generate_overlay(dist2021, pal[5])



# transparency
min_alpha = 0.5
time_per_year = dur_const + dur_trans
frames_per_year = time_per_year * fps

increase_frames= round(0.3*time_per_year * fps)
decrease_frames = round(0.6 * time_per_year * fps)
shown_frames = frames_per_year - increase_frames - decrease_frames


increase_alpha = round(seq(from = 0, to = 1, length.out = increase_frames), 2)
decrease_alpha = round(seq(from = 1, to = min_alpha, length.out = decrease_frames), 2)
shown_alpha = rep(1, shown_frames)

alpha_wave = c(increase_alpha, shown_alpha, decrease_alpha)

alpha_wave_last = c(increase_alpha, shown_alpha, round(seq(from = 1, to = 0, length.out = decrease_frames), 2))
alpha_end = c(rep(min_alpha, increase_frames + shown_frames),
              round(seq(from = min_alpha, to = 0, length.out = decrease_frames), 2))

alpha_2017 <- c(alpha_wave, rep(min_alpha, frames_per_year*3),  alpha_end)
alpha_2018 <- c(rep(0, frames_per_year), alpha_wave, rep(min_alpha, frames_per_year*2),  alpha_end)
alpha_2019 <- c(rep(0, frames_per_year*2), alpha_wave, rep(min_alpha, frames_per_year),  alpha_end)
alpha_2020 <- c(rep(0, frames_per_year*3), alpha_wave, alpha_end)
alpha_2021 <- c(rep(0, frames_per_year*4),  alpha_wave_last)

alpha = data.frame(alpha_2017, alpha_2018, alpha_2019, alpha_2020, alpha_2021)


#### render frames #####

zscale = 4

save_snapshot = function(image, angle, titletext, alphavalues){
  
  # # for testing
  # i=632
  # image = imageframes[i]
  # angle = camera_angle[i]
  # titletext = titletexts[i]
  # alphavalues = alpha[i,]

  
  titlecol <- scales::alpha("black",max(alphavalues))
  
  # convert image overlay to array
  img_array = as.integer(image[[1]])/255
  
  # create 3d view
  img_array %>%
    #rayshader::add_shadow(rayshadows, max_darken = 0.2) %>%
    #rayshader::add_shadow(ambientshadows, max_darken = 0) %>%
    rayshader::plot_3d(elmat, 
                       windowsize = c(1280,720),#c(1200, 845), 
                       zscale = zscale, 
                       solid = FALSE,
                       zoom=0.6, 
                       phi=25,
                       fov=45, 
    )

  render_floating_overlay(dist_overlay2017, altitude = 2000/zscale,
                            alpha = alphavalues$alpha_2017)
  
  render_floating_overlay(dist_overlay2018, altitude = 2050/zscale,
                          alpha = alphavalues$alpha_2018)
  
  render_floating_overlay(dist_overlay2019, altitude = 2100/zscale,
                          alpha = alphavalues$alpha_2019)
  
  render_floating_overlay(dist_overlay2020, altitude = 2150/zscale,
                          alpha = alphavalues$alpha_2020)
  
  render_floating_overlay(dist_overlay2021, altitude = 2200/zscale,
                          alpha = alphavalues$alpha_2021)
  
  rayshader::render_camera(theta= angle)
  # render_depth(focus = 2200,
  #              focallength = 100,
  #              fstop = 2,
  #              preview_focus=TRUE)
  
  rayshader::render_highquality(
    focal_distance = 2200,
    aperture = 25,
    lightdirection = c((180-angle)%%360,(80-angle)%%360,(315-angle)%%360), #c(0,80,315),
    lightintensity = c(300, 400, 500),
    lightaltitude= c(80, 45, 25),
    lightcolor= c("white","#ff4d4d","#ffff80"),
    samples = 200,
    sample_method = "sobol_blue",
    min_variance = 0.000025,
    environment_light = here("data/external", "kiara_1_dawn_2k.hdr"),
    intensity_env = 0.7,
    #rotate_env = angle,
    title_text = titletext,
    title_offset = c(60,650),
    title_color =  titlecol,
    title_size = 60,
    title_font = "Palatino",
    parallel = TRUE,
    tonemap= 'gamma',
    #clamp_value=2,
    width = 1280,
    height = 720,
    filename = here::here(videopath, "frames", paste0("videoframe_", i, ".png")))
  
  rgl::rgl.close()
}


# set camera angle for each frame
camera_angle= seq(0, rotation, length.out = num_frames+1)[-1]

# save snapshot for each frame as png image
start_time <- Sys.time()

for(i in 1:num_frames) {
  save_snapshot(image = imageframes[i],
                angle = camera_angle[i],
                titletext = titletexts[i],
                alphavalues = alpha[i,])
}

end_time <- Sys.time()

# check time used for rendering
print(end_time - start_time)


#### create video ####
av::av_encode_video(sprintf(here::here(videopath, "frames", "videoframe_%d.png"),seq(1,num_frames,by=1)), 
                    framerate = fps, 
                    output = here::here(videopath, videoname))




