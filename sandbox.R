# kaggle datasets download -d andrewmvd/leukemia-classification
# 
# https://www.kaggle.com/andrewmvd/leukemia-classification/download
# 
# download.file(url = "https://www.kaggle.com/andrewmvd/leukemia-classification/download",
#               destfile = paste(getwd(),"/files", sep = ""))
# 
# https://images.app.goo.gl/7hzP9yXz8ndmwoZE7
# 
# download.file(url = "https://en.wikipedia.org/wiki/File:SNice.svg",
#               destfile = paste(getwd(),"/smiley.svg", sep = ""))
# 
# download.file(url = "https://upload.wikimedia.org/wikipedia/commons/e/e0/SNice.svg",
#               destfile = file.path(getwd(),"smiley.svg"))

# Options ------------------------------------------------------------------------------------------------------------

options(max.print = 1000)


# Install and load libraries -----------------------------------------------------------------------------------------

if (!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
if (!require("fs")) {install.packages("fs"); library("fs")}
if (!require("bmp")) {install.packages("bmp"); library("bmp")}
if (!require("matrixStats")) {install.packages("matrixStats"); library("matrixStats")}
if (!require("caret")) {install.packages("caret"); library("caret")}


# Verify and create files folder

if (!dir_exists("files")) dir_create("files")

# Download file ------------------------------------------------------------------------------------------------------
# still not happy.....
              
# dir_ls(recurse = TRUE)
# dir_ls(path = "files")
# file_exists("files/smiley.svg")
# file_exists(path("files","smiley.svg"))

# if (!file_exists(path("files","smiley.svg"))) {
#   download.file(url = "https://upload.wikimedia.org/wikipedia/commons/e/e0/SNice.svg",
#                 destfile = file.path(getwd(),"files","smiley.svg"))
# }
# 
# if (!file_exists(path("files","smiley.svg"))) {
#   print("did not work")
#   error
# }
# 
# if (!file_exists(path(getwd(),"files","C_NMC_2019.zip"))) {
#   download.file(url = "https://1drv.ms/u/s!Ag3JsQApWJqLhMAzO3pjT0fRyw2QJw?e=zjuV06",
#                 destfile = file.path(getwd(),"files","C_NMC_2019.zip"))
# }
# 
# # OneDrive download link - not working......
# if (!file_exists(path(getwd(),"files","C_NMC_2019.zip"))) {
#   download.file(url = "https://tinyurl.com/CNMC2019",
#                 destfile = file.path(getwd(),"files","C_NMC_2019.zip"))
# }
# 
# file_delete(path(getwd(), "files", "archive.zip"))


# Make list of images ------------------------------------------------------------------------------------------------

hem_fold0 <- 
  dir_ls(path = path("files","C-NMC_Leukemia","training_data","fold_0","hem")) %>% 
  str_match(pattern = "UID_H(\\d+)_(\\d+)_(\\d+)_(hem).bmp$") %>% 
  as.data.frame() %>% 
  mutate(fold = 0)

hem_fold1 <- 
  dir_ls(path = path("files","C-NMC_Leukemia","training_data","fold_1","hem")) %>% 
  str_match(pattern = "UID_H(\\d+)_(\\d+)_(\\d+)_(hem).bmp$") %>% 
  as.data.frame() %>% 
  mutate(fold = 1)

hem_fold2 <- 
  dir_ls(path = path("files","C-NMC_Leukemia","training_data","fold_2","hem")) %>% 
  str_match(pattern = "UID_H(\\d+)_(\\d+)_(\\d+)_(hem).bmp$") %>% 
  as.data.frame() %>% 
  mutate(fold = 2)

all_fold0 <- 
  dir_ls(path = path("files","C-NMC_Leukemia","training_data","fold_0","all")) %>% 
  str_match(pattern = "UID_(\\d+)_(\\d+)_(\\d+)_(all).bmp$") %>% 
  as.data.frame() %>% 
  mutate(fold = 0)

all_fold1 <- 
  dir_ls(path = path("files","C-NMC_Leukemia","training_data","fold_1","all")) %>% 
  str_match(pattern = "UID_(\\d+)_(\\d+)_(\\d+)_(all).bmp$") %>% 
  as.data.frame() %>% 
  mutate(fold = 1)

all_fold2 <- 
  dir_ls(path = path("files","C-NMC_Leukemia","training_data","fold_2","all")) %>% 
  str_match(pattern = "UID_(\\d+)_(\\d+)_(\\d+)_(all).bmp$") %>% 
  as.data.frame() %>% 
  mutate(fold = 2)

image.index <-
  bind_rows(hem_fold0, hem_fold1, hem_fold2, all_fold0, all_fold1, all_fold2) %>% 
  select("filename" = "V1", "fold", "subjectId" = "V2", "imageNumber" = "V3",
         "cellCount" = "V4","diagnosis" = "V5")

rm(hem_fold0, hem_fold1, hem_fold2, all_fold0, all_fold1, all_fold2)

# Read random images --------------------------------------------------------------------------------------------------

random.images <-
  image.index %>% 
  sample_n(100) %>% 
  (function(df){
    sapply(1:nrow(df), simplify = FALSE, function(N){
      path("files","C-NMC_Leukemia","training_data",paste("fold_",df$fold[N],sep=""),
           df$diagnosis[N],df$filename[N]) %>% 
        bmp::read.bmp() %>% 
        matrix(nrow = 1, byrow = FALSE)
    })
  }) %>% 
  plyr::rbind.fill.matrix()

random.images.df <- data.frame(random.images)

# M <- bmp::read.bmp(path("files","C-NMC_Leukemia","training_data",paste("fold_",0,sep=""),"hem","UID_H11_10_1_hem.bmp"))
# M <- matrix(random.images, nrow = 450, byrow = FALSE)
# image(M[,])


PC <- prcomp(random.images)
summary(PC)

matrixStats::colSds(random.images) %>% quantile(seq(0,1,.01)) %>% plot()
matrixStats::colMaxs(random.images) %>% max()
random.images %>% max()

# object without unused columns
# random.images.reduced <- 
#   # convert to standard normal
#   random.images %>% 
#   magrittr::subtract(colMeans(random.images)) %>% 
#   magrittr::divide_by(matrixStats::colSds(random.images))
#   # get columns SDs
#   
#   (random.images - colMeans(random.images)) / colSds(random.images)

random.images.reduced <- random.images[,matrixStats::colSds(random.images) > 10]


# object with unused columns equal to zero (to keep dimensions and visualize what was removed)
random.images.simplified <- random.images
random.images.simplified[,matrixStats::colSds(random.images) < 20] <- 0

# plot one particular line
random.images[1,] %>% matrix(nrow = 450, byrow = FALSE) %>% image()
random.images.simplified[1,] %>% matrix(nrow = 450, byrow = FALSE) %>% image()

rm(random.images, PC, random.images.reduced, random.images.simplified, random.images.df)

# Train algorithm ------------------------------------------------------------------------------------------------------

random.images.index <- sample_n(image.index, 100)

random.images <-
  random.images.index %>% 
  (function(df){
    sapply(1:nrow(df), simplify = FALSE, function(N){
      path("files","C-NMC_Leukemia","training_data",paste("fold_",df$fold[N],sep=""),
           df$diagnosis[N],df$filename[N]) %>% 
        bmp::read.bmp() %>% 
        matrix(nrow = 1, byrow = FALSE)})}) %>% 
  plyr::rbind.fill.matrix()

random.images.simplified <- random.images[,matrixStats::colSds(random.images) > 10]

forest <- train(x = random.images.simplified,
                y = random.images.index$diagnosis,
                method = "rf")


















