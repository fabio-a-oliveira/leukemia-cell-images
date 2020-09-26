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

# options(max.print = 1000)


# Install and load libraries -----------------------------------------------------------------------------------------

# make sure these packages are installed (but do not load)
if (!("magrittr" %in% installed.packages())) {install.packages("magrittr")}
if (!("plyr" %in% installed.packages())) {install.packages("plyr")}

# load these packages (installed if not previously done)
if (!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
if (!require("fs")) {install.packages("fs"); library("fs")}
if (!require("bmp")) {install.packages("bmp"); library("bmp")}
if (!require("matrixStats")) {install.packages("matrixStats"); library("matrixStats")}
if (!require("caret")) {install.packages("caret"); library("caret")}
if (!require("randomForest")) {install.packages("randomForest"); library("randomForest")}
if (!require("e1071")) {install.packages("e1071"); library("e1071")}
if (!require("Rborist")) {install.packages("Rborist"); library("Rborist")}
if (!require("Matrix")) {install.packages("Matrix"); library("Matrix")}

# Verify and create files folder

# if (!dir_exists("files")) dir_create("files")

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
  str_match(pattern = "UID_[Hh](\\d+)_(\\d+)_(\\d+)_(hem).bmp$") %>% 
  as.data.frame() %>% 
  mutate(fold = 0)

hem_fold1 <- 
  dir_ls(path = path("files","C-NMC_Leukemia","training_data","fold_1","hem")) %>% 
  str_match(pattern = "UID_[Hh](\\d+)_(\\d+)_(\\d+)_(hem).bmp$") %>% 
  as.data.frame() %>% 
  mutate(fold = 1)

hem_fold2 <- 
  dir_ls(path = path("files","C-NMC_Leukemia","training_data","fold_2","hem")) %>% 
  str_match(pattern = "UID_[Hh](\\d+)_(\\d+)_(\\d+)_(hem).bmp$") %>% 
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
         "cellCount" = "V4","diagnosis" = "V5") %>% 
  mutate(fold = as.integer(fold),
         imageNumber = as.integer(imageNumber),
         cellCount = as.integer(cellCount))

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
  plyr::rbind.fill.matrix() %>% 
  as.integer()

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

# Train RF on reduced set of images ------------------------------------------------------------------------------------

random.images.index <- 
  image.index %>% 
  filter(!is.na(filename)) %>% 
  group_by(diagnosis) %>% 
  sample_n(200) %>% 
  ungroup()

random.images <-
  random.images.index %>% 
  (function(df){
    sapply(1:nrow(df), simplify = FALSE, function(N){
      path("files","C-NMC_Leukemia","training_data",paste("fold_",df$fold[N],sep=""),
           df$diagnosis[N],df$filename[N]) %>% 
        bmp::read.bmp() %>% 
        matrix(nrow = 1, byrow = FALSE)})}) %>% 
  plyr::rbind.fill.matrix() %>% 
  as.integer()

# remove columns with near zero variance
nzv <- nearZeroVar(random.images)
random.images.simplified <- random.images[,-nzv]

# transform predictors via PCA
PC <- prcomp(random.images.simplified)
# random.images.simplified <- prcomp(random.images.simplified) %>% magrittr::extract("x")
random.images.simplified <- PC$x


forest1 <- 
  train(x = random.images.simplified,
        y = random.images.index$diagnosis,
        method = "rf",
        trControl = trainControl(method = 'cv', number = 10, verboseIter = TRUE),
        tuneGrid = data.frame(mtry = c(1,3,5,10,20,50,100,150,200)))

forest2 <- 
  train(x = random.images.simplified[,1:50],
        y = random.images.index$diagnosis,
        method = "rf",
        trControl = trainControl(method = 'cv', number = 10, verboseIter = TRUE),
        tuneGrid = data.frame(mtry = c(1,3,5,10,20,30,40,50)))

forest3 <- 
  train(x = random.images.simplified[,1:10],
        y = random.images.index$diagnosis,
        method = "rf",
        trControl = trainControl(method = 'cv', number = 10, 
                                 repeats = 10, verboseIter = TRUE),
        tuneGrid = data.frame(mtry = 1:10))

forest4 <- 
  train(x = random.images.simplified,
        y = random.images.index$diagnosis,
        method = "Rborist",
        trControl = trainControl(method = 'cv', number = 10, verboseIter = TRUE),
        tuneGrid = data.frame(predFixed = c(1,3,5,10,20,50,100,150,200),
                              minNode = rep(2,9)))


varImpPlot(forest1$finalModel)
varImpPlot(forest2$finalModel)
varImpPlot(forest3$finalModel)


# plot de varImpPlot mostram que os PC não tem importância crescente, são bem fora de ordem
# pouca vantagem em fazer repeatedcv, a performance não varia tanto com o tuning
# tanto rf quanto Rborist tem parâmetros para o número de árvores, padrão 500, mas fora do tuning grid; verificar se dá pra passar isso como arg em train()
# avaliar como são constituídos os PCs, criar imagem com o quanto cada pixel tem de peso em cada PC
# criar PCs separados para cada canal do BMP, tratar cada canal separadamente

# load("RF001.Rdata")

# Investigate Principal Component Analysis -----------------------------------------------------------------------------

# Load a few random images

random.images.index <- 
  image.index %>% 
  filter(!is.na(filename)) %>% 
  group_by(diagnosis) %>% 
  sample_n(200) %>% 
  ungroup()

random.images <-
  random.images.index %>% 
  (function(df){
    sapply(1:nrow(df), simplify = FALSE, function(N){
      path("files","C-NMC_Leukemia","training_data",paste("fold_",df$fold[N],sep=""),
           df$diagnosis[N],df$filename[N]) %>% 
        bmp::read.bmp() %>% 
        matrix(nrow = 1, byrow = FALSE)})}) %>% 
  plyr::rbind.fill.matrix()

# Get PCA

nzv <- nearZeroVar(random.images) # NZV columns
I.red <- Diagonal(1, n = ncol(random.images))[,-nzv] # sparse I matrix without columns corresponding to NZV
random.images <- as.matrix(random.images %*% I.red) # dense matrix without NZV columns

pca <- prcomp(random.images)

# Plot images

rand <- sample(1:nrow(random.images.index),1)

# reduced image
matrix(I.red %*% random.images[rand,], byrow = FALSE, nrow = 450) %>% image()

# save compressed image for each component of the rotation matrix
fig.rotation <- sapply(1:400, function(i){
 
  print(paste("i =", i)) # print status
  
  p <- matrix(I.red %*% pca$rotation[,i],
              byrow = FALSE, nrow = 450) %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "row") %>% 
    pivot_longer(col = -row, names_to = "column", values_to = "value") %>% 
    mutate(column = as.integer(str_extract(column,"\\d+")),
           row = as.integer(row)) %>% 
    ggplot(aes(x = column, y = row)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient(low = "white", high = "black") +
    coord_cartesian(xlim = c(0 + 70,450*3-70), ylim = c(0+50,450-50)) +
    scale_y_reverse() +
    theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = 'none')
  
  title <- path("temp",paste("rotation_01_",str_pad(i, width = 3, side = "left", pad = "0"),".jpg", sep=""))
  
  ggsave(plot = p, filename = title, width = 21, height = 7)
}) 

# save one image recreated step-by-step via inclusion of principal components
fig.summation <- sapply(1:400, function(i){
  
  print(paste("i =", i)) # print status
  
  p <- 
    matrix(I.red %*% t(pca$x[1,1:i, drop = FALSE] %*% t(pca$rotation[,1:i, drop = FALSE])),
           byrow = FALSE, nrow = 450) %>%
    as.data.frame() %>% 
    rownames_to_column(var = "row") %>% 
    pivot_longer(col = -row, names_to = "column", values_to = "value") %>% 
    mutate(column = as.integer(str_extract(column,"\\d+")),
           row = as.integer(row)) %>% 
    ggplot(aes(x = column, y = row)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient(low = "white", high = "black") +
    coord_cartesian(xlim = c(0 + 70,450*3-70), ylim = c(0+50,450-50)) +
    scale_y_reverse() +
    theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = 'none')
  
  title <- path("temp",paste("summation_01_",str_pad(i, width = 3, side = "left", pad = "0"),".jpg", sep=""))
  
  ggsave(plot = p, filename = title, width = 21, height = 7)
}) 


# Identify NZV columns on reduced set ----------------------------------------------------------------------------------

# housekeeping and parameters
gc()
numImagesNZV <- 200

# make list of random images
random.images.index <- 
  image.index %>% 
  filter(!is.na(filename)) %>% 
  group_by(diagnosis) %>%
  sample_n(numImagesNZV/2) %>% 
  ungroup()

# load random images
random.images <-
  random.images.index %>% 
  (function(df){
    sapply(1:nrow(df), simplify = FALSE, function(N){
      path("files","C-NMC_Leukemia","training_data",paste("fold_",df$fold[N],sep=""),
           df$diagnosis[N],df$filename[N]) %>% 
        bmp::read.bmp() %>% 
        as.integer() %>% 
        matrix(nrow = 1, byrow = FALSE)})}) %>% 
  plyr::rbind.fill.matrix()

# remove columns with near zero variance
nzvVector <- nearZeroVar(random.images) # NZV columns
nzvMatrix <- Diagonal(1, n = ncol(random.images))[,-nzvVector] # sparse I matrix without columns corresponding to NZV
# random.images <- as.matrix(random.images %*% nzvMatrix) # dense matrix without NZV columns

# remove temporary variables
rm(numImagesNZV, random.images.index, random.images)

# Run PCA on reduced set -----------------------------------------------------------------------------------------------

# housekeeping and parameters
gc()
numImagesPCA <- 1000
numPartitionsPCA <- 100

# partition list of random images
random.images.index <- 
  image.index %>% 
  group_by(diagnosis) %>% 
  sample_n(numImagesPCA / 2) %>% 
  ungroup() %>% 
  mutate(partition = rep(x = 1:numPartitionsPCA, times = numImagesPCA/numPartitionsPCA))

# load images and remove NZV columns, one partition at a time
random.images <- 
  sapply(1:numPartitionsPCA,
         simplify = FALSE, 
         function(currentPartition){
           # filter only observations in current partition
           random.images.index %>% 
             filter(partition == currentPartition) %>% 
             # anonymous function to create list with results from read.bmp converted to row matrix
             (function(df){
               sapply(1:nrow(df), simplify = FALSE, function(N){
                 path("files","C-NMC_Leukemia","training_data",paste("fold_",df$fold[N],sep=""),
                      df$diagnosis[N],df$filename[N]) %>% 
                   bmp::read.bmp() %>% 
                   as.integer() %>%
                   matrix(nrow = 1, byrow = FALSE)})}) %>% 
             # # combination of all images into single matrix
             plyr::rbind.fill.matrix() %>% 
             # # remove columns with NZV
             magrittr::extract(,-nzvVector)
         }) %>% 
  # combine results into single matrix
  plyr::rbind.fill.matrix()

# perform PCA
pca <- prcomp(random.images)

# remove temporary variables
rm(random.images.index, random.images, numImagesPCA, numPartitionsPCA)


# Apply random forest to the entire training set through PCA -----------------------------------------------------------

# housekeeping and parameters
gc()
numPartitions <- 500

# create matrix with all observations (converted to PCA)

image.index <-
  image.index %>% 
  mutate(partition = createFolds(image.index$filename, k = numPartitions, list = FALSE))

# read files from each partition
images.all.pc <-
  sapply(1:numPartitions, simplify = FALSE,
         function(currentPartition){
           
           # filter only observations in current partition
           image.index %>% 
             filter(partition == currentPartition) %>% 
             # anonymous function to create list with results from read.bmp converted to row matrix
             (function(df){
               sapply(1:nrow(df), simplify = FALSE, function(N){
                 path("files","C-NMC_Leukemia","training_data",paste("fold_",df$fold[N],sep=""),
                      df$diagnosis[N],df$filename[N]) %>% 
                   bmp::read.bmp() %>% 
                   as.integer %>% 
                   matrix(nrow = 1, byrow = FALSE)})}) %>% 
             # combination of all images into single matrix
             plyr::rbind.fill.matrix() %>% 
             # remove columns with NZV
             magrittr::extract(,-nzvVector) %>% 
             # convert predictors to principal components
             magrittr::multiply_by_matrix(pca$rotation)
           
         }) %>% 
  # combine results into single matrix
  plyr::rbind.fill.matrix()

# parameters for training
grid <- expand.grid(predFixed = c(12,24,36,48),
                    minNode = c(1,2))

# fit random forest to data
fit <- 
  train(x = images.all.pc,
        y = image.index$diagnosis,
        method = "Rborist",
        trControl = trainControl(method = 'cv', number = 5, verboseIter = TRUE),
        tuneGrid = grid)

# beep!
beepr::beep(5)

# remove temporary variables
rm(numPartitions)
# rm(images.all.pc)
image.index <- image.index %>% select(-partition)