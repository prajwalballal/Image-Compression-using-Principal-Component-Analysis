# FUNCTION FOR CREATING AND ORGANISING COMPRESSED IMAGES. 

# imagee = you just need to give the image name in double quotes in this variable 
#          and the image should in jpeg format.
# directory = you need to give a complete directory to the folder in which the image is
#             stored in double quotes. If not given a default directory is given.
# folder_name = you can give a name of choice to the folder which will be created 
#               by this function or it will just take the name of the image and create
#               one for you using the image's name.
# grey_scale_img = this creates the red blue and green images of the original image.
#                  takes input in "Y" and "N" form.
# delete = this variable deletes the original image from the folder if needed.
#          takes input in "Y" and "N" form.
# clarity = is amount of variance in % you want in your compressed image and has a default
#        value set as 99.9%. The exact value given in this variable should be present 
#        in the % variances explained by the pca image or the fucntion will give errors.


pca_org_compressed <- function(imagee, directory="C:/", 
                folder_name=paste(imagee,"_compressed_imgs", sep=""), grey_scale_img='N', 
                delete='N', clarity=99.9)
{
  
  library(jpeg)
  
  
  setwd(directory)
  
  img = readJPEG(paste(imagee, ".jpg", sep = ""))
  
  final = 0
  if(delete=="Y")
  { 
    final<- readline("Are you sure you want to delete the original image? (yes/no):")
  }
  
  
  r <- img[,,1]
  g <- img[,,2]
  b <- img[,,3]
  
  # whenever we export these images we will get grey scale images
  if(grey_scale_img=="Y")
  {
    writeJPEG(r,paste(imagee, "_red.jpg", sep = ""))
    writeJPEG(g,paste(imagee, "_green.jpg", sep = ""))
    writeJPEG(b,paste(imagee, "_blue.jpg", sep = ""))
  }
  
  
  
  # here center = False means we dont want to do means centering 
  image.r.pca <- prcomp(r, center = F)
  image.g.pca <- prcomp(g, center = F)
  image.b.pca <- prcomp(b, center = F)
  
  # checking cummulitive sum of the variances exlained by columns of each image type
  
  rr <- round(cumsum(image.r.pca$sdev^2)/sum(image.r.pca$sdev^2)*100, 1)
  gg<- round(cumsum(image.g.pca$sdev^2)/sum(image.g.pca$sdev^2)*100,1)
  bb= round(cumsum(image.b.pca$sdev^2)/sum(image.b.pca$sdev^2)*100, 1)
  
  
  p=c()
  q=c()
  z=c()
  for(i in 1:length(rr))
  {
    if(rr[i]<=clarity)
    {
      p=c(p,i)
    }
  }
  for(i in 1:length(gg))
  {
    if(gg[i]<=clarity)
    {
      q=c(q,i)
    }
  }
  for(i in 1:length(bb))
  {
    if(bb[i]<=clarity)
    {
      z=c(z,i)
    }
  }
  
  ncomp_r = p[length(p)]
  ncomp_g = q[length(q)]
  ncomp_b = z[length(z)]
  
  if(is.null(ncomp_r) | is.null(ncomp_g) | is.null(ncomp_b))
  {
    stop("ERROR: THE CLARITY VALUE YOU HAVE ENTERED DOES NOT EXIST PLEASE TRY A BIGGER VARIANCE VALUE")
  }
  
  R= image.r.pca$x[,1:ncomp_r]%*%t(image.r.pca$rotation[,1:ncomp_r])
  G= image.g.pca$x[,1:ncomp_g]%*%t(image.g.pca$rotation[,1:ncomp_g])
  B= image.b.pca$x[,1:ncomp_b]%*%t(image.b.pca$rotation[,1:ncomp_b])
  
  imge = array(c(R,G,B), dim = c(dim(img)[1:2],3))
  
  # corrections in the pca matrices
  
  R = ifelse(R > 1, 1, R)
  R = ifelse(R<0, 0, R)
  
  G = ifelse(G > 1, 1, G)
  G = ifelse(G<0, 0, G)
  
  B = ifelse(B > 1, 1, B)
  B = ifelse(B<0, 0, B)
  
  imge = array(c(R,G,B), dim = c(dim(img)[1:2],3))
  
  dir.create(paste(folder_name))
  setwd(paste(directory, "/", folder_name, sep = ""))
  writeJPEG(imge, paste(imagee, "_compressed.jpg", sep = ""))
  
  
  # to delete the original image
  if (final=="yes")
  {
    unlink(paste(directory, "/", imagee, ".jpg", sep = ""), recursive = TRUE)
  }
  
}



pca_org_compressed(imagee = "dog",directory = 'C:/Users/balla/OneDrive/Pictures/comp_images/' ,
                   folder_name = 'doggy',grey_scale_img='Y', delete='N', clarity =99.5)



