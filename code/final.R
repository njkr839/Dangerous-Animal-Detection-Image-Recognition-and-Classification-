library(EBImage)
library(keras)
setwd("D:\\r scripts\\project\\final")
pics<-c('t1.jpg','t2.jpg','t3.jpg','t4.jpg','t5.jpg','t6.jpg','t7.jpg','t8.jpg','t9.jpg','t10.jpg','t11.jpg','t12.jpg','t13.jpg','t14.jpg','t15.jpg','t16.jpg','t17.jpg','t18.jpg','t19.jpg','t20.jpg','t21.jpg','t22.jpg','t23.jpg','t24.jpg','t25.jpg','t26.jpg','t27.jpg','t28.jpg','t29.jpg','t30.jpg',
        'n1.jpg','n2.jpg','n3.jpg','n4.jpg','n5.jpg','n6.jpg','n7.jpg','n8.jpg','n9.jpg','n10.jpg','n11.jpg','n12.jpg','n13.jpg','n14.jpg','n15.jpg','n16.jpg','n17.jpg','n18.jpg','n19.jpg','n20.jpg','n21.jpg','n22.jpg','n23.jpg','n24.jpg','n25.jpg','n26.jpg','n27.jpg','n28.jpg','n29.jpg','n30.jpg')
mypics<-list()

#to read the pics in mypics
for( i in 1:60)
{
  mypics[[i]]<-readImage(pics[i])  # readImage is a function under EBImage package 
}

# to resize the images 

for(i in 1:60){mypics[[i]]<-resize(mypics[[i]],50,50)}  

#to convert into one demision 

for(i in 1:60){mypics[[i]]<-array_reshape(mypics[[i]],c(50,50,3))}  #array_reshape is from keras package 

# x are the images, both tigers and non tigers 
# and y are the 0 and 1 that wether a pic is a tiger or a non tiger 

trainx<-NULL

for(i in 1:25){trainx<-rbind(trainx,mypics[[i]])}
for(j in 31:55){trainx<-rbind(trainx,mypics[[j]])}

testx<-NULL

for(i in 26:30){testx<-rbind(testx,mypics[[i]])}
for(j in 56:60){testx<-rbind(testx,mypics[[j]])}

trainy<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
testy<-c(1,1,1,1,1,0,0,0,0,0)

#one hot encoding 

trainLables<-to_categorical(trainy)
testlables<-to_categorical(testy)

#model
model<-keras_model_sequential()
model %>% layer_dense(units = 64,activation = 'relu',input_shape =7500 ) %>%
  layer_dense(units = 32,activation = 'relu')%>%
  layer_dense(units = 2,activation = 'softmax')

model %>% compile(loss = 'binary_crossentropy',optimizer = optimizer_rmsprop(),metrics = c('accuracy'))

#fitmodel 
history <-model%>% fit(trainx,trainLables,epochs = 30,batch_size = 32,validation_split = 0.2)

#evaluation 

model %>%evaluate(trainx,trainLables)
pred<-model %>% predict_classes(trainx)
pred
