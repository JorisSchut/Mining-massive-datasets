#Function calcuating whether a point is closer to a or b using the L1 norm
#(Manhattan distance). The function takes three arguments: a & b are numerical
#vectors with length 2 and x a n*2 matrix. The function compares each row to
#points a and b for each row in matrix x and returns a character string to which
#the row is closer to.
L1<- function(a, b, x){
  
  for(i in 1:nrow(x)){  
    y1<- abs(a[1]-x[i,1])+abs(a[2]-x[i,2])
    y2<- abs(b[1]-x[i,1])+abs(b[2]-x[i,2])
  
    if(y1<y2){
      print("a")
    }
  
    if(y2<y1){
      print("b")
    }
  }
}

#Function with similar characteristics as L1. However, instead of calculating the
#Manhattan distance, this function calculates the Euclidian distance between the points
L2<- function (a, b, x){
  
  for(i in 1:nrow(x)){  
    y1<- sqrt((abs(a[1]-x[i,1]))^2+(abs(a[2]-x[i,2]))^2)
    y2<- sqrt((abs(b[1]-x[i,1]))^2+(abs(b[2]-x[i,2]))^2)
    
    if(y1<y2){
      print("a")
    }
    
    if(y2<y1){
      print("b")
    }
  }
}
