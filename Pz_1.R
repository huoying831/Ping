########1. Basic Formulas #########
######Define p(x1,x2,t1,t2,theta) with integration#########
######p1 is pdf, Inta is result of integration######
p1<-function(x1,t1,t2,theta){
        p0<-function(x2){
        1/sqrt(2*pi*(t2-t1))*exp(-(x2-(x1+theta*(x2-x1)))^2/(2*(t2-t1)))
        }
     return(p0)
     }
Int1<-function(x1,t1,t2,a,b,theta){
         integrate(p1(x1,t1,t2,theta),lower=a,upper=b)$value
}

##########set parameters#########

set.seed(1)
n=10
a<-vector(mode='numeric',length=n)
a<-sample(6:10,10,replace = TRUE)
b<-vector(mode='numeric',length=n)
b<-sample(1:5,10,replace =TRUE)
t1<-1:10
t2<-2:11
x1<-1:10

#####1.1, theta = 0#########

######1.1.1 conditinal on x0 t0##########

theta=0


###########Each_x0 is result of each Intergration, Mul_x0 is result of Multiplication#####
Mul_x0=1
Each_x0<-vector()
for (i in 1:n){
     Each_x0[i]<-Int1(x1[i],t1[i],t2[i],a[i],b[i],theta)
     Mul_x0<-Int1(x1[i],t1[i],t2[i],a[i],b[i],theta)*Mul_x0
}  

######1.1.2 with x0 = 0 t0 = 0 #######

theta=0
x1[1]=0
t1[1]=0

###########Each_0 is result of each Intergration, Mul_0 is result of Multiplication#####
Mul_0=1
Each_0<-vector()
for (i in 1:n){
     Each_0[i]<-Int1(x1[i],t1[i],t2[i],a[i],b[i],theta)
     Mul_0<-Int1(x1[i],t1[i],t2[i],a[i],b[i],theta)*Mul_0
}  

#####1.2, theta != 0#########

######1.2.1 conditinal on x0 t0##########

theta=0.5


###########Each_x0_theta is result of each Intergration, Mul_x0_theta is result of Multiplication#####
Mul_x0_theta=1
Each_x0_theta<-vector()
for (i in 1:n){
     Each_x0_theta[i]<-Int1(x1[i],t1[i],t2[i],a[i],b[i],theta)
     Mul_x0_theta<-Int1(x1[i],t1[i],t2[i],a[i],b[i],theta)*Mul_x0_theta
}  

######1.1.2 with x0 = 0 t0 = 0 #######

theta=0.5

x1[1]=0
t1[1]=0

###########Each_0_theta is result of each Intergration, Mul_0_tehta is result of Multiplication#####
Mul_0_theta=1
Each_0_theta<-vector()
for (i in 1:n){
     Each_0_theta[i]<-Int1(x1[i],t1[i],t2[i],a[i],b[i],theta)
     Mul_0_theta<-Int1(x1[i],t1[i],t2[i],a[i],b[i],theta)*Mul_0_theta
}  




#####2 Exit probability####
####2.1 Exit at a single point####
Ex<-seq(0,1,0.1)
p_a1<-Int1(x1[1],t1[1],t2[1],Ex[1],Inf,theta)

####p_point single point  ####
p_point<-function(n){
    if (n==1){ 
       p_ai=Int1(x1[n],t1[n],t2[n],Ex[n],Inf,theta)
       }
    else if (n>1){
        p_ai=1
        for ( i in 1:(n-1)){
           p_ai <-Int1(x1[i],t1[i],t2[i],-Inf,Ex[i],theta)*p_ai
           }
        p_ai<-p_ai*Int1(x1[n],t1[n],t2[n],Ex[n],Inf,theta)
        }
return(p_ai)
}
p_point(n)
########2.2 Exit at any exit boundary######

####p_bound boundary####
p_b=0
p_bound<-function(k){
      for (i in 1:k){
         p_b=p_b+p_point(i)
         }   
      return(p_b)        
}
p_bound(2)

###########2.3 Critical Boundary under theta = 0 #############
theta = 0 
c<-1:10
p_crt0<- function(n){
       if (n==1){ 
           p_ai=Int1(x1[n],t1[n],t2[n],c[n]*sqrt(t2[n]),Inf,theta)
       }
       else if (n>1){
           p_ai=1
           for ( i in 1:(n-1)){
               p_ai <-Int1(x1[i],t1[i],t2[i],-Inf,c[i]*sqrt(t2[i]),theta)*p_ai
               }
           p_ai<-p_ai*Int1(x1[n],t1[n],t2[n],c[n]*sqrt(t2[n]),Inf,theta)
        }
        return(p_ai)
}



###transformation###########
s1 = t1/t2[n]
s2 = t2/t2[n]
x1new=x1/sqrt(t2[n])


p_crt1<-function(n){
       if (n==1){ 
           p_ai=Int1(x1new[n],s1[n],s2[n],c[n]*sqrt(s2[n]),Inf,theta)
       }
       else if (n>1){
           p_ai=1
           for ( i in 1:(n-1)){
               p_ai <-Int1(x1new[i],s1[i],s2[i],-Inf,c[i]*sqrt(s2[i]),theta)*p_ai
               }
           p_ai<-p_ai*Int1(x1new[n],s1[n],s2[n],c[n]*sqrt(s2[n]),Inf,theta)
        }
        return(p_ai)
}




