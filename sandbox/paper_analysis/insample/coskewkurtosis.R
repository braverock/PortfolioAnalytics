
vech <- function (x) 
{
    t(x[!upper.tri(x)])
}

fm2 = function(u){
   return( mean(u^2))
}
fm3 = function(u){
   return( mean(u^3))
}
fm4 = function(u){
   return( mean(u^4))
}
fm6 = function(u){
   return( mean(u^6))
}

# U = matrix( rnorm(400) , ncol = 4 )
coskewCC = function( U ){
   # Implementation for N >= 3
   # The coskewness matrix is based on stacking subcomoment matrices columnwise
   # Advantage: matrix notation:
   # M3 = E[  UU' %x% U  ]: [U[1]%*%UU' | U[2]%*%UU' | ... | U[N]%*%UU' ]
   # element [j,(i-1)N+k] is  E[U[i]*U[j]*U[k]]
   # U needs to be a TxN matrix holding the centred returns of asset i in column i
   # N rows, N^3 columns 
   # See Martellini and Ziemann (Review of Financial Studies)

   N = ncol(U); T = nrow(U);
   vm2 = apply(U , 2 , 'fm2' )
   vm3 = apply(U , 2 , 'fm3' )
   vm4 = apply(U , 2 , 'fm4' )

   vr2 = c(); 
   for( j in 2:N ){
      for( i in 1:(j-1) ){
         vr2 = c( vr2 , sum( U[,i]^2*U[,j] )/sqrt( vm4[i]*vm2[j] ) )
      }
   }
   r2 = sum(vr2)*2/(  T*N*(N-1) )   

   vr5 = c(); 
   for( j in 2:N ){
      for( i in 1:(j-1) ){
         vr5 = c( vr5 , sum( U[,i]^2*U[,j]^2 )/sqrt( vm4[i]*vm4[j] ) )
      }
   }
   r5 = sum(vr5)*2/(  T*N*(N-1) ) 

   vr4 = c(); 
   for( k in 3:N ){
      for( j in 2:(k-1) ){
         for( i in 1:(j-1) ){
            #vr4 = c( vr4 , sum( U[,i]*U[,j]*U[,k] )/sqrt( vm2[k]*r5*sqrt(vm4[i]*vm4[j]) ) )
            denom = mean(  
                       c(vm2[k]*r5*sqrt( vm4[i]*vm4[j] ) , vm2[i]*r5*sqrt( vm4[j]*vm4[k] ) , vm2[j]*r5*sqrt( vm4[i]*vm4[k] )   )  )
            vr4 = c( vr4 , sum( U[,i]*U[,j]*U[,k] )/sqrt( denom ) ) 
         }
      }
   }
   r4 = sum(vr4)*6/(  T*N*(N-1)*(N-2) )   



   M3CC = matrix( rep(0,N^3) , nrow = N )
   for( i in 1:N ){
      for( j in 1:N ){
         for( k in 1:N ){
            if( (i==j) & (j==k) & (i==k) ){ 
                   sijk =  vm3[i]
            }else{
               if( (i==j) | (i==k) | (j==k) ){
                   if( (i==j) ){ sijk = r2*sqrt(   vm4[i]*vm2[k]   )}
                   if( (i==k) ){ sijk = r2*sqrt(   vm4[i]*vm2[j]   )}
                   if( (j==k) ){ sijk = r2*sqrt(   vm4[j]*vm2[i]   )}
               }else{
                   # sijk = r4*sqrt(   vm2[k]*r5*sqrt( vm4[i]*vm4[j] )   )
                   sijk_all = r4*sqrt(   
                     mean(  
                       c(vm2[k]*r5*sqrt( vm4[i]*vm4[j] ) , vm2[i]*r5*sqrt( vm4[j]*vm4[k] ) , vm2[j]*r5*sqrt( vm4[i]*vm4[k] )   )  ))
                   sijk = sijk_all 
               }
            }
            # i denotes the subcomoment matrix
            M3CC[j,(i-1)*N+k] = sijk;
         }
      }
   }
   return(M3CC)
}

# cokurtCC(U)
cokurtCC = function( U ){
   # Implementation for N >=4
   # The coskewness matrix is based on stacking subcomoment matrices columnwise
   # Advantage: matrix notation
   # M4 = E[  UU' %x% U %x% U ]: 
   #[ U[1]^2%*%UU'| U[1]U[2]%*%UU' | .... | U[1]U[N]%*%UU'| 
   #   U[2]U[1]%*%UU'| U[2]^2%*%UU' | .... | U[2]U[N]%*%UU'|
   #   ...
   #   U[N]U[1]%*%UU'| U[N]U[2]%*%UU' | .... | U[N]^2%*%UU ]
   # element [k,( (i-1)*(N^2)+(j-1)*N+l )] is  E[ U[i]*U[j]*U[k]*U[l] ]
   # U needs to be a TxN matrix holding the centred returns of asset i in column i
   # N rows, N^4 columns 
   # See Martellini and Ziemann (Review of Financial Studies)

   N = ncol(U); T = nrow(U);
   vm2 = apply(U , 2 , 'fm2' )
   vm3 = apply(U , 2 , 'fm3' )
   vm4 = apply(U , 2 , 'fm4' )
   vm6 = apply(U , 2 , 'fm6' )

   vr3 = c(); 
   for( j in 2:N ){
      for( i in 1:(j-1) ){
         vr3 = c( vr3 , sum( U[,i]^3*U[,j] )/sqrt( vm6[i]*vm2[j] ) )
      }
   }
   r3 = sum(vr3)*2/(  T*N*(N-1) )   


   vr5 = c(); 
   for( j in 2:N ){
      for( i in 1:(j-1) ){
         vr5 = c( vr5 , sum( U[,i]^2*U[,j]^2 )/sqrt( vm4[i]*vm4[j] ) )
      }
   } 
   r5 = sum(vr5)*2/(  T*N*(N-1) ) 

   vr6 = c(); 
   for( k in 3:N ){
      for( j in 2:(k-1) ){
         for( i in 1:(j-1) ){
            vr6 = c( vr6 , sum( U[,i]^2*U[,j]*U[,k] )/sqrt( vm4[i]*r5*sqrt(vm4[j]*vm4[k]) ) )
         }
      }
   }
   r6 = sum(vr6)*6/(  T*N*(N-1)*(N-2) )   

   vr7 = c(); 
   for( l in 4:N ){
      for( k in 3:(l-1) ){
         for( j in 2:(k-1) ){
            for( i in 1:(j-1) ){
               vr7 = c( vr7 , sum( U[,i]*U[,j]*U[,k]*U[,l] )/sqrt( r5*sqrt(vm4[i]*vm4[j])*r5*sqrt(vm4[k]*vm4[l]) ) )
            }
         }
      }
   }
   r7 = sum(vr7)*24/(  T*N*(N-1)*(N-2)*(N-3) )  

   M4CC = matrix( rep(0,N^4) , nrow = N )
   for( i in 1:N ){
      for( j in 1:N ){
         for( k in 1:N ){
            for( l in 1:N ){
              # in the general case: i, j , k and l are all different
              kijkl = r7*sqrt( r5*sqrt(vm4[i]*vm4[j])*r5*sqrt(vm4[k]*vm4[l]) )
              # if at least one of them are equal: 

              if( (i==j) | (i==k) | (i==l) | (j==k) | (j==l) | (k==l)  ){

                 if( (i==j) & (i==k) & (i==l) ){ 
                   # These are the kurtosis estimates of the individual assets: E[u^4]
                   kijkl =  vm4[i]; 
                 }else{
                      if( ((i==j) & (i==k)) | ((i==j) & (i==l))  | ((i==k) & (i==l)) | ((j==k) & (j==l)) ){
                        # kiij E[ U[,i]^3*U[,j] ] = r3*sqrt( vm6[i]*vm2[j] )
                        if( ((i==j) & (i==k))   ){  kijkl = r3*sqrt(   vm6[i]*vm2[l]   ) }
                        if( ((i==j) & (i==l))   ){  kijkl = r3*sqrt(   vm6[i]*vm2[k]   ) }
                        if( ((i==k) & (i==l))   ){  kijkl = r3*sqrt(   vm6[i]*vm2[j]   ) }
                        if( ((j==k) & (j==l))   ){  kijkl = r3*sqrt(   vm6[j]*vm2[i]   ) }
                      }else{
                          if( ((i==j) & (k==l))| ( (i==k) & (j==l) ) | ( (i==l) & (j==k)) ){ 
                              # kiijj = E[ U[,i]^2*U[,j]^2 ] = r5*sqrt( vm4[i]*vm4[j]  )
                               if ( ((i==j) & (k==l)) ){ kijkl = r5*sqrt( vm4[i]*vm4[k] ) } 
                               if ( ((i==k) & (j==l)) ){ kijkl = r5*sqrt( vm4[i]*vm4[j] ) } 
                               if ( ((i==l) & (j==k)) ){ kijkl = r5*sqrt( vm4[i]*vm4[j] ) } 
                           }else{
                              # kiijk = E[ U[,i]^2*U[,j]*U[,k] ] = r6*sqrt( vm4[i]*r5*sqrt( vm4[j]*vm4[k] ) )
                               if ( (i==j) ){ kijkl = r6*sqrt( vm4[i]*r5*sqrt(vm4[k]*vm4[l]) ) } 
                               if ( (i==k) ){ kijkl = r6*sqrt( vm4[i]*r5*sqrt(vm4[j]*vm4[l]) ) } 
                               if ( (i==l) ){ kijkl = r6*sqrt( vm4[i]*r5*sqrt(vm4[j]*vm4[k]) ) } 
                               if ( (j==k) ){ kijkl = r6*sqrt( vm4[j]*r5*sqrt(vm4[i]*vm4[l]) ) }
                               if ( (j==l) ){ kijkl = r6*sqrt( vm4[j]*r5*sqrt(vm4[i]*vm4[k]) ) } 
                               if ( (k==l) ){ kijkl = r6*sqrt( vm4[k]*r5*sqrt(vm4[i]*vm4[j]) ) } 
                           }
                    }
                }
              }
              M4CC[k,( (i-1)*(N^2)+(j-1)*N+l )] = kijkl;
         }#loop l
      }#loop k
    }#loop j
   }#loop i
   return(M4CC)
}

