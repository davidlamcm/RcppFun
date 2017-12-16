#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
IntegerVector matchDate(IntegerVector refSeries, IntegerVector tsSeries, int validDays, int lag, bool nextIfNotFound=false){
int n = refSeries.size();
int m = tsSeries.size();
int counter =0 ;
int lagAdj = lag ;
IntegerVector out(m);

for(int j = 0; j < m; j++) {out[j] = NumericVector::get_na();}
for(int j = 0; j < m; j++){
if(j<=lag){
lagAdj = j;
}
if(nextIfNotFound){
  for(int i = 0 ; i <n; i++){
    if(tsSeries[j-lagAdj] > refSeries[i]){
      for(int k = i; k<n-1; k++){
        if(tsSeries[j-lagAdj] <= refSeries[k+1]){
          i = k;
          break;
        }
      }
      counter = i+1;
      if((tsSeries[j] - refSeries[i]) < validDays){
        out[j] = counter + 1;
      }
    }else{
      //break;
    }
  }  
  
  
}else{
  for(int i = 0 ; i <n; i++){
    if(tsSeries[j-lagAdj] >= refSeries[i]) {
      for(int k = i+1; k<n; k++){
        if(tsSeries[j-lagAdj] < refSeries[k]){
          i = k - 1;
          break;
        }
      }
      counter = i;
      if((tsSeries[j] - refSeries[i]) < validDays){
        out[j] = counter +1;
      }
    }else{  
    //break;
    }
  }
}
}
return out;
}