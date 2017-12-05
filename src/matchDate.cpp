#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
IntegerVector matchDate(IntegerVector refSeries, IntegerVector tsSeries, int validDays, int lag){
int n = refSeries.size();
int m = tsSeries.size();
int counter =0 ; //pointer for refSeries position to output
int lagAdj = lag ;
IntegerVector out(m);

for(int j = 0; j < m; j++) {out[j] = NumericVector::get_na();} //initialize with NA

for(int j = 0; j < m; j++){ //j in 0:m-1 (track tsSeries/target)
    if(j<=lag){
        lagAdj = j;//take care of first few j that is smaller than lag, will treat as no lag
        }
        for(int i = counter ; i <n; i++){// i in 0:n-1 (track refSeries)
            if(tsSeries[j-lagAdj] >= refSeries[i]){ //determine which mode to use
                for(int k = i+1; k<n; k++){ //mode 1 : possible match; further loop; k (same as i) tracks refSeries
                    if(tsSeries[j-lagAdj] < refSeries[k]){
                    i = k - 1;
                    break; 
                    }
                }
                counter = i; //update the pointer
                if((tsSeries[j] - refSeries[i]) < validDays){//check if the data is too old, in terms of actually calendar days
                    out[j] = counter +1;
                }
            }else{  
                break;
            }
        }
    }
    return out;
}