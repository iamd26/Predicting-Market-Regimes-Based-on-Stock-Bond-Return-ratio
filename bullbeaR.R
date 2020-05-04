#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Documentation ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Version 1.0 (9th August 2018)

#Find the turning points of a time series of prices

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Model Parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#units: units in which time based parameters are input (e.g. "days")
#initwindow: size of initial window to search for local max/min to seed LT model

#minbull: minimum price rise to qualify as bull phase
#minbear: minimum price fall to qualify as bear phase

#window: time interval used to determine local maxima/minima in PS model
#endbuffer: time interval used to filter points at start/end of series 

#mincycle: mimimum length of a cycle (peak to peak, trough to trough)
#minphase: mimimum length of a phase (peak to trough, trough to peak)
#minphaseexception: magnitude change required to override minimum phase threshold

#plotinterval: x-label spacing (e.g. "10 years")
#plotdateformat: x-label date format (e.g. "%Y")
#plotstartdate: plot start date (e.g. as.Date('1930-01-01') )
#plotenddate: plot end date (e.g. as.Date('2015-01-01') )

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Model PS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Pagan, A.R. and Sossounov, K.A., 2003. 
#A simple framework for analysing bull and bear markets. 
#Journal of Applied Econometrics, 18(1), pp.23-46.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Model LT
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Lunde, A. and Timmermann, A., 2004. 
#Duration dependence in stock prices: An analysis of bull and bear markets. 
#Journal of Business & Economic Statistics, 22(3), pp.253-273.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Model TD
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Hanna, A.J., 2018. 
#A top-down approach to identifying bull and bear market states. 
#International Review of Financial Analysis, 55, pp.93-110.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Libraries ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

require(lubridate, quietly = T)
require(ggplot2, quietly = T)
require(stringr, quietly = T)
require(scales, quietly=T)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Parent class ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setRefClass("bullbear", 
  fields = list(name = "character", ts = "data.frame", tp = "vector",
                minbear = "numeric", minbull = "numeric", initwindow = "numeric",
                window = "numeric", endbuffer = "numeric", mincycle = "numeric",
                minphase = "numeric", minphaseexception = "numeric",
                units = "character",
                plotinterval = "character", plotdateformat = "character", 
                plotstartdate = "Date", plotenddate = "Date",
                ylabels = "vector"
                ),
  methods = list(
    datediff = function(x1, x2) {
      
      if (units=="months"){
        return (interval(x1, x2) / months(1))
      } else if (units=="weeks"){
        return (interval(x1, x2) / weeks(1))
      } else {
        return (as.integer(x2-x1))
      }
      
    },
    addpoint = function(i, peak, reset=F) {
      
      if (reset){
        ts$peak   <<- 0
        ts$trough <<- 0
      }
      
      if (i %in% which(ts$peak | ts$trough)){
        return()
      }
      
      if (peak){
        ts$peak[i] <<- 1
      } else {
        ts$trough[i] <<- 1
      }
      
      tp <<- which(ts$peak==1 | ts$trough==1)
      
    },
    removepoint = function(i) {
      ts$peak[i]   <<- 0
      ts$trough[i] <<- 0
      
      tp <<- which(ts$peak==1 | ts$trough==1)
    },
    initialize = function(name, ts){
      name <<- name
      ts   <<- ts
      
      ts$peak   <<- 0
      ts$trough <<- 0
      ts$ylabels <<- c()
      
      tp <<- vector('integer')
    },
    turningpoints = function() {
      #To be implemented at child level
    },
    plot = function(file=F) {
      
      df = ts[tp,]
      df$group = as.factor(df$peak)
      levels(df$group) <- c("0","1")
      
      lcolors = c("#AAAAAA",'#FF0000')
      
      if (nrow(df)==1 & sum(df$peak)>0){
        lcolors = c('#FF0000')  
      }
      
      date_breaks = seq(from=plotstartdate, to=plotenddate, by=plotinterval)
      scale <- scale_x_date(labels=date_format(plotdateformat), breaks = date_breaks)
      
      g <- ggplot() 
      
      if (length(ylabels)>0){
        g <- g + scale_y_continuous(trans="log", breaks=ylabels)
      } else {
        g <- g + scale_y_continuous(trans="log")
      }
      
      g <- g + geom_line(data = ts, aes(x=date, y=value), size=0.5)
      g <- g + geom_point(data = df, aes(x=date, y=value, group = group, color = group), size=3)
      g <- g + theme_bw()
      g <- g + xlab("") 
      g <- g + ylab("")
      g <- g + theme(axis.text.x  = element_text(size=20))
      g <- g + theme(axis.text.y  = element_text(size=20))
      g <- g +  theme(legend.position="none")
      g <- g + scale_color_manual("", values=lcolors)
      g <- g + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
      g <- g + scale
      
      print(g)
      print(df)
      write.csv(df,"df.csv")

      
      if (file){
        png(file=paste(name,".png",sep=""), width = 30, height = 20, units = "cm", res=300)
        print(g)
        dev.off()
      }
      
    }
    
  )          
)

setRefClass("LT", 
  fields = list(),
  contains="bullbear",
  methods = list(
    turningpoints = function() {
      
      #Find first turning point, a local max/min
      for (i in 1:nrow(ts)){
        
        temp = which( abs( datediff(ts$date[i], ts$date)  ) < initwindow )            
        
        if ( sum( ts$value[i] >= ts$value[temp]) == length(temp) ) {
          localmax = TRUE
          break
        } else if ( sum( ts$value[i] <= ts$value[temp]) == length(temp) ) {
          localmax = FALSE
          break
        }
      }
      
      .self$addpoint(i,localmax)
      
      #Now seek alternate peaks and troughs
      k=i;   #Highest/lowest point this phase so far
      j=i;   #First point added
      i=i+1;
      
      while (i<=nrow(ts)) {
        
        if (localmax){
          
          if (ts$value[i]>=ts$value[k]){
            k=i;
          }
          else if ( ts$value[i] < (ts$value[k] * (1-minbear)) ){
            
            if ( length(tp)==1 && (tp[1] == j) && (ts$value[k]>ts$value[j] ) ){
              .self$addpoint(k,localmax, TRUE)
            } else {
              .self$addpoint(k,localmax)
            }
            localmax = FALSE;
            k=i;
          }
          
        } else {
          
          if (ts$value[i]<=ts$value[k]){
            k=i;
          }
          else if ( ts$value[i] > (ts$value[k] * (1+minbull)) ){
            
            if ( length(tp)==1 && (tp[1] == j) && (ts$value[k]<ts$value[j] ) ){
              .self$addpoint(k,localmax, TRUE)
            } else {
              .self$addpoint(k,localmax)
            }
            localmax = TRUE;
            k=i;
          }
          
        }
          
        i=i+1
        
      }
      
      if (localmax){
      
        if ( ts$value[k] > (1+minbull)*ts$value[tp[length(tp)]] ){
          .self$addpoint(k,localmax)
          print(k)
        }
      
      } else {
        
        if ( ts$value[k] < (1-minbear)*ts$value[tp[length(tp)]] ){
          .self$addpoint(k,localmax)
          print(k)
        }
      
      }
    }
  )          
)

setRefClass("PS", 
  fields = list(),
  contains="bullbear",
  methods = list(
    alternate = function() {
      
      keys = which(ts$peak==1 | ts$trough==1)
      
      i=1
      
      while (i<length(keys)) {
        
        k1 = keys[i]
        k2 = keys[i+1]
        
        if ( (ts$peak[k1]==1 && ts$peak[k2]==1) || (ts$trough[k1]==1 && ts$trough[k2]==1) ){
          
          #successive peaks or successive troughs - eliminate one    
          sgn = 2*ts$peak[k1] - 1; #peaks=1, troughs = -1
          
          if ( sgn*(df$value[k2] - df$value[k1]) > 0 ) {
            .self$removepoint(k1)
          } else {
            .self$removepoint(k2)
          }
          
          keys = which(ts$peak==1 | ts$trough==1)
          
        } else {
          i=i+1
        }
          
      }
      
    },
    trimends = function() {
      
      # 2(a) Elimination of turns within X months of beginning and end of series.
      tp <<- which(ts$peak==1 | ts$trough==1)
      
      for (i in tp){
        if (abs(datediff(ts$date[i], ts$date[1])) < endbuffer){
          .self$removepoint(i)
        } else if (abs(datediff(ts$date[i], ts$date[nrow(ts)])) < endbuffer){
          .self$removepoint(i)
        }
      }
      
      #2(b) Elimination of peaks (or troughs) at both ends of series which are lower or higher).
      tp <<- which(ts$peak==1 | ts$trough==1)
      
      #First point
      k = tp[1];
      sgn = 2*ts$peak[k]-1; #peaks=1, troughs = -1
      
      if (sum((sgn*(df$value[k] - df$value[1:k]))<0) > 1) {
        .self$removepoint(k)
      }
      
      #Last point
      k = tp[length(tp)];
      sgn = 2*ts$peak[k]-1; #peaks=1, troughs = -1
      
      if (sum((sgn*(df$value[k] - df$value[k:(nrow(ts))]))<0) > 1) {
        .self$removepoint(k)
      }
      
    },
    turningpoints = function() {
      
      #Find first turning point candidates: local max/min
      ts$peak   <<- 0
      ts$trough <<- 0
      
      for (i in 1:nrow(ts)){
        
        k = which( abs(datediff(ts$date[i], ts$date)) < window )            
        
        if (sum(ts$value[i] >= ts$value[k]) == length(k)){
          #Local max
          .self$addpoint(i,TRUE)
          print(i)
        } else if (sum(ts$value[i] <= ts$value[k]) == length(k)){
          #Local min
          .self$addpoint(i,FALSE)
          print(i)
        }
        
      }
      
      tp <<- which(ts$peak==1 | ts$trough==1)
      
      .self$alternate()
      tp <<- which(ts$peak==1 | ts$trough==1)
      
      #2. Trim Ends
      .self$trimends()
      tp <<- which(ts$peak==1 | ts$trough==1)
      
      #3. Impose minimum cycles
      finished = FALSE
      
      while (!finished){
        
        tp <<- which(ts$peak==1 | ts$trough==1)
        
        temp = ts[tp,]
        temp$interval = Inf
        
        for (i in 1:(length(tp)-2)){
          temp$interval[i] = .self$datediff(temp$date[i], temp$date[i+2])
        }
        
        candidates = which((temp$interval<mincycle));
        
        if (length(candidates)==0){
          finished = TRUE
        } else {
          
          p = which(min(temp$interval)==temp$interval);
          p = p[1] #in case of ties
          
          k1 =  tp[p]
          k2 =  tp[p+2]
          
          sgn = 2*ts$peak[k1]-1 #peaks=1, troughs = -1      
          
          if ( sgn*(ts$value[k2] - ts$value[k1]) > 0 ){
            #Rising peak or falling trough
            .self$removepoint(k1)
          } else {
            .self$removepoint(k2)
          }
          
          .self$alternate()
          
        }
        
      }
      
      tp <<- which(ts$peak==1 | ts$trough==1)
      
      #4. Min Phase
      finished = FALSE
      
      while (!finished){
       
        tp <<- which(ts$peak==1 | ts$trough==1)
        
        temp = ts[tp,]
        temp$interval = Inf
        temp$ret = Inf
        
        
        for (i in 1:(length(tp)-1)){
          temp$interval[i] = .self$datediff(temp$date[i], temp$date[i+1])
          temp$return[i]   = temp$value[i+1]/temp$value[i]-1
        }
        
        #Find candidates for removal
        candidates = which( (abs(temp$return) < minphaseexception) & (temp$interval<minphase));
        
        
        if (length(candidates)==0){
          finished = TRUE
        } else {
          
          #remove next point
          p = which(min(temp$interval)==temp$interval);
          p = p[1] #in case of ties
          
          .self$removepoint(tp[p])
          .self$alternate()

        }
         
      }
      
      tp <<- which(ts$peak==1 | ts$trough==1)
      .self$trimends()
      
      tp <<- which(ts$peak==1 | ts$trough==1)
      
    }
  )          
)

setRefClass("TD", 
            fields = list(),
            contains="bullbear",
            methods = list(
              phase1 = function() {
                
                pts = c(1,nrow(ts))
                n = length(pts)
                i=1
                
                while (i<length(pts)){
                
                  j = pts[i];
                  k = pts[i]:pts[i+1]
                  
                  a = which(min(ts$value[k])==ts$value[k])
                  b = which(max(ts$value[k])==ts$value[k])
                  
                  a = a[length(a)]
                  b = b[length(b)]
                  
                  a = j+a-1
                  b = j+b-1
                  
                  #Determine if the change in price is sufficiently large
                  if (a<b) {
                    keep = (ts$value[b]/ts$value[a]) > (1 + minbull)
                  } else {
                    keep = (ts$value[a]/ts$value[b]) < (1 - minbear)
                  }
                  
                  if ( (abs(.self$datediff(ts$date[a],ts$date[b]))< minphase) & ( abs(ts$value[b]/ts$value[a]-1) < minphaseexception) ) {
                    keep = FALSE
                  }
                  
                  #Additional Pagan/Sossounov style checks (across sub-interval boundaries)
                  checkpts = sort(unique(c(pts,a,b)))
                  
                  #Check phases
                  for (j in 1:(length(checkpts)-1)){
                    if ( .self$datediff(ts$date[checkpts[j]],ts$date[checkpts[j+1]]) < minphase ) {
                      if (abs( (ts$value[checkpts[j+1]] / ts$value[checkpts[j]] ) - 1 ) < minphaseexception){
                        if ( (checkpts[j]!=1) && (checkpts[j+1]!=length(ts)) ){
                          keep = FALSE
                        }
                      }
                    }          
                  }
                  
                  #Check cycles
                  for (j in 1:(length(checkpts)-2)){
                    if ( .self$datediff(ts$date[checkpts[j]],ts$date[checkpts[j+2]]) < mincycle ){
                      if ( (checkpts[j]!=1) && (checkpts[j+2]!=length(ts)) ){
                        keep = FALSE
                      }
                    }
                  }
                  
                  if (keep){
                    .self$addpoint(a,FALSE)
                    .self$addpoint(b,TRUE)
                    pts = sort(unique(c(pts,a,b)))
                    
                  } else {
                    i=i+1
                  }
                  
                  if (length(pts)==n){
                    #No new points have been added, so advance to next interval        
                    i = i+1
                  }
                  
                  n = length(pts);
                  
                }
                
                #Remove end pts (unless they are max/mins)
                
                if (n>1){
                  if ( .self$datediff(ts$date[pts[n-1]], ts$date[pts[n]]) < minphase) {
                    .self$removepoint(pts[n])
                    pts = pts[1:(n-1)]
                    
                  } else if ( (ts$value[pts[n]] == max(ts$value[pts[n-1]:pts[n]])) & ( (ts$value[pts[n]]/ts$value[pts[n-1]]) > (1+minbull) ) ) {
                    #keep
                  } else if ( (ts$value[pts[n]] == min(ts$value[pts[n-1]:pts[n]])) & ( (ts$value[pts[n]]/ts$value[pts[n-1]]) < (1-minbear) ) ) {
                    #keep
                  } else {
                    .self$removepoint(pts[n])
                    pts = pts[1:(n-1)]
                  }
                  
                }
                
                if (length(pts)>1){
                  if ( .self$datediff(ts$date[pts[1]], ts$date[pts[2]]) < minphase) {
                    .self$removepoint(pts[1])
                    pts = pts[2:length(pts)]
                  } else if ( (ts$value[pts[1]] == max(ts$value[pts[1]:pts[2]])) & ( (ts$value[pts[2]]/ts$value[pts[1]]) < (1-minbear) ) ) {
                    #keep
                  } else if ( (ts$value[pts[1]] == min(ts$value[pts[1]:pts[2]])) & ( (ts$value[pts[2]]/ts$value[pts[1]]) > (1+minbull) ) ) {
                    #keep
                  } else {
                    .self$removepoint(pts[1])
                    pts = pts[2:length(pts)]
                    
                  }
                  
                }
                
              },
              phase2 = function() {
                
                pts = tp
                exlist = c()
                
                j=1
                while (j<length(pts)){
                  
                  a = pts[j]
                  b = pts[j+1]
                  
                  if (ts$value[b]>ts$value[a]){
                    res = .self$findMaxReversal(ts$date[a:b], ts$value[a:b], minbear, 0)                           
                  } else {
                    res = .self$findMaxReversal(ts$date[a:b], 1/ts$value[a:b], minbull/(1+minbull), 1)
                  }
                  
                  if (length(res)>0){
                    
                    c = res[1]
                    d = res[2]
                    
                    c = c+a-1;
                    d = d+a-1;
                    
                    pts = c(pts[1:j], c, d, pts[(j+1):length(pts)])  
                    .self$addpoint(c,ts$value[c]>ts$value[d])
                    .self$addpoint(d,ts$value[d]>ts$value[c])
                    
                    
                  } else {
                    j=j+1
                  }
                  
                }
                
                #Check for cycle violations across phase 1 boundaries
                i=1
                
                while (i<length(pts)-2){
                  
                  if ( .self$datediff(ts$date[pts[i]],ts$date[pts[i+2]]) < mincycle ){
                    
                    if (ts$value[pts[i]]>ts$value[pts[i+1]]){
                      
                      #Peak > Trough > Peak
                      if ( ts$value[pts[i]] > ts$value[pts[i+2]] ){
                        #remove i+2/i+3
                        k=i+2
                        
                      } else {
                        
                        #remove i-1/i
                        k=i;
                        
                      }
                      
                    } else {
                      
                      #Trough > Peak > Trough
                      if ( ts$value[pts[i]] > ts$value[pts[i+2]] ){
                        #remove i-1/i
                        k=i
                        
                      } else {
                        #remove i+2/i+3
                        k=i+2
                      }
                    }
                    
                    if ((k==i) && (k>2)) {
                      pts = pts[c(1:(k-2),(k+1):length(pts))]
                      .self$removepoint(k-1)
                      .self$removepoint(k)
                      i=i-1
                      
                    } else if (k<length(pts)) {
                      pts = pts[c(1:(k-1),(k+2):length(pts))]
                      .self$removepoint(k)
                      .self$removepoint(k+1)
                      i=i-1
                    }
                    
                  }
                  
                  i=i+1
                }
                
              },
              findMaxReversal = function(x, y, minreversal, invert) {
                
                a = 0
                b = 0
                n = length(x);
                
                if ( .self$datediff(x[1], x[n]) < minphase) {
                  return()
                }
                
                if (invert){
                  z = -1
                } else{
                  z = 1
                }
                
                res = .self$LRminmax( y );  
                
                minPts = res[[1]]
                maxPts = res[[2]]
                ret = c()
                
                #Find all reversals (respecting time)
                for (i in 1:length(maxPts)){
                  
                  k = which(minPts>maxPts[i])
                  
                  if (length(k)==0) {
                    k = length(minPts) +1
                  } else {
                    k = k[1]
                    
                    for (j in k:length(minPts)){
                      
                      if (.self$isCornerBounded(y[maxPts[i]:minPts[j]])){
                        r = y[minPts[j]] / y[maxPts[i]]
                        ret = rbind(ret, c(r, maxPts[i], minPts[j]))
                      }
                      
                    }
                    
                  }
                  
                }
                
                if (length(ret)==0){
                  return()
                }
                
                #Convert ret into a sorted matrix 
                #Reverts to vector for single row
                ret = matrix(ret,ncol=3)
                ord = order(ret[,1]);
                ret = matrix(ret[ord,],ncol=3)
                
                #Check conditions are met for a split
                i=1
                
                while (i<=nrow(ret)) {
                  
                  if (abs(ret[i,1]-1) > minreversal) {
                  
                    if (( .self$datediff(x[1], x[ret[i,3]]) > mincycle) && ( ( x[length(x)] - x[ret[i,2]]) > mincycle)){
                      
                      #Default to keeping the point, then find reasons to exclude
                      keep = 1
                      
                      
                      if (( .self$datediff(x[ret[i,2]], x[ret[i,3]]) < minphase ) && (abs(ret[i,1]^z-1) < minphaseexception) ) {
                        keep = 0
                      }
                      
                      if (  .self$datediff(x[1], x[ret[i,2]]) < minphase ) {
                        if (abs((y[ret[i,2]]/y[1])^z-1) < minphaseexception) {
                          keep = 0
                        }
                      }
                      
                      if ( .self$datediff(x[ret[i,3]],x[length(x)]) < minphase) {
                        if (abs((y[length(y)]/y[ret[i,3]])^z-1) < minphaseexception) {
                          keep = 0
                        }
                      }
                      
                      if (keep){
                        
                        a = ret[i,2]
                        b = ret[i,3]
                        return(c(a,b))
                        
                      }
                      
                    } 
                    
                     
                  } else {
                    
                    #all the others will fail so exit early
                    return(c())
                    
                  }
                  
                  i=i+1
                }
                
                
              },
              LRminmax = function(y) {
                
                minPts = c()
                maxPts = c(1)
                
                n = length(y)
                
                #Assume looking for new maximums
                lastPointAdded = 0;
                
                for (i in 2:(n-1)){
                  
                  if (lastPointAdded==0) {
                    
                    if ( sum(y[i] >= y[1:(i-1)]) == (i-1) ){
                      #Replace existing max
                      maxPts[length(maxPts)] = i
                      
                    } else if (sum(y[i] <= y[(i+1):n]) == (n-i)){
                        minPts = c(minPts, i)
                        lastPointAdded = 1
                    }
                    
                  } else {
                    
                    if ( (sum(y[i] <= y[(i+1):n]) == (n-i)) && (y[i]<y[minPts[length(minPts)]])) {
                      #Replace existing min
                      minPts[length(minPts)] = i
                    } else if (sum(y[i] > y[1:(i-1)])==(i-1)) {
                      maxPts = c(maxPts, i)
                      lastPointAdded = 0
                    }
                    
                    
                  } 
                  
                }
                
                return(list(minPts, maxPts))

              },
              isCornerBounded = function(y) {
                
                A = sort(c(min(y), max(y)))
                B = sort(c(y[1], y[length(y)]))
                
                return(isTRUE(all.equal(A,B)))
                
              },
              turningpoints = function() {
                
                .self$phase1()
                tp <<- which(ts$peak==1 | ts$trough==1)
                
                .self$phase2()
                tp <<- which(ts$peak==1 | ts$trough==1)
                
              }
            )
)

