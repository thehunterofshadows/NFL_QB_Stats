#plot
library(ggplot2)
library(data.table)


FormatSI <- function(x, ...) {
  # Format a vector of numeric values according to the
  # International System of Units.
  # http://en.wikipedia.org/wiki/SI_prefix
  #
  # Args:
  #   x  : A vector of numeric values
  #   ...: Remaining args passed to format()
  #
  # Returns:
  #   A vector of strings using SI prefix notation
  #
  # Bugs:
  #   Does not (yet) work with small (<1) numbers
  #
  scale.frac <- 1000
  scale.unit <- c("k", "M", "G", "T", "P", "E", "Z", "Y")
  
  # Start with empty prefixes
  p <- rep(" ", length(x))
  
  # Divide by scale.frac and store scale.unit if value is
  # large enough. Repeat for all units.
  for(i in 1:length(scale.unit)) {
    p[x >= scale.frac] <- scale.unit[i]
    x[x >= scale.frac] <- x[x >= scale.frac] / scale.frac
  }
  
  return(paste(format(round(x,1), trim=TRUE, scientific=FALSE, ...), p))
}

get_date_range <- function(dates, by = "3 days"){
  z <- data.table(seq(min(dates), max(dates), by = by))
  z<-rbind(z, data.table(max(dates)))
  z<-ymd(z)
  return(z)
}

get_finaldate_mean <-function(df){
  return(
    mean(df$y[df$x==max(df$x)])
  )
}

jplot <-function(x, y, col = "NA", type = "line", y_label = "y_label", x_label = "x_label", title="My Title",num_y=5,x_value_type = "date", abline=FALSE){
  df<-data.frame(x = x,
                 y = y,
                 col=col)
  
  #date range - disabled for now becasue it's not working
  #dateRange<-get_date_range(df$x)
  if(type == "line"){
    if(x_value_type == "date"){ #date x values
      max_y <- max(df$y)
      min_y<-min(df$y)
      range<-seq(0,max_y,by = 1000)
      
      g<-ggplot(data = df , aes(x,y)) +
        geom_line(aes(col=col), size = 1.5) +
        labs(title=title, x=x_label, y = y_label) + 
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        scale_x_date(breaks = scales::pretty_breaks(), date_labels = "%A  %b-%d") +
        theme(legend.text=element_text(size = 16), legend.position = "bottom", 
              legend.title = element_blank(),
              axis.title = element_text(size=16),
              axis.text = element_text(size=14),
              axis.text.x = element_text(angle=45, vjust=1, hjust=1),
              plot.title = element_text(size=16, face = "bold")
        )
      
      #scale_x_date(date_breaks = "3 days", date_labels = "%A  %b-%d") +
      #scale_x_date(breaks = scales:pretty_breaks(), date_labels = "%A  %b-%d") +
      #scale_y_continuous(breaks = scales::pretty_breaks()) +
      
    } else { #none date x values
      max_y <- max(df$y)
      min_y<-min(df$y)
      range<-seq(0,max_y,by = 1000)
      
      g<-ggplot(data = df , aes(x,y)) +
        geom_line(aes(col=col), size = 1.5) +
        labs(title=title, x=x_label, y = y_label) + 
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        theme(legend.text=element_text(size = 16), legend.position = "bottom", 
              legend.title = element_blank(),
              axis.title = element_text(size=16),
              axis.text = element_text(size=14),
              axis.text.x = element_text(angle=45, vjust=1, hjust=1),
              plot.title = element_text(size=16, face = "bold")
        )
    }
    
    if(abline){
      g<-g+
        geom_hline(aes(yintercept = get_finaldate_mean(df)),
                   size =1.5, linetype="dashed",color="red")+
        geom_text(aes(min(df$x),
                      get_finaldate_mean(df),
                      label=paste(formatC(round(get_finaldate_mean(df)),format="d",big.mark=","),
                                  ": Avg Cases Final Date",
                                  collapse = ""),
                      vjust = -1, hjust = 0),
                  size = 5)
    }
  } else if (type == "bar") {
    g<-ggplot(data = df, mapping = aes(x=reorder(x,-y), 
                                       y=y))+
      geom_bar(stat="identity")+
      labs(title=title, x=x_label, y = y_label)+
      theme(axis.text = element_text(size=14),
            axis.text.x = element_text(angle=45, vjust=1, hjust=1))
    
  }
  
  
  return(g)
  
  #prior y breaks
  #scale_y_continuous(breaks=range)+
  #scale_x_date(breaks = scales::pretty_breaks(n = 4)) +
  
  
  #pior date breaks
  #scale_x_date(date_breaks = "3 days", date_labels = "%b-%d") +
  #prior date breaks
  #scale_x_date(date_breaks = "3 days", date_labels = "%b-%d") +
  
  #attempt at ensuring final date shows up, no luck so far
  #scale_x_date(date_breaks = dateRange, date_labels = "%b-%d") +
  
  
  
}

