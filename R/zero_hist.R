#' zero_hist
#'
#' @param data Input data
#' @param group Group variable in the data
#' @param value Numeric variable in the data
#'
#' @importFrom plyr ddply
#' @importFrom plyr .
#' @importFrom plyr summarise
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_vline
#'
#' @export
#'
#' @examples zero_hist(data=zip, group="Trt", value="Resp")
#'

zero_hist=function(data,group,value){
  mu <- ddply(data, .(get(group)), summarise, gr.mean=mean(get(value)))
  value<-data[value][[1]]
  group<-data[group][[1]]
  ggplot(data, aes(x=value, color=group))+
    geom_histogram(binwidth = 1,fill="white",position = "dodge")+
    geom_vline(data=mu, aes(xintercept=gr.mean, color=mu[,1]),
               linetype="dashed")+
    theme(legend.position = "top")


}

