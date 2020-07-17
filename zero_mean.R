#' zero_mean
#'
#' @param data Input data
#' @param group Group variable in the data
#' @param value Numeric variable in the data
#'
#' @importFrom plyr ddply
#' @importFrom plyr .
#' @importFrom plyr summarise
#'
#' @export
#'
#' @examples zero_mean(data=zip, group="Trt", value="Resp")
#'

zero_mean=function(data,group,value)
  {
  mu <- ddply(data, .(get(group)), summarise, gr.mean=mean(get(value)))
  return(mu)
}




