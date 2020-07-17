#' zero_box
#'
#' @param data Input data
#' @param group Group variable in the data
#' @param value Numeric variable in the data
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#'
#' @export
#'
#' @examples zero_box(data=zip, group="Trt", value="Resp")
#'
zero_box=function(data,group,value)
{
  group<-data[group][[1]]
  value<-data[value][[1]]
  ggplot(data, aes(x=group, y=value, group=group,color=group))+
    geom_boxplot()
}
