#' zero_count
#'
#' @param data Input data
#' @param group Group variable in the data
#' @param value Numeric variable in the data
#'
#' @importFrom graphics par
#'
#' @export
#'
#' @examples zero_count(data=zip, group="Trt", value="Resp")
#'
zero_count=function(data,group,value){
  group<-data[group][[1]]
  value<-data[value][[1]]

  par(mfrow=c(1,2))
  plot(factor(value > 0, levels = c(FALSE, TRUE), labels = c("=0", ">0")) ~ group,
       data = data, ylab = "count", main = "Zero hurdle")
  plot(value ~ group, data = data, subset = Resp > 0,
       log = "y", main = "Count of positive")

}
