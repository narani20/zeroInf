#' zero_rootgram
#'
#' @param data Input data
#' @param group Group variable in the data
#' @param value Numeric variable in the data
#'
#' @importFrom pscl zeroinfl
#' @importFrom graphics par
#' @importFrom countreg rootogram
#'
#' @export
#'
#' @examples zero_rootgram(data=zip, group="Trt", value="Resp")
#'
zero_rootgram=function(data,group,value){
  group<-data[group][[1]]
  value<-data[value][[1]]
  m.intercept <- zeroinfl(value ~ group|1, data = data)
  m.group <- zeroinfl(value ~ group|group, data = data)

  par(mfrow=c(1,2))
  rootogram( m.intercept, main = "group|1")
  rootogram( m.group, main = "group|group")

}

