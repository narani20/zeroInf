#' zero_hist
#'
#' @param data Input data
#' @param group Group variable in the data
#' @param value Numeric variable in the data
#'
#' @importFrom pscl zeroinfl
#' @importFrom pscl predict
#'
#' @export
#'
#' @examples zero_model(data=zip, group="Trt", value="Resp")
#'
zero_model=function(data,group,value){
  group<-data[group][[1]]
  value<-data[value][[1]]
  model <- zeroinfl(value ~ group|1, data = data)
  model2 <- zeroinfl(value ~ group|group, data = data)
  s_model2= summary(model2)
  if (0.05 < s_model2$coefficients$zero[2,4]){
    fin= model2}
  else {fin=model}
  AIC=-2*(fin$loglik)+2*fin$df.residual

  nd=data.frame(group=unique(group))
  pred=predict(fin,newdata=nd,type="count")

  final=list(model=summary(fin),aic=AIC,predict=pred)
  return(final)
}

