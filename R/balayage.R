#' scan_loss_gamma <- function(r = 0.02,
#'                             beta = 1,
#'                             data_microsimulated,
#'                             EP_2015,
#'                             EP_lon,
#'                             normalize = FALSE,
#'                             scale = 'level',
#'                             exclude_negative = FALSE){
#'   
#'   gamma <- c(seq(0.1,1.4,by = 0.1),
#'              seq(1.5,4.5, by = 0.5),
#'              seq(5,20, by = 5))
#'   
#'   gamma <- gamma[gamma!=1]  
#'   
#'   loss_gamma <- lapply(gamma, function(g) loss_dim1_gamma(g, r = r,
#'                                                  beta = beta,
#'                                                  data_microsimulated = data_microsimulated,
#'                                                  EP_2015 = EP_2015,
#'                                                  EP_lon = EP_lon,
#'                                                  normalize = normalize,
#'                                                  exclude_negative = exclude_negative))
#'   loss_gamma2 <- data.table::data.table(data.frame(gamma = gamma,
#'                                                    l = do.call(rbind,loss_gamma)
#'   ))
#'   
#'   
#'   p1 <- ggplot2::ggplot(loss_gamma2) +
#'     ggplot2::geom_line(ggplot2::aes_string(x = "gamma", y = "l")) +
#'     ggplot2::xlim(0,5) +
#'     ggplot2::scale_y_continuous(trans = 'log10')
#'   
#'   p1 <- p1 +
#'     ggplot2::theme_bw() +
#'     ggplot2::labs(x = "Risk aversion parameter (gamma)", y = "Loss (log10 scale)") +
#'     ggplot2::theme(text = ggplot2::element_text(size=24))  
#'   
#'   return(list(loss_gamma2,p1))
#'   
#' }
#' 
#' 
#' 
#' scan_loss_r <- function(gamma = 0.5,
#'                         beta = 1,
#'                         data_microsimulated,
#'                         EP_2015,
#'                         EP_lon,
#'                         normalize = FALSE,
#'                         scale = 'level',
#'                         exclude_negative = FALSE){
#'   
#'   
#'   r <- c(seq(.001, .1, by = 0.005),
#'          seq(0.1,0.25,by = 0.05))
#'   r <- r[r!=0]
#'   
#'   loss_gamma <- lapply(r, function(R) loss_dim1_r(R, gamma = gamma,
#'                                          beta = beta,
#'                                          data_microsimulated = data_microsimulated,
#'                                          EP_2015 = EP_2015,
#'                                          EP_lon = EP_lon,
#'                                          normalize = normalize,
#'                                          exclude_negative = exclude_negative))
#'   loss_gamma2 <- data.table::data.table(data.frame(r = r,
#'                                                    l = do.call(rbind,loss_gamma)
#'   ))
#'   
#'   
#'   p1 <- ggplot2::ggplot(loss_gamma2) +
#'     ggplot2::geom_line(ggplot2::aes_string(x = "r", y = "l")) +
#'     ggplot2::xlim(0,0.25)
#'   
#'   p1 <- p1 +
#'     ggplot2::theme_bw() +
#'     ggplot2::labs(x = "Interest rate (r)", y = "Loss") +
#'     ggplot2::theme(text = ggplot2::element_text(size=24))
#'   
#'   return(list(loss_gamma2,p1))
#'   
#' }
