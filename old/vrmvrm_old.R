

get_col <- function(symed, suffix, envir) {
  symed %>% rlang::as_string() %>% paste(suffix, sep="") %>% sym %>% eval(envir=envir)
}

#' Add numeric vectors representing points together
#'
#' @param addend_a,addend_b 3-vector specifications, either a name that can be
#'   expanded into three columns of the data.frame, or a 3-element R vector
#'   representing a constant
#' @return The vector sum of the addends as entries in the mutated data.frame
#'   using the assigned name
#' @example vrm_axes %>% mutate(vadd())
#' @example vrm_spiral %>% mutate(vadd(c(0,0,1), spiral))
vadd <- function(a, b) {
  envir <- rlang::caller_env()

  a_px <- get_col(ensym(a), "_px", envir)
  a_py <- get_col(ensym(a), "_py", envir)
  a_pz <- get_col(ensym(a), "_pz", envir)

  b_px <- get_col(ensym(b), "_px", envir)
  b_py <- get_col(ensym(b), "_py", envir)
  b_pz <- get_col(ensym(b), "_pz", envir)

  return(list(
    "_px" = a_px + b_px,
    "_py" = a_py + b_py,
    "_pz" = a_pz + b_pz
  ))
}

class(vadd) <- c("vrm_vector_function", class(vadd))

vqrot <- function(v, q) {
  envir <- rlang::caller_env()

  v_px <- get_col(ensym(v), "_px", envir)
  v_py <- get_col(ensym(v), "_py", envir)
  v_pz <- get_col(ensym(v), "_pz", envir)

  q_qw <- get_col(ensym(q), "_qw", envir)
  q_qx <- get_col(ensym(q), "_qx", envir)
  q_qy <- get_col(ensym(q), "_qy", envir)
  q_qz <- get_col(ensym(q), "_qz", envir)

  # pre-multiply by q, post-multiply by q*
  qp_qw <- q_qw*   0 - q_qx*v_px - q_qy*v_py - q_qz*v_pz
  qp_qx <- q_qw*v_px + q_qx*   0 + q_qy*v_pz - q_qz*v_py
  qp_qy <- q_qw*v_py - q_qx*v_pz + q_qy*   0 + q_qz*v_px
  qp_qz <- q_qw*v_pz + q_qx*v_py - q_qy*v_px + q_qz*   0

  qpq_qw <- qp_qw*-q_qw - qp_qx* q_qx - qp_qy* q_qy - qp_qz* q_qz
  qpq_qx <- qp_qw* q_qx + qp_qx*-q_qw + qp_qy* q_qz - qp_qz* q_qy
  qpq_qy <- qp_qw* q_qy - qp_qx* q_qz + qp_qy*-q_qw + qp_qz* q_qx
  qpq_qz <- qp_qw* q_qz + qp_qx* q_qy - qp_qy* q_qx + qp_qz*-q_qw

  #print(qpq_qw)
  stopifnot(any(abs(qpq_qw) < 0.00001))

  return(list(
    "_px" = qpq_qx,
    "_py" = qpq_qy,
    "_pz" = qpq_qz
  ))
}

class(vqrot) <- c("vrm_vector_function", class(vqrot))

q2euler <- function(q) {
  envir <- rlang::caller_env()

  q_pw <- get_col(ensym(q), "_qw", envir)
  q_px <- get_col(ensym(q), "_qx", envir)
  q_py <- get_col(ensym(q), "_qy", envir)
  q_pz <- get_col(ensym(q), "_qz", envir)


  #EulerAngles angles;

  # roll (z-axis rotation)

  # (x -> z)
  # (y -> x)
  # (z -> y)

  sinr_cosp = +2.0 * (q_pw * q_pz + q_px * q_py)
  cosr_cosp = +1.0 - 2.0 * (q_pz * q_pz + q_px *  q_px)
  roll = atan2(sinr_cosp, cosr_cosp)

  #// pitch (y-axis rotation)
  sinp = +2.0 * (q_pw * q_px - q_py * q_pz);
  #if (fabs(sinp) >= 1) {
  #  if (M_PI)
  #  pitch = copysign(M_PI / 2, sinp)
  #} else {
  pitch = asin(sinp)
  #}

  #// yaw (z-axis rotation)
  siny_cosp = +2.0 * (q_pw * q_py + q_pz * q_px)
  cosy_cosp = +1.0 - 2.0 * (q_px * q_px + q_py * q_py);
  yaw = atan2(siny_cosp, cosy_cosp)

  return(list(
    "_ex" = pitch,
    "_ey" = yaw,
    "_ez" = roll
  ))
}

class(q2euler) <- c("vrm_vector_function", class(q2euler))

mutate <- function(df, ...) {
  mutate_args <- as.list(match.call(expand.dots = FALSE))$`...`
  n_mutate_args <- length(mutate_args)

  # create_nse_eval_env
  vrm_env <- list2env(df)
  parent.env(vrm_env) <- rlang::caller_env()

  for (i in seq(n_mutate_args)) {
    name <- names(mutate_args)[[i]]
    exp_arg <- mutate_args[[i]]
    function_symbol <- exp_arg[[1]]
    # TODO: will this eval cause unexpected behavior? - are some functions not meant to be evaled?
    if (is.symbol(function_symbol) & inherits(eval(function_symbol), "vrm_vector_function")) {
      # do my fancy eval function, and return a list of suffixes,
      mutate_call <- eval(exp_arg, env=vrm_env)
      # to which we append the names
      names(mutate_call) <- paste(name, names(mutate_call), sep="")

    } else { # do the regular mutate...
      mutate_call <- exp_arg
      names(mutate_call) <- name

    }
    # jam the result back into the DF
    df <- dplyr::mutate(df, !!!mutate_call)
  }
  return(df)
}








