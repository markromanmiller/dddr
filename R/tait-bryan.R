
#' Tait-Bryan angles
#'
#' aka euler angles
#'
#' currently, it assumts valyes are in radians
#'
#' @export
tait_bryan <- function(yaw, pitch, roll) {

  # cast to double and set length equivalent.
  l <- vctrs::vec_cast_common(yaw, pitch, roll, .to = double())
  l <- vctrs::vec_recycle_common(l[[1]], l[[2]], l[[3]])
  yaw <- l[[1]]
  pitch <- l[[2]]
  roll <- l[[3]]

  # load up semantics
  sems <- get_dddr_semantics()

  matching_handedness <- sems$angles$hand == sems$axes$hand
  hand_factor <- ifelse(matching_handedness, 1, -1)

  extrinsic_axes <- lapply(
    sems$angles$extrinsic,
    function (x) {
      switch(x,
        # note this always returns the + side of the given dimension
        "yaw" = get_vector_from_dim(get_axis(sems$axes$up)),
        "pitch" = get_vector_from_dim(get_axis(sems$axes$left)),
        "roll" = get_vector_from_dim(get_axis(sems$axes$forward))
      )
    }
  )

  extrinsic_angles <- lapply(
    sems$angles$extrinsic,
    function (x) {
      switch(x,
        "yaw" = yaw,
        "pitch" = pitch,
        "roll" = roll
      )
    }
  )

  # perform the extrinsic rotations, in order.
  # I think it's outside then inside... we'll see.
  make_rotator(axis = extrinsic_axes[[3]],
               angle = hand_factor * extrinsic_angles[[3]],
               from = NULL, to = NULL) *
    make_rotator(axis = extrinsic_axes[[2]],
                 angle = hand_factor * extrinsic_angles[[2]],
                 from = NULL, to = NULL) *
    make_rotator(axis = extrinsic_axes[[1]],
                 angle = hand_factor * extrinsic_angles[[1]],
                 from = NULL, to = NULL)
}



#' Euler angles
#'
#' Extract euler angles from a quaternion. These follow Unity's conventions of
#' yaw around Y, pitch around X, and roll around Z. They are applied in order of
#' roll-pitch-yaw.
#'
#' @param q Quaternions to extract angles from
#'
#' @return Angle in radians
#' @name euler_angles
NULL


#' @rdname euler_angles
#' @export
yaw <- function(q) {
  # make z
  z <- rotate(vector3(0, 0, 1), q)
  # use x and z in atan2.
  atan2(z$x, z$z)
}

#' @rdname euler_angles
#' @export
pitch <- function(q) {
  z <- rotate(vector3(0, 0, 1), q)
  atan2(-z$y, sqrt(z$z^2 + z$x^2))
}

#' @rdname euler_angles
#' @export
roll <- function(q) {
  x <- rotate(vector3(1, 0, 0), q)
  y <- rotate(vector3(0, 1, 0), q)
  atan2(x$y, y$y)
}



