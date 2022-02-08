#' Tait-Bryan angles
#'
#' aka euler angles
#'
#' @param yaw,pitch,roll Angles specified in radians
#'
#' @export
tait_bryan <- function(yaw, pitch, roll) {
  # throw error if angles are clearly in degrees
  if (yaw > 2*pi || pitch > 2*pi || roll > 2*pi || yaw < -2*pi || pitch < -2*pi
      || roll < -2*pi) {
    stop("Expected angles 'yaw', 'pitch', and 'roll' to be specified in radians")
  }


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
    function(x) {
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
    function(x) {
      switch(x,
        "yaw" = yaw,
        "pitch" = pitch,
        "roll" = roll
      )
    }
  )

  # perform the extrinsic rotations, in order. Note that left-multiplication
  # rules are in effect here.
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


name_to_axis <- function(n) {
  switch(n,
    "pitch" = get_axis(get_axes_semantics()$right),
    "roll" = get_axis(get_axes_semantics()$forward),
    "yaw" = get_axis(get_axes_semantics()$up)
  )
}


fudge_negative <- function(a, b) {

  # there are fudge factors in the atan2 that flip a lot.
  # it matters whether a cross b is positive or negative,
  # and also whether angle and axis hands match

  prod <- cross(
    get_vector_from_dim(a),
    get_vector_from_dim(b)
  )

  direction <- sum(sapply(
    c("x", "y", "z"),
    function(x) {
      `$.dddr_vector3`(prod, x)
    }))

  sems <- get_dddr_semantics()

  if (sems$angle$hand == sems$axes$hand) {
    hand_factor <- 1
  } else {
    hand_factor <- -1
  }

  sign(direction) * hand_factor
}




# get the first intrinsic rot (global frame)

first_intrinsic_angle <- function(q) {
  sems <- get_dddr_semantics()

  axis_3 <- name_to_axis(sems$angles$intrinsic[[3]])
  axis_2 <- name_to_axis(sems$angles$intrinsic[[2]])

  # z is chosen because it's the axis of the last intrinsic rotation
  start_vector <- get_vector_from_dim(axis_3)

  v <- rotate(start_vector, q)

  atan2(
    fudge_negative(axis_3, axis_2) * `$.dddr_vector3`(v, axis_2),
    `$.dddr_vector3`(v, axis_3)
  )
}


second_intrinsic_angle <- function(q) {
  sems <- get_dddr_semantics()

  axis_3 <- name_to_axis(sems$angles$intrinsic[[3]])
  axis_2 <- name_to_axis(sems$angles$intrinsic[[2]])
  axis_1 <- name_to_axis(sems$angles$intrinsic[[1]])

  start_vector <- get_vector_from_dim(axis_3)
  v <- rotate(start_vector, q)
  atan2(
    fudge_negative(axis_3, axis_1) * `$.dddr_vector3`(v, axis_1),
    sqrt(
      `$.dddr_vector3`(v, axis_3)^2 +
        `$.dddr_vector3`(v, axis_2)^2)
  )
}

third_intrinsic_angle <- function(q) {
  sems <- get_dddr_semantics()

  axis_2 <- name_to_axis(sems$angles$intrinsic[[2]])
  axis_1 <- name_to_axis(sems$angles$intrinsic[[1]])

  v <- get_vector_from_dim(axis_2)
  w <- get_vector_from_dim(axis_1)
  atan2(
    fudge_negative(axis_2, axis_1) * `$.dddr_vector3`(rotate(v, q), axis_1),
    `$.dddr_vector3`(rotate(w, q), axis_1)
  )
}

get_rotation <- function(q, rotname) {
  sems <- get_dddr_semantics()
  switch(
    which(rotname == sems$angles$intrinsic),
    `1` = first_intrinsic_angle(q),
    `2` = second_intrinsic_angle(q),
    `3` = third_intrinsic_angle(q),
  )
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
  get_rotation(q, "yaw")
}

#' @rdname euler_angles
#' @export
pitch <- function(q) {
  get_rotation(q, "pitch")
}

#' @rdname euler_angles
#' @export
roll <- function(q) {
  get_rotation(q, "roll")
}
