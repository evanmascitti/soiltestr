#' \lifecycle{experimental}
#'
#' Trim the aluminum cylinder from a 3D infield mix specimen mesh
#'
#' Uses a dirty hack to remove the exterior of the specimen, leaving a disk
#' containing only the soil surface.
#'
#' @param mesh the mesh object to be trimmed
#' @param top_radius the radius of the quasi-cone, i.e. the radius from the center of the mesh object along with to trim (measured in mm)
#' @param bottom_radius the lower radius of the quasi-cone (measured in mm)
#' @param z_depth the distance between top and bottom of the quasi-cone
#'
#' @return A new "mesh3d" object with the cylinder exterior removed
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom Morpho cutMeshPlane
#'
#' @export
#'
scissor_trim_mesh <- function(mesh, top_radius = 70, bottom_radius= 10, z_depth = 800) {
  ##### generate coordinates of planes #####
  coords <- tibble::tibble(
                   plane_num= as.integer(c(1:360) ),
                   ra= top_radius,
                   rb= bottom_radius,
                   theta= circular::rad( c(0:359) ),
                   x1= .data$ra*cos(.data$theta),
                   y1= .data$ra*sin(.data$theta),
                   z1=0,
                   x2= .data$rb*cos(1.01*.data$theta),
                   y2= .data$rb*sin(1.01*.data$theta),
                   z2= -z_depth,
                   x3= .data$rb*cos(0.99*.data$theta),
                   y3= .data$rb*sin(0.99*.data$theta),
                   z3= -z_depth ) %>%
    select(.data$plane_num, .data$x1:.data$z3) %>%
    dplyr::group_by(.data$plane_num) %>%
    tidyr::nest(stuff= !.data$plane_num) %>%
    dplyr::mutate(v1= map(.data$stuff, ~c(.$x1, .$y1, .$z1)),
           v2= map(.data$stuff, ~c(.$x2, .$y2, .$z2)),
           v3= map(.data$stuff, ~c(.$x3, .$y3, .$z3))) %>%
    select(-.data$stuff) %>%
    dplyr::ungroup()

  ##### generate planes #####

  plane01 <-  coords %>%
    filter(.data$plane_num== 1) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane02 <- coords %>%
    filter(.data$plane_num== 2) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane03 <- coords %>%
    filter(.data$plane_num== 3) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane04 <- coords %>%
    filter(.data$plane_num== 4) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane05 <- coords %>%
    filter(.data$plane_num== 5) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane06 <- coords %>%
    filter(.data$plane_num== 6) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane07 <- coords %>%
    filter(.data$plane_num== 7) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane08 <- coords %>%
    filter(.data$plane_num== 8) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane09 <- coords %>%
    filter(.data$plane_num== 9) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane10 <- coords %>%
    filter(.data$plane_num== 10) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane11 <- coords %>%
    filter(.data$plane_num== 11) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane12 <- coords %>%
    filter(.data$plane_num== 12) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane13 <- coords %>%
    filter(.data$plane_num== 13) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane14 <- coords %>%
    filter(.data$plane_num== 14) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane15 <- coords %>%
    filter(.data$plane_num== 15) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane16 <- coords %>%
    filter(.data$plane_num== 16) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane17 <- coords %>%
    filter(.data$plane_num== 17) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane18 <- coords %>%
    filter(.data$plane_num== 18) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane19 <- coords %>%
    filter(.data$plane_num== 19) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane20 <- coords %>%
    filter(.data$plane_num== 20) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane21 <- coords %>%
    filter(.data$plane_num== 21) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane22 <- coords %>%
    filter(.data$plane_num== 22) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane23 <- coords %>%
    filter(.data$plane_num== 23) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane24 <- coords %>%
    filter(.data$plane_num== 24) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane25 <- coords %>%
    filter(.data$plane_num== 25) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane26 <- coords %>%
    filter(.data$plane_num== 26) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane27 <- coords %>%
    filter(.data$plane_num== 27) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane28 <- coords %>%
    filter(.data$plane_num== 28) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane29 <- coords %>%
    filter(.data$plane_num== 29) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane30 <- coords %>%
    filter(.data$plane_num== 30) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane31 <- coords %>%
    filter(.data$plane_num== 31) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane32 <- coords %>%
    filter(.data$plane_num== 32) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane33 <- coords %>%
    filter(.data$plane_num== 33) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane34 <- coords %>%
    filter(.data$plane_num== 34) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane35 <- coords %>%
    filter(.data$plane_num== 35) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane36 <- coords %>%
    filter(.data$plane_num== 36) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane37 <- coords %>%
    filter(.data$plane_num== 37) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane38 <- coords %>%
    filter(.data$plane_num== 38) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane39 <- coords %>%
    filter(.data$plane_num== 39) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane40 <- coords %>%
    filter(.data$plane_num== 40) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane41 <- coords %>%
    filter(.data$plane_num== 41) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane42 <- coords %>%
    filter(.data$plane_num== 42) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane43 <- coords %>%
    filter(.data$plane_num== 43) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane44 <- coords %>%
    filter(.data$plane_num== 44) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane45 <- coords %>%
    filter(.data$plane_num== 45) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane46 <- coords %>%
    filter(.data$plane_num== 46) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane47 <- coords %>%
    filter(.data$plane_num== 47) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane48 <- coords %>%
    filter(.data$plane_num== 48) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane49 <- coords %>%
    filter(.data$plane_num== 49) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane50 <- coords %>%
    filter(.data$plane_num== 50) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane51 <- coords %>%
    filter(.data$plane_num== 51) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane52 <- coords %>%
    filter(.data$plane_num== 52) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane53 <- coords %>%
    filter(.data$plane_num== 53) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane54 <- coords %>%
    filter(.data$plane_num== 54) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane55 <- coords %>%
    filter(.data$plane_num== 55) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane56 <- coords %>%
    filter(.data$plane_num== 56) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane57 <- coords %>%
    filter(.data$plane_num== 57) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane58 <- coords %>%
    filter(.data$plane_num== 58) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane59 <- coords %>%
    filter(.data$plane_num== 59) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane60 <- coords %>%
    filter(.data$plane_num== 60) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane61 <- coords %>%
    filter(.data$plane_num== 61) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane62 <- coords %>%
    filter(.data$plane_num== 62) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane63 <- coords %>%
    filter(.data$plane_num== 63) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane64 <- coords %>%
    filter(.data$plane_num== 64) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane65 <- coords %>%
    filter(.data$plane_num== 65) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane66 <- coords %>%
    filter(.data$plane_num== 66) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane67 <- coords %>%
    filter(.data$plane_num== 67) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane68 <- coords %>%
    filter(.data$plane_num== 68) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane69 <- coords %>%
    filter(.data$plane_num== 69) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane70 <- coords %>%
    filter(.data$plane_num== 70) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane71 <- coords %>%
    filter(.data$plane_num== 71) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane72 <- coords %>%
    filter(.data$plane_num== 72) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane73 <- coords %>%
    filter(.data$plane_num== 73) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane74 <- coords %>%
    filter(.data$plane_num== 74) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane75 <- coords %>%
    filter(.data$plane_num== 75) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane76 <- coords %>%
    filter(.data$plane_num== 76) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane77 <- coords %>%
    filter(.data$plane_num== 77) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane78 <- coords %>%
    filter(.data$plane_num== 78) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane79 <- coords %>%
    filter(.data$plane_num== 79) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane80 <- coords %>%
    filter(.data$plane_num== 80) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane81 <- coords %>%
    filter(.data$plane_num== 81) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane82 <- coords %>%
    filter(.data$plane_num== 82) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane83 <- coords %>%
    filter(.data$plane_num== 83) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane84 <- coords %>%
    filter(.data$plane_num== 84) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane85 <- coords %>%
    filter(.data$plane_num== 85) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane86 <- coords %>%
    filter(.data$plane_num== 86) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane87 <- coords %>%
    filter(.data$plane_num== 87) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane88 <- coords %>%
    filter(.data$plane_num== 88) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane89 <- coords %>%
    filter(.data$plane_num== 89) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane90 <- coords %>%
    filter(.data$plane_num== 90) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane91 <- coords %>%
    filter(.data$plane_num== 91) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane92 <- coords %>%
    filter(.data$plane_num== 92) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane93 <- coords %>%
    filter(.data$plane_num== 93) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane94 <- coords %>%
    filter(.data$plane_num== 94) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane95 <- coords %>%
    filter(.data$plane_num== 95) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane96 <- coords %>%
    filter(.data$plane_num== 96) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane97 <- coords %>%
    filter(.data$plane_num== 97) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane98 <- coords %>%
    filter(.data$plane_num== 98) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane99 <- coords %>%
    filter(.data$plane_num== 99) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane100 <- coords %>%
    filter(.data$plane_num== 100) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane101 <-  coords %>%
    filter(.data$plane_num== 101) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane102 <- coords %>%
    filter(.data$plane_num== 102) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane103 <- coords %>%
    filter(.data$plane_num== 103) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane104 <- coords %>%
    filter(.data$plane_num== 104) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane105 <- coords %>%
    filter(.data$plane_num== 105) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane106 <- coords %>%
    filter(.data$plane_num== 106) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane107 <- coords %>%
    filter(.data$plane_num== 107) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane108 <- coords %>%
    filter(.data$plane_num== 108) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane109 <- coords %>%
    filter(.data$plane_num== 109) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane110 <- coords %>%
    filter(.data$plane_num== 110) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane111 <- coords %>%
    filter(.data$plane_num== 111) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane112 <- coords %>%
    filter(.data$plane_num== 112) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane113 <- coords %>%
    filter(.data$plane_num== 113) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane114 <- coords %>%
    filter(.data$plane_num== 114) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane115 <- coords %>%
    filter(.data$plane_num== 115) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane116 <- coords %>%
    filter(.data$plane_num== 116) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane117 <- coords %>%
    filter(.data$plane_num== 117) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane118 <- coords %>%
    filter(.data$plane_num== 118) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane119 <- coords %>%
    filter(.data$plane_num== 119) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane120 <- coords %>%
    filter(.data$plane_num== 120) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane121 <- coords %>%
    filter(.data$plane_num== 121) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane122 <- coords %>%
    filter(.data$plane_num== 122) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane123 <- coords %>%
    filter(.data$plane_num== 123) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane124 <- coords %>%
    filter(.data$plane_num== 124) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane125 <- coords %>%
    filter(.data$plane_num== 125) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane126 <- coords %>%
    filter(.data$plane_num== 126) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane127 <- coords %>%
    filter(.data$plane_num== 127) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane128 <- coords %>%
    filter(.data$plane_num== 128) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane129 <- coords %>%
    filter(.data$plane_num== 129) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane130 <- coords %>%
    filter(.data$plane_num== 130) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane131 <- coords %>%
    filter(.data$plane_num== 131) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane132 <- coords %>%
    filter(.data$plane_num== 132) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane133 <- coords %>%
    filter(.data$plane_num== 133) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane134 <- coords %>%
    filter(.data$plane_num== 134) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane135 <- coords %>%
    filter(.data$plane_num== 135) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane136 <- coords %>%
    filter(.data$plane_num== 136) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane137 <- coords %>%
    filter(.data$plane_num== 137) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane138 <- coords %>%
    filter(.data$plane_num== 138) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane139 <- coords %>%
    filter(.data$plane_num== 139) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane140 <- coords %>%
    filter(.data$plane_num== 140) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane141 <- coords %>%
    filter(.data$plane_num== 141) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane142 <- coords %>%
    filter(.data$plane_num== 142) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane143 <- coords %>%
    filter(.data$plane_num== 143) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane144 <- coords %>%
    filter(.data$plane_num== 144) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane145 <- coords %>%
    filter(.data$plane_num== 145) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane146 <- coords %>%
    filter(.data$plane_num== 146) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane147 <- coords %>%
    filter(.data$plane_num== 147) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane148 <- coords %>%
    filter(.data$plane_num== 148) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane149 <- coords %>%
    filter(.data$plane_num== 149) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane150 <- coords %>%
    filter(.data$plane_num== 150) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane151 <- coords %>%
    filter(.data$plane_num== 151) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane152 <- coords %>%
    filter(.data$plane_num== 152) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane153 <- coords %>%
    filter(.data$plane_num== 153) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane154 <- coords %>%
    filter(.data$plane_num== 154) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane155 <- coords %>%
    filter(.data$plane_num== 155) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane156 <- coords %>%
    filter(.data$plane_num== 156) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane157 <- coords %>%
    filter(.data$plane_num== 157) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane158 <- coords %>%
    filter(.data$plane_num== 158) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane159 <- coords %>%
    filter(.data$plane_num== 159) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane160 <- coords %>%
    filter(.data$plane_num== 160) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane161 <- coords %>%
    filter(.data$plane_num== 161) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane162 <- coords %>%
    filter(.data$plane_num== 162) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane163 <- coords %>%
    filter(.data$plane_num== 163) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane164 <- coords %>%
    filter(.data$plane_num== 164) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane165 <- coords %>%
    filter(.data$plane_num== 165) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane166 <- coords %>%
    filter(.data$plane_num== 166) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane167 <- coords %>%
    filter(.data$plane_num== 167) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane168 <- coords %>%
    filter(.data$plane_num== 168) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane169 <- coords %>%
    filter(.data$plane_num== 169) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane170 <- coords %>%
    filter(.data$plane_num== 170) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane171 <- coords %>%
    filter(.data$plane_num== 171) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane172 <- coords %>%
    filter(.data$plane_num== 172) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane173 <- coords %>%
    filter(.data$plane_num== 173) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane174 <- coords %>%
    filter(.data$plane_num== 174) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane175 <- coords %>%
    filter(.data$plane_num== 175) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane176 <- coords %>%
    filter(.data$plane_num== 176) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane177 <- coords %>%
    filter(.data$plane_num== 177) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane178 <- coords %>%
    filter(.data$plane_num== 178) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane179 <- coords %>%
    filter(.data$plane_num== 179) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane180 <- coords %>%
    filter(.data$plane_num== 180) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane181 <- coords %>%
    filter(.data$plane_num== 181) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane182 <- coords %>%
    filter(.data$plane_num== 182) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane183 <- coords %>%
    filter(.data$plane_num== 183) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane184 <- coords %>%
    filter(.data$plane_num== 184) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane185 <- coords %>%
    filter(.data$plane_num== 185) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane186 <- coords %>%
    filter(.data$plane_num== 186) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane187 <- coords %>%
    filter(.data$plane_num== 187) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane188 <- coords %>%
    filter(.data$plane_num== 188) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane189 <- coords %>%
    filter(.data$plane_num== 189) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane190 <- coords %>%
    filter(.data$plane_num== 190) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane191 <- coords %>%
    filter(.data$plane_num== 191) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane192 <- coords %>%
    filter(.data$plane_num== 192) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane193 <- coords %>%
    filter(.data$plane_num== 193) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane194 <- coords %>%
    filter(.data$plane_num== 194) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane195 <- coords %>%
    filter(.data$plane_num== 195) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane196 <- coords %>%
    filter(.data$plane_num== 196) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane197 <- coords %>%
    filter(.data$plane_num== 197) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane198 <- coords %>%
    filter(.data$plane_num== 198) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane199 <- coords %>%
    filter(.data$plane_num== 199) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane200 <- coords %>%
    filter(.data$plane_num== 200) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane201 <-  coords %>%
    filter(.data$plane_num== 201) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane202 <- coords %>%
    filter(.data$plane_num== 202) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane203 <- coords %>%
    filter(.data$plane_num== 203) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane204 <- coords %>%
    filter(.data$plane_num== 204) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane205 <- coords %>%
    filter(.data$plane_num== 205) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane206 <- coords %>%
    filter(.data$plane_num== 206) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane207 <- coords %>%
    filter(.data$plane_num== 207) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane208 <- coords %>%
    filter(.data$plane_num== 208) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane209 <- coords %>%
    filter(.data$plane_num== 209) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane210 <- coords %>%
    filter(.data$plane_num== 210) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane211 <- coords %>%
    filter(.data$plane_num== 211) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane212 <- coords %>%
    filter(.data$plane_num== 212) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane213 <- coords %>%
    filter(.data$plane_num== 213) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane214 <- coords %>%
    filter(.data$plane_num== 214) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane215 <- coords %>%
    filter(.data$plane_num== 215) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane216 <- coords %>%
    filter(.data$plane_num== 216) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane217 <- coords %>%
    filter(.data$plane_num== 217) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane218 <- coords %>%
    filter(.data$plane_num== 218) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane219 <- coords %>%
    filter(.data$plane_num== 219) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane220 <- coords %>%
    filter(.data$plane_num== 220) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane221 <- coords %>%
    filter(.data$plane_num== 221) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane222 <- coords %>%
    filter(.data$plane_num== 222) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane223 <- coords %>%
    filter(.data$plane_num== 223) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane224 <- coords %>%
    filter(.data$plane_num== 224) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane225 <- coords %>%
    filter(.data$plane_num== 225) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane226 <- coords %>%
    filter(.data$plane_num== 226) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane227 <- coords %>%
    filter(.data$plane_num== 227) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane228 <- coords %>%
    filter(.data$plane_num== 228) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane229 <- coords %>%
    filter(.data$plane_num== 229) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane230 <- coords %>%
    filter(.data$plane_num== 230) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane231 <- coords %>%
    filter(.data$plane_num== 231) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane232 <- coords %>%
    filter(.data$plane_num== 232) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane233 <- coords %>%
    filter(.data$plane_num== 233) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane234 <- coords %>%
    filter(.data$plane_num== 234) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane235 <- coords %>%
    filter(.data$plane_num== 235) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane236 <- coords %>%
    filter(.data$plane_num== 236) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane237 <- coords %>%
    filter(.data$plane_num== 237) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane238 <- coords %>%
    filter(.data$plane_num== 238) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane239 <- coords %>%
    filter(.data$plane_num== 239) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane240 <- coords %>%
    filter(.data$plane_num== 240) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane241 <- coords %>%
    filter(.data$plane_num== 241) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane242 <- coords %>%
    filter(.data$plane_num== 242) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane243 <- coords %>%
    filter(.data$plane_num== 243) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane244 <- coords %>%
    filter(.data$plane_num== 244) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane245 <- coords %>%
    filter(.data$plane_num== 245) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane246 <- coords %>%
    filter(.data$plane_num== 246) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane247 <- coords %>%
    filter(.data$plane_num== 247) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane248 <- coords %>%
    filter(.data$plane_num== 248) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane249 <- coords %>%
    filter(.data$plane_num== 249) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane250 <- coords %>%
    filter(.data$plane_num== 250) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane251 <- coords %>%
    filter(.data$plane_num== 251) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane252 <- coords %>%
    filter(.data$plane_num== 252) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane253 <- coords %>%
    filter(.data$plane_num== 253) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane254 <- coords %>%
    filter(.data$plane_num== 254) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane255 <- coords %>%
    filter(.data$plane_num== 255) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane256 <- coords %>%
    filter(.data$plane_num== 256) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane257 <- coords %>%
    filter(.data$plane_num== 257) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane258 <- coords %>%
    filter(.data$plane_num== 258) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane259 <- coords %>%
    filter(.data$plane_num== 259) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane260 <- coords %>%
    filter(.data$plane_num== 260) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane261 <- coords %>%
    filter(.data$plane_num== 261) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane262 <- coords %>%
    filter(.data$plane_num== 262) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane263 <- coords %>%
    filter(.data$plane_num== 263) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane264 <- coords %>%
    filter(.data$plane_num== 264) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane265 <- coords %>%
    filter(.data$plane_num== 265) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane266 <- coords %>%
    filter(.data$plane_num== 266) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane267 <- coords %>%
    filter(.data$plane_num== 267) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane268 <- coords %>%
    filter(.data$plane_num== 268) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane269 <- coords %>%
    filter(.data$plane_num== 269) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane270 <- coords %>%
    filter(.data$plane_num== 270) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane271 <- coords %>%
    filter(.data$plane_num== 271) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane272 <- coords %>%
    filter(.data$plane_num== 272) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane273 <- coords %>%
    filter(.data$plane_num== 273) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane274 <- coords %>%
    filter(.data$plane_num== 274) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane275 <- coords %>%
    filter(.data$plane_num== 275) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane276 <- coords %>%
    filter(.data$plane_num== 276) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane277 <- coords %>%
    filter(.data$plane_num== 277) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane278 <- coords %>%
    filter(.data$plane_num== 278) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane279 <- coords %>%
    filter(.data$plane_num== 279) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane280 <- coords %>%
    filter(.data$plane_num== 280) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane281 <- coords %>%
    filter(.data$plane_num== 281) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane282 <- coords %>%
    filter(.data$plane_num== 282) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane283 <- coords %>%
    filter(.data$plane_num== 283) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane284 <- coords %>%
    filter(.data$plane_num== 284) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane285 <- coords %>%
    filter(.data$plane_num== 285) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane286 <- coords %>%
    filter(.data$plane_num== 286) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane287 <- coords %>%
    filter(.data$plane_num== 287) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane288 <- coords %>%
    filter(.data$plane_num== 288) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane289 <- coords %>%
    filter(.data$plane_num== 289) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane290 <- coords %>%
    filter(.data$plane_num== 290) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane291 <- coords %>%
    filter(.data$plane_num== 291) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane292 <- coords %>%
    filter(.data$plane_num== 292) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane293 <- coords %>%
    filter(.data$plane_num== 293) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane294 <- coords %>%
    filter(.data$plane_num== 294) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane295 <- coords %>%
    filter(.data$plane_num== 295) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane296 <- coords %>%
    filter(.data$plane_num== 296) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane297 <- coords %>%
    filter(.data$plane_num== 297) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane298 <- coords %>%
    filter(.data$plane_num== 298) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane299 <- coords %>%
    filter(.data$plane_num== 299) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane300 <- coords %>%
    filter(.data$plane_num== 300) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane301 <-  coords %>%
    filter(.data$plane_num== 301) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane302 <- coords %>%
    filter(.data$plane_num== 302) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane303 <- coords %>%
    filter(.data$plane_num== 303) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane304 <- coords %>%
    filter(.data$plane_num== 304) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane305 <- coords %>%
    filter(.data$plane_num== 305) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane306 <- coords %>%
    filter(.data$plane_num== 306) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane307 <- coords %>%
    filter(.data$plane_num== 307) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane308 <- coords %>%
    filter(.data$plane_num== 308) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane309 <- coords %>%
    filter(.data$plane_num== 309) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane310 <- coords %>%
    filter(.data$plane_num== 310) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane311 <- coords %>%
    filter(.data$plane_num== 311) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane312 <- coords %>%
    filter(.data$plane_num== 312) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane313 <- coords %>%
    filter(.data$plane_num== 313) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane314 <- coords %>%
    filter(.data$plane_num== 314) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane315 <- coords %>%
    filter(.data$plane_num== 315) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane316 <- coords %>%
    filter(.data$plane_num== 316) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane317 <- coords %>%
    filter(.data$plane_num== 317) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane318 <- coords %>%
    filter(.data$plane_num== 318) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane319 <- coords %>%
    filter(.data$plane_num== 319) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane320 <- coords %>%
    filter(.data$plane_num== 320) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane321 <- coords %>%
    filter(.data$plane_num== 321) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane322 <- coords %>%
    filter(.data$plane_num== 322) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane323 <- coords %>%
    filter(.data$plane_num== 323) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane324 <- coords %>%
    filter(.data$plane_num== 324) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane325 <- coords %>%
    filter(.data$plane_num== 325) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane326 <- coords %>%
    filter(.data$plane_num== 326) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane327 <- coords %>%
    filter(.data$plane_num== 327) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane328 <- coords %>%
    filter(.data$plane_num== 328) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane329 <- coords %>%
    filter(.data$plane_num== 329) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane330 <- coords %>%
    filter(.data$plane_num== 330) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane331 <- coords %>%
    filter(.data$plane_num== 331) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane332 <- coords %>%
    filter(.data$plane_num== 332) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane333 <- coords %>%
    filter(.data$plane_num== 333) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane334 <- coords %>%
    filter(.data$plane_num== 334) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane335 <- coords %>%
    filter(.data$plane_num== 335) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane336 <- coords %>%
    filter(.data$plane_num== 336) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane337 <- coords %>%
    filter(.data$plane_num== 337) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane338 <- coords %>%
    filter(.data$plane_num== 338) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane339 <- coords %>%
    filter(.data$plane_num== 339) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane340 <- coords %>%
    filter(.data$plane_num== 340) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane341 <- coords %>%
    filter(.data$plane_num== 341) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane342 <- coords %>%
    filter(.data$plane_num== 342) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane343 <- coords %>%
    filter(.data$plane_num== 343) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane344 <- coords %>%
    filter(.data$plane_num== 344) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane345 <- coords %>%
    filter(.data$plane_num== 345) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane346 <- coords %>%
    filter(.data$plane_num== 346) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane347 <- coords %>%
    filter(.data$plane_num== 347) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane348 <- coords %>%
    filter(.data$plane_num== 348) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane349 <- coords %>%
    filter(.data$plane_num== 349) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane350 <- coords %>%
    filter(.data$plane_num== 350) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane351 <- coords %>%
    filter(.data$plane_num== 351) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane352 <- coords %>%
    filter(.data$plane_num== 352) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane353 <- coords %>%
    filter(.data$plane_num== 353) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane354 <- coords %>%
    filter(.data$plane_num== 354) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane355 <- coords %>%
    filter(.data$plane_num== 355) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane356 <- coords %>%
    filter(.data$plane_num== 356) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane357 <- coords %>%
    filter(.data$plane_num== 357) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane358 <- coords %>%
    filter(.data$plane_num== 358) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane359 <- coords %>%
    filter(.data$plane_num== 359) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])
  plane360 <- coords %>%
    filter(.data$plane_num== 360) %>%
    select(-.data$plane_num) %>%
    as.list() %>%
    map(~.[[1]])

  ###

  ##### trim mesh with the series of planes #####

  #use the pipe to slice mesh using the series of 359 planes and keep any points above the planes, which are
  # at a near-vertical angle determined by the function arguments
  # for some reason the 1st plane leaves nothing remaining, so there is no use for plane 01.
  # However I will leave it in there just in case.

  clipped_mesh <- mesh %>%
    cutMeshPlane(v1= plane02$v1,
                 v2= plane02$v2,
                 v3= plane02$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane03$v1,
                 v2= plane03$v2,
                 v3= plane03$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane04$v1,
                 v2= plane04$v2,
                 v3= plane04$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane05$v1,
                 v2= plane05$v2,
                 v3= plane05$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane06$v1,
                 v2= plane06$v2,
                 v3= plane06$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane07$v1,
                 v2= plane07$v2,
                 v3= plane07$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane08$v1,
                 v2= plane08$v2,
                 v3= plane08$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane09$v1,
                 v2= plane09$v2,
                 v3= plane09$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane10$v1,
                 v2= plane10$v2,
                 v3= plane10$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane11$v1,
                 v2= plane11$v2,
                 v3= plane11$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane12$v1,
                 v2= plane12$v2,
                 v3= plane12$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane13$v1,
                 v2= plane13$v2,
                 v3= plane13$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane14$v1,
                 v2= plane14$v2,
                 v3= plane14$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane15$v1,
                 v2= plane15$v2,
                 v3= plane15$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane16$v1,
                 v2= plane16$v2,
                 v3= plane16$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane17$v1,
                 v2= plane17$v2,
                 v3= plane17$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane18$v1,
                 v2= plane18$v2,
                 v3= plane18$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane19$v1,
                 v2= plane19$v2,
                 v3= plane19$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane20$v1,
                 v2= plane20$v2,
                 v3= plane20$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane21$v1,
                 v2= plane21$v2,
                 v3= plane21$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane22$v1,
                 v2= plane22$v2,
                 v3= plane22$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane23$v1,
                 v2= plane23$v2,
                 v3= plane23$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane24$v1,
                 v2= plane24$v2,
                 v3= plane24$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane25$v1,
                 v2= plane25$v2,
                 v3= plane25$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane26$v1,
                 v2= plane26$v2,
                 v3= plane26$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane27$v1,
                 v2= plane27$v2,
                 v3= plane27$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane28$v1,
                 v2= plane28$v2,
                 v3= plane28$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane29$v1,
                 v2= plane29$v2,
                 v3= plane29$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane30$v1,
                 v2= plane30$v2,
                 v3= plane30$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane31$v1,
                 v2= plane31$v2,
                 v3= plane31$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane32$v1,
                 v2= plane32$v2,
                 v3= plane32$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane33$v1,
                 v2= plane33$v2,
                 v3= plane33$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane34$v1,
                 v2= plane34$v2,
                 v3= plane34$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane35$v1,
                 v2= plane35$v2,
                 v3= plane35$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane36$v1,
                 v2= plane36$v2,
                 v3= plane36$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane37$v1,
                 v2= plane37$v2,
                 v3= plane37$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane38$v1,
                 v2= plane38$v2,
                 v3= plane38$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane39$v1,
                 v2= plane39$v2,
                 v3= plane39$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane40$v1,
                 v2= plane40$v2,
                 v3= plane40$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane41$v1,
                 v2= plane41$v2,
                 v3= plane41$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane42$v1,
                 v2= plane42$v2,
                 v3= plane42$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane43$v1,
                 v2= plane43$v2,
                 v3= plane43$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane44$v1,
                 v2= plane44$v2,
                 v3= plane44$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane45$v1,
                 v2= plane45$v2,
                 v3= plane45$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane46$v1,
                 v2= plane46$v2,
                 v3= plane46$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane47$v1,
                 v2= plane47$v2,
                 v3= plane47$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane48$v1,
                 v2= plane48$v2,
                 v3= plane48$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane49$v1,
                 v2= plane49$v2,
                 v3= plane49$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane50$v1,
                 v2= plane50$v2,
                 v3= plane50$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane51$v1,
                 v2= plane51$v2,
                 v3= plane51$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane52$v1,
                 v2= plane52$v2,
                 v3= plane52$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane53$v1,
                 v2= plane53$v2,
                 v3= plane53$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane54$v1,
                 v2= plane54$v2,
                 v3= plane54$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane55$v1,
                 v2= plane55$v2,
                 v3= plane55$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane56$v1,
                 v2= plane56$v2,
                 v3= plane56$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane57$v1,
                 v2= plane57$v2,
                 v3= plane57$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane58$v1,
                 v2= plane58$v2,
                 v3= plane58$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane59$v1,
                 v2= plane59$v2,
                 v3= plane59$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane60$v1,
                 v2= plane60$v2,
                 v3= plane60$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane61$v1,
                 v2= plane61$v2,
                 v3= plane61$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane62$v1,
                 v2= plane62$v2,
                 v3= plane62$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane63$v1,
                 v2= plane63$v2,
                 v3= plane63$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane64$v1,
                 v2= plane64$v2,
                 v3= plane64$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane65$v1,
                 v2= plane65$v2,
                 v3= plane65$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane66$v1,
                 v2= plane66$v2,
                 v3= plane66$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane67$v1,
                 v2= plane67$v2,
                 v3= plane67$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane68$v1,
                 v2= plane68$v2,
                 v3= plane68$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane69$v1,
                 v2= plane69$v2,
                 v3= plane69$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane70$v1,
                 v2= plane70$v2,
                 v3= plane70$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane71$v1,
                 v2= plane71$v2,
                 v3= plane71$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane72$v1,
                 v2= plane72$v2,
                 v3= plane72$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane73$v1,
                 v2= plane73$v2,
                 v3= plane73$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane74$v1,
                 v2= plane74$v2,
                 v3= plane74$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane75$v1,
                 v2= plane75$v2,
                 v3= plane75$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane76$v1,
                 v2= plane76$v2,
                 v3= plane76$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane77$v1,
                 v2= plane77$v2,
                 v3= plane77$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane78$v1,
                 v2= plane78$v2,
                 v3= plane78$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane79$v1,
                 v2= plane79$v2,
                 v3= plane79$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane80$v1,
                 v2= plane80$v2,
                 v3= plane80$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane81$v1,
                 v2= plane81$v2,
                 v3= plane81$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane82$v1,
                 v2= plane82$v2,
                 v3= plane82$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane83$v1,
                 v2= plane83$v2,
                 v3= plane83$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane84$v1,
                 v2= plane84$v2,
                 v3= plane84$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane85$v1,
                 v2= plane85$v2,
                 v3= plane85$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane86$v1,
                 v2= plane86$v2,
                 v3= plane86$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane87$v1,
                 v2= plane87$v2,
                 v3= plane87$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane88$v1,
                 v2= plane88$v2,
                 v3= plane88$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane89$v1,
                 v2= plane89$v2,
                 v3= plane89$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane90$v1,
                 v2= plane90$v2,
                 v3= plane90$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane91$v1,
                 v2= plane91$v2,
                 v3= plane91$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane92$v1,
                 v2= plane92$v2,
                 v3= plane92$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane93$v1,
                 v2= plane93$v2,
                 v3= plane93$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane94$v1,
                 v2= plane94$v2,
                 v3= plane94$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane95$v1,
                 v2= plane95$v2,
                 v3= plane95$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane96$v1,
                 v2= plane96$v2,
                 v3= plane96$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane97$v1,
                 v2= plane97$v2,
                 v3= plane97$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane98$v1,
                 v2= plane98$v2,
                 v3= plane98$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane99$v1,
                 v2= plane99$v2,
                 v3= plane99$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane100$v1,
                 v2= plane100$v2,
                 v3= plane100$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane101$v1,
                 v2= plane101$v2,
                 v3= plane101$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane102$v1,
                 v2= plane102$v2,
                 v3= plane102$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane103$v1,
                 v2= plane103$v2,
                 v3= plane103$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane104$v1,
                 v2= plane104$v2,
                 v3= plane104$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane105$v1,
                 v2= plane105$v2,
                 v3= plane105$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane106$v1,
                 v2= plane106$v2,
                 v3= plane106$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane107$v1,
                 v2= plane107$v2,
                 v3= plane107$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane108$v1,
                 v2= plane108$v2,
                 v3= plane108$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane109$v1,
                 v2= plane109$v2,
                 v3= plane109$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane110$v1,
                 v2= plane110$v2,
                 v3= plane110$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane111$v1,
                 v2= plane111$v2,
                 v3= plane111$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane112$v1,
                 v2= plane112$v2,
                 v3= plane112$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane113$v1,
                 v2= plane113$v2,
                 v3= plane113$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane114$v1,
                 v2= plane114$v2,
                 v3= plane114$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane115$v1,
                 v2= plane115$v2,
                 v3= plane115$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane116$v1,
                 v2= plane116$v2,
                 v3= plane116$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane117$v1,
                 v2= plane117$v2,
                 v3= plane117$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane118$v1,
                 v2= plane118$v2,
                 v3= plane118$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane119$v1,
                 v2= plane119$v2,
                 v3= plane119$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane120$v1,
                 v2= plane120$v2,
                 v3= plane120$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane121$v1,
                 v2= plane121$v2,
                 v3= plane121$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane122$v1,
                 v2= plane122$v2,
                 v3= plane122$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane123$v1,
                 v2= plane123$v2,
                 v3= plane123$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane124$v1,
                 v2= plane124$v2,
                 v3= plane124$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane125$v1,
                 v2= plane125$v2,
                 v3= plane125$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane126$v1,
                 v2= plane126$v2,
                 v3= plane126$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane127$v1,
                 v2= plane127$v2,
                 v3= plane127$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane128$v1,
                 v2= plane128$v2,
                 v3= plane128$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane129$v1,
                 v2= plane129$v2,
                 v3= plane129$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane130$v1,
                 v2= plane130$v2,
                 v3= plane130$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane131$v1,
                 v2= plane131$v2,
                 v3= plane131$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane132$v1,
                 v2= plane132$v2,
                 v3= plane132$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane133$v1,
                 v2= plane133$v2,
                 v3= plane133$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane134$v1,
                 v2= plane134$v2,
                 v3= plane134$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane135$v1,
                 v2= plane135$v2,
                 v3= plane135$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane136$v1,
                 v2= plane136$v2,
                 v3= plane136$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane137$v1,
                 v2= plane137$v2,
                 v3= plane137$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane138$v1,
                 v2= plane138$v2,
                 v3= plane138$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane139$v1,
                 v2= plane139$v2,
                 v3= plane139$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane140$v1,
                 v2= plane140$v2,
                 v3= plane140$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane141$v1,
                 v2= plane141$v2,
                 v3= plane141$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane142$v1,
                 v2= plane142$v2,
                 v3= plane142$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane143$v1,
                 v2= plane143$v2,
                 v3= plane143$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane144$v1,
                 v2= plane144$v2,
                 v3= plane144$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane145$v1,
                 v2= plane145$v2,
                 v3= plane145$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane146$v1,
                 v2= plane146$v2,
                 v3= plane146$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane147$v1,
                 v2= plane147$v2,
                 v3= plane147$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane148$v1,
                 v2= plane148$v2,
                 v3= plane148$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane149$v1,
                 v2= plane149$v2,
                 v3= plane149$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane150$v1,
                 v2= plane150$v2,
                 v3= plane150$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane151$v1,
                 v2= plane151$v2,
                 v3= plane151$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane152$v1,
                 v2= plane152$v2,
                 v3= plane152$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane153$v1,
                 v2= plane153$v2,
                 v3= plane153$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane154$v1,
                 v2= plane154$v2,
                 v3= plane154$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane155$v1,
                 v2= plane155$v2,
                 v3= plane155$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane156$v1,
                 v2= plane156$v2,
                 v3= plane156$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane157$v1,
                 v2= plane157$v2,
                 v3= plane157$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane158$v1,
                 v2= plane158$v2,
                 v3= plane158$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane159$v1,
                 v2= plane159$v2,
                 v3= plane159$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane160$v1,
                 v2= plane160$v2,
                 v3= plane160$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane161$v1,
                 v2= plane161$v2,
                 v3= plane161$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane162$v1,
                 v2= plane162$v2,
                 v3= plane162$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane163$v1,
                 v2= plane163$v2,
                 v3= plane163$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane164$v1,
                 v2= plane164$v2,
                 v3= plane164$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane165$v1,
                 v2= plane165$v2,
                 v3= plane165$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane166$v1,
                 v2= plane166$v2,
                 v3= plane166$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane167$v1,
                 v2= plane167$v2,
                 v3= plane167$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane168$v1,
                 v2= plane168$v2,
                 v3= plane168$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane169$v1,
                 v2= plane169$v2,
                 v3= plane169$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane170$v1,
                 v2= plane170$v2,
                 v3= plane170$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane171$v1,
                 v2= plane171$v2,
                 v3= plane171$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane172$v1,
                 v2= plane172$v2,
                 v3= plane172$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane173$v1,
                 v2= plane173$v2,
                 v3= plane173$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane174$v1,
                 v2= plane174$v2,
                 v3= plane174$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane175$v1,
                 v2= plane175$v2,
                 v3= plane175$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane176$v1,
                 v2= plane176$v2,
                 v3= plane176$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane177$v1,
                 v2= plane177$v2,
                 v3= plane177$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane178$v1,
                 v2= plane178$v2,
                 v3= plane178$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane179$v1,
                 v2= plane179$v2,
                 v3= plane179$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane180$v1,
                 v2= plane180$v2,
                 v3= plane180$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane181$v1,
                 v2= plane181$v2,
                 v3= plane181$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane182$v1,
                 v2= plane182$v2,
                 v3= plane182$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane183$v1,
                 v2= plane183$v2,
                 v3= plane183$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane184$v1,
                 v2= plane184$v2,
                 v3= plane184$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane185$v1,
                 v2= plane185$v2,
                 v3= plane185$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane186$v1,
                 v2= plane186$v2,
                 v3= plane186$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane187$v1,
                 v2= plane187$v2,
                 v3= plane187$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane188$v1,
                 v2= plane188$v2,
                 v3= plane188$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane189$v1,
                 v2= plane189$v2,
                 v3= plane189$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane190$v1,
                 v2= plane190$v2,
                 v3= plane190$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane191$v1,
                 v2= plane191$v2,
                 v3= plane191$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane192$v1,
                 v2= plane192$v2,
                 v3= plane192$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane193$v1,
                 v2= plane193$v2,
                 v3= plane193$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane194$v1,
                 v2= plane194$v2,
                 v3= plane194$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane195$v1,
                 v2= plane195$v2,
                 v3= plane195$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane196$v1,
                 v2= plane196$v2,
                 v3= plane196$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane197$v1,
                 v2= plane197$v2,
                 v3= plane197$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane198$v1,
                 v2= plane198$v2,
                 v3= plane198$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane199$v1,
                 v2= plane199$v2,
                 v3= plane199$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane200$v1,
                 v2= plane200$v2,
                 v3= plane200$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane201$v1,
                 v2= plane201$v2,
                 v3= plane201$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane202$v1,
                 v2= plane202$v2,
                 v3= plane202$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane203$v1,
                 v2= plane203$v2,
                 v3= plane203$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane204$v1,
                 v2= plane204$v2,
                 v3= plane204$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane205$v1,
                 v2= plane205$v2,
                 v3= plane205$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane206$v1,
                 v2= plane206$v2,
                 v3= plane206$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane207$v1,
                 v2= plane207$v2,
                 v3= plane207$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane208$v1,
                 v2= plane208$v2,
                 v3= plane208$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane209$v1,
                 v2= plane209$v2,
                 v3= plane209$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane210$v1,
                 v2= plane210$v2,
                 v3= plane210$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane211$v1,
                 v2= plane211$v2,
                 v3= plane211$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane212$v1,
                 v2= plane212$v2,
                 v3= plane212$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane213$v1,
                 v2= plane213$v2,
                 v3= plane213$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane214$v1,
                 v2= plane214$v2,
                 v3= plane214$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane215$v1,
                 v2= plane215$v2,
                 v3= plane215$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane216$v1,
                 v2= plane216$v2,
                 v3= plane216$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane217$v1,
                 v2= plane217$v2,
                 v3= plane217$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane218$v1,
                 v2= plane218$v2,
                 v3= plane218$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane219$v1,
                 v2= plane219$v2,
                 v3= plane219$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane220$v1,
                 v2= plane220$v2,
                 v3= plane220$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane221$v1,
                 v2= plane221$v2,
                 v3= plane221$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane222$v1,
                 v2= plane222$v2,
                 v3= plane222$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane223$v1,
                 v2= plane223$v2,
                 v3= plane223$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane224$v1,
                 v2= plane224$v2,
                 v3= plane224$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane225$v1,
                 v2= plane225$v2,
                 v3= plane225$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane226$v1,
                 v2= plane226$v2,
                 v3= plane226$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane227$v1,
                 v2= plane227$v2,
                 v3= plane227$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane228$v1,
                 v2= plane228$v2,
                 v3= plane228$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane229$v1,
                 v2= plane229$v2,
                 v3= plane229$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane230$v1,
                 v2= plane230$v2,
                 v3= plane230$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane231$v1,
                 v2= plane231$v2,
                 v3= plane231$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane232$v1,
                 v2= plane232$v2,
                 v3= plane232$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane233$v1,
                 v2= plane233$v2,
                 v3= plane233$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane234$v1,
                 v2= plane234$v2,
                 v3= plane234$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane235$v1,
                 v2= plane235$v2,
                 v3= plane235$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane236$v1,
                 v2= plane236$v2,
                 v3= plane236$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane237$v1,
                 v2= plane237$v2,
                 v3= plane237$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane238$v1,
                 v2= plane238$v2,
                 v3= plane238$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane239$v1,
                 v2= plane239$v2,
                 v3= plane239$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane240$v1,
                 v2= plane240$v2,
                 v3= plane240$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane241$v1,
                 v2= plane241$v2,
                 v3= plane241$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane242$v1,
                 v2= plane242$v2,
                 v3= plane242$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane243$v1,
                 v2= plane243$v2,
                 v3= plane243$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane244$v1,
                 v2= plane244$v2,
                 v3= plane244$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane245$v1,
                 v2= plane245$v2,
                 v3= plane245$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane246$v1,
                 v2= plane246$v2,
                 v3= plane246$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane247$v1,
                 v2= plane247$v2,
                 v3= plane247$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane248$v1,
                 v2= plane248$v2,
                 v3= plane248$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane249$v1,
                 v2= plane249$v2,
                 v3= plane249$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane250$v1,
                 v2= plane250$v2,
                 v3= plane250$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane251$v1,
                 v2= plane251$v2,
                 v3= plane251$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane252$v1,
                 v2= plane252$v2,
                 v3= plane252$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane253$v1,
                 v2= plane253$v2,
                 v3= plane253$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane254$v1,
                 v2= plane254$v2,
                 v3= plane254$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane255$v1,
                 v2= plane255$v2,
                 v3= plane255$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane256$v1,
                 v2= plane256$v2,
                 v3= plane256$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane257$v1,
                 v2= plane257$v2,
                 v3= plane257$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane258$v1,
                 v2= plane258$v2,
                 v3= plane258$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane259$v1,
                 v2= plane259$v2,
                 v3= plane259$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane260$v1,
                 v2= plane260$v2,
                 v3= plane260$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane261$v1,
                 v2= plane261$v2,
                 v3= plane261$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane262$v1,
                 v2= plane262$v2,
                 v3= plane262$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane263$v1,
                 v2= plane263$v2,
                 v3= plane263$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane264$v1,
                 v2= plane264$v2,
                 v3= plane264$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane265$v1,
                 v2= plane265$v2,
                 v3= plane265$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane266$v1,
                 v2= plane266$v2,
                 v3= plane266$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane267$v1,
                 v2= plane267$v2,
                 v3= plane267$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane268$v1,
                 v2= plane268$v2,
                 v3= plane268$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane269$v1,
                 v2= plane269$v2,
                 v3= plane269$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane270$v1,
                 v2= plane270$v2,
                 v3= plane270$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane271$v1,
                 v2= plane271$v2,
                 v3= plane271$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane272$v1,
                 v2= plane272$v2,
                 v3= plane272$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane273$v1,
                 v2= plane273$v2,
                 v3= plane273$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane274$v1,
                 v2= plane274$v2,
                 v3= plane274$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane275$v1,
                 v2= plane275$v2,
                 v3= plane275$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane276$v1,
                 v2= plane276$v2,
                 v3= plane276$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane277$v1,
                 v2= plane277$v2,
                 v3= plane277$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane278$v1,
                 v2= plane278$v2,
                 v3= plane278$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane279$v1,
                 v2= plane279$v2,
                 v3= plane279$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane280$v1,
                 v2= plane280$v2,
                 v3= plane280$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane281$v1,
                 v2= plane281$v2,
                 v3= plane281$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane282$v1,
                 v2= plane282$v2,
                 v3= plane282$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane283$v1,
                 v2= plane283$v2,
                 v3= plane283$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane284$v1,
                 v2= plane284$v2,
                 v3= plane284$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane285$v1,
                 v2= plane285$v2,
                 v3= plane285$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane286$v1,
                 v2= plane286$v2,
                 v3= plane286$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane287$v1,
                 v2= plane287$v2,
                 v3= plane287$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane288$v1,
                 v2= plane288$v2,
                 v3= plane288$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane289$v1,
                 v2= plane289$v2,
                 v3= plane289$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane290$v1,
                 v2= plane290$v2,
                 v3= plane290$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane291$v1,
                 v2= plane291$v2,
                 v3= plane291$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane292$v1,
                 v2= plane292$v2,
                 v3= plane292$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane293$v1,
                 v2= plane293$v2,
                 v3= plane293$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane294$v1,
                 v2= plane294$v2,
                 v3= plane294$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane295$v1,
                 v2= plane295$v2,
                 v3= plane295$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane296$v1,
                 v2= plane296$v2,
                 v3= plane296$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane297$v1,
                 v2= plane297$v2,
                 v3= plane297$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane298$v1,
                 v2= plane298$v2,
                 v3= plane298$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane299$v1,
                 v2= plane299$v2,
                 v3= plane299$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane300$v1,
                 v2= plane300$v2,
                 v3= plane300$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane301$v1,
                 v2= plane301$v2,
                 v3= plane301$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane302$v1,
                 v2= plane302$v2,
                 v3= plane302$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane303$v1,
                 v2= plane303$v2,
                 v3= plane303$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane304$v1,
                 v2= plane304$v2,
                 v3= plane304$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane305$v1,
                 v2= plane305$v2,
                 v3= plane305$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane306$v1,
                 v2= plane306$v2,
                 v3= plane306$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane307$v1,
                 v2= plane307$v2,
                 v3= plane307$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane308$v1,
                 v2= plane308$v2,
                 v3= plane308$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane309$v1,
                 v2= plane309$v2,
                 v3= plane309$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane310$v1,
                 v2= plane310$v2,
                 v3= plane310$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane311$v1,
                 v2= plane311$v2,
                 v3= plane311$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane312$v1,
                 v2= plane312$v2,
                 v3= plane312$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane313$v1,
                 v2= plane313$v2,
                 v3= plane313$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane314$v1,
                 v2= plane314$v2,
                 v3= plane314$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane315$v1,
                 v2= plane315$v2,
                 v3= plane315$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane316$v1,
                 v2= plane316$v2,
                 v3= plane316$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane317$v1,
                 v2= plane317$v2,
                 v3= plane317$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane318$v1,
                 v2= plane318$v2,
                 v3= plane318$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane319$v1,
                 v2= plane319$v2,
                 v3= plane319$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane320$v1,
                 v2= plane320$v2,
                 v3= plane320$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane321$v1,
                 v2= plane321$v2,
                 v3= plane321$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane322$v1,
                 v2= plane322$v2,
                 v3= plane322$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane323$v1,
                 v2= plane323$v2,
                 v3= plane323$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane324$v1,
                 v2= plane324$v2,
                 v3= plane324$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane325$v1,
                 v2= plane325$v2,
                 v3= plane325$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane326$v1,
                 v2= plane326$v2,
                 v3= plane326$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane327$v1,
                 v2= plane327$v2,
                 v3= plane327$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane328$v1,
                 v2= plane328$v2,
                 v3= plane328$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane329$v1,
                 v2= plane329$v2,
                 v3= plane329$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane330$v1,
                 v2= plane330$v2,
                 v3= plane330$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane331$v1,
                 v2= plane331$v2,
                 v3= plane331$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane332$v1,
                 v2= plane332$v2,
                 v3= plane332$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane333$v1,
                 v2= plane333$v2,
                 v3= plane333$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane334$v1,
                 v2= plane334$v2,
                 v3= plane334$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane335$v1,
                 v2= plane335$v2,
                 v3= plane335$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane336$v1,
                 v2= plane336$v2,
                 v3= plane336$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane337$v1,
                 v2= plane337$v2,
                 v3= plane337$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane338$v1,
                 v2= plane338$v2,
                 v3= plane338$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane339$v1,
                 v2= plane339$v2,
                 v3= plane339$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane340$v1,
                 v2= plane340$v2,
                 v3= plane340$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane341$v1,
                 v2= plane341$v2,
                 v3= plane341$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane342$v1,
                 v2= plane342$v2,
                 v3= plane342$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane343$v1,
                 v2= plane343$v2,
                 v3= plane343$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane344$v1,
                 v2= plane344$v2,
                 v3= plane344$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane345$v1,
                 v2= plane345$v2,
                 v3= plane345$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane346$v1,
                 v2= plane346$v2,
                 v3= plane346$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane347$v1,
                 v2= plane347$v2,
                 v3= plane347$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane348$v1,
                 v2= plane348$v2,
                 v3= plane348$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane349$v1,
                 v2= plane349$v2,
                 v3= plane349$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane350$v1,
                 v2= plane350$v2,
                 v3= plane350$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane351$v1,
                 v2= plane351$v2,
                 v3= plane351$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane352$v1,
                 v2= plane352$v2,
                 v3= plane352$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane353$v1,
                 v2= plane353$v2,
                 v3= plane353$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane354$v1,
                 v2= plane354$v2,
                 v3= plane354$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane355$v1,
                 v2= plane355$v2,
                 v3= plane355$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane356$v1,
                 v2= plane356$v2,
                 v3= plane356$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane357$v1,
                 v2= plane357$v2,
                 v3= plane357$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane358$v1,
                 v2= plane358$v2,
                 v3= plane358$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane359$v1,
                 v2= plane359$v2,
                 v3= plane359$v3,
                 keep.upper = TRUE) %>%
    cutMeshPlane(v1= plane360$v1,
                 v2= plane360$v2,
                 v3= plane360$v3,
                 keep.upper = TRUE)

  return(clipped_mesh)

}
