
val build_line: float list -> float list -> point list
(** build line builds a alist of points from a list of xs and ys *)

val super_get_y_at_x: point list -> float -> float
(** Gets the y coordinate at a given x, if the x falls
    outside the line gets the y value of the nearest x *)

val difference_between_lines: point list -> point list -> float
(** given two lines - get the maximum difference in y between the two *)

val max_steepness: point list -> float -> float
(** given a line and a sample width, get the steepest section
    of the line over that sample width *)
