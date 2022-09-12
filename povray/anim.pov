#version 3.7;

#include "colors.inc"
#include "stones.inc"
#include "textures.inc"    // pre-defined scene elements
#include "shapes.inc"
#include "glass.inc"
#include "metals.inc"
#include "woods.inc"

global_settings {
        assumed_gamma 1.0
}

camera {
    location <5, 5, 8>
    look_at  <0, 0, 0>
}

union {
  difference {
    Round_Box_Merge( <-3.5,-3.5,-3.5>, <3.5,3.5,3.5>, 0.3)
    Round_Box_Merge( <-3.4,-3.4,-3.4>, <3.4,3.4,3.4>, 0.2)
    cylinder { <-1, -4, 0>, <-1,  4, 0>, 0.6 }
    cylinder { < 1, -4, 0>, < 1,  4, 0>, 0.6 }
    cylinder { < 0,  1,-4>, < 0,  1, 4>, 0.6 }
    cylinder { < 0, -1,-4>, < 0, -1, 4>, 0.6 }
    cylinder { <-4,  0,-1>, < 4,  0,-1>, 0.6 }
    cylinder { <-4,  0, 1>, < 4,  0, 1>, 0.6 }
    material{ M_Glass2 }
  }

#include "animation.pov"
}

light_source { <2, 4, 3> color White}

