0.8.0
=====================
* ggplot2 4.0.0+ compatibility
* Removed `coord_proj()` and `proj4` dependency (was archived on CRAN)
* Removed `plotly` ggplotly integration
* Replaced `alpha()` with `scales::alpha()` for ggplot2 3.5.0+ compatibility
* Renamed `panel_scales` to `panel_params` in all `draw_panel` methods
* Removed `MASS`, `dplyr`, `magrittr` from Imports (moved to Suggests)
* Fixed Rd cross-references to include package anchors
* Fixed `position_dodgev` to work with dplyr 1.1+ (replaced `group_modify` with base R)
* Added R 4.1+ dependency

0.5.0
=====================
* `geom_ubar()` : uniform bar charts based on `geom_segment()`

0.4.0
=====================
* Fixed `coord_proj()`
* Removed pokemon colors (et al)
* Added dotted-gridline guide for `geom\_dumbbell()`
* Complete parameter re-write to `geom\_dumbbell()`
* New geoms & stats by Ben Bolker, Jan Schulz and Carson Sievert
* Ben Marwick also did his best to keep up with my fat-fingering but I'm snuck a few in before the release

0.3.0
=====================
* Added `geom_lollipop()` to make it easer to create lollipop charts

0.2.0
=====================
* Incorporated ProPublica StateFace font
* `geom_encircle()` contributed by Ben Bolker

0.1.5
=====================
* Pokemon discrete color scales!
* `byte_format` (et al) scales (e.g. 10000 => 10 Kb)
