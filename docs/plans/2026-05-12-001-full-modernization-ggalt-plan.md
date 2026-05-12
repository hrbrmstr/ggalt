---
title: Full modernization of ggalt for ggplot2 3.5.0+ compatibility
type: refactor
status: active
date: 2026-05-12
origin: docs/brainstorms/2026-05-12-ggalt-modernization-brainstorm.md
---

# Full Modernization of ggalt for ggplot2 3.5.0+ Compatibility

## Overview

Comprehensive update to the ggalt R package (version 0.6.1) to achieve:
- ggplot2 3.5.0+ compatibility
- Replacement of deprecated/archived dependencies
- Clean CRAN submission readiness
- Basic test coverage

**Origin brainstorm:** [docs/brainstorms/2026-05-12-ggalt-modernization-brainstorm.md](docs/brainstorms/2026-05-12-ggalt-modernization-brainstorm.md)

Key decisions from brainstorm:
- **Scope:** Full modernization (not just critical fixes)
- **Dependency strategy:** Modernize all deps (proj4 → sf, fix extrafont, keep StateFace)
- **plotly:** Remove ggplotly integration
- **Tests:** Add basic test coverage
- **Backward compat:** No aliases (clean break)

---

## Problem Statement

The ggalt package is currently broken/incompatible due to:
1. **ggplot2 3.5.0+ deprecations:** `alpha()` removed, `panel_scales` → `panel_params` rename
2. **Archived CRAN packages:** `proj4` is archived and unmaintained
3. **Deprecated dependencies:** `plyr` deprecated, `extrafont` has loading failures
4. **API drift:** `size` parameter renamed to `linewidth` in ggplot2 3.0+
5. **No tests:** Package has zero real test coverage

---

## Technical Approach

### Dependency Updates

| Current | Replacement | Reason |
|---------|-------------|--------|
| `proj4` | `sf` | proj4 is archived on CRAN; sf is modern PROJ interface |
| `plyr` | `dplyr` (already imported) | plyr deprecated; dplyr already available |
| `extrafont` | Fix/conditional | StateFace requires it; find loading fix |
| `plotly` | REMOVE | plotly's ggplotly() evolved significantly |
| `ash` | Keep | Still on CRAN, needed for StatAsh |

### ggplot2 API Changes Required

**1. alpha() deprecation** (ggplot2 3.5.0)
- Files: annotation_ticks.r, geom_cartogram.r, geom_encircle.r, geom_spikelines.R, stateface.r, annotate_textp.r
- Replace `alpha(fill, alpha)` with `scales::alpha(fill, alpha)` or `fill_alpha()` when building grid grobs

**2. panel_scales → panel_params** (ggplot2 3.5.0 breaking change)
- Files: annotation_ticks.r (152), geom_cartogram.r (125), geom_encircle.r (28), geom_dumbbell.R (102), geom_lollipop.r (93), geom_ubar.r (69), geom_xspline2.r (10), stateface.r (149)
- Rename parameter in all `draw_panel()` methods

**3. draw_panel extra parameters**
- ggplot2 may pass `lineend`, `linejoin` etc. to `draw_panel` - ensure signatures accept via `...` or explicit params

**4. compute_group → compute_panel pattern** (Stats)
- Stats using `compute_group` pattern should be reviewed for `compute_panel` equivalence
- Affects: StatXspline, StatStepribbon, StatAsh, StatBkde, StatBkde2d, StatHorizon

**5. coord_proj rewrite**
- Based on PR #69 (eliocamp:new-coord-proj) - uses sf::st_crs() transformations
- New CoordSf class replacing CoordProj

---

## Implementation Phases

### Phase 1: ggplot2 3.5.0+ Compatibility

#### 1.1 Replace alpha() calls
**Files to modify:**
- `R/annotation_ticks.r` (lines 207, 223, 266, 281)
- `R/geom_cartogram.r` (line 143)
- `R/geom_encircle.r` (lines 2, 176)
- `R/geom_spikelines.R` (lines 88-89)
- `R/stateface.r` (line 176)
- `R/annotate_textp.r` (line 43)

**Pattern change:**
```r
# Old
alpha(colour, alpha)
alpha(data$fill, data$alpha)

# New
scales::alpha(colour, alpha)
scales::alpha(data$fill, data$alpha)
```

#### 1.2 Rename panel_scales → panel_params
**Files to modify** (all with draw_panel methods):
- `R/annotation_ticks.r`
- `R/geom_cartogram.r`
- `R/geom_encircle.r`
- `R/geom_dumbbell.R`
- `R/geom_lollipop.r`
- `R/geom_ubar.r`
- `R/geom_xspline2.r`
- `R/stateface.r`

#### 1.3 Ensure draw_panel accepts ... parameters
Check that all `draw_panel` methods accept `...` to handle extra parameters ggplot2 may pass.

### Phase 2: Dependency Modernization

#### 2.1 proj4 → sf for coord_proj
**Reference:** PR #69 (eliocamp:new-coord-proj) exists but was never merged

**Tasks:**
1. Implement new `CoordSf` class using `sf::st_crs()` and `sf::st_transform()`
2. Either replace `coord_proj` or deprecate it in favor of new coord
3. Test with various projections

**Implementation hint from PR #69:**
```r
CoordSf <- ggproto("CoordSf", Coord,
  distance = function(from, to, panel_params) {
    # Use sf::st_distance()
  },
  transform = function(data, panel_params) {
    # Use sf::st_transform()
  }
)
```

#### 2.2 plyr → dplyr in position-dodgev.R
**File:** `R/position-dodgev.R` (lines 126, 129)

Replace `plyr::ddply()` with dplyr equivalent:
```r
# Old
data <- plyr::ddply(data, "ymin", strategy, ..., height = height)

# New - use dplyr::group_modify or similar
```

#### 2.3 Remove plotly integration
**Files to review:**
- `R/geom2plotly.r` - mark functions as defunct or remove

### Phase 3: Fix Specific Issues

#### Issue #92: geom_xspline size parameter not recognized
- **PR reference:** PR #65 (mtorchiano:patch-1)
- **Task:** Review and merge/apply relevant fix

#### Issue #87: ash1() never sees m and kopt
- **PR reference:** PR #88 (fkohrt:patch-1)
- **Task:** Review and merge/apply fix

#### Issue #85: geom_cartogram errors
- **Task:** Investigate and fix

### Phase 4: extrafont/StateFace Fix

**Files:** `R/stateface.r`

**Options to investigate:**
1. Use `sysfonts` package instead of `extrafont`
2. Conditional loading with warning
3. Bundle StateFace font files directly

**Reference:** Issue #44 - extrafont loading failure

### Phase 5: CRAN Readiness & Tests

#### 5.1 Add test coverage
**Test file:** `tests/testthat/test-ggalt.R` (currently empty placeholder)

**Priority tests:**
- geom_dumbbell basic functionality
- geom_lollipop basic functionality
- geom_xspline basic functionality
- coord_proj (if retained) basic functionality
- stat_stepribbon basic functionality

#### 5.2 Run R CMD check
```bash
Rscript -e "devtools::check(error_on = 'warning')"
```

#### 5.3 Update DESCRIPTION
- Bump version to 0.7.0
- Update ggplot2 dependency to >= 3.5.0
- Update Dependencies list
- Consider adding Suggests: testthat

---

## Files Summary

| File | Changes Needed |
|------|---------------|
| `R/annotation_ticks.r` | alpha() replacement, panel_scales rename |
| `R/geom_cartogram.r` | alpha() replacement, panel_scales rename |
| `R/geom_encircle.r` | alpha() replacement, panel_scales rename, remove debug code |
| `R/geom_spikelines.R` | alpha() replacement, panel_scales rename |
| `R/stateface.r` | alpha() replacement, panel_scales rename |
| `R/annotate_textp.r` | alpha() replacement |
| `R/geom_dumbbell.R` | panel_scales rename |
| `R/geom_lollipop.r` | panel_scales rename |
| `R/geom_ubar.r` | panel_scales rename |
| `R/geom_xspline2.r` | panel_scales rename |
| `R/position-dodgev.R` | plyr → dplyr |
| `R/coord_proj.r` | proj4 → sf (rewrite) |
| `R/geom2plotly.r` | Remove or mark defunct |
| `DESCRIPTION` | Update dependencies, version |
| `NAMESPACE` | Regenerate after roxygen |
| `tests/testthat/test-ggalt.R` | Add actual tests |

---

## Acceptance Criteria

- [x] `devtools::check()` runs with no ERRORs and no WARNINGs (pending: proj4→sf rewrite)
- [x] All alpha() calls replaced with scales::alpha()
- [x] All panel_scales renamed to panel_params in draw_panel methods
- [ ] coord_proj rewritten using sf (or removed if sf-based alternative is complex) - DEFERRED
- [x] plyr::ddply replaced with dplyr equivalent
- [x] plotly ggplotly integration removed
- [x] StateFace/extrafont loading issue resolved
- [x] Basic test coverage added for core geoms
- [x] DESCRIPTION updated with correct dependencies
- [x] Roxygen documentation regenerated

---

## Dependencies & Risks

| Risk | Mitigation |
|------|------------|
| sf::st_transform behavior differs from proj4 | Test extensively with various projections |
| extrafont loading issue may be deep | Investigate sysfonts alternative or conditional loading |
| Breaking backward compatibility | Document changes in NEWS; this is a major version bump |
| CRAN reviewers may have additional feedback | Plan for iteration on CRAN submission |

---

## Sources & References

### Origin
- **Brainstorm:** [docs/brainstorms/2026-05-12-ggalt-modernization-brainstorm.md](docs/brainstorms/2026-05-12-ggalt-modernization-brainstorm.md)

### ggplot2 Documentation
- [Extending ggplot2](https://ggplot2.tidyverse.org/articles/extending-ggplot2.html)
- [ggplot2 3.5.0 NEWS](https://ggplot2.tidyverse.org/news/index.html)
- [Stat reference](https://ggplot2.tidyverse.org/reference/Stat.html)

### Related Issues (GitHub)
- #92: geom_xspline size parameter
- #87: ash1() m and kopt
- #85: geom_cartogram errors
- #44: extrafont loading failure

### Related PRs (to review/merge)
- #90: teunbrand/fix_links - Fix missing package anchors
- #89: fkohrt/patch-1 - Fix m and kopt not passed to ash1()
- #69: eliocamp/new-coord-proj - Updates CoordProj to new API
- #65: mtorchiano/patch-1 - Changed parent prototype for GeomXspline

### CRAN Policies
- [CRAN Package Policies](https://cran.r-project.org/web/packages/policies.html)