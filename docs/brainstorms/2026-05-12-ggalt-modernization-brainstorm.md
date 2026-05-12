---
name: 2026-05-12-ggalt-modernization-brainstorm
description: Plan for modernizing ggalt R package for ggplot2 3.5.0+ compatibility and CRAN compliance
metadata:
  type: brainstorm
---

# ggalt Modernization Brainstorm

**Date:** 2026-05-12
**Package:** ggalt (version 0.6.1)
**Current ggplot2:** 4.0.3
**Target:** Full modernization for ggplot2 3.5.0+ compatibility

---

## What We're Building

A comprehensively modernized ggalt package that:
1. Fixes all ggplot2 3.5.0+ compatibility issues (alpha deprecation, panel_params naming)
2. Replaces deprecated/archived dependencies (proj4, plyr, extrafont)
3. Updates API conventions (size→linewidth, compute_panel patterns)
4. Achieves clean CRAN submission readiness

---

## Why This Approach

The current ggalt is broken on multiple fronts:
- **proj4** is archived on CRAN (required for coord_proj)
- **plyr** is deprecated (used in position-dodgev.R)
- **extrafont** has loading issues
- **ggplot2 3.5.0+** deprecations not addressed
- **plotly** integration likely broken with ggplot2 4.x

A full modernization addresses root causes rather than patching symptoms.

---

## Key Decisions

### 1. proj4 Replacement Strategy
**Decision:** Replace `proj4` with `sf` package for `coord_proj`

**Why:** proj4 is archived on CRAN and unmaintained. The `sf` package provides the modern R interface to PROJ and is well-maintained.

**Approach:**
- Implement new `CoordSf` based on sf::st_crs() transformations
- Deprecate old coord_proj or provide sf-based alternative
- See PR #69 (eliocamp:new-coord-proj) for prior art

### 2. plyr Deprecation Fix
**Decision:** Replace `plyr::ddply()` with `dplyr` equivalents in position-dodgev.R

**Why:** plyr is deprecated; dplyr is already imported. The transformation is straightforward (group by + rowwise ops).

### 3. alpha() Deprecation Fix
**Decision:** Use `scales::alpha()` instead of ggplot2's deprecated `alpha()`

**Files affected:** annotation_ticks.r, geom_cartogram.r, geom_encircle.r, geom_spikelines.R, stateface.r, annotate_textp.r

**Why:** scales::alpha() is the maintained replacement and works identically.

### 4. panel_scales → panel_params Migration
**Decision:** Update all `panel_scales` parameter names to `panel_params` in draw_panel methods

**Why:** ggplot2 3.5.0+ renamed this parameter. All geoms using the old name will fail.

### 5. size → linewidth Migration
**Decision:** Update any deprecated `size` parameter usage to `linewidth` per ggplot2 3.0+ conventions

**Note:** ggplot2 has already transitioned; ggalt should follow.

### 6. extrafont Handling
**Decision:** Investigate and fix or conditionally load extrafont-dependent code

**Why:** extrafont has loading failures reported (issue #44). StateFace font functionality depends on it.

### 7. compute_group → compute_panel
**Decision:** Review stat classes and update from `compute_group` to `compute_panel` pattern where needed

**Why:** ggplot2's recommended pattern for panel-level computations.

### 8. geom_xspline "size" Parameter Issue
**Decision:** Investigate issue #92 - "size" parameter not recognized in new ggplot2

**Note:** PR #65 (mtorchiano:patch-1) attempted a fix but wasn't merged.

---

## Open Questions

~All questions now resolved based on user input~

---

## Resolved Questions

- **StateFace/extrafont:** Keep and fix (find/fix extrafont loading issue, maintain StateFace functionality)
- **plotly:** Remove ggplotly integration (plotly has evolved, mark as defunct and remove)
- **Tests:** Add basic test coverage for core geoms during modernization
- **Backward compatibility:** No aliases - clean break for future maintainability

---

## Implementation Phases

### Phase 1: ggplot2 3.5.0+ Compatibility
1. Replace `alpha()` with `scales::alpha()` in:
   - annotation_ticks.r
   - geom_cartogram.r
   - geom_encircle.r
   - geom_spikelines.R
   - stateface.r
   - annotate_textp.r

2. Rename `panel_scales` → `panel_params` in draw_panel methods:
   - annotation_ticks.r (line 152)
   - geom_cartogram.r (line 125)
   - geom_encircle.r (line 28)
   - geom_dumbbell.R (line 102)
   - geom_lollipop.r (line 93)
   - geom_ubar.r (line 69)
   - geom_xspline2.r (line 10)
   - stateface.r (line 149)

3. Review/update `size` → `linewidth` conventions

4. Update `compute_group` → `compute_panel` pattern in stats

### Phase 2: Dependency Modernization
1. **proj4 → sf:** Rewrite coord_proj using sf::st_crs() (see PR #69 for prior art)
2. **plyr → dplyr:** Replace ddply() in position-dodgev.R
3. **extrafont:** Fix loading issue for StateFace
4. **Remove plotly:** Mark geom2plotly functions as defunct

### Phase 3: Fix Specific Issues
1. **Issue #92:** Fix geom_xspline size parameter (see PR #65)
2. **Issue #87:** Fix ash1() m and kopt parameters
3. **Issue #85:** Fix geom_cartogram errors
4. Review and merge outstanding helpful PRs

### Phase 4: CRAN Readiness
1. Run `devtools::check()` until clean
2. Add testthat tests for core geoms
3. Update DESCRIPTION with proper dependencies
4. Ensure all examples run without errors