# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`gg.layers` is an R package that extends ggplot2 with custom geoms, stats, scales, and utilities focused on scientific visualization — particularly for geospatial/climatological data. It is on CRAN and developed by Dongdong Kong.

## Development Commands

```r
# Load package for interactive development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-GOF.R")

# Check package (equivalent to R CMD check)
devtools::check()

# Rebuild documentation from roxygen2 comments
devtools::document()

# Build pkgdown site
pkgdown::build_site()
```

## Architecture

The package follows standard ggplot2 extension patterns. Almost every file in `R/` exports one or more of:

- **`geom_*` / `Geom*`** — custom ggplot2 layers built with `ggproto`. The `Geom*` ggproto objects live in the same file as their `geom_*` constructor. Key examples:
  - `geom_taylor.R` — Taylor diagram via a fully custom `draw_panel` that draws arcs, ticks, and RMSE curves using grid grobs
  - `geom_signHatch.R` / `stat_signHatch.R` — significance hatching over raster data using sf polygons
  - `geom_signPoint.R`, `stat_signPattern.R` — alternative significance markers
  - `geom_prcpRunoff.R` — dual-axis precipitation/runoff bar+line geom
  - `geom_richtext_npc.R`, `geom_richtext2.R` — NPC-coordinate rich text annotations

- **`stat_*` / `Stat*`** — custom stats. `stat_gof.R` and `stat_gof2.R` wrap `GOF()` to annotate scatter plots with fit statistics. `stat_reg.R` handles regression overlays.

- **`colorbar.R` / `colorbar_*.R`** — standalone colorbar construction via `make_colorbar()`, bypassing ggplot2's legend system; uses lattice-style `draw.colorkey` internally.

- **`GOF.R`** — goodness-of-fit functions (`GOF()`, `KGE()`, `NSE()`, `cv_coef()`, `R2_sign()`) used both standalone and by `stat_gof`.

- **`st_hatched_polygon.R` / `st_point2poly.R`** — sf-based spatial helpers that convert raster grid points to polygons for hatching.

- **`guide_colorsteps2.R`**, **`scale_fill_gradientn2.R`** — extended ggplot2 color scale/guide components.

- **`facet_tag.R`**, **`ggplot_multiaxis.R`**, **`ggplot_legend.R`** — layout/facet utilities.

- **`layer_PosNeg.R`**, **`layer_statistic.R`** — composable layer helpers.

## Key Patterns

- ggproto objects are defined in the same file as the user-facing `geom_*`/`stat_*` wrapper. The naming convention is `GeomFoo` / `StatFoo` for the ggproto, `geom_foo` / `stat_foo` for the constructor.
- `R/backup/` holds deprecated/experimental code that is not exported or tested — do not rely on it.
- `R/temp/` holds in-progress work, also not exported.
- Example scripts in `R/examples/ex-*.R` are referenced via `@example` roxygen tags and are run by `devtools::check()` (unless wrapped in `\dontrun{}`).
- The `reexports.R` file re-exports symbols from dependency packages to expose them to users.
- `tools_ggplot2.R`, `tools_Ipaper.R`, `tools_lattice.R` contain internal helpers; `utilis.R` holds small utilities like `listk()` and `new_data_frame()`.

## Testing

Tests live in `tests/testthat/`. The test suite uses testthat edition 3. Most tests do visual snapshot comparisons or check that geoms render without error — run `devtools::test()` to execute all tests locally.
