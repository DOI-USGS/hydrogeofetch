# hydrogeofetch migration plan

nhdplusTools 1.5.0 is the last feature release under the old name. This plan covers the rename to hydrogeofetch, the deprecation shim, and the eventual CRAN archive of nhdplusTools. Each milestone ends with a gate — review or communication work that should finish before moving on.

## Status (as of 2026-06-22)

**Done:** Milestones 1–4. `hydrogeofetch` v2.0.0 builds and checks clean under its new name, dependencies are modernized (httr2, no fst/tidyr/R.utils/magrittr), every web-service caller degrades gracefully, and the functions that duplicated hydroloom are gone — replaced with a migration vignette, an updated README, and a NEWS.md entry. A fresh reverse-dependency sweep of `amadeus`, `elfgen`, and `StreamCatTools` (2026-06-22) confirmed none of them call a removed function.

**Outstanding:**
- **Milestone 5 (USGS release/DOI)** — not started. `inst/CITATION` has been pre-updated with the `hydrogeofetch` title/author/version text, but the DOI/URL fields are still `"TBD"`; `code.json` still describes `nhdplusTools` entirely; no DOI has been minted on code.usgs.gov yet. **This is the next concrete blocker** — milestones 6 and 7 both depend on identifiers/URLs that come out of this step.
- **Milestone 6 (CRAN submission)** — under review at CRAN.
- **Milestone 6a (reverse-dependency migration)** — not started, blocked on 6. The sweep quoted above is stale: as of 2026-07-31 there are **eight** CRAN reverse deps, not three. `elfgen` and `StreamCatTools` are `Imports`; `amadeus`, `hydroloom`, `ncdfgeom`, `geospatialsuite`, `lakefetch`, `manureshed` are `Suggests`. None calls a removed function.
- **Milestone 7 (GitHub repo rename)** — not started. `origin` is still `doi-usgs/nhdplusTools`.
- **Milestone 8 (deprecation shim)** — not started, and possibly unnecessary; see 6a.
- **Milestone 9 (archive nhdplusTools, Oct 2028)** — blocked on 8; not due for ~2 years.


## 1. Close out nhdplusTools 1.5.0 ✓

*Complete.* nhdplusTools v1.5.0 released on CRAN, tagged and released on both GitHub and GitLab (May 2026). pkgdown site live. README points to the rename plan.


## 2. Build hydrogeofetch v2.0.0 ✓

Rename all internal references so the package installs and loads as `hydrogeofetch`. Function names and signatures stay identical except where the package name is embedded in the function name itself (`nhdplusTools_data_dir`, `nhdplusTools_cache_settings`). New hex sticker is already in place.

### 2a. Package identity ✓

- [x] DESCRIPTION: `Package: hydrogeofetch`, `Title: Hydrologic Geospatial Fabric Extraction Tool Chain`, `Version: 2.0.0`, updated `Description:` (leave `URL:` and `BugReports:` pointing at current repo until milestone 6)
- [x] NEWS.md: added hydrogeofetch 2.0.0 header; nhdplusTools history preserved below
- [x] README.Rmd: updated title, heading, narrative text. Badge URLs, `install.packages()`, `remotes::install_github()` left pointing at nhdplusTools. README.md re-knit.

Deferred to later milestones:
- inst/CITATION: update after DOI is minted (milestone 4)
- code.json: update after USGS software release process begins (milestone 4)
- _pkgdown.yml `url:`: update after repo rename (milestone 6)
- DESCRIPTION `URL:`/`BugReports:`: update after repo rename (milestone 6)
- README badge URLs and install instructions: update after repo rename (milestone 6) and CRAN acceptance (milestone 5)

### 2b. R source — package internals ✓

- [x] Renamed `R/A_nhdplusTools.R` → `R/A_hydrogeofetch.R`
- [x] Internal environment: `nhdplusTools_env` → `hydrogeofetch_env` (all 10 R files)
- [x] Debug env var: `debug_nhdplusTools` → `debug_hydrogeofetch`
- [x] Cache directory: `tools::R_user_dir("nhdplusTools")` → `tools::R_user_dir("hydrogeofetch")`
- [x] Renamed exported functions: `nhdplusTools_data_dir()` → `hydrogeofetch_data_dir()`, `nhdplusTools_cache_settings()` → `hydrogeofetch_cache_settings()`
- [x] Renamed internal functions: `nhdplusTools_memoise_cache()` → `hydrogeofetch_memoise_cache()`, `nhdplusTools_memoise_timeout()` → `hydrogeofetch_memoise_timeout()`

### 2c. Environment variables (user-facing) ✓

Renamed to `HYDROGEOFETCH_MEMOISE_CACHE` and `HYDROGEOFETCH_MEMOISE_TIMEOUT`. Old `NHDPLUSTOOLS_*` names recognized as fallback with a one-time `packageStartupMessage()`. Remove the fallback in v2.0.

### 2d. Mechanical find-replace ✓

- [x] `package = "nhdplusTools"` → `package = "hydrogeofetch"` (50 files)
- [x] `library(nhdplusTools)` → `library(hydrogeofetch)` (8 files)
- [x] `nhdplusTools::` → `hydrogeofetch::` (30 files)
- [x] Roxygen prose and deprecation warning strings updated
- [x] `test_check("nhdplusTools")` → `test_check("hydrogeofetch")`
- [x] Temp directory name strings in inst/extdata updated
- [x] Vignette narrative prose updated (URLs preserved)

### 2e. File renames ✓

- [x] `vignettes/nhdplusTools.Rmd` → `vignettes/hydrogeofetch.Rmd` (VignetteIndexEntry was already generic)
- [x] `tests/testthat/test_nhdplusTools.R` → `tests/testthat/test_hydrogeofetch.R`
- [x] Deleted `man/nhdplusTools_*.Rd` (regenerated by document())
- [x] Deleted `docs/` directory (302 files — pkgdown rebuilt after repo rename)

### 2f. Rebuild and check ✓

- [x] `devtools::document()` — NAMESPACE and man pages regenerated clean
- [x] `devtools::build_readme()` — README.md re-knit
- [x] `devtools::check(vignettes = FALSE)` — 0 errors, 0 warnings, 0 notes (with `--no-examples` and `--no-build-vignettes` to avoid slow network-dependent examples)
- [ ] `pkgdown::build_site()` — deferred until repo rename (milestone 6)

Additional fixes applied during check:
- Stripped UTF-8 BOM from 71 files (introduced by PowerShell bulk edits)
- Changed deprecation messages from `message()` to `packageStartupMessage()` so they are suppressible
- Added once-per-session guard so each deprecation message fires only once
- Disabled parallel testing (`Config/testthat/parallel: false`) — subprocess crashes under parallel fst/arrow usage
- Skipped `test_get_vaa.R` "vaa examples" and `test_02_subset_extras.R` "big rpu test" (R session crashes, deferred to milestone 3 fst → arrow migration)
- Added `skip_if(is.null(...))` to `test_01_get_nldi.R` for NLDI service unavailability
- Added TODO comments for geoconnex TLS cert failures in examples

### 2g. Validate ✓

- [x] `grep -r "nhdplusTools"` in R/, tests/, vignettes/, man/, DESCRIPTION, NAMESPACE — only URLs, deferred install instructions, and DESCRIPTION `URL:`/`BugReports:` remain
- [x] `grep -r "NHDPLUSTOOLS_" R/` — only in env var fallback code
- [x] `library(hydrogeofetch)` loads without errors
- [x] `hydrogeofetch_data_dir()` returns new cache path (`R/data/R/hydrogeofetch`)
- [x] `hydrogeofetch_cache_settings()` returns mode/timeout correctly
- [x] R CMD check: 0 errors, 0 warnings, 0 notes
- [ ] `?hydrogeofetch_data_dir` / `?hydrogeofetch_cache_settings` help pages — verify manually after install
- [ ] `devtools::build_vignettes()` — deferred (slow, network-dependent)
- [ ] `example(get_nldi)` — NLDI pygeoapi returning server-side errors (not rename-related)
- [ ] `testthat::test_local()` in clean session — deferred; known crash issues in test_get_vaa and test_02_subset_extras tracked for milestone 3

**Done when:** ~~all items above checked off, R CMD check clean.~~ Complete. R CMD check passes clean. Remaining unchecked items are blocked by external services or deferred to milestone 3.

**Gate:** Internal review of the renamed package. Share with a few colleagues to install and test before anything goes public.


## 3. Dependency and code modernization ✓

Clean up legacy dependencies and bring the codebase to a consistent modern style.

### 3a. HTTP client ✓

- [x] httr → httr2 migration across all web-service callers
- [x] httptest2 adopted for unit tests (recorded fixtures via `with_mock_hgf`); live integration tests gated behind `skip_if_no_integration()`
- [x] Binary downloads (GeoPackage etc.) stay as live-only tests behind `skip_on_cran()`
- [x] `hgf_json` / `hgf_sf` wrappers at the HTTP boundary catch errors (including TLS failures) and return `NULL` with a warning instead of propagating

### 3b. Dependency cleanup ✓

- [x] fst removed (commit `1a43313`)
- [x] R.utils dropped — single `gunzip(remove = FALSE, skip = TRUE)` call replaced by a local `gunzip_keep()` helper using base-R `gzfile()`/`readBin()`/`writeBin()`
- [x] tidyr dropped — four call sites swapped:
  - `tidyr::unnest()` in `get_index_ids` → base-R `rep()` + `unlist()` against `make_index_ids()$to_list`
  - two `replace_na()` calls (`topo_sort_network`, `add_plus_network_attributes`) → base-R `x[is.na(x)] <- 0`
  - `pivot_wider()` in `rescale_catchment_characteristics` → `data.table::dcast`
  - stale `tidyr::contains` import → dplyr re-export
- [x] data.table added to Imports (replaces tidyr); arrow boundary in [get_vaa.R](R/get_vaa.R) hardened to call `as.data.frame()` on `collect()` / `read_parquet()` output so `[` keeps data.frame semantics when arrow's R metadata roundtrip falls back to data.table (the "Potentially unsafe or invalid elements ... externalptr" warning case)
- [x] maptiles, mapsf moved to Suggests with `check_pkg()` guards at the basemap call in [plot_nhdplus.R](R/plot_nhdplus.R#L165)
- [x] arrow confirmed wired up for VAA caching (`open_dataset`, `read_parquet` in `R/get_vaa.R`) — stays in Imports

### 3c. Code style ✓

- [x] All `%>%` converted to native `|>` — 51 occurrences across 10 R/ files, 32 across 8 test files, 6 in `vignettes/indexing.Rmd`, 7 in `inst/extdata/get_hr_data.R`
- [x] magrittr dropped from Imports; `@importFrom magrittr `%>%`` removed from `R/index_nhdplus.R`
- [x] Modernized formula-style lambdas in files we touched: `~ifelse(. == -9999, ...)` → `\(x) ifelse(x == -9999, ...)` in `R/get_vaa.R`; `~gsub("_rescaled", "", .)` and `~.x*.data$split_area_prop` → `\(x) ...` in `R/rescale_catchments.R`
- [x] One `.` placeholder in a magrittr pipe (`get_hydro.R` site-distance block) rewritten without the placeholder so the native pipe works without needing R 4.2's `_`
- [x] `R CMD check --no-tests` clean: 0 errors, 0 warnings, 0 notes. The pre-existing httptest2 fixture-path NOTE (61 paths >100 chars under `tests/testthat/fixtures/`) was resolved by bundling the fixture tree as `tests/testthat/fixtures.tar.gz` (1.3 MB) and `.Rbuildignore`-ing the loose directory. [helper.R](tests/testthat/helper.R#L59) handles both modes transparently: when the loose tree is present (dev), it is used directly and the tarball is auto-regenerated whenever any fixture is newer than it (so re-record runs never need a manual repack); when the loose tree is absent (R CMD check / CRAN unpack of the source tarball), the tarball is extracted to `tempdir()` on first use. Built source tarball confirmed to have zero paths over 100 chars.

### 3d. Caller hardening (web-service robustness)

Policy is set in CLAUDE.md and the `feedback_web_service_response_handling` memory: keep `\donttest` examples, but every web-service caller must validate response shape strictly and degrade gracefully so CRAN-run `\donttest` examples don't crash on degraded services. HTTP boundary now returns `NULL` on failure; the work left is making the downstream callers handle that `NULL` and unexpected payload shapes cleanly.

- [x] `test_01_get_nldi.R get_nldi_index()` flakiness — resolved by httptest2 mocking (the `skip_if(is.null(...))` carryover at [test_01_get_nldi.R:212](tests/testthat/test_01_get_nldi.R#L212) is a different `xs` test gated by integration skips)
- [x] `get_geoconnex_reference` TLS crash at the HTTP boundary — `hgf_sf`/`hgf_json` now warn + return `NULL` instead of crashing
- [x] `discover_oafeat` / `get_oafeat` at [oafeat_tools.R:164-199](R/oafeat_tools.R#L164-L199) — both `mem_get_json` returns are now `NULL`-checked with a warning + early return; `get_oafeat` short-circuits on `NULL avail` instead of falling through to a misleading "Type must be in available ids: ." `stop()`.
- [x] Methodical audit of downstream callers complete. Concrete crash sites and shape-inconsistencies fixed:
  - `hgf_download` ([downloading_tools.R:394-405](R/downloading_tools.R#L394-L405)) was the only HTTP-boundary helper without a tryCatch — now matches `hgf_json`/`hgf_sf` (warn + `NULL` on transport failure). Callers at [downloading_tools.R:127,186,240](R/downloading_tools.R#L127) and [get_vaa.R:188](R/get_vaa.R#L188) now guard with `if(is.null(hgf_download(...))) return(NULL)` so the next-step unzip/7z/`read_sf` doesn't run on a missing file. The [get_vaa.R:275](R/get_vaa.R#L275) site already has an enclosing `tryCatch` that converts the failure cleanly.
  - `query_usgs_arcrest` ([arcrest_tools.R:132](R/arcrest_tools.R#L132)) — `all_ids` is now declared `NULL` before the `tryCatch` so a future change to `hgf_json` that re-introduces throwing can't leave the variable unbound. Inert `out <- NULL` in the handler removed.
  - `discover_nhdplus_id` ([discover_nhdplus.R:54-77](R/discover_nhdplus.R#L54-L77)) — failure mode normalized from `integer(0)` (via `as.integer(NULL)`) to explicit `NULL`. Caller `is.null()` checks at [get_nhdplus.R:65](R/get_nhdplus.R#L65) and elsewhere now actually catch degraded service.
  - `extract_comid_nwis` ([get_nhdplus.R:145-153](R/get_nhdplus.R#L145-L153)) — `hgf_json` returning `NULL` no longer silently disappears into the caller's `unlist`; now warns with the NWIS id so failed lookups are visible. (Memoise caching of `NULL` accepted as-is — short timeout makes natural re-attempt cheap.)

### 3e. Test infrastructure ✓

- [x] `Config/testthat/parallel: true` re-enabled at [DESCRIPTION:36](DESCRIPTION#L36). Full `devtools::test()` run completed clean — no R subprocess crashes, no test failures, only pre-existing warnings (tidyselect bare-variable deprecation in `arrow::read_parquet(col_select = include_names)` inside `R/get_vaa.R` — known, not parallel-related) and the two `skip_if(...)` skips from milestone 3a. Confirms fst removal resolved the parallel/fst+arrow segfault.

### 3f. Vignettes ✓

- [x] `tools::buildVignettes(dir = '.')` run with `BUILD_VIGNETTES` unset — all 7 vignettes build clean (no errors, no warnings). This is the CRAN path: every vignette uses `eval = local` gated on `Sys.getenv("BUILD_VIGNETTES") == "TRUE"`, so on CRAN no chunk evaluates and no service is contacted. Confirms that flaky or unavailable services cannot break the CRAN vignette build.
- [x] Two live-build failures (`BUILD_VIGNETTES=TRUE`) surfaced and fixed:
  - `get_nhdplus()` ([R/get_nhdplus.R:131](R/get_nhdplus.R#L131)) returned `list()` instead of `NULL` when every realization's `query_usgs_oafeat()` returned `NULL` (the milestone-3a hardened-HTTP signature). Normalized the return contract — empty result now returns `NULL`, matching the milestone-3d caller-hardening convention. `@return` doc updated.
  - The `drainage_area_estimation.Rmd` chunk `[french_broad_boundaries]` failure was a direct downstream symptom: `all_catchments` was stored as a `list()` and `st_transform()` blew up. With the `NULL` contract in place, the existing `if(!is.null(da$all_catchments))` guard in [plot_boundaries](vignettes/drainage_area_estimation.Rmd#L678) drops that layer cleanly. Live render of the vignette now succeeds end-to-end. (Stale cached `da_est_french_broad.rds` had to be patched to `all_catchments = NULL` to simulate the new contract; re-fetching would produce the same result naturally.)
  - Hardened the two remaining unguarded `get_nhdplus()` call sites in [get_drainage_area_estimates.R](R/get_drainage_area_estimates.R): line 699 (`all_net`) now `stop()`s with a clear message when the upstream network fetch fails; line 1679 (`extra_cat`) now warns and falls back to an empty `sf` so scalar estimates still flow.
  - `get_data_overview.Rmd` WBD chunk: fixed the `length(wbd_out == 0)` typo, removed the brittle `list.files(..., pattern = ".gdb")` re-discovery in favor of using `download_wbd()`'s return value, and added a graceful skip when the service is unavailable.

**Done when:** No remaining httr, magrittr, R.utils, or tidyr usage. arrow wired up for VAA caching or removed. Tests pass in both mock and live modes. Vignettes build cleanly. Every web-service caller validates response shape strictly and degrades gracefully so `\donttest` examples remain CRAN-safe under failing services. R CMD check clean. Parallel testing re-enabled with no subprocess crashes.

**Gate:** Run the full test suite live to confirm service compatibility. Review httptest2 fixture sizes — if any are too large to check in, simplify or move those tests to live-only.

### Deferred: Lazy OGC API discovery

`discover_oafeat` ([oafeat_tools.R:164](R/oafeat_tools.R#L164)) eagerly fetches queryables for every collection (~30 requests) even though each caller needs only one. Make queryables fetching lazy — fetch only for the collection actually requested. Cuts ~30 HTTP round-trips to 3 (landing, collections, one queryables), shrinks httptest2 fixture directories from ~35 files to ~5, and reduces fragility when the API adds or renames collections.


## 4. Reduce hydroloom duplication ✓

hydrogeofetch should be a data-fetching and NHD-specific preparation package. Network analysis and spatial indexing belong in hydroloom. Functions that are thin wrappers over hydroloom — adding only column renames or trivial glue — should be removed so users call hydroloom directly.

### 4a. Drop re-exports ✓

Remove the 7 pure re-export *exports* from `R/A_hydrogeofetch.R` (the `@export` + `hydroloom::fn` lines, ~585–602). These pass through hydroloom functions with zero added logic:

- `st_compatibalize`
- `rename_geometry`
- `get_node`
- `fix_flowdir`
- `rescale_measures`
- `get_hydro_location`
- `get_partial_length`

Users who need these call `hydroloom::fn()` or `library(hydroloom)`.

**Keep four as plain imports, not exports.** Retained internal code calls these as bare names, and the re-export block is currently the only thing that imports them into the namespace:

- `get_hydro_location` — `get_drainage_area_estimates.R` (lines 386, 625)
- `rescale_measures` — `get_drainage_area_estimates.R` (line 593)
- `rename_geometry` — `get_drainage_area_estimates.R` (lines 895, 951)
- `get_node` — `get_nhdplus.R` (line 118)

When the re-export block is deleted, replace it with a plain roxygen import (no `@export`) so these still resolve:

```r
#' @importFrom hydroloom get_node get_hydro_location rescale_measures rename_geometry
NULL
```

Without this, `R CMD check` raises "no visible global function definition" and the functions error at runtime. `st_compatibalize`, `fix_flowdir`, and `get_partial_length` have no internal callers and can be dropped completely.

### 4b. Drop indexing functions ✓

Remove `R/index_nhdplus.R` entirely:

- `get_flowline_index` — wraps `hydroloom::index_points_to_lines` with optional auto-download and column rename
- `disambiguate_flowline_indexes` — renames `id` ↔ `point_id` around `hydroloom::disambiguate_indexes`
- `get_waterbody_index` — literally just `index_points_to_waterbodies(...)`

The only internal caller is `navigate_network` (see 4d below).

### 4c. Drop navigation shorthands ✓

Remove `get_UT`, `get_UM`, `get_DM`, `get_DD` from `R/get_network.R`. `get_UT` and `get_DD` are true one-liners over `hydroloom::navigate_hydro_network`. `get_UM` and `get_DM` are *not* — they call `align_nhdplus_names`, filter the network, add `sort`/`include` arguments, and return the `COMID` column rather than the raw navigation result. Users who only want the COMID set call hydroloom directly with the mode string; the `sort`/`include` behavior has to be reproduced by hand (and is preserved in the shim — see 4i).

Removing all four empties `R/get_network.R` (it also holds `navigate_network`, removed in 4d), so delete the file.

Internal callers to fix:
- `plot_nhdplus.R` (2 sites: lines 418, 475) — replace `get_UT(align_nhdplus_names(flowline), x)` with `navigate_hydro_network(align_nhdplus_names(flowline), x, "UT")`. The `navigate_hydro_network` import survives the `get_network.R` deletion because `get_drainage_area_estimates.R` also declares it.

### 4d. Drop `navigate_network` ✓

Remove the `navigate_network` function. It stitches together NLDI calls, navigation, indexing, and trimming — workflow glue that belongs in a vignette example, not a function. It's the main internal consumer of both indexing and navigation shorthands.

### 4e. Drop remaining thin wrappers ✓

Remove these exported functions that add only column renames around a single hydroloom call:

- `get_sorted` — rename cols → `sort_network` → rename back
- `get_streamorder` — `check_names` + `add_streamorder()$stream_order`
- `get_streamlevel` — `check_names` + `add_streamlevel()$stream_level`
- `get_pathlength` — rename cols → `add_pathlength` → rename back

Internal callers to fix:
- `subset_nhdplus.R` line 683 — call `sort_network` directly. `sort_network` takes `id`/`toid`, the call site passes `fline[c("comid", "tocomid")]`, and the next line reads `network$comid`. `get_sorted` did the rename round-trip internally, so the replacement must rename in and read the renamed column out:

  ```r
  network <- sort_network(
    stats::setNames(fline[c("comid", "tocomid")], c("id", "toid")),
    split = FALSE, outlets = outlets$comid)
  network$id
  ```

  Add `#' @importFrom hydroloom sort_network` to `subset_nhdplus.R` — that tag currently lives only in `get_paths.R` (line 98), which is deleted below.

Note: `topo_sort_network`, `get_index_ids`, and `get_fromids` (all internal, `@noRd`, in `get_paths.R`) have **no callers** anywhere in `R/`, `tests/`, or `vignettes/`. They are dead helpers, so there is nothing to rewrite — they are removed along with `get_sorted`. With `get_levelpaths`/`get_terminal` (4f) also gone, `R/get_paths.R` is empty and should be deleted. Their hydroloom imports (`make_index_ids`, `make_fromids`) drop out with them. (Confirm these aren't intentional scaffolding for planned work before deleting.)

### 4f. Drop all already-deprecated wrappers ✓

Remove these functions that already emit deprecation warnings and just forward to hydroloom:

- `get_levelpaths` → `hydroloom::add_levelpaths`
- `get_pfaf` → `hydroloom::add_pfafstetter`
- `get_terminal` → `sort_network` (via `get_sorted`)
- `calculate_total_drainage_area` → `hydroloom::accumulate_downstream`
- `calculate_arbolate_sum` → `hydroloom::accumulate_downstream`
- `get_path_members` / `get_path_lengths` → `hydroloom::navigate_connected_paths`
- `make_node_topology` → `hydroloom::make_node_topology`
- `add_plus_network_attributes` → multiple hydroloom fns

This removes `R/calc_network.R`, `R/get_path_lengths.R`, `R/rebuild_topology.R`, and `R/run_plus_attributes.R`. Combined with the thin-wrapper and dead-helper removals in 4e, `R/get_paths.R` and `R/get_codes.R` are also emptied and deleted (`get_paths.R`: `get_levelpaths`/`get_levelpaths_internal`, `get_terminal`, `get_sorted`, dead helpers; `get_codes.R`: `get_pfaf`, `get_streamorder`, `get_streamlevel`). `get_levelpaths_internal` and `get_paths_internal` are removed here too — their only callers (`add_plus_network_attributes`, `get_levelpaths`, `get_path_members`/`get_path_lengths`) are all in this set.

### 4g. Fix the `:::` call ✓

`hydroloom:::check_search_radius` in `index_nhdplus.R` uses an unexported hydroloom function. Since we're removing `get_flowline_index`, this goes away automatically. If any remaining code needs this logic, copy the few lines of unit-conversion rather than depending on an unexported function.

### 4h. What stays

Functions that have real NHD-specific logic beyond what hydroloom provides:

- **`get_tocomid`** — NHD-specific coastal filtering, divergence-to-headwater handling, terminalpa grouping around `add_toids`
- **`prepare_nhdplus`** — substantial NHD-specific filtering (network size, path length, dendritic purging), calls `get_tocomid`
- **`align_nhdplus_names`** — NHD column name translation (bridge between NHD names and hydroloom's expected names)

Internal hydroloom imports that stay (used as implementation details by functions above):
- `add_toids` (via `get_tocomid`)
- `navigate_hydro_network` (via `plot_nhdplus` after rewrite, and `get_drainage_area_estimates`)
- `navigate_network_dfs` (via `get_nhdplushr`, `get_drainage_area_estimates`)
- `index_points_to_lines` (via `get_drainage_area_estimates` — direct call, no wrapper)
- `accumulate_downstream` (if needed internally by retained code)
- `sort_network` (via `subset_nhdplus` after rewrite)
- `check_valid` (via `subset_nhdplus`, `oafeat_tools`, `arcrest_tools`)
- `get_node`, `get_hydro_location`, `rescale_measures`, `rename_geometry` (formerly re-exported; now plain imports — see 4a — used by `get_nhdplus` and `get_drainage_area_estimates`)

`make_fromids` and `make_index_ids` do **not** stay — their only users (`get_fromids`, `get_index_ids`) are dead helpers removed in 4e.

### 4i. Shim implications

The nhdplusTools shim (milestone 8) needs to forward the removed functions somewhere. For functions that mapped directly to hydroloom, the shim forwards to `hydroloom::*` directly rather than routing through hydrogeofetch. The shim's `get_UT` calls `hydroloom::navigate_hydro_network(..., "UT")`, etc. This removes the two-level indirection problem.

Exception: `get_UM` and `get_DM` are not bare forwards (see 4c). Their shim versions must carry the wrapper body — `align_nhdplus_names`, the network filter, and the `sort`/`include` arguments returning `$COMID` — so existing `get_UM(net, comid, sort = TRUE)` calls keep working. Copy the current function bodies into the shim rather than forwarding.

### 4j. Verification ✓

1. `devtools::document()` — NAMESPACE regenerates without the removed exports
2. `devtools::check()` — 0 errors, 0 warnings
3. `devtools::test()` — remove or relocate tests for dropped functions:
   - **Delete** (dedicated to removed functions): `test_get_network.R`, `test_index.R`, `test_navigate.R`, `test_calc_network.R`, `test_get_path.R`, `test_get_path_lengths.R`, `test_rebuild_topology.R`, `test_run_plus_attributes.R`, `test_wb_index.R`, `test_get_codes.R`, `test_get_geometry.R` (re-exports).
   - **Rewrite, don't delete**: `test_02_subset.R`, `test_get_nhdplushr.R`, `test_00_plot_nhdplus.R`, `test_00_plot_local_nhdplus.R`, `test_get_drainage_area_estimates.R` use `get_UT` only to build input COMID lists — swap to `navigate_hydro_network(..., "UT")`.
4. Grep: `grep -r "get_UT\|get_UM\|get_DM\|get_DD\|get_flowline_index\|get_waterbody_index\|get_sorted\|get_streamorder\|get_streamlevel" R/` returns zero hits outside of retained internal uses
5. NAMESPACE contains no `hydroloom::` re-exports (but retains the plain `importFrom` for `get_node`/`get_hydro_location`/`rescale_measures`/`rename_geometry`)
6. No `:::` calls remain
7. `R CMD check`'s "checking R code for possible problems" pass shows no "no visible global function definition" notes — the canary for a dropped import (4a)
8. Prune `_pkgdown.yml` reference sections: ~28 entries point at the removed functions and will be dangling man topics. `pkgdown::build_site()` (deferred to milestone 6) errors on missing topics, so either prune now or carry an explicit note into milestone 6's gate.

### 4k. Sequencing ✓

1. **Commit 1**: Remove the re-export *exports* but keep the four plain imports (4a). Remove `R/index_nhdplus.R` and `navigate_network` (4b, 4d) — `navigate_network` is the consumer of the indexing and navigation shorthands, so it goes before them.
2. **Commit 2**: Remove navigation shorthands and delete `R/get_network.R`. Fix `plot_nhdplus.R` internal calls (4c).
3. **Commit 3**: Remove all deprecated wrappers and their source files (`calc_network.R`, `get_path_lengths.R`, `rebuild_topology.R`, `run_plus_attributes.R`, plus `get_pfaf`/`get_terminal`/`get_levelpaths` and the internal helpers) (4f). **This must precede the thin-wrapper removal** — `add_plus_network_attributes` depends on `get_sorted`, `get_pathlength`, and `get_levelpaths_internal`, so removing those first would break the intermediate build.
4. **Commit 4**: Remove the remaining thin wrappers `get_sorted`, `get_streamorder`, `get_streamlevel`, `get_pathlength`; delete the now-empty `get_paths.R`/`get_codes.R`. Rewrite the `subset_nhdplus.R` caller to use `sort_network` directly (4e).
5. **Commit 5**: Update tests (4j step 3), scale back vignettes (4l, 4m), examples, NEWS.md, and the `_pkgdown.yml` reference index. NEWS: add a new entry listing the removed functions and their hydroloom replacements; do not rewrite the historical nhdplusTools entries (they document shipped versions).

Each commit leaves R CMD check clean. (Commits 3 and 4 are deliberately ordered deprecated-before-thin so no intermediate state references a removed function.)

### 4l. Migration vignette ✓

Write `vignettes/articles/migrating_from_nhdplusTools.Rmd` — a concise guide for users switching from nhdplusTools.

Contents:

1. **What moved where** — a lookup table grouped by category (indexing, navigation, network attributes, re-exports). Two columns: old nhdplusTools call → replacement call (`hydroloom::fn()` or `hydrogeofetch::fn()`).
2. **Why the split** — one paragraph. hydrogeofetch fetches and prepares NHD data; hydroloom does network analysis and spatial indexing. Users who did both now load both.
3. **The three migration patterns** — before/after code blocks:
   - Functions that moved to hydroloom with a changed signature (e.g. `get_UT(net, 101)` → `hydroloom::navigate_hydro_network(net, 101, "UT")`). For `get_UM`/`get_DM`, note the extra behavior: the old wrappers ran `align_nhdplus_names`, filtered, and returned the sorted/included `COMID` vector, so a bare `navigate_hydro_network(..., "UM")` is not equivalent — show how to reproduce `sort`/`include` if a user relied on them.
   - Pure re-exports that just need the package qualifier (e.g. `st_compatibalize` → `hydroloom::st_compatibalize`)
   - Thin wrappers that had column-name magic — show the hydroloom call plus `align_nhdplus_names()` if needed
4. **The deprecation shim** — `install.packages("nhdplusTools")` still works during the sunset period, gives deprecation warnings naming the replacement, and forwards to the right place.
5. **Environment variables** — `NHDPLUSTOOLS_*` still recognized as fallback; `HYDROGEOFETCH_*` is the new name.

Skip internal refactoring details, exhaustive hydroloom API docs (link to hydroloom's site), and history of the rename decision.

### 4m. Scale back existing vignettes ✓

Four existing article vignettes document functionality that moved to hydroloom and will fail to build once the functions are gone. Scale them back: remove the moved-function material, and where a retained example still needs that behavior, call the hydroloom function directly rather than a re-export.

- `vignettes/articles/indexing.Rmd` — the entire vignette is built on `get_flowline_index`, `get_waterbody_index`, `disambiguate_flowline_indexes`, `get_path_lengths`, and `get_partial_length`. This is now hydroloom's territory. Cut it down to a short pointer that indexing lives in hydroloom (link to hydroloom's indexing vignette), keeping only any hydrogeofetch-specific framing (e.g. fetching the flowlines to index with `get_nhdplus()`, then handing off to `hydroloom::index_points_to_lines()`). Do not maintain a parallel indexing tutorial here.
- `vignettes/hydrogeofetch.Rmd` (main intro) — the navigation walk-through uses `navigate_network`, `get_UM`/`get_UT`/`get_DM`/`get_DD`, and `get_flowline_index`. Replace the navigation section with the fetch-then-hand-off pattern: use hydrogeofetch to get/prepare the network, then `hydroloom::navigate_hydro_network()` for navigation and `hydroloom::index_points_to_lines()` for indexing. Drop the `navigate_network()` mention.
- `vignettes/articles/get_data_overview.Rmd` — uses `navigate_network`, `get_DM`, `get_flowline_index`, `disambiguate_flowline_indexes`. Replace the navigation/indexing snippets with direct hydroloom calls; keep the data-overview/fetching content that is the point of the vignette.
- `vignettes/articles/nhdplushr.Rmd` — uses `get_flowline_index` and `get_UT`. Swap to `hydroloom::index_points_to_lines()` and `hydroloom::navigate_hydro_network(..., "UT")`.

Where these examples currently lean on re-exported helpers (`get_node`, `get_hydro_location`, `rescale_measures`, `rename_geometry`, etc.), call `hydroloom::fn()` directly so the vignettes don't depend on imports that are no longer user-facing.

**Done when:** ~~hydrogeofetch exports only data-fetching, NHD-specific preparation, and visualization functions. No thin hydroloom wrappers remain. R CMD check clean.~~ Complete. Verified directly against the repo: all target files and functions removed, zero `:::` calls remain, NAMESPACE carries no `hydroloom::` re-exports (only the plain `importFrom`), and `_pkgdown.yml` has zero dangling references to removed functions.

**Gate:** ~~Update vignettes and README to show users how to combine hydrogeofetch (for fetching) with hydroloom (for network analysis/indexing). Confirm reverse-dep packages don't import the removed functions from hydrogeofetch directly.~~ Complete. README.Rmd and `vignettes/hydrogeofetch.Rmd`/`migrating_from_nhdplusTools.Rmd` document the fetch-then-hand-off pattern. Reverse-dependency sweep (2026-06-22, cloned each repo and grepped its source): `amadeus` uses only `get_huc`, `elfgen` uses only `get_nhdplus`, `StreamCatTools` uses `get_waterbodies`/`navigate_nldi`/`get_nldi_basin`/`get_nhdplus`/`discover_nhdplus_id` — none call a function removed in this milestone.


## 5. USGS software release

Create a new entry on code.usgs.gov for hydrogeofetch. Mint a new DOI. The existing nhdplusTools DOI stays pointed at a historical release page.

**Status:** Not started — no DOI minted yet. `inst/CITATION` title/author/version/year text has already been updated to `hydrogeofetch` 2.0.0, but `url`/`textVersion` are placeholder `"TBD"` pending the DOI. `code.json` still describes `nhdplusTools` (name, version history) and has not been touched. **This is the current blocker** — milestones 6 and 7 depend on identifiers and URLs that come out of this step.

**Done when:** DOI assigned for hydrogeofetch.

**Gate:** Update the items deferred from milestone 2:
- inst/CITATION: `url`/`textVersion` DOI fields (title/author/version/year already done)
- code.json: `name`, URLs, version entries
- DESCRIPTION: DOI in any relevant fields
Confirm the plan for the old nhdplusTools DOI redirect with the DOI coordinator.


## 6. hydrogeofetch on CRAN

Submit hydrogeofetch v1.0 to CRAN. The repo is still named nhdplusTools at this point — that's fine, CRAN only cares about the package tarball.

**Pre-submission checklist:**
- `R CMD check --as-cran` clean (0 errors, 0 warnings, ideally 0 notes)
- `devtools::build_vignettes()` succeeds — vignettes render without network failures
- All `\donttest` examples fail gracefully under degraded service conditions (CRAN runs `\donttest` examples) — strict response-type checking in every web-service caller, not `\dontrun` conversions
- Help pages for renamed functions (`?hydrogeofetch_data_dir`, `?hydrogeofetch_cache_settings`) render correctly

**Release workflow:** Push an `rc/2.0.0` branch to code.usgs.gov. The GitLab CI pipeline runs a lightweight structural check, builds the source tarball, uploads it to the GitLab package registry, and runs `R CMD check --as-cran` on that tarball. Download the verified tarball from the registry and submit it to CRAN. See `.gitlab-ci.yml` and the README "Build and release" section for details.

**Done when:** Package accepted and available on CRAN.

**Gate:** Update README.Rmd `install.packages()` line to `install.packages("hydrogeofetch")`. Announce hydrogeofetch v1.0. Open the reverse-dependency PRs in milestone 6a.


## 6a. Reverse-dependency migration

CRAN rejects a package naming a dependency that isn't on a mainstream repo — including in `Suggests` — so nothing here can move until milestone 6 lands. Once it does, all eight of these are name substitutions: every function any reverse dep calls is still exported from hydrogeofetch 2.0.0 with an unchanged signature. Verified by diffing `nhdplusTools-1.5:NAMESPACE` against current (2026-07-31); none of the eight touches a function removed in milestone 4.

If all eight migrate, nhdplusTools has no CRAN reverse dependencies left and milestone 8 (the shim) becomes optional — nhdplusTools could go straight to the milestone 9 archive.

**Strong dependencies — do these first.** Under a shim these emit `.Deprecated()` on every call into their users' code.

- [ ] **elfgen** (HARPgroup, agreed to a PR) — `Imports`. Only call is `nhdplusTools::get_nhdplus()` at `R/elfdata.R:136`. Also drop the `@import nhdplusTools` roxygen tag at `R/elfdata.R:19` — it pulls the whole namespace in for a call that's already `::`-qualified, and under a shim it would attach ~30 deprecated aliases. Swap DESCRIPTION `Imports`, re-roxygenize.
- [ ] **StreamCatTools** (USEPA, agreed to a PR) — `Imports`. All `::`-qualified, no NAMESPACE directive. `get_waterbodies` (`R/lc_get_comid.R:59,61`), `discover_nhdplus_id` (`R/sc_get_comid.R:60`), `get_nhdplus` (`R/sc_plot.R:208`), plus `navigate_nldi`/`get_nldi_basin`/`get_waterbodies` in `vignettes/Articles/{Applications,LakeCat,NNI}.Rmd`. Swap DESCRIPTION `Imports`. Note for Marc: hydrogeofetch `Suggests: StreamCatTools` for the `source = "streamcat"` path in `get_catchment_characteristics()`, so merging this creates a Suggests↔Imports cycle. CRAN permits it, but it should not be a surprise in a check log.

**Own packages — no coordination needed.**

- [ ] **hydroloom** — `Suggests`. Largest diff of the eight, ~30 sites across `@examples`, tests, and vignettes. Two things beyond renaming: `nhdplusTools_data_dir()` → `hydrogeofetch_data_dir()` (`tests/testthat/test_add_pfafstetter.R:5`, `vignettes/flow-table.Rmd:18,42,108`), and `vignettes/advanced_network.Rmd:214` describes `add_plus_network_attributes` as an nhdplusTools function, which no longer exists. Six tests and four examples also `source(system.file("extdata/...", package = "nhdplusTools"))` — all four of those files (`sample_data.R`, `sample_flines.R`, `nhdplushr_data.R`, `3dhp_yahara_flowlines.R`) still ship in hydrogeofetch, only the `package =` argument changes.
- [ ] **ncdfgeom** — `Suggests`. One vignette: `get_nldi_basin`, `get_huc` at `vignettes/polygon_intersection.Rmd:23,48-50`.

**Third-party Suggests — all `requireNamespace`-guarded, none urgent.**

- [ ] **amadeus** (NIEHS) — one call site, `nhdplusTools::get_huc()` at `R/process.R:4201` behind a `requireNamespace` guard at `:4193`. Also `@param`/`@seealso` doc lines at `R/process.R:4147,4149` and two `withr::local_package("nhdplusTools")` calls in `tests/testthat/test-huc.R:118,190`.
- [ ] **geospatialsuite** — `get_huc` at `R/15-helpers.R:1038`, guard and install message at `:1015-1017`. Not yet contacted.
- [ ] **lakefetch** — `get_waterbodies`, `get_nhdplus`, `discover_nhdplus_id`, `get_nldi_basin` in `R/nhd_integration.R`; availability check in `R/globals.R:162`; startup message in `R/zzz.R:84-87`. Not yet contacted.
- [ ] **manureshed** — vignette only (`vignettes/advanced-features.Rmd:487-490`) plus a metadata string in `R/package_documentation.R:340`. Pins `nhdplusTools (>= 0.5.0)`; that bound should be dropped or rewritten to `hydrogeofetch (>= 2.0.0)`. Not yet contacted.

**Also fix in hydrogeofetch itself:** `nhdplusTools_data_dir()` and `nhdplusTools_cache_settings()` were *renamed*, not removed. NEWS.md should list them as renames rather than leaving them implicit in the removed-function section.

**Done when:** All eight merged and released, and `tools::package_dependencies("nhdplusTools", reverse = TRUE)` against CRAN returns empty.

**Gate:** Re-run the reverse-dependency sweep before declaring this done — the list grew from three to eight between 2026-06-22 and 2026-07-31, so assume it has grown again.


## 7. Rename the GitHub repo

Rename doi-usgs/nhdplusTools to doi-usgs/hydrogeofetch. GitHub maintains redirects from the old URL. Requires org admin access — this milestone can move earlier if access becomes available.

**Done when:** The repo is accessible at doi-usgs/hydrogeofetch and old nhdplusTools URLs redirect.

**Gate:** Verify redirects work for clone URLs, issue links, and pkgdown. Then update the items deferred from milestone 2:
- DESCRIPTION `URL:` and `BugReports:` → new repo URL
- _pkgdown.yml `url:` → new GitHub Pages URL
- README.Rmd badge URLs and `remotes::install_github()` → new repo name
- CI config and any other hardcoded URLs that GitHub won't redirect (e.g., raw.githubusercontent.com paths)
- Re-knit README.md, deploy pkgdown site


## 8. nhdplusTools deprecation shim — conditional

**Decide this after 6a.** The shim exists to keep reverse dependencies working; if all eight migrate, its only remaining audience is user scripts calling `library(nhdplusTools)` directly, which the CRAN archive plus a README pointer may serve well enough. Skipping it means going from 6a straight to milestone 9.

One thing to check before skipping: hydroloom's tests and examples `source(system.file("extdata/...", package = "nhdplusTools"))`. A function-alias shim ships no `inst/extdata`, so it would not have covered that case anyway — which is an argument that 6a is the real fix and the shim was never load-bearing.

If the shim does get built: maintain it as a branch in the hydrogeofetch repo. Every exported function becomes a `.Deprecated()` wrapper that forwards to `hydrogeofetch::*` or `hydroloom::*` as appropriate. Functions that were removed from hydrogeofetch because they duplicated hydroloom (indexing, navigation shorthands, network analysis wrappers) forward directly to hydroloom. Functions that remain in hydrogeofetch forward there. Submit to CRAN.

**Done when:** Shim accepted on CRAN. Installing nhdplusTools gives deprecation warnings that name hydrogeofetch or hydroloom as the replacement.

**Gate:** Confirm reverse-dep maintainers have a migration path and timeline. Announce the deprecation and the October 2028 archive date.


## 9. Archive nhdplusTools — October 2028

Request CRAN archive after the two-year sunset. Archived packages remain installable from the CRAN archive, so existing code pinned to nhdplusTools keeps working.

**Done when:** nhdplusTools archived on CRAN.

**Gate:** Verify no remaining reverse dependencies on CRAN. Post a final announcement. Confirm the archived package installs cleanly from the archive.
