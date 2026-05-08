# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

## 1.0.1

### Fixed 

- Removed and unused transient prefix key for favorites management that I accidentally left in when dropping support for it.
- `cloudwatch--read-multiline` now shows just your recent query history and not the entire mini buffer history. 

## 1.0.0

### Features

This release represents a lot of polish and things I've learned after writing and using this package exclusively for several months. I can't tell you the last time I looked at CloudWatch in the console. I hope you find it as useful and enjoy it as much as I do.W

- **Transient-based interface** — Full interactive menu for navigating CloudWatch settings, queries, and actions via `M-x cloudwatch`
- **Live log tailing** — Stream logs in real-time with `aws logs tail --follow`, stoppable without killing the buffer
- **Snapshot queries** — Retrieve historical log events with configurable result limits and async execution
- **CloudWatch Insights support** — Run advanced SQL-like queries with aggregations, time series, sorting, and field extraction
- **Insights presets** — Ship with common query templates (error counts, slow requests, request rate, pod restarts, etc.) and support user-defined presets
- **Insights detail view** — Press RET on any result row to view the full record with pretty-printed JSON
- **Filter patterns** — Simple text matching, JSON field filters, and quick presets for errors, warnings, 5xx status codes, Kubernetes namespaces, and pod names
- **Time range selection** — Relative mode (minutes back from now) or absolute mode (start/end date range in local timezone)
- **Region switching** — Change AWS regions on the fly with automatic cache invalidation
- **Favorites management** — Quick-access numbered shortcuts for up to 5 favorite log groups, with add/remove from the transient menu
- **Log groups browser** — Fetch and browse all available log groups in the current region with completion, backed by a 10-minute cache
- **Syntax highlighting** — Automatic colorization for ERROR, FATAL, WARN, INFO, and DEBUG levels, plus JSON keys and timestamps
- **Async operations** — All AWS CLI calls run asynchronously to keep Emacs responsive
- **Wide mode** — Toggle doubled column widths for message fields in Insights results
- **Adjustable query limits** — Increase or decrease result limits on the fly with `+` and `-` in result buffers
- **Insights time expansion** — Double the query time range and rerun with `+` in Insights result buffers
- **Multiline query input** — Enter complex Insights queries with RET for newlines and C-c C-c to submit
- **Polling timeout** — Configurable timeout for Insights query polling to prevent runaway timer chains
- **Error handling** — Friendly messages for missing credentials, expired tokens, access denied, rate limits, and invalid parameters
- **Doom Emacs integration** — Optional `cloudwatch-doom.el` module binding `SPC o C` to the CloudWatch interface

### Configuration

- `cloudwatch-default-region` — Default AWS region
- `cloudwatch-favorite-log-groups` — Persistable list of frequently used log groups
- `cloudwatch-query-limit` — Maximum events for basic queries (default 2500)
- `cloudwatch-insights-column-widths` — Per-field column widths for Insights result tables
- `cloudwatch-insights-presets` — User-extensible alist of named Insights query templates
- `cloudwatch-insights-poll-timeout` — Maximum seconds to wait for Insights results (default 120)

## 0.6.0 

### Added
- Absolute date range selection (`T` in transient menu)
- `cloudwatch--parse-time` with input validation
- Time range helpers (`--start-time-millis`, `--end-time-millis`, `--time-description`)

### Changed
- Both query and Insights paths now use shared time helpers
- Time mode displayed in transient (relative vs absolute range)
- Expand time (`+`) gracefully declines in absolute mode with helpful message

### Fixed
- Query and Insights used inconsistent inline time calculations

## 0.5.1

Just some clean up.

### Removed
- Unused `cloudwatch--pad-or-truncate` function
- Redundant description block in commentary
- Unnecessary `after!` block and wrong variable name in Doom integration

## 0.5.0

### Added
- New `cloudwatch-insights-results-mode` minor mode for Insights results buffers with dedicated keymap
- `cloudwatch-insights-expand-time` command (bound to `+`) to double the time range and rerun queries
- Buffer-local storage of query parameters (log-group, region, minutes, query) for reliable refresh and time expansion

### Changed
- `cloudwatch-rerun-insights` now uses stored buffer-local parameters instead of global state
- Results buffer keybindings are now managed through the minor mode keymap, preventing conflicts with Evil mode

### Fixed
- Keybindings in results buffers no longer get overridden by Evil mode

## 0.4.1

### Fixed
- Removed mouse hover highlighting on Insights result rows that was visually distracting
- Space key no longer triggers detail view in results buffer (only RET), preventing conflicts with Doom leader key setups
- Removed automatic "Add to favorites?" prompt when browsing log groups. Use `a` key in favorites management menu instead

## 0.4.0

### Added

- README section to explain the `--output text` approach (no JSON parsing needed)

### Changed
- Switched from `highlight-regexp` to `font-lock-add-keywords` with more comprehensive patterns (handles `"ERROR"`, `[ERROR]`, `:ERROR:` variants)

### Fixed
- Bug when adding favorites to a full list. The old code used `(nthcdr 9 ...)` but comment said "first 5". Now correctly uses `(seq-take ... 5)`
- `cloudwatch-add-to-favorites` now calls `(cloudwatch-transient)` at the end to return to the menu
- Moved `cloudwatch--setup-highlighting` to after `read-only-mode` in insights, ensuring font-lock applies properly

## 0.3.0

### Added
- CloudWatch Insights support with preset and custom queries
- Interactive detail view for Insights results (press RET)
- Configurable column widths (`cloudwatch-insights-column-widths`)
- Wide mode toggle for larger monitors
- Favorites management (add/remove from transient)
- Transient fallback - ESC and errors return to menu

### Changed
- Reorganized code structure for better maintainability
- Improved AWS CLI error handling with helpful messages
- Lazy region initialization from `cloudwatch-default-region`

### Fixed
- Error matching log group names containing "error"
- Pod/namespace filter syntax

## 0.2.0

### Added
- CloudWatch Insights integration with SQL-like query support for aggregations, time series, and statistics
- Preset query templates for common use cases (error counts, performance metrics, pod restarts)
- Interactive results viewer with clickable rows to view full log entry details
- Pretty-printed JSON in detail views
- Dynamic query limit adjustment (`+`/`-` keys in result buffers)

### Improved
- Transient menu organization with descriptive sections for filter types
- Result formatting with intelligent column width allocation

## 0.1.0

### Added

- Initial release with full CloudWatch log viewing functionality
- Transient-based menu interface for all operations
- Live log tailing with automatic updates
- Snapshot querying with configurable limits
- Advanced filter patterns with JSON field support
- Favorites management for frequently accessed log groups
- Region switching for multi-region AWS deployments
- Async operations to maintain Emacs responsiveness
- Syntax highlighting for log levels (ERROR, WARN, INFO, DEBUG)
- Doom Emacs integration with `SPC o C` keybinding
