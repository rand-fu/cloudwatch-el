# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

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
