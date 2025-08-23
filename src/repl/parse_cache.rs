//! Parse result caching for REPL completion system.
//!
//! This module provides an LRU-based cache for tree-sitter parse results to improve
//! completion performance by avoiding redundant parsing operations.
//!
//! ## Example Usage
//!
//! ```rust
//! # use caescript::repl::parse_cache::ParseCache;
//! let cache = ParseCache::new(100); // Cache up to 100 entries
//! let metrics = cache.metrics();
//! assert_eq!(metrics.hits, 0);
//! assert_eq!(metrics.misses, 0);
//! ```

use lru::LruCache;
use std::num::NonZeroUsize;
use tree_sitter::{Parser, Tree};

/// Metrics tracking for parse cache performance.
#[derive(Default, Debug, Clone)]
pub struct CacheMetrics {
    /// Number of cache hits (successful cache retrievals)
    pub hits: u64,
    /// Number of cache misses (required parsing)
    pub misses: u64,
    /// Number of cache evictions due to size limits
    pub evictions: u64,
}

impl CacheMetrics {
    /// Calculate the cache hit rate as a percentage.
    pub fn hit_rate(&self) -> f64 {
        if self.hits + self.misses == 0 {
            0.0
        } else {
            (self.hits as f64) / ((self.hits + self.misses) as f64) * 100.0
        }
    }

    /// Get total number of cache accesses.
    pub fn total_accesses(&self) -> u64 {
        self.hits + self.misses
    }
}

/// LRU cache for tree-sitter parse results.
///
/// This cache stores parsed Tree objects keyed by input strings to avoid
/// redundant parsing operations during REPL completion. The cache automatically
/// evicts least recently used entries when the size limit is reached.
///
/// ## Performance Characteristics
///
/// - Cache lookup: O(1) average case
/// - Cache insertion: O(1) average case
/// - Memory usage: Bounded by max_size parameter
/// - Thread safety: Not thread-safe (designed for single-threaded REPL)
pub struct ParseCache {
    /// LRU cache storing input string -> parsed Tree mappings
    cache: LruCache<String, Tree>,
    /// Performance metrics for monitoring cache effectiveness
    metrics: CacheMetrics,
    /// Maximum cache size (number of entries)
    max_size: usize,
}

impl ParseCache {
    /// Create a new ParseCache with the specified maximum size.
    ///
    /// # Arguments
    ///
    /// * `max_size` - Maximum number of cache entries (must be > 0)
    ///
    /// # Panics
    ///
    /// Panics if max_size is 0.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use caescript::repl::parse_cache::ParseCache;
    /// let cache = ParseCache::new(100); // Cache up to 100 parse results
    /// assert_eq!(cache.capacity(), 100);
    /// ```
    pub fn new(max_size: usize) -> Self {
        assert!(max_size > 0, "Cache size must be greater than 0");

        Self {
            cache: LruCache::new(NonZeroUsize::new(max_size).expect("max_size must be > 0")),
            metrics: CacheMetrics::default(),
            max_size,
        }
    }

    /// Get a parsed tree from cache or parse the input if not cached.
    ///
    /// This is the primary method for cache-aware parsing. It first checks
    /// if the input string has been parsed before and is in the cache. If so,
    /// it returns a clone of the cached Tree. Otherwise, it parses the input
    /// using the provided parser, caches the result, and returns the Tree.
    ///
    /// # Arguments
    ///
    /// * `input` - The input string to parse
    /// * `parser` - Mutable reference to the tree-sitter parser
    ///
    /// # Returns
    ///
    /// A Tree object representing the parsed input. Returns None if parsing fails.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use caescript::repl::parse_cache::ParseCache;
    /// # use tree_sitter::Parser;
    /// let mut cache = ParseCache::new(50);
    /// let mut parser = Parser::new();
    /// // Would parse and cache if parser was properly configured
    /// assert_eq!(cache.len(), 0); // Cache starts empty
    /// ```
    pub fn get_or_parse(&mut self, input: &str, parser: &mut Parser) -> Option<Tree> {
        // Check cache first
        if let Some(cached_tree) = self.cache.get(input) {
            self.metrics.hits += 1;
            // Clone the tree to avoid ownership issues
            // Note: Tree cloning in tree-sitter is relatively efficient
            Some(cached_tree.clone())
        } else {
            self.metrics.misses += 1;

            // Parse the input
            if let Some(tree) = parser.parse(input, None) {
                // Track evictions before insertion
                let was_at_capacity = self.cache.len() >= self.max_size;

                // Insert into cache (may evict LRU entry)
                self.cache.put(input.to_string(), tree.clone());

                if was_at_capacity {
                    self.metrics.evictions += 1;
                }

                Some(tree)
            } else {
                // Don't cache failed parses
                None
            }
        }
    }

    /// Get current cache metrics.
    ///
    /// Returns a snapshot of cache performance metrics including hits, misses,
    /// evictions, and calculated hit rate.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use caescript::repl::parse_cache::ParseCache;
    /// let cache = ParseCache::new(100);
    /// let metrics = cache.metrics();
    /// assert_eq!(metrics.hit_rate(), 0.0); // No accesses yet
    /// ```
    pub fn metrics(&self) -> &CacheMetrics {
        &self.metrics
    }

    /// Get current cache size (number of entries).
    pub fn len(&self) -> usize {
        self.cache.len()
    }

    /// Check if cache is empty.
    pub fn is_empty(&self) -> bool {
        self.cache.len() == 0
    }

    /// Get maximum cache capacity.
    pub fn capacity(&self) -> usize {
        self.max_size
    }

    /// Clear all cached entries and reset metrics.
    ///
    /// This is useful for testing or when memory usage needs to be minimized.
    pub fn clear(&mut self) {
        self.cache.clear();
        self.metrics = CacheMetrics::default();
    }

    /// Check if the cache contains a specific input.
    ///
    /// This method doesn't affect LRU ordering or metrics.
    pub fn contains(&self, input: &str) -> bool {
        self.cache.peek(input).is_some()
    }
}

impl std::fmt::Debug for ParseCache {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParseCache")
            .field("len", &self.len())
            .field("capacity", &self.capacity())
            .field("metrics", &self.metrics)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cache_creation() {
        let cache = ParseCache::new(10);
        assert_eq!(cache.capacity(), 10);
        assert_eq!(cache.len(), 0);
        assert!(cache.is_empty());
    }

    #[test]
    #[should_panic(expected = "Cache size must be greater than 0")]
    fn test_cache_creation_zero_size() {
        ParseCache::new(0);
    }

    #[test]
    fn test_cache_metrics_initial() {
        let cache = ParseCache::new(5);
        let metrics = cache.metrics();
        assert_eq!(metrics.hits, 0);
        assert_eq!(metrics.misses, 0);
        assert_eq!(metrics.evictions, 0);
        assert_eq!(metrics.hit_rate(), 0.0);
        assert_eq!(metrics.total_accesses(), 0);
    }

    #[test]
    fn test_cache_metrics_hit_rate() {
        let mut metrics = CacheMetrics::default();
        metrics.hits = 7;
        metrics.misses = 3;
        assert_eq!(metrics.hit_rate(), 70.0);
        assert_eq!(metrics.total_accesses(), 10);
    }

    #[test]
    fn test_cache_contains() {
        let cache = ParseCache::new(5);
        assert!(!cache.contains("test"));

        // We can't easily test actual parsing without setting up tree-sitter,
        // so we'll test the basic structure and leave parser integration for
        // integration tests.
    }

    #[test]
    fn test_cache_clear() {
        let mut cache = ParseCache::new(5);
        cache.metrics.hits = 10;
        cache.metrics.misses = 5;

        cache.clear();

        assert_eq!(cache.len(), 0);
        assert!(cache.is_empty());
        assert_eq!(cache.metrics().hits, 0);
        assert_eq!(cache.metrics().misses, 0);
    }

    #[test]
    fn test_cache_with_parsing() {
        let mut cache = ParseCache::new(2);
        let mut parser = Parser::new();

        // Without language set, parser.parse() will return None
        // This tests the error handling path
        let result = cache.get_or_parse("let x = 5", &mut parser);
        assert!(result.is_none());

        // Should record a miss but not cache the failure
        assert_eq!(cache.metrics().misses, 1);
        assert_eq!(cache.metrics().hits, 0);
        assert_eq!(cache.len(), 0);
    }

    #[test]
    fn test_cache_capacity_methods() {
        let cache = ParseCache::new(42);
        assert_eq!(cache.capacity(), 42);
        assert_eq!(cache.len(), 0);
        assert!(cache.is_empty());
        assert!(!cache.contains("anything"));
    }

    #[test]
    fn test_debug_formatting() {
        let cache = ParseCache::new(10);
        let debug_str = format!("{:?}", cache);
        assert!(debug_str.contains("ParseCache"));
        assert!(debug_str.contains("len: 0"));
        assert!(debug_str.contains("capacity: 10"));
    }

    #[test]
    fn test_metrics_edge_cases() {
        let metrics = CacheMetrics::default();
        assert_eq!(metrics.hit_rate(), 0.0);
        assert_eq!(metrics.total_accesses(), 0);

        let mut metrics_with_data = CacheMetrics::default();
        metrics_with_data.hits = 100;
        assert_eq!(metrics_with_data.hit_rate(), 100.0);
        assert_eq!(metrics_with_data.total_accesses(), 100);
    }
}
