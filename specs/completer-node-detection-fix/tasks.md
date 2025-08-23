# Implementation Plan: REPL Completer Node Detection Fix

## Tasks

### Phase 1: Core Implementation
- [ ] 1. Implement `find_node_at_cursor` method in CaescriptCompleter
  - [ ] 1.1 Add private method with multi-fallback node detection logic
  - [ ] 1.2 Implement Method 1: Exact position query with program node filtering
  - [ ] 1.3 Implement Method 2: Previous position (pos-1) query for token boundaries
  - [ ] 1.4 Implement Method 3: Byte range query as additional fallback
  - [ ] 1.5 Implement Method 4: Graceful fallback to original behavior
- [ ] 2. Integrate new method into get_completion_context
  - [ ] 2.1 Replace line 162 single descendant_for_point_range call
  - [ ] 2.2 Update method to use find_node_at_cursor with all required parameters
  - [ ] 2.3 Preserve all existing match arm logic and fallback behavior
- [ ] 3. Verify existing debug prints show correct node types
  - [ ] 3.1 Test with debug_completer to confirm node detection improvements
  - [ ] 3.2 Ensure println! statements show specific node kinds instead of "program"

### Phase 2: Testing & Validation
- [ ] 4. Create comprehensive unit tests
  - [ ] 4.1 Test node detection for all cases in debug_completer.rs
  - [ ] 4.2 Test identifier detection at various cursor positions
  - [ ] 4.3 Test literal detection (integer, string, boolean)
  - [ ] 4.4 Test builtin function detection
  - [ ] 4.5 Test error handling with malformed input
  - [ ] 4.6 Test whitespace and boundary cases
- [ ] 5. Verify completion context improvements
  - [ ] 5.1 Test that identifier match arm is now reachable
  - [ ] 5.2 Test let statement context detection works correctly
  - [ ] 5.3 Test function call context detection
  - [ ] 5.4 Test ERROR node handling still works for incomplete input
- [ ] 6. Integration testing with REPL
  - [ ] 6.1 Test actual completion suggestions improve in REPL session
  - [ ] 6.2 Verify context-aware completions work as expected
  - [ ] 6.3 Test performance doesn't regress significantly

### Phase 3: Performance & Polish
- [ ] 7. Performance validation
  - [ ] 7.1 Benchmark completion latency before and after changes
  - [ ] 7.2 Verify no significant regression (< 10% overhead acceptable)
  - [ ] 7.3 Profile tree-sitter query frequency and cost
- [ ] 8. Edge case validation
  - [ ] 8.1 Test all debug_completer scenarios pass
  - [ ] 8.2 Test empty input handling
  - [ ] 8.3 Test very long input handling
  - [ ] 8.4 Test cursor at various token boundaries
- [ ] 9. Code cleanup and documentation
  - [ ] 9.1 Add comprehensive documentation to find_node_at_cursor method
  - [ ] 9.2 Update existing method comments to reflect improved behavior
  - [ ] 9.3 Ensure code follows existing style patterns

### Phase 4: Final Verification
- [ ] 10. Comprehensive testing
  - [ ] 10.1 Run all existing REPL completion tests
  - [ ] 10.2 Run debug_completer and verify all test cases show improvement
  - [ ] 10.3 Manual REPL testing with various completion scenarios
- [ ] 11. Documentation updates
  - [ ] 11.1 Update implementation comments to reflect the fix
  - [ ] 11.2 Document the multi-fallback approach for future maintainers
- [ ] 12. Final validation
  - [ ] 12.1 Confirm all original issue symptoms are resolved
  - [ ] 12.2 Verify no regressions in existing functionality
  - [ ] 12.3 Performance impact is within acceptable bounds

## Detailed Implementation Notes

### Key Method Signature
```rust
fn find_node_at_cursor(&self, root: &tree_sitter::Node, point: Point, line: &str, pos: usize) -> Option<tree_sitter::Node>
```

### Critical Success Criteria
1. **Node Detection**: `debug_completer` shows specific node types instead of "program"
2. **Match Arms**: All match arms in `get_completion_context` become reachable
3. **Completions**: REPL provides better context-aware completion suggestions
4. **Performance**: No significant completion latency regression
5. **Reliability**: All existing functionality continues to work

### Testing Commands
```bash
# Test node detection improvements
cargo run --bin debug_completer --features=build-binary

# Test REPL completion functionality  
cargo run --features=build-binary
# Then test completions interactively

# Run completion-related tests
cargo test --all-features completion
```

### Risk Mitigation
- **Backwards Compatibility**: Original logic preserved as final fallback
- **Minimal Changes**: Only one method modified, existing logic unchanged
- **Extensive Testing**: Cover all debug_completer cases plus edge cases
- **Performance Monitoring**: Benchmark before/after to catch regressions

## Rules & Tips

### Implementation Guidelines
1. **Preserve Existing Behavior**: The fix should enhance, not change, existing completion logic
2. **Handle All Edge Cases**: Every case in debug_completer.rs must work correctly
3. **Maintain Performance**: Additional queries should not significantly impact user experience
4. **Follow Patterns**: Use existing RefCell patterns and error handling approaches
5. **Test Thoroughly**: Both automated tests and manual REPL testing are required

### Key Insights from Analysis
1. **Root Cause**: `descendant_for_point_range(point, point)` returns outermost containing node
2. **Solution**: Multi-fallback approach tries different query strategies
3. **Critical Fix**: Testing `pos - 1` handles cursor-at-end-of-token cases
4. **Validation**: Debug completer provides perfect test harness for verification

### Common Pitfalls to Avoid
1. **Don't** assume single query method will work for all cases
2. **Don't** break existing ERROR node handling for incomplete input
3. **Don't** introduce performance regressions with excessive queries
4. **Don't** change completion context enum or candidate generation logic
5. **Don't** forget to test both VM and interpreter completion modes

This implementation plan provides a systematic approach to fixing the node detection issue while maintaining code quality and existing functionality.