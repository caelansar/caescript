# Requirements: REPL Completer Node Detection Fix

## Overview

The REPL completer in `src/repl/completer.rs` has a critical bug where the tree-sitter node detection at cursor positions only returns "program" or "ERROR" nodes, causing all other match arms in the completion context analysis to be ineffective. This results in poor completion suggestions and prevents context-aware completion.

## Problem Statement

### Current Behavior
- When requesting completions, the `get_completion_context` method calls `descendant_for_point_range(point, point)` 
- This method consistently returns either:
  - "program" node (root node) when input is valid
  - "ERROR" node when input has parse errors
- The specific nodes like "identifier", "let_statement", "function_call" are never returned
- This makes the match statement at lines 166-205 ineffective except for the "ERROR" case

### Expected Behavior
- The completer should identify specific nodes at cursor positions (e.g., "identifier", "integer_literal", "builtin_function")
- Context-aware completion should work based on the actual syntax element at the cursor
- Different completion suggestions should be provided based on the syntactic context

## User Stories

### Story 1: Identifier Completion
**As a** developer using the REPL  
**I want** to get identifier suggestions when my cursor is on or after an identifier  
**So that** I can quickly complete variable names and function names

**Acceptance Criteria:**
- [ ] When cursor is at end of "x", node kind should be "identifier"
- [ ] When cursor is at end of "puts", node kind should be "builtin_function" 
- [ ] Completion suggestions should include variables and functions
- [ ] Match arm for "identifier" should be executed

### Story 2: Let Statement Context Detection
**As a** developer writing variable declarations  
**I want** the completer to detect when I'm in a let statement context  
**So that** I get appropriate completion suggestions for variable assignment

**Acceptance Criteria:**
- [ ] When cursor is after "let x", node should be detected as part of let_statement
- [ ] When cursor is after "let x = ", context should indicate expression position
- [ ] NewVariable context should be detected for variable name position
- [ ] Keywords and expressions should be suggested appropriately

### Story 3: Function Call Context Detection  
**As a** developer writing function calls  
**I want** the completer to detect function call contexts  
**So that** I get parameter completion suggestions

**Acceptance Criteria:**
- [ ] When cursor is after "puts(", context should indicate function argument position
- [ ] Node detection should identify function call structure
- [ ] Expression completions should be provided in argument positions

### Story 4: Error Recovery
**As a** developer with incomplete input  
**I want** the completer to provide meaningful suggestions even with parse errors  
**So that** I can continue typing with assistance

**Acceptance Criteria:**
- [ ] ERROR nodes should be analyzed for partial completions
- [ ] Incomplete statements should still provide relevant suggestions
- [ ] System should gracefully handle malformed input

## Non-Functional Requirements

### Performance
- Node detection fix should not significantly impact completion latency
- Tree traversal should be efficient for typical input sizes
- Solution should work with existing parse caching

### Reliability  
- Fix should handle all edge cases found in debug output
- Should not break existing completion functionality
- Must work consistently across different input patterns

### Maintainability
- Solution should be clear and well-documented
- Should follow existing code patterns in the completer
- Must include comprehensive test coverage

## Constraints

- Must maintain compatibility with existing `tree_sitter::Node` API
- Cannot modify the tree-sitter grammar or parser generation
- Should reuse existing tree traversal patterns where possible
- Must work with both VM and interpreter completion states

## Success Criteria

1. **Functional Testing**: All match arms in `get_completion_context` are reachable
2. **Integration Testing**: Context-aware completions work as expected in REPL
3. **Debug Verification**: Debug completer shows specific node types instead of just "program"
4. **Performance Testing**: No significant regression in completion performance
5. **Edge Case Handling**: All test cases in debug_completer.rs return appropriate nodes

## Out of Scope

- Modifying the tree-sitter grammar itself
- Adding new completion context types beyond what's needed for the fix
- Performance optimizations beyond maintaining current performance
- Changes to the overall completion architecture

This fix is critical for making the REPL completion system actually functional as intended.