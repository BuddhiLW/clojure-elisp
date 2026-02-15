# ClojureElisp Refactoring Plan

> Generated via SAA (Silence/Abstract/Act) Strategy Analysis
> Date: 2026-02-01 (Updated: 2026-02-02)

## Executive Summary

This plan identifies architectural improvements for ClojureElisp based on Domain-Driven Design (DDD), SOLID principles, TDD readiness, CLARITY, and **SLAP (Single Level of Abstraction Principle)** analysis. The compiler is functional but has accumulated technical debt as features were added organically.

**Current State (Updated 2026-02-02):**
- analyzer.clj: 1,460 LOC (50+ special forms, macro system, env management)
- emitter.clj: 1,285 LOC (326 function mappings, type system)
- core.clj: 213 LOC (public API, file handling)
- runtime.el: 1,754 LOC (150+ runtime functions including transducers, sets)
- Tests: ~5,620 LOC across 7 test files (292 tests, 1800+ assertions)

**SCC Metrics:**
| Language | Files | Code LOC | Complexity |
|----------|-------|----------|------------|
| Clojure | 12 | 6,777 | 1,011 |
| Elisp | 3 | 1,589 | 64 |
| Total | 24 | 9,419 | 1,142 |

---

## SILENCE Phase: Codebase Observations

### File Structure
```
src/clojure_elisp/
├── analyzer.clj    # AST construction (largest file)
├── emitter.clj     # Elisp code generation
├── core.clj        # Public API
├── nrepl.clj       # nREPL middleware
└── repl.clj        # REPL server

resources/clojure-elisp/
└── clojure-elisp-runtime.el  # Elisp runtime library

test/clojure_elisp/
├── analyzer_test.clj
├── emitter_test.clj
├── runtime_test.clj
├── core_test.clj
├── nrepl_test.clj
├── repl_test.clj
└── examples_test.clj
```

### Key Observations

1. **Monolithic Analyzer**: `analyzer.clj` handles too many concerns:
   - Special form analysis (50+ forms)
   - Macro expansion system
   - Environment/locals tracking
   - Destructuring expansion
   - Source location tracking
   - Namespace resolution

2. **Flat Core Function Mapping**: `core-fn-mapping` in emitter.clj is a single 300+ entry map mixing:
   - Arithmetic operators
   - Collection functions
   - String functions (clojure.string/*)
   - Set operations (clojure.set/*)
   - Emacs buffer operations
   - Process operations

3. **Runtime Coupling**: Runtime.el replicates Clojure semantics but has no contract with the emitter (implicit protocol).

4. **Test Coverage Pattern**: Heavy happy-path testing, minimal error-path or property-based tests.

---

## ABSTRACT Phase: Design Analysis

### 1. Domain-Driven Design (DDD) Lens

#### Bounded Contexts Identified

| Context | Responsibility | Current Location |
|---------|---------------|------------------|
| **Reader** | Parse Clojure forms | Clojure's built-in reader |
| **Analyzer** | AST construction, macro expansion | analyzer.clj |
| **Emitter** | Code generation | emitter.clj |
| **Runtime** | Elisp runtime support | clojure-elisp-runtime.el |
| **Tooling** | REPL, nREPL integration | nrepl.clj, repl.clj |

#### Domain Entities

- **AST Node**: `{:op keyword, :env map, ...}` - Well-defined
- **Environment**: `{:ns, :locals, :aliases, :refers}` - Implicit, mutable via dynamic vars
- **Macro Registry**: Global atom - Hidden coupling

#### Missing DDD Concepts

1. **Aggregate Root**: No clear aggregate for compilation unit
2. **Domain Events**: No compile-time event system for plugins/extensions
3. **Value Objects**: AST nodes are close but lack validation
4. **Anti-Corruption Layer**: No boundary between Clojure reader and our analyzer

#### DDD Recommendations

```
Priority: HIGH
- [ ] Extract Environment as explicit value object with clear transitions
- [ ] Define CompilationUnit aggregate containing ns metadata + forms
- [ ] Add AST validation at context boundaries
```

---

### 2. SOLID Principles Lens

#### Single Responsibility Principle (SRP)

**Violations Found:**

| File | Responsibilities | Should Be |
|------|-----------------|-----------|
| analyzer.clj | Special forms + Macros + Env + Destructuring | 4 modules |
| emitter.clj | Code gen + Name mangling + Core mappings | 3 modules |
| core.clj | API + File I/O + Dependency graph | 2 modules |

**SRP Recommendations:**
```
Priority: HIGH
- [ ] Extract macro system to macros.clj
- [ ] Extract destructuring to destructure.clj
- [ ] Extract core-fn-mapping to mappings.clj (organized by category)
- [ ] Extract file handling to files.clj
```

#### Open/Closed Principle (OCP)

**Violations Found:**

1. Adding new special forms requires modifying `special-forms` map in analyzer.clj
2. Adding new Elisp mappings requires modifying `core-fn-mapping` map
3. No plugin/extension mechanism

**OCP Recommendations:**
```
Priority: MEDIUM
- [ ] Create extensible registry for special forms
- [ ] Create category-based mapping modules with merge semantics
- [ ] Add analyze-form multimethod for user extensions
```

#### Liskov Substitution Principle (LSP)

**Status:** ✅ Good

Multimethod dispatch (`emit-node`, `analyze`) provides proper substitutability. AST nodes with `:op` keys are polymorphic.

#### Interface Segregation Principle (ISP)

**Status:** ✅ Good

Functions are small and focused. Public API in core.clj is minimal.

#### Dependency Inversion Principle (DIP)

**Violations Found:**

1. Emitter directly depends on `mangle-name` implementation
2. No abstraction over runtime functions (compile-time contract)
3. Hard-coded Elisp target (no multi-target potential)

**DIP Recommendations:**
```
Priority: LOW (for now)
- [ ] Define TargetLanguage protocol for future backends
- [ ] Create RuntimeContract spec validating emitter/runtime alignment
```

---

### 3. TDD Readiness Lens

#### Current Test Architecture

| Test File | Tests | Assertions | Coverage Focus |
|-----------|-------|------------|----------------|
| analyzer_test.clj | ~100 | ~600 | AST structure |
| emitter_test.clj | ~100 | ~700 | Output strings |
| runtime_test.clj | ~80 | ~500 | End-to-end in Emacs |
| core_test.clj | ~30 | ~150 | Integration |

#### Test Strengths
- Comprehensive happy-path coverage
- Good isolation between analyzer and emitter tests
- End-to-end tests via Emacs subprocess

#### Test Gaps

1. **Missing Error Tests**: No tests for malformed input handling
2. **Missing Property Tests**: No generative testing for invariants
3. **Missing Contract Tests**: No tests verifying emitter/runtime alignment
4. **Slow Feedback**: End-to-end tests require Emacs subprocess

#### TDD Recommendations

```
Priority: HIGH
- [ ] Add error-path tests for analyzer (malformed forms)
- [ ] Add property-based tests for invariants:
      - Balanced parens in output
      - All AST nodes have :op
      - All locals are bound
- [ ] Add contract tests: emitter output matches runtime expectations
- [ ] Create fast unit-test-only profile

Priority: MEDIUM  
- [ ] Add mutation testing to validate test quality
- [ ] Create test fixtures for common AST patterns
```

---

### 4. CLAIRY Analysis

> Note: CLAIRY is not a widely-known methodology. Interpreting as "Clarity" - code readability and maintainability analysis.

#### Clarity Issues Found

1. **Long Functions**: `analyze-defn` is 70+ lines with nested conditionals
2. **Implicit State**: `*env*` and `*source-context*` dynamic vars hide data flow
3. **Magic Strings**: Elisp output built via string interpolation
4. **Naming**: Some inconsistency (`emit-node` vs `emit`, `analyze` vs `analyze-*`)

#### Clarity Recommendations

```
Priority: MEDIUM
- [ ] Extract sub-functions from analyze-defn (handle-single-arity, handle-multi-arity)
- [ ] Add type hints/specs for public functions
- [ ] Create Elisp output builders instead of raw format strings
- [ ] Standardize naming: all special-form analyzers as analyze-<form>
```

---

### 5. SLAP (Single Level of Abstraction Principle) Analysis

> SLAP: Each function should operate at a single level of abstraction. Don't mix high-level orchestration with low-level details.

#### SLAP Violations Found

**analyzer.clj:**

| Function | Lines | Violation |
|----------|-------|-----------|
| `analyze` | 1344-1436 | Mixes type dispatch (high) with literal analysis (low) |
| `analyze-defn` | 130-198 | Mixes arity detection (high) with param processing (low) |
| `process-fn-params` | 410-457 | Mixes extraction (high) with destructure expansion (low) |
| `parse-iteration-clauses` | 987-1027 | Clean - single abstraction level |
| `analyze-iteration-clauses` | 1066-1074 | Clean - proper delegation |

**emitter.clj:**

| Function | Lines | Violation |
|----------|-------|-----------|
| `emit-node :defn` | 439-525 | Mixes arity dispatch (high) with binding formatting (low) |
| `emit-node :defmethod` | 715-739 | Mixes destructure handling (high) with string building (low) |
| `emit-node :reify` | 931-977 | Multiple abstraction levels in one method |
| `emit-doseq-clauses` | 583-612 | Clean - single reduce operation |
| `emit-for-clauses` | 632-670 | Clean - single reduce operation |

**runtime.el:**

| Function | Lines | Violation |
|----------|-------|-----------|
| `clel-transduce` | 1282-1302 | Mixes argument parsing (high) with reduction (low) |
| `clel-partition-all` | 1582-1614 | Mixes transducer/lazy-seq branching with implementation |
| `clel-get` | 47-61 | Acceptable - type dispatch is inherent |

#### SLAP Recommendations

```
Priority: HIGH
- [ ] Extract `analyze` type dispatch into helper predicates
- [ ] Split `analyze-defn` into analyze-single-arity-defn + analyze-multi-arity-defn
- [ ] Extract `emit-node :defn` into emit-single-arity + emit-multi-arity + emit-variadic
- [ ] Create emit-binding-block helper for repeated binding formatting

Priority: MEDIUM
- [ ] Extract argument parsing from clel-transduce into clel--parse-transduce-args
- [ ] Create unified arity-dispatch pattern for dual-mode functions (transducer/lazy)
```

---

### 6. Cyclomatic Complexity Hotspots

Functions with highest complexity (manual analysis):

| File | Function | Est. Complexity | Issue |
|------|----------|-----------------|-------|
| analyzer.clj | `analyze` | ~25 | Large cond with 15+ branches |
| analyzer.clj | `analyze-defn` | ~12 | Nested if/let/when |
| analyzer.clj | `expand-map-destructuring` | ~10 | Multiple cond-> branches |
| emitter.clj | `emit-node :defn` | ~15 | cond + nested format |
| emitter.clj | `emit-node :defrecord` | ~8 | Multiple for comprehensions |
| runtime.el | `clel-get` | ~8 | Type dispatch cond |
| runtime.el | `clel-set-join` | ~10 | Nested loops + conditionals |

**Complexity Reduction Strategy:**
1. Extract type-dispatch into predicates: `const? local? invoke?`
2. Use multimethod dispatch instead of cond where > 5 branches
3. Extract string-building helpers to reduce format complexity

---

## ACT Phase: Refactoring Tasks

### Phase 1: Foundation (Low Risk)

These changes are additive and don't break existing functionality.

| ID | Task | Effort | Risk | Dependencies |
|----|------|--------|------|--------------|
| R1.1 | Extract macro system to `macros.clj` | Medium | Low | None |
| R1.2 | Extract destructuring to `destructure.clj` | Medium | Low | None |
| R1.3 | Split `core-fn-mapping` into category modules | Small | Low | None |
| R1.4 | Add error-path tests for malformed input | Small | Low | None |
| R1.5 | Create test fixtures module | Small | Low | None |

### Phase 2: Structure (Medium Risk)

These changes reorganize code while maintaining the same behavior.

| ID | Task | Effort | Risk | Dependencies |
|----|------|--------|------|--------------|
| R2.1 | Extract Environment as value object | Medium | Medium | R1.1 |
| R2.2 | Create CompilationUnit aggregate | Medium | Medium | R2.1 |
| R2.3 | Add property-based tests for AST invariants | Medium | Low | R1.4 |
| R2.4 | Split large functions (analyze-defn, emit-node :defn) | Medium | Medium | None |
| R2.5 | Create Elisp builder helpers | Small | Low | None |

### Phase 3: Architecture (Higher Risk)

These changes affect the fundamental architecture.

| ID | Task | Effort | Risk | Dependencies |
|----|------|--------|------|--------------|
| R3.1 | Extensible special-forms registry | Large | Medium | R2.2 |
| R3.2 | Runtime contract validation | Large | Medium | R2.2 |
| R3.3 | Multi-target abstraction (TargetLanguage protocol) | Large | High | R3.1, R3.2 |
| R3.4 | AST validation at boundaries | Medium | Medium | R2.1 |

---

## Proposed New Module Structure

```
src/clojure_elisp/
├── analyzer/
│   ├── core.clj          # Main analyze function, dispatch
│   ├── special_forms.clj # All special form analyzers
│   ├── macros.clj        # Macro system (registry, expansion)
│   ├── destructure.clj   # Destructuring logic
│   └── env.clj           # Environment management
├── emitter/
│   ├── core.clj          # Main emit function, dispatch
│   ├── forms.clj         # Form-specific emitters
│   ├── mappings/
│   │   ├── arithmetic.clj
│   │   ├── collections.clj
│   │   ├── strings.clj
│   │   ├── emacs.clj     # Buffer/process operations
│   │   └── sets.clj
│   └── builders.clj      # Elisp output builders
├── compiler.clj          # Compilation pipeline, CompilationUnit
├── api.clj               # Public API (renamed from core.clj)
├── files.clj             # File I/O, dependency graph
├── nrepl.clj             # nREPL middleware
└── repl.clj              # REPL server
```

---

## Migration Path

### Keeping Tests Green

1. **Create new modules alongside old code** - don't delete yet
2. **Redirect old functions to new locations** with deprecation warnings
3. **Run full test suite after each move**
4. **Only remove old code after all tests pass with new structure**

### Backward Compatibility

Public API functions in `clojure-elisp.core` should remain stable:
- `emit` - Compile single form
- `emit-forms` - Compile multiple forms
- `compile-string` - Compile string input
- `compile-file` - Compile file
- `compile-ns` - Compile namespace
- `compile-project` - Compile project

Internal namespaces can change freely.

---

## Risk Assessment

| Change | Risk Level | Mitigation |
|--------|------------|------------|
| Extracting macros.clj | Low | Isolated subsystem with clear boundaries |
| Extracting destructure.clj | Low | Pure functions, easily tested |
| Splitting core-fn-mapping | Low | Just reorganization, no logic change |
| Environment refactor | Medium | Many touch points, need comprehensive tests |
| Extensible registries | Medium | API design decisions, may affect plugins |
| Multi-target abstraction | High | Large scope, defer until clear use case |

---

## Metrics to Track

Post-refactoring, monitor:

1. **File sizes** - No file > 500 LOC
2. **Cyclomatic complexity** - No function > 10
3. **Test coverage** - Maintain > 80%
4. **Build time** - Should not increase significantly
5. **New form addition** - Measure lines of code needed

---

## Immediate Next Steps

1. **R1.4**: Add 5-10 error-path tests for malformed input
2. **R1.3**: Split `core-fn-mapping` into 5 category files
3. **R1.1**: Extract macro system to `macros.clj`
4. **R1.5**: Create test fixtures for common patterns

These are low-risk, high-value improvements that establish patterns for the larger refactoring.

---

## Appendix: Key Decisions

### Decision 1: Keep analyzer/emitter separation

**Rationale**: The current separation is sound. Analyzer produces AST, emitter consumes it. This follows classic compiler architecture.

### Decision 2: Don't over-abstract prematurely

**Rationale**: TargetLanguage protocol is interesting but there's no second target yet. Keep it simple until there's a real use case.

### Decision 3: Prioritize test infrastructure

**Rationale**: Better tests enable safer refactoring. Invest in test fixtures, property tests, and error tests before large structural changes.

### Decision 4: Preserve the multimethod pattern

**Rationale**: `emit-node` multimethod dispatching on `:op` is idiomatic and extensible. Keep this pattern.

### Decision 5: Address SLAP violations before structural changes

**Rationale**: SLAP violations make code harder to test and maintain. Extracting helper functions (Phase 1) creates cleaner seams for later module extraction (Phase 2+).

---

## Appendix B: Recent Feature Growth (2026-01-29 to 2026-02-01)

These recent additions increased codebase size and complexity:

| Feature | Files Modified | LOC Added | Complexity Impact |
|---------|---------------|-----------|-------------------|
| clel-031: Buffer/Process Interop | analyzer, emitter | +200 | +60 mappings, 6 special forms |
| clel-039: `for` list comprehension | analyzer, emitter | +100 | Multi-binding iteration logic |
| clel-042: clojure.string namespace | emitter, runtime | +150 | 18 string function mappings |
| clel-043: Transducers | emitter, runtime | +400 | 20+ transducer factories |
| clel-044: clojure.set namespace | emitter, runtime | +350 | 18 set functions |
| clel-045: Multi-binding for/doseq | analyzer, emitter | +100 | Clause parsing complexity |

**Observation**: runtime.el grew from ~1200 to ~1750 LOC in one week. This validates the need for runtime modularization.

---

## Appendix C: Code Quality Standards (from Memory)

These are the project's established code quality standards:

1. **TDD**: Write tests first, implement to pass. All PRs must maintain 0 failures.
2. **DDD**: Compiler stages are bounded contexts. Don't leak abstractions.
3. **SOLID**: Single responsibility per function/namespace. Depend on abstractions.
4. **CLARITY**: Code should be self-evident. Name things for what they are.
5. **SLAP**: Each function operates at one level of abstraction.

**Target Metrics:**
- No source file > 500 LOC
- No function complexity > 10
- Prefer multimethods for dispatch over nested conditionals
