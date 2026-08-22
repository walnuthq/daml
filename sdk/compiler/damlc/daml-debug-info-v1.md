# `daml-debug-info/v1`

Draft specification of the Daml debug metadata format, and of the runtime
debug trace that source-level debuggers consume.

This document is implementation-backed. The producer is
`daml build --experimental-debug-info` in this repository, which derives
the metadata from the compiled Daml-LF package rather than by scanning
source text. The runtime trace is written by the Daml Script runner.
Section 12 records where the implementation still differs from this text.

A machine-checkable JSON Schema for this format lives beside this document
at `daml-debug-info-v1.schema.json`. It covers structure. The rules in
section 11 cover everything a schema cannot express.

The key words MUST, MUST NOT, SHOULD, and MAY are used as in RFC 2119.

Status: draft. The format is versioned, and v1 is marked experimental until
it has been reviewed with Daml maintainers.

---

## 1. Artifact placement

A producer emits the metadata twice:

1. **Sidecar:** next to the DAR, named `<dar-basename>.debug-info.json`
   (for example `.daml/dist/asset-demo-1.0.0.debug-info.json`).
2. **DAR member:** `META-INF/daml-debug-info/<package-id>.json`.
   Existing DAR consumers ignore unknown `META-INF` members, so embedding is
   backward compatible. Consumers SHOULD also accept the legacy prototype
   member `META-INF/daml-debug-info.json`.

Rules:

- The two copies MUST be byte-identical, and a consumer that finds
  different bytes MUST treat the metadata as invalid. The DAR member is
  authoritative, and the sidecar exists for convenience.
- v1 producers emit metadata for the main package only. Dependencies carry
  their own metadata in their own DARs.
- Embedding changes the DAR file hash, never the package id.
- Producers SHOULD emit a canonical serialization (UTF-8, LF newlines,
  stable key order per producer version), so identical inputs produce
  identical bytes.

## 2. Top-level document

```json
{
  "schema": "daml-debug-info/v1",
  "version": "1.0",
  "producer": { "tool": "damlc", "version": "3.x",
                "buildMode": "experimental",
                "features": ["source-spans", "symbols", "lf-refs",
                             "value-slots", "steps", "failure-sites"] },
  "package": { "packageId": "<hex>", "name": "asset-demo",
               "version": "1.0.0", "lfVersion": "2.1",
               "sdkVersion": "3.x" },
  "sources": [ ... ],
  "unmappedModules": [ ... ],
  "spans": [ ... ],
  "symbols": [ ... ],
  "valueSlots": [ ... ],
  "failureSites": [ ... ],
  "steps": [ ... ],
  "compatibility": { "minConsumerSchema": "daml-debug-info/v1",
                     "ignoreUnknownFields": true }
}
```

**Versioning.** Two version fields do different jobs, and both are
required.

`schema` is the major-versioned identifier of the format, for example
`daml-debug-info/v1`. It is the compatibility gate: a consumer MUST reject
a document whose major version it does not support, and MUST ignore unknown
fields in one it does support.

`version` is the precise revision of the format the document follows, as
`MAJOR.MINOR`, for example `1.0`. Its major part MUST agree with `schema`.
Minor revisions are additive only: they may add optional fields or new
enumerated values, never remove or repurpose an existing one. A consumer
built for `1.0` therefore reads `1.3` safely, and a consumer that wants a
field added in `1.2` can require it by testing `version` rather than by
guessing from the presence of the field. Producers SHOULD also list the
sections they emitted in `producer.features`, which answers "was this
section omitted or is it genuinely empty".

`producer.version` is a third, unrelated thing: the version of the tool
that wrote the file, for diagnosing producer bugs. It says nothing about
the format.

The `compatibility` object is informative only: producers MAY emit it, and
consumers MUST NOT rely on it.

**Producer invariants.**

- Enabling metadata emission MUST NOT change the compiled Daml-LF output.
  The package id of a package built with and without the emission flag MUST
  be identical, so that metadata from a debug build describes the exact
  artifact that is deployed.
- Metadata MUST be derived from the compiled Daml-LF package, never from
  textual scanning of sources.

**Positions.** All line and column positions in the document are **1-based**
(Daml-LF stores 0-based source locations, and producers convert). The `end`
line is inclusive. The `end` column is **exclusive**: it points one past the
last character of the span, following the GHC and Daml-LF convention that
the reference implementation preserves. Columns count Unicode code points,
not UTF-8 bytes and not UTF-16 code units. Consumers converting spans to
protocols with different units (for example LSP, whose default is UTF-16)
MUST convert explicitly.

**Supported LF range.** v1 describes Daml-LF 2.1 and later (Daml SDK 3.x).

**Required and optional fields.**

| Object | Required | Optional |
| --- | --- | --- |
| top level | `schema`, `version`, `producer`, `package`, `sources`, `spans`, `symbols`, `valueSlots`, `steps` | `unmappedModules`, `failureSites`, `compatibility` |
| `producer` | `tool`, `version`, `buildMode`, `features` | |
| `package` | `packageId`, `name`, `lfVersion`, `sdkVersion` | `version` (present whenever the package declares one, which upgradable packages always do) |
| source | `id`, `module`, `path`, `sha256` | `uri` |
| span | `id`, `source`, `kind`, `start`, `end` | |
| symbol | `id`, `kind`, `module`, `name`, `qualifiedName` | `parent`, `span`, `source`, `lfRef`, `type` |
| value slot | `id`, `symbol`, `kind`, `availability` | `name`, `type` |
| failure site | `id`, `symbol`, `source`, `kind`, `start`, `end` | `message`, `errorId` |
| step | `id`, `symbol`, `index`, `source`, `start`, `end` | |

Arrays MAY be empty. `features` lists the sections the producer emitted, so
consumers can distinguish "not supported by this producer" from "supported
and empty".

## 3. `sources`

One entry per package module whose source file resolved under the package
source root at build time.

- `path` is package-relative (relative to the `source:` root of
  `daml.yaml`). Producers MUST NOT emit absolute local paths.
- `sha256` is the hash of the **exact bytes the compiler read**, with no
  normalization, verified by consumers before trusting spans. A checkout
  with translated line endings (git `core.autocrlf` on Windows) has
  different bytes, so consumers SHOULD retry with newline-normalized
  content and, on a match, report a line-ending mismatch instead of a
  generic hash failure.
- `module` gives consumers the module-to-file mapping directly (Daml-LF
  does not serialize module source paths).
- `uri` is informative display material only. Its authority is the package
  name, which is not unique, so consumers MUST NOT use `uri` as a
  resolution key. `path` plus `sha256` are the normative identifiers.
- Modules whose sources did not resolve under the source root (files
  included from outside it, generated modules) are listed by name in the
  top-level `unmappedModules` array, so "no mapping" is distinguishable
  from "no such module".

## 4. `spans`

Kinds emitted by the reference producer: `template-definition`,
`choice-definition`, `interface-definition`, `interface-method-definition`,
`exception-definition`, `data-type-definition`, `value-definition`, and
`<slot-kind>-expression` for located value-slot expressions (for example
`signatories-expression`). Spans are only emitted when they provably belong
to the module's own source file. Cross-module inlined spans are dropped
rather than mislabeled.

Position semantics (1-based, end-exclusive columns, code points) are defined
in section 2 and apply to every `start`/`end` pair in the document.

## 5. `symbols`

```json
{ "id": "sym:Asset:Asset:Transfer", "kind": "choice",
  "module": "Asset", "name": "Transfer",
  "qualifiedName": "Asset:Asset.Transfer",
  "parent": "sym:Asset:Asset", "span": "span:Asset:Asset:Transfer",
  "source": "src:Asset",
  "lfRef": { "packageId": "<pkg>", "module": "Asset",
             "entity": "Asset", "choice": "Transfer" } }
```

- Kinds: `module`, `template`, `choice`, `interface`, `interface-choice`,
  `interface-method`, `exception`, `record`, `variant`, `enum`, `value`.
- `interface-method` and its span kind refer to the declaration inside the
  interface definition. Implementation spans in a template's interface
  instance are not emitted in v1 (`interface-instance-method` is reserved).
- `qualifiedName` conventions: `Module:Entity` (templates, interfaces,
  types, values) and `Module:Entity.Choice` (choices), matching the
  identifiers that appear in Ledger API events, completions, and error
  messages.
- `lfRef` is the Daml-LF reference where one exists. Tools can join it
  against transaction data without string heuristics.
- `type` (optional) is the rendered LF type for values, methods, and slots.
- Compiler-generated definitions (names containing `$`) are excluded.
- Data types that merely back a template payload, exception, or choice
  argument are not repeated as standalone symbols. Their fields surface as
  value slots of the owning symbol.

## 6. `valueSlots` and availability

`availability` says what a tool is allowed to show. A `transaction-visible`
slot is populatable from participant-visible transaction data by a party
entitled to see the event. An `interpreter-only` slot is observable only
with interpreter or runtime support: trace-only tools MUST NOT claim it
from transaction data, and should show the slot with the value marked as
not captured. (`source-only` and `not-tracked` are reserved.) The
availability of each slot kind is normative, so a validator can check
conformance:

| Slot kind | Availability | Populated from |
| --- | --- | --- |
| `contract-payload-field` | `transaction-visible` | `CreatedEvent.create_arguments` |
| `choice-argument` | `transaction-visible` | `ExercisedEvent.choice_argument` |
| `choice-argument-field` | `transaction-visible` | `ExercisedEvent.choice_argument` |
| `choice-result` | `transaction-visible` | `ExercisedEvent.exercise_result` |
| `self-contract-id` | `transaction-visible` | event `contract_id` |
| `choice-controllers` | `transaction-visible` | `ExercisedEvent.acting_parties` |
| `choice-observers` | `interpreter-only` | not exposed as a Ledger API event field |
| `choice-authorizers` | `interpreter-only` | not exposed as a Ledger API event field |
| `signatories` | `transaction-visible` | `CreatedEvent.signatories` |
| `observers` | `transaction-visible` | `CreatedEvent.observers` |
| `precondition` | `interpreter-only` | evaluated at interpretation time only |
| `contract-key` | `transaction-visible` | `CreatedEvent.contract_key` |
| `key-maintainers` | `interpreter-only` | maintainers are a subset of signatories but are not identified as maintainers in event data |
| `interface-view` | `transaction-visible` | `CreatedEvent.interface_views` |
| `exception-message` | `interpreter-only` | only failure text reaches completions |

A producer MUST NOT assign a more permissive availability than this table,
and a validator MUST flag violations.

## 7. `failureSites`

An indexed table of the places a package can fail with a user-relevant
message, so failed completions can be joined to source without free-text
search:

```json
{ "id": "site:Asset:Asset:Transfer:0",
  "symbol": "sym:Asset:Asset:Transfer", "source": "src:Asset",
  "kind": "fail-with-status", "errorId": "AssetError.NotOwner",
  "message": "only the owner can transfer",
  "start": {"line": 31, "column": 5}, "end": {"line": 31, "column": 62} }
```

- Kinds: `abort`, `error`, `assert`, `ensure`, `fail-with-status`, `throw`.
- `errorId` is present when the site is a `failWithStatus` call with a
  statically known error id. `message` is present when the failure text is a
  string literal at the site. Both are omitted when the value is computed
  dynamically.
- `ensure` sites point at the template precondition expression, joinable
  from precondition-failure errors via the owning template symbol.
- Consumers join a failed completion to a site in order of strength: exact
  `errorId`, exact static `message`, then heuristic substring. Each
  degradation MUST be labeled, so a heuristic match is never presented as
  exact.
- `failWithStatus` is the primary failure mechanism of this section,
  matching Daml 3.x, where user-defined exceptions are deprecated. `throw`
  sites exist for packages that still use exceptions.

Producers that emit this section include `failure-sites` in
`producer.features`.

## 8. `steps`

Deterministic evaluation step descriptors: the source spans of the compiled
Daml-LF expression's location nodes in **pre-order**, per owning symbol
(choice bodies and top-level values, which include Daml Script entry
points).

- `index` is **document order** (pre-order of the compiled expression), not
  execution order. Consumers MUST NOT render steps as a timeline.
- De-duplication: when the same `(source, start, end)` span occurs more than
  once in pre-order, only the first occurrence is kept.
- Step ids are stable for a given package id, so runtime events and
  breakpoints can reference them portably. Runtime events join these spans
  by `(packageId, module, start, end)`: under smart contract upgrades,
  several versions of a package with identical module names and spans can
  coexist in one run, so the package id is part of the key.
- Known limitation: create-time expressions (signatories, observers,
  precondition, key) have spans (section 4) but no steps in v1, so stepping
  through a create does not stop inside them.

## 9. Runtime debug-trace format (JSONL)

`daml script --debug-trace-file <file>` writes one JSON object per line.
Consumers MUST ignore unknown `event` kinds and unknown fields. Events are
strictly sequential (Daml Script executes sequentially), and scripts in one
run do not interleave.

```
{"event":"script-start","script":"Asset:test_transfer"}
{"event":"trace","message":"...","location":{LOC}}
{"event":"warning","message":"...","location":{LOC}}
{"event":"question","name":"Submit","version":1,"stackTrace":[{LOC},...]}
{"event":"submission","actAs":["Alice::1220.."],"readAs":[],"location":{LOC}}
{"event":"created","templateId":"<pkgid>:Asset:Asset","contractId":"00..",
 "argument":{...}}
{"event":"exercised","templateId":"<pkgid>:Asset:Asset","interfaceId":null,
 "choice":"Transfer","contractId":"00..","argument":{...},"result":...}
{"event":"step","location":{LOC}}
{"event":"script-end","status":"success"}
{"event":"script-end","status":"error","error":"...","location":{LOC}}
```

`LOC = {"packageId","module","definition","startLine","startCol","endLine",
"endCol"}` with 1-based positions and end-exclusive columns (the runtime's
0-based locations are normalized by the emitter), matching
`daml-debug-info/v1` spans. `definition` is the unqualified name of the
top-level definition within `module` (the `script-start` field
`Module:definition` is its qualified form). `location` may be `null` when
the runtime has no location. Trace locations join metadata spans and steps
by `(packageId, module, start, end)`, as in section 8.

**Step events.** A `step` event reports that evaluation reached a source
location. Ordinary builds do not produce them: they come from a debug
build, which a producer emits on request (`daml build --debug` in the
reference implementation) and which carries a marker at each source
location the metadata records in `steps`. A debugger joins a `step` event
to the `steps` table by `(packageId, module, start, end)` as in section 8,
so it can show the line about to run.

Because the markers are compiled into the package, a debug build has a
different package id from an ordinary build of the same source, in the same
way a `-g -O0` binary differs from a release binary. A debug build is for
local development. Metadata emission itself never changes the package id
(section 2), so the two concerns stay separate: any build can carry
metadata, and only a debug build can be stepped through.

A consumer that receives `step` events synchronously MAY delay returning
from the callback in order to pause evaluation, which is how a debugger
implements breakpoints without the interpreter needing to know about them.

**Value encoding.** `argument` and `result` values are encoded with the
daml-lf API JSON codec (`ApiCodecCompressed`, the compact encoding used by
the JSON API for LF values): records as objects, Numerics, dates,
timestamps, parties, and contract ids as strings, and that codec's rules for
optionals and variants. If a value resists that encoding, the producer falls
back to the value's textual rendering as a single JSON string, and consumers
MUST tolerate that fallback.

**Modes.** The `submission`, `created`, and `exercised` events are emitted
in IDE-ledger runs. Against a real participant over gRPC, the reference
implementation emits the script-level events only. The ignore-unknown-events
rule makes adding ledger-mode emission later backward compatible.

**Stability.** `question` event names and versions (for example `Submit`)
are Daml Script internals with no stability guarantee. They are informative,
and consumers MUST NOT build behavior on specific question names.

**Sensitivity.** Trace values come from a local script run and never
contain data the submitting context could not see, but they do contain
ledger data (party identifiers, payloads). Treat trace files as sensitive
and keep them out of version control. Local variables and intermediate
expression values are **not** captured. Consumers must present them as
`interpreter-only`, not invent them.

## 10. Compiler changes in the reference implementation

Beyond the emitter itself, two compiler fixes were needed to make
choice-level debugging possible, both in the GHC to Daml-LF conversion:

1. `chcLocation` was hard-coded to `Nothing`. It is now populated from the
   desugared choice binder's source span, so choices carry
   `choice-definition` spans.
2. Choice update expressions had **all** source locations stripped
   (`removeLocations`) as a side effect of a structural match. Locations are
   now preserved in the choice body, which both improves runtime stack
   traces and provides `steps` for choices.

Both fixes are **unconditional** compiler changes, kept independent of the
debug flag so that the flag itself never alters the compiled output (the
section 2 producer invariant). Two consequences follow:

- Packages built with a fixed compiler have different package ids than
  stock-compiler builds of the same source, as with any compiler change.
  Choice `steps` therefore exist only for packages built with the fixes.
  Stepping cannot be retrofitted onto older builds, whose choice bodies
  contain no location nodes.
- Preserved locations are semantically transparent but not free: they add
  location nodes to the serialized package and to interpretation. The
  upstream pull requests include DALF size and interpreter overhead
  measurements, so maintainers can judge the cost with data.

## 11. Verification

These rules define what it means for a `daml-debug-info/v1` file to be
correct. They are what a verifier checks and what a consumer should apply
before trusting a file. Verification has three levels: the JSON Schema
covers shape, the rules below cover internal consistency and meaning, and
the strongest checks compare the file against the artifacts it describes
(the package id against the DAR, the source hashes against the files on
disk, and every span against the file it names).

A producer SHOULD be able to run these checks over its own output, so an
emission bug is caught where it is introduced rather than by a consumer
later.

- Reject unsupported major schema versions. Ignore unknown fields
  otherwise.
- Verify `package.packageId` against the DAR or DALF, and verify that
  sidecar and DAR-member copies are byte-identical. Treat divergence as
  invalid metadata.
- Verify source `sha256` before trusting spans. On mismatch, retry with
  newline-normalized content and report a line-ending mismatch
  specifically. Otherwise degrade to span-less symbol information.
- Reject absolute source paths (strict mode) or warn (lenient mode).
- Check availability labels against the table in section 6, and never populate an
  `interpreter-only` slot from transaction data.
- Resolve metadata strictly by package id, never by package name.
- Treat all metadata as advisory, trusted as much as its distribution
  channel. For packages the consumer did not build, the strong check is to
  rebuild from source and compare package ids.

## 12. Reference implementation status

The reference implementation
(`walnuthq/daml@feature/debug-info`,
`walnuthq/dpm-trace@feature/debug-info`) emits the
following today: `source-spans`, `symbols`, `lf-refs`, `value-slots`,
`steps`, the informative `compatibility` object, the sidecar and DAR-member
copies rendered from one serialization, and the section 9 runtime trace with
IDE-ledger event emission.

Specified here but not yet emitted: `failureSites` (section 7),
`unmappedModules` (section 3), debug builds and the `step` events they
produce (section 9), the package-id invariance CI test (section 2),
reclassifying `choice-observers` and `key-maintainers` to
`interpreter-only` in the emitter (the prototype labels them
`transaction-visible`), and optional ledger-mode emission of trace events
(section 9).

Consumers should detect what a given artifact contains from
`producer.features`, not from this list, which describes one implementation
as of this writing.
