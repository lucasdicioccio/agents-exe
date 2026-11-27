# Files toolbox one-shot sweep

A small harness that checks how well a ~25B-class local model edits files with
the DeveloperToolbox file capabilities (`read-file-range`, `write-file-range`,
`patch-file`). Each task runs in a fresh `agents-exe run` session against a
copy of a 3057-line Haskell file, and the resulting file is graded against the
expected edit.

## Running it

```sh
cabal build exe:agents-exe
export AGENTS_EXE=$(cabal list-bin exe:agents-exe)
export SWEEP_OUT=/some/scratch/dir      # runs, logs and sessions land here
export BOXNAME=fs                       # toolbox Name (see finding 1)
# model, defaults shown; API_KEY_ID is an id in the agents-exe secret-keys file
export MODEL_URL=http://mac.home:8000/v1 MODEL_NAME=Qwen/Qwen3.6-35B-A3B API_KEY_ID=workbench-key
repro-tools/files-toolbox-sweep/sweep.sh main-fs 1 2 3   # label, then reps
python3 repro-tools/files-toolbox-sweep/summary.py "$SWEEP_OUT"
python3 repro-tools/files-toolbox-sweep/tokens.py "$SWEEP_OUT"
```

`sweep.sh` writes the fixture (`src/System/Agents/ToolRegistration.hs` at
`FIXTURE_REV`, default `4471a94`) to `$SWEEP_OUT/fixture.hs` once, then runs
tasks 1 to 5 for each rep. Runs are sequential: `run1.sh` moves the newest
saved session out of `~/.config/agents-exe/sessions` into the run directory.
To compare two builds, run the sweep once per binary with a different label.

## Tasks

1. Rename the local `hasCapability` in `buildDeveloperToolParams` to `hasCap`
   and update its uses (read a range, multi-line edit).
2. Add an import of `Data.Maybe (fromMaybe)` after the last import (`N+`).
3. Delete the function `devCapabilityToText` and its type signature.
4. Change the default of `recursive` in list-directory's description to say
   'default: true', with `patch-file`.
5. Create `notes/TODO.md` with three bullet points (`head`).

The prompt gives the file's absolute path. Grading ignores blank lines. Task 3
also accepts deleting the function's own `--` comment. Task 4 accepts flipping
either of the two `recursive` params, or the help text as well. **One-shot**
means a correct file and no tool call that returned an error. The bar is 4/5
tasks one-shot per rep.

## Results of sweep #1 (Qwen/Qwen3.6-35B-A3B, 5 tasks x 3 reps)

Builds:
- **before**: `48fde0c`, main just before #638 (schema and help fixes).
- **after**: `4471a94`, with #638 and #640, without #642.
- **main**: `7f62372`, with #642 (`read-file-range` caps a whole-file read at
  400 lines / 32 KB and says how to page).

| build | toolbox `Name` | one-shot | per rep | prompt tokens/run (median) | largest prompt (median) |
|---|---|---|---|---|---|
| before | `developer` | 9/15 | 4, 3, 2 | 96k | 48k |
| after | `developer` | 5/15 | 3, 0, 2 | 69k | 7k |
| before | `fs` | 11/15 | 4, 4, 3 | 96k | 48k |
| after | `fs` | 12/15 | 5, 3, 4 | 98k | 48k |
| main | `fs` | 9/15 | 3, 2, 4 | **38k** | **15k** |

(The `after`/`developer` token numbers are low because many runs gave up
early on a wrong tool name.)

**main, Name `fs`**

| task | correct | one-shot | calls per run | errors |
|---|---|---|---|---|
| 1 rename | 2/3 | 0/3 | 12, 21, 20 | missing contentBlocks x3, block/range count x2, missing path, patch context, missing capability |
| 2 import | 2/3 | 2/3 | 3, 5, 3 | - (one run inserted one line too late) |
| 3 delete | 3/3 | 1/3 | 6, 7, 15 | missing contentBlocks x2 |
| 4 patch | 3/3 | 3/3 | 5, 3, 4 | - |
| 5 create | 3/3 | 3/3 | 1, 1, 1 | - |

**before, Name `fs`**

| task | correct | one-shot | calls per run | errors |
|---|---|---|---|---|
| 1 rename | 2/3 | 1/3 | 11, 13, 2 | patch context x3, block/range count x2, missing patch |
| 2 import | 3/3 | 2/3 | 2, 2, 3 | patch context |
| 3 delete | 3/3 | 2/3 | 2, 2, 4 | patch context x2 |
| 4 patch | 3/3 | 3/3 | 2, 2, 2 | - |
| 5 create | 3/3 | 3/3 | 1, 1, 1 | - |

### Findings

1. **The tool name stutters and the model mistypes it.** The tool is
   `developer_<Name>_developer_tools`, so the documented `Name: "developer"`
   gives `developer_developer_developer_tools`. Qwen often calls
   `developer_developer_tools` instead. With Name `developer` this broke 7/15
   runs on **after** and 4/15 on **before**; with `fs`, 0/30.
2. **"Tool not found" gives no way back.** The error is
   `{"error": "Tool not found"}` with nothing else; models retried the same
   wrong name up to 20 times.
3. **#638 has no measurable effect on one-shot rate** with the name problem out
   of the way: 11/15 before, 12/15 after. At 15 runs a difference of one or two
   is noise.
4. **#642 cuts tokens by about 2.5x, and costs calls.** Whole-file reads used
   to send the full 163 KB file (about 48k prompt tokens) in nearly
   every run of tasks 1 to 4. On
   main no read result is over 24 KB, median prompt tokens per run drop from
   96k to 38k, and the largest single prompt from 48k to 15k. But the model has
   no way to find a symbol, so on tasks 1 and 3 it pages through the file blind
   (up to 14 reads in one run), and more calls means more chances for a malformed one.
   One-shot went from 11-12/15 to 9/15, within noise but in the wrong
   direction.
5. **`write-file-range` without `contentBlocks` is the main error on main**
   (5 calls in 15 runs, vs 2 on after and 0 on before). The model sends `ranges` alone, meaning
   "replace this line" or "delete these lines". The error message gets a
   correct retry every time, but it costs a call and the one-shot.

No build reaches the 4/5 bar in every rep.
