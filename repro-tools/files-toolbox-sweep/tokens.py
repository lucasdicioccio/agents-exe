"""Token cost per variant: sum of prompt and completion tokens over a run's
chat completions, read from the TokenUsage lines of each run's raw.log (agents-exe --log-file).
usage: tokens.py <sweep-out-dir>"""
import collections, glob, os, re, statistics, sys

S = sys.argv[1] if len(sys.argv) > 1 else os.environ.get("SWEEP_OUT", ".")
U = re.compile(r"TokenUsage \{tokenPromptTokens = (\d+), tokenCompletionTokens = (\d+)")
rows = collections.defaultdict(list)
for W in sorted(glob.glob(os.path.join(S, "runs", "*-t?-r[123]"))):
    v = re.match(r"(.*)-t\d-r\d$", os.path.basename(W)).group(1)
    p = os.path.join(W, "raw.log")
    if not os.path.exists(p):
        continue
    prompt = compl = peak = 0
    for m in U.finditer(open(p, errors="replace").read()):
        pr, c = int(m.group(1)), int(m.group(2))
        prompt += pr; compl += c; peak = max(peak, pr)
    rows[v].append((prompt, compl, peak))
print("| variant | prompt tokens/run (median) | completion tokens/run (median) | largest single prompt (median) |")
print("|---|---|---|---|")
for v, rs in sorted(rows.items()):
    print(f"| {v} | {statistics.median(r[0] for r in rs):,.0f} | {statistics.median(r[1] for r in rs):,.0f} | {statistics.median(r[2] for r in rs):,.0f} |")
