import os, sys, glob, re, json, statistics, collections
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import grade

S = sys.argv[1] if len(sys.argv) > 1 else os.environ.get("SWEEP_OUT", ".")
rows = collections.defaultdict(list)
for W in sorted(glob.glob(os.path.join(S, "runs", "*-t?-r[123]"))):
    b = os.path.basename(W)
    m = re.match(r"(.*)-t(\d)-r(\d)", b)
    v, t = m.group(1), int(m.group(2))
    ok, _ = grade.grade(W, t)
    cs = grade.calls(W) or []
    errs = []
    whole = 0
    for a, c in cs:
        cap, err, extra, n = grade.summarize(a, c)
        if err and "HELP DOCUMENTATION" not in err:
            if "Tool not found" in err:
                errs.append("notfound")
            elif "Missing 'contentBlocks'" in err:
                errs.append("no-contentBlocks")
            elif "Missing 'path'" in err:
                errs.append("no-path")
            elif "PatchContextMismatch" in err or "Context doesn't match" in err:
                errs.append("patch-ctx")
            else:
                errs.append(err[:60])
        if cap == "read-file-range" and n > 50000:
            whole += 1
    rows[(v, t)].append(dict(ok=ok, one=ok and not errs, calls=len(cs), errs=errs, whole=whole))

variants = sorted({v for v, _ in rows})
for v in variants:
    print(f"### {v}")
    print("| task | correct | one-shot | calls (median, per run) | whole-file reads | errors |")
    print("|---|---|---|---|---|---|")
    tot_one = tot = 0
    for t in range(1, 6):
        rs = rows.get((v, t), [])
        if not rs:
            continue
        c = collections.Counter(e for r in rs for e in r["errs"])
        tot += len(rs); tot_one += sum(r["one"] for r in rs)
        print(f"| {t} | {sum(r['ok'] for r in rs)}/{len(rs)} | {sum(r['one'] for r in rs)}/{len(rs)} | {statistics.median(r['calls'] for r in rs)} ({','.join(str(r['calls']) for r in rs)}) | {sum(r['whole'] for r in rs)} | {', '.join(f'{k}x{n}' for k, n in c.items()) or '-'} |")
    print(f"one-shot total: {tot_one}/{tot}\n")
