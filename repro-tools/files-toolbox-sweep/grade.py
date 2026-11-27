import json, os, re, sys

# The untouched fixture lives at <out>/fixture.hs, next to <out>/runs/<run>/.
ORIG = None


def load_fixture(W):
    global ORIG
    p = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(W))), "fixture.hs")
    ORIG = open(p).read().split("\n")


def nb(lines):
    return [l.rstrip() for l in lines if l.strip() != ""]


def expected(task):
    L = list(ORIG)
    if task == 1:
        start = next(i for i, l in enumerate(L) if l.startswith("buildDeveloperToolParams ::"))
        end = start + 2
        while end < len(L) and not (L[end] and not L[end][0].isspace() and not L[end].startswith("buildDeveloperToolParams")):
            end += 1
        for i in range(start, end):
            L[i] = re.sub(r"\bhasCapability\b", "hasCap", L[i])
        return L
    if task == 2:
        last = max(i for i, l in enumerate(L) if l.startswith("import "))
        return L[: last + 1] + ["import Data.Maybe (fromMaybe)"] + L[last + 1:]
    if task == 3:
        return [l for l in L if not l.startswith("devCapabilityToText")]
    if task == 4:
        return [l.replace("Include subdirectories recursively (default: false)", "Include subdirectories recursively (default: true)") for l in L]
    return L


def grade(W, task):
    load_fixture(W)
    work = os.path.join(W, "work")
    got = open(os.path.join(work, "ToolRegistration.hs")).read().split("\n")
    ok_file = nb(got) == nb(expected(task))
    if task == 3 and not ok_file:
        # also accept removing the function's own leading comment line
        alt = [l for l in expected(3) if l.strip() != "-- Helper to convert developer capability to text"]
        ok_file = nb(got) == nb(alt)
    if task == 4 and not ok_file:
        # two list-directory 'recursive' params exist (system and developer); changing either one alone is accepted too
        idx = [i for i, l in enumerate(ORIG) if "Include subdirectories recursively (default: false)" in l]
        for i in idx:
            alt = list(ORIG)
            alt[i] = alt[i].replace("(default: false)", "(default: true)")
            if nb(got) == nb(alt):
                ok_file = True
        # also accept flipping the help text's 'recursive' default along with the params
        g, o = nb(got), nb(ORIG)
        if not ok_file and len(g) == len(o):
            diffs = [(a, b) for a, b in zip(o, g) if a != b]
            ok_file = bool(diffs) and all("ecurs" in a and a.replace("(default: false)", "(default: true)") == b for a, b in diffs)
    if task == 5:
        p = os.path.join(work, "notes", "TODO.md")
        if not os.path.exists(p):
            return False, "no TODO.md"
        bl = [l for l in open(p).read().split("\n") if re.match(r"^\s*[-*+] \S", l)]
        return ok_file and len(bl) == 3, f"bullets={len(bl)}"
    if ok_file:
        return True, ""
    # summarise diff size
    import difflib
    d = [l for l in difflib.unified_diff(nb(expected(task)), nb(got), lineterm="", n=0) if l[:1] in "+-" and l[:3] not in ("+++", "---")]
    return False, f"diff-vs-expected {len(d)} lines: " + " | ".join(x[:90] for x in d[:6])


def calls(W):
    p = os.path.join(W, "session.json")
    if not os.path.exists(p):
        return None
    d = json.load(open(p))
    out = []
    for t in reversed(d["turns"]):
        for pair in t["contents"].get("userToolResponses") or []:
            call, res = pair[0], pair[1]
            try:
                args = json.loads(call["function"]["arguments"])
            except Exception:
                args = {"_raw": call["function"]["arguments"]}
            content = res.get("content") if isinstance(res, dict) else res
            out.append((args, content))
    return out


def summarize(args, content):
    cap = args.get("capability", "?")
    s = json.dumps(content)
    err = None
    if isinstance(content, str):
        err = content[:160]
    elif isinstance(content, dict) and any(k in content for k in ("error", "errorMessage")):
        err = s[:160]
    elif isinstance(content, dict) and content.get("tag") in ("Error", "ToolError"):
        err = s[:160]
    extra = ""
    if cap == "read-file-range":
        r = args.get("ranges")
        extra = f" ranges={r!r} meta={args.get('metadata_only')} bytes={len(s)}"
    elif cap == "write-file-range":
        extra = f" ranges={args.get('ranges')!r}"
    return cap, err, extra, len(s)


if __name__ == "__main__":
    W = sys.argv[1]
    task = int(re.search(r"-t(\d)-", W).group(1))
    ok, why = grade(W, task)
    cs = calls(W) or []
    print(f"== {os.path.basename(W)} correct={ok} {why} calls={len(cs)} exit={open(os.path.join(W,'exit.txt')).read().strip() if os.path.exists(os.path.join(W,'exit.txt')) else '?'}")
    for a, c in cs:
        cap, err, extra, n = summarize(a, c)
        print(f"   {cap}{extra}" + (f"  ERR: {err}" if err else ""))
