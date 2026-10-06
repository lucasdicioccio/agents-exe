// Search box of the `documentation` layout: looks the typed words up in the
// headings and text of every documentation page (json/doc-search.json).
(function(){
  const root = document.getElementById("doc-search");
  if (!root) { return; }
  const indexUrl = root.dataset.index;
  const maxResults = 12;

  const input = document.createElement("input");
  input.type = "search";
  input.className = "doc-search-input";
  input.placeholder = "Search the documentation ( / )";
  input.setAttribute("aria-label", "Search the documentation");
  input.autocomplete = "off";
  const results = document.createElement("ul");
  results.className = "doc-search-results";
  results.hidden = true;
  root.appendChild(input);
  root.appendChild(results);

  let entries = null; // null until fetched
  let loading = null;
  let selected = -1;

  const load = () => {
    if (loading === null) {
      loading = fetch(indexUrl)
        .then((res) => res.ok ? res.json() : [])
        .catch(() => [])
        .then((xs) => {
          entries = xs.map((e) => ({
            entry: e,
            title: (e.title ?? "").toLowerCase(),
            heading: (e.heading ?? "").toLowerCase(),
            text: (e.text ?? "").toLowerCase(),
          }));
        });
    }
    return loading;
  };

  // every word must be found; a word found in a heading or a title weighs
  // more than one found in the text only
  const score = (item, words) => {
    let total = 0;
    for (const w of words) {
      let s = 0;
      if (item.heading.includes(w)) { s += 6; }
      if (item.title.includes(w)) { s += item.entry.heading ? 2 : 8; }
      if (item.text.includes(w)) { s += 1; }
      if (s === 0) { return 0; }
      total += s;
    }
    return total;
  };

  const search = (query) => {
    const words = query.toLowerCase().split(/\s+/).filter((w) => w.length > 0);
    if (words.length === 0 || entries === null) { return { words, hits: [] }; }
    const hits = entries
      .map((item, position) => ({ item, position, score: score(item, words) }))
      .filter((hit) => hit.score > 0)
      .sort((a, b) => b.score - a.score || a.position - b.position)
      .slice(0, maxResults)
      .map((hit) => hit.item);
    return { words, hits };
  };

  // the text around the first word found, with the words highlighted
  const snippet = (item, words) => {
    const text = item.entry.text ?? "";
    const firsts = words.map((w) => item.text.indexOf(w)).filter((i) => i >= 0);
    const start = Math.max(0, (firsts.length > 0 ? Math.min(...firsts) : 0) - 40);
    const excerpt = text.slice(start, start + 160);
    const lower = excerpt.toLowerCase();
    const p = document.createElement("span");
    p.className = "doc-search-snippet";
    if (start > 0) { p.append("…"); }
    let pos = 0;
    while (pos < excerpt.length) {
      let next = -1;
      let len = 0;
      for (const w of words) {
        const i = lower.indexOf(w, pos);
        if (i >= 0 && (next < 0 || i < next)) { next = i; len = w.length; }
      }
      if (next < 0) { break; }
      p.append(excerpt.slice(pos, next));
      const mark = document.createElement("mark");
      mark.textContent = excerpt.slice(next, next + len);
      p.append(mark);
      pos = next + len;
    }
    p.append(excerpt.slice(pos));
    if (start + 160 < text.length) { p.append("…"); }
    return p;
  };

  const select = (i) => {
    const items = [...results.querySelectorAll("a")];
    selected = items.length === 0 ? -1 : (i + items.length) % items.length;
    items.forEach((a, j) => a.classList.toggle("doc-search-selected", j === selected));
    if (selected >= 0) { items[selected].scrollIntoView({ block: "nearest" }); }
  };

  const render = () => {
    const { words, hits } = search(input.value);
    results.replaceChildren();
    selected = -1;
    if (words.length === 0) { results.hidden = true; return; }
    if (hits.length === 0) {
      const li = document.createElement("li");
      li.className = "doc-search-empty";
      li.textContent = entries === null ? "Loading…" : "No result";
      results.appendChild(li);
    }
    for (const item of hits) {
      const e = item.entry;
      const li = document.createElement("li");
      const a = document.createElement("a");
      a.href = e.url;
      const title = document.createElement("span");
      title.className = "doc-search-title";
      title.textContent = e.heading ? e.heading : e.title;
      const where = document.createElement("span");
      where.className = "doc-search-page";
      where.textContent = [e.group, e.heading ? e.title : null].filter((x) => x).join(" › ");
      a.append(title, where, snippet(item, words));
      li.appendChild(a);
      results.appendChild(li);
    }
    results.hidden = false;
    if (hits.length > 0) { select(0); }
  };

  const close = () => { results.hidden = true; };

  input.addEventListener("focus", () => { load().then(render); });
  input.addEventListener("input", () => { render(); load().then(render); });
  input.addEventListener("keydown", (ev) => {
    if (ev.key === "ArrowDown") { ev.preventDefault(); select(selected + 1); }
    else if (ev.key === "ArrowUp") { ev.preventDefault(); select(selected - 1); }
    else if (ev.key === "Enter") {
      const items = results.querySelectorAll("a");
      if (selected >= 0 && items[selected]) { ev.preventDefault(); close(); window.location.href = items[selected].href; }
    }
    else if (ev.key === "Escape") { close(); input.blur(); }
  });
  // a result may lead to another heading of the current page
  results.addEventListener("click", (ev) => { if (ev.target.closest("a")) { close(); } });
  document.addEventListener("click", (ev) => { if (!root.contains(ev.target)) { close(); } });
  document.addEventListener("keydown", (ev) => {
    const typing = ["INPUT", "TEXTAREA", "SELECT"].includes(ev.target.tagName) || ev.target.isContentEditable;
    if (ev.key === "/" && !typing && !ev.ctrlKey && !ev.metaKey && !ev.altKey) {
      ev.preventDefault();
      input.focus();
      input.select();
    }
  });

  // on a narrow screen the list of pages is above the content: start it folded
  const foldNavigation = () => {
    const menu = document.querySelector(".doc-nav-menu");
    if (menu && window.matchMedia("(max-width: 800px)").matches) { menu.open = false; }
  };
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", foldNavigation);
  } else {
    foldNavigation();
  }
})()
