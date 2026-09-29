// Small, dependency-free presentation controls. The Markdown is the source.
const slides = [...document.querySelectorAll("section.level2")];
const position = document.getElementById("position");
const previous = document.getElementById("previous");
const next = document.getElementById("next");
let current = Math.max(0, slides.findIndex(slide => `#${slide.id}` === location.hash));

function show(index) {
  current = Math.max(0, Math.min(slides.length - 1, index));
  slides.forEach((slide, number) => slide.classList.toggle("active", number === current));
  position.textContent = `${current + 1} / ${slides.length}`;
  previous.disabled = current === 0;
  next.disabled = current === slides.length - 1;
  history.replaceState(null, "", `#${slides[current].id}`);
}
previous.addEventListener("click", () => show(current - 1));
next.addEventListener("click", () => show(current + 1));
document.addEventListener("keydown", event => {
  if (event.target.closest("a, button, textarea, input")) return;
  if (["ArrowRight", "PageDown", " "].includes(event.key)) {
    event.preventDefault(); show(current + 1);
  } else if (["ArrowLeft", "PageUp"].includes(event.key)) {
    event.preventDefault(); show(current - 1);
  }
});
window.addEventListener("hashchange", () => {
  const index = slides.findIndex(slide => `#${slide.id}` === location.hash);
  if (index >= 0) show(index);
});

// Playground runs on its own site, so the presentation needs no server-side
// code execution and the printed version always includes the original code.
for (const pre of document.querySelectorAll("pre.runnable")) {
  const code = pre.querySelector("code").textContent;
  const actions = document.createElement("div");
  actions.className = "example-actions";
  const run = document.createElement("button");
  run.type = "button";
  run.textContent = "Edit and run in Rust Playground";
  run.addEventListener("click", () => {
    const url = new URL("https://play.rust-lang.org/");
    url.searchParams.set("edition", "2021");
    url.searchParams.set("code", code);
    window.open(url, "_blank", "noopener");
  });
  actions.append(run);
  pre.after(actions);
}
show(current);
