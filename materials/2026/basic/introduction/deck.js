const slides = [...document.querySelectorAll("section.level2")];
let current = Math.max(0, slides.findIndex(slide => `#${slide.id}` === location.hash));

// Preserve the composition and relative type sizes on any projector resolution.
function resize() {
  const scale = Math.min(window.innerWidth / 1280, window.innerHeight / 720);
  document.documentElement.style.setProperty("--slide-scale", scale);
}
function show(index) {
  current = Math.max(0, Math.min(slides.length - 1, index));
  slides.forEach((slide, number) => slide.classList.toggle("active", number === current));
  history.replaceState(null, "", `#${slides[current].id}`);
}
document.addEventListener("keydown", event => {
  if (event.altKey || event.ctrlKey || event.metaKey ||
      event.target.closest("textarea, input, select, [contenteditable]")) return;
  let index;
  if (["ArrowRight", "PageDown", " "].includes(event.key)) index = current + 1;
  if (["ArrowLeft", "PageUp"].includes(event.key)) index = current - 1;
  if (event.key === "Home") index = 0;
  if (event.key === "End") index = slides.length - 1;
  if (index !== undefined) {
    event.preventDefault();
    if (document.activeElement instanceof HTMLElement) document.activeElement.blur();
    show(index);
  }
});
window.addEventListener("hashchange", () => {
  const index = slides.findIndex(slide => `#${slide.id}` === location.hash);
  if (index >= 0) show(index);
});
window.addEventListener("resize", resize);

slides.forEach((slide, index) => {
  const number = document.createElement("span");
  number.className = "slide-number";
  number.textContent = `${index + 1} / ${slides.length}`;
  slide.append(number);
});

for (const pre of document.querySelectorAll("pre.runnable")) {
  const url = new URL("https://play.rust-lang.org/");
  url.searchParams.set("edition", "2021");
  url.searchParams.set("code", pre.querySelector("code").textContent);
  const run = document.createElement("a");
  run.className = "run-code";
  run.textContent = "▶";
  run.href = url.href;
  run.target = "_blank";
  run.rel = "noopener";
  run.title = "Run in Rust Playground";
  run.setAttribute("aria-label", "Run example in Rust Playground (new tab)");
  pre.parentElement.append(run);
}
resize();
show(current);
