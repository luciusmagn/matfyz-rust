const slides = [...document.querySelectorAll("section.level2")];
let current = Math.max(0, slides.findIndex(slide => `#${slide.id}` === location.hash));
const bullets = slides.map(slide => [...slide.querySelectorAll('li')]);
let revealed = 0;

function reveal(count) {
  revealed = Math.max(0, Math.min(bullets[current].length, count));
  bullets[current].forEach((bullet, index) => {
    const hidden = index >= revealed;
    bullet.classList.toggle('pending', hidden);
    bullet.inert = hidden;
  });
}
function advance() {
  if (revealed < bullets[current].length) reveal(revealed + 1);
  else if (current < slides.length - 1) show(current + 1);
}
function retreat() {
  if (revealed > 0) reveal(revealed - 1);
  else if (current > 0) show(current - 1, true);
}

// Preserve the composition and relative type sizes on any projector resolution.
function resize() {
  const scale = Math.min(window.innerWidth / 1280, window.innerHeight / 720);
  document.documentElement.style.setProperty("--slide-scale", scale);
}
function show(index, complete = false) {
  current = Math.max(0, Math.min(slides.length - 1, index));
  slides.forEach((slide, number) => slide.classList.toggle("active", number === current));
  reveal(complete ? bullets[current].length : 0);
  history.replaceState(null, "", `#${slides[current].id}`);
}
document.addEventListener("keydown", event => {
  if (event.altKey || event.ctrlKey || event.metaKey ||
      event.target.closest("textarea, input, select, [contenteditable]")) return;
  let action;
  if (["ArrowRight", "PageDown", " "].includes(event.key)) action = advance;
  if (["ArrowLeft", "PageUp"].includes(event.key)) action = retreat;
  if (event.key === "Home") action = () => show(0);
  if (event.key === "End") action = () => show(slides.length - 1, true);
  if (action) {
    event.preventDefault();
    if (document.activeElement instanceof HTMLElement) document.activeElement.blur();
    action();
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
