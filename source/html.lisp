(in-package :matfyz)

(defmacro with-page ((&key title) &body body)
  "Generate a complete HTML page with standard layout."
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:meta :charset "UTF-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
       (:title ,title)
       (:style (:raw (page--get-css))))
      (:body
       (:div :class "container"
             ,@body
             ,(page--render-footer))))))

(defparameter *intro-2026-lessons*
  (list
   (list :id "L1"
         :topic "Ownership and borrowing"
         :lecture-dir "materials/2026/basic/lecture-01-ownership-borrowing"
         :lab-dir "materials/2026/basic/lab-01-ownership-borrowing"
         :hw-a "homeworks/2026/introductory/pair-01-ownership/hw-a-executable/README.md"
         :hw-b "homeworks/2026/introductory/pair-01-ownership/hw-b-library/README.md"
         :summary "Ownership model, moves, borrows, and API signatures that avoid unnecessary cloning."
         :snippet "fn first_word(s: &str) -> &str {\n    s.split_whitespace().next().unwrap_or(\"\")\n}")
   (list :id "L2"
         :topic "Enums, pattern matching, Option/Result"
         :lecture-dir "materials/2026/basic/lecture-02-enums-patterns"
         :lab-dir "materials/2026/basic/lab-02-enums-patterns"
         :hw-a "homeworks/2026/introductory/pair-02-patterns/hw-a-executable/README.md"
         :hw-b "homeworks/2026/introductory/pair-02-patterns/hw-b-library/README.md"
         :summary "Model finite states with enums and make invalid states explicit through exhaustive matching."
         :snippet "match cmd {\n    Command::Add(a, b) => a + b,\n    Command::Sub(a, b) => a - b,\n    Command::Print(_) => 0,\n}")
   (list :id "L3"
         :topic "Structs, modules, custom errors"
         :lecture-dir "materials/2026/basic/lecture-03-structs-modules-errors"
         :lab-dir "materials/2026/basic/lab-03-structs-modules-errors"
         :hw-a "homeworks/2026/introductory/pair-03-structs-errors/hw-a-executable/README.md"
         :hw-b "homeworks/2026/introductory/pair-03-structs-errors/hw-b-library/README.md"
         :summary "Design cohesive modules, domain structs, and typed error surfaces for predictable behavior."
         :snippet "#[derive(Debug)]\nenum ParseError {\n    MissingField,\n    InvalidNumber,\n}")
   (list :id "L4"
         :topic "Traits, generics, iterators"
         :lecture-dir "materials/2026/basic/lecture-04-traits-generics-iterators"
         :lab-dir "materials/2026/basic/lab-04-traits-generics-iterators"
         :hw-a "homeworks/2026/introductory/pair-04-generics/hw-a-executable/README.md"
         :hw-b "homeworks/2026/introductory/pair-04-generics/hw-b-library/README.md"
         :summary "Static polymorphism and iterator-driven transformations with clear trait bounds."
         :snippet "fn max_of<T: Ord + Copy>(a: T, b: T) -> T {\n    if a > b { a } else { b }\n}")
   (list :id "L5"
         :topic "Concurrency foundations"
         :lecture-dir "materials/2026/basic/lecture-05-concurrency-foundations"
         :lab-dir "materials/2026/basic/lab-05-concurrency-foundations"
         :hw-a "homeworks/2026/introductory/pair-05-concurrency/hw-a-executable/README.md"
         :hw-b "homeworks/2026/introductory/pair-05-concurrency/hw-b-library/README.md"
         :summary "Threads, channels, mutexes, and deterministic output in parallel workloads."
         :snippet "let (tx, rx) = std::sync::mpsc::channel();\ntx.send(10).unwrap();\nprintln!(\"{}\", rx.recv().unwrap());")
   (list :id "L6"
         :topic "Async foundations with Tokio"
         :lecture-dir "materials/2026/basic/lecture-06-async-tokio"
         :lab-dir "materials/2026/basic/lab-06-async-tokio"
         :hw-a "homeworks/2026/introductory/pair-06-async/hw-a-executable/README.md"
         :hw-b "homeworks/2026/introductory/pair-06-async/hw-b-library/README.md"
         :summary "Async tasks, runtime model, timeouts, and practical task orchestration."
         :snippet "#[tokio::main]\nasync fn main() {\n    let h = tokio::spawn(async { 42 });\n    println!(\"{}\", h.await.unwrap());\n}")
   (list :id "LX"
         :topic "Extension: performance and profiling"
         :lecture-dir "materials/2026/basic/lecture-07-extension-performance"
         :lab-dir "materials/2026/basic/lab-07-extension-performance"
         :hw-a nil
         :hw-b nil
         :summary "Optional block on benchmarking discipline, profiling basics, and performance methodology."
         :snippet "let start = std::time::Instant::now();\nlet sum: i64 = (0..1_000_000).sum();\nprintln!(\"{} {:?}\", sum, start.elapsed());")))

(defparameter *advanced-2026-lessons*
  (list
   (list :id "L1"
         :topic "Parallel programming and memory model"
         :lecture-dir "materials/2026/advanced/lecture-01-parallel-model"
         :lab-dir "materials/2026/advanced/lab-01-parallel-model"
         :hw-a "homeworks/2026/advanced/pair-01-parallel-model/hw-a-library/README.md"
         :hw-b "homeworks/2026/advanced/pair-01-parallel-model/hw-b-executable/README.md"
         :summary "Data races vs logic races, synchronization primitives, atomics, and correctness models."
         :snippet "while LOCK.compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed).is_err() {}\nLOCK.store(false, Ordering::Release);")
   (list :id "L2"
         :topic "Borrow-checker-driven API architecture"
         :lecture-dir "materials/2026/advanced/lecture-02-borrow-architecture"
         :lab-dir "materials/2026/advanced/lab-02-borrow-architecture"
         :hw-a "homeworks/2026/advanced/pair-02-borrow-architecture/hw-a-library/README.md"
         :hw-b "homeworks/2026/advanced/pair-02-borrow-architecture/hw-b-executable/README.md"
         :summary "Lifetime-aware API design, owner/view separation, and borrow-scope minimization."
         :snippet "fn prefixed<'a>(&'a self, pfx: &'a str) -> impl Iterator<Item = &'a str> + 'a")
   (list :id "L3"
         :topic "Declarative macros"
         :lecture-dir "materials/2026/advanced/lecture-03-macros"
         :lab-dir "materials/2026/advanced/lab-03-macros"
         :hw-a "homeworks/2026/advanced/pair-03-macros/hw-a-library/README.md"
         :hw-b "homeworks/2026/advanced/pair-03-macros/hw-b-executable/README.md"
         :summary "`macro_rules!` pattern design, repetition, hygiene, and diagnostics."
         :snippet "macro_rules! sum {\n    ($first:expr $(, $rest:expr)*) => {{ let mut acc = $first; $(acc += $rest;)* acc }};\n}")
   (list :id "L4"
         :topic "Library engineering and testing"
         :lecture-dir "materials/2026/advanced/lecture-04-library-engineering"
         :lab-dir "materials/2026/advanced/lab-04-library-engineering"
         :hw-a "homeworks/2026/advanced/pair-04-library-engineering/hw-a-library/README.md"
         :hw-b "homeworks/2026/advanced/pair-04-library-engineering/hw-b-executable/README.md"
         :summary "Public API contracts, error surfaces, deterministic tests, and semver-aware design."
         :snippet "#[derive(Debug)]\npub enum ParseError { Empty, InvalidNumber(String), Overflow }")
   (list :id "L5"
         :topic "Unsafe boundaries and sound wrappers"
         :lecture-dir "materials/2026/advanced/lecture-05-unsafe-boundaries"
         :lab-dir "materials/2026/advanced/lab-05-unsafe-boundaries"
         :hw-a "homeworks/2026/advanced/pair-05-unsafe/hw-a-library/README.md"
         :hw-b "homeworks/2026/advanced/pair-05-unsafe/hw-b-executable/README.md"
         :summary "How to keep unsafe code minimal, explicit, and wrapped by safe invariants."
         :snippet "unsafe { std::slice::from_raw_parts(ptr, len) }")
   (list :id "L6"
         :topic "Systems interfaces"
         :lecture-dir "materials/2026/advanced/lecture-06-systems-interfaces"
         :lab-dir "materials/2026/advanced/lab-06-systems-interfaces"
         :hw-a "homeworks/2026/advanced/pair-06-systems/hw-a-library/README.md"
         :hw-b "homeworks/2026/advanced/pair-06-systems/hw-b-executable/README.md"
         :summary "OS boundaries, process I/O, failure policy, and robust systems-level wrappers."
         :snippet "let output = std::process::Command::new(\"rustc\").arg(\"--version\").output()?;")
   (list :id "LX"
         :topic "Extension: performance and profiling"
         :lecture-dir "materials/2026/advanced/lecture-07-extension-performance"
         :lab-dir "materials/2026/advanced/lab-07-extension-performance"
         :hw-a nil
         :hw-b nil
         :summary "Optional block on benchmarking discipline, cache effects, and contention analysis."
         :snippet "c.bench_function(\"sum\", |b| b.iter(|| data.iter().copied().sum::<i64>()));")))

(defparameter *intro-2026-resources*
  '(("The Rust Book" "https://doc.rust-lang.org/book/" "Primary narrative for language fundamentals.")
    ("Rust by Example" "https://doc.rust-lang.org/rust-by-example/" "Runnable examples by language topic.")
    ("Rustlings" "https://github.com/rust-lang/rustlings" "Short exercises for fast fundamentals practice.")
    ("Error Handling chapter" "https://doc.rust-lang.org/book/ch09-00-error-handling.html" "Result/Option patterns and propagation.")))

(defparameter *advanced-2026-resources*
  '(("Rust Reference" "https://doc.rust-lang.org/reference/" "Language-level specification and precise semantics.")
    ("Rustonomicon" "https://doc.rust-lang.org/nomicon/" "Unsafe Rust and soundness constraints.")
    ("Tokio docs" "https://docs.rs/tokio/latest/tokio/" "Runtime internals and async systems APIs.")
    ("Crossbeam docs" "https://docs.rs/crossbeam/latest/crossbeam/" "Practical lock-free and channel primitives.")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (-> course-display-name (string) string)
  (defun course-display-name (course-name)
    "Return human-facing name for known course directories."
    (cond
      ((string= course-name "basic-rust") "Introductory Rust (old)")
      ((string= course-name "advanced-rust") "Advanced Rust (old)")
      (t (name-humanize course-name))))

  (-> course-unit-title (string) string)
  (defun course-unit-title (unit-name)
    "Return label for a course unit (lecture/lab directory)."
    (if (scan "^lab-" unit-name)
        (format nil "Lab: ~A" (name-humanize unit-name))
        (format nil "Lecture: ~A" (name-humanize unit-name))))

  (-> sort-pathnames-by-name ((list-of pathname)) (list-of pathname))
  (defun sort-pathnames-by-name (paths)
    "Sort PATHS by file/directory name."
    (sort (copy-list paths)
          #'string<
          :key #'file-namestring))

  (-> page--render-footer () list)
  (defun page--render-footer ()
    "Render the page footer."
    `(:div :class "footer"
           (:p "Maintained by " (:strong *author*))
           (:h3 "Contacts")
           (dolist (contact *contacts*)
             (:p (:strong (car contact)) ": " (cadr contact) " "))
           (:p (:a :href *department-url*
                   "Department of Distributed and Dependable Systems"))))

  (-> page--get-css () string)
  (defun page--get-css ()
    "Return the CSS styles for the page."
    ":root {
       --bg: #f8f7f4;
       --surface: #ffffff;
       --surface-alt: #f1efe9;
       --text: #1c1b18;
       --muted: #57534d;
       --border: #d9d4cb;
       --accent: #9b3d20;
       --accent-soft: #f6dfd4;
       --mono-bg: #f2f1ec;
     }

     @font-face {
       font-family: \"TT Livret\";
       src: url(\"/static/TTLivretTextRegular.woff2\") format(\"woff2\");
       font-weight: normal;
       font-style: normal;
     }

     * { box-sizing: border-box; }

     body {
       margin: 0;
       background: radial-gradient(circle at top right, #fff7ed 0%, var(--bg) 55%);
       color: var(--text);
       font-family: \"TT Livret\", \"Iowan Old Style\", Georgia, serif;
       line-height: 1.55;
       padding: 28px 18px 48px;
     }

     a {
       color: var(--accent);
       text-decoration-thickness: 0.09em;
       text-underline-offset: 0.14em;
     }

     a:hover { color: #6f230d; }

     .container {
       max-width: 1080px;
       margin: 0 auto;
     }

     h1, h2, h3 {
       line-height: 1.2;
       letter-spacing: 0.01em;
       margin-top: 0;
     }

     h1 {
       font-size: clamp(1.7rem, 2.8vw, 2.5rem);
       margin-bottom: 0.5rem;
     }

     h2 {
       margin-top: 2rem;
       margin-bottom: 0.7rem;
       font-size: clamp(1.2rem, 2vw, 1.6rem);
     }

     h3 {
       margin-top: 1.2rem;
       margin-bottom: 0.45rem;
       font-size: 1.1rem;
     }

     p { margin: 0.6rem 0; }

     .lead {
       color: var(--muted);
       font-size: 1.03rem;
       max-width: 78ch;
     }

     .grid {
       display: grid;
       grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
       gap: 14px;
       margin: 1rem 0 1.4rem;
     }

     .card {
       background: var(--surface);
       border: 1px solid var(--border);
       border-radius: 12px;
       padding: 14px 16px;
       box-shadow: 0 2px 10px rgba(43, 31, 15, 0.03);
     }

     .card p { margin: 0.45rem 0; }

     .badge {
       display: inline-block;
       background: var(--accent-soft);
       border: 1px solid #f0c7b5;
       color: #6f230d;
       border-radius: 999px;
       padding: 2px 10px;
       font-size: 0.82rem;
       margin-bottom: 0.5rem;
     }

     .table-wrap {
       overflow-x: auto;
       border: 1px solid var(--border);
       border-radius: 12px;
       background: var(--surface);
       margin: 0.8rem 0 1.2rem;
     }

     table {
       width: 100%;
       border-collapse: collapse;
       min-width: 880px;
     }

     th, td {
       text-align: left;
       vertical-align: top;
       padding: 10px 12px;
       border-bottom: 1px solid var(--border);
       font-size: 0.95rem;
     }

     th {
       background: var(--surface-alt);
       font-weight: 700;
     }

     tr:last-child td { border-bottom: none; }

     details {
       margin: 0.7rem 0;
       background: var(--surface);
       border: 1px solid var(--border);
       border-radius: 10px;
       padding: 8px 10px;
     }

     summary {
       cursor: pointer;
       font-weight: 700;
     }

     pre {
       margin: 0.6rem 0 0;
       padding: 10px;
       border-radius: 8px;
       border: 1px solid var(--border);
       background: var(--mono-bg);
       font-family: \"Berkeley Mono\", \"JetBrains Mono\", \"Fira Code\", monospace;
       font-size: 0.86rem;
       overflow-x: auto;
     }

     .resource-list {
       margin: 0.4rem 0 0;
       padding-left: 1.1rem;
     }

     .resource-list li { margin: 0.35rem 0; }

     .muted { color: var(--muted); }

     .footer {
       margin-top: 2.2rem;
       padding-top: 1rem;
       border-top: 1px solid var(--border);
     }

     @media (max-width: 700px) {
       body { padding: 18px 12px 28px; }
       .card { padding: 12px; }
     }"))

  (defun page--lesson-link-pair (lesson)
    (with-html-string
      (let ((hw-a (getf lesson :hw-a))
            (hw-b (getf lesson :hw-b)))
        (if hw-a
            (:span
             (:a :href (format nil "/~A" hw-a) "A")
             " / "
             (:a :href (format nil "/~A" hw-b) "B"))
            "optional"))))

  (defun page--render-2026-section (label d3s-link lessons resources)
    (with-html-string
      (:section
       (:h2 label)
       (:p :class "lead"
           "Lecture and lab blocks are 90 minutes. Homeworks are assigned in pairs tied to each lecture theme. "
           "Recordings are published separately after processing.")
       (:p "D3S course page: " (:a :href d3s-link d3s-link))
       (:div :class "table-wrap"
             (:table
              (:thead
               (:tr
                (:th "Block")
                (:th "Topic")
                (:th "Lecture")
                (:th "Lab")
                (:th "Homework")
                (:th "Recording")))
              (:tbody
               (dolist (lesson lessons)
                 (let ((lecture-dir (getf lesson :lecture-dir))
                       (lab-dir (getf lesson :lab-dir)))
                   (:tr
                    (:td (getf lesson :id))
                    (:td (getf lesson :topic))
                    (:td
                     (:a :href (format nil "/~A/main.pdf" lecture-dir) "pdf")
                     " · "
                     (:a :href (format nil "/~A/main.typ" lecture-dir) "typst"))
                    (:td
                     (:a :href (format nil "/~A/main.pdf" lab-dir) "pdf")
                     " · "
                     (:a :href (format nil "/~A/main.org" lab-dir) "org"))
                    (:td (:raw (page--lesson-link-pair lesson)))
                    (:td "recording link to be added")))))))
       (:h3 "Lesson Texts and Code Examples")
       (dolist (lesson lessons)
         (:details
          (:summary (format nil "~A - ~A" (getf lesson :id) (getf lesson :topic)))
          (:p :class "muted" (getf lesson :summary))
          (:pre (:code (getf lesson :snippet)))))
       (:h3 "Curated Resources")
       (:ul :class "resource-list"
            (dolist (resource resources)
              (:li (:a :href (second resource) (first resource))
                   " - "
                   (third resource)))))))

  (-> page-generate-index () string)
  (defun page-generate-index ()
    "Generate the main index page."
    (with-page (:title "Rust @ MatFyz CUNI")
      (:h1 "Rust @ MatFyz CUNI")
      (:p :class "lead"
          "This site hosts active 2026 Rust materials, homeworks, and archived historical versions. "
          "The 2026 pages below are the primary source of truth.")

      (:div :class "grid"
            (:div :class "card"
                  (:span :class "badge" "2026")
                  (:h3 "Introductory Rust")
                  (:p "D3S page: " (:a :href "https://d3s.mff.cuni.cz/teaching/rust" "/teaching/rust"))
                  (:p "Material tree: " (:a :href "/materials/2026/basic/lecture-01-ownership-borrowing/main.pdf"
                                            "materials/2026/basic")))
            (:div :class "card"
                  (:span :class "badge" "2026")
                  (:h3 "Advanced Rust")
                  (:p "D3S page: " (:a :href "https://d3s.mff.cuni.cz/teaching/morerust" "/teaching/morerust"))
                  (:p "Material tree: " (:a :href "/materials/2026/advanced/lecture-01-parallel-model/main.pdf"
                                            "materials/2026/advanced")))
            (:div :class "card"
                  (:span :class "badge" "Archive")
                  (:h3 "Legacy Subjects")
                  (:p (:a :href "/basic-rust/" "Introductory Rust (old)") " and "
                      (:a :href "/advanced-rust/" "Advanced Rust (old)"))
                  (:p "Mirror hub: " (:a :href "https://matfyz.lho.sh" "matfyz.lho.sh"))))

      (:raw (page--render-2026-section
             "Introductory Rust (2026)"
             "https://d3s.mff.cuni.cz/teaching/rust"
             *intro-2026-lessons*
             *intro-2026-resources*))

      (:raw (page--render-2026-section
             "Advanced Rust (2026)"
             "https://d3s.mff.cuni.cz/teaching/morerust"
             *advanced-2026-lessons*
             *advanced-2026-resources*))

      (:h2 "Legacy Course Materials")
      (:ul
       (dolist (dir (directory-list-subdirs *base-directory*))
         (let ((dir-name (directory-get-name dir)))
           (when (or (string= dir-name "advanced-rust")
                     (string= dir-name "basic-rust"))
             (:li (:a :href (format nil "/~A/" dir-name)
                      (course-display-name dir-name)))))))

      (:h2 "Shared References")
      (:ul :class "resource-list"
           (:li (:a :href "https://doc.rust-lang.org/std/" "Rust standard library docs")
                " - API-level reference for std components.")
           (:li (:a :href "https://rust-lang.github.io/async-book/" "Async Book")
                " - Conceptual model for asynchronous Rust.")
           (:li (:a :href "https://nnethercote.github.io/perf-book/" "Rust Performance Book")
                " - Measurement-oriented optimization guidance."))))

  (-> page-generate-course (string) string)

  (-> page-generate-course (string) string)
  (defun page-generate-course (course-name)
    "Generate the page for a specific archived course."
    (let ((course-path (merge-pathnames
                        (make-pathname :directory (list :relative course-name))
                        *base-directory*)))
      (with-page (:title (course-display-name course-name))
        (:h1 (course-display-name course-name))
        (:p (:a :href "/" "Back to 2026 overview"))
        (:p :class "lead"
            "This page lists archived files from the previous course version.")
        (:h2 "Material Units")
        (:ul
         (dolist (unit-dir (sort (copy-list (directory-list-subdirs course-path))
                                 #'string<
                                 :key #'directory-get-name))
           (let ((unit-name (directory-get-name unit-dir)))
             (:li (:a :href (format nil "/~A/~A/" course-name unit-name)
                      (course-unit-title unit-name))))))
        (let ((root-files (sort-pathnames-by-name (directory-list-files course-path))))
          (when root-files
            (:h2 "General Files")
            (:ul
             (dolist (file root-files)
               (let ((file-name (file-namestring file))
                     (extension (string-downcase (subseq (file-get-extension file) 1))))
                 (:li
                  (:a :href (format nil "/~A/~A" course-name file-name)
                      (format nil "~A (~A)"
                              (name-humanize (file-get-basename file))
                              extension)))))))))))

  (-> page-generate-lab (string string) string)
  (defun page-generate-lab (course-name lab-name)
    "Generate the page for a specific archived lab/lecture unit."
    (let ((lab-path (merge-pathnames
                     (make-pathname :directory (list :relative course-name lab-name))
                     *base-directory*)))
      (with-page (:title (format nil "~A - ~A"
                                 (name-humanize course-name)
                                 (name-humanize lab-name)))
        (:h1 (format nil "~A - ~A"
                     (name-humanize course-name)
                     (name-humanize lab-name)))
        (:p (:a :href (format nil "/~A/" course-name) "Back to archive listing"))
        (:h2 "Materials")
        (:ul
         (dolist (file (directory-list-files lab-path))
           (let ((file-name (file-namestring file))
                 (extension (string-downcase (subseq (file-get-extension file) 1))))
             (:li
              (:a :href (format nil "/~A/~A/~A"
                                course-name
                                lab-name
                                file-name)
                  (format nil "~A (~A)"
                          (name-humanize (file-get-basename file))
                          extension)))))))))
