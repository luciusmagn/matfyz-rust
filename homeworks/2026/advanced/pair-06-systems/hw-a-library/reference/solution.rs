fn normalize_components(path: &str) -> (bool, Vec<String>) {
    let is_abs = path.starts_with('/');
    let mut stack: Vec<String> = Vec::new();

    for part in path.split('/') {
        if part.is_empty() || part == "." {
            continue;
        }

        if part == ".." {
            if let Some(last) = stack.last() {
                if last != ".." {
                    stack.pop();
                    continue;
                }
            }
            if !is_abs {
                stack.push("..".to_string());
            }
            continue;
        }

        stack.push(part.to_string());
    }

    (is_abs, stack)
}

pub fn normalize_path(path: &str) -> String {
    let (is_abs, parts) = normalize_components(path);
    let joined = parts.join("/");

    if is_abs {
        if joined.is_empty() {
            "/".to_string()
        } else {
            format!("/{joined}")
        }
    } else if joined.is_empty() {
        ".".to_string()
    } else {
        joined
    }
}

pub fn join_path(base: &str, child: &str) -> String {
    if child.starts_with('/') {
        return normalize_path(child);
    }

    let combined = if base.is_empty() {
        child.to_string()
    } else if child.is_empty() {
        base.to_string()
    } else {
        format!("{base}/{child}")
    };

    normalize_path(&combined)
}

pub fn split_extension(path: &str) -> (String, Option<String>) {
    let slash = path.rfind('/');
    let start = slash.map(|i| i + 1).unwrap_or(0);
    let file = &path[start..];

    let Some(dot_pos_rel) = file.rfind('.') else {
        return (path.to_string(), None);
    };
    if dot_pos_rel == 0 || dot_pos_rel + 1 == file.len() {
        return (path.to_string(), None);
    }

    let dot_pos = start + dot_pos_rel;
    let stem = path[..dot_pos].to_string();
    let ext = path[dot_pos + 1..].to_string();
    (stem, Some(ext))
}
