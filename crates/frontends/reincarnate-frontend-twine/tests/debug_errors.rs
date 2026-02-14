//! Diagnostic test: prints all parse errors from DoL and TRC to understand
//! what patterns the parser fails on.

use reincarnate_frontend_twine::extract;
use reincarnate_frontend_twine::sugarcube;
use std::collections::HashMap;

fn analyze_errors(html: &str, label: &str) {
    let story = extract::extract_story(html).expect("failed to extract story");

    let mut error_counts: HashMap<String, usize> = HashMap::new();
    let mut examples: HashMap<String, Vec<(String, String)>> = HashMap::new();
    let mut total_errors = 0;

    for passage in &story.passages {
        let ast = sugarcube::parse_passage(&passage.source);
        for err in &ast.errors {
            total_errors += 1;
            // Categorize by first few words of the error message
            let category = err.message.split_whitespace().take(4).collect::<Vec<_>>().join(" ");
            *error_counts.entry(category.clone()).or_default() += 1;

            let examples_for_cat = examples.entry(category).or_default();
            if examples_for_cat.len() < 3 {
                // Get context around the error
                let src = &passage.source;
                let start = err.span.start.saturating_sub(20).min(src.len());
                let end = (err.span.end + 80).min(src.len());
                // Find char boundaries
                let start = src.floor_char_boundary(start);
                let end = src.ceil_char_boundary(end);
                let context = &src[start..end];
                examples_for_cat.push((passage.name.clone(), context.to_string()));
            }
        }
    }

    eprintln!("\n=== {label} — {total_errors} total errors ===\n");

    let mut sorted: Vec<_> = error_counts.into_iter().collect();
    sorted.sort_by(|a, b| b.1.cmp(&a.1));

    for (category, count) in &sorted {
        eprintln!("  [{count:4}x] {category}");
        if let Some(exs) = examples.get(category) {
            for (name, ctx) in exs {
                let ctx_escaped = ctx.replace('\n', "\\n").replace('\t', "\\t");
                let truncated = if ctx_escaped.len() > 120 {
                    format!("{}...", &ctx_escaped[..120])
                } else {
                    ctx_escaped
                };
                eprintln!("         passage={name}: {truncated}");
            }
        }
        eprintln!();
    }
}

#[test]
#[ignore]
fn debug_dol_errors() {
    let path = format!(
        "{}/reincarnate/twine/dol/Degrees of Lewdity 0.4.0.9.html",
        std::env::var("HOME").unwrap()
    );
    let html = std::fs::read_to_string(&path).expect("failed to read DoL");
    analyze_errors(&html, "Degrees of Lewdity");
}

#[test]
#[ignore]
fn debug_trc_errors() {
    let path = format!(
        "{}/reincarnate/twine/trc/The Repurposing Center.html",
        std::env::var("HOME").unwrap()
    );
    let html = std::fs::read_to_string(&path).expect("failed to read TRC");
    analyze_errors(&html, "The Repurposing Center");
}
