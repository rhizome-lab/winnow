//! End-to-end tests for SugarCube passage parsing and translation against real
//! game data.
//!
//! These tests require the test projects to be present at their canonical
//! locations. They are marked `#[ignore]` so they don't run in CI without
//! the test data. Run with:
//!
//! ```sh
//! cargo test -p reincarnate-frontend-twine -- --ignored --nocapture
//! ```

use reincarnate_frontend_twine::extract;
use reincarnate_frontend_twine::sugarcube;

/// Parse all passages from a SugarCube story and return (total, errors, panic_count).
fn parse_all_passages(html: &str) -> (usize, usize, usize) {
    let story = extract::extract_story(html).expect("failed to extract story");
    assert_eq!(story.format, "SugarCube");

    let total = story.passages.len();
    let mut error_passages = 0;
    let mut panic_count = 0;

    for passage in &story.passages {
        let result = std::panic::catch_unwind(|| sugarcube::parse_passage(&passage.source));
        match result {
            Ok(ast) => {
                if !ast.errors.is_empty() {
                    error_passages += 1;
                }
            }
            Err(_) => {
                panic_count += 1;
                eprintln!("PANIC parsing passage: {:?}", passage.name);
            }
        }
    }

    (total, error_passages, panic_count)
}

/// Parse + translate all passages and return (total, translate_panics).
fn translate_all_passages(html: &str) -> (usize, usize, usize) {
    let story = extract::extract_story(html).expect("failed to extract story");
    assert_eq!(story.format, "SugarCube");

    let total = story.passages.len();
    let mut translate_panics = 0;
    let mut total_functions = 0;

    for passage in &story.passages {
        let ast = sugarcube::parse_passage(&passage.source);
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            sugarcube::translate::translate_passage(&passage.name, &ast)
        }));
        match result {
            Ok(tr) => {
                total_functions += 1 + tr.widgets.len();
                // Also translate widgets to catch panics there
                for (name, body, source) in &tr.widgets {
                    let wr = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        sugarcube::translate::translate_widget(name, body, source)
                    }));
                    if wr.is_err() {
                        translate_panics += 1;
                        eprintln!("PANIC translating widget {name:?} from passage {:?}", passage.name);
                    }
                }
                // Verify the function has at least one block
                assert!(
                    !tr.func.blocks.is_empty(),
                    "passage {:?} produced empty function",
                    passage.name
                );
            }
            Err(_) => {
                translate_panics += 1;
                eprintln!("PANIC translating passage: {:?}", passage.name);
            }
        }
    }

    (total, translate_panics, total_functions)
}

#[test]
#[ignore]
fn parse_degrees_of_lewdity() {
    let path = format!(
        "{}/reincarnate/twine/dol/Degrees of Lewdity 0.4.0.9.html",
        std::env::var("HOME").unwrap()
    );
    let html = std::fs::read_to_string(&path).expect("failed to read DoL HTML");
    let (total, errors, panics) = parse_all_passages(&html);

    let success = total - errors - panics;
    let rate = (success as f64 / total as f64) * 100.0;

    eprintln!("=== Degrees of Lewdity ===");
    eprintln!("Total passages: {total}");
    eprintln!("Parsed OK:      {success}");
    eprintln!("With errors:    {errors}");
    eprintln!("Panicked:       {panics}");
    eprintln!("Success rate:   {rate:.1}%");

    assert_eq!(panics, 0, "parser panicked on {panics} passages");
    assert!(
        rate >= 99.0,
        "parse success rate {rate:.1}% is below 99% threshold"
    );
}

#[test]
#[ignore]
fn parse_repurposing_center() {
    let path = format!(
        "{}/reincarnate/twine/trc/The Repurposing Center.html",
        std::env::var("HOME").unwrap()
    );
    let html = std::fs::read_to_string(&path).expect("failed to read TRC HTML");
    let (total, errors, panics) = parse_all_passages(&html);

    let success = total - errors - panics;
    let rate = (success as f64 / total as f64) * 100.0;

    eprintln!("=== The Repurposing Center ===");
    eprintln!("Total passages: {total}");
    eprintln!("Parsed OK:      {success}");
    eprintln!("With errors:    {errors}");
    eprintln!("Panicked:       {panics}");
    eprintln!("Success rate:   {rate:.1}%");

    assert_eq!(panics, 0, "parser panicked on {panics} passages");
    assert!(
        rate >= 99.0,
        "parse success rate {rate:.1}% is below 99% threshold"
    );
}

#[test]
#[ignore]
fn translate_degrees_of_lewdity() {
    let path = format!(
        "{}/reincarnate/twine/dol/Degrees of Lewdity 0.4.0.9.html",
        std::env::var("HOME").unwrap()
    );
    let html = std::fs::read_to_string(&path).expect("failed to read DoL HTML");
    let (total, translate_panics, total_functions) = translate_all_passages(&html);

    eprintln!("=== Degrees of Lewdity (translation) ===");
    eprintln!("Total passages:   {total}");
    eprintln!("Total functions:  {total_functions}");
    eprintln!("Translate panics: {translate_panics}");

    assert_eq!(
        translate_panics, 0,
        "translator panicked on {translate_panics} passages"
    );
}

#[test]
#[ignore]
fn translate_repurposing_center() {
    let path = format!(
        "{}/reincarnate/twine/trc/The Repurposing Center.html",
        std::env::var("HOME").unwrap()
    );
    let html = std::fs::read_to_string(&path).expect("failed to read TRC HTML");
    let (total, translate_panics, total_functions) = translate_all_passages(&html);

    eprintln!("=== The Repurposing Center (translation) ===");
    eprintln!("Total passages:   {total}");
    eprintln!("Total functions:  {total_functions}");
    eprintln!("Translate panics: {translate_panics}");

    assert_eq!(
        translate_panics, 0,
        "translator panicked on {translate_panics} passages"
    );
}
