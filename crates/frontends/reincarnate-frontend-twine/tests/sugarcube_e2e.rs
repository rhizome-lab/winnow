//! End-to-end tests for SugarCube passage parsing against real game data.
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
