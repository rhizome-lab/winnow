use std::collections::HashMap;
use std::process::Command;

use reincarnate_core::error::CoreError;
use reincarnate_core::pipeline::{
    CheckSummary, CheckerInput, CheckerOutput, Checker, Diagnostic, Severity,
};

pub struct TsChecker;

impl Checker for TsChecker {
    fn name(&self) -> &str {
        "typescript"
    }

    fn check(&self, input: CheckerInput) -> Result<CheckerOutput, CoreError> {
        let tsconfig = input.output_dir.join("tsconfig.json");
        if !tsconfig.exists() {
            return Err(CoreError::Project(format!(
                "no tsconfig.json found in {}",
                input.output_dir.display()
            )));
        }

        let output = Command::new("bunx")
            .args(["@typescript/native-preview", "--noEmit", "--pretty", "false"])
            .current_dir(&input.output_dir)
            .output()
            .map_err(|e| {
                CoreError::Project(format!(
                    "failed to run tsgo: {e} (is bun installed?)"
                ))
            })?;

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        let mut diagnostics = Vec::new();
        for line in stdout.lines().chain(stderr.lines()) {
            if let Some(d) = parse_diagnostic(line) {
                diagnostics.push(d);
            }
        }

        let summary = build_summary(&input.output_dir.to_string_lossy(), &diagnostics);
        Ok(CheckerOutput {
            diagnostics,
            summary,
        })
    }
}

/// Parse a TypeScript diagnostic line in non-pretty format:
/// `path/to/file.ts(42,5): error TS2304: Cannot find name 'foo'.`
fn parse_diagnostic(line: &str) -> Option<Diagnostic> {
    // Find "): error TS" or "): warning TS" marker.
    let (severity, marker) = if let Some(pos) = line.find("): error TS") {
        (Severity::Error, pos)
    } else if let Some(pos) = line.find("): warning TS") {
        (Severity::Warning, pos)
    } else {
        return None;
    };

    // Everything before the marker is "file(line,col"
    let loc_part = &line[..marker];
    let paren_pos = loc_part.rfind('(')?;
    let file = &loc_part[..paren_pos];
    let coords = &loc_part[paren_pos + 1..];
    let comma = coords.find(',')?;
    let line_num: u32 = coords[..comma].parse().ok()?;
    let col: u32 = coords[comma + 1..].parse().ok()?;

    // After "): " is "error TSxxxx: message" or "warning TSxxxx: message"
    let after_paren = &line[marker + 2..]; // skip "): "
    let colon_pos = after_paren.find(": ")?;
    let severity_and_code = &after_paren[..colon_pos];
    let message = &after_paren[colon_pos + 2..];

    // Extract code from "error TS2304" → "TS2304"
    let code = severity_and_code
        .split_whitespace()
        .nth(1)
        .unwrap_or(severity_and_code);

    Some(Diagnostic {
        file: file.to_string(),
        line: line_num,
        col,
        code: code.to_string(),
        severity,
        message: message.to_string(),
    })
}

fn build_summary(output_dir: &str, diagnostics: &[Diagnostic]) -> CheckSummary {
    let mut total_errors = 0usize;
    let mut total_warnings = 0usize;
    let mut by_code: HashMap<String, usize> = HashMap::new();
    let mut by_message: HashMap<(String, String), usize> = HashMap::new();

    for d in diagnostics {
        match d.severity {
            Severity::Error => total_errors += 1,
            Severity::Warning => total_warnings += 1,
        }
        *by_code.entry(d.code.clone()).or_default() += 1;
        *by_message
            .entry((d.message.clone(), d.code.clone()))
            .or_default() += 1;
    }

    let mut by_code: Vec<(String, usize)> = by_code.into_iter().collect();
    by_code.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));

    let mut by_message: Vec<(String, String, usize)> = by_message
        .into_iter()
        .map(|((msg, code), count)| (msg, code, count))
        .collect();
    by_message.sort_by(|a, b| b.2.cmp(&a.2).then_with(|| a.1.cmp(&b.1)).then_with(|| a.0.cmp(&b.0)));

    CheckSummary {
        output_dir: output_dir.to_string(),
        total_errors,
        total_warnings,
        by_code,
        by_message,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_error_line() {
        let line = "src/obj_player.ts(42,5): error TS2304: Cannot find name 'foo'.";
        let d = parse_diagnostic(line).expect("should parse");
        assert_eq!(d.file, "src/obj_player.ts");
        assert_eq!(d.line, 42);
        assert_eq!(d.col, 5);
        assert_eq!(d.code, "TS2304");
        assert_eq!(d.severity, Severity::Error);
        assert_eq!(d.message, "Cannot find name 'foo'.");
    }

    #[test]
    fn parse_warning_line() {
        let line = "lib/util.ts(10,1): warning TS6133: 'x' is declared but its value is never read.";
        let d = parse_diagnostic(line).expect("should parse");
        assert_eq!(d.file, "lib/util.ts");
        assert_eq!(d.severity, Severity::Warning);
        assert_eq!(d.code, "TS6133");
    }

    #[test]
    fn skip_non_diagnostic() {
        assert!(parse_diagnostic("Found 42 errors.").is_none());
        assert!(parse_diagnostic("").is_none());
    }

    #[test]
    fn summary_aggregation() {
        let diagnostics = vec![
            Diagnostic {
                file: "a.ts".into(),
                line: 1,
                col: 1,
                code: "TS2304".into(),
                severity: Severity::Error,
                message: "x".into(),
            },
            Diagnostic {
                file: "a.ts".into(),
                line: 2,
                col: 1,
                code: "TS2304".into(),
                severity: Severity::Error,
                message: "y".into(),
            },
            Diagnostic {
                file: "b.ts".into(),
                line: 1,
                col: 1,
                code: "TS2339".into(),
                severity: Severity::Error,
                message: "z".into(),
            },
        ];
        let summary = build_summary("/out", &diagnostics);
        assert_eq!(summary.total_errors, 3);
        assert_eq!(summary.total_warnings, 0);
        assert_eq!(summary.by_code[0], ("TS2304".into(), 2));
        assert_eq!(summary.by_code[1], ("TS2339".into(), 1));
        // Each message is unique in this test, so by_message has 3 entries sorted by count.
        assert_eq!(summary.by_message.len(), 3);
        // Messages "x" and "y" each appear once under TS2304; "z" appears once under TS2339.
        // All have count 1, so order is by code then message: TS2304/"x", TS2304/"y", TS2339/"z".
        assert_eq!(summary.by_message[0], ("x".into(), "TS2304".into(), 1));
        assert_eq!(summary.by_message[1], ("y".into(), "TS2304".into(), 1));
        assert_eq!(summary.by_message[2], ("z".into(), "TS2339".into(), 1));
    }
}
