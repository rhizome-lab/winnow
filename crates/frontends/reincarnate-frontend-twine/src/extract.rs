//! Twine story extraction from compiled HTML files.
//!
//! All Twine 2 story formats (SugarCube, Harlowe, Snowman, Chapbook) compile
//! to a single HTML file with embedded `<tw-storydata>` and `<tw-passagedata>`
//! elements. This module extracts the raw passage data from that container,
//! producing a format-agnostic `Story` that the per-format parsers consume.

/// A Twine story extracted from its HTML container.
#[derive(Debug)]
pub struct Story {
    /// The story title from the `name` attribute.
    pub name: String,
    /// The story format (e.g. "SugarCube", "Harlowe").
    pub format: String,
    /// The story format version (e.g. "2.36.1").
    pub format_version: String,
    /// The IFID (Interactive Fiction IDentifier).
    pub ifid: String,
    /// The pid of the starting passage.
    pub start_pid: u32,
    /// All passages in the story.
    pub passages: Vec<Passage>,
    /// Contents of `<script id="twine-user-script">` blocks.
    pub user_scripts: Vec<String>,
    /// Contents of `<style id="twine-user-stylesheet">` blocks.
    pub user_styles: Vec<String>,
    /// The story format's built-in CSS (e.g. from `<style title="Twine CSS">`).
    pub format_css: Option<String>,
}

/// A single passage extracted from a `<tw-passagedata>` element.
#[derive(Debug)]
pub struct Passage {
    /// The passage's numeric id (`pid` attribute).
    pub pid: u32,
    /// The passage name (used for linking).
    pub name: String,
    /// Space-separated tags.
    pub tags: Vec<String>,
    /// The raw passage source text (macros, links, HTML, plain text).
    pub source: String,
}

/// Extract a `Story` from Twine 2 compiled HTML.
///
/// Parses the `<tw-storydata>` element and all child `<tw-passagedata>`
/// elements. The passage `source` is left unparsed — that's the job of
/// the format-specific parser (SugarCube, Harlowe, etc.).
pub fn extract_story(html: &str) -> Result<Story, ExtractError> {
    // Find <tw-storydata ...>
    let sd_start = html
        .find("<tw-storydata")
        .ok_or(ExtractError::NoStoryData)?;
    let sd_tag_end = html[sd_start..]
        .find('>')
        .ok_or(ExtractError::MalformedStoryData)?;
    let sd_tag = &html[sd_start..sd_start + sd_tag_end];

    let name = extract_attr(sd_tag, "name").unwrap_or_default();
    let format = extract_attr(sd_tag, "format").unwrap_or_default();
    let format_version = extract_attr(sd_tag, "format-version").unwrap_or_default();
    let ifid = extract_attr(sd_tag, "ifid").unwrap_or_default();
    let start_pid: u32 = extract_attr(sd_tag, "startnode")
        .unwrap_or_default()
        .parse()
        .unwrap_or(1);

    // Extract all <tw-passagedata ...>...</tw-passagedata>
    let mut passages = Vec::new();
    let mut search_from = sd_start;
    while let Some(pd_start) = html[search_from..].find("<tw-passagedata") {
        let abs_start = search_from + pd_start;
        let tag_end = html[abs_start..]
            .find('>')
            .ok_or(ExtractError::MalformedPassage)?;
        let tag = &html[abs_start..abs_start + tag_end];

        let pid: u32 = extract_attr(tag, "pid")
            .unwrap_or_default()
            .parse()
            .unwrap_or(0);
        let pname = extract_attr(tag, "name").unwrap_or_default();
        let tags_str = extract_attr(tag, "tags").unwrap_or_default();
        let tags: Vec<String> = if tags_str.is_empty() {
            Vec::new()
        } else {
            tags_str.split_whitespace().map(String::from).collect()
        };

        // Source is between > and </tw-passagedata>
        let content_start = abs_start + tag_end + 1;
        let content_end = html[content_start..]
            .find("</tw-passagedata>")
            .map(|i| content_start + i)
            .unwrap_or(content_start);
        let source = decode_html_entities(&html[content_start..content_end]);

        passages.push(Passage {
            pid,
            name: pname,
            tags,
            source,
        });
        search_from = content_end + "</tw-passagedata>".len();
    }

    if passages.is_empty() {
        return Err(ExtractError::NoPassages);
    }

    // Extract user scripts
    let user_scripts = extract_tagged_blocks(html, "twine-user-script");
    let user_styles = extract_tagged_blocks(html, "twine-user-stylesheet");

    // Extract format CSS (e.g. Harlowe's built-in stylesheet)
    let format_css = extract_format_css(html);

    Ok(Story {
        name,
        format,
        format_version,
        ifid,
        start_pid,
        passages,
        user_scripts,
        user_styles,
        format_css,
    })
}

/// Extract the value of an attribute from a tag string.
/// Handles both single and double quotes.
fn extract_attr(tag: &str, attr_name: &str) -> Option<String> {
    // Match `attr_name="value"` or `attr_name='value'`
    for quote in ['"', '\''] {
        let pattern = format!("{attr_name}={quote}");
        if let Some(start) = tag.find(&pattern) {
            let value_start = start + pattern.len();
            if let Some(end) = tag[value_start..].find(quote) {
                return Some(decode_html_entities(&tag[value_start..value_start + end]));
            }
        }
    }
    None
}

/// Extract content from `<script id="ID">` or `<style id="ID">` blocks.
fn extract_tagged_blocks(html: &str, id: &str) -> Vec<String> {
    let mut blocks = Vec::new();
    let pattern = format!("id=\"{id}\"");
    let mut search_from = 0;
    while let Some(attr_pos) = html[search_from..].find(&pattern) {
        let abs_pos = search_from + attr_pos;
        // Find the end of the opening tag
        if let Some(tag_end) = html[abs_pos..].find('>') {
            let content_start = abs_pos + tag_end + 1;
            // Find whichever closing tag (</script> or </style>) comes first.
            // We must pick the minimum to avoid crossing into sibling blocks.
            let rest = &html[content_start..];
            let close = match (rest.find("</script>"), rest.find("</style>")) {
                (Some(a), Some(b)) => Some(a.min(b)),
                (Some(a), None) => Some(a),
                (None, Some(b)) => Some(b),
                (None, None) => None,
            };
            if let Some(close) = close {
                blocks.push(rest[..close].to_string());
                search_from = content_start + close;
                continue;
            }
        }
        search_from = abs_pos + pattern.len();
    }
    blocks
}

/// Decode HTML entities in passage source.
fn decode_html_entities(s: &str) -> String {
    html_escape::decode_html_entities(s).into_owned()
}

#[derive(Debug)]
pub enum ExtractError {
    NoStoryData,
    MalformedStoryData,
    MalformedPassage,
    NoPassages,
}

impl std::fmt::Display for ExtractError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoStoryData => write!(f, "no <tw-storydata> element found in HTML"),
            Self::MalformedStoryData => write!(f, "malformed <tw-storydata> element"),
            Self::MalformedPassage => write!(f, "malformed <tw-passagedata> element"),
            Self::NoPassages => write!(f, "no passages found in story data"),
        }
    }
}

impl std::error::Error for ExtractError {}

/// Extract the story format's built-in CSS from `<style title="Twine CSS">`.
///
/// Harlowe (and potentially other formats) embed a large built-in stylesheet
/// in the compiled HTML under this title. Returns `None` if not found.
fn extract_format_css(html: &str) -> Option<String> {
    let marker = "title=\"Twine CSS\"";
    let attr_pos = html.find(marker)?;
    // Walk backward to find the opening `<style` tag
    let style_start = html[..attr_pos].rfind("<style")?;
    let tag_end = html[style_start..].find('>')? + style_start + 1;
    let close = html[tag_end..].find("</style>")?;
    let css = html[tag_end..tag_end + close].trim();
    if css.is_empty() {
        None
    } else {
        Some(css.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extract_minimal_story() {
        let html = r#"
<html><head></head><body>
<tw-storydata name="Test Story" startnode="1" format="SugarCube" format-version="2.36.1" ifid="AAAA-BBBB" hidden>
<tw-passagedata pid="1" name="Start" tags="" position="0,0" size="100,100">Hello &amp; welcome!</tw-passagedata>
<tw-passagedata pid="2" name="Room" tags="nobr" position="100,0" size="100,100">You are in a room.
&lt;&lt;link [[Leave|Start]]&gt;&gt;&lt;&lt;/link&gt;&gt;</tw-passagedata>
</tw-storydata>
</body></html>
"#;
        let story = extract_story(html).unwrap();
        assert_eq!(story.name, "Test Story");
        assert_eq!(story.format, "SugarCube");
        assert_eq!(story.format_version, "2.36.1");
        assert_eq!(story.ifid, "AAAA-BBBB");
        assert_eq!(story.start_pid, 1);
        assert_eq!(story.passages.len(), 2);
        assert_eq!(story.passages[0].name, "Start");
        assert_eq!(story.passages[0].source, "Hello & welcome!");
        assert_eq!(story.passages[1].name, "Room");
        assert_eq!(story.passages[1].tags, vec!["nobr"]);
    }

    #[test]
    fn extract_user_scripts() {
        let html = r#"
<tw-storydata name="S" startnode="1" format="SugarCube" format-version="2.0" ifid="X" hidden>
<tw-passagedata pid="1" name="Start" tags="">Hi</tw-passagedata>
</tw-storydata>
<script id="twine-user-script" type="text/twine-javascript">window.setup = {};</script>
"#;
        let story = extract_story(html).unwrap();
        assert_eq!(story.user_scripts.len(), 1);
        assert_eq!(story.user_scripts[0], "window.setup = {};");
    }

    #[test]
    fn no_story_data_errors() {
        let html = "<html><body>Nothing here</body></html>";
        assert!(extract_story(html).is_err());
    }

    #[test]
    fn extract_format_css_from_style_tag() {
        let html = r#"
<html><head>
<style title="Twine CSS">tw-story { color: white; }</style>
</head><body>
<tw-storydata name="Test" startnode="1" format="Harlowe" format-version="3.3.9" ifid="X" hidden>
<tw-passagedata pid="1" name="Start" tags="">Hello</tw-passagedata>
</tw-storydata>
</body></html>
"#;
        let story = extract_story(html).unwrap();
        assert_eq!(
            story.format_css.as_deref(),
            Some("tw-story { color: white; }")
        );
    }

    #[test]
    fn no_format_css_returns_none() {
        let html = r#"
<tw-storydata name="S" startnode="1" format="SugarCube" format-version="2.0" ifid="X" hidden>
<tw-passagedata pid="1" name="Start" tags="">Hi</tw-passagedata>
</tw-storydata>
"#;
        let story = extract_story(html).unwrap();
        assert!(story.format_css.is_none());
    }

    /// Regression test: stylesheet block must not capture past </style> into a later </script>.
    /// extract_tagged_blocks used to prefer </script> regardless of order, causing the entire
    /// user-script block to be swept into user_styles when </script> appeared after </style>.
    #[test]
    fn stylesheet_block_stops_at_style_not_script() {
        let html = r#"
<tw-storydata name="S" startnode="1" format="SugarCube" format-version="2.0" ifid="X" hidden>
<tw-passagedata pid="1" name="Start" tags="">Hi</tw-passagedata>
</tw-storydata>
<style id="twine-user-stylesheet" type="text/twine-css">body { color: red; }</style>
<script id="twine-user-script" type="text/twine-javascript">window.setup = {};</script>
"#;
        let story = extract_story(html).unwrap();
        assert_eq!(story.user_styles.len(), 1);
        assert_eq!(story.user_styles[0], "body { color: red; }");
        assert_eq!(story.user_scripts.len(), 1);
        assert_eq!(story.user_scripts[0], "window.setup = {};");
    }
}
