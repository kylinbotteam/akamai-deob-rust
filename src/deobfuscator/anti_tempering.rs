use regex::Regex;

pub fn extract_anti_tempering(code: &str) -> Option<(&str, &str)> {
    let re = Regex::new(r"(?i)function ([a-zA-Z]+)\(\)\{.+}").unwrap();

    if let Some(c) = re.captures(code) {
        if c.len() == 2 {
            return Some((c.get(0).unwrap().as_str(), c.get(1).unwrap().as_str()));
        }
    }
    return None
}