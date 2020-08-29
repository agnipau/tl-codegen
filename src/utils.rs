pub fn capitalize(s: &str) -> String {
    let mut v = s.chars().collect::<Vec<_>>();
    v[0] = v[0].to_uppercase().next().unwrap();
    v.into_iter().collect()
}
