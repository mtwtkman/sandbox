fn find(heystack: &str, needle: char) -> Option<usize> {
    for (offset, c) in heystack.char_indices() {
        if c == needle {
            return Some(offset);
        }
    }
    None
}

fn extension_explicit(file_name: &str) -> Option<&str> {
    find(file_name, '.').map(|i| &file_name[i+1..])
}

fn file_path_ext_explicit(file_path: &str) -> Option(&str) {
    file_name(file_path).and_then(extension)
}

fn file_name(file_path: &str) -> {
    unimplemented!()
}

fn main() {
    let file_name = "foobarrs.rs.hog";
    println!("{}", extension_explicit(file_name).unwrap_or("rsd"));
}
