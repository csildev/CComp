use std::io;

pub fn scanf() -> Option<String> {
    let mut input = String::new();
    match io::stdin().read_line(&mut input) {
            Ok(_) => { 
                Some(input.trim().to_string())
            }
            Err(_) => {
                None
            }
    }
}
