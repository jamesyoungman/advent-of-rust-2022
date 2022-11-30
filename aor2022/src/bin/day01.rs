use lib::error::Fail;

fn main() {
    let e = Fail("we need to write this program".to_string());
    panic!("{e}");
}
