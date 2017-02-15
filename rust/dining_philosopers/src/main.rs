struct Philosher {
    name: String,
}

impl Philosopher {
    fn new(name: &str) -> Philosher {
        Philosher {
            name: name.to_string(),
        }
    }
}

fn main() {
    let p1 = Philosher::new("Judith Butler");
    let p2 = Philosher::new("Gilles Deleuze");
    let p3 = Philosher::new("Karl Marx");
    let p4 = Philosher::new("Emma Goldman");
    let p5 = Philosher::new("Michel Foucault");
}
