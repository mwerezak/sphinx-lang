use clap::{App, Arg};

fn main() {
    let app = App::new("hello")
        .version("1.0")
        .author("M. Werezak <mwerezak@gmail.com>")
        .about("Basic command line greeter")
        .arg(
            Arg::new("name")
            .short('N')
            .long("name")
            .help("a name to greet with")
            .value_name("NAME")
            .takes_value(true)
            .forbid_empty_values(true)
            
        );
    
    let args = app.get_matches();
    
    let name = match args.value_of("name") {
        Some(name) => name,
        None => "world",
    };
    
    println!("Hello, {}!", name);
}
