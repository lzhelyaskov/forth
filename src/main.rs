use std::fs::File;

use forthvm::{ForthVM, VmConfig, in_stream_from_file};
use getopts::Options;
extern crate getopts;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("", "debug", "run in debug mode");
    opts.optflag("h", "help", "print this help menu");
    opts.optopt("i", "include", "include forth file", "FILE");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => {
            panic!("{}", f.to_string())
        }
    };
    if matches.opt_present("h") {
        print_usage(&program, opts);
        return;
    }

    if let Some(filename) = matches.opt_str("i") {
        if let Ok(file) = File::open(&filename) {
            in_stream_from_file(file);
        } else {
            println!("could not open {filename}.");
            print_usage(&program, opts);
            return;
        }
    }

    if matches.opt_present("debug") {
        debug_quit();
    } else {
        run_quit();
    }
}

fn run_quit() {
    let mut vm = create_vm();
    let quit = vm.find("quit").unwrap();

    vm.run_word(quit as usize);
}

fn debug_quit() {
    let mut vm = create_vm();
    let quit = vm.find("quit").unwrap();

    vm.run_word_debug(quit as usize);
}

fn create_vm() -> ForthVM {
    let conf = VmConfig {
        memory_size_bytes: 0x10000,
        parameter_stack_size_cells: 256,
        return_stack_size_cells: 256,
        call_stack_size_cells: 256,
        locals_stack_size_cells: 256,
    };
    let mut vm = ForthVM::from_config(conf);

    vm.init_dictionary();
    vm
}

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} [options]", program);
    print!("{}", opts.usage(&brief));
}
