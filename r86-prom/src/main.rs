extern crate r86_prom;

use std::error::Error;
use std::fs::File;
use std::io::{self, Write, Read};
use std::path::PathBuf;

use console::{style, Emoji};
use rand::Rng;

use r86_prom::cli::{Mode, Options, WriteMode};
use r86_prom::cli::utils::{Bar, Spinner};
use r86_prom::message::DeviceRequest;
use r86_prom::programmer::Programmer;

async fn run_test() {
    println!("{} {} {}", Emoji("ℹ️", "ℹ"), style("Mode:").bright().cyan(), "chip testing...");
    let mut rng = rand::thread_rng();
    let mut stdout = io::stdout();

    let spinner = Spinner::spawn("Connecting to device...");
    let mut programmer = Programmer::new().unwrap();
    spinner.finish().await;

    let spinner = Spinner::spawn("Enabling chip...");
    programmer.request(DeviceRequest::ChipEnable)
        .send()
        .resolve()
        .await.0.unwrap();
    spinner.finish().await;



    let spinner = Spinner::spawn("Checking if protection is enabled...");
    let addr = rng.gen_range(0..32768);

    let (res, payload) = programmer.request(DeviceRequest::ReadByteAt(addr))
        .send()
        .resolve()
        .await;
    let first = payload.unwrap();
    res.unwrap();

    programmer.request(DeviceRequest::WriteByteAt(addr, !first[0]))
        .send()
        .resolve()
        .await.0.unwrap();

    let (res, payload) = programmer.request(DeviceRequest::ReadByteAt(addr))
        .send()
        .resolve()
        .await;
    let second = payload.unwrap();
    res.unwrap();

    let write_protection = first[0] == second[0];
    spinner.finish().await;

    if write_protection {
        println!("{} {} {}", Emoji("ℹ️", "ℹ"), style("Info:").bright().cyan(), "write protection is enabled");
    } else {
        println!("{} {} {}", Emoji("ℹ️", "ℹ"), style("Info:").bright().cyan(), "write protection is disabled");
    }

    let payload = vec![0x55; 32768];

    let bar = Bar::spawn("Writing to device...");
    let res = programmer.request(DeviceRequest::ProtectedWriteAll)
        .payload(payload.clone())
        .send();
    let (res, _) = bar.await_response(res).await;
    res.unwrap();

    let bar = Bar::spawn("Reading from device...");
    let res = programmer.request(DeviceRequest::ReadAll).send();
    let (res, payload_check) = bar.await_response(res).await;
    res.unwrap();

    let spinner = Spinner::spawn("Checking equality...");
    let ok = payload == payload_check.unwrap();
    spinner.finish().await;

    if ok {
        println!("{} {} {}", Emoji("✔️", "✔"), style("Success:").bright().green(), "first test passed");
    }

    let spinner = Spinner::spawn("Disabling chip...");
    programmer.request(DeviceRequest::ChipDisable)
        .send()
        .resolve()
        .await.0.unwrap();
    spinner.finish().await;



    std::mem::drop(programmer);
    write!(io::stdout(), "   Disconnect the device, let it discharge any residual charge and reconnect the device; then, press any key").unwrap();
    stdout.flush().unwrap();
    let _ = io::stdin().read_line(&mut String::new()).unwrap();



    let spinner = Spinner::spawn("Connecting to device...");
    let mut programmer = Programmer::new().unwrap();
    spinner.finish().await;

    let spinner = Spinner::spawn("Enabling chip...");
    programmer.request(DeviceRequest::ChipEnable)
        .send()
        .resolve()
        .await.0.unwrap();
    spinner.finish().await;

    let bar = Bar::spawn("Reading from device...");
    let res = programmer.request(DeviceRequest::ReadAll).send();
    let (res, payload_check) = bar.await_response(res).await;
    res.unwrap();

    let spinner = Spinner::spawn("Checking equality...");
    let ok = payload == payload_check.unwrap();
    spinner.finish().await;

    if !ok {
        println!("{} {} {}", Emoji("❌", "X"), style("Error:").bright().red(), "data is not persistent!");

        let spinner = Spinner::spawn("Disabling chip...");
        programmer.request(DeviceRequest::ChipDisable)
            .send()
            .resolve()
            .await.0.unwrap();
        spinner.finish().await;

        return;
    }

    let payload = vec![0xAA; 32768];

    let bar = Bar::spawn("Writing to device...");
    let res = programmer.request(DeviceRequest::ProtectedWriteAll)
        .payload(payload.clone())
        .send();
    let (res, _) = bar.await_response(res).await;
    res.unwrap();

    let bar = Bar::spawn("Reading from device...");
    let res = programmer.request(DeviceRequest::ReadAll).send();
    let (res, payload_check) = bar.await_response(res).await;
    res.unwrap();

    let spinner = Spinner::spawn("Checking equality...");
    let ok = payload == payload_check.unwrap();
    spinner.finish().await;

    if ok {
        println!("{} {} {}", Emoji("✔️", "✔"), style("Success:").bright().green(), "second test passed");
    }

    let spinner = Spinner::spawn("Disabling chip...");
    programmer.request(DeviceRequest::ChipDisable)
        .send()
        .resolve()
        .await.0.unwrap();
    spinner.finish().await;



    std::mem::drop(programmer);
    write!(io::stdout(), "   Disconnect the device, let it discharge any residual charge and reconnect the device; then, press any key").unwrap();
    stdout.flush().unwrap();
    let _ = io::stdin().read_line(&mut String::new()).unwrap();



    let spinner = Spinner::spawn("Connecting to device...");
    let mut programmer = Programmer::new().unwrap();
    spinner.finish().await;

    let spinner = Spinner::spawn("Enabling chip...");
    programmer.request(DeviceRequest::ChipEnable)
        .send()
        .resolve()
        .await.0.unwrap();
    spinner.finish().await;

    let bar = Bar::spawn("Reading from device...");
    let res = programmer.request(DeviceRequest::ReadAll).send();
    let (res, payload_check) = bar.await_response(res).await;
    res.unwrap();

    let spinner = Spinner::spawn("Checking equality...");
    let ok = payload == payload_check.unwrap();
    spinner.finish().await;

    if !ok {
        println!("{} {} {}", Emoji("❌", "X"), style("Error:").bright().red(), "data is not persistent!");
    }

    let spinner = Spinner::spawn("Disabling chip...");
    programmer.request(DeviceRequest::ChipDisable)
        .send()
        .resolve()
        .await.0.unwrap();
    spinner.finish().await;

    if ok {
        println!("{} {} {}", Emoji("ℹ️", "ℹ"), style("Success:").bright().green(), "all tests passed!");
    }

}

pub async fn run_read(filename: Option<PathBuf>) {
    if filename.is_some() {
        println!("{} {} {}", Emoji("ℹ️", "ℹ"), style("Mode:").bright().cyan(), "dump to file");
    } else {
        println!("{} {} {}", Emoji("ℹ️", "ℹ"), style("Mode:").bright().cyan(), "dump to stdout");
    }

    let spinner = Spinner::spawn("Connecting to device...");
    let mut programmer = Programmer::new().unwrap();
    spinner.finish().await;

    let spinner = Spinner::spawn("Enabling chip...");
    programmer.request(DeviceRequest::ChipEnable)
        .send()
        .resolve()
        .await.0.unwrap();
    spinner.finish().await;

    let bar = Bar::spawn("Reading from device...");
    let res = programmer.request(DeviceRequest::ReadAll).send();
    let (res, payload) = bar.await_response(res).await;
    res.unwrap();

    let spinner = Spinner::spawn("Disabling chip...");
    programmer.request(DeviceRequest::ChipDisable)
        .send()
        .resolve()
        .await.0.unwrap();
    spinner.finish().await;

    if let Some(filename) = filename {
        let spinner = Spinner::spawn("Writing to file...");
        let mut file = File::create(filename).expect("Cannot open file");
        file.write_all(&payload.unwrap()[..]).unwrap();
        spinner.finish().await;

        println!("{} {}", Emoji("✔️", "✔"), style("Success!").bright().green());
    } else {
        println!("{} {} {}", Emoji("❌", "X"), style("Error:").bright().red(), "writing to stdout is currently not implemented.");
    }
}

async fn run_write(filename: Option<PathBuf>, write_mode: WriteMode) {
    let mut payload = vec![0; 32768];

    if let Some(filename) = filename {
        println!("{} {} {}", Emoji("ℹ️", "ℹ"), style("Mode:").bright().cyan(), "read from file");

        let mut file = File::open(filename).expect("Cannot open file");
        let size = file.metadata().expect("Cannot extract information")
            .len();

        if size != 32768 {
            if write_mode == WriteMode::Strict {
                println!("{} {} {}", Emoji("❌", "X"), style("Error:").bright().red(), "file must be exactly 32768 Bytes in 'strict' mode.");
                return;
            } else {
                println!("{} {} {}", Emoji("⚠️", "⚠"), style("Warning:").bright().yellow(), "file does not match EEPROM size of 32768 Bytes.");
            }
        }

        file.read_exact(&mut payload[..]).expect("Cannot read from file");
    } else {
        println!("{} {} {}", Emoji("ℹ️", "ℹ"), style("Mode:").bright().cyan(), "read from stdin");
        println!("{} {} {}", Emoji("❌", "X"), style("Error:").bright().red(), "reading from stdin is currently not implemented.");
        return;
    }

    let spinner = Spinner::spawn("Connecting to device...");
    let mut programmer = Programmer::new().unwrap();
    spinner.finish().await;

    let spinner = Spinner::spawn("Enabling chip...");
    programmer.request(DeviceRequest::ChipEnable)
        .send()
        .resolve()
        .await.0.unwrap();
    spinner.finish().await;

    let bar = Bar::spawn("Writing to device...");
    let res = programmer.request(DeviceRequest::ProtectedWriteAll)
        .payload(payload)
        .send();
    let (res, _) = bar.await_response(res).await;
    res.unwrap();

    let spinner = Spinner::spawn("Disabling chip...");
    programmer.request(DeviceRequest::ChipDisable)
        .send()
        .resolve()
        .await.0.unwrap();
    spinner.finish().await;

    println!("{} {}", Emoji("✔️", "✔"), style("Success!").bright().green());
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let mode = Options::from_command_line().into_mode();

    match mode {
        Mode::Test => run_test().await,
        Mode::Interactive => unimplemented!("Sorry: not yet implemented."),
        Mode::Read { filename } => run_read(filename).await,
        Mode::Write { filename, write_mode } => run_write(filename, write_mode).await
    }

    return Ok(());
}
