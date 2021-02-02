use indicatif::{ProgressBar, ProgressStyle};
use tokio::sync::watch;
use tokio::sync::watch::Sender;
use tokio::task::JoinHandle;
use tokio::time;
use tokio::time::Duration;

use crate::error::DeviceResult;
use crate::message::DeviceResponse;
use crate::programmer::{ProgrammerResponseStatus, ProgrammerResponse};

pub struct Spinner {
    tx: Sender<bool>,
    handle: JoinHandle<()>
}
impl Spinner {
    pub fn spawn<S>(message: S) -> Self
        where
            S: AsRef<str>
    {
        let style = ProgressStyle::default_spinner()
            .template("{spinner}  {wide_msg}");
        let progress = ProgressBar::new_spinner();
        progress.set_style(style);
        progress.set_message(message.as_ref());

        let (tx, rx) = watch::channel(false);

        let handle = tokio::spawn(async move {
            while !*rx.borrow() {
                progress.tick();
                time::sleep(Duration::from_millis(50)).await;
            }
            progress.finish();
        });

        Spinner { tx, handle }
    }

    pub async fn finish(self) {
        self.tx.send(true).unwrap();
        self.handle.await.unwrap();
    }
}

pub struct Bar {
    tx: Sender<(u64, u64)>,
    handle: JoinHandle<()>
}
impl Bar {
    pub fn spawn<S>(message: S) -> Self
        where
            S: AsRef<str>
    {
        let style = ProgressStyle::default_bar()
            .template("{spinner}  {msg:24} [{bar:64.cyan/blue}]  {percent}%")
            .progress_chars("#>-");
        let progress = ProgressBar::new(1000);
        progress.set_style(style);
        progress.set_message(message.as_ref());

        let (tx, rx) = watch::channel((0, 1));

        let handle = tokio::spawn(async move {
            loop {
                let (val, max) = *rx.borrow();
                progress.set_length(max);
                progress.set_position(val);
                if val >= max { break; }
                time::sleep(Duration::from_millis(50)).await;
            }
            progress.finish();
        });

        Bar { tx, handle }
    }

    pub async fn await_response(mut self, mut res: ProgrammerResponse) -> (DeviceResult<DeviceResponse>, Option<Vec<u8>>) {
        let mut status = res.query();

        while status != ProgrammerResponseStatus::Ready {
            if let ProgrammerResponseStatus::MultipartIO { num, len } = status {
                self.progress(num, len);
            }
            time::sleep(Duration::from_millis(50)).await;
            status = res.query();
        }

        let response = res.resolve().await;
        self.finish().await;
        response
    }

    pub fn progress(&mut self, value: u64, len: u64) {
        self.tx.send((value, len)).unwrap();
    }

    pub async fn finish(self) {
        self.tx.send((1000, 1000)).unwrap();
        self.handle.await.unwrap();
    }
}